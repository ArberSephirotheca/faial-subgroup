open Protocols
module Memory = Memory_event.Subgroup_obligation
module SM = Inference.Subgroup_matrix
module Uniformity = Subgroup_uniformity

let dedup_conditions (conditions : Exp.bexp list) : Exp.bexp list =
  let rec loop seen kept = function
    | [] -> List.rev kept
    | condition :: rest ->
        if List.exists (fun existing -> existing = condition) seen then
          loop seen kept rest
        else loop (condition :: seen) (condition :: kept) rest
  in
  loop [] [] conditions

let rec nexp_definedness_conditions (expr : Exp.nexp) : Exp.bexp list =
  match expr with
  | Num _ | Var _ -> []
  | Binary ((Div _ | Mod _), lhs, rhs) ->
      nexp_definedness_conditions lhs
      @ nexp_definedness_conditions rhs
      @ [ Exp.n_neq rhs (Exp.Num 0) ]
  | Binary (_, lhs, rhs) ->
      nexp_definedness_conditions lhs @ nexp_definedness_conditions rhs
  | Unary (_, expr) -> nexp_definedness_conditions expr
  | NIf (cond, then_expr, else_expr) ->
      bexp_definedness_conditions cond
      @ nexp_definedness_conditions then_expr
      @ nexp_definedness_conditions else_expr
  | NCall (_, exprs) -> List.concat_map nexp_definedness_conditions exprs
  | CastInt cond -> bexp_definedness_conditions cond

and bexp_definedness_conditions (condition : Exp.bexp) : Exp.bexp list =
  match condition with
  | Bool _ -> []
  | NRel (_, lhs, rhs) ->
      nexp_definedness_conditions lhs @ nexp_definedness_conditions rhs
  | BRel (_, lhs, rhs) ->
      bexp_definedness_conditions lhs @ bexp_definedness_conditions rhs
  | BNot condition -> bexp_definedness_conditions condition
  | Pred (_, exprs) -> List.concat_map nexp_definedness_conditions exprs
  | CastBool expr -> nexp_definedness_conditions expr
  | Distinct exprs -> List.concat_map nexp_definedness_conditions exprs
  | AtomicResult { index; operation; _ } ->
      List.concat_map nexp_definedness_conditions index
      @ Atomic.Operation.fold
          (fun expr accum -> nexp_definedness_conditions expr @ accum)
          operation []
  | ThreadUnif expr -> nexp_definedness_conditions expr

let condition_free_names (conditions : Exp.bexp list) : Variable.Set.t =
  List.fold_left
    (fun names condition -> Exp.b_free_names condition names)
    Variable.Set.empty conditions

let relevant_numeric_alias_conditions (control : Uniformity.control) :
    Exp.bexp list =
  let rec collect seen pending facts =
    match pending with
    | [] -> List.rev facts
    | var :: rest when Variable.Set.mem var seen -> collect seen rest facts
    | var :: rest -> (
        let seen = Variable.Set.add var seen in
        match Variable.Map.find_opt var control.numeric_aliases with
        | None -> collect seen rest facts
        | Some expr ->
            let dependencies =
              Exp.n_free_names expr Variable.Set.empty |> Variable.Set.elements
            in
            let alias_facts =
              Exp.n_eq (Exp.Var var) expr :: nexp_definedness_conditions expr
            in
            collect seen (dependencies @ rest)
              (List.rev_append alias_facts facts))
  in
  collect Variable.Set.empty
    (condition_free_names control.conditions |> Variable.Set.elements)
    []
  |> dedup_conditions

let projection_globals ~(globals : Variable.Set.t)
    (control : Uniformity.control) : Variable.Set.t =
  globals
  |> Variable.Set.union control.uniform_vars
  |> Variable.Set.union Variable.bid_set
  |> Variable.Set.add Variable.bdim_x
  |> Variable.Set.add Variable.bdim_y
  |> Variable.Set.add Variable.bdim_z
  |> Variable.Set.add Variable.gdim_x
  |> Variable.Set.add Variable.gdim_y
  |> Variable.Set.add Variable.gdim_z

let symbolic_invocation_domain_condition () : Exp.bexp =
  let dimensions =
    [
      (Variable.tid_x, Variable.bdim_x);
      (Variable.tid_y, Variable.bdim_y);
      (Variable.tid_z, Variable.bdim_z);
    ]
  in
  let positive_dimensions =
    List.map
      (fun (_, block_dim) -> Exp.n_gt (Exp.Var block_dim) (Exp.Num 0))
      dimensions
  in
  let task_bounds task =
    List.map
      (fun (thread_idx, block_dim) ->
        let projected = Exp.Var (Memory.project_var task thread_idx) in
        Exp.b_and
          (Exp.n_ge projected (Exp.Num 0))
          (Exp.n_lt projected (Exp.Var block_dim)))
      dimensions
  in
  Exp.b_and_ex
    (positive_dimensions @ task_bounds Task.Task1 @ task_bounds Task.Task2)

let invocation_domain_condition ?checked_block_dim ?block_dim () :
    (Exp.bexp, Memory.error) result =
  match (checked_block_dim, block_dim) with
  | Some _, Some _ -> Error Memory.Conflicting_checked_block_dim_inputs
  | Some checked_block_dim, None ->
      Memory.checked_invocation_domain_condition checked_block_dim
  | None, Some block_dim ->
      let ( let* ) = Result.bind in
      let* checked_block_dim = Memory.checked_block_dim_of_dim3 block_dim in
      Memory.checked_invocation_domain_condition checked_block_dim
  | None, None -> Ok (symbolic_invocation_domain_condition ())

let disagreement_condition ~(globals : Variable.Set.t)
    (control : Uniformity.control) : Exp.bexp =
  let condition = Exp.b_and_ex control.conditions in
  let left = Memory.project_bexp globals Task.Task1 condition in
  let right = Memory.project_bexp globals Task.Task2 condition in
  Exp.b_or (Exp.b_and left (Exp.b_not right)) (Exp.b_and (Exp.b_not left) right)

let proves_control_uniform ?checked_block_dim ?block_dim
    ?(timeout : int option = None) ?(logic : string option = None)
    ~(globals : Variable.Set.t) ~(precondition : Exp.bexp)
    ~(target_config : SM.Target_config.t) (control : Uniformity.control) : bool
    =
  let globals = projection_globals ~globals control in
  let globals =
    match checked_block_dim with
    | None -> globals
    | Some checked_block_dim ->
        Memory.checked_block_dim_free_names checked_block_dim globals
  in
  match
    ( invocation_domain_condition ?checked_block_dim ?block_dim (),
      Memory.same_subgroup_condition target_config )
  with
  | Ok invocation_domain, Ok same_subgroup -> (
      let alias_facts =
        relevant_numeric_alias_conditions control
        @ List.concat_map bexp_definedness_conditions control.conditions
        |> dedup_conditions |> Exp.b_and_ex
      in
      let project task condition = Memory.project_bexp globals task condition in
      let goal =
        Exp.b_and_ex
          [
            project Task.Task1 precondition;
            project Task.Task2 precondition;
            project Task.Task1 alias_facts;
            project Task.Task2 alias_facts;
            invocation_domain;
            Memory.block_index_domain_condition;
            same_subgroup;
            disagreement_condition ~globals control;
          ]
      in
      try Gen_z3.is_unsat ~timeout ~logic goal
      with
      | Gen_z3.Not_implemented _ | Gen_z3.Preprocessing_error _ | Z3.Error _ ->
        false)
  | Error _, _ | _, Error _ -> false
