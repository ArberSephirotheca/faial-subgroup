open Stage0
open Protocols
module Memory = Memory_event.Subgroup_obligation
module Uniformity = Subgroup_uniformity
module Solver = Z3.Solver

type solver_config = { timeout_ms : int option; logic : string option }

let solver_config ?timeout_ms ?logic () : solver_config = { timeout_ms; logic }
let default_config : solver_config = solver_config ()

let solver_config_to_string (config : solver_config) : string =
  let timeout =
    config.timeout_ms |> Option.map string_of_int
    |> Option.value ~default:"none"
  in
  let logic = Option.value config.logic ~default:"default" in
  Printf.sprintf "logic=%s timeout_ms=%s z3_version=%s" logic timeout
    Z3.Version.to_string

type classification =
  | Solver_unsat_drf
  | Solver_sat_racy
  | Solver_unknown of string
  | Solver_timeout of string
  | Pre_solver_unsat of string
  | Unsupported of string

let normalize_reason (reason : string) : string =
  reason |> String.split_on_char '\n' |> String.concat " "

let reason_is_timeout (reason : string) : bool =
  let reason = String.lowercase_ascii reason in
  Common.contains ~substring:"timeout" reason
  || Common.contains ~substring:"canceled" reason

let classification_of_unknown_reason (reason : string) : classification =
  let reason = normalize_reason reason in
  if reason_is_timeout reason then Solver_timeout reason
  else Solver_unknown reason

let classification_to_string : classification -> string = function
  | Solver_unsat_drf -> "solver=unsat(drf)"
  | Solver_sat_racy -> "solver=sat(racy)"
  | Solver_unknown reason -> "solver=unknown(reason=" ^ reason ^ ")"
  | Solver_timeout reason -> "solver=timeout(reason=" ^ reason ^ ")"
  | Pre_solver_unsat reason -> "pre_solver=unsat(reason=" ^ reason ^ ")"
  | Unsupported reason -> "unsupported(reason=" ^ reason ^ ")"

let classification_is_memory_drf : classification -> bool = function
  | Solver_unsat_drf | Pre_solver_unsat _ -> true
  | Solver_sat_racy | Solver_unknown _ | Solver_timeout _ | Unsupported _ ->
      false

type obligation_evidence = {
  obligation_id : int;
  phase_id : int;
  array_name : string;
  goal : Exp.bexp;
  left_origin : Memory.access_origin;
  left_source_site : string option;
  left_subgroup_phase : Memory.Subgroup_phase_key.t;
  right_origin : Memory.access_origin;
  right_source_site : string option;
  right_subgroup_phase : Memory.Subgroup_phase_key.t;
  classification : classification;
}

type report = {
  kernel_name : string;
  config : solver_config;
  z3_version : string;
  obligations : obligation_evidence list;
}

type unsupported_boundary = {
  kernel_name : string;
  config : solver_config;
  z3_version : string;
  reason : string;
}

type loop_protocol_report = {
  kernel_name : string;
  config : solver_config;
  z3_version : string;
  classifications : classification list;
  evidence : string list;
}

type memory_outcome =
  | Memory_report of report
  | Memory_loop_protocol of loop_protocol_report
  | Memory_unsupported of unsupported_boundary

type memory_verdict =
  | Memory_drf
  | Memory_racy
  | Memory_unknown
  | Memory_timeout
  | Memory_unsupported

let memory_verdict_to_string : memory_verdict -> string = function
  | Memory_drf -> "drf"
  | Memory_racy -> "not_drf"
  | Memory_unknown -> "unknown"
  | Memory_timeout -> "timeout"
  | Memory_unsupported -> "unsupported"

let uniformity_memory_component : memory_verdict -> Uniformity.memory_component
    = function
  | Memory_drf -> Uniformity.Memory_drf
  | _ -> Uniformity.Memory_not_drf

let context_options (config : solver_config) : (string * string) list =
  [ ("model", "true"); ("proof", "false") ]
  @
  match config.timeout_ms with
  | Some timeout -> [ ("timeout", string_of_int timeout) ]
  | None -> []

let b_to_expr (config : solver_config) : Z3.context -> Exp.bexp -> Z3.Expr.expr
    =
  match config.logic with
  | Some logic when String.ends_with ~suffix:"BV" logic ->
      fun ctx goal -> Gen_z3.Bv64Gen.b_to_expr ctx (Formula.make goal)
  | _ -> fun ctx goal -> Gen_z3.IntGen.b_to_expr ctx (Formula.make goal)

let mk_solver (config : solver_config) (ctx : Z3.context) : Solver.solver =
  match config.logic with
  | None -> Solver.mk_simple_solver ctx
  | Some logic -> Solver.mk_solver_s ctx logic

let add_numeric_constant_binding (var : Variable.t) (value : int)
    (bindings : int option Variable.Map.t) : int option Variable.Map.t =
  match Variable.Map.find_opt var bindings with
  | None -> Variable.Map.add var (Some value) bindings
  | Some (Some existing) when Int.equal existing value -> bindings
  | Some _ -> Variable.Map.add var None bindings

let numeric_constant_binding_of_term : Exp.bexp -> (Variable.t * int) option =
  function
  | Exp.NRel (N_rel.Eq, Exp.Var var, Exp.Num value)
  | Exp.NRel (N_rel.Eq, Exp.Num value, Exp.Var var) ->
      Some (var, value)
  | _ -> None

let numeric_constant_bindings (goal : Exp.bexp) : Exp.nexp Variable.Map.t =
  goal |> Constfold.b_opt |> Constfold.norm
  |> List.fold_left
       (fun bindings term ->
         match numeric_constant_binding_of_term term with
         | None -> bindings
         | Some (var, value) -> add_numeric_constant_binding var value bindings)
       Variable.Map.empty
  |> Variable.Map.filter_map (fun _ -> function
    | Some value -> Some (Exp.Num value)
    | None -> None)

let substitute_numeric_constants_once (goal : Exp.bexp) : Exp.bexp =
  let bindings = numeric_constant_bindings goal in
  if Variable.Map.is_empty bindings then Constfold.b_opt goal
  else
    let subst =
      bindings |> Variable.Map.bindings
      |> List.map (fun (var, expr) -> (Variable.name var, expr))
      |> Subst.SubstAssoc.make
    in
    goal |> Subst.ReplaceAssoc.b_subst subst |> Constfold.b_opt

let normalize_goal_for_solver (goal : Exp.bexp) : Exp.bexp =
  let rec loop fuel goal =
    if fuel = 0 then goal
    else
      let simplified = substitute_numeric_constants_once goal in
      if simplified = goal then simplified else loop (fuel - 1) simplified
  in
  loop 8 (Constfold.b_opt goal)

let solve_goal ?(config = default_config) (goal : Exp.bexp) : classification =
  try
    let ctx = Z3.mk_context (context_options config) in
    let solver = mk_solver config ctx in
    Solver.add solver
      [
        b_to_expr config ctx
          (Predicates.b_inline goal |> normalize_goal_for_solver);
      ];
    match Solver.check solver [] with
    | UNSATISFIABLE -> Solver_unsat_drf
    | SATISFIABLE -> Solver_sat_racy
    | UNKNOWN ->
        Solver.get_reason_unknown solver |> classification_of_unknown_reason
  with
  | Gen_z3.Not_implemented reason ->
      Unsupported ("not_implemented: " ^ normalize_reason reason)
  | Gen_z3.Preprocessing_error reason ->
      Unsupported ("preprocessing_error: " ^ normalize_reason reason)
  | Z3.Error reason -> Unsupported ("z3_error: " ^ normalize_reason reason)

let evidence_of_obligation ?(classification = None) ?(config = default_config)
    (obligation : Memory.obligation) : obligation_evidence =
  let classification =
    match classification with
    | Some classification -> classification
    | None -> solve_goal ~config obligation.goal
  in
  {
    obligation_id = obligation.id;
    phase_id = obligation.phase_id;
    array_name = obligation.array_name;
    goal = obligation.goal;
    left_origin = obligation.left.origin;
    left_source_site = obligation.left.source_site;
    left_subgroup_phase = obligation.left.subgroup_phase;
    right_origin = obligation.right.origin;
    right_source_site = obligation.right.source_site;
    right_subgroup_phase = obligation.right.subgroup_phase;
    classification;
  }

let pre_solver_unsat_evidence ~(reason : string)
    (obligation : Memory.obligation) : obligation_evidence =
  evidence_of_obligation
    ~classification:(Some (Pre_solver_unsat (normalize_reason reason)))
    obligation

let task_suffix (task : Task.t) : string = "$" ^ Task.to_string task

let projection_globals (globals : Variable.Set.t) : Variable.Set.t =
  globals
  |> Variable.Set.union Variable.bid_set
  |> Variable.Set.add Variable.bdim_x
  |> Variable.Set.add Variable.bdim_y
  |> Variable.Set.add Variable.bdim_z
  |> Variable.Set.add Variable.gdim_x
  |> Variable.Set.add Variable.gdim_y
  |> Variable.Set.add Variable.gdim_z

let strip_task_suffix ~(suffix : string) (name : string) : string option =
  if String.ends_with ~suffix name then
    Some (String.sub name 0 (String.length name - String.length suffix))
  else None

let projected_base_name ~(task : Task.t) : Exp.nexp -> string option = function
  | Exp.Var x -> strip_task_suffix ~suffix:(task_suffix task) (Variable.name x)
  | _ -> None

let projected_index_pair_shares_base_variable (left : Exp.nexp)
    (right : Exp.nexp) : bool =
  match
    ( projected_base_name ~task:Task.Task1 left,
      projected_base_name ~task:Task.Task2 right )
  with
  | Some left, Some right -> String.equal left right
  | _ -> false

let modulus_matches_stride ~(stride : int) : Exp.nexp -> bool = function
  | Exp.Num value -> Int.equal value stride
  | Exp.Var variable -> Variable.equal variable Variable.bdim_x
  | _ -> false

let numeric_expr_is_index_minus_thread (expr : Exp.nexp) ~(index : Exp.nexp)
    ~(thread_index_x : Variable.t) : bool =
  match expr with
  | Exp.Binary (N_binary.Minus _, lhs, Exp.Var rhs) ->
      lhs = index && Variable.equal rhs thread_index_x
  | _ -> false

let numeric_expr_is_strided_owner_mod (expr : Exp.nexp) ~(index : Exp.nexp)
    ~(thread_index_x : Variable.t) ~(stride : int) : bool =
  match expr with
  | Exp.Binary (N_binary.Mod _, dividend, modulus) ->
      modulus_matches_stride ~stride modulus
      && numeric_expr_is_index_minus_thread dividend ~index ~thread_index_x
  | _ -> false

let term_is_strided_owner_mod_zero (term : Exp.bexp) ~(index : Exp.nexp)
    ~(thread_index_x : Variable.t) ~(stride : int) : bool =
  let is_zero : Exp.nexp -> bool = function Exp.Num 0 -> true | _ -> false in
  match term with
  | Exp.NRel (N_rel.Eq, lhs, rhs) ->
      is_zero rhs
      && numeric_expr_is_strided_owner_mod lhs ~index ~thread_index_x ~stride
      || is_zero lhs
         && numeric_expr_is_strided_owner_mod rhs ~index ~thread_index_x ~stride
  | _ -> false

let condition_contains_strided_thread_owner (condition : Exp.bexp)
    ~(index : Exp.nexp) ~(thread_index_x : Variable.t) ~(stride : int) : bool =
  condition |> Exp.b_and_split
  |> List.exists (term_is_strided_owner_mod_zero ~index ~thread_index_x ~stride)

type strided_owner_index_shape = {
  owned_index : Exp.nexp;
  shared_offset : Exp.nexp;
}

let strided_owner_index_shapes (index : Exp.nexp) :
    strided_owner_index_shape list =
  let shape owned_index shared_offset = { owned_index; shared_offset } in
  match index with
  | Exp.Var _ -> [ shape index (Exp.Num 0) ]
  | Exp.Binary (N_binary.Plus _, Exp.Var owner, offset)
  | Exp.Binary (N_binary.Plus _, offset, Exp.Var owner) ->
      [ shape (Exp.Var owner) offset ]
  | Exp.Binary (N_binary.Minus _, Exp.Var owner, offset) ->
      [ shape (Exp.Var owner) (Exp.n_uminus offset) ]
  | _ -> []

let projected_variable_shares_base ~(left_task : Task.t) ~(right_task : Task.t)
    (left : Variable.t) (right : Variable.t) : bool =
  match
    ( projected_base_name ~task:left_task (Exp.Var left),
      projected_base_name ~task:right_task (Exp.Var right) )
  with
  | Some left, Some right -> String.equal left right
  | _ -> false

let expr_is_zero : Exp.nexp -> bool = function Exp.Num 0 -> true | _ -> false

let equality_sides : Exp.bexp -> (Exp.nexp * Exp.nexp) option = function
  | Exp.NRel (N_rel.Eq, lhs, rhs) -> Some (lhs, rhs)
  | _ -> None

let rec nexp_equiv (left : Exp.nexp) (right : Exp.nexp) : bool =
  match (left, right) with
  | Exp.Num left, Exp.Num right -> Int.equal left right
  | Exp.Var left, Exp.Var right -> Variable.equal left right
  | ( Exp.Binary (left_op, left_lhs, left_rhs),
      Exp.Binary (right_op, right_lhs, right_rhs) ) ->
      left_op = right_op
      && nexp_equiv left_lhs right_lhs
      && nexp_equiv left_rhs right_rhs
  | Exp.Unary (left_op, left_expr), Exp.Unary (right_op, right_expr) ->
      left_op = right_op && nexp_equiv left_expr right_expr
  | Exp.NCall (left_name, left_args), Exp.NCall (right_name, right_args) ->
      String.equal left_name right_name
      && List.equal nexp_equiv left_args right_args
  | ( Exp.NIf (left_cond, left_then, left_else),
      Exp.NIf (right_cond, right_then, right_else) ) ->
      left_cond = right_cond
      && nexp_equiv left_then right_then
      && nexp_equiv left_else right_else
  | Exp.CastInt left, Exp.CastInt right -> left = right
  | _, _ -> false

let expr_constant_bindings (terms : Exp.bexp list) : int Variable.Map.t =
  terms
  |> List.filter_map numeric_constant_binding_of_term
  |> List.fold_left
       (fun bindings (var, value) -> Variable.Map.add var value bindings)
       Variable.Map.empty

let rec expr_eval_with_constants (constants : int Variable.Map.t)
    (expr : Exp.nexp) : int option =
  match expr with
  | Exp.Num value -> Some value
  | Exp.Var var -> Variable.Map.find_opt var constants
  | Exp.Binary (op, lhs, rhs) -> (
      match
        ( expr_eval_with_constants constants lhs,
          expr_eval_with_constants constants rhs )
      with
      | Some lhs, Some rhs -> (
          try Some (N_binary.eval op lhs rhs) with Division_by_zero -> None)
      | _ -> None)
  | Exp.Unary (op, expr) ->
      expr_eval_with_constants constants expr |> Option.map (N_unary.eval op)
  | Exp.Convert { arg; ty } ->
      Option.bind (expr_eval_with_constants constants arg) (fun value ->
          Scalar.reduce value ty)
  | Exp.NCall _ | Exp.NIf _ | Exp.CastInt _ | Exp.ReadResult _ -> None

let expr_proven_constant (terms : Exp.bexp list) (expr : Exp.nexp) : int option
    =
  expr_eval_with_constants (expr_constant_bindings terms) expr

let expr_proven_as ~(value : int) (terms : Exp.bexp list) (expr : Exp.nexp) :
    bool =
  expr_proven_constant terms expr
  |> Option.map (Int.equal value)
  |> Option.value ~default:false

let expression_is_multiple_of ~(factor : int) (terms : Exp.bexp list)
    (expr : Exp.nexp) : bool =
  factor > 0
  &&
  let constants = expr_constant_bindings terms in
  let rec loop expr =
    match expr_eval_with_constants constants expr with
    | Some value -> value mod factor = 0
    | None -> (
        match expr with
        | Exp.Binary (N_binary.Mult _, lhs, rhs) -> loop lhs || loop rhs
        | _ -> false)
  in
  loop expr

let normalized_condition_terms_with_constants (condition : Exp.bexp) :
    Exp.bexp list =
  condition |> normalize_goal_for_solver |> Constfold.norm

let condition_equalities (terms : Exp.bexp list) : (Exp.nexp * Exp.nexp) list =
  List.filter_map equality_sides terms

let equalities_for_var (terms : Exp.bexp list) (var : Variable.t) :
    Exp.nexp list =
  condition_equalities terms
  |> List.filter_map (fun (lhs, rhs) ->
      match (lhs, rhs) with
      | Exp.Var lhs, rhs when Variable.equal lhs var -> Some rhs
      | lhs, Exp.Var rhs when Variable.equal rhs var -> Some lhs
      | _ -> None)

let term_has_relation (terms : Exp.bexp list) ~(op : N_rel.t) ~(lhs : Exp.nexp)
    ~(rhs : Exp.nexp) : bool =
  List.exists
    (function
      | Exp.NRel (candidate_op, candidate_lhs, candidate_rhs) ->
          candidate_op = op
          && nexp_equiv candidate_lhs lhs
          && nexp_equiv candidate_rhs rhs
      | _ -> false)
    terms

let term_has_left_relation (terms : Exp.bexp list) ~(op : N_rel.t)
    ~(lhs : Exp.nexp) : bool =
  List.exists
    (function
      | Exp.NRel (candidate_op, candidate_lhs, _) ->
          candidate_op = op && nexp_equiv candidate_lhs lhs
      | _ -> false)
    terms

let term_has_eq_mod_zero (terms : Exp.bexp list) ~(dividend : Exp.nexp)
    ~(modulus : Exp.nexp) : bool =
  let mod_expr =
    Exp.Binary (N_binary.Mod Signedness.Signed, dividend, modulus)
  in
  List.exists
    (function
      | Exp.NRel (N_rel.Eq, lhs, rhs) ->
          (nexp_equiv lhs mod_expr && expr_is_zero rhs)
          || (expr_is_zero lhs && nexp_equiv rhs mod_expr)
      | _ -> false)
    terms

let add_terms (expr : Exp.nexp) : Exp.nexp list =
  Exp.n_bin_split (N_binary.Plus Signedness.Signed) expr

let mult_terms (expr : Exp.nexp) : Exp.nexp list =
  Exp.n_bin_split (N_binary.Mult Signedness.Signed) expr

let sum_terms (terms : Exp.nexp list) : Exp.nexp =
  List.fold_left Exp.n_plus (Exp.Num 0) terms

let additive_constant_and_terms (expr : Exp.nexp) : int * Exp.nexp list =
  add_terms expr
  |> List.fold_left
       (fun (constant, terms) -> function
         | Exp.Num value -> (constant + value, terms)
         | term -> (constant, term :: terms))
       (0, [])
  |> fun (constant, terms) -> (constant, List.rev terms)

type lane_vector_index_shape = {
  row_base : Variable.t;
  elem_base : Variable.t;
  component : int;
}

let lane_vector_index_shapes (index : Exp.nexp) : lane_vector_index_shape list =
  let component, terms = additive_constant_and_terms index in
  match terms with
  | [ Exp.Var first; Exp.Var second ] ->
      [
        { row_base = first; elem_base = second; component };
        { row_base = second; elem_base = first; component };
      ]
  | _ -> []

let thread_x_expr (task : Task.t) : Exp.nexp =
  Exp.Var (Variable.add_suffix (task_suffix task) Variable.tid_x)

let term_equates (terms : Exp.bexp list) (left : Exp.nexp) (right : Exp.nexp) :
    bool =
  List.exists
    (function
      | Exp.NRel (N_rel.Eq, lhs, rhs) ->
          (nexp_equiv lhs left && nexp_equiv rhs right)
          || (nexp_equiv lhs right && nexp_equiv rhs left)
      | _ -> false)
    terms

let expression_is_thread_x_or_alias (terms : Exp.bexp list) ~(task : Task.t)
    (expr : Exp.nexp) : bool =
  let thread_x = thread_x_expr task in
  nexp_equiv expr thread_x || term_equates terms expr thread_x

let match_var_times_int (expr : Exp.nexp) : (Variable.t * int) option =
  match mult_terms expr with
  | [ Exp.Var var; Exp.Num value ] | [ Exp.Num value; Exp.Var var ] ->
      Some (var, value)
  | _ -> None

type lane_vector_owner = { lane : Variable.t; width : int; subgroup : Exp.nexp }

let subgroup_stride_from_lane_vector_stride ~(width : int) (stride : Exp.nexp) :
    Exp.nexp option =
  if width <= 0 then None
  else
    match stride with
    | Exp.Num value when value > 0 && value mod width = 0 ->
        Some (Exp.Num (value / width))
    | _ -> (
        match mult_terms stride with
        | [ Exp.Num factor; subgroup ] when Int.equal factor width ->
            Some subgroup
        | [ subgroup; Exp.Num factor ] when Int.equal factor width ->
            Some subgroup
        | _ -> None)

let lane_vector_owner_matches (terms : Exp.bexp list) ~(task : Task.t)
    ~(elem_base : Variable.t) : lane_vector_owner list =
  terms
  |> List.filter_map (function
    | Exp.NRel (N_rel.Eq, lhs, rhs) when expr_is_zero lhs || expr_is_zero rhs
      -> (
        let mod_expr = if expr_is_zero lhs then rhs else lhs in
        match mod_expr with
        | Exp.Binary
            ( N_binary.Mod _,
              Exp.Binary (N_binary.Minus _, Exp.Var elem, lane_scaled),
              stride )
          when Variable.equal elem elem_base -> (
            match match_var_times_int lane_scaled with
            | Some (lane, width) -> (
                match subgroup_stride_from_lane_vector_stride ~width stride with
                | Some subgroup ->
                    let lane_expr = Exp.Var lane in
                    let lane_base = Exp.n_mult lane_expr (Exp.Num width) in
                    let has_lane_id =
                      List.exists
                        (function
                          | Exp.NRel
                              ( N_rel.Eq,
                                Exp.Var lhs,
                                Exp.Binary
                                  ( N_binary.Mod _,
                                    thread_candidate,
                                    subgroup_candidate ) )
                          | Exp.NRel
                              ( N_rel.Eq,
                                Exp.Binary
                                  ( N_binary.Mod _,
                                    thread_candidate,
                                    subgroup_candidate ),
                                Exp.Var lhs )
                            when Variable.equal lhs lane ->
                              nexp_equiv subgroup_candidate subgroup
                              && expression_is_thread_x_or_alias terms ~task
                                   thread_candidate
                          | _ -> false)
                        terms
                    in
                    if
                      has_lane_id
                      && term_has_relation terms
                           ~op:(N_rel.Le Signedness.Signed) ~lhs:lane_base
                           ~rhs:(Exp.Var elem_base)
                      && term_has_left_relation terms
                           ~op:(N_rel.Lt Signedness.Signed)
                           ~lhs:(Exp.Var elem_base)
                    then Some { lane; width; subgroup }
                    else None
                | None -> None)
            | None -> None)
        | _ -> None)
    | _ -> None)

type row_stride_shape = { row_anchor : Exp.nexp; row_stride : Exp.nexp }

let row_stride_shapes_from_alias (alias : Exp.nexp) : row_stride_shape list =
  let terms = add_terms alias in
  terms
  |> List.mapi (fun index term ->
      match mult_terms term with
      | [ left; right ] ->
          let anchor =
            terms
            |> List.filteri (fun other_index _ -> other_index <> index)
            |> sum_terms
          in
          [
            { row_anchor = anchor; row_stride = left };
            { row_anchor = anchor; row_stride = right };
          ]
      | _ -> [])
  |> List.concat

let row_stride_shapes (terms : Exp.bexp list) ~(row_base : Variable.t) :
    row_stride_shape list =
  equalities_for_var terms row_base
  |> List.map row_stride_shapes_from_alias
  |> List.concat

let row_stride_is_multiple_of_width (terms : Exp.bexp list) ~(width : int)
    (stride : Exp.nexp) : bool =
  expression_is_multiple_of ~factor:width terms stride
  ||
  match stride with
  | Exp.Var var ->
      equalities_for_var terms var
      |> List.exists (expression_is_multiple_of ~factor:width terms)
  | _ -> false

type lane_vector_descriptor = {
  lane_shape : lane_vector_index_shape;
  owner : lane_vector_owner;
  row_stride : row_stride_shape;
}

let lane_vector_descriptors (terms : Exp.bexp list) ~(task : Task.t)
    (index : Exp.nexp) : lane_vector_descriptor list =
  lane_vector_index_shapes index
  |> List.map (fun lane_shape ->
      lane_vector_owner_matches terms ~task ~elem_base:lane_shape.elem_base
      |> List.map (fun owner ->
          row_stride_shapes terms ~row_base:lane_shape.row_base
          |> List.filter (fun (shape : row_stride_shape) ->
              row_stride_is_multiple_of_width terms ~width:owner.width
                shape.row_stride)
          |> List.map (fun row_stride -> { lane_shape; owner; row_stride }))
      |> List.concat)
  |> List.concat

let hierarchical_subgroup_row_lane_vector_matches
    ?(globals = Variable.Set.empty) ~(block_dim : Dim3.t)
    (obligation : Memory.obligation) : bool =
  block_dim.x > 0 && Int.equal block_dim.y 1 && Int.equal block_dim.z 1
  &&
  match (obligation.left.access.index, obligation.right.access.index) with
  | [ left_index ], [ right_index ] ->
      let globals = projection_globals globals in
      let left_index = Memory.project_nexp globals Task.Task1 left_index in
      let right_index = Memory.project_nexp globals Task.Task2 right_index in
      let left_terms =
        Memory.project_bexp globals Task.Task1 obligation.left.condition
        |> normalized_condition_terms_with_constants
      in
      let right_terms =
        Memory.project_bexp globals Task.Task2 obligation.right.condition
        |> normalized_condition_terms_with_constants
      in
      let left_descriptors =
        lane_vector_descriptors left_terms ~task:Task.Task1 left_index
      in
      let right_descriptors =
        lane_vector_descriptors right_terms ~task:Task.Task2 right_index
      in
      List.exists
        (fun left ->
          List.exists
            (fun right ->
              projected_variable_shares_base ~left_task:Task.Task1
                ~right_task:Task.Task2 left.lane_shape.row_base
                right.lane_shape.row_base
              && projected_variable_shares_base ~left_task:Task.Task1
                   ~right_task:Task.Task2 left.lane_shape.elem_base
                   right.lane_shape.elem_base
              && Int.equal left.owner.width right.owner.width
              && left.owner.width > 0
              && nexp_equiv left.row_stride.row_anchor
                   right.row_stride.row_anchor
              && nexp_equiv left.row_stride.row_stride
                   right.row_stride.row_stride
              && (left.lane_shape.component - right.lane_shape.component)
                 mod left.owner.width
                 <> 0)
            right_descriptors)
        left_descriptors
  | _ -> false

let hierarchical_subgroup_row_lane_vector_goal_matches ~(block_dim : Dim3.t)
    (obligation : Memory.obligation) : bool =
  block_dim.x > 0 && Int.equal block_dim.y 1 && Int.equal block_dim.z 1
  &&
  let terms = obligation.goal |> normalize_goal_for_solver |> Constfold.norm in
  condition_equalities terms
  |> List.exists (fun (left_index, right_index) ->
      let left_descriptors =
        lane_vector_descriptors terms ~task:Task.Task1 left_index
      in
      let right_descriptors =
        lane_vector_descriptors terms ~task:Task.Task2 right_index
      in
      List.exists
        (fun left ->
          List.exists
            (fun right ->
              projected_variable_shares_base ~left_task:Task.Task1
                ~right_task:Task.Task2 left.lane_shape.row_base
                right.lane_shape.row_base
              && projected_variable_shares_base ~left_task:Task.Task1
                   ~right_task:Task.Task2 left.lane_shape.elem_base
                   right.lane_shape.elem_base
              && Int.equal left.owner.width right.owner.width
              && left.owner.width > 0
              && nexp_equiv left.row_stride.row_anchor
                   right.row_stride.row_anchor
              && nexp_equiv left.row_stride.row_stride
                   right.row_stride.row_stride
              && (left.lane_shape.component - right.lane_shape.component)
                 mod left.owner.width
                 <> 0)
            right_descriptors)
        left_descriptors)

let numeric_relation_is_negation (left : N_rel.t) (right : N_rel.t) : bool =
  match (left, right) with
  | Eq, Neq | Neq, Eq | Lt _, Ge _ | Ge _, Lt _ | Gt _, Le _ | Le _, Gt _ ->
      true
  | _ -> false

let normalized_condition_terms (condition : Exp.bexp) : Exp.bexp list =
  condition |> Constfold.b_opt |> Constfold.norm

let bool_terms_are_syntactic_complements (left : Exp.bexp) (right : Exp.bexp) :
    bool =
  left = Exp.b_not right
  || right = Exp.b_not left
  ||
  match (left, right) with
  | ( Exp.NRel (left_op, left_lhs, left_rhs),
      Exp.NRel (right_op, right_lhs, right_rhs) ) ->
      left_lhs = right_lhs && left_rhs = right_rhs
      && numeric_relation_is_negation left_op right_op
  | _ -> false

let conditions_are_syntactically_disjoint (left : Exp.bexp) (right : Exp.bexp) :
    bool =
  let left_terms = normalized_condition_terms left in
  let right_terms = normalized_condition_terms right in
  List.exists
    (fun left ->
      left = Exp.Bool false
      || List.exists
           (fun right -> bool_terms_are_syntactic_complements left right)
           right_terms)
    left_terms
  || List.exists (fun right -> right = Exp.Bool false) right_terms

let contradictory_path_conditions_match ?(globals = Variable.Set.empty)
    (obligation : Memory.obligation) : bool =
  let globals = projection_globals globals in
  let left_condition =
    Memory.project_bexp globals Task.Task1 obligation.left.condition
  in
  let right_condition =
    Memory.project_bexp globals Task.Task2 obligation.right.condition
  in
  conditions_are_syntactically_disjoint left_condition right_condition

let one_dimensional_strided_thread_owner_matches ?(globals = Variable.Set.empty)
    ~(block_dim : Dim3.t) (obligation : Memory.obligation) : bool =
  block_dim.x > 0 && Int.equal block_dim.y 1 && Int.equal block_dim.z 1
  && List.length obligation.left.access.index
     = List.length obligation.right.access.index
  &&
  let globals = projection_globals globals in
  let project_index task = List.map (Memory.project_nexp globals task) in
  let left_indices = project_index Task.Task1 obligation.left.access.index in
  let right_indices = project_index Task.Task2 obligation.right.access.index in
  let left_condition =
    Memory.project_bexp globals Task.Task1 obligation.left.condition
  in
  let right_condition =
    Memory.project_bexp globals Task.Task2 obligation.right.condition
  in
  let left_thread_x =
    Variable.add_suffix (task_suffix Task.Task1) Variable.tid_x
  in
  let right_thread_x =
    Variable.add_suffix (task_suffix Task.Task2) Variable.tid_x
  in
  List.exists2
    (fun left_index right_index ->
      let left_shapes = strided_owner_index_shapes left_index in
      let right_shapes = strided_owner_index_shapes right_index in
      List.exists
        (fun left_shape ->
          List.exists
            (fun right_shape ->
              projected_index_pair_shares_base_variable left_shape.owned_index
                right_shape.owned_index
              && nexp_equiv left_shape.shared_offset right_shape.shared_offset
              && condition_contains_strided_thread_owner left_condition
                   ~index:left_shape.owned_index ~thread_index_x:left_thread_x
                   ~stride:block_dim.x
              && condition_contains_strided_thread_owner right_condition
                   ~index:right_shape.owned_index ~thread_index_x:right_thread_x
                   ~stride:block_dim.x)
            right_shapes)
        left_shapes)
    left_indices right_indices

let concrete_block_dim_from_goal (goal : Exp.bexp) : Dim3.t option =
  let terms = goal |> normalize_goal_for_solver |> Constfold.norm in
  let constants = expr_constant_bindings terms in
  match
    ( Variable.Map.find_opt Variable.bdim_x constants,
      Variable.Map.find_opt Variable.bdim_y constants,
      Variable.Map.find_opt Variable.bdim_z constants )
  with
  | Some x, Some y, Some z when x > 0 && y > 0 && z > 0 ->
      Some (Dim3.make ~x ~y ~z ())
  | _ -> None

let checked_block_dim_for_pre_solver ?block_dim (obligation : Memory.obligation)
    : Dim3.t option =
  match block_dim with
  | Some block_dim -> Some block_dim
  | None -> concrete_block_dim_from_goal obligation.goal

let pre_solver_classification ?globals ?block_dim
    (obligation : Memory.obligation) : classification option =
  if contradictory_path_conditions_match ?globals obligation then
    Some (Pre_solver_unsat "contradictory path-condition filter")
  else
    match checked_block_dim_for_pre_solver ?block_dim obligation with
    | Some block_dim
      when one_dimensional_strided_thread_owner_matches ?globals ~block_dim
             obligation ->
        Some (Pre_solver_unsat "one-dimensional strided thread ownership")
    | Some block_dim
      when hierarchical_subgroup_row_lane_vector_matches ?globals ~block_dim
             obligation
           || hierarchical_subgroup_row_lane_vector_goal_matches ~block_dim
                obligation ->
        Some
          (Pre_solver_unsat "hierarchical subgroup row/lane-vector ownership")
    | _ -> None

let evidence_with_pre_solver ?globals ?block_dim ~(config : solver_config)
    (obligation : Memory.obligation) : obligation_evidence =
  match pre_solver_classification ?globals ?block_dim obligation with
  | Some classification ->
      evidence_of_obligation ~classification:(Some classification) obligation
  | None -> evidence_of_obligation ~config obligation

let solve_obligations ?(config = default_config) ?globals ?block_dim
    ~kernel_name (obligations : Memory.obligation list) : report =
  {
    kernel_name;
    config;
    z3_version = Z3.Version.to_string;
    obligations =
      List.map
        (evidence_with_pre_solver ?globals ?block_dim ~config)
        obligations;
  }

let solve_obligation_result ?(config = default_config) ?globals ?block_dim
    ~kernel_name (obligations : (Memory.obligation list, Memory.error) result) :
    memory_outcome =
  match obligations with
  | Ok obligations ->
      Memory_report
        (solve_obligations ~config ?globals ?block_dim ~kernel_name obligations)
  | Error error ->
      Memory_unsupported
        {
          kernel_name;
          config;
          z3_version = Z3.Version.to_string;
          reason = Memory.error_to_string error;
        }

let memory_verdict_of_classifications (classifications : classification list) :
    memory_verdict =
  if
    List.exists
      (function Solver_sat_racy -> true | _ -> false)
      classifications
  then Memory_racy
  else if
    List.exists (function Unsupported _ -> true | _ -> false) classifications
  then Memory_unsupported
  else if
    List.exists
      (function Solver_timeout _ -> true | _ -> false)
      classifications
  then Memory_timeout
  else if
    List.exists
      (function Solver_unknown _ -> true | _ -> false)
      classifications
  then Memory_unknown
  else Memory_drf

let memory_verdict_of_report (report : report) : memory_verdict =
  report.obligations
  |> List.map (fun evidence -> evidence.classification)
  |> memory_verdict_of_classifications

let memory_verdict : memory_outcome -> memory_verdict = function
  | Memory_report report -> memory_verdict_of_report report
  | Memory_loop_protocol report ->
      memory_verdict_of_classifications report.classifications
  | Memory_unsupported _ -> Memory_unsupported

let loop_protocol_outcome ?(config = default_config) ~(kernel_name : string)
    ~(classifications : classification list) ~(evidence : string list) :
    unit -> memory_outcome =
 fun () ->
  Memory_loop_protocol
    {
      kernel_name;
      config;
      z3_version = Z3.Version.to_string;
      classifications;
      evidence;
    }

let is_repeated_site_boundary : memory_outcome -> bool = function
  | Memory_unsupported boundary ->
      Common.contains ~substring:"a memory-ordering barrier" boundary.reason
      && Common.contains ~substring:"dynamic invocation matching"
           boundary.reason
  | Memory_report _ | Memory_loop_protocol _ -> false

let resolve_repeated_site_with_loop_protocol ~(protocol : memory_outcome)
    (primary : memory_outcome) : memory_outcome =
  if is_repeated_site_boundary primary then protocol else primary

module Counts = struct
  type t = {
    total : int;
    racy : int;
    unknown : int;
    timeout : int;
    unsupported : int;
    pre_solver_unsat : int;
  }

  let empty : t =
    {
      total = 0;
      racy = 0;
      unknown = 0;
      timeout = 0;
      unsupported = 0;
      pre_solver_unsat = 0;
    }

  let add_classification (counts : t) : classification -> t = function
    | Solver_unsat_drf -> { counts with total = counts.total + 1 }
    | Solver_sat_racy ->
        { counts with total = counts.total + 1; racy = counts.racy + 1 }
    | Solver_unknown _ ->
        { counts with total = counts.total + 1; unknown = counts.unknown + 1 }
    | Solver_timeout _ ->
        { counts with total = counts.total + 1; timeout = counts.timeout + 1 }
    | Unsupported _ ->
        {
          counts with
          total = counts.total + 1;
          unsupported = counts.unsupported + 1;
        }
    | Pre_solver_unsat _ ->
        {
          counts with
          total = counts.total + 1;
          pre_solver_unsat = counts.pre_solver_unsat + 1;
        }

  let of_report (report : report) : t =
    report.obligations
    |> List.map (fun evidence -> evidence.classification)
    |> List.fold_left add_classification empty

  let unsupported_boundary : t = { empty with unsupported = 1 }

  let to_line (counts : t) : string =
    Printf.sprintf
      "memory_checks: %d total, %d racy, %d unknown, %d timeout, %d \
       unsupported, %d pre_solver_unsat"
      counts.total counts.racy counts.unknown counts.timeout counts.unsupported
      counts.pre_solver_unsat
end

let obligation_evidence_to_string (evidence : obligation_evidence) : string =
  let source_site = Option.value ~default:"unknown" in
  Printf.sprintf
    "obligation#%d phase=%d array=%s left=%s/%s left_site=%s right=%s/%s \
     right_site=%s %s goal=%s"
    evidence.obligation_id evidence.phase_id evidence.array_name
    (Memory.access_origin_to_string evidence.left_origin)
    (Memory.Subgroup_phase_key.to_string evidence.left_subgroup_phase)
    (source_site evidence.left_source_site)
    (Memory.access_origin_to_string evidence.right_origin)
    (Memory.Subgroup_phase_key.to_string evidence.right_subgroup_phase)
    (source_site evidence.right_source_site)
    (classification_to_string evidence.classification)
    (Exp.b_to_string evidence.goal)

let memory_outcome_kernel_name : memory_outcome -> string = function
  | Memory_report report -> report.kernel_name
  | Memory_loop_protocol report -> report.kernel_name
  | Memory_unsupported boundary -> boundary.kernel_name

let memory_outcome_config : memory_outcome -> solver_config = function
  | Memory_report report -> report.config
  | Memory_loop_protocol report -> report.config
  | Memory_unsupported boundary -> boundary.config

let memory_counts : memory_outcome -> Counts.t = function
  | Memory_report report -> Counts.of_report report
  | Memory_loop_protocol report ->
      List.fold_left Counts.add_classification Counts.empty
        report.classifications
  | Memory_unsupported _ -> Counts.unsupported_boundary

let memory_evidence_lines : memory_outcome -> string list = function
  | Memory_report report ->
      List.map obligation_evidence_to_string report.obligations
  | Memory_loop_protocol report -> report.evidence
  | Memory_unsupported boundary ->
      [ "unsupported(reason=" ^ normalize_reason boundary.reason ^ ")" ]

let supplement_with_protocol ~(protocol : memory_outcome)
    (primary : memory_outcome) : memory_outcome =
  match primary, protocol with
  | Memory_report primary, Memory_loop_protocol protocol ->
      Memory_loop_protocol { protocol with
        classifications =
          List.map (fun e -> e.classification) primary.obligations
          @ protocol.classifications;
        evidence = List.map obligation_evidence_to_string primary.obligations
          @ protocol.evidence }
  | _ -> primary

let summary_lines ?uniformity (outcome : memory_outcome) : string list =
  let memory_verdict = memory_verdict outcome in
  let base =
    [
      "kernel: " ^ memory_outcome_kernel_name outcome;
      "solver_config: "
      ^ solver_config_to_string (memory_outcome_config outcome);
      "mem_drf: " ^ memory_verdict_to_string memory_verdict;
      Counts.to_line (memory_counts outcome);
    ]
  in
  let uniformity_lines =
    match uniformity with
    | None -> []
    | Some result ->
        let subgroup = Uniformity.function_verdict result in
        [
          "subgroup_uniformity: " ^ Uniformity.verdict_to_string subgroup;
          "drf_full: "
          ^ Uniformity.full_verdict_to_string
              (Uniformity.compose
                 (uniformity_memory_component memory_verdict)
                 subgroup);
        ]
        @ List.map Uniformity.site_result_to_string (Uniformity.sites result)
  in
  base @ uniformity_lines @ memory_evidence_lines outcome
