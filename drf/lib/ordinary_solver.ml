open Protocols
module Memory = Memory_event.Subgroup_obligation

type classification = Pre_solver_unsat of string

let classification_to_string : classification -> string = function
  | Pre_solver_unsat reason -> "pre_solver=unsat(reason=" ^ reason ^ ")"

let access_origin_of_mode : Access.Mode.t -> Memory.access_origin = function
  | Read -> Ordinary_read
  | Write _ -> Ordinary_write
  | Atomic _ -> Ordinary_atomic

let conditional_access_of_summary (summary : Symbexp.AccessSummary.t) :
    Memory.conditional_access =
  {
    origin = access_origin_of_mode summary.access.mode;
    collective_site = None;
    source_site = None;
    source_order = None;
    access = summary.access;
    condition = summary.condition;
    subgroup_phase = Memory.Subgroup_phase_key.root;
  }

let candidate_pairs (accesses : Symbexp.AccessSummary.t list) :
    (Symbexp.AccessSummary.t * Symbexp.AccessSummary.t) list =
  accesses
  |> List.mapi (fun left_idx (left : Symbexp.AccessSummary.t) ->
      accesses
      |> List.filteri (fun right_idx (right : Symbexp.AccessSummary.t) ->
          right_idx >= left_idx && Access.can_conflict left.access right.access)
      |> List.map (fun right -> (left, right)))
  |> List.concat

let pair_obligation ~(array_name : string) (left : Symbexp.AccessSummary.t)
    (right : Symbexp.AccessSummary.t) : Memory.obligation =
  {
    id = 0;
    phase_id = 0;
    array_name;
    left = conditional_access_of_summary left;
    right = conditional_access_of_summary right;
    goal = Exp.Bool true;
  }

let globals_of_pair (left : Symbexp.AccessSummary.t)
    (right : Symbexp.AccessSummary.t) : Variable.Set.t =
  Variable.Set.union left.globals right.globals

let one_dimensional_strided_thread_ownership_matches ~(block_dim : Dim3.t)
    ~(array_name : string) (left : Symbexp.AccessSummary.t)
    (right : Symbexp.AccessSummary.t) : bool =
  let globals = globals_of_pair left right in
  let obligation = pair_obligation ~array_name left right in
  Subgroup_solver.one_dimensional_strided_thread_owner_matches ~globals
    ~block_dim obligation

let pre_solver_classification ?block_dim (proof : Symbexp.Proof.t) :
    classification option =
  match block_dim with
  | None -> None
  | Some block_dim ->
      let pairs = candidate_pairs proof.accesses in
      if
        pairs <> []
        && List.for_all
             (fun (left, right) ->
               one_dimensional_strided_thread_ownership_matches ~block_dim
                 ~array_name:proof.array_name left right)
             pairs
      then Some (Pre_solver_unsat "one-dimensional strided thread ownership")
      else None
