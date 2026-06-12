open Protocols
open Exp
module SM = Inference.Subgroup_matrix
module SS = Inference.Subgroup_source

type error =
  | Subgroup_config_error of SM.Target_config.error
  | Missing_block_dim_for_subgroup_ordering
  | Invalid_block_dim of Dim3.t
  | Ordinary_effect_target_config_mismatch of {
      expected : string;
      actual : string;
      memory_effect : string;
    }

let error_to_string : error -> string = function
  | Subgroup_config_error error -> SM.Target_config.error_to_string error
  | Missing_block_dim_for_subgroup_ordering ->
      "missing checked block dimensions for subgroup memory ordering"
  | Invalid_block_dim block_dim ->
      Printf.sprintf
        "invalid checked block dimensions %s: dimensions must be positive"
        (Dim3.to_string block_dim)
  | Ordinary_effect_target_config_mismatch { expected; actual; memory_effect }
    ->
      Printf.sprintf
        "ordinary source memory effect target configuration mismatch: expected \
         %s, got %s for %s"
        expected actual memory_effect

module Subgroup_phase_key = struct
  type t = int list

  let root : t = []
  let push (site : SM.Site.t) (phase : t) : t = phase @ [ SM.Site.id site ]
  let equal : t -> t -> bool = List.equal Int.equal

  let to_string (phase : t) : string =
    match phase with
    | [] -> "S[]"
    | ids -> "S[" ^ (ids |> List.map string_of_int |> String.concat ";") ^ "]"
end

type access_origin =
  | Ordinary_read
  | Ordinary_write
  | Ordinary_atomic
  | Matrix_load
  | Matrix_store

let access_origin_to_string : access_origin -> string = function
  | Ordinary_read -> "ordinary_read"
  | Ordinary_write -> "ordinary_write"
  | Ordinary_atomic -> "ordinary_atomic"
  | Matrix_load -> "matrix_load"
  | Matrix_store -> "matrix_store"

let access_origin_is_matrix_collective : access_origin -> bool = function
  | Matrix_load | Matrix_store -> true
  | Ordinary_read | Ordinary_write | Ordinary_atomic -> false

type conditional_access = {
  origin : access_origin;
  collective_site : int option;
  source_site : string option;
  source_order : int option;
  access : Access.t;
  condition : Exp.bexp;
  subgroup_phase : Subgroup_phase_key.t;
}

type workgroup_phase = { id : int; accesses : conditional_access list }
type phased_kernel = { name : string; phases : workgroup_phase list }

type obligation = {
  id : int;
  phase_id : int;
  array_name : string;
  left : conditional_access;
  right : conditional_access;
  goal : Exp.bexp;
}

let task_suffix (task : Task.t) : string = "$" ^ Task.to_string task

let project_var (task : Task.t) (x : Variable.t) : Variable.t =
  Variable.add_suffix (task_suffix task) x

let var_is_global (globals : Variable.Set.t) (x : Variable.t) : bool =
  Variable.Set.mem x globals
  || Variable.Set.exists
       (fun root ->
         String.starts_with ~prefix:(Variable.name root ^ ".") (Variable.name x))
       globals

let thread_projection (task : Task.t) : SM.Target_config.thread_projection =
  SM.Target_config.cuda_thread_idx_with_suffix (task_suffix task)

let same_subgroup_condition (config : SM.Target_config.t) :
    (Exp.bexp, error) result =
  SM.Target_config.same_subgroup config
    ~left:(thread_projection Task.Task1)
    ~right:(thread_projection Task.Task2)
  |> Result.map_error (fun error -> Subgroup_config_error error)

let same_matrix_collective_site (left : conditional_access)
    (right : conditional_access) : bool =
  access_origin_is_matrix_collective left.origin
  && access_origin_is_matrix_collective right.origin
  &&
  match (left.collective_site, right.collective_site) with
  | Some left_site, Some right_site -> Int.equal left_site right_site
  | _ -> false

let not_ordered_by_subgroup_condition (config : SM.Target_config.t)
    (left : conditional_access) (right : conditional_access) :
    (Exp.bexp, error) result =
  if same_matrix_collective_site left right then
    same_subgroup_condition config |> Result.map Exp.b_not
  else if Subgroup_phase_key.equal left.subgroup_phase right.subgroup_phase then
    Ok (Exp.Bool true)
  else same_subgroup_condition config |> Result.map Exp.b_not

let subgroup_ordering_needs_identity (left : conditional_access)
    (right : conditional_access) : bool =
  same_matrix_collective_site left right
  || not (Subgroup_phase_key.equal left.subgroup_phase right.subgroup_phase)

let matrix_site_control (site_controls : SS.site_control list)
    (site : SM.Site.t) : SS.site_control option =
  site_controls
  |> List.find_opt (fun (control : SS.site_control) ->
      Int.equal control.site_id (SM.Site.id site))

let matrix_site_memory_condition (site_controls : SS.site_control list)
    (site : SM.Site.t) : Exp.bexp =
  matrix_site_control site_controls site
  |> Option.map (fun control ->
      Exp.b_and_ex (SS.site_control_memory_conditions control))
  |> Option.value ~default:(Exp.Bool true)

let matrix_site_source_order (site_controls : SS.site_control list)
    (site : SM.Site.t) : int option =
  matrix_site_control site_controls site
  |> Option.map (fun (control : SS.site_control) -> control.source_order)

let matrix_memory_access ~(site_controls : SS.site_control list)
    (site : SM.Site.t) (subgroup_phase : Subgroup_phase_key.t)
    (memory : SM.Matrix.memory_effect) : conditional_access =
  let footprint = SM.Matrix.memory_effect_footprint memory in
  let origin =
    match memory with
    | SM.Matrix.Read _ -> Matrix_load
    | SM.Matrix.Write _ -> Matrix_store
  in
  {
    origin;
    collective_site = Some (SM.Site.id site);
    source_site = Some (SM.Site.to_string site);
    source_order = matrix_site_source_order site_controls site;
    access = SM.Matrix.indexed_access footprint;
    condition =
      Exp.b_and
        (matrix_site_memory_condition site_controls site)
        (SM.Matrix.bounds_condition footprint);
    subgroup_phase;
  }

let ordinary_memory_kind_to_origin : SS.ordinary_memory_kind -> access_origin =
  function
  | SS.Ordinary_read -> Ordinary_read
  | SS.Ordinary_write -> Ordinary_write
  | SS.Ordinary_atomic -> Ordinary_atomic

let ordinary_memory_site_to_string (site : SS.ordinary_memory_site) : string =
  let location =
    site.location
    |> Option.map (fun location -> "@" ^ Stage0.Location.to_string location)
    |> Option.value ~default:""
  in
  Printf.sprintf "ordinary#%d[%s]%s" site.id site.label location

let ordinary_memory_condition (memory_effect : SS.ordinary_memory_effect) :
    Exp.bexp =
  let runtime = memory_effect.runtime_condition |> Option.to_list in
  Exp.b_and_ex (memory_effect.source_conditions @ runtime)

let ordinary_memory_access (memory_effect : SS.ordinary_memory_effect) :
    conditional_access =
  {
    origin = ordinary_memory_kind_to_origin memory_effect.kind;
    collective_site = None;
    source_site = Some (ordinary_memory_site_to_string memory_effect.site);
    source_order = Some memory_effect.site.source_order;
    access = memory_effect.access;
    condition = ordinary_memory_condition memory_effect;
    subgroup_phase = memory_effect.phase.subgroup;
  }

module Phase_builder = struct
  type t = {
    next_phase_id : int;
    subgroup_phase : Subgroup_phase_key.t;
    current_rev : conditional_access list;
    completed_rev : workgroup_phase list;
  }

  let empty : t =
    {
      next_phase_id = 0;
      subgroup_phase = Subgroup_phase_key.root;
      current_rev = [];
      completed_rev = [];
    }

  let current_phase (builder : t) : workgroup_phase =
    { id = builder.next_phase_id; accesses = List.rev builder.current_rev }

  let close_current (builder : t) : t =
    if builder.current_rev = [] then builder
    else
      {
        builder with
        current_rev = [];
        completed_rev = current_phase builder :: builder.completed_rev;
      }

  let add_access (access : conditional_access) (builder : t) : t =
    { builder with current_rev = access :: builder.current_rev }

  let enter_workgroup_phase (builder : t) : t =
    let builder = close_current builder in
    { builder with next_phase_id = builder.next_phase_id + 1 }

  let enter_subgroup_phase (site : SM.Site.t) (builder : t) : t =
    {
      builder with
      subgroup_phase = Subgroup_phase_key.push site builder.subgroup_phase;
    }

  let finish (builder : t) : workgroup_phase list =
    let builder = close_current builder in
    List.rev builder.completed_rev
end

let phases_of_kernel ?(site_controls = []) (kernel : SM.Kernel.t) :
    phased_kernel =
  let add_stmt (builder : Phase_builder.t) (stmt : SM.Stmt.t) : Phase_builder.t
      =
    match stmt with
    | Workgroup_barrier _ -> Phase_builder.enter_workgroup_phase builder
    | Subgroup_barrier barrier ->
        Phase_builder.enter_subgroup_phase barrier.site builder
    | Subgroup_collective collective ->
        Phase_builder.enter_subgroup_phase collective.site builder
    | Matrix_collective collective ->
        let builder =
          match collective.memory with
          | None -> builder
          | Some memory ->
              Phase_builder.add_access
                (matrix_memory_access ~site_controls collective.site
                   builder.subgroup_phase memory)
                builder
        in
        Phase_builder.enter_subgroup_phase collective.site builder
  in
  {
    name = kernel.name;
    phases =
      List.fold_left add_stmt Phase_builder.empty kernel.body
      |> Phase_builder.finish;
  }

module IntMap = Map.Make (Int)

let add_access_to_phase (phase_id : int) (access : conditional_access)
    (phases : conditional_access list IntMap.t) :
    conditional_access list IntMap.t =
  let accesses = IntMap.find_opt phase_id phases |> Option.value ~default:[] in
  IntMap.add phase_id (accesses @ [ access ]) phases

let phases_with_ordinary_memory_effects ?(site_controls = [])
    ~(ordinary_memory_effects : SS.ordinary_memory_effect list)
    (kernel : SM.Kernel.t) : phased_kernel =
  let phased = phases_of_kernel ~site_controls kernel in
  let phases =
    phased.phases
    |> List.fold_left
         (fun phases (phase : workgroup_phase) ->
           List.fold_left
             (fun phases access -> add_access_to_phase phase.id access phases)
             phases phase.accesses)
         IntMap.empty
  in
  let phases =
    ordinary_memory_effects
    |> List.fold_left
         (fun phases (memory_effect : SS.ordinary_memory_effect) ->
           add_access_to_phase memory_effect.phase.workgroup
             (ordinary_memory_access memory_effect)
             phases)
         phases
  in
  {
    phased with
    phases =
      phases |> IntMap.bindings
      |> List.map (fun (id, accesses) -> { id; accesses });
  }

let access_free_names (access : conditional_access) (fns : Variable.Set.t) :
    Variable.Set.t =
  Access.free_names access.access fns |> Exp.b_free_names access.condition

let project_nexp (globals : Variable.Set.t) (task : Task.t) (expr : Exp.nexp) :
    Exp.nexp =
  let rec project_n (expr : Exp.nexp) : Exp.nexp =
    match expr with
    | Num _ -> expr
    | Var x when var_is_global globals x -> expr
    | Var x -> Var (project_var task x)
    | Unary (op, expr) -> Unary (op, project_n expr)
    | Binary (op, left, right) -> Binary (op, project_n left, project_n right)
    | NCall (name, expr) -> NCall (name, project_n expr)
    | NIf (cond, left, right) ->
        NIf (project_b cond, project_n left, project_n right)
    | Other expr -> Other (project_n expr)
    | CastInt cond -> CastInt (project_b cond)
  and project_b (cond : Exp.bexp) : Exp.bexp =
    match cond with
    | Bool _ -> cond
    | NRel (op, left, right) -> NRel (op, project_n left, project_n right)
    | BRel (op, left, right) -> BRel (op, project_b left, project_b right)
    | BNot cond -> BNot (project_b cond)
    | Pred (name, expr) -> Pred (name, project_n expr)
    | CastBool expr -> CastBool (project_n expr)
    | Distinct exprs -> Distinct (List.map project_n exprs)
  in
  project_n expr

let project_bexp (globals : Variable.Set.t) (task : Task.t) (cond : Exp.bexp) :
    Exp.bexp =
  match project_nexp globals task (CastInt cond) with
  | CastInt cond -> cond
  | _ -> failwith "internal error: boolean projection changed expression kind"

let projected_thread_distinct : Exp.bexp =
  let neq x =
    Exp.n_neq
      (Exp.Var (project_var Task.Task1 x))
      (Exp.Var (project_var Task.Task2 x))
  in
  Exp.b_or_ex (List.map neq Variable.tid_list)

let block_dim_is_valid (block_dim : Dim3.t) : bool =
  List.for_all (fun value -> value > 0) (Dim3.to_list block_dim)

let checked_invocation_domain_condition (block_dim : Dim3.t) :
    (Exp.bexp, error) result =
  if not (block_dim_is_valid block_dim) then Error (Invalid_block_dim block_dim)
  else
    let block_dim_facts =
      List.map2
        (fun dimension bound -> Exp.n_eq (Exp.Var dimension) (Exp.Num bound))
        Variable.bdim_list (Dim3.to_list block_dim)
    in
    let task_bounds task =
      List.map2
        (fun thread_idx block_dim ->
          let projected = Exp.Var (project_var task thread_idx) in
          Exp.b_and
            (Exp.n_ge projected (Exp.Num 0))
            (Exp.n_lt projected (Exp.Var block_dim)))
        Variable.tid_list Variable.bdim_list
    in
    Ok
      (Exp.b_and_ex
         (block_dim_facts @ task_bounds Task.Task1 @ task_bounds Task.Task2))

let invocation_domain_condition ?block_dim (left : conditional_access)
    (right : conditional_access) : (Exp.bexp, error) result =
  match block_dim with
  | Some block_dim -> checked_invocation_domain_condition block_dim
  | None ->
      if subgroup_ordering_needs_identity left right then
        Error Missing_block_dim_for_subgroup_ordering
      else Ok (Exp.Bool true)

let index_match_condition (left : Access.t) (right : Access.t) : Exp.bexp =
  let clauses =
    List.map2
      (fun left_index right_index ->
        Exp.b_and
          (Exp.n_eq left_index right_index)
          (Exp.n_ge left_index (Exp.Num 0)))
      left.index right.index
  in
  Exp.b_and_ex clauses

let obligation_goal ?(globals = Variable.Set.empty) ?block_dim
    (config : SM.Target_config.t) (left : conditional_access)
    (right : conditional_access) : (Exp.bexp, error) result =
  let globals =
    globals
    |> Variable.Set.union Variable.bid_set
    |> Variable.Set.add Variable.bdim_x
    |> Variable.Set.add Variable.bdim_y
    |> Variable.Set.add Variable.bdim_z
    |> Variable.Set.add Variable.gdim_x
    |> Variable.Set.add Variable.gdim_y
    |> Variable.Set.add Variable.gdim_z
  in
  let left_access = Access.map (project_nexp globals Task.Task1) left.access in
  let right_access =
    Access.map (project_nexp globals Task.Task2) right.access
  in
  let left_condition = project_bexp globals Task.Task1 left.condition in
  let right_condition = project_bexp globals Task.Task2 right.condition in
  let ( let* ) = Result.bind in
  let* invocation_domain = invocation_domain_condition ?block_dim left right in
  let* subgroup_condition =
    not_ordered_by_subgroup_condition config left right
  in
  Ok
    (Exp.b_and_ex
       [
         invocation_domain;
         projected_thread_distinct;
         left_condition;
         right_condition;
         index_match_condition left_access right_access;
         subgroup_condition;
       ])

let group_by_location (accesses : conditional_access list) :
    (string * conditional_access list) list =
  let add_access groups access =
    let name = access.access.array |> Variable.name in
    let existing = List.assoc_opt name groups |> Option.value ~default:[] in
    (name, existing @ [ access ]) :: List.remove_assoc name groups
  in
  List.fold_left add_access [] accesses |> List.rev

let candidate_pairs (accesses : conditional_access list) :
    (conditional_access * conditional_access) list =
  accesses
  |> List.mapi (fun left_idx left ->
      accesses
      |> List.filteri (fun right_idx right ->
          right_idx >= left_idx && Access.can_conflict left.access right.access)
      |> List.map (fun right -> (left, right)))
  |> List.concat

let validate_ordinary_memory_effect_config (config : SM.Target_config.t)
    (memory_effect : SS.ordinary_memory_effect) : (unit, error) result =
  let expected = SM.Target_config.to_string config in
  let actual = SM.Target_config.to_string memory_effect.target_config in
  if String.equal expected actual then Ok ()
  else
    Error
      (Ordinary_effect_target_config_mismatch
         {
           expected;
           actual;
           memory_effect = SS.ordinary_memory_effect_to_string memory_effect;
         })

let obligations ?globals ?config ?block_dim ?(site_controls = [])
    ?(ordinary_memory_effects = []) (kernel : SM.Kernel.t) :
    (obligation list, error) result =
  let ( let* ) = Result.bind in
  let config = Option.value config ~default:kernel.target_config in
  let* () =
    ordinary_memory_effects
    |> List.fold_left
         (fun result memory_effect ->
           let* () = result in
           validate_ordinary_memory_effect_config config memory_effect)
         (Ok ())
  in
  let phased =
    match ordinary_memory_effects with
    | [] -> phases_of_kernel ~site_controls kernel
    | _ ->
        phases_with_ordinary_memory_effects ~site_controls
          ~ordinary_memory_effects kernel
  in
  let next_id = ref 0 in
  let make_obligation phase_id array_name left right =
    let* goal = obligation_goal ?globals ?block_dim config left right in
    let id = !next_id in
    next_id := id + 1;
    Ok { id; phase_id; array_name; left; right; goal }
  in
  let obligations_for_phase (phase : workgroup_phase) =
    group_by_location phase.accesses
    |> List.map (fun (array_name, accesses) ->
        candidate_pairs accesses
        |> List.map (fun (left, right) ->
            make_obligation phase.id array_name left right))
    |> List.concat
  in
  let obligations =
    phased.phases |> List.map obligations_for_phase |> List.concat
  in
  List.fold_right
    (fun obligation accum ->
      match (obligation, accum) with
      | Ok obligation, Ok obligations -> Ok (obligation :: obligations)
      | Error error, _ | _, Error error -> Error error)
    obligations (Ok [])

let obligation_to_string (obligation : obligation) : string =
  Printf.sprintf
    "obligation#%d phase=%d array=%s left=%s/%s right=%s/%s goal=%s"
    obligation.id obligation.phase_id obligation.array_name
    (access_origin_to_string obligation.left.origin)
    (Subgroup_phase_key.to_string obligation.left.subgroup_phase)
    (access_origin_to_string obligation.right.origin)
    (Subgroup_phase_key.to_string obligation.right.subgroup_phase)
    (Exp.b_to_string obligation.goal)
