(*
  DRF-local memory-event boundary.

  The ordinary adapter is intentionally parallel to Symbexp.translate:
  it consumes Flatacc.Kernel.t values, builds explicit ordinary memory events
  for one workgroup phase/location, and can re-emit the ordinary Symbexp.Proof.t
  shape for parity tests. Ordinary production routing enters through this
  module before downstream Symbexp decoration and solving.
*)
open Stage0
open Protocols
open Exp

module Ordinary_event = struct
  type origin = Ordinary

  type t = {
    id : int;
    phase_id : int;
    origin : origin;
    access : Access.t;
    guard : bexp;
    runtime : bexp;
  }

  let origin_to_string : origin -> string = function Ordinary -> "ordinary"
  let condition (event : t) : bexp = b_and event.runtime event.guard
  let dim (event : t) : int = List.length event.access.index
  let location (event : t) : Location.t = Access.location event.access

  let from_cond_access ~(phase_id : int) ~(runtime : bexp) (id : int)
      (ca : Flatacc.CondAccess.t) : t =
    {
      id;
      phase_id;
      origin = Ordinary;
      access = ca.access;
      guard = ca.cond;
      runtime;
    }

  let to_cond_access (event : t) : Flatacc.CondAccess.t =
    { access = event.access; cond = condition event }
end

module Ordinary_phase = struct
  type t = {
    phase_id : int;
    kernel_name : string;
    array_name : string;
    exact_local_variables : Variable.Set.t;
    approx_local_variables : Variable.Set.t;
    precondition : bexp;
    events : Ordinary_event.t list;
  }

  let locals (phase : t) : Variable.Set.t =
    Variable.Set.union phase.exact_local_variables phase.approx_local_variables

  let dim (phase : t) : int =
    match phase.events with
    | event :: _ -> Ordinary_event.dim event
    | [] -> failwith "Memory_event.Ordinary_phase.dim: empty event phase"

  let from_flat ~(phase_id : int) (kernel : Flatacc.Kernel.t) : t =
    let events =
      kernel.code |> Flatacc.Code.to_list
      |> List.mapi
           (Ordinary_event.from_cond_access ~phase_id ~runtime:kernel.runtime)
    in
    {
      phase_id;
      kernel_name = kernel.name;
      array_name = kernel.array_name;
      exact_local_variables = kernel.exact_local_variables;
      approx_local_variables = kernel.approx_local_variables;
      precondition = kernel.pre;
      events;
    }
end

module Ordinary_obligation = struct
  let project_event_access (locals : Variable.Set.t) (task : Task.t)
      (event : Ordinary_event.t) : Flatacc.CondAccess.t =
    event |> Ordinary_event.to_cond_access |> Symbexp.project_access locals task

  let event_to_bexp ?(assign_index = true) (locals : Variable.Set.t)
      (task : Task.t) (event : Ordinary_event.t) : bexp =
    let projected = project_event_access locals task event in
    Symbexp.Gen.assign_access_id task event.id
    :: projected.cond
    :: Symbexp.Gen.assign_mode task projected.access.mode
    ::
    (if assign_index then
       List.mapi (Symbexp.Gen.assign_index N_rel.Eq task) projected.access.index
     else [])
    |> b_and_ex

  let task_choices ?(assign_index = true) (phase : Ordinary_phase.t)
      (task : Task.t) : bexp =
    let locals = Ordinary_phase.locals phase in
    phase.events
    |> List.map (event_to_bexp ~assign_index locals task)
    |> b_or_ex

  let goal ?(assign_index = true) (arch : Architecture.t)
      (phase : Ordinary_phase.t) : bexp =
    let access_order =
      n_le (Symbexp.Gen.access_id Task.Task1) (Symbexp.Gen.access_id Task.Task2)
    in
    b_and_ex
      [
        task_choices ~assign_index phase Task.Task1;
        task_choices ~assign_index phase Task.Task2;
        access_order;
        Ordinary_phase.dim phase |> Symbexp.Gen.assign_dim;
        Symbexp.Gen.mode_spec arch;
      ]
    |> b_and phase.precondition

  let access_summaries (phase : Ordinary_phase.t) : Symbexp.AccessSummary.t list
      =
    let locals = Ordinary_phase.locals phase in
    let pre_fns = Exp.b_free_names phase.precondition Variable.Set.empty in
    List.map
      (fun (event : Ordinary_event.t) ->
        let cond_fns = Exp.b_free_names event.guard Variable.Set.empty in
        let data_fns = Access.free_names event.access Variable.Set.empty in
        let ctrl_fns = Variable.Set.union pre_fns cond_fns in
        let all_fns = Variable.Set.union data_fns ctrl_fns in
        {
          Symbexp.AccessSummary.access = event.access;
          variables = all_fns;
          globals = Variable.Set.diff all_fns locals;
          data_approx = Variable.Set.inter phase.approx_local_variables data_fns;
          control_approx =
            Variable.Set.inter phase.approx_local_variables ctrl_fns;
        })
      phase.events

  let to_proof ?(assign_index = true) (arch : Architecture.t)
      (phase : Ordinary_phase.t) : Symbexp.Proof.t =
    Symbexp.Proof.make ~id:phase.phase_id ~kernel_name:phase.kernel_name
      ~array_name:phase.array_name ~accesses:(access_summaries phase)
      ~goal:(goal ~assign_index arch phase)

  let from_flat ?(assign_index = true) (arch : Architecture.t) (phase_id : int)
      (kernel : Flatacc.Kernel.t) : Symbexp.Proof.t =
    kernel |> Ordinary_phase.from_flat ~phase_id |> to_proof ~assign_index arch
end

module Subgroup_event = struct
  module SM = Inference.Subgroup_matrix
  module SS = Inference.Subgroup_source

  type error =
    | Missing_explicit_target_config of {
        kernel : string;
        target_config : string;
      }
    | Ordinary_effect_target_config_mismatch of {
        expected : string;
        actual : string;
        memory_effect : string;
      }

  let error_to_string : error -> string = function
    | Missing_explicit_target_config { kernel; target_config } ->
        Printf.sprintf
          "kernel '%s' needs explicit subgroup target configuration for \
           unified memory events; got %s"
          kernel target_config
    | Ordinary_effect_target_config_mismatch { expected; actual; memory_effect }
      ->
        Printf.sprintf
          "ordinary source memory effect target configuration mismatch: \
           expected %s, got %s for %s"
          expected actual memory_effect

  module Phase = struct
    type t = { workgroup : int; subgroup : SM.Site.id list }

    let root : t = { workgroup = 0; subgroup = [] }

    let enter_workgroup (phase : t) : t =
      { phase with workgroup = phase.workgroup + 1 }

    let enter_subgroup (site : SM.Site.t) (phase : t) : t =
      { phase with subgroup = phase.subgroup @ [ SM.Site.id site ] }

    let subgroup_to_string (subgroup : SM.Site.id list) : string =
      match subgroup with
      | [] -> "S[]"
      | ids -> "S[" ^ (ids |> List.map string_of_int |> String.concat ";") ^ "]"

    let to_string (phase : t) : string =
      Printf.sprintf "W%d/%s" phase.workgroup
        (subgroup_to_string phase.subgroup)
  end

  type memory_origin =
    | Ordinary_read
    | Ordinary_write
    | Ordinary_atomic
    | Matrix_load
    | Matrix_store

  type boundary_kind =
    | Workgroup_barrier
    | Subgroup_barrier
    | Subgroup_collective
    | Matrix_collective of SM.Matrix.collective_kind

  type memory = {
    origin : memory_origin;
    matrix_site : SM.Site.t option;
    source_site : string option;
    source_order : int option;
    access : Access.t;
    footprint : SM.Matrix.footprint option;
    source_conditions : bexp list;
    runtime_condition : bexp option;
    condition : bexp;
    phase : Phase.t;
    target_config : SM.Target_config.t;
  }

  type boundary = {
    kind : boundary_kind;
    site : SM.Site.t;
    source_order : int option;
    control_conditions : bexp list;
    memory_conditions : bexp list;
    uniform_vars : Variable.Set.t;
    phase_before : Phase.t;
    phase_after : Phase.t;
    target_config : SM.Target_config.t;
  }

  type event = Memory of memory | Boundary of boundary

  type t = {
    name : string;
    target_config : SM.Target_config.t;
    memory_globals : Variable.Set.t;
    uniform_vars : Variable.Set.t;
    events : event list;
  }

  let memory_origin_to_string : memory_origin -> string = function
    | Ordinary_read -> "ordinary_read"
    | Ordinary_write -> "ordinary_write"
    | Ordinary_atomic -> "ordinary_atomic"
    | Matrix_load -> "matrix_load"
    | Matrix_store -> "matrix_store"

  let boundary_kind_to_string : boundary_kind -> string = function
    | Workgroup_barrier -> "workgroup_barrier"
    | Subgroup_barrier -> "subgroup_barrier"
    | Subgroup_collective -> "subgroup_collective"
    | Matrix_collective kind ->
        "matrix_collective:" ^ SM.Matrix.collective_kind_to_string kind

  let memory_events (kernel : t) : memory list =
    kernel.events
    |> List.filter_map (function
      | Memory memory -> Some memory
      | Boundary _ -> None)

  let boundary_events (kernel : t) : boundary list =
    kernel.events
    |> List.filter_map (function
      | Boundary boundary -> Some boundary
      | Memory _ -> None)

  let site_control (site_controls : SS.site_control list) (site : SM.Site.t) :
      SS.site_control option =
    site_controls
    |> List.find_opt (fun (control : SS.site_control) ->
        Int.equal control.site_id (SM.Site.id site))

  let site_source_order (site_controls : SS.site_control list)
      (site : SM.Site.t) : int option =
    site_control site_controls site
    |> Option.map (fun (control : SS.site_control) -> control.source_order)

  let site_control_conditions (site_controls : SS.site_control list)
      (site : SM.Site.t) : bexp list =
    site_control site_controls site
    |> Option.map (fun (control : SS.site_control) -> control.conditions)
    |> Option.value ~default:[]

  let site_memory_conditions (site_controls : SS.site_control list)
      (site : SM.Site.t) : bexp list =
    site_control site_controls site
    |> Option.map SS.site_control_memory_conditions
    |> Option.value ~default:[]

  let site_uniform_vars (site_controls : SS.site_control list)
      (site : SM.Site.t) : Variable.Set.t =
    site_control site_controls site
    |> Option.map (fun (control : SS.site_control) -> control.uniform_vars)
    |> Option.value ~default:Variable.Set.empty

  let matrix_memory_origin : SM.Matrix.memory_effect -> memory_origin = function
    | SM.Matrix.Read _ -> Matrix_load
    | SM.Matrix.Write _ -> Matrix_store

  let matrix_memory ~(target_config : SM.Target_config.t)
      ~(site_controls : SS.site_control list) ~(phase : Phase.t)
      (site : SM.Site.t) (memory_effect : SM.Matrix.memory_effect) : memory =
    let footprint = SM.Matrix.memory_effect_footprint memory_effect in
    let source_conditions = site_memory_conditions site_controls site in
    {
      origin = matrix_memory_origin memory_effect;
      matrix_site = Some site;
      source_site = Some (SM.Site.to_string site);
      source_order = site_source_order site_controls site;
      access = SM.Matrix.indexed_access footprint;
      footprint = Some footprint;
      source_conditions;
      runtime_condition = None;
      condition =
        Exp.b_and
          (Exp.b_and_ex source_conditions)
          (SM.Matrix.bounds_condition footprint);
      phase;
      target_config;
    }

  let ordinary_memory_origin : SS.ordinary_memory_kind -> memory_origin =
    function
    | SS.Ordinary_read -> Ordinary_read
    | SS.Ordinary_write -> Ordinary_write
    | SS.Ordinary_atomic -> Ordinary_atomic

  let ordinary_site_to_string (site : SS.ordinary_memory_site) : string =
    let location =
      site.location
      |> Option.map (fun location -> "@" ^ Stage0.Location.to_string location)
      |> Option.value ~default:""
    in
    Printf.sprintf "ordinary#%d/order#%d[%s]%s" site.id site.source_order
      site.label location

  let ordinary_memory (memory_effect : SS.ordinary_memory_effect) : memory =
    let runtime = memory_effect.runtime_condition |> Option.to_list in
    let phase : Phase.t =
      {
        workgroup = memory_effect.phase.workgroup;
        subgroup = memory_effect.phase.subgroup;
      }
    in
    {
      origin = ordinary_memory_origin memory_effect.kind;
      matrix_site = None;
      source_site = Some (ordinary_site_to_string memory_effect.site);
      source_order = Some memory_effect.site.source_order;
      access = memory_effect.access;
      footprint = None;
      source_conditions = memory_effect.source_conditions;
      runtime_condition = memory_effect.runtime_condition;
      condition = Exp.b_and_ex (memory_effect.source_conditions @ runtime);
      phase;
      target_config = memory_effect.target_config;
    }

  let boundary ~(target_config : SM.Target_config.t)
      ~(site_controls : SS.site_control list) ~(kind : boundary_kind)
      ~(site : SM.Site.t) ~(phase_before : Phase.t) ~(phase_after : Phase.t) :
      boundary =
    {
      kind;
      site;
      source_order = site_source_order site_controls site;
      control_conditions = site_control_conditions site_controls site;
      memory_conditions = site_memory_conditions site_controls site;
      uniform_vars = site_uniform_vars site_controls site;
      phase_before;
      phase_after;
      target_config;
    }

  type builder = {
    target_config : SM.Target_config.t;
    site_controls : SS.site_control list;
    phase : Phase.t;
    events_rev : event list;
  }

  let add_event (event : event) (builder : builder) : builder =
    { builder with events_rev = event :: builder.events_rev }

  let add_boundary ~(kind : boundary_kind) ~(site : SM.Site.t)
      ~(phase_after : Phase.t) (builder : builder) : builder =
    let event =
      boundary ~target_config:builder.target_config
        ~site_controls:builder.site_controls ~kind ~site
        ~phase_before:builder.phase ~phase_after
      |> fun boundary -> Boundary boundary
    in
    { (add_event event builder) with phase = phase_after }

  let add_matrix_memory (site : SM.Site.t)
      (memory_effect : SM.Matrix.memory_effect) (builder : builder) : builder =
    matrix_memory ~target_config:builder.target_config
      ~site_controls:builder.site_controls ~phase:builder.phase site
      memory_effect
    |> fun memory -> add_event (Memory memory) builder

  let add_stmt (builder : builder) (stmt : SM.Stmt.t) : builder =
    match stmt with
    | Workgroup_barrier barrier ->
        add_boundary ~kind:Workgroup_barrier ~site:barrier.site
          ~phase_after:(Phase.enter_workgroup builder.phase)
          builder
    | Subgroup_barrier barrier ->
        add_boundary ~kind:Subgroup_barrier ~site:barrier.site
          ~phase_after:(Phase.enter_subgroup barrier.site builder.phase)
          builder
    | Subgroup_collective collective ->
        add_boundary ~kind:Subgroup_collective ~site:collective.site
          ~phase_after:(Phase.enter_subgroup collective.site builder.phase)
          builder
    | Matrix_collective collective ->
        let builder =
          match collective.memory with
          | None -> builder
          | Some memory_effect ->
              add_matrix_memory collective.site memory_effect builder
        in
        add_boundary ~kind:(Matrix_collective collective.kind)
          ~site:collective.site
          ~phase_after:(Phase.enter_subgroup collective.site builder.phase)
          builder

  let explicit_target_config (kernel : SM.Kernel.t) :
      (SM.Target_config.t, error) result =
    match kernel.target_config with
    | SM.Target_config.Cuda _ -> Ok kernel.target_config
    | SM.Target_config.Missing _ ->
        Error
          (Missing_explicit_target_config
             {
               kernel = kernel.name;
               target_config = SM.Target_config.to_string kernel.target_config;
             })

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

  let from_subgroup_kernel (kernel : SS.subgroup_kernel) : (t, error) result =
    let ( let* ) = Result.bind in
    let matrix_kernel = kernel.matrix_kernel in
    let* target_config = explicit_target_config matrix_kernel in
    let* () =
      kernel.ordinary_memory_effects
      |> List.fold_left
           (fun result memory_effect ->
             let* () = result in
             validate_ordinary_memory_effect_config target_config memory_effect)
           (Ok ())
    in
    let builder =
      {
        target_config;
        site_controls = kernel.site_controls;
        phase = Phase.root;
        events_rev = [];
      }
    in
    let events =
      List.fold_left add_stmt builder matrix_kernel.body |> fun builder ->
      List.rev builder.events_rev
    in
    let ordinary_events =
      kernel.ordinary_memory_effects
      |> List.map (fun memory_effect -> Memory (ordinary_memory memory_effect))
    in
    Ok
      {
        name = matrix_kernel.name;
        target_config;
        memory_globals = kernel.memory_globals;
        uniform_vars = kernel.uniform_vars;
        events = events @ ordinary_events;
      }
end

module Subgroup_obligation = struct
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
          "ordinary source memory effect target configuration mismatch: \
           expected %s, got %s for %s"
          expected actual memory_effect

  let error_of_event_error : Subgroup_event.error -> error = function
    | Missing_explicit_target_config _ ->
        Subgroup_config_error
          (SM.Target_config.Unsupported_target_configuration
             {
               target = SM.Target_config.Cuda_like;
               reason = "missing explicit subgroup lane mapping";
             })
    | Ordinary_effect_target_config_mismatch { expected; actual; memory_effect }
      ->
        Ordinary_effect_target_config_mismatch
          { expected; actual; memory_effect }

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

  type access_origin = Subgroup_event.memory_origin =
    | Ordinary_read
    | Ordinary_write
    | Ordinary_atomic
    | Matrix_load
    | Matrix_store

  let access_origin_to_string : access_origin -> string =
    Subgroup_event.memory_origin_to_string

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
           String.starts_with
             ~prefix:(Variable.name root ^ ".")
             (Variable.name x))
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
    else if Subgroup_phase_key.equal left.subgroup_phase right.subgroup_phase
    then Ok (Exp.Bool true)
    else same_subgroup_condition config |> Result.map Exp.b_not

  let subgroup_ordering_needs_identity (left : conditional_access)
      (right : conditional_access) : bool =
    same_matrix_collective_site left right
    || not (Subgroup_phase_key.equal left.subgroup_phase right.subgroup_phase)

  let matrix_site_control = Subgroup_event.site_control
  let matrix_site_source_order = Subgroup_event.site_source_order

  let matrix_site_memory_condition (site_controls : SS.site_control list)
      (site : SM.Site.t) : Exp.bexp =
    Subgroup_event.site_memory_conditions site_controls site |> Exp.b_and_ex

  let matrix_memory_access ~(site_controls : SS.site_control list)
      (site : SM.Site.t) (subgroup_phase : Subgroup_phase_key.t)
      (memory : SM.Matrix.memory_effect) : conditional_access =
    let phase : Subgroup_event.Phase.t =
      { workgroup = 0; subgroup = subgroup_phase }
    in
    let memory =
      Subgroup_event.matrix_memory ~target_config:SM.Target_config.missing_cuda
        ~site_controls ~phase site memory
    in
    {
      origin = memory.origin;
      collective_site = Option.map SM.Site.id memory.matrix_site;
      source_site = memory.source_site;
      source_order = memory.source_order;
      access = memory.access;
      condition = memory.condition;
      subgroup_phase = memory.phase.subgroup;
    }

  let ordinary_memory_kind_to_origin = Subgroup_event.ordinary_memory_origin

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

  let conditional_access_of_memory (memory : Subgroup_event.memory) :
      conditional_access =
    {
      origin = memory.origin;
      collective_site = Option.map SM.Site.id memory.matrix_site;
      source_site = memory.source_site;
      source_order = memory.source_order;
      access = memory.access;
      condition = memory.condition;
      subgroup_phase = memory.phase.subgroup;
    }

  let ordinary_memory_access (memory_effect : SS.ordinary_memory_effect) :
      conditional_access =
    Subgroup_event.ordinary_memory memory_effect |> conditional_access_of_memory

  module IntMap = Map.Make (Int)

  let add_access_to_phase (phase_id : int) (access : conditional_access)
      (phases : conditional_access list IntMap.t) :
      conditional_access list IntMap.t =
    let accesses =
      IntMap.find_opt phase_id phases |> Option.value ~default:[]
    in
    IntMap.add phase_id (accesses @ [ access ]) phases

  let phases_of_unified_events (kernel : Subgroup_event.t) : phased_kernel =
    let phases =
      kernel |> Subgroup_event.memory_events
      |> List.fold_left
           (fun phases (memory : Subgroup_event.memory) ->
             add_access_to_phase memory.phase.workgroup
               (conditional_access_of_memory memory)
               phases)
           IntMap.empty
    in
    {
      name = kernel.name;
      phases =
        phases |> IntMap.bindings
        |> List.map (fun (id, accesses) -> { id; accesses });
    }

  let kernel_with_config (config : SM.Target_config.t) (kernel : SM.Kernel.t) :
      SM.Kernel.t =
    SM.Kernel.make ~target_config:config ~name:kernel.name kernel.body

  let subgroup_kernel ?config ?(site_controls = [])
      ?(memory_globals = Variable.Set.empty)
      ?(uniform_vars = Variable.Set.empty) ?(ordinary_memory_effects = [])
      (kernel : SM.Kernel.t) : SS.subgroup_kernel =
    let matrix_kernel =
      match config with
      | Some config -> kernel_with_config config kernel
      | None -> kernel
    in
    {
      matrix_kernel;
      site_controls;
      uniform_vars;
      memory_globals;
      ordinary_memory_effects;
    }

  let unified_events_of_kernel ?config ?site_controls ?memory_globals
      ?uniform_vars ?ordinary_memory_effects (kernel : SM.Kernel.t) :
      (Subgroup_event.t, error) result =
    subgroup_kernel ?config ?site_controls ?memory_globals ?uniform_vars
      ?ordinary_memory_effects kernel
    |> Subgroup_event.from_subgroup_kernel
    |> Result.map_error error_of_event_error

  let phases_of_kernel ?(site_controls = []) (kernel : SM.Kernel.t) :
      phased_kernel =
    match unified_events_of_kernel ~site_controls kernel with
    | Ok events -> phases_of_unified_events events
    | Error error ->
        failwith
          ("Memory_event.Subgroup_obligation.phases_of_kernel: "
         ^ error_to_string error)

  let phases_with_ordinary_memory_effects ?(site_controls = [])
      ~(ordinary_memory_effects : SS.ordinary_memory_effect list)
      (kernel : SM.Kernel.t) : phased_kernel =
    match
      unified_events_of_kernel ~site_controls ~ordinary_memory_effects kernel
    with
    | Ok events -> phases_of_unified_events events
    | Error error ->
        failwith
          ("Memory_event.Subgroup_obligation.phases_with_ordinary_memory_effects: "
         ^ error_to_string error)

  let access_free_names (access : conditional_access) (fns : Variable.Set.t) :
      Variable.Set.t =
    Access.free_names access.access fns |> Exp.b_free_names access.condition

  let project_nexp (globals : Variable.Set.t) (task : Task.t) (expr : Exp.nexp)
      : Exp.nexp =
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

  let project_bexp (globals : Variable.Set.t) (task : Task.t) (cond : Exp.bexp)
      : Exp.bexp =
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
    if not (block_dim_is_valid block_dim) then
      Error (Invalid_block_dim block_dim)
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
    let left_access =
      Access.map (project_nexp globals Task.Task1) left.access
    in
    let right_access =
      Access.map (project_nexp globals Task.Task2) right.access
    in
    let left_condition = project_bexp globals Task.Task1 left.condition in
    let right_condition = project_bexp globals Task.Task2 right.condition in
    let ( let* ) = Result.bind in
    let* invocation_domain =
      invocation_domain_condition ?block_dim left right
    in
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
            right_idx >= left_idx
            && Access.can_conflict left.access right.access)
        |> List.map (fun right -> (left, right)))
    |> List.concat

  let obligations_of_phased ?globals ?config ?block_dim
      (kernel : Subgroup_event.t) (phased : phased_kernel) :
      (obligation list, error) result =
    let ( let* ) = Result.bind in
    let globals = Option.value globals ~default:kernel.memory_globals in
    let config = Option.value config ~default:kernel.target_config in
    let next_id = ref 0 in
    let make_obligation phase_id array_name left right =
      let* goal = obligation_goal ~globals ?block_dim config left right in
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

  let obligations_of_events ?globals ?config ?block_dim
      (kernel : Subgroup_event.t) : (obligation list, error) result =
    phases_of_unified_events kernel
    |> obligations_of_phased ?globals ?config ?block_dim kernel

  let obligations ?globals ?config ?block_dim ?(site_controls = [])
      ?(ordinary_memory_effects = []) (kernel : SM.Kernel.t) :
      (obligation list, error) result =
    let memory_globals = Option.value globals ~default:Variable.Set.empty in
    let ( let* ) = Result.bind in
    let* events =
      unified_events_of_kernel ?config ~site_controls ~memory_globals
        ~ordinary_memory_effects kernel
    in
    obligations_of_events ~globals:memory_globals ?config ?block_dim events

  let obligation_to_string (obligation : obligation) : string =
    Printf.sprintf
      "obligation#%d phase=%d array=%s left=%s/%s right=%s/%s goal=%s"
      obligation.id obligation.phase_id obligation.array_name
      (access_origin_to_string obligation.left.origin)
      (Subgroup_phase_key.to_string obligation.left.subgroup_phase)
      (access_origin_to_string obligation.right.origin)
      (Subgroup_phase_key.to_string obligation.right.subgroup_phase)
      (Exp.b_to_string obligation.goal)
end

let translate (arch : Architecture.t)
    (stream : Flatacc.Kernel.t Streamutil.stream) :
    Symbexp.Proof.t Streamutil.stream =
  Streamutil.mapi (Ordinary_obligation.from_flat arch) stream

let sanity_check (arch : Architecture.t)
    (stream : Flatacc.Kernel.t Streamutil.stream) :
    Symbexp.Proof.t Streamutil.stream =
  Streamutil.mapi
    (Ordinary_obligation.from_flat ~assign_index:false arch)
    stream
