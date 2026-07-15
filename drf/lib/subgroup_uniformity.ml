open Protocols
module SM = Inference.Subgroup_matrix

type error = Subgroup_config_error of SM.Target_config.error

let error_to_string : error -> string = function
  | Subgroup_config_error error -> SM.Target_config.error_to_string error

type violation_kind = Non_uniform_control

let violation_kind_to_string : violation_kind -> string = function
  | Non_uniform_control -> "non_uniform_control"

type outcome = Site_drf | Site_undefined_behavior of violation_kind

type verdict =
  | Subgroup_uniformity_drf
  | Subgroup_uniformity_undefined_behavior

type memory_component = Memory_drf | Memory_not_drf
type full_verdict = Drf_full | Not_drf

type operation =
  | Op_subgroup_barrier
  | Op_subgroup_collective of SM.Collective.kind
  | Op_matrix_collective of SM.Matrix.collective_kind

type control = { conditions : Exp.bexp list; uniform_vars : Variable.Set.t }

type site_result = {
  site : SM.Site.t;
  operation : operation;
  control_depth : int;
  outcome : outcome;
}

type function_result = { name : string; sites : site_result list }

let top_level_control : control =
  { conditions = []; uniform_vars = Variable.Set.empty }

let control ~(conditions : Exp.bexp list) : control =
  { conditions; uniform_vars = Variable.Set.empty }

let control_with_uniform_vars ~(conditions : Exp.bexp list)
    ~(uniform_vars : Variable.Set.t) : control =
  { conditions; uniform_vars }

let control_depth (control : control) : int = List.length control.conditions
let sites (result : function_result) : site_result list = result.sites
let site_result_site (site : site_result) : SM.Site.t = site.site
let site_result_control_depth (site : site_result) : int = site.control_depth
let site_result_outcome (site : site_result) : outcome = site.outcome

let outcome_is_drf : outcome -> bool = function
  | Site_drf -> true
  | Site_undefined_behavior _ -> false

let verdict_is_drf : verdict -> bool = function
  | Subgroup_uniformity_drf -> true
  | Subgroup_uniformity_undefined_behavior -> false

let function_verdict (result : function_result) : verdict =
  if List.for_all (fun site -> outcome_is_drf site.outcome) result.sites then
    Subgroup_uniformity_drf
  else Subgroup_uniformity_undefined_behavior

let compose (memory : memory_component) (subgroup : verdict) : full_verdict =
  match (memory, subgroup) with
  | Memory_drf, Subgroup_uniformity_drf -> Drf_full
  | Memory_not_drf, _ | _, Subgroup_uniformity_undefined_behavior -> Not_drf

let verdict_to_string : verdict -> string = function
  | Subgroup_uniformity_drf -> "drf"
  | Subgroup_uniformity_undefined_behavior -> "undefined_behavior"

let memory_component_to_string : memory_component -> string = function
  | Memory_drf -> "drf"
  | Memory_not_drf -> "not_drf"

let full_verdict_to_string : full_verdict -> string = function
  | Drf_full -> "drf"
  | Not_drf -> "not_drf"

let operation_to_string : operation -> string = function
  | Op_subgroup_barrier -> "subgroup_barrier"
  | Op_subgroup_collective _ -> "subgroup_collective"
  | Op_matrix_collective kind ->
      "matrix<" ^ SM.Matrix.collective_kind_to_string kind ^ ">"

let outcome_to_string : outcome -> string = function
  | Site_drf -> "drf"
  | Site_undefined_behavior reason ->
      "ub(reason=" ^ violation_kind_to_string reason ^ ")"

let site_result_to_string (site : site_result) : string =
  Printf.sprintf "%s %s control_depth=%d outcome=%s"
    (SM.Site.to_string site.site)
    (operation_to_string site.operation)
    site.control_depth
    (outcome_to_string site.outcome)

let summary_lines ~(memory : memory_component) (result : function_result) :
    string list =
  let subgroup = function_verdict result in
  [
    "kernel: " ^ result.name;
    "mem_drf: " ^ memory_component_to_string memory;
    "subgroup_uniformity: " ^ verdict_to_string subgroup;
    "drf_full: " ^ full_verdict_to_string (compose memory subgroup);
    "sites: " ^ string_of_int (List.length result.sites);
  ]
  @ List.map site_result_to_string result.sites

let site_of_stmt : SM.Stmt.t -> (SM.Site.t * operation) option = function
  | Workgroup_barrier _ -> None
  | Subgroup_barrier barrier -> Some (barrier.site, Op_subgroup_barrier)
  | Subgroup_collective collective ->
      Some
        (collective.site, Op_subgroup_collective (SM.Collective.kind collective))
  | Matrix_collective collective ->
      Some (collective.site, Op_matrix_collective collective.kind)

let subgroup_result_variables (kernel : SM.Kernel.t) : Variable.Set.t =
  List.fold_left
    (fun vars -> function
      | SM.Stmt.Subgroup_collective collective ->
          Variable.Set.add (SM.Collective.result collective) vars
      | Workgroup_barrier _ | Subgroup_barrier _ | Matrix_collective _ -> vars)
    Variable.Set.empty kernel.body

let site_control (site_controls : (SM.Site.id * control) list)
    (site : SM.Site.t) : control =
  List.assoc_opt (SM.Site.id site) site_controls
  |> Option.value ~default:top_level_control

let is_known_subgroup_uniform_builtin (x : Variable.t) : bool =
  List.exists (Variable.equal x)
    (Variable.bid_list @ Variable.bdim_list @ Variable.gdim_list)

let thread_coordinate_is_subgroup_uniform ~(config : SM.Target_config.t)
    (x : Variable.t) : bool =
  Option.is_some (SM.Target_config.cuda_x_contiguous_subgroup_size config)
  && (Variable.equal x Variable.tid_y || Variable.equal x Variable.tid_z)

let is_lane_variable (x : Variable.t) : bool =
  match Variable.name x with
  | "lane" | "lane_id" | "subgroup_lane" -> true
  | _ -> false

let is_explicit_uniform_var ~(uniform_vars : Variable.Set.t) (x : Variable.t) :
    bool =
  Variable.Set.mem x uniform_vars
  || Variable.Set.exists
       (fun root ->
         String.starts_with ~prefix:(Variable.name root ^ ".") (Variable.name x))
       uniform_vars

let subgroup_size_value (config : SM.Target_config.t) :
    (int option, error) result =
  match SM.Target_config.cuda_x_contiguous_subgroup_size config with
  | Some size -> Ok (Some (SM.Target_config.subgroup_size_value size))
  | None -> Ok None

let require_subgroup_size (config : SM.Target_config.t) : (int, error) result =
  match subgroup_size_value config with
  | Ok (Some size) -> Ok size
  | Ok None -> (
      match
        SM.Target_config.same_subgroup config
          ~left:SM.Target_config.cuda_thread_idx
          ~right:SM.Target_config.cuda_thread_idx
      with
      | Ok _ -> failwith "same_subgroup unexpectedly succeeded without size"
      | Error error -> Error (Subgroup_config_error error))
  | Error error -> Error error

let rec nexp_is_subgroup_uniform ~(config : SM.Target_config.t)
    ~(uniform_vars : Variable.Set.t) ~(varying_vars : Variable.Set.t)
    (expr : Exp.nexp) : (bool, error) result =
  let ( let* ) = Result.bind in
  let both left right =
    let* left =
      nexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars left
    in
    if not left then Ok false
    else nexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars right
  in
  match expr with
  | Num _ -> Ok true
  | Var x when Variable.Set.mem x varying_vars || is_lane_variable x -> Ok false
  | Var x when Variable.equal x Variable.tid_x -> Ok false
  | Var x
    when Variable.equal x Variable.tid_y || Variable.equal x Variable.tid_z ->
      Ok (thread_coordinate_is_subgroup_uniform ~config x)
  | Var x ->
      Ok
        (is_known_subgroup_uniform_builtin x
        || is_explicit_uniform_var ~uniform_vars x)
  | Binary (Div _, Var x, Num size) when Variable.equal x Variable.tid_x ->
      let* subgroup_size = require_subgroup_size config in
      Ok (Int.equal size subgroup_size)
  | Binary (Mod _, Var x, _) when Variable.equal x Variable.tid_x -> Ok false
  | Binary (_, left, right) -> both left right
  | Unary (_, expr) ->
      nexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars expr
  | NIf (cond, left, right) ->
      let* cond =
        bexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars cond
      in
      if not cond then Ok false else both left right
  | NCall _ -> Ok false
  | CastInt cond ->
      bexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars cond

and bexp_is_subgroup_uniform ~(config : SM.Target_config.t)
    ~(uniform_vars : Variable.Set.t) ~(varying_vars : Variable.Set.t)
    (cond : Exp.bexp) : (bool, error) result =
  let ( let* ) = Result.bind in
  let both left right =
    let* left =
      bexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars left
    in
    if not left then Ok false
    else bexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars right
  in
  match cond with
  | Bool _ -> Ok true
  | NRel (_, left, right) ->
      let* left =
        nexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars left
      in
      if not left then Ok false
      else nexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars right
  | BRel (_, left, right) -> both left right
  | BNot cond ->
      bexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars cond
  | Pred _ -> Ok false
  | CastBool expr ->
      nexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars expr
  | Distinct exprs ->
      exprs
      |> List.fold_left
           (fun accum expr ->
             let* accum = accum in
             if not accum then Ok false
             else
               nexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars expr)
           (Ok true)
  | AtomicResult _ | ThreadUnif _ -> Ok false

let control_is_subgroup_uniform ~(config : SM.Target_config.t)
    ~(uniform_vars : Variable.Set.t) ~(varying_vars : Variable.Set.t)
    (control : control) : (bool, error) result =
  control.conditions
  |> List.fold_left
       (fun accum cond ->
         let ( let* ) = Result.bind in
         let* accum = accum in
         if not accum then Ok false
         else bexp_is_subgroup_uniform ~config ~uniform_vars ~varying_vars cond)
       (Ok true)

let check_kernel ?(site_controls = []) ?(uniform_vars = Variable.Set.empty)
    (kernel : SM.Kernel.t) : (function_result, error) result =
  let varying_vars = subgroup_result_variables kernel in
  let check_site (site, operation) =
    let ( let* ) = Result.bind in
    let control = site_control site_controls site in
    let uniform_vars = Variable.Set.union uniform_vars control.uniform_vars in
    let* uniform =
      control_is_subgroup_uniform ~config:kernel.target_config ~uniform_vars
        ~varying_vars control
    in
    let outcome =
      if uniform then Site_drf else Site_undefined_behavior Non_uniform_control
    in
    Ok { site; operation; control_depth = control_depth control; outcome }
  in
  kernel.body
  |> List.filter_map site_of_stmt
  |> List.fold_left
       (fun accum site ->
         let ( let* ) = Result.bind in
         let* accum = accum in
         let* site = check_site site in
         Ok (site :: accum))
       (Ok [])
  |> Result.map (fun sites -> { name = kernel.name; sites = List.rev sites })
