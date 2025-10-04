open Protocols
open Protocols.Exp
open Rel_cost

type t = {
  threads_per_warp : int option;
  block_dim : Dim3.t option;
  locals : Variable.t list;
  globals : Variable.t list;
  active_threads : bexp option;
  assumptions : bexp option;
  goals : Symbolic_metric_analysis.Theorem.Goal.t list;
}

let make =
  {
    threads_per_warp = None;
    block_dim = None;
    locals = [];
    globals = [];
    active_threads = None;
    assumptions = None;
    goals = [];
  }

let update_config (c : Rel_cost.Config.t) (s : t) : Rel_cost.Config.t =
  let open Rel_cost.Config in
  let threads_per_warp =
    Option.value ~default:c.threads_per_warp s.threads_per_warp
  in
  let block_dim = Option.value ~default:c.block_dim s.block_dim in
  { c with threads_per_warp; block_dim }

let to_theorem (config : Config.t) (s : t) : Symbolic_metric_analysis.Theorem.t
    =
  let open Symbolic_metric_analysis.Theorem in
  {
    cfg = update_config config s;
    locals = Variable.Set.of_list s.locals;
    globals = Variable.Set.of_list s.globals;
    active_threads = Option.value ~default:b_true s.active_threads;
    assumptions = Option.value ~default:b_true s.assumptions;
    goals = s.goals;
  }

let set_threads_per_warp v file = { file with threads_per_warp = Some v }
let set_block_dim dim file = { file with block_dim = Some dim }
let set_locals vars file = { file with locals = vars }
let set_globals vars file = { file with globals = vars }
let set_active_threads expr file = { file with active_threads = Some expr }
let set_assumptions expr file = { file with assumptions = Some expr }
let add_goal goal file = { file with goals = file.goals @ [ goal ] }

let of_theorem (thm : Symbolic_metric_analysis.Theorem.t) : t =
  let open Symbolic_metric_analysis.Theorem in
  {
    threads_per_warp = Some thm.cfg.threads_per_warp;
    block_dim = Some thm.cfg.block_dim;
    locals = Variable.Set.elements thm.locals;
    globals = Variable.Set.elements thm.globals;
    active_threads = Some thm.active_threads;
    assumptions = Some thm.assumptions;
    goals = thm.goals;
  }

let to_string (file : t) : string =
  (* Render all fields with default values for serializable format *)
  let threads_per_warp = Option.value ~default:32 file.threads_per_warp in
  let block_dim = Option.value ~default:(Dim3.make ~x:32 ()) file.block_dim in
  let locals_list =
    file.locals |> List.map Variable.name |> String.concat ", "
  in
  let globals_list =
    file.globals |> List.map Variable.name |> String.concat ", "
  in
  let active_threads = Option.value ~default:b_true file.active_threads in
  let assumptions = Option.value ~default:b_true file.assumptions in
  let block_dim_str =
    Printf.sprintf "{x: %d, y: %d, z: %d}" block_dim.x block_dim.y block_dim.z
  in

  (* Format goals with proper keywords *)
  let goals_str =
    let open Symbolic_metric_analysis.Theorem.Goal in
    List.map
      (function
        | Prop bexp -> "prove " ^ b_to_string bexp ^ ";"
        | Optimize { strategy = Gen_z3.Optimizer.Strategy.Maximize; expr } ->
            "max " ^ n_to_string expr ^ ";"
        | Optimize { strategy = Gen_z3.Optimizer.Strategy.Minimize; expr } ->
            "min " ^ n_to_string expr ^ ";")
      file.goals
    |> String.concat "\n"
  in

  Printf.sprintf
    "threads_per_warp: %d;\n\
     block_dim: %s;\n\
     locals: [%s];\n\
     globals: [%s];\n\
     active_threads: %s;\n\
     assumptions: %s;\n\
     %s"
    threads_per_warp block_dim_str locals_list globals_list
    (b_to_string active_threads)
    (b_to_string assumptions) goals_str
