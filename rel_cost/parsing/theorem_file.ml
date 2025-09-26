open Protocols
open Protocols.Exp
open Rel_cost

type t = {
  threads_per_warp : int option;
  block_dim : Dim3.t option;
  locals : Variable.t list option;
  local_context : bexp option;
  global_context : bexp option;
  goals : Symbolic_metric_analysis.Theorem.Goal.t list;
}

let make =
  {
    threads_per_warp = None;
    block_dim = None;
    locals = None;
    local_context = None;
    global_context = None;
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
    locals = Option.value ~default:[] s.locals |> Variable.Set.of_list;
    local_context = Option.value ~default:b_true s.local_context;
    global_context = Option.value ~default:b_true s.global_context;
    goals = s.goals;
  }

let set_threads_per_warp v file = { file with threads_per_warp = Some v }
let set_block_dim dim file = { file with block_dim = Some dim }
let set_locals vars file = { file with locals = Some vars }
let set_local_context expr file = { file with local_context = Some expr }
let set_global_context expr file = { file with global_context = Some expr }
let add_goal goal file = { file with goals = file.goals @ [ goal ] }

let of_theorem (thm : Symbolic_metric_analysis.Theorem.t) : t =
  let open Symbolic_metric_analysis.Theorem in
  {
    threads_per_warp = Some thm.cfg.threads_per_warp;
    block_dim = Some thm.cfg.block_dim;
    locals = Some (Variable.Set.elements thm.locals);
    local_context = Some thm.local_context;
    global_context = Some thm.global_context;
    goals = thm.goals;
  }

let to_string (file : t) : string =
  (* Render all fields with default values for serializable format *)
  let threads_per_warp = Option.value ~default:32 file.threads_per_warp in
  let block_dim = Option.value ~default:(Dim3.make ~x:32 ()) file.block_dim in
  let locals_list =
    Option.value ~default:[] file.locals
    |> List.map Variable.name |> String.concat ", "
  in
  let local_context = Option.value ~default:b_true file.local_context in
  let global_context = Option.value ~default:b_true file.global_context in
  let block_dim_str =
    Printf.sprintf "{x: %d, y: %d, z: %d}" block_dim.x block_dim.y block_dim.z
  in

  (* Format goals with proper keywords *)
  let goals_str =
    let open Symbolic_metric_analysis.Theorem.Goal in
    List.map
      (function
        | Prop bexp -> "prove " ^ b_to_string bexp
        | Optimize { strategy = Gen_z3.Optimizer.Strategy.Maximize; expr } ->
            "max " ^ n_to_string expr
        | Optimize { strategy = Gen_z3.Optimizer.Strategy.Minimize; expr } ->
            "min " ^ n_to_string expr)
      file.goals
    |> String.concat "\n"
  in

  Printf.sprintf
    "threads_per_warp: %d;\n\
     block_dim: %s;\n\
     locals: [%s];\n\
     local_context: %s;\n\
     global_context: %s;\n\
     %s"
    threads_per_warp block_dim_str locals_list
    (b_to_string local_context)
    (b_to_string global_context)
    goals_str
