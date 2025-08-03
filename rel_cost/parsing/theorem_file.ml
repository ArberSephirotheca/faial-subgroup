open Protocols
open Protocols.Exp
open Rel_cost

type t = {
  threads_per_warp : int option;
  block_dim : Dim3.t option;
  locals : Variable.t list option;
  local_context : bexp option;
  global_context : bexp option;
  index : nexp;
  rel : N_rel.t;
  cost : nexp;
}

let make =
  {
    threads_per_warp = None;
    block_dim = None;
    locals = None;
    local_context = None;
    global_context = None;
    rel = N_rel.Eq;
    index = Num 0;
    cost = Num 0;
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
    index = s.index;
    rel = s.rel;
    expected_cost = s.cost;
  }

let set_threads_per_warp v file = { file with threads_per_warp = Some v }
let set_block_dim dim file = { file with block_dim = Some dim }
let set_locals vars file = { file with locals = Some vars }
let set_local_context expr file = { file with local_context = Some expr }
let set_global_context expr file = { file with global_context = Some expr }
let set_goal index rel cost file = { file with index; rel; cost }

let to_string (file : t) : string =
  let opt_to_string ~prefix f = function
    | None -> ""
    | Some v -> Printf.sprintf "%s: %s;\n" prefix (f v)
  in

  let var_list_to_string vars =
    vars |> List.map Variable.name |> String.concat ", "
    |> Printf.sprintf "[%s]"
  in

  let dim3_to_string dim =
    let open Dim3 in
    Printf.sprintf "{x: %d, y: %d, z: %d}" dim.x dim.y dim.z
  in

  let fields =
    [
      opt_to_string ~prefix:"threads_per_warp" string_of_int
        file.threads_per_warp;
      opt_to_string ~prefix:"block_dim" dim3_to_string file.block_dim;
      opt_to_string ~prefix:"locals" var_list_to_string file.locals;
      opt_to_string ~prefix:"local_context" b_to_string file.local_context;
      opt_to_string ~prefix:"global_context" b_to_string file.global_context;
    ]
    |> List.filter (fun s -> s <> "")
    |> String.concat ""
  in

  let theorem =
    Printf.sprintf "ua(%s) %s %s" (n_to_string file.index)
      (N_rel.to_string file.rel) (n_to_string file.cost)
  in

  fields ^ theorem
