open Stage0
open Protocols
open Inference
open Rel_cost
open Vectors

module Data = struct
  type t = Int of int | Vector of int list

  let from_json (j : Yojson.Basic.t) : t option =
    let open Yojson.Basic.Util in
    match j with
    | `Int i -> Some (Int i)
    | `List l -> (
        try Some (Vector (List.filter_map Yojson.Basic.Util.to_int_option l))
        with Type_error _ -> None)
    | _ -> None

  let to_nmap ~count : t -> NMap.t option = function
    | Int value -> Some (NMap.constant ~count ~value)
    | Vector v ->
        if count <> List.length v then None
        else Some (NMap.from_array (Array.of_list v))

  let to_string : t -> string = function
    | Int i -> string_of_int i
    | Vector l ->
        let l = List.map string_of_int l |> String.concat ", " in
        "[" ^ l ^ "]"
end

module Env = struct
  type t = (string * Data.t) list

  let lookup (var : string) (e : t) : Data.t option = List.assoc_opt var e

  let lookup_scalar (var : string) (e : t) : int option =
    match lookup var e with Some (Int n) -> Some n | _ -> None

  let lookup_dim3 (prefix : string) (e : t) : Dim3.t option =
    let ( let* ) = Option.bind in
    let field k = lookup_scalar (prefix ^ "." ^ k) e in
    let* x = field "x" in
    let* y = field "y" in
    let* z = field "z" in
    Some Dim3.{ x; y; z }

  let load (fname : string) : t =
    try
      let open Yojson.Basic in
      let j = from_file ~fname fname in
      j |> Util.to_assoc
      |> List.filter_map (fun (k, v) ->
          v |> Data.from_json |> Option.map (fun v -> (k, v)))
    with Yojson.Json_error e | Sys_error e ->
      prerr_endline ("Error parsing '" ^ fname ^ "': " ^ e);
      []

  let to_string (env : t) : string =
    let env =
      env
      |> List.map (fun (x, y) -> x ^ "=" ^ Data.to_string y)
      |> String.concat ", "
    in
    "{" ^ env ^ "}"
end

module Runner = struct
  type t = { config : Config.t; vec : Vectorized.t }

  let to_string (r : t) : string =
    Printf.sprintf "{\n  config=%s,\n  vec=\n%s\n}"
      (Config.to_string r.config)
      (Vectorized.to_string r.vec)

  let make ?(threads_per_warp = 32) ?(bank_count = 32) (env : Env.t) : t =
    let block_dim =
      Env.lookup_dim3 "blockDim" env
      |> Option.value ~default:Dim3.{ x = 32; y = 1; z = 1 }
    in
    let grid_dim =
      Env.lookup_dim3 "gridDim" env
      |> Option.value ~default:Dim3.{ x = 1; y = 1; z = 1 }
    in
    let config : Config.t =
      Config.make ~bank_count ~block_dim ~grid_dim ~threads_per_warp ()
    in
    let vec : Vectorized.t =
      let ctx = Vectorized.make config |> Vectorized.put_tids block_dim in
      List.fold_left
        (fun ctx ((k : string), v) ->
          let k = Variable.from_name k in
          match Data.to_nmap v ~count:bank_count with
          | Some v' -> Vectorized.put k v' ctx
          | None -> ctx)
        ctx env
    in
    { config; vec }

  let run (m : Metric.t) (k : Kernel.t) (e : t) : int =
    let linearize = Linearize_index.Default.linearize e.config k.arrays in
    e.vec |> Vectorized.restrict k.pre
    |> Vectorized.eval ~verbose:true m linearize k.code
end

let main (fname : string) (m : Metric.t) : unit =
  try
    let parsed = Protocol_parser.Default.to_proto fname in
    let proto = parsed.kernels in
    let env = Env.load "env.json" in
    print_endline ("env.json: " ^ Env.to_string env);
    let r = Runner.make env in
    print_endline ("Runner: " ^ Runner.to_string r);
    try
      List.iter
        (fun k ->
          print_endline (Kernel.to_string k);
          let v = Runner.run m k r in
          print_endline ("Total cost: " ^ string_of_int v))
        proto;
      ()
    with Failure x -> print_endline ("Dynamic analysis failed: " ^ x)
  with Common.ParseError b ->
    Buffer.output_buffer stderr b;
    exit (-1)

(* Command-line interface *)

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let metric =
  let doc = "Select the metric to measure the cost." in
  Arg.(
    required
    & opt (some (enum Metric.choices)) None
    & info [ "m"; "metric" ] ~doc)

let main_t = Term.(const main $ get_fname $ metric)

let info =
  let doc = "Dynamic performance analysis for GPU programs" in
  Cmd.info "faial-bc-dyn" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
