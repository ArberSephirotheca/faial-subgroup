open Rel_cost
open Protocols
open Exp
open Cmdliner
open Symbolic_metric_analysis

(* Factory function for Config objects *)
let make_config (threads_per_warp : int) (block_dim : Dim3.t) : Config.t =
  Config.make ~threads_per_warp ~block_dim ~grid_dim:(Dim3.make ~x:1 ()) ()

let thm1 (cfg : Config.t) : Theorem.t =
  (* cost (2 tid) = 2 *)
  {
    cfg;
    locals = Variable.Set.empty;
    thread_context = b_true;
    global_context = b_true;
    index = n_mult (Num 2) (Var Variable.tid_x);
    (* 2 * tid *)
    rel = N_rel.Eq;
    expected_cost = Num 2;
  }

let thm2 (cfg : Config.t) : Theorem.t =
  let x = Var (Variable.from_name "x") in
  let opt = b_and (n_ge x (Num 1)) (n_le x (Num 10)) in
  {
    cfg;
    locals = Variable.Set.empty;
    thread_context = b_true;
    global_context = opt;
    index = n_mult x (Var Variable.tid_x);
    rel = N_rel.Eq;
    expected_cost = x;
  }

let thm3 (cfg : Config.t) : Theorem.t =
  let x = Var (Variable.from_name "x") in
  let opt =
    b_and_ex
      [
        n_ge x (Num 1);
        n_le x (Num 32);
        (*n_eq (Var (Variable.from_name "$warp_id")) (Num 0);*)
      ]
  in
  {
    cfg;
    locals = Variable.Set.empty;
    thread_context = b_true;
    global_context = opt;
    index = n_mult x (Var Variable.tid_x);
    rel = N_rel.Eq;
    expected_cost = x;
  }

(* List of available theorems with titles *)
let theorems : (string * (Config.t -> Theorem.t)) list =
  [
    ("cost(2 * threadIdx.x) = 2", thm1);
    ("cost(x * threadIdx.x) = x where 1 ≤ x ≤ 10", thm2);
    ("cost(x * threadIdx.x) = x where 1 ≤ x ≤ 10 && warp_id = 0", thm3);
  ]

let get_theorem (n : int) : Config.t -> Theorem.t =
  if n >= 1 && n <= List.length theorems then List.nth theorems (n - 1) |> snd
  else failwith (Printf.sprintf "Invalid theorem number %d" n)

(* Print all theorems *)
let list_theorems () =
  Printf.printf "Available theorems:\n\n";
  List.iteri
    (fun i (title, thm_fn) ->
      let cfg = make_config 32 (Dim3.make ~x:32 ()) in
      (* Use default config for display *)
      let theorem = thm_fn cfg in
      Printf.printf "%d. %s\n" (i + 1) title;
      Printf.printf "   %s\n\n" (Theorem.to_string theorem))
    theorems

module BenchmarkMode = struct
  type t = Prove | Optimize

  let to_string : t -> string = function
    | Prove -> "prove"
    | Optimize -> "optimize"

  let of_string : string -> t option = function
    | "prove" | "p" -> Some Prove
    | "optimize" | "o" -> Some Optimize
    | _ -> None

  let default : t = Prove
end

let time_it (f : unit -> unit) : float =
  let start_time = Unix.gettimeofday () in
  f ();
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

(* Benchmark theorem proving *)
let benchmark_prove ~generator ~theorem =
  time_it (fun () ->
      match Theorem.prove ~generator theorem with
      | ProofResult.Proved -> ()
      | ProofResult.Counterexample model ->
          print_endline
            ("Proof failed with counterexample:\n" ^ Z3.Model.to_string model)
      | ProofResult.Unknown msg ->
          print_endline ("Proof failed with unknown result: " ^ msg))

(* Benchmark cost optimization *)
let benchmark_optimize ~generator ~theorem =
  time_it (fun () ->
      match Theorem.optimize_cost ~generator theorem with
      | Some cost -> Printf.printf "Optimized cost: %d\n" cost
      | None -> print_endline "Optimization failed (no solution found)")

let run_benchmarks ~(strategy : Constraints.t) ~(threads_per_warp : int)
    ~(all : bool) ~(theorem : int) ~(mode : BenchmarkMode.t)
    ~(block_dim : Dim3.t) =
  let strategies = if all then Constraints.values else [ strategy ] in
  let cfg = make_config threads_per_warp block_dim in
  let theorem : Theorem.t = get_theorem theorem cfg in
  List.iter
    (fun generator ->
      Printf.printf "Strategy: %s\n" (Constraints.to_string generator);
      let time =
        match mode with
        | BenchmarkMode.Prove -> benchmark_prove ~generator ~theorem
        | BenchmarkMode.Optimize -> benchmark_optimize ~generator ~theorem
      in
      Printf.printf "Time: %.3fs\n\n" time)
    strategies

(* Main benchmark function *)
let main (strategy : Constraints.t) (threads_per_warp : int) (theorem : int)
    (all : bool) (list_thms : bool) (mode : BenchmarkMode.t)
    (block_dim : Dim3.t) : unit =
  if list_thms then list_theorems ()
  else run_benchmarks ~strategy ~all ~threads_per_warp ~theorem ~mode ~block_dim

let constraints_conv : Constraints.t Arg.conv =
  let parse s =
    match Constraints.of_string s with
    | Some v -> Ok v
    | None ->
        let valid_options =
          Constraints.values
          |> List.map Constraints.to_string
          |> String.concat ", "
        in
        Error
          (`Msg
             (Printf.sprintf "Invalid constraint '%s'. Valid options are: %s" s
                valid_options))
  in
  let print fmt v = Format.fprintf fmt "%s" (Constraints.to_string v) in
  Arg.conv (parse, print)

let strategy_arg =
  let doc =
    let valid_options =
      Constraints.values |> List.map Constraints.to_string |> String.concat ", "
    in
    Printf.sprintf "Constraint type. Valid options: %s" valid_options
  in
  Arg.(
    value
    & opt constraints_conv Constraints.default
    & info [ "c"; "constraint" ] ~doc)

let threads_arg =
  let doc = "Threads per warp" in
  Arg.(value & opt int 32 & info [ "t"; "threads" ] ~doc)

let theorem_arg =
  let doc =
    Printf.sprintf "Theorem number to test (1-%d, default: 2)"
      (List.length theorems)
  in
  Arg.(value & opt int 2 & info [ "T"; "theorem" ] ~doc)

let all_arg =
  let doc = "Run all constraint versions" in
  Arg.(value & flag & info [ "all" ] ~doc)

let list_theorems_arg =
  let doc = "List all available theorems" in
  Arg.(value & flag & info [ "list-theorems" ] ~doc)

let benchmark_mode_conv : BenchmarkMode.t Arg.conv =
  let parse s =
    match BenchmarkMode.of_string s with
    | Some v -> Ok v
    | None ->
        Error
          (`Msg
             (Printf.sprintf
                "Invalid mode '%s'. Valid options are: prove, p, optimize, o" s))
  in
  let print fmt v = Format.fprintf fmt "%s" (BenchmarkMode.to_string v) in
  Arg.conv (parse, print)

let mode_arg =
  let doc = "Benchmark mode. Valid options: prove, p, optimize, o" in
  Arg.(
    value
    & opt benchmark_mode_conv BenchmarkMode.default
    & info [ "m"; "mode" ] ~doc)

let dim3_conv : Dim3.t Arg.conv =
  let parse s = Dim3.parse s |> Result.map_error (fun x -> `Msg x) in
  let print fmt v = Format.fprintf fmt "%s" (Dim3.to_string v) in
  Arg.conv (parse, print)

let block_dim_arg =
  let doc = "Block dimensions in format 'x,y,z' or 'x'" in
  Arg.(value & opt dim3_conv (Dim3.make ~x:32 ()) & info [ "block-dim" ] ~doc)

(* Command definition *)
let main_cmd =
  let doc = "Benchmark constraint generation strategies" in
  let info = Cmd.info "benchmark_constraints" ~doc in
  Cmd.v info
    Term.(
      const main $ strategy_arg $ threads_arg $ theorem_arg $ all_arg
      $ list_theorems_arg $ mode_arg $ block_dim_arg)

(* Main entry point *)
let () = Cmd.eval main_cmd |> exit
