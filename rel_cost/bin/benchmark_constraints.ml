open Rel_cost
open Protocols
open Exp
open Cmdliner
open Symbolic_metric_analysis

(* Factory function for Config objects *)
let make_config (threads_per_warp : int) : Config.t =
  Config.make ~bank_count:1 ~threads_per_warp
    ~block_dim:(Dim3.make ~x:threads_per_warp ())
    ~grid_dim:(Dim3.make ~x:1 ()) ()

let _thm1 (cfg : Config.t) : Theorem.t =
  let k = 2 in
  {
    cfg;
    locals = Variable.Set.empty;
    thread_context = b_true;
    global_context = b_true;
    index = n_mult (Num k) (Var Variable.tid_x);
    comparison = Comparison.Equal;
    expected_cost = Num k;
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
    comparison = Comparison.Equal;
    expected_cost = x;
  }

(* Benchmark theorem proving *)
let benchmark (generator : Constraints.t) (threads_per_warp : int) =
  let cfg = make_config threads_per_warp in
  let theorem = thm2 cfg in

  let start_time = Unix.gettimeofday () in
  (match Theorem.prove ~generator theorem with
  | ProofResult.Proved -> ()
  | ProofResult.Counterexample model ->
      failwith ("Proof failed with counterexample: " ^ Z3.Model.to_string model)
  | ProofResult.Unknown msg ->
      failwith ("Proof failed with unknown result: " ^ msg));
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

(* Parse strategy from string *)
let strategy_of_string = function
  | "V1" -> Constraints.V1
  | "V2" -> Constraints.V2
  | s -> failwith ("Unknown strategy: " ^ s ^ ". Use V1 or V2.")

(* Main benchmark function *)
let run_benchmark strategy threads_per_warp both =
  let strategies =
    if both then [ ("V1", Constraints.V1); ("V2", Constraints.V2) ]
    else [ (strategy, strategy_of_string strategy) ]
  in

  List.iter
    (fun (name, gen) ->
      Printf.printf "Strategy: %s\n" name;
      let time = benchmark gen threads_per_warp in
      Printf.printf "Time: %.3fs\n\n" time)
    strategies

(* Command line argument definitions *)
let strategy_arg =
  let doc = "Strategy to use (V1 or V2)" in
  Arg.(value & opt string "V2" & info [ "strategy" ] ~doc)

let threads_arg =
  let doc = "Threads per warp" in
  Arg.(value & opt int 32 & info [ "threads" ] ~doc)

let both_arg =
  let doc = "Run both V1 and V2 strategies" in
  Arg.(value & flag & info [ "both" ] ~doc)

(* Command definition *)
let benchmark_cmd =
  let doc = "Benchmark constraint generation strategies" in
  let info = Cmd.info "benchmark_constraints" ~doc in
  Cmd.v info Term.(const run_benchmark $ strategy_arg $ threads_arg $ both_arg)

(* Main entry point *)
let () = Cmd.eval benchmark_cmd |> exit
