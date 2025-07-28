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

let thm1 (cfg : Config.t) : Theorem.t =
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

(* List of available theorems with titles *)
let theorems : (string * (Config.t -> Theorem.t)) list = [
  ("cost(2 * threadIdx.x) = 2", thm1);
  ("cost(x * threadIdx.x) = x where 1 ≤ x ≤ 10", thm2);
]

let get_theorem (n : int) : Config.t -> Theorem.t =
  if n >= 1 && n <= List.length theorems then
    List.nth theorems (n - 1) |> snd
  else
    failwith (Printf.sprintf "Invalid theorem number %d" n)

(* Print all theorems *)
let list_theorems () =
  Printf.printf "Available theorems:\n\n";
  List.iteri (fun i (title, thm_fn) ->
    let cfg = make_config 32 in (* Use default config for display *)
    let theorem = thm_fn cfg in
    Printf.printf "%d. %s\n" (i + 1) title;
    Printf.printf "   %s\n\n" (Theorem.to_string theorem)
  ) theorems

let time_it (f: unit -> unit) : float =
  let start_time = Unix.gettimeofday () in
  f ();
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

(* Benchmark theorem proving *)
let benchmark ~generator ~theorem =
  time_it (fun () ->
    match Theorem.prove ~generator theorem with
    | ProofResult.Proved -> ()
    | ProofResult.Counterexample model ->
        print_endline ("Proof failed with counterexample:\n" ^ Z3.Model.to_string model)
    | ProofResult.Unknown msg ->
        print_endline ("Proof failed with unknown result: " ^ msg)
  )

let run_benchmarks ~(strategy : Constraints.t) ~(threads_per_warp : int) ~(all : bool) ~(theorem : int) =
  let strategies =
    if all then Constraints.values
    else [ strategy ]
  in
  let cfg = make_config threads_per_warp in
  let theorem : Theorem.t = get_theorem theorem cfg in
  List.iter
    (fun generator ->
      Printf.printf "Strategy: %s\n" (Constraints.to_string generator);
      let time = benchmark ~generator ~theorem in
      Printf.printf "Time: %.3fs\n\n" time)
    strategies

(* Main benchmark function *)
let main (strategy:Constraints.t) (threads_per_warp:int) (theorem:int) (all:bool) (list_thms:bool) : unit =
  if list_thms then
    list_theorems ()
  else
    run_benchmarks ~strategy ~all ~threads_per_warp ~theorem

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
        Error (`Msg (Printf.sprintf "Invalid constraint '%s'. Valid options are: %s" s valid_options))
  in
  let print fmt v = Format.fprintf fmt "%s" (Constraints.to_string v) in
  Arg.conv (parse, print)

let strategy_arg =
  let doc =
    let valid_options =
      Constraints.values
      |> List.map Constraints.to_string
      |> String.concat ", "
    in
    Printf.sprintf "Constraint type. Valid options: %s" valid_options
  in
  Arg.(value & opt constraints_conv Constraints.default & info ["c"; "constraint"] ~doc)

let threads_arg =
  let doc = "Threads per warp" in
  Arg.(value & opt int 32 & info [ "t"; "threads" ] ~doc)

let theorem_arg =
  let doc = Printf.sprintf "Theorem number to test (1-%d, default: 2)" (List.length theorems) in
  Arg.(value & opt int 2 & info [ "T"; "theorem" ] ~doc)

let all_arg =
  let doc = "Run all constraint versions" in
  Arg.(value & flag & info [ "all" ] ~doc)

let list_theorems_arg =
  let doc = "List all available theorems" in
  Arg.(value & flag & info [ "list-theorems" ] ~doc)

(* Command definition *)
let main_cmd =
  let doc = "Benchmark constraint generation strategies" in
  let info = Cmd.info "benchmark_constraints" ~doc in
  Cmd.v info Term.(const main $ strategy_arg $ threads_arg $ theorem_arg $ all_arg $ list_theorems_arg)

(* Main entry point *)
let () = Cmd.eval main_cmd |> exit
