open Rel_cost
open Protocols
open Protocols.Gen_z3
open Exp
open Cmdliner
open Symbolic_metric_analysis
open Protocols_parsing.Parsers

(* Factory function for Config objects *)
let make_config (threads_per_warp : int) (block_dim : Dim3.t) : Config.t =
  Config.make ~threads_per_warp ~block_dim ~grid_dim:(Dim3.make ~x:1 ()) ()

(* Use thm2 as our test case *)
let thm2 (cfg : Config.t) : Theorem.t =
  let x = Var (Variable.from_name "x") in
  let opt = b_and (n_ge x (Num 1)) (n_le x (Num 16)) in
  {
    cfg;
    locals = Variable.Set.empty;
    active_threads = b_true;
    assumptions = opt;
    goals = [ Prop (Exp.n_eq (n_mult x (Var Variable.tid_x)) x) ];
  }

let parse (s : string) : Tactic.t option =
  Some (match TacticParser.of_string s with Ok e -> e | Error e -> failwith e)

(* Define different tactic strategies to benchmark *)
module TacticStrategy = struct
  type t = { name : string; description : string; tactic : Tactic.t option }

  let baseline =
    {
      name = "baseline";
      description = "Default solver (no tactics) - baseline for comparison";
      tactic = None;
    }

  let default_smt =
    {
      name = "smt";
      description = "Default SMT solver";
      tactic = parse {| smt; |};
    }

  let bit_vector =
    {
      name = "bit-blast";
      description = "Bit-vector approach with bit-blasting";
      tactic =
        parse {|
        simplify;
        bit-blast;
        sat;
      |};
    }

  let lia_solver =
    {
      name = "lia";
      description = "Linear Integer Arithmetic solver";
      tactic = parse {|
        simplify;
        lia;
      |};
    }

  let equation_solving =
    {
      name = "solve-eqs";
      description = "Equation solving followed by SMT";
      tactic = parse {|
        solve-eqs;
        smt;
      |};
    }

  let timeout_protected =
    {
      name = "timeout-smt";
      description = "SMT with 10s timeout";
      tactic = parse {|
        timeout 10 s { smt; }
      |};
    }

  let comprehensive =
    {
      name = "comprehensive";
      description = "Simplify -> solve equations -> bit-blast -> SAT";
      tactic =
        parse
          {|
        simplify;
        solve-eqs;
        bit-blast;
        sat;
      |};
    }

  let parallel_attempt =
    {
      name = "parallel-or";
      description = "Try LIA and bit-blasting in parallel";
      tactic =
        parse
          {|
        par {
          lia;
          seq {
            bit-blast;
            sat;
          }
        }
      |};
    }

  let z3_default_simplified =
    {
      name = "z3-default-simplified";
      description = "Simplified version of Z3's default tactic with probes";
      tactic =
        parse
          {|
        simplify;
        if (is-qfbv) {
          qfbv;
        } else if (is-qflia) {
          qflia;
        } else {
          smt;
        }
      |};
    }

  let o1 =
    {
      name = "o1";
      description = "Simplified version of Z3's default tactic with probes";
      tactic =
        parse
          {|
                      solve-eqs;
                       bit-blast;
                       aig;
                       sat;
      |};
      (*
              if (is-qfbv) {
          qfbv;
        } else if (is-qflia) {
          qflia;
        } else {
          simplify;
          bit-blast;
          sat;
        }
*)
    }

  let t1 =
    {
      name = "t1";
      description = "Simplified version of Z3's default tactic with probes";
      tactic =
        parse
          {|
        par {
          lia;
          seq {
            bit-blast;
            sat;
          }
        }
      |};
    }

  let tuned_smt =
    {
      name = "tuned-smt";
      description = "SMT with performance tuning parameters";
      tactic =
        parse
          {|
        with (restart.max: 100) {
          smt;
        }
      |};
    }

  let safe_bit_blast =
    {
      name = "safe-bit-blast";
      description = "Bit-blast only when problem is bit-vector";
      tactic =
        parse
          {|
        if (is-qfbv) {
          bit-blast;
          sat;
        }
      |};
    }

  let adaptive_solver =
    {
      name = "adaptive";
      description = "Adaptive solver that routes based on problem type";
      tactic =
        parse
          {|
        simplify;
        if (is-propositional && ! produce-proofs) {
          sat;
        } else if (is-qfbv) {
          bit-blast;
          sat;
        } else par {
          qflia;
          stmt;
        }
      |};
    }

  let all =
    [
      baseline;
      default_smt;
      bit_vector;
      lia_solver;
      equation_solving;
      timeout_protected;
      comprehensive;
      parallel_attempt;
      z3_default_simplified;
      tuned_smt;
      safe_bit_blast;
      adaptive_solver;
      o1;
      t1;
    ]
end

let time_it (f : unit -> 'a) : float * 'a =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  (end_time -. start_time, result)

(* Solver backend selection *)
module SolverBackend = struct
  type t = IntGen | Bv32Gen | Bv64Gen

  let to_string : t -> string = function
    | IntGen -> "int"
    | Bv32Gen -> "bv32"
    | Bv64Gen -> "bv64"

  let of_string : string -> t option = function
    | "int" | "IntGen" -> Some IntGen
    | "bv32" | "Bv32Gen" -> Some Bv32Gen
    | "bv64" | "Bv64Gen" -> Some Bv64Gen
    | _ -> None

  let to_module : t -> (module Gen_z3.Z3_SOLVER) = function
    | IntGen -> (module Gen_z3.IntGen)
    | Bv32Gen -> (module Gen_z3.Bv32Gen)
    | Bv64Gen -> (module Gen_z3.Bv64Gen)

  let default : t = Bv64Gen
  let all : t list = [ IntGen; Bv32Gen; Bv64Gen ]
end

(* Benchmark a single tactic strategy using Theorem.execute *)
let benchmark_tactic ~(solver_backend : SolverBackend.t)
    ~(strategy : TacticStrategy.t) ~(theorem : Theorem.t) =
  let solver_module = SolverBackend.to_module solver_backend in
  let time, result =
    time_it (fun () ->
        Theorem.execute ~solver:solver_module ~debug:false
          ~tactic:strategy.tactic theorem)
  in
  (time, result)

let run_benchmarks ~(solver_backend : SolverBackend.t)
    ~(strategies : TacticStrategy.t list) ~(threads_per_warp : int)
    ~(block_dim : Dim3.t) =
  let cfg = make_config threads_per_warp block_dim in
  let theorem = thm2 cfg in

  Printf.printf "Benchmarking tactics on: %s\n" (Theorem.to_string theorem);
  Printf.printf "Solver backend: %s, Threads per warp: %d, Block dim: %s\n\n"
    (SolverBackend.to_string solver_backend)
    threads_per_warp (Dim3.to_string block_dim);

  List.iter
    (fun strategy ->
      let open TacticStrategy in
      Printf.printf "\nStrategy: %s\n" strategy.name;
      Printf.printf "Description: %s\n" strategy.description;
      Printf.printf "Tactic: %s\n"
        (match strategy.tactic with
        | Some tactic -> Tactic.to_string tactic
        | None -> "Default solver (no tactics)");
      flush stdout;

      try
        let time, results =
          benchmark_tactic ~solver_backend ~strategy ~theorem
        in
        Printf.printf "Time: %.3fs\n" time;
        Printf.printf "Results:\n";
        List.iteri
          (fun i result ->
            match result with
            | Ok res ->
                Printf.printf "  Goal %d: %s\n" i (TheoremResult.to_string res)
            | Error msg -> Printf.printf "  Goal %d: ERROR - %s\n" i msg)
          results
      with exn ->
        Printf.printf "FAILED: %s\n" (Printexc.to_string exn);

        Printf.printf "\n")
    strategies

let list_strategies () =
  Printf.printf "Available tactic strategies:\n\n";
  List.iteri
    (fun i strategy ->
      Printf.printf "%d. %s\n" (i + 1) strategy.TacticStrategy.name;
      Printf.printf "   %s\n" strategy.TacticStrategy.description;
      Printf.printf "   Tactic: %s\n\n"
        (match strategy.TacticStrategy.tactic with
        | Some tactic -> Tactic.to_string tactic
        | None -> "Default solver (no tactics)"))
    TacticStrategy.all

(* Main function *)
let main (strategy_name : string option) (threads_per_warp : int) (all : bool)
    (list_strategies_flag : bool) (solver_backend : SolverBackend.t)
    (block_dim : Dim3.t) : unit =
  if list_strategies_flag then list_strategies ()
  else
    let strategies =
      if all then TacticStrategy.all
      else
        match strategy_name with
        | Some name -> (
            match
              List.find_opt
                (fun s -> s.TacticStrategy.name = name)
                TacticStrategy.all
            with
            | Some strategy -> [ strategy ]
            | None ->
                Printf.eprintf
                  "Unknown strategy '%s'. Use --list-strategies to see \
                   available options.\n"
                  name;
                exit 1)
        | None -> [ TacticStrategy.baseline ]
    in
    run_benchmarks ~solver_backend ~strategies ~threads_per_warp ~block_dim

(* Command line arguments *)
let strategy_arg =
  let doc = "Tactic strategy to use (use --list-strategies to see options)" in
  Arg.(value & opt (some string) None & info [ "s"; "strategy" ] ~doc)

let threads_arg =
  let doc = "Threads per warp" in
  Arg.(value & opt int 32 & info [ "t"; "threads" ] ~doc)

let all_arg =
  let doc = "Run all tactic strategies" in
  Arg.(value & flag & info [ "all" ] ~doc)

let list_strategies_arg =
  let doc = "List all available tactic strategies" in
  Arg.(value & flag & info [ "list-strategies" ] ~doc)

let solver_backend_conv : SolverBackend.t Arg.conv =
  let parse s =
    match SolverBackend.of_string s with
    | Some v -> Ok v
    | None ->
        let valid_options =
          SolverBackend.all
          |> List.map SolverBackend.to_string
          |> String.concat ", "
        in
        Error
          (`Msg
             (Printf.sprintf "Invalid solver '%s'. Valid options are: %s" s
                valid_options))
  in
  let print fmt v = Format.fprintf fmt "%s" (SolverBackend.to_string v) in
  Arg.conv (parse, print)

let solver_backend_arg =
  let doc =
    let valid_options =
      SolverBackend.all
      |> List.map SolverBackend.to_string
      |> String.concat ", "
    in
    Printf.sprintf "Z3 solver backend. Valid options: %s" valid_options
  in
  Arg.(
    value
    & opt solver_backend_conv SolverBackend.default
    & info [ "solver"; "backend" ] ~doc)

let dim3_conv : Dim3.t Arg.conv =
  let parse s = Dim3.parse s |> Result.map_error (fun x -> `Msg x) in
  let print fmt v = Format.fprintf fmt "%s" (Dim3.to_string v) in
  Arg.conv (parse, print)

let block_dim_arg =
  let doc = "Block dimensions in format 'x,y,z' or 'x'" in
  Arg.(value & opt dim3_conv (Dim3.make ~x:32 ()) & info [ "block-dim" ] ~doc)

(* Command definition *)
let main_cmd =
  let doc = "Benchmark Z3 tactic strategies on theorem proving" in
  let info = Cmd.info "benchmark_tactics" ~doc in
  Cmd.v info
    Term.(
      const main $ strategy_arg $ threads_arg $ all_arg $ list_strategies_arg
      $ solver_backend_arg $ block_dim_arg)

(* Main entry point *)
let () = Cmd.eval main_cmd |> exit
