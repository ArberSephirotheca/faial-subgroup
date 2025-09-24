open Rel_cost
open Protocols
open Cmdliner
open Symbolic_metric_analysis
open Rel_cost_parsing.Parsers
open Rel_cost_parsing

(* Factory function for Config objects *)
let make_config (threads_per_warp : int) (block_dim : Dim3.t) : Config.t =
  Config.make ~threads_per_warp ~block_dim ~grid_dim:(Dim3.make ~x:1 ()) ()

(* Load and parse theorem from file *)
let load_theorem_from_file (filename : string) (cfg : Config.t) : Theorem.t =
  match TheoremFileParser.of_filename filename with
  | Ok theorem_file ->
      print_endline ("Parsed:\n" ^ Theorem_file.to_string theorem_file);
      print_endline "==========================";
      let thm = Rel_cost_parsing.Theorem_file.to_theorem cfg theorem_file in
      thm
  | Error msg ->
      failwith
        (Printf.sprintf "Failed to parse theorem file '%s': %s" filename msg)

module RunMode = struct
  type t = Prove | Max | Min

  let to_string : t -> string = function
    | Prove -> "prove"
    | Max -> "max"
    | Min -> "min"

  let of_string : string -> t option = function
    | "prove" | "p" -> Some Prove
    | "max" | "maximize" -> Some Max
    | "min" | "minimize" -> Some Min
    | _ -> None

  let default : t = Prove
end

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

let time_it (f : unit -> unit) : float =
  let start_time = Unix.gettimeofday () in
  f ();
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

(* Load and parse tactic from file *)
let load_tactic_from_file = function
  | None -> None
  | Some filename -> (
      match Protocols_parsing.Parsers.TacticParser.of_filename filename with
      | Ok tactic -> Some tactic
      | Error msg ->
          failwith
            (Printf.sprintf "Failed to parse tactic file '%s': %s" filename msg)
      )

(* Benchmark theorem proving *)
let benchmark_prove ~generator ~tactic ~debug ~solver ~theorem =
  time_it (fun () ->
      match Theorem.prove ~generator ~tactic ~debug ~solver theorem with
      | ProofResult.Proved -> ()
      | ProofResult.Counterexample model ->
          print_endline
            ("Proof failed with counterexample:\n" ^ Z3.Model.to_string model)
      | ProofResult.Unknown msg ->
          print_endline ("Proof failed with unknown result: " ^ msg))

(* Benchmark cost optimization *)
let benchmark_optimize ~strategy ~generator ~theorem =
  time_it (fun () ->
      match Theorem.optimize_cost ~strategy ~generator theorem with
      | Some cost -> Printf.printf "Optimized cost: %d\n" cost
      | None -> print_endline "Optimization failed (no solution found)")

let run_benchmarks ~(strategy : Constraints.t) ~(threads_per_warp : int)
    ~(all : bool) ~(filename : string) ~(mode : RunMode.t)
    ~(tactic_file : string option) ~(debug : bool)
    ~(solver_backend : SolverBackend.t) ~(block_dim : Dim3.t) =
  let strategies = if all then Constraints.values else [ strategy ] in
  let cfg = make_config threads_per_warp block_dim in
  let theorem : Theorem.t = load_theorem_from_file filename cfg in

  (* Validate tactic file usage *)
  (match (mode, tactic_file) with
  | (RunMode.Max | RunMode.Min), Some _ ->
      Printf.printf
        "Warning: Tactic file ignored for %s mode (tactics only supported in \
         prove mode)\n\n"
        (RunMode.to_string mode)
  | RunMode.Prove, Some tfile -> Printf.printf "Using tactic file: %s\n" tfile
  | _ -> ());

  let tactic =
    match mode with
    | RunMode.Prove -> load_tactic_from_file tactic_file
    | _ -> None
  in

  let solver_module = SolverBackend.to_module solver_backend in

  Printf.printf "Loaded theorem:\n%s\n" (Theorem.to_string theorem);
  Printf.printf "Using solver: %s\n" (SolverBackend.to_string solver_backend);
  print_endline "=========================================";
  List.iter
    (fun generator ->
      Printf.printf "Strategy: %s\n" (Constraints.to_string generator);
      let time =
        match mode with
        | RunMode.Prove ->
            benchmark_prove ~generator ~tactic ~debug ~solver:solver_module
              ~theorem
        | RunMode.Max ->
            benchmark_optimize ~strategy:Gen_z3.Optimizer.Strategy.Maximize
              ~generator ~theorem
        | RunMode.Min ->
            benchmark_optimize ~strategy:Gen_z3.Optimizer.Strategy.Minimize
              ~generator ~theorem
      in
      Printf.printf "Time: %.3fs\n\n" time)
    strategies

(* Main benchmark function *)
let main (strategy : Constraints.t) (threads_per_warp : int) (filename : string)
    (all : bool) (mode : RunMode.t) (tactic_file : string option) (debug : bool)
    (solver_backend : SolverBackend.t) (block_dim : Dim3.t) : unit =
  run_benchmarks ~strategy ~all ~threads_per_warp ~filename ~mode ~tactic_file
    ~debug ~solver_backend ~block_dim

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
    & info [ "v"; "constraint" ] ~doc)

let threads_arg =
  let doc = "Threads per warp" in
  Arg.(value & opt int 32 & info [ "t"; "threads" ] ~doc)

let filename_arg =
  let doc = "Theorem file to load and benchmark" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"THEOREM_FILE" ~doc)

let tactic_file_arg =
  let doc = "Optional tactic file for prove mode (ignored for max/min modes)" in
  Arg.(value & opt (some file) None & info [ "tactics" ] ~doc)

let all_arg =
  let doc = "Run all constraint versions" in
  Arg.(value & flag & info [ "all" ] ~doc)

let debug_arg =
  let doc = "Enable debug output from Z3 solver" in
  Arg.(value & flag & info [ "debug" ] ~doc)

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

let solver_arg =
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
    & info [ "s"; "solver" ] ~doc)

let benchmark_mode_conv : RunMode.t Arg.conv =
  let parse s =
    match RunMode.of_string s with
    | Some v -> Ok v
    | None ->
        Error
          (`Msg
             (Printf.sprintf
                "Invalid mode '%s'. Valid options are: prove, p, max, \
                 maximize, min, minimize"
                s))
  in
  let print fmt v = Format.fprintf fmt "%s" (RunMode.to_string v) in
  Arg.conv (parse, print)

let mode_arg =
  let doc =
    "Benchmark mode. Valid options: prove, p, max, maximize, min, minimize"
  in
  Arg.(
    value & opt benchmark_mode_conv RunMode.default & info [ "m"; "mode" ] ~doc)

let dim3_conv : Dim3.t Arg.conv =
  let parse s = Dim3.parse s |> Result.map_error (fun x -> `Msg x) in
  let print fmt v = Format.fprintf fmt "%s" (Dim3.to_string v) in
  Arg.conv (parse, print)

let block_dim_arg =
  let doc = "Block dimensions in format 'x,y,z' or 'x'" in
  Arg.(value & opt dim3_conv (Dim3.make ~x:32 ()) & info [ "block-dim" ] ~doc)

(* Command definition *)
let main_cmd =
  let doc =
    "Prove cost theorems and optimize constraint generation strategies"
  in
  let info = Cmd.info "faial-cost-prover" ~doc in
  Cmd.v info
    Term.(
      const main $ strategy_arg $ threads_arg $ filename_arg $ all_arg
      $ mode_arg $ tactic_file_arg $ debug_arg $ solver_arg $ block_dim_arg)

(* Main entry point *)
let () = Cmd.eval main_cmd |> exit
