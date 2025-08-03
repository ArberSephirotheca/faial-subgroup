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
    ~(all : bool) ~(filename : string) ~(mode : BenchmarkMode.t)
    ~(block_dim : Dim3.t) =
  let strategies = if all then Constraints.values else [ strategy ] in
  let cfg = make_config threads_per_warp block_dim in
  let theorem : Theorem.t = load_theorem_from_file filename cfg in
  Printf.printf "Loaded theorem:\n%s\n" (Theorem.to_string theorem);
  print_endline "=========================================";
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
let main (strategy : Constraints.t) (threads_per_warp : int) (filename : string)
    (all : bool) (mode : BenchmarkMode.t) (block_dim : Dim3.t) : unit =
  run_benchmarks ~strategy ~all ~threads_per_warp ~filename ~mode ~block_dim

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

let filename_arg =
  let doc = "Theorem file to load and benchmark" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"THEOREM_FILE" ~doc)

let all_arg =
  let doc = "Run all constraint versions" in
  Arg.(value & flag & info [ "all" ] ~doc)

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
  let doc = "Benchmark constraint generation strategies on theorem files" in
  let info = Cmd.info "benchmark_constraints" ~doc in
  Cmd.v info
    Term.(
      const main $ strategy_arg $ threads_arg $ filename_arg $ all_arg
      $ mode_arg $ block_dim_arg)

(* Main entry point *)
let () = Cmd.eval main_cmd |> exit
