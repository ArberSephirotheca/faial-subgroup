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

(* Check if a variable name is thread-indexed (ends with $digit) *)
let is_thread_indexed (var_name : string) : bool =
  let parts = String.split_on_char '$' var_name in
  match parts with
  | [ _; suffix ] -> (
      try
        ignore (int_of_string suffix);
        true
      with _ -> false)
  | _ -> false

(* Extract base name and thread index from thread-indexed variable *)
let parse_thread_var (var_name : string) : (string * int) option =
  let parts = String.split_on_char '$' var_name in
  match parts with
  | [ base; suffix ] -> (
      try Some (base, int_of_string suffix) with _ -> None)
  | _ -> None

(* Group variables by base name for thread-indexed variables *)
let group_thread_variables (var_values : (Variable.t * int) list) :
    (string * (int * int) list) list * (Variable.t * int) list =
  let thread_vars = ref [] in
  let regular_vars = ref [] in

  List.iter
    (fun (var, value) ->
      let var_name = Variable.label var in
      if is_thread_indexed var_name then
        match parse_thread_var var_name with
        | Some (base, thread_idx) ->
            thread_vars := (base, thread_idx, value) :: !thread_vars
        | None -> regular_vars := (var, value) :: !regular_vars
      else regular_vars := (var, value) :: !regular_vars)
    var_values;

  (* Group by base name *)
  let grouped =
    List.fold_left
      (fun acc (base, thread_idx, value) ->
        let existing = try List.assoc base acc with Not_found -> [] in
        let updated = (thread_idx, value) :: existing in
        (base, updated) :: List.remove_assoc base acc)
      [] !thread_vars
  in

  (grouped, !regular_vars)

(* Format counterexample with readable decimal values and tabular display *)
let format_counterexample (theorem : Theorem.t) (model : Z3.Model.model) :
    string =
  (* Get all variables from the Z3 model *)
  let all_decls = Z3.Model.get_const_decls model in
  let var_values =
    List.filter_map
      (fun decl ->
        let var = Gen_z3.decl_to_variable decl in
        match Gen_z3.Bv64Gen.get_int_decl model decl with
        | Some value -> Some (var, value)
        | None -> None)
      all_decls
  in

  if List.length var_values = 0 then "No variables found in counterexample"
  else
    let thread_groups, regular_vars = group_thread_variables var_values in
    let threads_per_warp = theorem.cfg.threads_per_warp in

    let format_regular_var (var, value) =
      let var_name = Variable.label var in
      Printf.sprintf "%s = %d" var_name value
    in

    let format_thread_table (base_name, thread_values) =
      (* Sort by thread index *)
      let sorted_values =
        List.sort (fun (i1, _) (i2, _) -> compare i1 i2) thread_values
      in

      (* Create header row with bold variable name *)
      let header_row =
        [
          PrintBox.text "Thread";
          PrintBox.text_with_style PrintBox.Style.bold base_name;
        ]
      in

      (* Create data rows: T1 through T{threads_per_warp} *)
      let data_rows =
        List.init threads_per_warp (fun i ->
            let thread_label = "T" ^ string_of_int (i + 1) in
            (* 1-based display *)
            let value =
              try
                string_of_int (List.assoc i sorted_values) (* 0-based lookup *)
              with Not_found -> ""
            in
            [ PrintBox.text thread_label; PrintBox.text value ])
      in

      (* Combine header and data rows *)
      let all_rows = header_row :: data_rows in
      let box_array = Array.of_list (List.map Array.of_list all_rows) in
      let table = PrintBox.(grid box_array |> frame) in
      PrintBox_text.to_string table
    in

    let regular_output =
      if List.length regular_vars > 0 then
        "Regular variables:\n"
        ^ (List.map format_regular_var regular_vars |> String.concat "\n")
      else ""
    in

    let thread_output =
      if List.length thread_groups > 0 then
        (* Sort thread groups alphabetically by base name *)
        let sorted_groups =
          List.sort
            (fun (name1, _) (name2, _) -> String.compare name1 name2)
            thread_groups
        in
        "Thread-indexed variables:\n\n"
        ^ (List.map format_thread_table sorted_groups |> String.concat "\n\n")
      else ""
    in

    let parts =
      List.filter (fun s -> s <> "") [ regular_output; thread_output ]
    in
    String.concat "\n\n" parts

(* Print theorem execution result with consistent formatting *)
let print_theorem_result (theorem : Theorem.t) = function
  | Ok (TheoremResult.ProofResult ProofResult.Proved) ->
      print_endline "✓ Proof succeeded"
  | Ok (TheoremResult.ProofResult (ProofResult.Counterexample model)) ->
      print_endline
        ("✗ Proof failed with counterexample:\n"
        ^ format_counterexample theorem model)
  | Ok (TheoremResult.OptimizationResult value) ->
      print_endline ("Optimization result: " ^ string_of_int value)
  | Error msg -> print_endline ("Error: " ^ msg)

(* Unified benchmark function for all theorem execution modes *)
let benchmark_execution ~generator ~tactic ~debug ~verbose ~solver ~theorem =
  time_it (fun () ->
      let results =
        Theorem.execute ~generator ~tactic ~debug ~verbose ~solver theorem
      in
      List.iter (print_theorem_result theorem) results)

let run_benchmarks ~(strategy : Constraints.t) ~(threads_per_warp : int)
    ~(all : bool) ~(filename : string) ~(mode : RunMode.t)
    ~(tactic_file : string option) ~(debug : bool) ~(verbose : bool)
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
        benchmark_execution ~generator ~tactic ~debug ~verbose
          ~solver:solver_module ~theorem
      in
      Printf.printf "Time: %.3fs\n\n" time)
    strategies

(* Main benchmark function *)
let main (strategy : Constraints.t) (threads_per_warp : int) (filename : string)
    (all : bool) (mode : RunMode.t) (tactic_file : string option) (debug : bool)
    (verbose : bool) (solver_backend : SolverBackend.t) (block_dim : Dim3.t) :
    unit =
  run_benchmarks ~strategy ~all ~threads_per_warp ~filename ~mode ~tactic_file
    ~debug ~verbose ~solver_backend ~block_dim

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

let verbose_arg =
  let doc = "Enable verbose output for optimization formulas" in
  Arg.(value & flag & info [ "verbose" ] ~doc)

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
      $ mode_arg $ tactic_file_arg $ debug_arg $ verbose_arg $ solver_arg
      $ block_dim_arg)

(* Main entry point *)
let () = Cmd.eval main_cmd |> exit
