open Stage0
open Inference
open Rel_cost
open Protocols
open Perf_bottleneck

module MemoryFilter = struct
  type t = SharedOnly | GlobalOnly | Both

  let contains (hierarchy : Mem_hierarchy.t) (filter : t) : bool =
    match filter with
    | Both -> true
    | SharedOnly -> Mem_hierarchy.is_shared hierarchy
    | GlobalOnly -> not (Mem_hierarchy.is_shared hierarchy)

  let conv : t Cmdliner.Arg.conv =
    let parse s =
      match s with
      | "shared" -> Ok SharedOnly
      | "global" -> Ok GlobalOnly
      | "both" -> Ok Both
      | _ -> Error (`Msg "Expected shared, global, or both")
    in
    let print ppf = function
      | SharedOnly -> Format.fprintf ppf "shared"
      | GlobalOnly -> Format.fprintf ppf "global"
      | Both -> Format.fprintf ppf "both"
    in
    Cmdliner.Arg.conv (parse, print)
end

let abort_when (b : bool) (msg : string) : unit =
  if b then (
    Logger.Colors.error msg;
    exit (-2))
  else ()

let time_analysis (f : unit -> 'a) : float * 'a =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  (elapsed, result)

let format_time_sliding (seconds : float) : string =
  if seconds >= 60.0 then
    let mins = int_of_float (seconds /. 60.0) in
    let remaining_secs = seconds -. (float_of_int mins *. 60.0) in
    Printf.sprintf "%dm %.1fs" mins remaining_secs
  else if seconds >= 1.0 then Printf.sprintf "%.2fs" seconds
  else Printf.sprintf "%.3fms" (seconds *. 1000.0)

module Hotspot = struct
  type t = {
    bank : Bank.t;
    divergence : Divergence_analysis.t;
    index : Metric_analysis.IndexCost.t;
    sim : (Cost.t, string) Result.t;
    analysis_time_secs : float;
  }

  let hierarchy (e : t) : Mem_hierarchy.t =
    let open Bank in
    e.bank.hierarchy
end

module Solver = struct
  type t = {
    kernels : Kernel.t list;
    skip_zero : bool;
    config : Config.t;
    ignore_absent : bool;
    only_reads : bool;
    only_writes : bool;
    block_dim : Dim3.t;
    grid_dim : Dim3.t;
    params : (string * int) list;
    simulate : bool;
    memory_filter : MemoryFilter.t;
    erase_ctx : bool;
    line_filter : int option;
    col_filter : int option;
    metric : Metric.t;
    verbose : bool;
  }

  let make ~kernels ~skip_zero ~skip_distinct_vars ~config ~ignore_absent
      ~only_reads ~only_writes ~block_dim ~grid_dim ~params ~simulate
      ~memory_filter ~erase_ctx ~line_filter ~col_filter ~metric ~verbose : t =
    let kernels =
      if skip_distinct_vars then kernels
      else List.map Kernel.vars_distinct kernels
    in
    {
      kernels;
      skip_zero;
      config;
      ignore_absent;
      only_reads;
      only_writes;
      block_dim;
      grid_dim;
      params;
      simulate;
      memory_filter;
      erase_ctx;
      line_filter;
      col_filter;
      metric;
      verbose;
    }

  let sliced_cost (a : t) (k : Kernel.t) : Hotspot.t list =
    Bank.from_proto a.config k
    |> Seq.filter (fun bank ->
        let open Bank in
        let memory_match =
          MemoryFilter.contains bank.hierarchy a.memory_filter
        in
        let loc = Bank.location bank in
        let line_match =
          match a.line_filter with
          | None -> true
          | Some target_line -> Index.to_base1 loc.line = target_line
        in
        let col_match =
          match a.col_filter with
          | None -> true
          | Some target_col ->
              let start_col =
                loc.interval |> Interval.start |> Index.to_base1
              in
              start_col = target_col
        in
        memory_match && line_match && col_match)
    |> Seq.map (fun bank ->
        let bank = Bank.normalize bank |> Bank.trim_decls in
        let bank = if a.erase_ctx then Bank.erase_context bank else bank in
        if a.verbose then prerr_endline (Bank.to_string bank);
        let to_cost value = Cost.from_int ~value ~exact:true () in
        let max_cost = Metric.max_cost_from a.config a.metric |> to_cost in
        let analysis_time_secs, r_cost =
          time_analysis (fun () ->
              Bank.index_cost ~verbose:a.verbose a.config a.metric bank)
        in
        let _ = a.skip_zero in
        let divergence = Divergence_analysis.from_bank bank in
        let sim =
          if a.simulate && Divergence_analysis.is_known divergence then
            Bank.eval_res ~max_cost:max_cost.value a.config a.metric bank
          else Error "Run with --simulate to output simulated cost."
        in
        Hotspot.{ index = r_cost; bank; divergence; sim; analysis_time_secs })
    |> List.of_seq

  let run (s : t) : (Kernel.t * Hotspot.t list) list =
    let pair f k = (k, f k) in
    (* optimize *)
    let retain_acc =
      if s.only_reads then Protocols.Access.is_read
      else if s.only_writes then Protocols.Access.is_write
      else fun a -> Protocols.Access.is_read a || Protocols.Access.is_write a
    in
    let ks =
      s.kernels
      |> List.map (fun (k : Protocols.Kernel.t) ->
          let open Protocols.Kernel in
          let k = Kernel.filter_access retain_acc k in
          let k =
            let vs : Variable.Set.t =
              let open Kernel in
              Metric.supported_arrays k.arrays s.metric
            in
            Kernel.filter_array (fun x -> Variable.Set.mem x vs) k
          in
          k |> set_block_dim s.block_dim |> set_grid_dim s.grid_dim
          |> inline_globals s.params |> opt)
    in
    List.map (pair (sliced_cost s)) ks
end

module TUI = struct
  let run (s : Solver.t) =
    let print_slice ((k : Kernel.t), (s : Hotspot.t list)) : unit =
      ANSITerminal.(
        print_string [ Bold; Foreground Green ]
          ("\n### Kernel '" ^ k.name ^ "' ###\n\n"));
      Logger.Colors.info ("Accesses found: " ^ string_of_int (List.length s));
      Stdlib.flush_all ();
      s
      |> List.iter (fun conflict ->
          let open Hotspot in
          let is_bc =
            conflict |> Hotspot.hierarchy |> Mem_hierarchy.is_shared
          in
          let lbl =
            if is_bc then "shared transactions" else "global transactions"
          in
          let bc =
            match (conflict.index, conflict.sim) with
            | _, Ok e ->
                let e = e.value |> string_of_int in
                let pot =
                  if Divergence_analysis.is_known conflict.divergence then ""
                  else " (potential)"
                in
                e ^ pot
            | e, _ ->
                let e = e |> Metric_analysis.IndexCost.to_string in
                let pot =
                  if Divergence_analysis.is_thread_uniform conflict.divergence
                  then ""
                  else " (potential)"
                in
                e ^ pot
          in
          let cost =
            let open PrintBox in
            ([
               [| text_with_style Style.bold ("Max " ^ lbl); text bc |];
               [|
                 text_with_style Style.bold "Thread-divergence";
                 text (Divergence_analysis.to_string conflict.divergence);
               |];
               [|
                 text_with_style Style.bold "Analysis time";
                 text (format_time_sliding conflict.analysis_time_secs);
               |];
               [|
                 text_with_style Style.bold "Context";
                 text (conflict.bank |> Bank.trim_decls |> Bank.to_string);
               |];
             ]
            @
            let tsx =
              if Result.is_ok conflict.sim then conflict.sim
              else conflict.index |> Metric_analysis.IndexCost.to_cost
            in
            match tsx with
            | Ok Cost.{ value; state = Some { accesses = accs; _ }; _ } ->
                let b = string_of_int value in
                let accs = accs |> List.sort compare in
                let idx =
                  accs
                  |> List.map (fun (a : Transaction.Task.t) ->
                      text (string_of_int a.index))
                in
                let tids =
                  accs
                  |> List.map (fun (a : Transaction.Task.t) ->
                      let id =
                        match a.id with
                        | { x; y; z } ->
                            "x:" ^ string_of_int x ^ ", " ^ "y:"
                            ^ string_of_int y ^ ", " ^ "z:" ^ string_of_int z
                      in
                      text id)
                in
                let rows =
                  [|
                    text_with_style Style.bold "threadIdx";
                    text_with_style Style.bold "Index";
                  |]
                  :: List.map2 (fun x y -> [| x; y |]) tids idx
                  |> Array.of_list
                in
                [ [| text_with_style Style.bold ("Bank " ^ b); grid rows |] ]
            | _ -> [])
            |> Array.of_list |> grid |> frame
          in
          (* Flatten the expression *)
          let problem =
            if is_bc then "Bank-conflict" else "Uncoalesced access"
          in
          ANSITerminal.(
            print_string [ Bold; Foreground Blue ]
              ("\n~~~~ " ^ problem ^ " ~~~~\n\n"));
          conflict.bank |> Bank.location |> Tui_helper.LocationUI.print;
          print_endline "";
          PrintBox_text.output stdout cost;
          print_endline "\n")
    in
    Stdlib.flush_all ();
    let l = Solver.run s in
    if l = [] then
      abort_when (not s.ignore_absent)
        "No kernels using __shared__ arrays found.";
    List.iter print_slice l
end

module JUI = struct
  open Yojson.Basic

  type json = Yojson.Basic.t

  let to_json (s : Solver.t) : json =
    let s_to_j ((k : Kernel.t), l) : json =
      let accs : json =
        `List
          (l
          |> List.map (fun c ->
              let open Hotspot in
              let loc = Bank.location c.bank in
              let loc =
                [
                  ( "location",
                    `Assoc
                      [
                        ("filename", `String loc.filename);
                        ("line", `Int (Index.to_base1 loc.line));
                        ( "col_start",
                          `Int (loc.interval |> Interval.start |> Index.to_base1)
                        );
                        ( "col_finish",
                          `Int
                            (loc.interval |> Interval.finish |> Index.to_base1)
                        );
                      ] );
                ]
              in
              let cost =
                [
                  ( "index_analysis",
                    match c.index.code with
                    | Tick value -> `Int value
                    | e -> `String (Ra.Stmt.to_string e) );
                  ( "access",
                    `String (c.bank |> Bank.trim_decls |> Bank.to_string) );
                  ( "thread_divergence_analysis",
                    `String
                      (c.bank |> Divergence_analysis.from_bank
                     |> Divergence_analysis.to_string) );
                  ("cond_size", `Int (c.bank |> Bank.cond_size));
                  ("index_size", `Int (c.bank |> Bank.index_size));
                  ("analysis_time_secs", `Float c.analysis_time_secs);
                  ( "sim",
                    match c.sim with
                    | Ok { value; _ } -> `Int value
                    | Error _ -> `Null );
                ]
              in
              `Assoc (loc @ cost)))
      in
      `Assoc [ ("kernel_name", `String k.name); ("accesses", accs) ]
    in
    let kernels =
      let l = Solver.run s in
      `List (List.map s_to_j l)
    in
    `Assoc
      [
        ("kernels", kernels);
        ( "argv",
          `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x)) );
        ("executable_name", `String Sys.executable_name);
        ("z3_version", `String Z3.Version.to_string);
      ]

  let run (s : Solver.t) : unit = s |> to_json |> to_string |> print_endline
end

module TheoremExporter = struct
  open Rel_cost.Symbolic_metric_analysis

  (* Convert Bank analysis to Theorem structure *)
  let hotspot_to_theorem (cfg : Config.t) (h : Hotspot.t) : Theorem.t =
    let b = h.bank in
    (* Extract thread-local variables from Bank analysis *)
    let locals = Bank.local_binders b in
    (* Extract the memory access index expression *)
    let index = Bank.index b in
    (* Extract condition from Bank context *)
    let local_context = Bank.to_bexp b in
    (* Calculate expected cost from analysis *)
    let expected_cost =
      let cost = match h.index.code with Tick value -> value | _ -> 32 in
      let open Exp in
      Num cost
    in
    (* Create a goal comparing ua(index) == expected_cost *)
    let goal =
      Theorem.Goal.Prop (NRel (N_rel.Eq, NCall ("ua", index), expected_cost))
    in
    {
      cfg;
      locals;
      globals = Variable.Set.empty;
      active_threads = local_context;
      assumptions = Exp.b_true;
      goals = [ goal ];
    }

  (* Export theorems to stdout *)
  let export_theorems (s : Solver.t) : unit =
    let results = Solver.run s in
    List.iteri
      (fun kernel_idx ((kernel : Kernel.t), hotspots) ->
        List.iteri
          (fun hotspot_idx hotspot ->
            let cfg = s.config in
            let theorem = hotspot_to_theorem cfg hotspot in
            let content =
              theorem |> Rel_cost_parsing.Theorem_file.of_theorem
              |> Rel_cost_parsing.Theorem_file.to_string
            in
            prerr_endline
              (Printf.sprintf "(* Theorem for kernel '%s' hotspot %d_%d *)\n"
                 kernel.name kernel_idx hotspot_idx);
            prerr_endline content;
            prerr_endline "")
          hotspots)
      results
end

let run ?(skip_zero = true) ~skip_distinct_vars ~config ~output_json
    ~export_theorems ~ignore_absent ~only_reads ~only_writes ~block_dim
    ~grid_dim ~params ~simulate ~memory_filter ~erase_ctx ~line_filter
    ~col_filter ~metric ~verbose (kernels : Kernel.t list) : unit =
  let app : Solver.t =
    Solver.make ~skip_zero ~skip_distinct_vars ~config ~kernels ~ignore_absent
      ~only_reads ~only_writes ~block_dim ~grid_dim ~params ~simulate
      ~memory_filter ~erase_ctx ~line_filter ~col_filter ~metric ~verbose
  in
  if export_theorems then TheoremExporter.export_theorems app;
  if output_json then JUI.run app else TUI.run app

let main (fname : string) (block_dim : Dim3.t option) (grid_dim : Dim3.t option)
    (show_all : bool) (skip_distinct_vars : bool) (ignore_absent : bool)
    (output_json : bool) (export_theorems : bool) (only_reads : bool)
    (only_writes : bool) (params : (string * int) list) (simulate : bool)
    (memory_filter : MemoryFilter.t) (erase_ctx : bool)
    (line_filter : int option) (col_filter : int option) (metric : Metric.t)
    (verbose : bool) =
  let parsed = Protocol_parser.Silent.to_proto ~block_dim ~grid_dim fname in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  let config = Config.make ~block_dim ~grid_dim () in
  run ~skip_zero:(not show_all) ~skip_distinct_vars ~config ~output_json
    ~export_theorems ~ignore_absent ~only_reads ~only_writes ~block_dim
    ~grid_dim ~params ~simulate ~memory_filter ~erase_ctx ~line_filter
    ~col_filter ~metric ~verbose parsed.kernels

(* Command-line interface *)

open Cmdliner

let dim3 : Dim3.t Cmdliner.Arg.conv =
  let parse =
   fun s -> match Dim3.parse s with Ok r -> Ok r | Error e -> Error (`Msg e)
  in
  let print : Dim3.t Cmdliner.Arg.printer =
   fun ppf v -> Format.fprintf ppf "%s" (Dim3.to_string v)
  in
  Arg.conv (parse, print)

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let block_dim =
  let doc =
    "Sets the CUDA variable blockDim, the number of threads per block.\n"
    ^ "Input is a single integer or a list of integers, signifying the x, y, \
       and z positions.\n" ^ "Default: '"
    ^ Dim3.to_string Gv_parser.default_block_dim
    ^ "'\n" ^ "Examples: '1024' and '[16,16]'"
  in
  Arg.(
    value
    & opt (some dim3) None
    & info [ "b"; "block-dim"; "blockDim" ] ~docv:"BLOCK_DIM" ~doc)

let grid_dim =
  let doc =
    "Sets the CUDA variable gridDim, the number of blocks per grid.\n"
    ^ "Input is a single integer or a list of integers, signifying the x, y, \
       and z positions.\n" ^ "Default: '"
    ^ Dim3.to_string Gv_parser.default_grid_dim
    ^ "'\n" ^ " Examples: '1024' and '[16,16]'"
  in
  Arg.(
    value
    & opt (some dim3) None
    & info [ "g"; "grid-dim"; "gridDim" ] ~docv:"GRID_DIM" ~doc)

let ignore_absent =
  let doc =
    "Makes it not an error to analyze a kernel without shared errors."
  in
  Arg.(value & flag & info [ "ignore-absent" ] ~doc)

let skip_distinct_vars =
  let doc =
    "By default we make all loop varibles distinct, as a workaround for \
     certain solvers' limitations."
  in
  Arg.(value & flag & info [ "skip-distinct-vars" ] ~doc)

let show_all =
  let doc = "By default we skip accesses that yield 0 bank-conflicts." in
  Arg.(value & flag & info [ "show-all" ] ~doc)

let output_json =
  let doc = "Output in JSON." in
  Arg.(value & flag & info [ "json" ] ~doc)

let export_theorems =
  let doc = "Show analysis results as theorem for faial-cost-prover." in
  Arg.(value & flag & info [ "show-theorems" ] ~doc)

let only_reads =
  let doc = "Only account for load transactions (access reads)." in
  Arg.(value & flag & info [ "only-reads" ] ~doc)

let only_writes =
  let doc = "Only account for store transactions (access writes)." in
  Arg.(value & flag & info [ "only-writes" ] ~doc)

let params =
  let doc = "Set the value of an integer parameter" in
  Arg.(
    value
    & opt_all (pair ~sep:'=' string int) []
    & info [ "p"; "param" ] ~docv:"KEYVAL" ~doc)

let simulate =
  let doc = "Simulate the cost if possible." in
  Arg.(value & flag & info [ "sim" ] ~doc)

let memory_type =
  let doc =
    "Filter analysis by memory type: shared (bank conflicts), global \
     (uncoalesced accesses), or both."
  in
  Arg.(
    value
    & opt MemoryFilter.conv MemoryFilter.Both
    & info [ "memory-type" ] ~docv:"TYPE" ~doc)

let erase_ctx =
  let doc = "Apply context erasure to simplify analysis after normalization." in
  Arg.(value & flag & info [ "erase-ctx" ] ~doc)

let line_filter =
  let doc = "Show only accesses at the specified line number (1-indexed)." in
  Arg.(value & opt (some int) None & info [ "line" ] ~docv:"LINE" ~doc)

let col_filter =
  let doc = "Show only accesses at the specified column number (1-indexed)." in
  Arg.(value & opt (some int) None & info [ "col" ] ~docv:"COL" ~doc)

let metric =
  let doc =
    Printf.sprintf "Select a metric: (%s)."
      (Metric.values |> List.map Metric.to_string |> String.concat ", ")
  in
  Arg.(
    required
    & opt (some (enum Metric.choices)) None
    & info [ "m"; "metric" ] ~doc)

let verbose =
  let doc = "Enable verbose output for analysis." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let main_t =
  Term.(
    const main $ get_fname $ block_dim $ grid_dim $ show_all
    $ skip_distinct_vars $ ignore_absent $ output_json $ export_theorems
    $ only_reads $ only_writes $ params $ simulate $ memory_type $ erase_ctx
    $ line_filter $ col_filter $ metric $ verbose)

let info =
  let doc = "Static analysis of bank-conflicts for GPU programs" in
  Cmd.info "faial-bc" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
