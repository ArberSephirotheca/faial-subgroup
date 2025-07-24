open Stage0
open Inference
open Rel_cost
open Protocols

let abort_when (b : bool) (msg : string) : unit =
  if b then (
    Logger.Colors.error msg;
    exit (-2))
  else ()

module Hotspot = struct
  type t = {
    bank : Bank.t;
    divergence : Divergence_analysis.t;
    max_cost : Cost.t;
    index : (Cost.t, string) Result.t;
    sim : (Cost.t, string) Result.t;
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
  }

  let make ~kernels ~skip_zero ~skip_distinct_vars ~config ~ignore_absent
      ~only_reads ~only_writes ~block_dim ~grid_dim ~params ~simulate : t =
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
    }

  let sliced_cost (a : t) (k : Kernel.t) : Hotspot.t list =
    Bank.from_proto a.config k
    |> Seq.filter_map (fun bank ->
           let m =
             let open Bank in
             match bank.hierarchy with
             | Mem_hierarchy.SharedMemory -> Metric.BankConflicts
             | GlobalMemory -> UncoalescedAccesses
           in
           let to_cost value = Cost.from_int ~value ~exact:true () in
           let max_cost = Metric.max_cost_from a.config m |> to_cost in
           let r_cost = Bank.index_cost a.config m bank in
           let max_cost = Result.value ~default:max_cost r_cost in
           let min_cost = Metric.min_cost m |> to_cost in
           let divergence = Divergence_analysis.from_bank bank in
           let cost = Result.value ~default:max_cost r_cost in
           let sim =
             if
               a.simulate
               && Divergence_analysis.is_known divergence
               && Cost.(cost > min_cost)
             then Bank.eval_res ~max_cost:max_cost.value a.config m bank
             else Error "Run with --simulate to output simulated cost."
           in
           (* Convert a slice into an expression *)
           if Cost.(cost <= min_cost) && a.skip_zero then None
           else Some Hotspot.{ index = r_cost; max_cost; bank; divergence; sim })
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
      |> List.map (Kernel.filter_access retain_acc)
      |> List.map
           (Kernel.inline_all ~block_dim:(Some s.block_dim)
              ~grid_dim:(Some s.grid_dim) ~globals:s.params)
      |> List.map Kernel.opt
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
               | Ok e, _ ->
                   let e = e.value |> string_of_int in
                   let pot =
                     if
                       Divergence_analysis.is_thread_uniform conflict.divergence
                     then ""
                     else " (potential)"
                   in
                   e ^ pot
               | _, _ -> string_of_int conflict.max_cost.value ^ " (potential)"
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
                    text_with_style Style.bold "Context";
                    text (conflict.bank |> Bank.trim_decls |> Bank.to_string);
                  |];
                ]
               @
               let tsx =
                 if Result.is_ok conflict.sim then conflict.sim
                 else conflict.index
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
                                  ^ string_of_int y ^ ", " ^ "z:"
                                  ^ string_of_int z
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
                             `Int
                               (loc.interval |> Interval.start |> Index.to_base1)
                           );
                           ( "col_finish",
                             `Int
                               (loc.interval |> Interval.finish
                              |> Index.to_base1) );
                         ] );
                   ]
                 in
                 let cost =
                   [
                     ( "index_analysis",
                       match c.index with
                       | Ok { value; _ } -> `Int value
                       | Error _ -> `Null );
                     ( "access",
                       `String (c.bank |> Bank.trim_decls |> Bank.to_string) );
                     ( "thread_divergence_analysis",
                       `String
                         (c.bank |> Divergence_analysis.from_bank
                        |> Divergence_analysis.to_string) );
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

let run ?(skip_zero = true) ~skip_distinct_vars ~config ~output_json
    ~ignore_absent ~only_reads ~only_writes ~block_dim ~grid_dim ~params
    ~simulate (kernels : Kernel.t list) : unit =
  let app : Solver.t =
    Solver.make ~skip_zero ~skip_distinct_vars ~config ~kernels ~ignore_absent
      ~only_reads ~only_writes ~block_dim ~grid_dim ~params ~simulate
  in
  if output_json then JUI.run app else TUI.run app

let pico (fname : string) (block_dim : Dim3.t option) (grid_dim : Dim3.t option)
    (show_all : bool) (skip_distinct_vars : bool) (ignore_absent : bool)
    (output_json : bool) (only_reads : bool) (only_writes : bool)
    (params : (string * int) list) (simulate : bool) =
  let parsed = Protocol_parser.Silent.to_proto ~block_dim ~grid_dim fname in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  let config = Config.make ~block_dim ~grid_dim () in
  run ~skip_zero:(not show_all) ~skip_distinct_vars ~config ~output_json
    ~ignore_absent ~only_reads ~only_writes ~block_dim ~grid_dim ~params
    ~simulate parsed.kernels

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

let pico_t =
  Term.(
    const pico $ get_fname $ block_dim $ grid_dim $ show_all
    $ skip_distinct_vars $ ignore_absent $ output_json $ only_reads
    $ only_writes $ params $ simulate)

let info =
  let doc = "Static analysis of bank-conflicts for GPU programs" in
  Cmd.info "faial-bc" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info pico_t |> Cmd.eval |> exit
