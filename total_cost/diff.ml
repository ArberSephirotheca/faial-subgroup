open Stage0
open Inference
open Bank_conflicts
open Protocols
open Analyze_cost

type kernel = Protocols.Kernel.t

let abort_when (b:bool) (msg:string) : unit =
  if b then (
    Logger.Colors.error msg;
    exit (-2)
  ) else ()

module Solver = struct

  type t = {
    kernels1: kernel list;
    kernels2: kernel list;
    use_maxima: bool;
    maxima_exe: string;
    show_code: bool;
    show_map: bool;
    show_ra: bool;
    skip_simpl_ra: bool;
    config: Config.t;
    ignore_absent: bool;
    only_reads: bool;
    only_writes: bool;
    block_dim: Dim3.t;
    grid_dim: Dim3.t;
    params: (string * int) list;
    approx_ifs: bool;
    strategy: Analysis_strategy.t;
    metric: Metric.t;
    compact: bool;
  }

  let make
    ~kernels1
    ~kernels2
    ~use_maxima
    ~show_code
    ~maxima_exe
    ~show_ra
    ~skip_simpl_ra
    ~config
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
    ~approx_ifs
    ~strategy
    ~metric
    ~compact
    ~show_map
    ()
  :
    t
  =
    {
      kernels1;
      kernels2;
      use_maxima;
      show_code;
      maxima_exe;
      show_ra;
      skip_simpl_ra;
      config;
      ignore_absent;
      only_reads;
      only_writes;
      block_dim;
      grid_dim;
      params;
      approx_ifs;
      strategy;
      metric;
      compact;
      show_map;
    }

  type cost = {
    amount: string;
    analysis_duration: float;
  }

  let get_cost (app:t) (r1:Ra.Stmt.t) (r2:Ra.Stmt.t) : (cost, string) Result.t =
    if app.show_ra then (
      Ra.Stmt.to_string r1 |> print_endline;
      Ra.Stmt.to_string r2 |> print_endline;
    );
    let start = Unix.gettimeofday () in
    let s : Summation.t =
      let open Summation in
      minus (from_stmt r1) (from_stmt r2)
    in
    (if app.use_maxima then (
      s
      |> Maxima.from_summation
      |> Maxima.compile ~compact:app.compact
      |> Maxima.run_exe
          ~verbose:app.show_code
          ~exe:app.maxima_exe
    ) else
      let s = Summation.to_string s in
      if app.show_code then (print_endline s);
      Ok s
    )
    |> Result.map (fun c ->
      {amount=c; analysis_duration=Unix.gettimeofday () -. start}
    )
    |> Result.map_error Errors.to_string

  let get_ra
    (a:t)
    (k:kernel)
  :
    (Ra.Stmt.t, string) Result.t
  =
    let ( let* ) = Result.bind in
    let unif_cond =
      if a.approx_ifs then
        Ra_compiler.UniformCond.Approximate
      else
        Ra_compiler.UniformCond.Exact
    in
    let* (r, _) =
      Ra_compiler.Default.from_kernel
        ~unif_cond ~strategy:a.strategy a.metric a.config k
    in
    Ok (if a.skip_simpl_ra then r else Ra.Stmt.simplify r)

  type r_cost = (cost, string) Result.t

  let total_cost (a:t) ((k1,k2):kernel*kernel) : r_cost =
    let ( let* ) = Result.bind in
    let* r1 = get_ra a k1 in
    let* r2 = get_ra a k2 in
    get_cost a r1 r2

  type summary =
    | TotalCost of ((kernel * kernel) * r_cost) list

  let run (s:t) : summary =
    let pair f k =
      (k, f k)
    in
    (* optimize *)
    let retain_acc =
      if s.only_reads then
        Protocols.Access.is_read
      else if s.only_writes then
        Protocols.Access.is_write
      else
        fun a ->
          Protocols.Access.is_read a || Protocols.Access.is_write a
    in
    let clean ks =
      ks
      |> (
        if s.only_reads || s.only_writes then
          List.map (Protocols.Kernel.filter_access retain_acc)
        else
          fun x -> x
        )
      |> List.map (fun k ->
          if s.metric = CountAccesses then k else
          let vs : Variable.Set.t =
            match s.metric with
            | BankConflicts -> Protocols.Kernel.shared_arrays k
            | UncoalescedAccesses -> Protocols.Kernel.global_arrays k
            | _ -> failwith "internal error"
          in
          Protocols.Kernel.filter_array (fun x -> Variable.Set.mem x vs) k
        )
      |> List.map (
          Protocols.Kernel.inline_all
          ~block_dim:(Some s.block_dim)
          ~grid_dim:(Some s.grid_dim)
          ~globals:s.params
        )
      |> List.map Protocols.Kernel.opt
    in
    let kernels1 = clean s.kernels1 in
    let kernels2 =
      s.kernels2
      |> clean
      |> List.map (fun k -> (Kernel.name k, k))
    in
    let ks =
      List.filter_map (fun k1 ->
        List.assoc_opt (Kernel.name k1) kernels2
        |> Option.map (fun k2 -> (k1, k2))
      ) kernels1
    in
    if s.show_map then (
      List.iter (fun (k1, k2) ->
        Protocols.Kernel.print k1;
        Protocols.Kernel.print k2;
      ) ks
    );
    TotalCost (List.map (pair (total_cost s)) ks)

end

module TUI = struct

  let run ~only_cost (s:Solver.t) =
    let print_r_cost ((k1,_:kernel*kernel), (r:Solver.r_cost)) : unit =
      match r with
      | Ok c ->
        if only_cost then (
          print_endline c.amount
        ) else (
          let d = Float.to_int (c.analysis_duration *. 1000.) in
          print_string (k1.name ^ " (" ^ string_of_int d  ^ "ms):\n");
          PrintBox.(
            c.amount
            |> text
            |> hpad 1
            |> frame
          )
          |> PrintBox_text.to_string
          |> print_endline
        )
      | Error e ->
        prerr_endline e;
        exit (-1)
    in
    Stdlib.flush_all ();
    match Solver.run s with
    | TotalCost [] ->
      abort_when (not s.ignore_absent) "No kernels found.";
    | TotalCost l ->
      List.iter print_r_cost l
end

module JUI = struct
  open Yojson.Basic
  type json = Yojson.Basic.t
  let to_json (s:Solver.t) : json =
    let c_to_j name ((k1,_:kernel * kernel), r) : json =
      let open Solver in
      match r with
      | Ok e ->
        `Assoc [
          (name, `String e.amount);
          ("kernel_name", `String k1.name);
          ("analysis_duration_seconds", `Float e.analysis_duration);
        ]
      | Error e ->
        `Assoc [
          ("kernel_name", `String k1.name);
          ("error", `String e);
        ]
    in
    let kernels =
      match Solver.run s with
      | TotalCost l -> `List (List.map (c_to_j "total_cost") l)
    in
    `Assoc [
      "kernels", kernels;
      "argv", `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x));
      "executable_name", `String Sys.executable_name;
      "z3_version", `String (Z3.Version.to_string);
    ]

  let run (s:Solver.t) : unit =
    s
    |> to_json
    |> to_string
    |> print_endline
end

let run
  ?(use_maxima=false)
  ?(show_code=false)
  ?(maxima_exe="maxima")
  ?(show_ra=false)
  ?(show_map=false)
  ?(skip_simpl_ra=true)
  ~only_cost
  ~config
  ~output_json
  ~ignore_absent
  ~only_reads
  ~only_writes
  ~block_dim
  ~grid_dim
  ~params
  ~approx_ifs
  ~strategy
  ~metric
  ~compact
  ~kernels1
  ~kernels2
  ()
:
  unit
=
  let app : Solver.t = Solver.make
    ~use_maxima
    ~maxima_exe
    ~show_code
    ~show_ra
    ~skip_simpl_ra
    ~config
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
    ~approx_ifs
    ~strategy
    ~metric
    ~compact
    ~show_map
    ~kernels1
    ~kernels2
    ()
  in
  if output_json then
    JUI.run app
  else
    TUI.run ~only_cost app


let pico
  (fname1:string)
  (fname2:string)
  (block_dim:Dim3.t option)
  (grid_dim:Dim3.t option)
  (use_maxima:bool)
  (show_ra:bool)
  (show_code:bool)
  (maxima_exe:string)
  (skip_simpl_ra:bool)
  (only_cost:bool)
  (ignore_absent:bool)
  (output_json:bool)
  (only_reads:bool)
  (only_writes:bool)
  (params:(string * int) list)
  (approx_ifs:bool)
  (strategy:Analysis_strategy.t)
  (metric:Metric.t)
  (compact:bool)
  (ignore_parsing_errors:bool)
  (show_map:bool)
  (bank_count:int)
  (threads_per_warp:int)
=
  let parse fname =
    let parsed = Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~block_dim
      ~grid_dim
      fname in
    parsed.kernels
  in
  let block_dim : Dim3.t = Option.value ~default:(Dim3.make ~x:32 ()) block_dim in
  let grid_dim : Dim3.t = Option.value ~default:Dim3.one grid_dim in
  let config =
    Config.make ~block_dim ~grid_dim ~bank_count ~threads_per_warp () in
  run
    ~use_maxima
    ~show_code
    ~show_ra
    ~maxima_exe
    ~skip_simpl_ra
    ~config
    ~only_cost
    ~output_json
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
    ~approx_ifs
    ~strategy
    ~metric
    ~compact
    ~show_map
    ~kernels1:(parse fname1)
    ~kernels2:(parse fname2)
    ()


(* Command-line interface *)

open Cmdliner


let dim3 : Dim3.t Cmdliner.Arg.conv =
  let parse =
    fun s ->
      match Dim3.parse s with
      | Ok r -> Ok r
      | Error e -> Error (`Msg e)
  in
  let print : Dim3.t Cmdliner.Arg.printer =
    fun ppf v -> Format.fprintf ppf "%s" (Dim3.to_string v)
  in
  Arg.conv (parse, print)

let fname1 =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"KERNEL1" ~doc)

let fname2 =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"KERNEL2" ~doc)

let block_dim =
  let doc = "Sets the CUDA variable blockDim, the number of threads per block.\n" ^
  "Input is a single integer or a list of integers, signifying the x, y, and z positions.\n" ^
  "Default: '" ^ Dim3.to_string Gv_parser.default_block_dim ^ "'\n" ^
  "Examples: '1024' and '[16,16]'" in
  Arg.(value & opt (some dim3) None & info ["b"; "block-dim"; "blockDim"] ~docv:"BLOCK_DIM" ~doc)

let grid_dim =
  let doc = "Sets the CUDA variable gridDim, the number of blocks per grid.\n" ^
  "Input is a single integer or a list of integers, signifying the x, y, and z positions.\n" ^
  "Default: '" ^ Dim3.to_string Gv_parser.default_grid_dim ^ "'\n" ^
  " Examples: '1024' and '[16,16]'" in
  Arg.(value & opt (some dim3) None & info ["g"; "grid-dim"; "gridDim"] ~docv:"GRID_DIM" ~doc)

let maxima_exe =
  let doc = "Sets the path to the Maxima executable." in
  Arg.(value & opt string "maxima" & info ["maxima-exe"] ~doc)

let use_maxima =
  let doc = "Uses maxima to simplify the cost of each access." in
  Arg.(value & flag & info ["maxima"] ~doc)

let only_cost =
  let doc = "Only prints out the cost, no kernel name, and no UI flourishes. This option is only available when computing the total cost (default option)." in
  Arg.(value & flag & info ["only-cost"] ~doc)

let ignore_absent =
  let doc = "Makes it not an error to analyze a kernel without shared errors." in
  Arg.(value & flag & info ["ignore-absent"] ~doc)

let skip_simpl_ra =
  let doc = "By default we simplify the RA to improve performance of solvers." in
  Arg.(value & flag & info ["skip-simpl-ra"] ~doc)

let show_ra =
  let doc = "Print out the resource-analysis problem that represents the bank conflicts." in
  Arg.(value & flag & info ["show-ra"] ~doc)

let show_code =
  let doc = "Show the code being sent to the solver if any." in
  Arg.(value & flag & info ["show-code"] ~doc)

let output_json =
  let doc = "Output in JSON." in
  Arg.(value & flag & info ["json"] ~doc)

let only_reads =
  let doc = "Only account for load transactions (access reads)." in
  Arg.(value & flag & info ["only-reads"] ~doc)

let only_writes =
  let doc = "Only account for store transactions (access writes)." in
  Arg.(value & flag & info ["only-writes"] ~doc)

let params =
  let doc = "Set the value of an integer parameter" in
  Arg.(value & opt_all (pair ~sep:'=' string int) [] & info ["p"; "param"] ~docv:"KEYVAL" ~doc)

let approx_ifs =
  let doc = "Approximate conditionals." in
  Arg.(value & flag & info ["approx"] ~doc)

let strategy =
  let doc = "Generate minimum cost for approximate costs (default is maximum cost)." in
  Arg.(
    value &
    opt
      (enum
        [
          "min", Analysis_strategy.UnderApproximation;
          "max", Analysis_strategy.OverApproximation;
        ]
      )
      Analysis_strategy.OverApproximation &
    info ["cost"] ~doc
  )

let metric =
  let doc = "Select the metric to measure the cost." in
  Arg.(required & opt (some (enum Metric.choices)) None & info ["m"; "metric"] ~doc)

let compact =
  let doc = "Render Maxima formulas on a single line." in
  Arg.(value & flag & info ["compact"] ~doc)

let ignore_parsing_errors =
  let doc = "Parsing errors do not abort analysis" in
  Arg.(value & flag & info ["ignore-parsing-errors"] ~doc)

let show_map =
  let doc = "Show the inferred Memory Access Protocol (MAP)." in
  Arg.(value & flag & info ["show-map"] ~doc)

let bank_count =
  let default = 32 in
  let info =
    Arg.info ["bank-count"]
      ~docv:"BANK_COUNT"
      ~doc:"The number of banks available in the GPU device."
  in
  Arg.value (Arg.opt Arg.int default info)

let warp_size =
  let default = 32 in
  let info =
    Arg.info ["warp-size"]
      ~docv:"WARP_SIZE"
      ~doc:"The number of threads per warp available in the GPU device."
  in
  Arg.value (Arg.opt Arg.int default info)

let pico_t = Term.(
  const pico
  $ fname1
  $ fname2
  $ block_dim
  $ grid_dim
  $ use_maxima
  $ show_ra
  $ show_code
  $ maxima_exe
  $ skip_simpl_ra
  $ only_cost
  $ ignore_absent
  $ output_json
  $ only_reads
  $ only_writes
  $ params
  $ approx_ifs
  $ strategy
  $ metric
  $ compact
  $ ignore_parsing_errors
  $ show_map
  $ bank_count
  $ warp_size
)

let info =
  let doc = "The cost between two GPU kernels" in
  Cmd.info "faial-cost-diff" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info pico_t
  |> Cmd.eval
  |> exit
