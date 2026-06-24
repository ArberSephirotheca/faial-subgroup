open Stage0
open Protocols
open Drf
open Inference
module SM = Subgroup_matrix
module Subgroup_solver = Drf.Subgroup_solver
module Subgroup_uniformity = Drf.Subgroup_uniformity
module Subgroup_obligation = Drf.Memory_event.Subgroup_obligation

type kernel =
  | Ordinary_kernel of Protocols.Kernel.t
  | Subgroup_kernel of Subgroup_source.subgroup_kernel

(* The pipeline stages [--stop-at] can target. Mirrors the order in
   [translate]: each stage prints what's left after its own
   transformation runs. Listed roughly innermost-to-outermost in the
   pipeline so [--help] reads in execution order. *)
module Stage = struct
  type t =
    | Map
    | Well_formed
    | Aligned
    | Delin
    | Phase_split
    | Loc_split
    | Flat_acc
    | Symbexp

  let to_string : t -> string = function
    | Map -> "map"
    | Well_formed -> "well-formed"
    | Aligned -> "aligned"
    | Delin -> "delin"
    | Phase_split -> "phase-split"
    | Loc_split -> "loc-split"
    | Flat_acc -> "flat-acc"
    | Symbexp -> "symbexp"

  let cmdliner_choices : (string * t) list =
    [
      ("map", Map);
      ("well-formed", Well_formed);
      ("aligned", Aligned);
      ("delin", Delin);
      ("phase-split", Phase_split);
      ("loc-split", Loc_split);
      ("flat-acc", Flat_acc);
      ("symbexp", Symbexp);
    ]
end

(* Raised by [show_or_stop] when [stop_at] matches the current
   stage. Caught at the per-kernel boundary in [run] so the
   surrounding [List.map] continues to the next kernel. The
   exception unwinds the lazy stream computation cleanly: every
   downstream stage in the [|>] chain is bypassed without forcing
   further work. *)
exception Stop_at_stage

(* CLI re-export; the type and driver mapping live in [Delinearize.Algo]. *)
module Delin_algo = Delinearize.Algo

type t = {
  filename : string;
  kernels : kernel list;
  timeout : int option;
  show_proofs : bool;
  show_proto : bool;
  show_wf : bool;
  show_align : bool;
  show_delin : bool;
  show_phase_split : bool;
  show_loc_split : bool;
  show_flat_acc : bool;
  show_symbexp : bool;
  logic : string option;
  solve_tactic : Gen_z3.Tactic.t option;
  (* Per-kernel tracked-assertion clauses for UNSAT-core shrinking,
     keyed by [Kernel.name]. Each kernel's list pairs an integer ID
     with a [bexp] that gets added to that kernel's per-proof Z3
     solver via [assert_and_track] using the symbol [extra_<id>]. The
     ID space is per-kernel; the same integer in two different
     kernels refers to different clauses. Empty by default; populated
     by genie's [shrink_via_core] path. *)
  core_extras : (string * (int * Exp.bexp) list) list;
  le_index : int list;
  ge_index : int list;
  eq_index : int list;
  only_kernel : string option;
  only_array : string option;
  only_true_data_races : bool;
  subgroup_size : int option;
  thread_idx_1 : Dim3.t option;
  thread_idx_2 : Dim3.t option;
  block_idx_1 : Dim3.t option;
  block_idx_2 : Dim3.t option;
  archs : Architecture.t list;
  block_dim : Dim3.t option;
  grid_dim : Dim3.t option;
  params : (string * int) list;
  launch_contract : Launch_contract.t option;
  macros : string list;
  ignore_asserts : bool;
  (* [assume_delin] runs delin in assume mode: the recovered axis bounds
     are assumed rather than proven (the consistency oracle, unsound but
     guarded against vacuity). [rewrite_delin] re-encodes accesses as
     multidimensional subscripts (sound, performance only); it defaults
     on. Delin runs when either is set. *)
  assume_delin : bool;
  rewrite_delin : bool;
  delin_elide : bool;
  delin_algo : Delin_algo.t;
  delin_check_vacuosity : bool;
  (* Per-kernel pre-condition list, keyed by [Kernel.name]. Genie's
     internal model treats assumptions as kernel-scoped: a variable
     declared in two kernels is a different variable in each, so an
     assumption mentioning it is meaningful only against a specific
     kernel. The CLI's [--assume BEXP] is a UX shorthand that
     populates every kernel; [--assume-for K:BEXP] targets a single
     kernel by name. Look up via [assumes_of]. *)
  assumes : (string * Exp.bexp list) list;
  assume_dims : bool;
  assume_launch : bool;
  (* Opt-in Z3 pre-flight on the merged [k.pre]: when set, [run]
     SAT-checks each kernel's precondition before race goals run
     and prints a warning if it is UNSAT. An UNSAT [k.pre] makes
     every race query unsat too, so the kernel reports "is DRF"
     for the wrong reason. Defaults to false; the check costs one
     Z3 call per kernel. *)
  check_pre_sat : bool;
  memory_model : Memory_model.t;
  stop_at : Stage.t option;
}

(* The list of user-supplied (and abductive-supplied) pre-condition
   clauses for a specific kernel. Returns [[]] when no kernel of that
   name has any assumes recorded. *)
let assumes_of (k : Protocols.Kernel.t) (app : t) : Exp.bexp list =
  List.assoc_opt (Protocols.Kernel.name k) app.assumes
  |> Option.value ~default:[]

(* Replace the per-kernel assumes for [k] with [bs] (or insert if the
   kernel had no entry). All other kernels' assumes are unchanged. *)
let set_assumes_for (k : Protocols.Kernel.t) (bs : Exp.bexp list)
    (app : t) : t =
  let name = Protocols.Kernel.name k in
  let updated =
    if List.mem_assoc name app.assumes
    then List.map (fun (n, v) -> if n = name then (n, bs) else (n, v)) app.assumes
    else (name, bs) :: app.assumes
  in
  { app with assumes = updated }

(* Extend the per-kernel assumes for [k] with [extras] (concatenate). *)
let add_assumes_for (k : Protocols.Kernel.t) (extras : Exp.bexp list)
    (app : t) : t =
  set_assumes_for k (assumes_of k app @ extras) app

type parsed = { options : Gv_parser.t; kernels : kernel list }

let to_string (app : t) : string =
  let opt_s (o : string option) : string = Option.value ~default:"null" o in
  let opt : 'a. ('a -> string) -> 'a option -> string =
   fun f o -> o |> Option.map f |> opt_s
  in
  let int = string_of_int in
  let bool (b : bool) : string = if b then "true" else "false" in
  let dim3 (o : Dim3.t) : string = Dim3.to_string o in

  let list_string (l : string list) : string =
    "[" ^ String.concat ", " l ^ "]"
  in

  let list_arch (l : Architecture.t list) : string =
    list_string (List.map Architecture.to_string l)
  in
  match app with
  | {
   filename;
   kernels;
   timeout;
   show_proofs;
   show_proto;
   show_wf;
   show_align;
   show_delin;
   show_phase_split;
   show_loc_split;
   show_flat_acc;
   show_symbexp;
   logic;
   solve_tactic = _;
   core_extras = _;
   le_index = _;
   ge_index = _;
   eq_index = _;
   only_array = _;
   thread_idx_1 = _;
   block_idx_1 = _;
   thread_idx_2 = _;
   block_idx_2 = _;
   archs;
   block_dim;
   grid_dim;
   params = _;
   launch_contract;
   only_kernel;
   macros;
   only_true_data_races;
   subgroup_size;
   ignore_asserts;
   assume_delin;
   rewrite_delin;
   delin_elide;
   delin_algo;
   delin_check_vacuosity;
   assumes;
   assume_dims;
   assume_launch;
   check_pre_sat;
   memory_model;
   stop_at;
  } ->
      let only_kernel = Option.value ~default:"(null)" only_kernel in
      let kernels = List.length kernels |> string_of_int in
      let subgroup_size = subgroup_size |> Option.map string_of_int |> opt_s in
      let launch_contract =
        launch_contract
        |> Option.map (fun contract -> contract.Launch_contract.row_id)
        |> opt_s
      in
      "filename: " ^ filename ^ "\nonly_kernel: " ^ only_kernel
      ^ "\nblock_dim: " ^ opt dim3 block_dim ^ "\ngrid_dim: "
      ^ opt dim3 grid_dim ^ "\nkernels: " ^ kernels ^ "\ntimeout: "
      ^ opt int timeout ^ "\nlogic: " ^ opt_s logic ^ "\narchs: "
      ^ list_arch archs ^ "\nshow_proofs: " ^ bool show_proofs
      ^ "\nshow_proto: " ^ bool show_proto ^ "\nshow_wf: " ^ bool show_wf
      ^ "\nshow_align: " ^ bool show_align ^ "\nshow_delin: "
      ^ bool show_delin ^ "\nshow_phase_split: "
      ^ bool show_phase_split ^ "\nshow_loc_split: " ^ bool show_loc_split
      ^ "\nshow_flat_acc: " ^ bool show_flat_acc ^ "\nshow_symbexp: "
      ^ bool show_symbexp ^ "\nmacros = " ^ list_string macros
      ^ "\nonly_true_data_races = ^ " ^ bool only_true_data_races
      ^ "\nsubgroup_size = " ^ subgroup_size
      ^ "\nlaunch_contract = " ^ launch_contract
      ^ "\nassume_delin = " ^ bool assume_delin
      ^ "\nrewrite_delin = " ^ bool rewrite_delin
      ^ "\ndelin_elide = " ^ bool delin_elide
      ^ "\ndelin_algo = " ^ Delin_algo.to_string delin_algo
      ^ "\ndelin_check_vacuosity = " ^ bool delin_check_vacuosity
      ^ "\nignore_asserts = " ^ bool ignore_asserts
      ^ "\nassume_dims = " ^ bool assume_dims
      ^ "\nassume_launch = " ^ bool assume_launch
      ^ "\ncheck_pre_sat = " ^ bool check_pre_sat
      ^ "\nmemory_model = " ^ Memory_model.to_string memory_model
      ^ "\nstop_at = " ^ opt Stage.to_string stop_at
      ^ "\nassumes: "
      ^ list_string (
          assumes
          |> List.concat_map (fun (k, bs) ->
              List.map (fun b -> k ^ ":" ^ Exp.b_to_string b) bs))
      ^ "\n"

let launch_contract_error (error : Launch_contract.error) : 'a =
  Logger.Colors.error (Launch_contract.error_to_string error);
  exit 2

let require_ok (result : ('a, Launch_contract.error) result) : 'a =
  match result with
  | Ok value -> value
  | Error error -> launch_contract_error error

let parse_launch_contract (row_id : string option) : Launch_contract.t option =
  row_id
  |> Option.map (fun row_id -> require_ok (Launch_contract.of_row_id row_id))

let validate_launch_contract_options ~(all_dims : bool)
    ~(only_kernel : string option) ~(block_dim : Dim3.t option)
    ~(grid_dim : Dim3.t option) (contract : Launch_contract.t) :
    Dim3.t option * (string * int) list -> Dim3.t option * (string * int) list =
 fun (_old_block_dim, params) ->
  require_ok (Launch_contract.check_all_dims contract all_dims);
  require_ok (Launch_contract.check_only_kernel contract only_kernel);
  require_ok (Launch_contract.check_grid_dim contract grid_dim);
  let block_dim =
    require_ok (Launch_contract.check_block_dim contract block_dim)
  in
  let params = require_ok (Launch_contract.merge_params contract params) in
  (block_dim, params)

let checked_source_options ~block_dim ~grid_dim (filename : string) :
    Gv_parser.t =
  let options : Gv_parser.t =
    match Gv_parser.parse filename with
    | Some gv ->
        Logger.Colors.info
          ("Found GPUVerify args in source file: " ^ Gv_parser.to_string gv);
        gv
    | None -> Gv_parser.make ()
  in
  {
    options with
    block_dim = (match block_dim with Some b -> b | None -> options.block_dim);
    grid_dim = (match grid_dim with Some g -> g | None -> options.grid_dim);
  }

let parse_cuda_program ~abort_on_parsing_failure ~block_dim ~grid_dim ~includes
    ~macros ~cu_to_json ~ignore_asserts:_ ~launch_params ~cbor
    (filename : string) :
    Gv_parser.t * D_lang.Program.t =
  let json =
    Cu_to_json.cu_to_json
      ~ignore_fail:(not abort_on_parsing_failure)
      ~on_error:(fun _ -> exit 2)
      ~includes ~macros ~exe:cu_to_json ~launch_params ~cbor filename
  in
  let options = checked_source_options ~block_dim ~grid_dim filename in
  match C_lang.Program.parse json with
  | Ok program -> (options, D_lang.rewrite_program program)
  | Error error ->
      Rjson.print_error error;
      exit 2

let subgroup_target_config (subgroup_size : int) : SM.Target_config.t =
  match SM.Target_config.subgroup_size subgroup_size with
  | Ok size -> SM.Target_config.cuda_x_contiguous size
  | Error error ->
      Logger.Colors.error (SM.Target_config.error_to_string error);
      exit 2

let split_context_and_kernel (name : string) (program : D_lang.Program.t) :
    D_lang.Def.t list * D_lang.Kernel.t =
  let context_rev, target =
    List.fold_left
      (fun (context_rev, target) def ->
        match def with
        | D_lang.Def.Kernel kernel when String.equal kernel.name name ->
            (context_rev, Some kernel)
        | D_lang.Def.Kernel _ -> (context_rev, target)
        | D_lang.Def.Declaration _ | Typedef _ | Enum _ ->
            (def :: context_rev, target))
      ([], None) program
  in
  match target with
  | Some kernel -> (List.rev context_rev, kernel)
  | None ->
      Logger.Colors.error ("kernel '" ^ name ^ "' not found!");
      exit (-1)

let route_program ?target_config ~(only_kernel : string option)
    (program : D_lang.Program.t) :
    (Subgroup_source.routed_kernel list, Subgroup_source.error) result =
  match only_kernel with
  | Some name ->
      let context_defs, kernel = split_context_and_kernel name program in
      Subgroup_source.route_kernel ?target_config ~context_defs kernel
      |> Result.map (fun kernel -> [ kernel ])
  | None -> Subgroup_source.route_program ?target_config program

let compile_ordinary_kernels ~(inline_calls : bool) ~(ignore_asserts : bool)
    (kernels : Imp.Kernel.t list) : kernel list =
  let kernels =
    if ignore_asserts then List.map Imp.Kernel.remove_global_asserts kernels
    else kernels
  in
  kernels
  |> Imp.Compiler.compile_all ~inline_calls
  |> List.filter Protocols.Kernel.is_global
  |> List.map (fun kernel -> Ordinary_kernel kernel)

let compile_ordinary_program ~(inline_calls : bool) ~(ignore_asserts : bool)
    (program : D_lang.Program.t) : kernel list =
  let kernels =
    try D_to_imp.Silent.parse_program program
    with D_to_imp.Unsupported_source msg ->
      Logger.Colors.error msg;
      exit 2
  in
  compile_ordinary_kernels ~inline_calls ~ignore_asserts kernels

let kernels_of_routed ~(inline_calls : bool) ~(ignore_asserts : bool)
    (kernel : Subgroup_source.routed_kernel) : kernel list =
  match kernel with
  | Subgroup_source.Ordinary_imp kernel ->
      compile_ordinary_kernels ~inline_calls ~ignore_asserts [ kernel ]
  | Subgroup_source.Subgroup_matrix kernel -> [ Subgroup_kernel kernel ]

let parse_with_subgroup_config ~filename ~block_dim ~grid_dim ~includes
    ~inline_calls ~ignore_parsing_errors ~macros ~cu_to_json ~ignore_asserts
    ~assume_launch ~cbor ~only_kernel ~(subgroup_size : int) : parsed =
  if String.ends_with ~suffix:".wgsl" filename then (
    Logger.Colors.error
      "--subgroup-size is only supported for CUDA subgroup/matrix analysis.";
    exit 2);
  let options, program =
    parse_cuda_program
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~block_dim ~grid_dim ~includes ~macros ~cu_to_json ~ignore_asserts
      ~launch_params:assume_launch ~cbor
      filename
  in
  let target_config = subgroup_target_config subgroup_size in
  match route_program ~target_config ~only_kernel program with
  | Error error ->
      Logger.Colors.error (Subgroup_source.error_to_string error);
      exit 2
  | Ok routed ->
      {
        options;
        kernels =
          routed
          |> List.map (kernels_of_routed ~inline_calls ~ignore_asserts)
          |> List.concat;
      }

let subgroup_kernel_without_config (only_kernel : string option)
    (program : D_lang.Program.t) : string option =
  let context_defs =
    List.filter
      (function
        | D_lang.Def.Kernel _ -> false
        | Declaration _ | Typedef _ | Enum _ -> true)
      program
  in
  program
  |> List.find_map (function
    | D_lang.Def.Kernel kernel
      when Option.fold ~none:true
             ~some:(fun name -> String.equal name kernel.name)
             only_kernel -> (
        match Subgroup_source.route_kernel ~context_defs kernel with
        | Error (Subgroup_source.Missing_subgroup_config _) -> Some kernel.name
        | Ok _ | Error _ -> None)
    | D_lang.Def.Kernel _ | Declaration _ | Typedef _ | Enum _ -> None)

let parse_without_subgroup_config ~filename ~block_dim ~grid_dim ~includes
    ~inline_calls ~ignore_parsing_errors ~macros ~cu_to_json ~ignore_asserts
    ~assume_launch ~cbor ~only_kernel : parsed =
  if String.ends_with ~suffix:".wgsl" filename then
    let parsed =
      Protocol_parser.Silent.to_proto
        ~abort_on_parsing_failure:(not ignore_parsing_errors)
        ~includes ~block_dim ~grid_dim ~inline_calls ~macros ~cu_to_json
        ~ignore_asserts ~assume_launch ~launch_params:assume_launch ~cbor
        filename
    in
    {
      options = parsed.options;
      kernels = List.map (fun kernel -> Ordinary_kernel kernel) parsed.kernels;
    }
  else
    let options, program =
      parse_cuda_program
        ~abort_on_parsing_failure:(not ignore_parsing_errors)
        ~block_dim ~grid_dim ~includes ~macros ~cu_to_json ~ignore_asserts
        ~launch_params:assume_launch ~cbor
        filename
    in
    begin match subgroup_kernel_without_config only_kernel program with
    | None -> ()
    | Some kernel ->
        Logger.Colors.error
          (Subgroup_source.error_to_string
             (Subgroup_source.Missing_subgroup_config { kernel }));
        exit 2
    end;
    {
      options;
      kernels = compile_ordinary_program ~inline_calls ~ignore_asserts program;
    }

let parse ~filename ~timeout ~show_proofs ~show_proto ~show_wf ~show_align
    ~show_delin ~show_phase_split ~show_loc_split ~show_flat_acc ~show_symbexp
    ~logic ~solve_tactic ~ge_index ~le_index ~eq_index ~only_array ~only_kernel
    ~only_true_data_races ~thread_idx_1 ~thread_idx_2 ~block_idx_1 ~block_idx_2
    ~block_dim ~grid_dim ~includes ~inline_calls ~archs ~ignore_parsing_errors
    ~params ~macros ~cu_to_json ~all_dims ~ignore_asserts ~assume_delin
    ~rewrite_delin ~delin_elide ~delin_algo ~delin_check_vacuosity ~assumes
    ~assume_dims ~assume_launch ~check_pre_sat ~memory_model ~cbor ~stop_at
    ~subgroup_size ~launch_contract : t =
  let launch_contract = parse_launch_contract launch_contract in
  let block_dim, params =
    match launch_contract with
    | None -> (block_dim, params)
    | Some contract ->
        validate_launch_contract_options ~all_dims ~only_kernel ~block_dim
          ~grid_dim contract (block_dim, params)
  in
  let parsed =
    match subgroup_size with
    | None ->
        parse_without_subgroup_config ~filename ~block_dim ~grid_dim ~includes
          ~inline_calls ~ignore_parsing_errors ~macros ~cu_to_json
          ~ignore_asserts ~assume_launch ~cbor ~only_kernel
    | Some subgroup_size ->
        parse_with_subgroup_config ~filename ~block_dim ~grid_dim ~includes
          ~inline_calls ~ignore_parsing_errors ~macros ~cu_to_json
          ~ignore_asserts ~assume_launch ~cbor ~only_kernel ~subgroup_size
  in
  (* Uniquify duplicate kernel names so that the user-facing flag
     [--assume "K:BEXP"], the [--list-kernels] output, the per-kernel
     [assumes] map, and the genie verdict JSON all address each
     kernel by a distinct identifier. *)
  let uniquify_mixed_kernels (kernels : kernel list) : kernel list =
    let legacy =
      kernels
      |> List.filter_map (function
        | Ordinary_kernel kernel -> Some kernel
        | Subgroup_kernel _ -> None)
      |> Protocols.Kernel.uniquify_names
    in
    let rec rebuild legacy kernels =
      match (legacy, kernels) with
      | _, [] -> []
      | kernel :: legacy, Ordinary_kernel _ :: kernels ->
          Ordinary_kernel kernel :: rebuild legacy kernels
      | legacy, Subgroup_kernel kernel :: kernels ->
          Subgroup_kernel kernel :: rebuild legacy kernels
      | [], Ordinary_kernel _ :: _ ->
          failwith "internal error: missing uniquified legacy kernel"
    in
    rebuild legacy kernels
  in
  let kernels =
    match launch_contract with
    | None -> parsed.kernels
    | Some contract ->
        parsed.kernels
        |> List.map (function
          | Ordinary_kernel kernel ->
              Ordinary_kernel
                (require_ok (Launch_contract.apply_to_kernel contract kernel))
          | Subgroup_kernel kernel ->
              launch_contract_error
                (Launch_contract.Subgroup_kernel_unsupported
                   kernel.matrix_kernel.name))
  in
  let kernels = uniquify_mixed_kernels kernels in
  let block_dim = if all_dims then None else Some parsed.options.block_dim in
  let block_dim =
    match launch_contract with
    | None -> block_dim
    | Some contract -> Some (Launch_contract.block_dim contract)
  in
  let grid_dim =
    match launch_contract with
    | None -> if all_dims then None else Some parsed.options.grid_dim
    | Some _ -> None
  in
  (* [assumes] entries are [(kernel_name option, bexp)]. A [None]
     prefix means "apply to every kernel that can take this clause"
     — i.e., every kernel whose declared params plus the
     launch-config dims cover the clause's free variables. A [Some n]
     prefix restricts to kernel [n]; absent names are silently
     dropped. *)
  let kernel_has_vars (k : Protocols.Kernel.t) (b : Exp.bexp) : bool =
    let fvs = Exp.b_free_names b Variable.Set.empty in
    let p =
      Protocols.Params.union_left k.global_variables k.local_variables
    in
    Variable.Set.for_all (fun v ->
      Variable.is_launch_config v || Protocols.Params.mem v p)
      fvs
  in
  let assumes : (string * Exp.bexp list) list =
    List.filter_map (function
      | Subgroup_kernel _ -> None
      | Ordinary_kernel (k : Protocols.Kernel.t) ->
      let kn = Protocols.Kernel.name k in
      let entries =
        List.filter_map (fun (prefix, b) ->
          match prefix with
          | None ->
            (* Global clause: include only if [k] can take it. *)
            if kernel_has_vars k b then Some b else None
          | Some n ->
            if n = kn then Some b else None)
          assumes
      in
      Some (kn, entries))
      kernels
  in
  {
    filename;
    timeout;
    show_proofs;
    show_proto;
    show_wf;
    show_align;
    show_delin;
    show_phase_split;
    show_loc_split;
    show_flat_acc;
    show_symbexp;
    logic;
    solve_tactic;
    core_extras = [];
    kernels;
    ge_index;
    le_index;
    eq_index;
    only_array;
    thread_idx_1;
    thread_idx_2;
    block_idx_1;
    block_idx_2;
    archs;
    grid_dim;
    block_dim;
    params;
    launch_contract;
    only_kernel;
    only_true_data_races;
    subgroup_size;
    macros;
    ignore_asserts;
    assume_delin;
    rewrite_delin;
    delin_elide;
    delin_algo;
    delin_check_vacuosity;
    assumes;
    assume_dims;
    assume_launch;
    (* Assume mode forces the pre-condition SAT pre-flight: an assumed
       bound must never make a kernel vacuously DRF. *)
    check_pre_sat = check_pre_sat || assume_delin;
    memory_model;
    stop_at;
  }

let show (b : bool) (call : 'a -> unit) (x : 'a) : 'a =
  if b then call x else ();
  x

(* [show_or_stop] is the [--stop-at]-aware sibling of [show]. It
   prints when either (a) the matching [--show-X] flag is set, or
   (b) [stop_at] names this stage; then raises [Stop_at_stage] in
   case (b) to unwind the rest of the pipeline. The exception is
   caught at the per-kernel boundary in [run]. *)
let show_or_stop ~(stop_at : Stage.t option) ~(stage : Stage.t)
    ~(show : bool) (call : 'a -> unit) (x : 'a) : 'a =
  let matched = stop_at = Some stage in
  if show || matched then call x;
  if matched then raise Stop_at_stage else x

(* Steps 0-1.2 of [translate]: apply array filtering, dim pinning,
   architecture defaults, user [--assume] clauses, and (when
   [--assume-dims] is on) the unused-dim pin-to-1 assumptions. After
   this prefix the kernel's [pre] is the full merged precondition
   that downstream race checks will assume. Factored out so [run]
   can ask "is this precondition satisfiable?" before kicking off
   the rest of the pipeline; without that pre-flight, an unsat [pre]
   produces a silent "kernel is DRF" verdict because every race goal
   inherits the contradiction. *)
let prepare_pre (arch : Architecture.t) (a : t) (k : Kernel.t) : Kernel.t =
  k
  (* 0. filter arrays *)
  |> (fun k ->
    match a.only_array with
    | Some arr -> Protocols.Kernel.filter_array (fun x -> Variable.name x = arr) k
    | None -> k)
  (* 1. apply block-level/grid-level analysis constraints and set dimensions *)
  |> Protocols.Kernel.try_set_block_dim a.block_dim
  |> Protocols.Kernel.try_set_grid_dim a.grid_dim
  |> Protocols.Kernel.apply_arch arch
  (* 1.1 inject user-provided assumptions into the kernel precondition.
     Look up per-kernel; an absent entry means no extra assumes. *)
  |> (fun k ->
    let assumes =
      List.assoc_opt (Protocols.Kernel.name k) a.assumes
      |> Option.value ~default:[]
    in
    List.fold_left (fun k b -> Protocols.Kernel.add_pre b k) k assumes)
  (* 1.2 optionally pin unreferenced launch dimensions to 1 *)
  |> (if a.assume_dims then Protocols.Kernel.add_dim_assumptions else Fun.id)

let translate (arch : Architecture.t) (a : t) (k : Kernel.t) :
    Flatacc.Kernel.t Streamutil.stream =
  (* The "map" phase is single-kernel work (no stream), so we wrap it
     in [Phase_timer.measure] rather than using [boundary]. The
     constant-folding [Kernel.opt] is folded in too: it runs eagerly on
     the kernel before [Wellformed.translate] turns it into a stream,
     so attributing it to "map" keeps "well-formed" measuring only the
     stream-producing work. *)
  let k =
    Phase_timer.measure "map" (fun () ->
      k
      |> prepare_pre arch a
      (* 2. inline global assignments, including block_dim/grid_dim *)
      |> Protocols.Kernel.inline_globals a.params
      (* 2.1 inline block_id as a constant when architecture is Grid *)
      |> (fun k ->
      match (arch, a.block_idx_1) with
      | Architecture.Block, Some bid ->
          let kvs = Dim3.to_assoc ~prefix:"blockIdx." bid in
          Protocols.Kernel.assign_globals kvs k
      | _, _ -> k)
      |> Protocols.Kernel.add_missing_binders
      |> (if a.only_true_data_races then Protocols.Kernel.to_ci_di else Fun.id)
      |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Map ~show:a.show_proto
           Protocols.Kernel.print
      (* 3. constant folding optimization *)
      |> Protocols.Kernel.opt)
  in
  k
  (* 4. convert to well-formed protocol *)
  |> Wellformed.translate
  (* 4.1. remove unnecessary binders *)
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> Phase_timer.boundary "well-formed"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Well_formed
       ~show:a.show_wf Wellformed.print_kernels
  (* 5. align protocol *)
  |> Aligned.translate
  |> Phase_timer.boundary "aligned"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Aligned
       ~show:a.show_align Aligned.print_kernels
  (* 6. delinearize accesses *)
  |> Delinearize.translate ~enabled:a.assume_delin ~rewrite:a.rewrite_delin
       ~elide:a.delin_elide ~check_vacuosity:a.delin_check_vacuosity
       ~algo:a.delin_algo
  |> Phase_timer.boundary "delin"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Delin
       ~show:a.show_delin Aligned.print_kernels
  (* 7. split per sync *)
  |> Phasesplit.translate
  |> Phase_timer.boundary "phase-split"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Phase_split
       ~show:a.show_phase_split Phasesplit.print_kernels
  (* 8. split per location *)
  |> Locsplit.translate
  |> Phase_timer.boundary "loc-split"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Loc_split
       ~show:a.show_loc_split Locsplit.print_kernels
  (* 9. flatten control-flow structures *)
  |> Flatacc.translate arch
  |> Phase_timer.boundary "flat-acc"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Flat_acc
       ~show:a.show_flat_acc Flatacc.print_kernels

let kernel_name : kernel -> string = function
  | Ordinary_kernel kernel -> Protocols.Kernel.name kernel
  | Subgroup_kernel kernel -> kernel.matrix_kernel.name

let only_kernel (a : t) (ks : kernel list) : kernel list =
  match a.only_kernel with
  | Some name ->
      let ks = ks |> List.filter (fun k -> String.equal (kernel_name k) name) in
      if ks = [] then (
        Logger.Colors.error (fun () -> "kernel '" ^ name ^ "' not found!");
        exit (-1))
      else ks
  | None -> ks

let run (a : t) : Analysis.t list =
  let check_ordinary_kernel arch (kernel : Protocols.Kernel.t) :
      Analysis.ordinary =
    let report =
      kernel |> translate arch a
      |> Memory_event.translate arch
      |> Symbexp.add_rel_index N_rel.Le a.le_index
      |> Symbexp.add_rel_index N_rel.Ge a.ge_index
      |> Symbexp.add_rel_index N_rel.Eq a.eq_index
      |> Symbexp.add ~tid:a.thread_idx_1 ~bid:a.block_idx_1
      |> Symbexp.add ~tid:a.thread_idx_2 ~bid:a.block_idx_2
      |> Phase_timer.boundary "symbexp"
      |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Symbexp
           ~show:a.show_symbexp Symbexp.print_kernels
      |> (fun ps ->
          let kernel_extras =
            List.assoc_opt (Protocols.Kernel.name kernel) a.core_extras
            |> Option.value ~default:[]
          in
          Solve_drf.Solution.solve ~timeout:a.timeout
            ~show_proofs:a.show_proofs ~logic:a.logic
            ~solve_tactic:a.solve_tactic ~extras:kernel_extras ps)
      |> Phase_timer.boundary "solve"
      |> Streamutil.to_list
    in
    Analysis.{ kernel; report; vacuous = None }
  in
  let check_subgroup_kernel (subgroup : Subgroup_source.subgroup_kernel) :
      Analysis.subgroup =
    let kernel = subgroup.matrix_kernel in
    let config =
      Subgroup_solver.solver_config ?timeout_ms:a.timeout ?logic:a.logic ()
    in
    let memory =
      let globals = subgroup.memory_globals in
      kernel
      |> Subgroup_obligation.obligations ~globals ?block_dim:a.block_dim
           ~site_controls:subgroup.site_controls
           ~ordinary_memory_effects:subgroup.ordinary_memory_effects
      |> Subgroup_solver.solve_obligation_result ~config ~globals
           ?block_dim:a.block_dim ~target_config:kernel.target_config
           ~kernel_name:kernel.name
    in
    let uniformity =
      let site_controls =
        List.map
          (fun (control : Subgroup_source.site_control) ->
            ( control.site_id,
              Subgroup_uniformity.control_with_uniform_vars
                ~conditions:control.conditions
                ~uniform_vars:control.uniform_vars ))
          subgroup.site_controls
      in
      match
        Subgroup_uniformity.check_kernel ~site_controls
          ~uniform_vars:subgroup.uniform_vars kernel
      with
      | Ok result -> result
      | Error error ->
          Logger.Colors.error
            ("subgroup uniformity configuration error: "
            ^ Subgroup_uniformity.error_to_string error);
          exit 2
    in
    Analysis.{ kernel; memory; uniformity }
  in
  a.kernels |> only_kernel a
  |> List.map (function
    | Subgroup_kernel kernel -> Analysis.Subgroup (check_subgroup_kernel kernel)
    | Ordinary_kernel kernel ->
        let vacuous : Exp.bexp option =
          if not a.check_pre_sat then None
          else
            match a.archs with
            | arch :: _ ->
                let prepared = prepare_pre arch a kernel in
                if
                  Phase_timer.measure "pre-sat" (fun () ->
                    Gen_z3.is_unsat ~timeout:a.timeout ~logic:a.logic
                      prepared.pre)
                then Some prepared.pre
                else None
            | [] -> None
        in
        match vacuous with
        | Some _ -> Analysis.Ordinary { kernel; report = []; vacuous }
        | None -> (
        let rec check_until (archs : Architecture.t list) : Analysis.t =
          match archs with
          | [] -> Analysis.Ordinary { kernel; report = []; vacuous = None }
          | [ arch ] -> Analysis.Ordinary (check_ordinary_kernel arch kernel)
          | arch :: archs ->
              let ordinary = check_ordinary_kernel arch kernel in
              if Analysis.ordinary_is_safe ordinary then check_until archs
              else Analysis.Ordinary ordinary
        in
        try check_until a.archs
        with Stop_at_stage ->
          Analysis.Ordinary { kernel; report = []; vacuous = None }))
