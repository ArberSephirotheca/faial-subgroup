open Stage0
open Protocols
module App_analysis = Analysis
open Drf
open Inference
module SM = Subgroup_matrix
module Subgroup_solver = Drf.Subgroup_solver
module Subgroup_uniformity = Drf.Subgroup_uniformity
module Subgroup_uniformity_solver = Drf.Subgroup_uniformity_solver
module Subgroup_obligation = Drf.Memory_event.Subgroup_obligation
module StringMap = Common.StringMap

type subgroup_kernel = {
  subgroup : Subgroup_source.subgroup_kernel;
  loop_protocol : Protocols.Kernel.t;
}

type kernel =
  | Ordinary_kernel of Protocols.Kernel.t
  | Subgroup_kernel of subgroup_kernel

let kernel_name : kernel -> string = function
  | Ordinary_kernel kernel -> Protocols.Kernel.name kernel
  | Subgroup_kernel kernel -> kernel.subgroup.matrix_kernel.name

let kernel_protocol : kernel -> Protocols.Kernel.t = function
  | Ordinary_kernel kernel -> kernel
  | Subgroup_kernel kernel -> kernel.loop_protocol

let kernel_signature kernel =
  Protocols.Kernel.signature_string (kernel_protocol kernel)

let with_kernel_name (name : string) : kernel -> kernel = function
  | Ordinary_kernel kernel -> Ordinary_kernel { kernel with name }
  | Subgroup_kernel kernel ->
      let matrix_kernel =
        SM.Kernel.make
          ~target_config:kernel.subgroup.matrix_kernel.target_config ~name
          kernel.subgroup.matrix_kernel.body
      in
      let subgroup = { kernel.subgroup with matrix_kernel } in
      let loop_protocol = { kernel.loop_protocol with name } in
      Subgroup_kernel { subgroup; loop_protocol }

(* Kernel enumeration and [--kernel] selection must use one identifier space.
   Apply the same collision policy to ordinary and subgroup kernels in source
   order so a token printed by [--list-kernels] always replays exactly. *)
let uniquify_kernel_names (kernels : kernel list) : kernel list =
  let module SS = Common.StringSet in
  let initial =
    List.fold_left
      (fun names kernel -> SS.add (kernel_name kernel) names)
      SS.empty kernels
  in
  let used = ref SS.empty in
  List.map
    (fun kernel ->
      let name = kernel_name kernel in
      if not (SS.mem name !used) then (
        used := SS.add name !used;
        kernel)
      else
        let rec fresh suffix =
          let candidate = Printf.sprintf "%s_%d" name suffix in
          if SS.mem candidate !used || SS.mem candidate initial then
            fresh (suffix + 1)
          else candidate
        in
        let name = fresh 2 in
        used := SS.add name !used;
        with_kernel_name name kernel)
    kernels

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

(* Raised by [only_kernel] when [--only-kernel NAME] matches no kernel
   in the translation unit. It is an argument error, so the CLI layer
   ([main]/[genie]) catches it and reports it through cmdliner's
   [Error msg] channel rather than letting it exit as a raw failure. *)
exception Kernel_not_found of string

(* Raised when a [--assume] clause cannot be applied to a kernel (an
   unknown binder, an ambiguous binder, or a clause that would leave an
   unbound name). [run] validates every assumption against every kernel
   before any analysis, so a bad clause aborts the whole run up front. *)
exception Assumption_error of string

(* CLI re-export; the type and driver mapping live in [Delinearize.Algo]. *)
module Delin_algo = Delinearize.Algo
module Opaque_calls = Opaque_call_policy

type t = {
  filenames : string list;
  kernels : kernel list;
  rejected : Imp.Rejected_kernel.t list;
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
  deterministic_sat : bool;
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
  opaque_calls : Opaque_call_policy.t;
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
  delin_weak_in_range : bool;
  delin_weak_in_range_for : string list;
  (* User [--assume] clauses. Each [Assumption.t] carries its own kernel
     filter and target (the precondition, or a specific binder); [run]
     applies them to every matching kernel via [Assumption.add_to_kernel].
     genie appends its own [Target.Pre] clauses to this list and re-runs. *)
  assumptions : Assumption.t list;
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

type parsed = {
  options : Gv_parser.t;
  kernels : kernel list;
  rejected : Imp.Rejected_kernel.t list;
}

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
   filenames;
   kernels;
   rejected = _;
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
   deterministic_sat = _;
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
   opaque_calls;
   assume_delin;
   rewrite_delin;
   delin_elide;
   delin_algo;
   delin_check_vacuosity;
   delin_weak_in_range;
   delin_weak_in_range_for;
   assumptions;
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
      "filenames: " ^ list_string filenames ^ "\nonly_kernel: " ^ only_kernel
      ^ "\nblock_dim: " ^ opt dim3 block_dim ^ "\ngrid_dim: "
      ^ opt dim3 grid_dim ^ "\nkernels: " ^ kernels ^ "\ntimeout: "
      ^ opt int timeout ^ "\nlogic: " ^ opt_s logic ^ "\narchs: "
      ^ list_arch archs ^ "\nshow_proofs: " ^ bool show_proofs
      ^ "\nshow_proto: " ^ bool show_proto ^ "\nshow_wf: " ^ bool show_wf
      ^ "\nshow_align: " ^ bool show_align ^ "\nshow_delin: " ^ bool show_delin
      ^ "\nshow_phase_split: " ^ bool show_phase_split ^ "\nshow_loc_split: "
      ^ bool show_loc_split ^ "\nshow_flat_acc: " ^ bool show_flat_acc
      ^ "\nshow_symbexp: " ^ bool show_symbexp ^ "\nmacros = "
      ^ list_string macros ^ "\nonly_true_data_races = ^ "
      ^ bool only_true_data_races ^ "\nsubgroup_size = " ^ subgroup_size
      ^ "\nlaunch_contract = " ^ launch_contract ^ "\nassume_delin = "
      ^ bool assume_delin ^ "\nrewrite_delin = " ^ bool rewrite_delin
      ^ "\ndelin_elide = " ^ bool delin_elide ^ "\ndelin_algo = "
      ^ Delin_algo.to_string delin_algo
      ^ "\ndelin_check_vacuosity = " ^ bool delin_check_vacuosity
      ^ "\ndelin_weak_in_range = " ^ bool delin_weak_in_range
      ^ "\nignore_asserts = " ^ bool ignore_asserts ^ "\nassume_dims = "
      ^ bool assume_dims ^ "\nassume_launch = " ^ bool assume_launch
      ^ "\ncheck_pre_sat = " ^ bool check_pre_sat ^ "\nmemory_model = "
      ^ Memory_model.to_string memory_model
      ^ "\nstop_at = "
      ^ opt Stage.to_string stop_at
      ^ "\ndelin_weak_in_range_for = "
      ^ list_string delin_weak_in_range_for
      ^ "\nopaque_calls = "
      ^ Opaque_calls.to_string opaque_calls
      ^ "\nassumptions: "
      ^ list_string (List.map Assumption.to_string assumptions)
      ^ "\n"

let launch_contract_error (error : Launch_contract.error) : 'a =
  Logger.Colors.error (fun () -> Launch_contract.error_to_string error);
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
        Logger.Colors.info (fun () ->
            "Found GPUVerify args in source file: " ^ Gv_parser.to_string gv);
        gv
    | None -> Gv_parser.make ()
  in
  {
    options with
    block_dim = (match block_dim with Some b -> b | None -> options.block_dim);
    grid_dim = (match grid_dim with Some g -> g | None -> options.grid_dim);
  }

let load_cjson ~exit_status (filename : string) : Yojson.Basic.t =
  let raw =
    Phase_timer.measure "inference/cjson-load" (fun () ->
        try In_channel.with_open_text filename In_channel.input_all
        with Sys_error error ->
          prerr_endline ("cjson: " ^ error);
          exit exit_status)
  in
  Phase_timer.measure "inference/yojson-parse" (fun () ->
      try Yojson.Basic.from_string raw
      with Yojson.Json_error error ->
        prerr_endline ("cjson: invalid JSON in " ^ filename ^ ": " ^ error);
        exit exit_status)

let parse_cuda_json ~assume_launch (json : Yojson.Basic.t) :
    D_lang.Program.t * Common.StringSet.t =
  match C_lang.Program.parse json with
  | Ok program ->
      let program = D_lang.rewrite_program program in
      let launch_wrappers =
        if not assume_launch then Common.StringSet.empty
        else
          List.fold_left
            (fun names -> function
              | D_lang.Def.LaunchParam launch ->
                  Common.StringSet.add
                    (Synthesise_launches.synth_name launch)
                    names
              | D_lang.Def.Kernel _ | Declaration _ | Typedef _ | Enum _
              | Prototype _ | Record _ | UsingNamespace _ ->
                  names)
            Common.StringSet.empty program
      in
      let program =
        if assume_launch then Synthesise_launches.rewrite_program program
        else program
      in
      (program, launch_wrappers)
  | Error error ->
      Rjson.print_error error;
      exit 2

let parse_cuda_program ~abort_on_parsing_failure ~block_dim ~grid_dim ~includes
    ~macros ~cu_to_json ~ignore_asserts:_ ~launch_params ~cbor ~extra_files
    (filename : string) : Gv_parser.t * D_lang.Program.t * Common.StringSet.t =
  let json, options =
    if String.ends_with ~suffix:".cjson" filename then
      ( load_cjson ~exit_status:2 filename,
        checked_source_options ~block_dim ~grid_dim filename )
    else
      let json =
        Cu_to_json.cu_to_json
          ~ignore_fail:(not abort_on_parsing_failure)
          ~on_error:(fun _ -> exit 2)
          ~includes ~macros ~exe:cu_to_json ~launch_params ~cbor
          (filename :: extra_files)
      in
      (json, checked_source_options ~block_dim ~grid_dim filename)
  in
  let program, launch_wrappers =
    parse_cuda_json ~assume_launch:launch_params json
  in
  (options, program, launch_wrappers)

let subgroup_target_config (subgroup_size : int) : SM.Target_config.t =
  match SM.Target_config.subgroup_size subgroup_size with
  | Ok size -> SM.Target_config.cuda_x_contiguous size
  | Error error ->
      Logger.Colors.error (fun () -> SM.Target_config.error_to_string error);
      exit 2

let compile_original_ordinary_program ~opaque_calls ~infer_cond_bound
    ~(ignore_asserts : bool) ~(rules : Exp_match.rule list)
    (options : Gv_parser.t) (program : D_lang.Program.t) :
    kernel StringMap.t * Imp.Rejected_kernel.t list =
  let parsed =
    Protocol_parser.Silent.d_program_to_proto ~opaque_calls ~infer_cond_bound
      ~ignore_asserts ~rules options program
  in
  let kernels =
    parsed.kernels
    |> List.map (fun kernel -> Ordinary_kernel kernel)
    |> uniquify_kernel_names
    |> List.fold_left
         (fun kernels kernel ->
           StringMap.add (kernel_name kernel) kernel kernels)
         StringMap.empty
  in
  (kernels, parsed.rejected)

let kernels_of_routed ~rejected ~(ordinary_kernels : kernel StringMap.t)
    (kernel : Subgroup_source.routed_kernel) : kernel list =
  let rejected_name name =
    List.exists (fun (r : Imp.Rejected_kernel.t) -> r.kernel = name) rejected
  in
  match kernel with
  | Subgroup_source.Ordinary_source source -> (
      match
        StringMap.find_opt (D_lang.Kernel.label source) ordinary_kernels
      with
      | Some kernel -> [ kernel ]
      | None when rejected_name (D_lang.Kernel.label source) -> []
      | None ->
          Logger.Colors.error (fun () ->
              Printf.sprintf
                "ordinary route '%s' is missing from the original Faial \
                 pipeline output"
                (D_lang.Kernel.label source));
          exit 2)
  | Subgroup_source.Subgroup_matrix subgroup -> (
      match StringMap.find_opt subgroup.matrix_kernel.name ordinary_kernels with
      | Some (Ordinary_kernel loop_protocol) ->
          [ Subgroup_kernel { subgroup; loop_protocol } ]
      | None when rejected_name subgroup.matrix_kernel.name -> []
      | Some (Subgroup_kernel _) | None ->
          Logger.Colors.error (fun () ->
              Printf.sprintf
                "subgroup route '%s' is missing its loop-aware Faial protocol"
                subgroup.matrix_kernel.name);
          exit 2)

let parse_with_subgroup_config ~filename ~block_dim ~grid_dim ~includes
    ~opaque_calls ~infer_cond_bound ~extra_files ~ignore_parsing_errors ~macros
    ~cu_to_json ~ignore_asserts ~assume_launch ~cbor ~only_kernel ~rules
    ~(subgroup_size : int) : parsed =
  if String.ends_with ~suffix:".wgsl" filename then (
    Logger.Colors.error (fun () ->
        "--subgroup-size is only supported for CUDA subgroup/matrix analysis.");
    exit 2);
  let includes = Cu_to_json.default_include_dirs () @ includes in
  let options, program, launch_wrappers =
    parse_cuda_program
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~block_dim ~grid_dim ~includes ~macros ~cu_to_json ~ignore_asserts
      ~launch_params:assume_launch ~cbor ~extra_files filename
  in
  let target_config = subgroup_target_config subgroup_size in
  match
    Subgroup_source.route_program ~target_config ?only_kernel ~launch_wrappers
      program
  with
  | Error error ->
      Logger.Colors.error (fun () -> Subgroup_source.error_to_string error);
      exit 2
  | Ok routed ->
      let ordinary_kernels, rejected =
        compile_original_ordinary_program ~opaque_calls ~infer_cond_bound
          ~ignore_asserts ~rules options program
      in
      {
        options;
        rejected;
        kernels =
          routed
          |> List.map (kernels_of_routed ~rejected ~ordinary_kernels)
          |> List.concat;
      }

let parse_without_subgroup_config ~filename ~block_dim ~grid_dim ~includes
    ~opaque_calls ~infer_cond_bound ~extra_files ~ignore_parsing_errors ~macros
    ~cu_to_json ~ignore_asserts ~assume_launch ~cbor ~rules : parsed =
  let parsed =
    Phase_timer.measure "inference" (fun () ->
        Protocol_parser.Silent.to_proto ~rules ~infer_cond_bound ~opaque_calls
          ~extra_files
          ~abort_on_parsing_failure:(not ignore_parsing_errors)
          ~includes ~block_dim ~grid_dim ~macros ~cu_to_json ~ignore_asserts
          ~assume_launch ~launch_params:assume_launch ~cbor filename)
  in
  {
    options = parsed.options;
    rejected = parsed.rejected;
    kernels = List.map (fun kernel -> Ordinary_kernel kernel) parsed.kernels;
  }

let parse ~extra_files ~filename ~timeout ~show_proofs ~show_proto ~show_wf
    ~show_align ~show_delin ~show_phase_split ~show_loc_split ~show_flat_acc
    ~show_symbexp ~logic ~solve_tactic ~deterministic_sat ~ge_index ~le_index
    ~eq_index ~only_array ~only_kernel ~only_true_data_races ~thread_idx_1
    ~thread_idx_2 ~block_idx_1 ~block_idx_2 ~block_dim ~grid_dim ~includes
    ~opaque_calls ~infer_cond_bound ~archs ~ignore_parsing_errors ~params
    ~macros ~cu_to_json ~all_dims ~ignore_asserts ~assume_delin ~rewrite_delin
    ~delin_elide ~delin_algo ~delin_check_vacuosity ~delin_weak_in_range
    ~delin_weak_in_range_for ~assumptions ~assume_dims ~assume_launch
    ~check_pre_sat ~memory_model ~cbor ~stop_at ~subgroup_size ~launch_contract
    ~rules_file : t =
  let rules =
    match rules_file with
    | None -> Imp.Idiom_rewrite.all
    | Some path -> (
        let text = In_channel.with_open_text path In_channel.input_all in
        match Imp.Idiom_rewrite.parse text with
        | Ok custom_rules -> Imp.Idiom_rewrite.all @ custom_rules
        | Error message ->
            prerr_endline ("--rules " ^ path ^ ": " ^ message);
            exit 2)
  in
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
          ~opaque_calls ~infer_cond_bound ~extra_files ~ignore_parsing_errors
          ~macros ~cu_to_json ~ignore_asserts ~assume_launch ~cbor ~rules
    | Some subgroup_size ->
        parse_with_subgroup_config ~filename ~block_dim ~grid_dim ~includes
          ~opaque_calls ~infer_cond_bound ~extra_files ~ignore_parsing_errors
          ~macros ~cu_to_json ~ignore_asserts ~assume_launch ~cbor ~only_kernel
          ~rules ~subgroup_size
  in
  let kernels =
    match launch_contract with
    | None -> parsed.kernels
    | Some contract ->
        let selected =
          parsed.kernels
          |> List.filter (function
            | Ordinary_kernel kernel ->
                String.equal
                  (Protocols.Kernel.name kernel)
                  contract.parsed_kernel
            | Subgroup_kernel kernel ->
                String.equal kernel.subgroup.matrix_kernel.name
                  contract.parsed_kernel)
        in
        if List.length selected = 0 then
          let actual =
            parsed.kernels
            |> List.map (function
              | Ordinary_kernel kernel -> Protocols.Kernel.name kernel
              | Subgroup_kernel kernel -> kernel.subgroup.matrix_kernel.name)
            |> String.concat ", "
          in
          launch_contract_error
            (Launch_contract.Kernel_mismatch
               { expected = contract.parsed_kernel; actual })
        else
          selected
          |> List.map (function
            | Ordinary_kernel kernel
              when Launch_contract.requires_subgroup_route contract ->
                launch_contract_error
                  (Launch_contract.Subgroup_route_required
                     (Protocols.Kernel.name kernel))
            | Ordinary_kernel kernel ->
                Ordinary_kernel
                  (require_ok (Launch_contract.apply_to_kernel contract kernel))
            | Subgroup_kernel kernel
              when Launch_contract.allows_subgroup_route contract ~subgroup_size
              ->
                Subgroup_kernel kernel
            | Subgroup_kernel kernel ->
                launch_contract_error
                  (Launch_contract.Subgroup_kernel_unsupported
                     kernel.subgroup.matrix_kernel.name))
  in
  let kernels = uniquify_kernel_names kernels in
  let block_dim = if all_dims then None else Some parsed.options.block_dim in
  let block_dim =
    match launch_contract with
    | None -> block_dim
    | Some contract -> Launch_contract.block_dim_option contract
  in
  let grid_dim =
    match launch_contract with
    | None -> if all_dims then None else Some parsed.options.grid_dim
    | Some _ -> None
  in
  let rejected =
    parsed.rejected
    |> Common.uniquify
         ~name:(fun (r : Imp.Rejected_kernel.t) -> r.kernel)
         ~rename:(fun (r : Imp.Rejected_kernel.t) kernel -> { r with kernel })
         ~taken:(List.map kernel_name kernels |> Common.StringSet.of_list)
  in
  {
    filenames = filename :: extra_files;
    rejected;
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
    deterministic_sat;
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
    opaque_calls;
    assume_delin;
    rewrite_delin;
    delin_elide;
    delin_algo;
    delin_check_vacuosity;
    delin_weak_in_range;
    delin_weak_in_range_for;
    assumptions;
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
let show_or_stop ~(stop_at : Stage.t option) ~(stage : Stage.t) ~(show : bool)
    (call : 'a -> unit) (x : 'a) : 'a =
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
  (* 1.1 apply user-provided assumptions. Each clause conjoins onto the
     precondition or a specific binder, filtered by its own kernel scope;
     a clause that fails to apply (unknown/ambiguous binder, or an unbound
     name introduced) raises [Assumption_error]. *)
  |> (fun k ->
  List.fold_left
    (fun k assumption ->
      match Assumption.add_to_kernel assumption k with
      | Ok k -> k
      | Error msg -> raise (Assumption_error msg))
    k a.assumptions)
  (* 1.2 optionally pin unreferenced launch dimensions to 1 *)
  |> if a.assume_dims then Protocols.Kernel.add_dim_assumptions else Fun.id

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
        k |> prepare_pre arch a
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
  let weak_in_range =
    a.delin_weak_in_range
    || List.mem (Protocols.Kernel.name k) a.delin_weak_in_range_for
  in
  k
  (* 4. convert to well-formed protocol *)
  |> Wellformed.translate
  (* 4.1. remove unnecessary binders *)
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> Phase_timer.boundary "well-formed"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Well_formed ~show:a.show_wf
       Wellformed.print_kernels
  (* 5. align protocol *)
  |> Aligned.translate
  |> Phase_timer.boundary "aligned"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Aligned ~show:a.show_align
       Aligned.print_kernels
  (* 6. delinearize accesses *)
  |> Delinearize.translate ~enabled:a.assume_delin ~rewrite:a.rewrite_delin
       ~elide:a.delin_elide ~check_vacuosity:a.delin_check_vacuosity
       ~algo:a.delin_algo ~weak_in_range
  |> Phase_timer.boundary "delin"
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Delin ~show:a.show_delin
       Aligned.print_kernels
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
  |> show_or_stop ~stop_at:a.stop_at ~stage:Stage.Flat_acc ~show:a.show_flat_acc
       Flatacc.print_kernels

let only_kernel (a : t) (ks : kernel list) : kernel list =
  match a.only_kernel with
  | Some name ->
      let ks = ks |> List.filter (fun k -> String.equal (kernel_name k) name) in
      if ks <> [] then ks
      else if
        List.exists
          (fun (r : Imp.Rejected_kernel.t) -> r.kernel = name)
          a.rejected
      then []
      else raise (Kernel_not_found name)
  | None -> ks

let only_rejected (a : t) : Imp.Rejected_kernel.t list =
  match a.only_kernel with
  | Some name ->
      List.filter
        (fun (r : Imp.Rejected_kernel.t) -> r.kernel = name)
        a.rejected
  | None -> a.rejected

module Listing = struct
  type entry = Analysable of kernel | Discarded of Imp.Rejected_kernel.t

  let name = function Analysable k -> kernel_name k | Discarded r -> r.kernel

  let of_app (a : t) =
    List.map (fun k -> Analysable k) a.kernels
    @ List.map (fun r -> Discarded r) a.rejected
    |> List.sort (fun x y -> String.compare (name x) (name y))
end

let subgroup_assumptions (a : t) (routed : subgroup_kernel) : Exp.bexp =
  a.assumptions
  |> List.filter_map (fun (assumption : Assumption.t) ->
      let applies =
        match assumption.kernel with
        | Assumption.Match.Any -> true
        | Assumption.Match.Exact name ->
            name = routed.subgroup.matrix_kernel.name
      in
      if not applies then None
      else
        match assumption.target with
        | Assumption.Target.Pre -> Some assumption.bexp
        | Assumption.Target.Binder _ ->
            raise
              (Assumption_error
                 "binder-scoped --assume is not supported by subgroup \
                  analysis; use a kernel precondition or analyze without \
                  --subgroup-size"))
  |> Exp.b_and_ex

let run (a : t) : App_analysis.t list =
  let kernels = a.kernels |> only_kernel a in
  if
    Option.is_some a.stop_at
    && List.exists
         (function Subgroup_kernel _ -> true | Ordinary_kernel _ -> false)
         kernels
  then
    raise
      (Assumption_error
         "--stop-at is only supported for ordinary kernels, not subgroup \
          analysis");
  (match a.archs with
  | arch :: _ ->
      List.iter
        (fun kernel ->
          ignore (prepare_pre arch a (kernel_protocol kernel));
          match kernel with
          | Ordinary_kernel _ -> ()
          | Subgroup_kernel routed -> ignore (subgroup_assumptions a routed))
        kernels
  | [] -> ());
  let check_ordinary_kernel (options : t) arch (kernel : Protocols.Kernel.t) :
      App_analysis.ordinary =
    let report =
      kernel |> translate arch options
      |> Symbexp.translate ~memory_model:options.memory_model arch
      |> Symbexp.add_rel_index (N_rel.Le Signedness.Signed) options.le_index
      |> Symbexp.add_rel_index (N_rel.Ge Signedness.Signed) options.ge_index
      |> Symbexp.add_rel_index N_rel.Eq options.eq_index
      |> Symbexp.add ~tid:options.thread_idx_1 ~bid:options.block_idx_1
      |> Symbexp.add ~tid:options.thread_idx_2 ~bid:options.block_idx_2
      |> Phase_timer.boundary "symbexp"
      |> show_or_stop ~stop_at:options.stop_at ~stage:Stage.Symbexp
           ~show:options.show_symbexp Symbexp.print_kernels
      |> (fun ps ->
      let kernel_extras =
        List.assoc_opt (Protocols.Kernel.name kernel) options.core_extras
        |> Option.value ~default:[]
      in
      Solve_drf.Solution.solve ~timeout:options.timeout
        ~show_proofs:options.show_proofs ~logic:options.logic
        ~solve_tactic:options.solve_tactic
        ~deterministic:options.deterministic_sat ~extras:kernel_extras
        ~pre_solver:(Option.is_some options.launch_contract)
        ?block_dim:options.block_dim ps)
      |> Phase_timer.boundary "solve"
      |> Streamutil.to_list
    in
    App_analysis.{ kernel; report; vacuous = None }
  in
  let rec check_ordinary_until (options : t) (kernel : Protocols.Kernel.t)
      (archs : Architecture.t list) : App_analysis.ordinary =
    match archs with
    | [] -> App_analysis.{ kernel; report = []; vacuous = None }
    | [ arch ] -> check_ordinary_kernel options arch kernel
    | arch :: rest ->
        let ordinary = check_ordinary_kernel options arch kernel in
        if App_analysis.ordinary_is_safe ordinary then
          check_ordinary_until options kernel rest
        else ordinary
  in
  let loop_protocol_memory_outcome ~(config : Subgroup_solver.solver_config)
      (kernel : Protocols.Kernel.t) : Subgroup_solver.memory_outcome =
    (* The subgroup result is a DRF judgment, so its loop-aligned fallback must
       not inherit the detector-only access filter. *)
    let complete_options = { a with only_true_data_races = false } in
    let ordinary = check_ordinary_until complete_options kernel a.archs in
    let classify (solution : Solve_drf.Solution.t) =
      match solution.outcome with
      | Solve_drf.Outcome.Drf | Solve_drf.Outcome.Drf_with_core _ ->
          Subgroup_solver.Solver_unsat_drf
      | Solve_drf.Outcome.Racy _ -> Subgroup_solver.Solver_sat_racy
      | Solve_drf.Outcome.Unknown ->
          Subgroup_solver.Solver_unknown "loop-aware Faial protocol"
    in
    let classifications = List.map classify ordinary.report in
    let classifications =
      match classifications with
      | [] -> [ Subgroup_solver.Solver_unsat_drf ]
      | classifications -> classifications
    in
    let evidence =
      match ordinary.report with
      | [] ->
          [ "loop_protocol#0 source=faial_aligned_protocol solver=unsat(drf)" ]
      | report ->
          report
          |> List.mapi (fun index (solution : Solve_drf.Solution.t) ->
              let classification = classify solution in
              let details =
                match solution.outcome with
                | Solve_drf.Outcome.Racy witness ->
                    let left, right = witness.tasks in
                    Printf.sprintf
                      " array=%s left=%s left_site=%s right=%s right_site=%s"
                      witness.array_name
                      (Access.Mode.to_string left.access.mode)
                      (Location.to_string (Access.location left.access))
                      (Access.Mode.to_string right.access.mode)
                      (Location.to_string (Access.location right.access))
                | Solve_drf.Outcome.Drf | Solve_drf.Outcome.Drf_with_core _
                | Solve_drf.Outcome.Unknown ->
                    ""
              in
              Printf.sprintf
                "loop_protocol#%d source=faial_aligned_protocol%s %s" index
                details
                (Subgroup_solver.classification_to_string classification))
    in
    Subgroup_solver.loop_protocol_outcome ~config
      ~kernel_name:(Protocols.Kernel.name kernel)
      ~classifications ~evidence ()
  in
  let check_subgroup_kernel (routed : subgroup_kernel) : App_analysis.subgroup =
    let subgroup = routed.subgroup in
    let user_precondition = subgroup_assumptions a routed in
    let subgroup =
      {
        subgroup with
        launch_precondition =
          Exp.b_and subgroup.launch_precondition user_precondition;
      }
    in
    let ordinary_memory_effects =
      Subgroup_delinearize.rewrite ~enabled:a.assume_delin
        ~rewrite_access:a.rewrite_delin ~check_vacuity:a.delin_check_vacuosity
        ~algo:a.delin_algo ~weak_in_range:a.delin_weak_in_range
        ~globals:subgroup.memory_globals subgroup.ordinary_memory_effects
      |> function
      | Ok effects -> effects
      | Error error ->
          Logger.Colors.error (fun () ->
              Subgroup_delinearize.error_to_string error);
          exit 2
    in
    let subgroup = { subgroup with ordinary_memory_effects } in
    let kernel = subgroup.matrix_kernel in
    let checked_block_dim =
      match a.block_dim with
      | Some _ -> None
      | None -> (
          Subgroup_obligation.checked_block_dim_of_launch_dimensions
            subgroup.launch_dimensions
          |> function
          | Ok checked -> checked
          | Error error ->
              Logger.Colors.error (fun () ->
                  Subgroup_obligation.error_to_string error);
              exit 2)
    in
    let config =
      Subgroup_solver.solver_config ?timeout_ms:a.timeout ?logic:a.logic ()
    in
    let vacuous =
      if not a.check_pre_sat then None
      else
        let checked_for_pre_sat =
          match (checked_block_dim, a.block_dim) with
          | Some checked, None -> Some checked
          | None, Some block_dim -> (
              Subgroup_obligation.checked_block_dim_of_dim3 block_dim
              |> function
              | Ok checked -> Some checked
              | Error error ->
                  Logger.Colors.error (fun () ->
                      Subgroup_obligation.error_to_string error);
                  exit 2)
          | None, None -> None
          | Some _, Some _ ->
              Logger.Colors.error (fun () ->
                  Subgroup_obligation.error_to_string
                    Subgroup_obligation.Conflicting_checked_block_dim_inputs);
              exit 2
        in
        let checked_precondition =
          checked_for_pre_sat
          |> Option.map Subgroup_obligation.checked_block_dim_precondition
          |> Option.value ~default:(Exp.Bool true)
        in
        let precondition =
          Exp.b_and subgroup.launch_precondition checked_precondition
        in
        if
          Phase_timer.measure "pre-sat" (fun () ->
              Gen_z3.is_unsat ~timeout:a.timeout ~logic:a.logic
                (Formula.make precondition))
        then Some precondition
        else None
    in
    (match
       Drf.Symbolic_launch_evidence.maybe_write_artifacts
         ~filename:(List.hd a.filenames) ~contract:a.launch_contract
         ~kernel:subgroup ~globals:subgroup.memory_globals ~config
     with
    | Ok () -> ()
    | Error error ->
        Logger.Colors.error (fun () -> error);
        exit 2);
    let primary_memory =
      let globals = subgroup.memory_globals in
      kernel
      |> Subgroup_obligation.obligations ~globals ?checked_block_dim
           ?block_dim:a.block_dim ~precondition:subgroup.launch_precondition
           ~site_controls:subgroup.site_controls
           ~ordinary_memory_effects:subgroup.ordinary_memory_effects
      |> Subgroup_solver.solve_obligation_result ~config ~globals
           ?block_dim:a.block_dim ~kernel_name:kernel.name
    in
    let memory =
      if Subgroup_solver.is_repeated_site_boundary primary_memory then
        let protocol =
          loop_protocol_memory_outcome ~config routed.loop_protocol
        in
        Subgroup_solver.resolve_repeated_site_with_loop_protocol ~protocol
          primary_memory
      else primary_memory
    in
    let uniformity =
      let site_controls =
        List.map
          (fun (control : Subgroup_source.site_control) ->
            ( control.site_id,
              Subgroup_uniformity.control_with_facts
                ~conditions:control.conditions
                ~uniform_vars:control.uniform_vars
                ~numeric_aliases:control.numeric_aliases ))
          subgroup.site_controls
      in
      let semantic_prover control =
        Phase_timer.measure "subgroup-uniformity/semantic" (fun () ->
            Subgroup_uniformity_solver.proves_control_uniform ?checked_block_dim
              ?block_dim:a.block_dim ~timeout:a.timeout ~logic:a.logic
              ~globals:subgroup.memory_globals
              ~precondition:subgroup.launch_precondition
              ~target_config:kernel.target_config control)
      in
      match
        Subgroup_uniformity.check_kernel ~site_controls ~semantic_prover
          ~uniform_vars:subgroup.uniform_vars kernel
      with
      | Ok result -> result
      | Error error ->
          Logger.Colors.error (fun () ->
              "subgroup uniformity configuration error: "
              ^ Subgroup_uniformity.error_to_string error);
          exit 2
    in
    App_analysis.{ kernel; memory; uniformity; vacuous }
  in
  kernels
  |> List.map (function
    | Subgroup_kernel kernel ->
        App_analysis.Subgroup (check_subgroup_kernel kernel)
    | Ordinary_kernel kernel -> (
        let vacuous : Exp.bexp option =
          if not a.check_pre_sat then None
          else
            match a.archs with
            | arch :: _ ->
                let prepared = prepare_pre arch a kernel in
                if
                  Phase_timer.measure "pre-sat" (fun () ->
                      Gen_z3.is_unsat ~timeout:a.timeout ~logic:a.logic
                        (Formula.make prepared.pre))
                then Some prepared.pre
                else None
            | [] -> None
        in
        match vacuous with
        | Some _ -> App_analysis.Ordinary { kernel; report = []; vacuous }
        | None -> (
            try App_analysis.Ordinary (check_ordinary_until a kernel a.archs)
            with Stop_at_stage ->
              App_analysis.Ordinary { kernel; report = []; vacuous = None })))
