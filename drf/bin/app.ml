open Stage0
open Protocols
open Drf
open Inference

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
  kernels : Kernel.t list;
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
  thread_idx_1 : Dim3.t option;
  thread_idx_2 : Dim3.t option;
  block_idx_1 : Dim3.t option;
  block_idx_2 : Dim3.t option;
  archs : Architecture.t list;
  block_dim : Dim3.t option;
  grid_dim : Dim3.t option;
  params : (string * int) list;
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
   only_kernel;
   macros;
   only_true_data_races;
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
      "filenames: " ^ list_string filenames ^ "\nonly_kernel: " ^ only_kernel
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
      ^ "\nassume_delin = " ^ bool assume_delin
      ^ "\nrewrite_delin = " ^ bool rewrite_delin
      ^ "\ndelin_elide = " ^ bool delin_elide
      ^ "\ndelin_algo = " ^ Delin_algo.to_string delin_algo
      ^ "\ndelin_check_vacuosity = " ^ bool delin_check_vacuosity
      ^ "\ndelin_weak_in_range = " ^ bool delin_weak_in_range
      ^ "\ndelin_weak_in_range_for = " ^ list_string delin_weak_in_range_for
      ^ "\nignore_asserts = " ^ bool ignore_asserts
      ^ "\nopaque_calls = " ^ Opaque_call_policy.to_string opaque_calls
      ^ "\nassume_dims = " ^ bool assume_dims
      ^ "\nassume_launch = " ^ bool assume_launch
      ^ "\ncheck_pre_sat = " ^ bool check_pre_sat
      ^ "\nmemory_model = " ^ Memory_model.to_string memory_model
      ^ "\nstop_at = " ^ opt Stage.to_string stop_at
      ^ "\nassumptions: "
      ^ list_string (List.map Assumption.to_string assumptions)
      ^ "\n"

let parse ~extra_files ~filename ~timeout ~show_proofs ~show_proto
    ~show_wf ~show_align ~show_delin ~show_phase_split ~show_loc_split
    ~show_flat_acc ~show_symbexp
    ~logic ~solve_tactic ~deterministic_sat ~ge_index ~le_index ~eq_index
    ~only_array ~only_kernel
    ~only_true_data_races ~thread_idx_1 ~thread_idx_2 ~block_idx_1 ~block_idx_2
    ~block_dim ~grid_dim ~includes ~archs ~ignore_parsing_errors
    ~params ~macros ~cu_to_json ~all_dims ~ignore_asserts ~opaque_calls
    ~assume_delin ~rewrite_delin ~delin_elide ~delin_algo
    ~delin_check_vacuosity ~delin_weak_in_range ~delin_weak_in_range_for
    ~assumptions ~assume_dims ~assume_launch ~check_pre_sat
    ~memory_model ~cbor ~stop_at ~infer_cond_bound ~rules_file : t =
  let rules =
    match rules_file with
    | None -> Imp.Idiom_rewrite.all
    | Some path -> (
        let text = In_channel.with_open_text path In_channel.input_all in
        match Imp.Idiom_rewrite.parse text with
        | Ok rs -> Imp.Idiom_rewrite.all @ rs
        | Error msg ->
            prerr_endline ("--rules " ^ path ^ ": " ^ msg);
            exit 2)
  in
  let parsed =
    Phase_timer.measure "inference" (fun () ->
      Protocol_parser.Silent.to_proto ~rules ~infer_cond_bound
        ~abort_on_parsing_failure:(not ignore_parsing_errors)
        ~includes ~block_dim ~grid_dim ~macros ~cu_to_json
        ~ignore_asserts ~assume_launch ~launch_params:assume_launch ~cbor
        ~opaque_calls ~extra_files filename)
  in
  (* Uniquify duplicate kernel names so that a [--assume kernel=K:BEXP]
     clause, the [--list-kernels] output, and the genie verdict JSON all
     address each kernel by a distinct identifier. Discarded kernels are
     enumerated and selected by name too, so they are uniquified against
     the analyzable names: the analyzable kernel keeps the shared name
     and the discarded one takes the suffix. *)
  let kernels = parsed.kernels |> Protocols.Kernel.uniquify_names in
  let rejected =
    parsed.rejected
    |> Common.uniquify
         ~name:(fun (r : Imp.Rejected_kernel.t) -> r.kernel)
         ~rename:(fun (r : Imp.Rejected_kernel.t) (kernel : string) ->
           { r with kernel })
         ~taken:
           (kernels
            |> List.map Protocols.Kernel.name
            |> Common.StringSet.of_list)
  in
  let block_dim = if all_dims then None else Some parsed.options.block_dim in
  let grid_dim = if all_dims then None else Some parsed.options.grid_dim in
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
    only_kernel;
    only_true_data_races;
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
  (* 1.1 apply user-provided assumptions. Each clause conjoins onto the
     precondition or a specific binder, filtered by its own kernel scope;
     a clause that fails to apply (unknown/ambiguous binder, or an unbound
     name introduced) raises [Assumption_error]. *)
  |> (fun k ->
    List.fold_left
      (fun k a ->
        match Assumption.add_to_kernel a k with
        | Ok k -> k
        | Error msg -> raise (Assumption_error msg))
      k a.assumptions)
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
       ~algo:a.delin_algo ~weak_in_range
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

(* A discarded kernel is a name [--kernel] answers to, so an empty
   selection is only an error when the name matches no kernel at all,
   analyzable or discarded. Naming a discarded kernel selects no
   analyzable kernel and leaves [only_rejected] to report it. *)
let only_kernel (a : t) (ks : Protocols.Kernel.t list) : Protocols.Kernel.t list
    =
  match a.only_kernel with
  | Some name ->
      let ks = ks |> List.filter (fun k -> Protocols.Kernel.name k = name) in
      if ks <> [] then ks
      else if
        List.exists (fun (r : Imp.Rejected_kernel.t) -> r.kernel = name)
          a.rejected
      then []
      else raise (Kernel_not_found name)
  | None -> ks

let only_rejected (a : t) : Imp.Rejected_kernel.t list =
  match a.only_kernel with
  | Some name ->
      a.rejected
      |> List.filter (fun (r : Imp.Rejected_kernel.t) -> r.kernel = name)
  | None -> a.rejected

(* The kernels the translation unit names, analyzable and discarded
   alike, as [--list-kernels] enumerates them. *)
module Listing = struct
  type entry =
    | Analysable of Protocols.Kernel.t
    | Discarded of Imp.Rejected_kernel.t

  let name : entry -> string = function
    | Analysable k -> Protocols.Kernel.name k
    | Discarded r -> r.kernel

  let of_app (a : t) : entry list =
    List.map (fun k -> Analysable k) a.kernels
    @ List.map (fun r -> Discarded r) a.rejected
    |> List.sort (fun x y -> String.compare (name x) (name y))
end

let run (a : t) : Analysis.t list =
  let check_kernel arch (kernel : Protocols.Kernel.t) : Analysis.t =
    let report =
      kernel |> translate arch a
      |> Symbexp.translate ~memory_model:a.memory_model arch
      |> Symbexp.add_rel_index (N_rel.Le Signedness.Signed) a.le_index
      |> Symbexp.add_rel_index (N_rel.Ge Signedness.Signed) a.ge_index
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
            ~solve_tactic:a.solve_tactic ~deterministic:a.deterministic_sat
            ~extras:kernel_extras ps)
      |> Phase_timer.boundary "solve"
      |> Streamutil.to_list
    in
    Analysis.{ kernel; report; vacuous = None }
  in
  let kernels = a.kernels |> only_kernel a in
  (* Validate every assumption against every kernel before any analysis, so
     a bad [--assume] aborts the whole run up front rather than mid-stream. *)
  (match a.archs with
   | arch :: _ ->
       (try List.iter (fun k -> ignore (prepare_pre arch a k)) kernels
        with Assumption_error msg -> prerr_endline msg; exit 2)
   | [] -> ());
  kernels
  |> List.map (fun kernel ->
      let vacuous : Exp.bexp option =
        if not a.check_pre_sat then None
        else match a.archs with
          | arch :: _ ->
            let prepared = prepare_pre arch a kernel in
            if Phase_timer.measure "pre-sat"
                 (fun () -> Gen_z3.is_unsat ~timeout:a.timeout
                              ~logic:a.logic (Formula.make prepared.pre))
            then Some prepared.pre else None
          | [] -> None
      in
      match vacuous with
      | Some _ ->
        Analysis.{ kernel; report = []; vacuous }
      | None ->
        let rec check_until (archs : Architecture.t list) : Analysis.t =
          match archs with
          | [] -> Analysis.{ kernel; report = []; vacuous = None }
          | [ arch ] -> check_kernel arch kernel
          | arch :: archs ->
              let a = check_kernel arch kernel in
              if Analysis.is_safe a then check_until archs else a
        in
        try check_until a.archs
        with Stop_at_stage ->
          Analysis.{ kernel; report = []; vacuous = None })
