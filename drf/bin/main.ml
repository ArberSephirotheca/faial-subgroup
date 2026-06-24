open Stage0
open Protocols
open Protocols_parsing
open Cmdliner

let dim_help =
  {|
The value will be loaded from header if omitted.
Examples (without quotes): "[2,2,2]" or "32".
|}
  |> Common.replace ~substring:"\n" ~by:""

let conv_dim3 default =
  let parse s =
    match Dim3.parse ~default s with Ok e -> Ok e | Error e -> Error (`Msg e)
  in
  let print ppf (l : Dim3.t) = Format.fprintf ppf "%s" (Dim3.to_string l) in
  Arg.conv (parse, print)

let conv_int_list =
  let parse s =
    let msg = "Invalid JSON format: Expected a list of ints, but got: " ^ s in
    try
      match Yojson.Basic.from_string s with
      | `List lst ->
          if List.for_all (function `Int _ -> true | _ -> false) lst then
            Ok
              (List.map
                 (function `Int n -> n | _ -> failwith "impossible")
                 lst)
          else Error (`Msg msg)
      | _ -> Error (`Msg msg)
    with Yojson.Json_error e -> Error (`Msg ("JSON parse error: " ^ e))
  in
  let print ppf (l : int list) =
    let s = "[" ^ (List.map string_of_int l |> String.concat ", ") ^ "]" in
    Format.fprintf ppf "%s" s
  in
  Arg.conv (parse, print)

(* [--assume "BEXP"] or [--assume "KERNEL:BEXP"]. The optional prefix
   targets a single kernel by name; without it, the clause applies to
   every kernel whose declared params plus the launch-config dims
   cover the clause's free variables. The prefix must look like a C
   identifier (letters / digits / underscore); a [:] inside the BEXP
   itself never matches because the bexp grammar uses no [:] tokens. *)
let conv_assume =
  let looks_like_ident s =
    s <> ""
    && String.for_all (fun c ->
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c = '_')
      s
  in
  let parse_bexp s =
    match Parsers.BExpParser.of_string s with
    | Ok b -> Ok b
    | Error msg -> Error (`Msg msg)
  in
  let parse s =
    match String.index_opt s ':' with
    | None ->
      (match parse_bexp s with
       | Ok b -> Ok (None, b)
       | Error e -> Error e)
    | Some i ->
      let prefix = String.sub s 0 i |> String.trim in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      if looks_like_ident prefix then
        match parse_bexp rest with
        | Ok b -> Ok (Some prefix, b)
        | Error e -> Error e
      else
        (match parse_bexp s with
         | Ok b -> Ok (None, b)
         | Error e -> Error e)
  in
  let print ppf = function
    | (Some n, b) ->
      Format.fprintf ppf "%s:%s" n (Exp.b_to_string b)
    | (None, b) ->
      Format.fprintf ppf "%s" (Exp.b_to_string b)
  in
  Arg.conv (parse, print)

let conv_tactic =
  let parse s =
    match Parsers.TacticParser.of_string s with
    | Ok t -> Ok t
    | Error msg -> Error (`Msg msg)
  in
  let print ppf (t : Gen_z3.Tactic.t) =
    Format.fprintf ppf "%s" (Gen_z3.Tactic.to_string t)
  in
  Arg.conv (parse, print)

let conv_subgroup_size =
  let parse s =
    match int_of_string_opt s with
    | None -> Error (`Msg ("Invalid subgroup size: " ^ s))
    | Some value -> (
        match Inference.Subgroup_matrix.Target_config.subgroup_size value with
        | Ok _ -> Ok value
        | Error error ->
            Error
              (`Msg
                 (Inference.Subgroup_matrix.Target_config.error_to_string error))
        )
  in
  let print ppf value = Format.fprintf ppf "%d" value in
  Arg.conv (parse, print)

let main =
  let doc = "Verify if CUDA file is free from data races." in
  let info = Cmd.info "faial-drf" ~doc in
  Cmd.v info
  @@
  let open Cmdliner.Term.Syntax in
  let+ filename =
    Arg.(
      required
      & pos 0 (some file) None
      & info [] ~docv:"FILENAME" ~doc:"The path $(docv) of the GPU program.")
  and+ timeout =
    Arg.(
      value
      & opt (some int) None
      & info [ "t"; "timeout" ] ~docv:"MILISECS"
          ~doc:"Sets a timeout in millisecs. Default: $(docv)")
  and+ show_proofs =
    Arg.(
      value & flag
      & info [ "show-proofs" ] ~doc:"Show the Z3 proofs being generated.")
  and+ show_proto =
    Arg.(value & flag & info [ "show-map" ] ~doc:"Show the MAP kernel.")
  and+ show_wf =
    Arg.(
      value & flag
      & info [ "show-well-formed" ] ~doc:"Show the well-formed kernel.")
  and+ show_align =
    Arg.(value & flag & info [ "show-aligned" ] ~doc:"Show the aligned kernel.")
  and+ show_delin =
    Arg.(
      value & flag
      & info [ "show-delin" ]
          ~doc:
            "Show the kernel after the delinearization pass. Identical to \
             --show-aligned when --assume-delin is off.")
  and+ show_phase_split =
    Arg.(
      value & flag
      & info [ "show-phase-split" ] ~doc:"Show the phase-split kernel.")
  and+ show_loc_split =
    Arg.(
      value & flag
      & info [ "show-loc-split" ] ~doc:"Show the location-split kernel.")
  and+ show_flat_acc =
    Arg.(
      value & flag
      & info [ "show-flat-acc" ] ~doc:"Show the flat-access kernel.")
  and+ show_symbexp =
    Arg.(value & flag & info [ "show-symbexp" ] ~doc:"Show the symbexp kernel.")
  and+ logic =
    Arg.(
      value
      & opt (some string) None
      & info [ "logic" ] ~doc:"Set the logic used by the Z3 solver.")
  and+ solve_tactic =
    Arg.(
      value
      & opt (some conv_tactic) None
      & info [ "solve-tactic" ] ~docv:"TACTIC"
          ~doc:
            "Z3 tactic expression for the race-query solver. When omitted, \
             the solver uses Z3's default strategy.")
  and+ output_json =
    Arg.(value & flag & info [ "json" ] ~doc:"Output result as JSON.")
  and+ ignore_parsing_errors =
    Arg.(
      value & flag
      & info [ "ignore-parsing-errors" ] ~doc:"Ignore parsing errors.")
  and+ block_dim =
    let d = Gv_parser.default_block_dim |> Dim3.to_string in
    let doc =
      "Sets the number of threads per block." ^ dim_help ^ " Default: " ^ d
    in
    Arg.(
      value
      & opt (some (conv_dim3 Dim3.one)) None
      & info [ "b"; "block-dim"; "blockDim" ] ~docv:"DIM3" ~doc)
  and+ subgroup_size =
    Arg.(
      value
      & opt (some conv_subgroup_size) None
      & info [ "subgroup-size" ] ~docv:"N"
          ~doc:
            "Use explicit CUDA x-contiguous subgroup configuration for \
             subgroup/matrix analysis.")
  and+ grid_dim =
    let d = Gv_parser.default_grid_dim |> Dim3.to_string in
    let doc =
      "Sets the number of blocks per grid." ^ dim_help ^ " Default: " ^ d
    in
    Arg.(
      value
      & opt (some (conv_dim3 Dim3.one)) None
      & info [ "g"; "grid-dim"; "gridDim" ] ~docv:"DIM3" ~doc)
  and+ includes =
    Arg.(
      value & opt_all string []
      & info [ "I"; "include-dir" ] ~docv:"DIR"
          ~doc:
            "Add the specified directory to the search path for include files.")
  and+ ignore_calls =
    Arg.(
      value & flag
      & info [ "ignore-calls" ]
          ~doc:"By default we inline kernel calls, this option skips that step.")
  and+ ge_index =
    Arg.(
      value & opt conv_int_list []
      & info [ "ge-index" ] ~docv:"LIST"
          ~doc:
            "Check that each index is greater-or-equal than the argument. \
             Expects an integer, or a (JSON) list of integers. Example: 1 or \
             [1,2]")
  and+ le_index =
    Arg.(
      value & opt conv_int_list []
      & info [ "le-index" ] ~docv:"LIST"
          ~doc:
            "Check that each index is lesser-or-equal than the argument. \
             Expects an integer, or a (JSON) list of integers. Example: 1 or \
             [1,2]")
  and+ eq_index =
    Arg.(
      value & opt conv_int_list []
      & info [ "index" ] ~docv:"LIST"
          ~doc:
            "Check that each index is greater-or-equal than the argument. \
             Expects an integer, or a (JSON) list of integers. Example: 1 or \
             [1,2]")
  and+ only_array =
    Arg.(
      value
      & opt (some string) None
      & info [ "array" ] ~doc:"Only check a specific array.")
  and+ only_kernel =
    Arg.(
      value
      & opt (some string) None
      & info [ "kernel" ] ~doc:"Only check a specific kernel.")
  and+ only_true_data_races =
    Arg.(
      value & flag
      & info [ "find-true-dr" ]
          ~doc:
            "Only analyze accesses that yield true-data races. WARNING: SHOULD \
             ONLY BE USED TO DETECT DATA-RACES. CANNOT GUARANTEE DRF!")
  and+ thread_idx_1 =
    let doc =
      "Sets the thread index for one thread." ^ dim_help ^ " Default: (none)"
    in
    Arg.(
      value
      & opt (some (conv_dim3 Dim3.zero)) None
      & info [ "thread-idx-1"; "tid1" ] ~docv:"DIM3" ~doc)
  and+ thread_idx_2 =
    let doc =
      "Sets the thread index for another thread." ^ dim_help
      ^ " Default: (none)"
    in
    Arg.(
      value
      & opt (some (conv_dim3 Dim3.zero)) None
      & info [ "thread-idx-2"; "tid2" ] ~docv:"DIM3" ~doc)
  and+ block_idx_1 =
    let doc =
      "Sets the block index for one thread. Only available in grid-level \
       analysis." ^ dim_help ^ " Default: (none)"
    in
    Arg.(
      value
      & opt (some (conv_dim3 Dim3.zero)) None
      & info [ "block-idx-1"; "bid1" ] ~docv:"DIM3" ~doc)
  and+ block_idx_2 =
    let doc =
      "Sets the block index for another thread. Only available in grid-level \
       analysis." ^ dim_help ^ " Default: (none)"
    in
    Arg.(
      value
      & opt (some (conv_dim3 Dim3.zero)) None
      & info [ "block-idx-2"; "bid2" ] ~docv:"DIM3" ~doc)
  and+ grid_level =
    Arg.(
      value & flag
      & info [ "grid-level" ]
          ~doc:
            "By default we perform block-level verification, this option \
             performs grid-level verification.")
  and+ all_levels =
    Arg.(
      value & flag
      & info [ "all-levels" ]
          ~doc:
            "By default we perform block-level verification, this option \
             performs block-level AND grid-level verification.")
  and+ params =
    Arg.(
      value
      & opt_all (pair ~sep:'=' string int) []
      & info [ "p"; "param" ] ~docv:"KEYVAL"
          ~doc:"Set the value of an integer parameter")
  and+ launch_contract =
    Arg.(
      value
      & opt (some string) None
      & info [ "launch-contract" ] ~docv:"ROW"
          ~doc:
            "Apply a guarded launch/template/shape contract for a concrete \
             manifest row. Currently supported rows: L072, L073.")
  and+ macros =
    Arg.(
      value & opt_all string []
      & info [ "D"; "macro" ] ~docv:"<macro>=<value>"
          ~doc:"Define <macro> to <value> (or 1 if <value> omitted)")
  and+ cu_to_json =
    Arg.(
      value & opt string "cu-to-json"
      & info [ "cu-to-json" ] ~docv:"PATH" ~doc:"Set path to cu-to-json.")
  and+ all_dims =
    Arg.(
      value & flag
      & info [ "all-dims" ]
          ~doc:
            "Do not set gridDim and blockDim; Verifier ranges over all \
             possible dimensions.")
  and+ ignore_asserts =
    Arg.(value & flag & info [ "ignore-asserts" ] ~doc:"Ignore asserts.")
  and+ no_delin_elide =
    Arg.(
      value & flag
      & info [ "no-delin-elide" ]
          ~doc:
            "Keep every per-axis bound [0 <= i_k < d_k] on delinearised \
             accesses. By default the trivial bounds, those statically \
             provable from the enclosing loop's [Range.t], are elided, \
             which only shrinks the formula and never changes a verdict.")
  and+ delin_check_vacuosity =
    Arg.(
      value & flag
      & info [ "delin-avoid-vacuous" ]
          ~doc:
            "Under $(b,--assume-delin), verify each recovered axis bound is \
             consistent with the kernel precondition before committing it, \
             declining vacuous delinearisations. Off by default: bounds are \
             assumed and any resulting vacuity is caught by the \
             $(b,--check-pre-sat) pre-flight, which $(b,--assume-delin) forces \
             on.")
  and+ delin_algo =
    Arg.(
      last
      & opt_all (enum App.Delin_algo.enum) [ App.Delin_algo.default ]
      & info [ "delin-algo" ] ~docv:"ALGO"
          ~doc:
            "Strategy [--assume-delin] uses to recover each array's \
             dimensions from a flattened index. $(b,greedy): fast \
             heuristic, factors sizes pairwise. $(b,ics15): permutation \
             search (Grosser et al.'s optimistic delinearization). \
             $(b,ics15-opt): same results as $(b,ics15) with a pruned \
             search. $(b,cramer): exact integer linear-algebra solve. \
             $(b,weak): for opaque runtime strides that cannot be factored \
             (e.g. ggml tensor $(b,nb) strides); reads the strides straight \
             off the index and assumes they nest, disjoined over every stride \
             ordering and vacuity-guarded. Unsound in general, assume-only. \
             Default $(b,ics15-opt). May be repeated; the last one wins.")
  and+ delin_weak_in_range =
    Arg.(
      value & flag
      & info [ "delin-weak-in-range" ]
          ~doc:
            "For $(b,--delin-algo weak): also assert each digit's in-range span \
             [0 <= i_k*nb_k < nb_{k+1}] on top of the stride nesting. Off by \
             default, because the nesting alone already clears the \
             multidimensional access, while the span multiplies div/mod index \
             digits by opaque strides and makes Z3 blow up on indices with \
             div/mod coordinates. Enable for the extra assumed soundness at a \
             large solver cost.")
  and+ no_rewrite_delin =
    Arg.(
      value & flag
      & info [ "no-rewrite-delin" ]
          ~doc:
            "Do not re-encode delinearised accesses as multidimensional \
             subscripts. The rewrite is on by default: it is sound (only \
             applied where the recovered bounds are provable) and \
             verdict-neutral, turning the nonlinear flat-index collision \
             test into a linear per-axis one, so it only speeds up Z3. \
             Disable it to leave accesses in 1D form, e.g. to isolate the \
             rewrite's effect from [--assume-delin]'s bounds.")
  and+ assume_delin =
    Arg.(
      value & flag
      & info [ "assume-delin" ]
          ~doc:
            "Assume the recovered per-axis bounds [0 <= i_k < d_k] instead \
             of proving them, so delinearisation applies even where the \
             bounds cannot be discharged from [kernel.pre /\\ runtime /\\ \
             loop_scope]. UNSOUND in general (a wrong inferred dimension \
             hides races), but never vacuous: each assumed bound is \
             consistency-checked, and the pre-condition SAT pre-flight is \
             forced on, so a delinearisation that would empty the state \
             space is refused. Without this flag the bounds must be \
             proven (sound). Composes with [--no-rewrite-delin].")
  and+ assumes =
    Arg.(
      value & opt_all conv_assume []
      & info [ "assume" ] ~docv:"[KERNEL:]BEXP"
          ~doc:
            "Add a boolean expression as a kernel pre-condition. With no \
             prefix, the clause is applied to every kernel whose declared \
             params (plus the launch-config dims) cover the clause's free \
             variables. With a [KERNEL:] prefix the clause is scoped to a \
             specific kernel by name; the prefix is treated as an \
             identifier match only when [KERNEL] is a valid C identifier. \
             Names not matching any kernel in the file are silently \
             ignored. May be repeated. Examples: --assume \"blockDim.x == \
             32 && N > 0\" or --assume \"ckMedian:blockDim.x == 16\"")
  and+ assume_dims =
    Arg.(
      value & flag
      & info [ "assume-dims" ]
          ~doc:
            "For each thread/block index axis that is not referenced in \
             the kernel, assert that the matching launch dimension is 1 \
             (e.g. if threadIdx.y is unused, assume blockDim.y == 1; \
             same for threadIdx.{x,z} / blockIdx.{x,y,z}). UNSOUND in \
             general: a kernel that writes memory still races between \
             threads that differ only in an unreferenced axis, and \
             this flag hides those races. Use --show-map to inspect \
             the resulting precondition.")
  and+ assume_launch =
    Arg.(
      value & flag
      & info [ "assume-launch" ]
          ~doc:
            "For every CUDA <<<grid, block>>> launch site emitted by \
             cu-to-json, synthesise a pseudo-kernel that binds the \
             launch's grid/block dimensions and arguments to the \
             called kernel's parameters and demotes the original \
             kernel to __device__ for inlining. Off by default; only \
             the parsed launch metadata is used.")
  and+ check_pre_sat =
    Arg.(
      value & flag
      & info [ "check-pre-sat" ]
          ~doc:
            "Before checking each kernel for races, ask Z3 whether its \
             merged precondition is satisfiable. If UNSAT, every race \
             goal is also UNSAT, so the kernel's \"is DRF\" verdict \
             is vacuous regardless of the actual access pattern. \
             Prints a per-kernel warning naming the affected kernel. \
             Off by default; turn on as a soundness check when \
             combining multiple --assume / --assume-launch / \
             --assume-dims sources that may contradict each other.")
  and+ assume_warp_synch =
    Arg.(
      value & flag
      & info [ "assume-warp-synch" ]
          ~doc:
            "Assume pre-Volta warp-synchronous execution: threads \
             inside the same warp execute in lockstep, so an implicit \
             barrier holds between every statement for any two \
             threads whose linear in-block tids share a warpSize \
             quotient. Race witnesses where both tasks fall into the \
             same warp are excluded; cross-warp pairs keep normal \
             race-detection semantics. UNSOUND on post-Volta \
             hardware, which has independent thread scheduling; \
             kernels relying on this assumption need explicit \
             __syncwarp() for portability. Most useful with a known \
             blockDim (via --assume-launch or --block-dim) — the \
             linear-tid encoding contains a blockDim.x * blockDim.y \
             cross-term that stays non-linear otherwise.")
  and+ cbor =
    Arg.(
      value & flag
      & info [ "cbor" ]
          ~doc:
            "Run cu-to-json with --cbor and decode its output as CBOR \
             instead of JSON. Produces a smaller wire payload; the \
             decoded tree is identical.")
  and+ list_kernels =
    Arg.(
      value & flag
      & info [ "list-kernels" ]
          ~doc:
            "Print one kernel name per line on stdout, taken from the \
             parsed protocol-level kernel list, then exit. No analysis \
             is run. Synthesised pseudo-kernels emitted by \
             [--assume-launch] are included if that flag is also set. \
             Duplicate names in the parsed list are uniquified with a \
             [_N] suffix so each printed name is a distinct identifier \
             suitable for [--kernel] / [--assume KERNEL:...] filters. \
             Combine with [--show-signature] to also print each \
             kernel's parameter list with C type and signedness.")
  and+ show_signature =
    Arg.(
      value & flag
      & info [ "show-signature" ]
          ~doc:
            "Modify [--list-kernels] output: under each kernel name, \
             print its global and local parameters with declared C \
             type and ([signed] | [unsigned]) annotation. Useful for \
             writing [--assume KERNEL:...] flags against the right \
             variable names.")
  and+ stop_at =
    let stages =
      App.Stage.cmdliner_choices
      |> List.map fst
      |> String.concat "|"
    in
    Arg.(
      value
      & opt (some (enum App.Stage.cmdliner_choices)) None
      & info [ "stop-at" ] ~docv:"STAGE"
          ~doc:
            ("Stop after the given pipeline stage and exit. Implies \
              the matching --show-<stage>; the rest of the analysis \
              (downstream stages and the SMT solver) is skipped. \
              Stages, in pipeline order: " ^ stages ^ "."))
  in
  if all_dims && (Option.is_some block_dim || Option.is_some grid_dim) then
    Error
      "Cannot run with options: --all-dims and --grid-dim/--block-dim.\n\
       Use --all-dims and -p instead."
  else if assume_launch && not all_dims then
    Error
      "--assume-launch requires --all-dims. The synthesised pseudo-kernels \
       constrain blockDim/gridDim via assert(...) calls derived from the \
       launch site; pinning the default block/grid dims on top would \
       conflict and trivialise the precondition."
  else
    let archs =
      if all_levels then [ Architecture.Grid; Architecture.Block ]
      else if grid_level then [ Architecture.Grid ]
      else [ Architecture.Block ]
    in
    let app =
      App.parse ~filename ~timeout ~show_proofs ~show_proto ~show_wf ~show_align
        ~show_delin ~show_phase_split ~show_loc_split ~show_flat_acc
        ~show_symbexp ~logic ~solve_tactic ~ge_index ~le_index ~eq_index ~only_array
        ~thread_idx_1 ~thread_idx_2 ~block_idx_1 ~block_idx_2 ~archs
        ~inline_calls:(not ignore_calls) ~ignore_parsing_errors ~includes
        ~block_dim ~grid_dim ~params ~only_kernel ~only_true_data_races ~macros
        ~cu_to_json ~all_dims ~ignore_asserts ~assume_delin
        ~rewrite_delin:(not no_rewrite_delin)
        ~delin_elide:(not no_delin_elide) ~delin_algo ~delin_check_vacuosity
        ~delin_weak_in_range
        ~assumes ~assume_dims ~assume_launch ~check_pre_sat
        ~memory_model:{ Memory_model.warp_synchronous = assume_warp_synch }
        ~cbor ~stop_at ~subgroup_size ~launch_contract
    in
    let ui = if output_json then Jui.render else Tui.render in
    let run () =
      if list_kernels then
        app.kernels
        |> List.iter (fun k ->
          match k with
          | App.Ordinary_kernel kernel ->
              if show_signature
              then print_endline (Protocols.Kernel.signature_string kernel)
              else print_endline (Protocols.Kernel.name kernel)
          | App.Subgroup_kernel kernel ->
              print_endline kernel.matrix_kernel.name)
      else if Option.is_some stop_at then
        (* Run the pipeline for its printing side effects (each
           [show_or_stop] dumps the IR at its stage when matched), but
           skip the UI render — an empty Analysis report from the
           [Stop_at_stage] catch in [App.run] would otherwise print as
           "Kernel ... is DRF!", which is misleading when no analysis
           actually ran. *)
        let _ = App.run app in ()
      else App.run app |> ui
    in
    (try run (); Ok ()
     with App.Kernel_not_found name ->
       Error (Printf.sprintf "kernel '%s' not found!" name))

let () = exit (Cmd.eval_result main)
