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

let conv_bexp =
  let parse s =
    match Parsers.BExpParser.of_string s with
    | Ok b -> Ok b
    | Error msg -> Error (`Msg msg)
  in
  let print ppf (b : Exp.bexp) = Format.fprintf ppf "%s" (Exp.b_to_string b) in
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
  and+ unreachable =
    Arg.(
      value & flag & info [ "unreachable" ] ~doc:"Check unreachable accesses.")
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
  and+ assumes =
    Arg.(
      value & opt_all conv_bexp []
      & info [ "assume" ] ~docv:"BEXP"
          ~doc:
            "Add a boolean expression as a kernel pre-condition. May be \
             repeated. Example: --assume \"blockDim.x == 32 && N > 0\"")
  in
  if all_dims && (Option.is_some block_dim || Option.is_some grid_dim) then
    Error
      "Cannot run with options: --all-dims and --grid-dim/--block-dim.\n\
       Use --all-dims and -p instead."
  else
    let archs =
      if all_levels then [ Architecture.Grid; Architecture.Block ]
      else if grid_level then [ Architecture.Grid ]
      else [ Architecture.Block ]
    in
    let app =
      App.parse ~filename ~timeout ~show_proofs ~show_proto ~show_wf ~show_align
        ~show_phase_split ~show_loc_split ~show_flat_acc ~show_symbexp ~logic
        ~ge_index ~le_index ~eq_index ~only_array ~thread_idx_1 ~thread_idx_2
        ~block_idx_1 ~block_idx_2 ~archs ~inline_calls:(not ignore_calls)
        ~ignore_parsing_errors ~includes ~block_dim ~grid_dim ~params
        ~only_kernel ~only_true_data_races ~macros ~cu_to_json ~all_dims
        ~ignore_asserts ~assumes
    in
    let ui = if output_json then Jui.render else Tui.render in
    if unreachable then App.check_unreachable app else App.run app |> ui;
    Ok ()

let () = exit (Cmd.eval_result main)
