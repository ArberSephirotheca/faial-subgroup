open Stage0
open Protocols

let main (filename : string) (timeout : int option) (show_proofs : bool)
    (show_proto : bool) (show_wf : bool) (show_align : bool)
    (show_phase_split : bool) (show_loc_split : bool) (show_flat_acc : bool)
    (show_symbexp : bool) (logic : string option) (output_json : bool)
    (ignore_parsing_errors : bool) (block_dim : Dim3.t option)
    (grid_dim : Dim3.t option) (includes : string list) (ignore_calls : bool)
    (ge_index : int list) (le_index : int list) (eq_index : int list)
    (only_array : string option) (only_kernel : string option)
    (only_true_data_races : bool) (thread_idx_1 : Dim3.t option)
    (thread_idx_2 : Dim3.t option) (block_idx_1 : Dim3.t option)
    (block_idx_2 : Dim3.t option) (grid_level : bool) (unreachable : bool)
    (all_levels : bool) (params : (string * int) list) (macros : string list)
    (cu_to_json : string) (all_dims : bool) (ignore_asserts : bool) : unit =
  if all_dims && (Option.is_some block_dim || Option.is_some grid_dim) then (
    Logger.Default.error
      "Cannot run with options: --all-dims and --grid-dim/--block-dim.\n\
       Use --all-dims and -p instead.";
    exit 3);
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
      ~ignore_parsing_errors ~includes ~block_dim ~grid_dim ~params ~only_kernel
      ~only_true_data_races ~macros ~cu_to_json ~all_dims ~ignore_asserts
  in
  let ui = if output_json then Jui.render else Tui.render in
  if unreachable then App.check_unreachable app else App.run app |> ui

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let get_timeout =
  let doc = "Sets a timeout in millisecs. Default: $(docv)" in
  Arg.(
    value & opt (some int) None & info [ "t"; "timeout" ] ~docv:"MILISECS" ~doc)

let logic =
  let doc = "Set the logic used by the Z3 solver." in
  Arg.(value & opt (some string) None & info [ "logic" ] ~doc)

let show_proofs =
  let doc = "Show the Z3 proofs being generated." in
  Arg.(value & flag & info [ "show-proofs" ] ~doc)

let show_proto =
  let doc = "Show the MAP kernel." in
  Arg.(value & flag & info [ "show-map" ] ~doc)

let show_wf =
  let doc = "Show the well-formed kernel." in
  Arg.(value & flag & info [ "show-well-formed" ] ~doc)

let show_aligned =
  let doc = "Show the aligned kernel." in
  Arg.(value & flag & info [ "show-aligned" ] ~doc)

let show_phase_split =
  let doc = "Show the phase-split kernel." in
  Arg.(value & flag & info [ "show-phase-split" ] ~doc)

let show_loc_split =
  let doc = "Show the location-split kernel." in
  Arg.(value & flag & info [ "show-loc-split" ] ~doc)

let show_flat_acc =
  let doc = "Show the flat-access kernel." in
  Arg.(value & flag & info [ "show-flat-acc" ] ~doc)

let show_symbexp =
  let doc = "Show the symbexp kernel." in
  Arg.(value & flag & info [ "show-symbexp" ] ~doc)

let output_json =
  let doc = "Output result as JSON." in
  Arg.(value & flag & info [ "json" ] ~doc)

let unreachable =
  let doc = "Check unreachable accesses." in
  Arg.(value & flag & info [ "unreachable" ] ~doc)

let ignore_parsing_errors =
  let doc = "Ignore parsing errors." in
  Arg.(value & flag & info [ "ignore-parsing-errors" ] ~doc)

let dim_help =
  {|
The value will be loaded from header if omitted.
Examples (without quotes): "[2,2,2]" or "32".
|}
  |> Common.replace ~substring:"\n" ~by:""

let conv_dim3 default =
  let parse =
   fun s ->
    match Dim3.parse ~default s with Ok e -> `Ok e | Error e -> `Error e
  in
  let print _ (l : Dim3.t) : unit = Dim3.to_string l |> print_string in
  (parse, print)

let block_dim =
  let d = Gv_parser.default_block_dim |> Dim3.to_string in
  let doc =
    "Sets the number of threads per block." ^ dim_help ^ " Default: " ^ d
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.one)) None
    & info [ "b"; "block-dim"; "blockDim" ] ~docv:"DIM3" ~doc)

let grid_dim =
  let d = Gv_parser.default_grid_dim |> Dim3.to_string in
  let doc =
    "Sets the number of blocks per grid." ^ dim_help ^ " Default: " ^ d
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.one)) None
    & info [ "g"; "grid-dim"; "gridDim" ] ~docv:"DIM3" ~doc)

let all_dims =
  let doc =
    "Do not set gridDim and blockDim; Verifier ranges over all possible \
     dimensions."
  in
  Arg.(value & flag & info [ "all-dims" ] ~doc)

let thread_idx_1 =
  let doc =
    "Sets the thread index for one thread." ^ dim_help ^ " Default: (none)"
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.zero)) None
    & info [ "thread-idx-1"; "tid1" ] ~docv:"DIM3" ~doc)

let thread_idx_2 =
  let doc =
    "Sets the thread index for another thread." ^ dim_help ^ " Default: (none)"
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.zero)) None
    & info [ "thread-idx-2"; "tid2" ] ~docv:"DIM3" ~doc)

let block_idx_1 =
  let doc =
    "Sets the block index for one thread. Only available in grid-level \
     analysis." ^ dim_help ^ " Default: (none)"
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.zero)) None
    & info [ "block-idx-1"; "bid1" ] ~docv:"DIM3" ~doc)

let block_idx_2 =
  let doc =
    "Sets the block index for another thread. Only available in grid-level \
     analysis." ^ dim_help ^ " Default: (none)"
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.zero)) None
    & info [ "block-idx-2"; "bid2" ] ~docv:"DIM3" ~doc)

let include_dir =
  let doc =
    "Add the specified directory to the search path for include files."
  in
  Arg.(value & opt_all string [] & info [ "I"; "include-dir" ] ~docv:"DIR" ~doc)

let params =
  let doc = "Set the value of an integer parameter" in
  Arg.(
    value
    & opt_all (pair ~sep:'=' string int) []
    & info [ "p"; "param" ] ~docv:"KEYVAL" ~doc)

let macros =
  let doc = "Define <macro> to <value> (or 1 if <value> omitted)" in
  Arg.(
    value & opt_all string []
    & info [ "D"; "macro" ] ~docv:"<macro>=<value>" ~doc)

let ignore_calls =
  let doc = "By default we inline kernel calls, this option skips that step." in
  Arg.(value & flag & info [ "ignore-calls" ] ~doc)

let grid_level =
  let doc =
    "By default we perform block-level verification, this option performs \
     grid-level verification."
  in
  Arg.(value & flag & info [ "grid-level" ] ~doc)

let all_levels =
  let doc =
    "By default we perform block-level verification, this option performs \
     block-level AND grid-level verification."
  in
  Arg.(value & flag & info [ "all-levels" ] ~doc)

let conv_int_list =
  let parse =
   fun s ->
    let msg = "Invalid JSON format: Expected a list of ints, but got: " ^ s in
    try
      match Yojson.Basic.from_string s with
      | `List lst ->
          `Ok (List.map (function `Int n -> n | _ -> failwith msg) lst)
      | _ -> failwith msg
    with _ -> `Error msg
  in
  let print f (l : int list) : unit =
    let s = "[" ^ (List.map string_of_int l |> String.concat ", ") ^ "]" in
    Format.fprintf f "%s" s
  in
  (parse, print)

let eq_index =
  let doc =
    "Check that each index is greater-or-equal than the argument. Expects an \
     integer, or a (JSON) list of integers. Example: 1 or [1,2]"
  in
  Arg.(value & opt conv_int_list [] & info [ "index" ] ~docv:"LIST" ~doc)

let ge_index =
  let doc =
    "Check that each index is greater-or-equal than the argument. Expects an \
     integer, or a (JSON) list of integers. Example: 1 or [1,2]"
  in
  Arg.(value & opt conv_int_list [] & info [ "ge-index" ] ~docv:"LIST" ~doc)

let le_index =
  let doc =
    "Check that each index is lesser-or-equal than the argument. Expects an \
     integer, or a (JSON) list of integers. Example: 1 or [1,2]"
  in
  Arg.(value & opt conv_int_list [] & info [ "le-index" ] ~docv:"LIST" ~doc)

let only_array =
  let doc = "Only check a specific array." in
  Arg.(value & opt (some string) None & info [ "array" ] ~doc)

let only_kernel =
  let doc = "Only check a specific kernel." in
  Arg.(value & opt (some string) None & info [ "kernel" ] ~doc)

let only_true_data_races =
  let doc =
    "Only analyze accesses that yield true-data races. WARNING: SHOULD ONLY BE \
     USED TO DETECT DATA-RACES. CANNOT GUARANTEE DRF!"
  in
  Arg.(value & flag & info [ "find-true-dr" ] ~doc)

let ignore_asserts =
  let doc = "Ignore asserts." in
  Arg.(value & flag & info [ "ignore-asserts" ] ~doc)

let cu_to_json =
  let doc = "Set path to cu-to-json." in
  Arg.(
    value & opt string "cu-to-json" & info [ "cu-to-json" ] ~docv:"PATH" ~doc)

let main_t =
  Term.(
    const main $ get_fname $ get_timeout $ show_proofs $ show_proto $ show_wf
    $ show_aligned $ show_phase_split $ show_loc_split $ show_flat_acc
    $ show_symbexp $ logic $ output_json $ ignore_parsing_errors $ block_dim
    $ grid_dim $ include_dir $ ignore_calls $ ge_index $ le_index $ eq_index
    $ only_array $ only_kernel $ only_true_data_races $ thread_idx_1
    $ thread_idx_2 $ block_idx_1 $ block_idx_2 $ grid_level $ unreachable
    $ all_levels $ params $ macros $ cu_to_json $ all_dims $ ignore_asserts)

let info =
  let doc = "Verify if CUDA file is free from data races." in
  Cmd.info "faial-drf" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
