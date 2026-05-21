open Stage0
open Protocols

type imp_kernel = Imp.Kernel.t
type proto_kernel = Protocols.Kernel.t
type 'a t = { options : Gv_parser.t; kernels : 'a list }

module Make (L : Logger.Logger) = struct
  module D = D_to_imp.Make (L)

  (* Shared JSON-to-Imp pipeline used by both [cu_to_imp] (live cu-to-json
     subprocess) and [cjson_to_imp] (cached cu-to-json output on disk). *)
  let imp_of_json ?(block_dim = None) ?(grid_dim = None)
      ?(ignore_asserts = false) ?(assume_launch = false) ?(exit_status = 2)
      (options : Gv_parser.t) (j : Yojson.Basic.t) : imp_kernel t =
    (* Override block_dim/grid_dim if they user provided *)
    let options =
      {
        options with
        block_dim =
          (match block_dim with Some b -> b | None -> options.block_dim);
        grid_dim =
          (match grid_dim with Some g -> g | None -> options.grid_dim);
      }
    in
    match Phase_timer.measure "inference/c-lang" (fun () ->
            C_lang.Program.parse j) with
    | Ok k1 ->
        let synth =
          if assume_launch then Synthesise_launches.rewrite_program
          else Fun.id
        in
        let d_ast =
          Phase_timer.measure "inference/d-lang" (fun () ->
            k1 |> D_lang.rewrite_program |> synth)
        in
        let kernels =
          Phase_timer.measure "inference/d-to-imp" (fun () ->
            D.parse_program d_ast)
        in
        let kernels =
          if ignore_asserts then
            List.map Imp.Kernel.remove_global_asserts kernels
          else kernels
        in
        { options; kernels }
    | Error e ->
        Rjson.print_error e;
        exit exit_status

  let cu_to_imp ?(abort_on_parsing_failure = true) ?(block_dim = None)
      ?(grid_dim = None) ?(includes = []) ?(macros = []) ?(exit_status = 2)
      ?(cu_to_json = "cu-to-json") ?(ignore_asserts = false)
      ?(assume_launch = false) ?(launch_params = false) ?(cbor = false)
      (fname : string) : imp_kernel t =
    (* [Cu_to_json.cu_to_json] internally records "inference/cu-to-json"
       (subprocess + pipe read) and either "inference/yojson-parse" or
       "inference/cbor-decode" depending on the wire format. *)
    let j =
      Cu_to_json.cu_to_json
        ~ignore_fail:(not abort_on_parsing_failure)
        ~on_error:(fun _ -> exit exit_status)
        ~includes ~macros ~exe:cu_to_json ~launch_params ~cbor fname
    in
    let options : Gv_parser.t =
      match Gv_parser.parse fname with
      | Some gv ->
          Logger.Colors.info (fun () ->
            "Found GPUVerify args in source file: " ^ Gv_parser.to_string gv);
          gv
      | None -> Gv_parser.make ()
    in
    imp_of_json ~block_dim ~grid_dim ~ignore_asserts ~assume_launch
      ~exit_status options j

  (* Loads a cached cu-to-json output (.cjson). [Gv_parser] is intentionally
     skipped — the source file's // args: header isn't reachable from the
     cache path, so block/grid/etc. must come from CLI flags. *)
  let cjson_to_imp ?(block_dim = None) ?(grid_dim = None)
      ?(ignore_asserts = false) ?(assume_launch = false) ?(exit_status = 2)
      (fname : string) : imp_kernel t =
    (* Mirror the cu-to-json split: time the read separately from the
       Yojson parse. The "inference/yojson-parse" label is shared with
       the cu-to-json path so dataset sweeps can compare like-for-like. *)
    let raw =
      Phase_timer.measure "inference/cjson-load" (fun () ->
        try In_channel.with_open_text fname In_channel.input_all
        with Sys_error e -> prerr_endline ("cjson: " ^ e); exit exit_status)
    in
    let j =
      Phase_timer.measure "inference/yojson-parse" (fun () ->
        try Yojson.Basic.from_string raw
        with Yojson.Json_error e ->
          prerr_endline ("cjson: invalid JSON in " ^ fname ^ ": " ^ e);
          exit exit_status)
    in
    imp_of_json ~block_dim ~grid_dim ~ignore_asserts ~assume_launch
      ~exit_status (Gv_parser.make ()) j

  let wgsl_to_imp ?(block_dim = None) ?(grid_dim = None) ?(exit_status = 2)
      ?(wgsl_to_json = "wgsl-to-json") ?(ignore_asserts = false)
      (fname : string) : imp_kernel t =
    let j =
      Phase_timer.measure "inference/wgsl-to-json" (fun () ->
        Wgsl_to_json.wgsl_to_json
          ~on_error:(fun _ -> exit exit_status)
          ~exe:wgsl_to_json fname)
    in
    let options : Gv_parser.t = Gv_parser.make () in
    (* Override block_dim/grid_dim if they user provided *)
    let options =
      {
        options with
        block_dim =
          (match block_dim with Some b -> b | None -> options.block_dim);
        grid_dim =
          (match grid_dim with Some g -> g | None -> options.grid_dim);
      }
    in
    match Phase_timer.measure "inference/w-lang" (fun () ->
            W_lang.Program.parse j) with
    | Ok p ->
        let kernels =
          Phase_timer.measure "inference/w-to-imp" (fun () ->
            W_to_imp.translate p)
        in
        let kernels =
          if ignore_asserts then
            List.map Imp.Kernel.remove_global_asserts kernels
          else kernels
        in
        { options; kernels }
    | Error e ->
        Rjson.print_error e;
        exit exit_status

  let to_imp ?(abort_on_parsing_failure = true) ?(block_dim = None)
      ?(grid_dim = None) ?(includes = []) ?(macros = []) ?(exit_status = 2)
      ?(cu_to_json = "cu-to-json") ?(wgsl_to_json = "wgsl-to-json")
      ?(ignore_asserts = false) ?(assume_launch = false)
      ?(launch_params = false) ?(cbor = false) (fname : string) :
      imp_kernel t =
    if String.ends_with ~suffix:".wgsl" fname then
      wgsl_to_imp ~block_dim ~grid_dim ~exit_status ~wgsl_to_json
        ~ignore_asserts fname
    else if String.ends_with ~suffix:".cjson" fname then
      cjson_to_imp ~block_dim ~grid_dim ~exit_status ~ignore_asserts
        ~assume_launch fname
    else
      cu_to_imp ~abort_on_parsing_failure ~block_dim ~grid_dim ~includes ~macros
        ~exit_status ~cu_to_json ~ignore_asserts ~assume_launch ~launch_params
        ~cbor fname

  let to_proto ?(abort_on_parsing_failure = true) ?(block_dim = None)
      ?(grid_dim = None) ?(includes = []) ?(exit_status = 2)
      ?(inline_calls = true) ?(only_globals = true) ?(macros = [])
      ?(cu_to_json = "cu-to-json") ?(ignore_asserts = false)
      ?(assume_launch = false) ?(launch_params = false) ?(cbor = false)
      (fname : string) : proto_kernel t =
    let parsed =
      to_imp ~cu_to_json ~abort_on_parsing_failure ~block_dim ~grid_dim
        ~includes ~exit_status ~macros ~ignore_asserts ~assume_launch
        ~launch_params ~cbor fname
    in
    let compiled =
      Phase_timer.measure "inference/imp-to-proto" (fun () ->
        Imp.Compiler.compile_all ~inline_calls parsed.kernels)
    in
    {
      parsed with
      kernels =
        compiled
        |> List.filter (fun k ->
            (not only_globals) || (only_globals && Protocols.Kernel.is_global k));
    }
end

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
