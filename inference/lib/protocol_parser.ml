open Stage0
open Protocols

type imp_kernel = Imp.Kernel.t
type proto_kernel = Protocols.Kernel.t
type 'a t = { options : Gv_parser.t; kernels : 'a list }

module Make (L : Logger.Logger) = struct
  module D = D_to_imp.Make (L)

  let cu_to_imp ?(abort_on_parsing_failure = true) ?(block_dim = None)
      ?(grid_dim = None) ?(includes = []) ?(macros = []) ?(exit_status = 2)
      ?(cu_to_json = "cu-to-json") ?(ignore_asserts = false) (fname : string) :
      imp_kernel t =
    let j =
      Cu_to_json.cu_to_json
        ~ignore_fail:(not abort_on_parsing_failure)
        ~on_error:(fun _ -> exit exit_status)
        ~includes ~macros ~exe:cu_to_json fname
    in
    let options : Gv_parser.t =
      match Gv_parser.parse fname with
      | Some gv ->
          Logger.Colors.info
            ("Found GPUVerify args in source file: " ^ Gv_parser.to_string gv);
          gv
      | None -> Gv_parser.make ()
    in
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
    match C_lang.Program.parse j with
    | Ok k1 ->
        let kernels = k1 |> D_lang.rewrite_program |> D.parse_program in
        let kernels =
          if ignore_asserts then
            List.map Imp.Kernel.remove_global_asserts kernels
          else kernels
        in
        Stdlib.flush_all ();
        { options; kernels }
    | Error e ->
        Rjson.print_error e;
        exit exit_status

  let wgsl_to_imp ?(block_dim = None) ?(grid_dim = None) ?(exit_status = 2)
      ?(wgsl_to_json = "wgsl-to-json") ?(ignore_asserts = false)
      (fname : string) : imp_kernel t =
    let j =
      Wgsl_to_json.wgsl_to_json
        ~on_error:(fun _ -> exit exit_status)
        ~exe:wgsl_to_json fname
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
    match W_lang.Program.parse j with
    | Ok p ->
        let kernels = W_to_imp.translate p in
        let kernels =
          if ignore_asserts then
            List.map Imp.Kernel.remove_global_asserts kernels
          else kernels
        in
        Stdlib.flush_all ();
        { options; kernels }
    | Error e ->
        Rjson.print_error e;
        exit exit_status

  let to_imp ?(abort_on_parsing_failure = true) ?(block_dim = None)
      ?(grid_dim = None) ?(includes = []) ?(macros = []) ?(exit_status = 2)
      ?(cu_to_json = "cu-to-json") ?(wgsl_to_json = "wgsl-to-json")
      ?(ignore_asserts = false) (fname : string) : imp_kernel t =
    if String.ends_with ~suffix:".wgsl" fname then
      wgsl_to_imp ~block_dim ~grid_dim ~exit_status ~wgsl_to_json
        ~ignore_asserts fname
    else
      cu_to_imp ~abort_on_parsing_failure ~block_dim ~grid_dim ~includes ~macros
        ~exit_status ~cu_to_json ~ignore_asserts fname

  let to_proto ?(abort_on_parsing_failure = true) ?(block_dim = None)
      ?(grid_dim = None) ?(includes = []) ?(exit_status = 2)
      ?(inline_calls = true) ?(only_globals = true) ?(macros = [])
      ?(cu_to_json = "cu-to-json") ?(ignore_asserts = false) (fname : string) :
      proto_kernel t =
    let parsed =
      to_imp ~cu_to_json ~abort_on_parsing_failure ~block_dim ~grid_dim
        ~includes ~exit_status ~macros ~ignore_asserts fname
    in
    {
      parsed with
      kernels =
        parsed.kernels
        |> Imp.Compiler.compile_all ~inline_calls
        |> List.filter (fun k ->
               (not only_globals)
               || (only_globals && Protocols.Kernel.is_global k));
    }
end

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
