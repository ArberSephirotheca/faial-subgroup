open Inference
open Protocols
open Barrier_div

(* Driver for the mutation operators. Reads a CUDA file and an input
   label, applies every operator in [Mutation.all], and writes the
   resulting kernels to <output_dir>/<kernel>.<op>.<idx>.<label>.cu. *)

(* Same kernel-level preprocessing the [drf] driver and [check.ml]
   apply: substitute integer parameters into globals, ensure free
   names have binders, and run constant folding. Mutation operators
   then see a simplified IR. *)
let preprocess (params : (string * int) list) (k : Kernel.t) : Kernel.t =
  k
  |> Kernel.inline_globals params
  |> Kernel.add_missing_binders
  |> Kernel.opt

let read_kernels ~macros ~params (fname : string) : Kernel.t list =
  Protocol_parser.Silent.to_proto ~macros fname
  |> (fun x -> x.kernels)
  |> List.map (preprocess params)

(* The IR uses [C_type.unknown] (literal "?") as a placeholder when
   inference can't recover the type of a binder — e.g. the anonymous
   loop counters introduced for [Star] iterations. The cgen emits
   "? <var>;" which doesn't parse as C++, so any mutant generated
   from such a kernel crashes [faial-sync]. Skip the whole kernel
   until inference is taught to recover these. *)
let has_unknown_type_decl (c : Code.t) : bool =
  Code.exists
    (function
      | Code.Decl { ty; _ } -> C_type.to_string ty = "?"
      | _ -> false)
    c

let read_params (fname : string) : Gv_parser.t =
  Gv_parser.parse fname |> Option.value ~default:Gv_parser.default

let write_string (fname : string) (data : string) : unit =
  let oc = open_out fname in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
      output_string oc data)

let main (input : string) (output_dir : string) (ws_label_str : string)
    (bd_label_str : string) (macros : string list)
    (params : (string * int) list) : unit =
  let input_label : Mutation.Label.pair =
    {
      well_sync = Mutation.Label.of_string ws_label_str;
      barrier_div = Mutation.Label.of_string bd_label_str;
    }
  in
  let g : Generator.t =
    Generator.make ~const_fold:false ~distinct_vars:false ~div_to_mult:false
      ~expand_device:false ~gen_params:false ~mod_gv_args:false ~racuda:false
      ~simplify_kernel:false ~toml:false ~use_dummy_array:false
  in
  let gv = read_params input in
  if not (Sys.file_exists output_dir) then Unix.mkdir output_dir 0o755;
  let input_stem = Filename.basename input |> Filename.remove_extension in
  let kernels = read_kernels ~macros ~params input in
  kernels
  |> List.iter (fun (k : Kernel.t) ->
         if has_unknown_type_decl k.code then
           print_endline
             ("skip " ^ input_stem ^ "." ^ k.name
            ^ ": kernel has decls with unknown type (?)")
         else
           Mutation.all
           |> List.iter (fun (m : Mutation.t) ->
                  let mutants = m.apply k in
                  let out_label = Mutation.relabel_pair m input_label in
                  List.iteri
                    (fun i (mut : Kernel.t) ->
                      let cuda = Cgen.gen_cuda g gv mut in
                      let stem =
                        Printf.sprintf "%s.%s.%s.%d.%s.cu" input_stem k.name
                          m.name i
                          (Mutation.Label.pair_to_filename out_label)
                      in
                      let path = Filename.concat output_dir stem in
                      write_string path cuda;
                      print_endline ("wrote " ^ path))
                    mutants))

open Cmdliner

let input_file : string Term.t =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let output_dir : string Term.t =
  let doc = "Directory to write mutants into." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"OUTPUT_DIR" ~doc)

let ws_label : string Term.t =
  let doc = "Well-sync label of the input kernel: well_sync or ill_sync." in
  Arg.(
    required
    & opt (some string) None
    & info [ "ws-label" ] ~docv:"LABEL" ~doc)

let bd_label : string Term.t =
  let doc =
    "Barrier-div label of the input kernel: well_sync or ill_sync."
  in
  Arg.(
    required
    & opt (some string) None
    & info [ "bd-label" ] ~docv:"LABEL" ~doc)

let macros : string list Term.t =
  let doc = "Define <macro> to <value> (or 1 if <value> omitted)." in
  Arg.(
    value & opt_all string []
    & info [ "D"; "macro" ] ~docv:"<macro>=<value>" ~doc)

let params : (string * int) list Term.t =
  let doc = "Set the value of an integer parameter." in
  Arg.(
    value & opt_all (pair ~sep:'=' string int) []
    & info [ "p"; "param" ] ~docv:"KEYVAL" ~doc)

let main_t : unit Term.t =
  Term.(
    const main $ input_file $ output_dir $ ws_label $ bd_label $ macros
    $ params)

let info =
  let doc = "Apply mutation operators to a CUDA kernel for dataset growth." in
  Cmd.info "faial-sync-mut" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
