open Inference
open Protocols
open Barrier_div

(* Driver for the mutation operators. Reads a CUDA file and an input
   label, applies every operator in [Mutation.all], and writes the
   resulting kernels to <output_dir>/<kernel>.<op>.<idx>.<label>.cu. *)

let read_kernels (fname : string) : Kernel.t list =
  Protocol_parser.Silent.to_proto fname
  |> (fun x -> x.kernels)
  |> List.map Kernel.add_missing_binders

let read_params (fname : string) : Gv_parser.t =
  Gv_parser.parse fname |> Option.value ~default:Gv_parser.default

let write_string (fname : string) (data : string) : unit =
  let oc = open_out fname in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
      output_string oc data)

let label_of_string : string -> Mutation.Label.t = function
  | "well" | "well_sync" | "wellsync" | "WellSync" -> WellSync
  | "ill" | "ill_sync" | "illsync" | "IllSync" -> IllSync
  | s -> invalid_arg ("unknown label: " ^ s)

let main (input : string) (output_dir : string) (label_str : string) : unit =
  let input_label = label_of_string label_str in
  let g : Generator.t =
    Generator.make ~const_fold:false ~distinct_vars:false ~div_to_mult:false
      ~expand_device:false ~gen_params:false ~mod_gv_args:false ~racuda:false
      ~simplify_kernel:false ~toml:false ~use_dummy_array:false
  in
  let gv = read_params input in
  if not (Sys.file_exists output_dir) then Unix.mkdir output_dir 0o755;
  let input_stem = Filename.basename input |> Filename.remove_extension in
  let kernels = read_kernels input in
  kernels
  |> List.iter (fun (k : Kernel.t) ->
         Mutation.all
         |> List.iter (fun (m : Mutation.t) ->
                let mutants = m.apply k in
                let out_label = m.relabel input_label in
                List.iteri
                  (fun i (mut : Kernel.t) ->
                    let cuda = Cgen.gen_cuda g gv mut in
                    let stem =
                      Printf.sprintf "%s.%s.%s.%d.%s.cu" input_stem k.name
                        m.name i
                        (Mutation.Label.to_string out_label)
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

let input_label : string Term.t =
  let doc = "Label of the input kernel: well_sync or ill_sync." in
  Arg.(
    required & opt (some string) None & info [ "l"; "label" ] ~docv:"LABEL" ~doc)

let main_t : unit Term.t =
  Term.(const main $ input_file $ output_dir $ input_label)

let info =
  let doc = "Apply mutation operators to a CUDA kernel for dataset growth." in
  Cmd.info "faial-mutate" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
