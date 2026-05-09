open Stage0
open Inference
open Protocols

(* Parses GPUVerify arguments from the CUDA file *)
let read_params (fname : string) : Gv_parser.t =
  match Gv_parser.parse fname with
  | Some gv ->
      Logger.Colors.info (fun () ->
        "Found GPUVerify args in source file: " ^ Gv_parser.to_string gv);
      gv
  | None -> Gv_parser.default

(* Parses a list of protocols from the CUDA file *)
let read_kernels (fname : string) : Protocols.Kernel.t list =
  Protocol_parser.Silent.to_proto fname |> fun x -> x.kernels

(* Generates the output file *)
let write_string (fname : string) (data : string) : unit =
  let oc = open_out fname in
  try
    output_string oc data;
    close_out oc
  with ex ->
    close_out oc;
    raise ex

(* Main function *)
let corvo (input_file : string) (output_file : string) (const_fold : bool)
    (distinct_vars : bool) (div_to_mult : bool) (expand_device : bool)
    (gen_params : bool) (mod_gv_args : bool) (racuda : bool)
    (simplify_kernel : bool) (toml : bool) (use_dummy_array : bool) : unit =
  let g : Generator.t =
    Generator.make ~const_fold ~distinct_vars ~div_to_mult ~expand_device
      ~gen_params ~mod_gv_args ~racuda ~simplify_kernel ~toml ~use_dummy_array
  in
  let gv, params = read_params input_file |> Prep.prepare_params g in
  let prepare_kernel = Prep.prepare_kernel g params in
  let kernels = read_kernels input_file |> List.map prepare_kernel in
  let generator = (if toml then Tgen.gen_toml else Cgen.gen_cuda) g gv in
  List.map generator kernels |> String.concat "\n" |> write_string output_file;
  if g.gen_params then
    Cgen.gen_params gv |> write_string (output_file ^ ".params")

open Cmdliner

(* Command-line interface *)
let input_file =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let output_file =
  let doc = "The path of the output file." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"OUTPUT" ~doc)

let const_fold =
  let doc = "Use constant folding to simplify the kernel code." in
  Arg.(value & flag & info [ "const-fold" ] ~doc)

let distinct_vars =
  let doc = "Make all loop variables distinct." in
  Arg.(value & flag & info [ "distinct-vars" ] ~doc)

let div_to_mult =
  let doc = "Convert division loops to multiplication loops." in
  Arg.(value & flag & info [ "div-to-mult" ] ~doc)

let expand_device =
  let doc = "Expand the __device__ attribute of function prototypes." in
  Arg.(value & flag & info [ "expand-device" ] ~doc)

let gen_params =
  let dim_to_s (d : Dim3.t) : string =
    let x, y, z = (string_of_int d.x, string_of_int d.y, string_of_int d.z) in
    "[" ^ x ^ ", " ^ y ^ ", " ^ z ^ "]"
  in
  let gv_to_s (gv : Gv_parser.t) : string =
    "pass = " ^ string_of_bool gv.pass ^ ", blockDim = " ^ dim_to_s gv.block_dim
    ^ ", and gridDim = " ^ dim_to_s gv.grid_dim ^ "."
  in
  let doc =
    "Generate a .params file of the GV args and include a preamble at "
    ^ "the beginning of the kernel. By default, the following is used: "
    ^ gv_to_s Gv_parser.default
  in
  Arg.(value & flag & info [ "gen-params" ] ~doc)

let mod_gv_args =
  let doc = "Modify blockDim parameters to improve RaCUDA's index analysis." in
  Arg.(value & flag & info [ "mod-gv-args" ] ~doc)

let racuda =
  let doc = "Generate a RaCUDA-friendly kernel." in
  Arg.(value & flag & info [ "r"; "racuda" ] ~doc)

let simplify_kernel =
  let doc =
    "Apply the following transformations to the generated "
    ^ "kernel: type-mult, nd-array, and uniform ranges."
  in
  Arg.(value & flag & info [ "simplify-kernel" ] ~doc)

let toml =
  let doc = "Generate a TOML file." in
  Arg.(value & flag & info [ "t"; "toml" ] ~doc)

let use_dummy_array =
  let doc = "Initialize local variables with an array instead of a function." in
  Arg.(value & flag & info [ "use-dummy-array" ] ~doc)

let corvo_t =
  Term.(
    const corvo $ input_file $ output_file $ const_fold $ distinct_vars
    $ div_to_mult $ expand_device $ gen_params $ mod_gv_args $ racuda
    $ simplify_kernel $ toml $ use_dummy_array)

let info =
  let doc = "Generates CUDA code from a protocol" in
  Cmd.info "faial-gen" ~doc

let () = Cmd.v info corvo_t |> Cmd.eval |> exit
