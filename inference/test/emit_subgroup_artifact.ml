open Stage0
open Inference

let source = ref None
let kernel_name = ref None
let cu_to_json = ref "./bin/cu-to-json"
let macros = ref []
let includes = ref []
let subgroup_size = ref 32
let ignore_asserts = ref false

let fail (msg : string) : 'a =
  prerr_endline msg;
  exit 2

let set_once (slot : string option ref) (name : string) (value : string) : unit
    =
  match !slot with
  | None -> slot := Some value
  | Some _ -> fail ("duplicate " ^ name)

let parse_args () : unit =
  let spec =
    [
      ( "--cu-to-json",
        Arg.Set_string cu_to_json,
        "PATH Use the given cu-to-json executable" );
      ("--kernel", Arg.String (set_once kernel_name "--kernel"), "NAME Kernel");
      ( "--subgroup-size",
        Arg.Set_int subgroup_size,
        "N Explicit CUDA x-contiguous subgroup size" );
      ( "-D",
        Arg.String (fun value -> macros := value :: !macros),
        "MACRO Add a CUDA preprocessor definition" );
      ( "-I",
        Arg.String (fun value -> includes := value :: !includes),
        "DIR Add a CUDA include directory" );
      ( "--ignore-asserts",
        Arg.Set ignore_asserts,
        "Record assertion stripping in artifact metadata" );
    ]
  in
  let anon value = set_once source "source file" value in
  Arg.parse spec anon
    "emit_subgroup_artifact [OPTIONS] SOURCE\n\
     Emits the focused OCaml subgroup/matrix source artifact."

let parse_source (fname : string) : D_lang.Program.t =
  match
    Cu_to_json.cu_to_json_res ~exe:!cu_to_json
      ~includes:(Cu_to_json.default_include_dirs () @ List.rev !includes)
      ~macros:(List.rev !macros) [fname]
  with
  | Error (status, msg) ->
      fail (Printf.sprintf "cu-to-json failed with status %d: %s" status msg)
  | Ok json -> (
      match C_lang.Program.parse json with
      | Ok program -> D_lang.rewrite_program program
      | Error error ->
          Rjson.print_error error;
          exit 2)

let split_context_and_kernel (name : string) (program : D_lang.Program.t) :
    D_lang.Def.t list * D_lang.Kernel.t =
  let context_rev, target =
    List.fold_left
      (fun (context_rev, target) def ->
        match def with
        | D_lang.Def.Kernel kernel when String.equal (D_lang.Kernel.name kernel) name ->
            (context_rev, Some kernel)
        | D_lang.Def.Kernel _ -> (context_rev, target)
        | D_lang.Def.Declaration _ | Typedef _ | Enum _ | LaunchParam _
        | Prototype _ | Record _ | UsingNamespace _ ->
            (def :: context_rev, target))
      ([], None) program
  in
  match target with
  | Some kernel -> (List.rev context_rev, kernel)
  | None -> fail ("kernel not found: " ^ name)

let subgroup_config () : Subgroup_matrix.Target_config.t =
  match Subgroup_matrix.Target_config.subgroup_size !subgroup_size with
  | Ok size -> Subgroup_matrix.Target_config.cuda_x_contiguous size
  | Error error -> fail (Subgroup_matrix.Target_config.error_to_string error)

let emit_artifact () : unit =
  parse_args ();
  let fname =
    match !source with
    | Some value -> value
    | None -> fail "missing source file"
  in
  let kernel =
    match !kernel_name with
    | Some value -> value
    | None -> fail "missing --kernel"
  in
  let program = parse_source fname in
  let context_defs, kernel_def = split_context_and_kernel kernel program in
  match
    Subgroup_source.route_kernel ~target_config:(subgroup_config ())
      ~context_defs kernel_def
  with
  | Error error -> fail (Subgroup_source.error_to_string error)
  | Ok (Ordinary_source _) ->
      fail ("kernel did not route to subgroup/matrix: " ^ kernel)
  | Ok (Subgroup_matrix subgroup) ->
      [
        "claim_label: rust_extension_oracle";
        "source: " ^ fname;
        "kernel: " ^ subgroup.matrix_kernel.name;
        "subgroup_size: " ^ string_of_int !subgroup_size;
        "ignore_asserts: " ^ string_of_bool !ignore_asserts;
        "macros: " ^ String.concat ", " (List.rev !macros);
      ]
      @ Subgroup_source.subgroup_kernel_artifact_summary subgroup
      |> List.iter print_endline

let () = emit_artifact ()
