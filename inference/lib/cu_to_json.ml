open Stage0

(* Drains the subprocess pipe into a Buffer.t. Kept separate from JSON
   parsing so [Phase_timer] can attribute pipe-read+subprocess time to
   "inference/cu-to-json" and parse time to "inference/yojson-parse".
   Streaming via [Yojson.Basic.from_channel] would conflate both. *)
let read_all (ic : in_channel) : string =
  let buf = Buffer.create (1 lsl 20) in
  let chunk = Bytes.create (1 lsl 16) in
  let rec loop () =
    let n = input ic chunk 0 (Bytes.length chunk) in
    if n > 0 then (Buffer.add_subbytes buf chunk 0 n; loop ())
  in
  loop ();
  Buffer.contents buf

let default_include_dirs () : string list =
  let include_dirs =
    [ "inference/cuda_include"; "faial/inference/cuda_include" ]
  in
  let find_include dir =
    List.find_map
      (fun include_dir ->
        let candidate =
          Filename.concat dir (Filename.concat include_dir "mma.h")
        in
        if Sys.file_exists candidate then Some (Filename.concat dir include_dir)
        else None)
      include_dirs
  in
  let rec search dir =
    match find_include dir with
    | Some dir -> Some dir
    | None ->
        let parent = Filename.dirname dir in
        if String.equal parent dir then None else search parent
  in
  match search (Sys.getcwd ()) with Some dir -> [ dir ] | None -> []

let cu_to_json_res ?(exe = "cu-to-json") ?(ignore_fail = false) ?(includes = [])
    ?(macros = []) ?(launch_params = false) ?(cbor = false)
    (fnames : string list) : (Yojson.Basic.t, int * string) Result.t =
  let includes = List.map (fun x -> "-I" ^ x) includes in
  let macros = List.map (fun x -> "-D" ^ x) macros in
  let extra =
    (* [--print-id] is what makes a call site resolvable to a definition.
       A [DeclRefExpr] carries only the referenced function's name and
       type, which two instantiations of one template share, so without
       the declaration identifier the two call sites are indistinguishable. *)
    [ "--print-id" ]
    @ (if launch_params then [ "--launch-params" ] else [])
    @ if cbor then [ "--cbor" ] else []
  in
  let args = fnames @ includes @ macros @ extra in
  let cmd = Filename.quote_command exe args in
  let r, raw =
    Phase_timer.measure "inference/cu-to-json" (fun () ->
      Unix.open_process_in cmd
      |> Subprocess.with_process_in read_all)
  in
  let j =
    if cbor then
      Phase_timer.measure "inference/cbor-decode" (fun () ->
        Cbor_json.from_string raw
        |> Result.map_error (fun e -> "CBOR decode error: " ^ e))
    else
      Phase_timer.measure "inference/yojson-parse" (fun () ->
        try Ok (Yojson.Basic.from_string raw)
        with Yojson.Json_error e -> Error e)
  in
  match (r, j) with
  | Unix.WEXITED 0, Ok j -> Ok j
  | Unix.WEXITED n, Ok j ->
      if ignore_fail then Ok j
      else Error (n, "Expecting exit status 0, but got " ^ string_of_int n)
  | Unix.WEXITED n, Error e -> Error (n, "Error parsing output: " ^ e)
  | _, Error e -> Error (1, e)
  | _, _ -> Error (1, "Unknown error")

let cu_to_json ?(exe = "cu-to-json") ?(ignore_fail = false) ?(includes = [])
    ?(macros = []) ?(launch_params = false) ?(cbor = false)
    (* If some integer is given, then we return that on exit, otherwise we return
     whatever cu-to-json returns *)
    ?(on_error = exit) (fnames : string list) : Yojson.Basic.t =
  match
    cu_to_json_res ~exe ~includes ~ignore_fail ~macros ~launch_params ~cbor
      fnames
  with
  | Ok x -> x
  | Error (r, m) ->
      prerr_endline ("cu-to-json: " ^ m);
      on_error r
