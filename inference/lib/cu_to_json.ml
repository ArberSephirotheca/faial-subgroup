open Stage0

let cu_to_json_res ?(exe = "cu-to-json") ?(ignore_fail = false) ?(includes = [])
    ?(macros = []) (fname : string) : (Yojson.Basic.t, int * string) Result.t =
  let includes = List.map (fun x -> "-I" ^ x) includes in
  let macros = List.map (fun x -> "-D" ^ x) macros in
  let args = [ fname ] @ includes @ macros in
  let cmd = Filename.quote_command exe args in
  let r, j =
    Unix.open_process_in cmd
    |> Subprocess.with_process_in (fun ic ->
        try Ok (Yojson.Basic.from_channel ic)
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
    ?(macros = [])
    (* If some integer is given, then we return that on exit, otherwise we return
     whatever cu-to-json returns *)
    ?(on_error = exit) (fname : string) : Yojson.Basic.t =
  match cu_to_json_res ~exe ~includes ~ignore_fail ~macros fname with
  | Ok x -> x
  | Error (r, m) ->
      prerr_endline ("cu-to-json: " ^ m);
      on_error r
