open Stage0

let wgsl_to_json_res ?(exe = "wgsl-to-json") (fname : string) :
    (Yojson.Basic.t, int * string) Result.t =
  let args = [ fname ] in
  let cmd = Filename.quote_command exe args in
  let r, j =
    Unix.open_process_in cmd
    |> Subprocess.with_process_in (fun ic ->
           try Ok (Yojson.Basic.from_channel ic)
           with Yojson.Json_error e -> Error e)
  in
  match (r, j) with
  | Unix.WEXITED 0, Ok j -> Ok j
  | Unix.WEXITED n, Ok _ ->
      Error (n, "Expecting exit status 0, but got " ^ string_of_int n)
  | Unix.WEXITED n, Error e -> Error (n, "Error parsing output: " ^ e)
  | _, Error e -> Error (1, e)
  | _, _ -> Error (1, "Unknown error")

let wgsl_to_json ?(exe = "wgsl-to-json")
    (* If some integer is given, then we return that on exit, otherwise we return
     whatever wgsl-to-json returns *)
    ?(on_error = exit) (fname : string) : Yojson.Basic.t =
  match wgsl_to_json_res ~exe fname with
  | Ok x -> x
  | Error (r, m) ->
      prerr_endline ("wgsl-to-json: " ^ m);
      on_error r
