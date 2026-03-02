open Stage0

let run ~data_dep ~faial_drf (f : Fpath.t) : string option =
  let approx, variables =
    match Common.get_lines ~offset:0 ~count:2 (Fpath.to_string f) with
    | [ l1; l2 ] ->
        ( Slice.from_start 2 |> Slice.substring l1 |> String.trim,
          Slice.from_start 2 |> Slice.substring l2 |> String.trim )
    | _ -> failwith "impossible"
  in
  let exe = data_dep f in
  let given = exe |> Subprocess.run_split in
  let output = given.stdout |> String.trim in
  if given.status <> Unix.WEXITED 0 then
    Some
      ("ERROR RUNNING: " ^ Subprocess.to_string exe
     ^ "\nExpecting status 0, but got: "
      ^ Subprocess.Completed2.to_string given)
  else if output <> approx then
    Some ("Expecting '" ^ approx ^ "' got '" ^ output ^ "'")
  else
    let exe = faial_drf f in
    let given = exe |> Subprocess.run_split in
    let output = given.stdout |> Common.string_split_lines in
    let idx = List.nth_opt output 4 in
    if Option.is_none idx then
      Some
        ("Unable to parse output: expecting at least 4 lines got:\n"
       ^ given.stdout)
    else
      let output =
        idx |> Option.get
        (* approx locals: _unknown_1; *)
        |> Common.split ':'
        |> Option.get
        (* ('approx locals:', ' _unknown_1;') *)
        |> snd
        (* ' _unknown_1;' *)
        |> String.trim
        (* '_unknown_1;' *)
      in
      (* '_unknown_1' *)
      let output = Slice.from_finish (-1) |> Slice.substring output in
      if variables <> output then
        Some ("Expecting variables '" ^ variables ^ "' but got '" ^ output ^ "'")
      else None

let () =
  print_endline "Checking examples for approximation analysis:";
  let base_dir =
    Array.get Sys.argv 0 |> Fpath.v
    |> Fpath.parent (* get the parent directory *)
  in
  let data_dep_path : Fpath.t =
    Fpath.append base_dir (Files.from_string "../../approx/bin/main.exe")
  in
  let data_dep (fname : Fpath.t) : Subprocess.t =
    Subprocess.make (Fpath.to_string data_dep_path) [ fname |> Fpath.to_string ]
  in
  let faial_drf_path : Fpath.t =
    Fpath.append base_dir (Files.from_string "../../drf/bin/main.exe")
  in
  let faial_drf (fname : Fpath.t) : Subprocess.t =
    Subprocess.make
      (Fpath.to_string faial_drf_path)
      [ fname |> Fpath.to_string; "--show-flat-acc" ]
  in
  base_dir |> Files.read_dir
  |> List.filter (Fpath.has_ext ".cu")
  |> List.sort Fpath.compare
  |> List.iter (fun f ->
      print_string (" - " ^ Fpath.filename f);
      Stdlib.flush_all ();
      (match run ~data_dep ~faial_drf f with
      | None -> print_endline " ✔"
      | Some e ->
          print_endline " ✘\n";
          print_endline ("faial-drf --show-flat-acc " ^ Fpath.filename f);
          print_endline e;
          print_endline "";
          exit 1);
      Stdlib.flush_all ())
