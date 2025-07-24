open Stage0

(* -------- Define the actual tests: ------------- *)

let tests =
  [
    (* This is the simplest data-race. *)
    ("racy-saxpy.wgsl", [], 1);
    (* This is the simplest data-race free example. *)
    ("drf-saxpy.wgsl", [], 0);
    (* The kernel contains constraints that makes it DRF: blockDim.{y,z}=1
     and gridDim.{y,z}=1. *)
    ("drf-saxpy.wgsl", [ "--all-dims"; "--all-levels" ], 0);
    ("drf-read-const.wgsl", [], 0);
    ("drf-indirect-read.wgsl", [], 0);
    (* Shows an example of a loop with a break_if keyword set *)
    ("drf-loop-break-if.wgsl", [], 0);
    ("drf-array-array.wgsl", [], 0);
    ("drf-array-vector-2.wgsl", [], 0);
    ("drf-array-vector.wgsl", [], 0);
    (* Include support for constant declarations *)
    ("drf-const.wgsl", [], 0);
    (* Overrides and unary operators *)
    ("drf-unary.wgsl", [], 0);
    (* Parse uniform load *)
    ("drf-uniform-load.wgsl", [], 0);
    (* Loop inference *)
    ("drf-loop-lt.wgsl", [], 0);
    (* Loop inference *)
    ("drf-loop-relminus.wgsl", [], 0);
    (* Kernel inlining with return value *)
    ("drf-inline-ret.wgsl", [], 0);
  ]

let unsupported : Fpath.t list = [] |> List.map (fun x -> Fpath.(v "." / x))

(* ---- Testing-specific code ----- *)

let faial_drf_path : Fpath.t = Files.from_string "../../drf/bin/main.exe"

let faial_drf ?(args = []) (fname : Fpath.t) : Subprocess.t =
  Subprocess.make
    (Fpath.to_string faial_drf_path)
    (args @ [ fname |> Fpath.to_string ])

let used_files : Fpath.Set.t =
  tests
  (* get just the filenames as paths *)
  |> List.map (fun (x, _, _) -> Fpath.(v "." / x))
  (* convert to a set *)
  |> Fpath.Set.of_list

let missed_files (dir : Fpath.t) : Fpath.Set.t =
  let all_cu_files : Fpath.Set.t =
    dir |> Files.read_dir
    |> List.filter (Fpath.has_ext ".wgsl")
    |> Fpath.Set.of_list
  in
  let unsupported = Fpath.Set.of_list unsupported in
  Fpath.Set.diff (Fpath.Set.diff all_cu_files used_files) unsupported

let check_wgsl_to_json () : unit =
  match
    Subprocess.make "wgsl-to-json" [ "--version" ] |> Subprocess.check_output
  with
  | Some ver -> print_endline ("Found: " ^ ver)
  | None ->
      print_endline "WGSL-TO-JSON NOT FOUND! Skipping tests related to WGSL.";
      exit 0

let () =
  let open Fpath in
  check_wgsl_to_json ();
  print_endline "Checking examples for DRF:";
  tests
  |> List.iter (fun (filename, args, expected_status) ->
         let str_args =
           if args = [] then "" else String.concat " " args ^ " "
         in
         let bullet =
           match expected_status with
           | 0 -> "DRF:   "
           | 1 -> "RACY:  "
           | 2 -> "PARSE: "
           | _ -> "?:     "
         in
         print_string (bullet ^ "faial-drf " ^ str_args ^ filename);
         Stdlib.flush_all ();
         let given = faial_drf ~args (v filename) |> Subprocess.run_split in
         (if given.status = Unix.WEXITED expected_status then print_endline " ✔"
          else
            let exit_code =
              Subprocess.exit_code given.status |> string_of_int
            in
            print_endline " ✘";
            print_endline
              "------------------------ OUTPUT ------------------------";
            print_endline given.stdout;
            print_endline given.stderr;
            print_endline
              ("ERROR: Expected return code "
              ^ string_of_int expected_status
              ^ " but got " ^ exit_code);
            exit 1);
         Stdlib.flush_all ());
  unsupported
  |> List.iter (fun f ->
         if not (Files.exists f) then (
           print_endline ("Missing unsupported file: " ^ Fpath.to_string f);
           exit 1)
         else print_endline ("TODO:  " ^ Fpath.to_string f));
  let missed = missed_files (v ".") in
  if not (Fpath.Set.is_empty missed) then (
    let missed =
      missed |> Fpath.Set.to_list |> List.sort Fpath.compare
      |> List.map Fpath.to_string |> String.concat " "
    in
    print_endline "";
    print_endline ("ERROR: The following files are not being checked: " ^ missed);
    exit (-1))
  else ()
