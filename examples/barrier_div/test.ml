open Stage0

(* -------- Define the actual tests: -------------

   Each test is a triple: (filename, args, expected_status).
   Exit codes from faial-sync-sym:
     0 — every kernel is clean.
     1 — at least one kernel has participant errors. *)

let tests =
  [
    (* All threads sync uniformly — clean. *)
    ("uniform-sync.cu", [], 0);
    (* Branch is uniform across the block — clean. *)
    ("convergent-if.cu", [], 0);
    (* Two sequential syncs in straight-line code — clean. *)
    ("sequential-syncs.cu", [], 0);
    (* Loop with uniform bound — clean. *)
    ("loop-uniform.cu", [], 0);
    (* Sync inside a tid-conditional — only some threads arrive. *)
    ("tid-conditional.cu", [], 1);
    (* asm("bar.sync 0, 32") executed in a 16-thread block — undersized. *)
    ("named-bar-undersize.cu", [ "--block-dim=16" ], 1);
  ]

(* These are kernels that the analysis handles, but are too costly to
   include in the routine test suite. *)
let unsupported : Fpath.t list =
  [
    (* Loop bound depends on threadIdx.x. The diagnosis is correct
       (Missing participants, cohort_size = 0), but the run currently
       costs up to 4 * --timeout because is_finished and of_phase_solo
       each issue a max+min pair against the same arrive_cohort and the
       results are not memoized. With --timeout=30000 the run takes
       ~120s — too slow for the test suite until the duplication is
       fixed. *)
    "loop-tid-bound.cu";
  ]
  |> List.map (fun x -> Fpath.(v "." / x))

(* ---- Testing-specific code ----- *)

let test_exe : Fpath.t = Fpath.(v Sys.executable_name |> normalize)
let test_dir : Fpath.t = Fpath.(test_exe |> parent)
let build_dir : Fpath.t = Fpath.(test_dir |> parent |> parent |> normalize)
let workspace_dir : Fpath.t = Fpath.(build_dir |> parent |> parent)

let faial_sync_sym_exe : Fpath.t =
  Fpath.(build_dir / "barrier_div" / "bin" / "participants.exe")

let faial_sync_sym ?(args = []) (fname : Fpath.t) : Subprocess.t =
  Subprocess.make
    (Fpath.to_string faial_sync_sym_exe)
    (args @ [ fname |> Fpath.to_string ])

let used_files : Fpath.Set.t =
  tests
  |> List.map (fun (x, _, _) -> Fpath.(v "." / x))
  |> Fpath.Set.of_list

let missed_files (dir : Fpath.t) : Fpath.Set.t =
  let all_cu_files : Fpath.Set.t =
    dir |> Files.read_dir
    |> List.filter (Fpath.has_ext ".cu")
    |> Fpath.Set.of_list
  in
  let unsupported = Fpath.Set.of_list unsupported in
  Fpath.Set.diff (Fpath.Set.diff all_cu_files used_files) unsupported

let () =
  let open Fpath in
  print_endline "Checking examples for barrier participants:";
  Unix.chdir (Fpath.to_string test_dir);
  tests
  |> List.iter (fun (filename, args, expected_status) ->
      let str_args = if args = [] then "" else String.concat " " args ^ " " in
      let bullet =
        match expected_status with
        | 0 -> "OK:    "
        | 1 -> "ERR:   "
        | _ -> "?:     "
      in
      print_string (bullet ^ "faial-sync-sym " ^ str_args ^ filename);
      Stdlib.flush_all ();
      let given = faial_sync_sym ~args (v filename) |> Subprocess.run_split in
      (if given.status = Unix.WEXITED expected_status then print_endline " ✔"
       else
         let exit_code = Subprocess.exit_code given.status |> string_of_int in
         print_endline " ✘";
         print_endline
           "------------------------ OUTPUT ------------------------";
         print_endline given.stdout;
         print_endline given.stderr;
         print_endline
           ("ERROR: Expected return code "
           ^ string_of_int expected_status
           ^ " but got " ^ exit_code);
         print_endline "";
         let exe =
           faial_sync_sym_exe
           |> Fpath.relativize ~root:workspace_dir
           |> Option.value ~default:(Fpath.v "faial-sync-sym")
           |> Fpath.to_string
         in
         let filename =
           test_dir / filename
           |> Fpath.relativize ~root:workspace_dir
           |> Option.get |> Fpath.to_string
         in
         print_endline "Re-run file:";
         print_endline
           (" - " ^ exe ^ " " ^ String.concat " " (args @ [ filename ]));
         let test_exe =
           test_exe
           |> Fpath.relativize ~root:workspace_dir
           |> Option.get |> Fpath.to_string
         in
         print_endline "Re-run test:";
         print_endline (" - dune exec " ^ test_exe);
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
