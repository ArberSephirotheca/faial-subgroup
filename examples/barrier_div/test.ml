open Stage0

(* Each kernel in this directory documents the behavior of one or more of
   the synchronization properties. Three checks are exercised:

   - well-sync             — same thread, two executions of the same launch
                             agree on whether they reach each barrier.
   - barrier-div           — distinct threads in the same group agree on
                             whether they reach each barrier.
   - missing-participants  — every thread reaches every block-wide barrier
                             (path-condition VC; doesn't see named-bar
                             cardinality).

   Tests are tuples [(filename, args, expected_exit)] driving [faial-sync].
   Exit 0 means the property holds; exit 1 means the analyser flagged a
   counter-model. A small parallel list at the bottom drives [faial-sync-sym]
   on the cases where its symbolic-execution view diverges from the
   syntax-directed VC. *)

let sync_tests : (string * string list * int) list =
  [
    (* uniform-sync: a single unconditional __syncthreads. The trivial
       baseline — nothing to disagree on. *)
    ("uniform-sync.cu", [ "--check=well-sync" ], 0);
    ("uniform-sync.cu", [ "--check=barrier-div" ], 0);
    ("uniform-sync.cu", [ "--check=missing-participants" ], 0);
    (* sequential-syncs: two independent block-wide barriers in straight-
       line code — both reachable by all threads. *)
    ("sequential-syncs.cu", [ "--check=well-sync" ], 0);
    ("sequential-syncs.cu", [ "--check=barrier-div" ], 0);
    ("sequential-syncs.cu", [ "--check=missing-participants" ], 0);
    (* loop-uniform: barrier inside a loop with a kernel-parameter bound.
       The range goes to U; binder is shared. All checks pass. *)
    ("loop-uniform.cu", [ "--check=well-sync" ], 0);
    ("loop-uniform.cu", [ "--check=barrier-div" ], 0);
    ("loop-uniform.cu", [ "--check=missing-participants" ], 0);
    (* named-bar-tid-id: bar.sync with a per-thread bar id but uniform
       reach. The id is not part of the reach decision, so all checks pass. *)
    ("named-bar-tid-id.cu", [ "--check=well-sync" ], 0);
    ("named-bar-tid-id.cu", [ "--check=barrier-div" ], 0);
    ("named-bar-tid-id.cu", [ "--check=missing-participants" ], 0);
    (* named-bar-undersize: bar.sync 0,32 in a 16-thread block. The path-
       condition view sees every thread reaching the asm — no missing
       reachability. The cardinality mismatch is detected separately by
       faial-sync-sym (see [sym_tests]). *)
    ("named-bar-undersize.cu", [ "--block-dim=16"; "--check=well-sync" ], 0);
    ("named-bar-undersize.cu", [ "--block-dim=16"; "--check=barrier-div" ], 0);
    ("named-bar-undersize.cu",
      [ "--block-dim=16"; "--check=missing-participants" ], 0);
    (* named-bar-oversize: bar.sync 0,32 in a 64-thread block. Symmetric
       cardinality mismatch (cohort of 64 > expected 32). faial-sync's
       path-condition view doesn't see it for the same reason as undersize. *)
    ("named-bar-oversize.cu", [ "--block-dim=64"; "--check=well-sync" ], 0);
    ("named-bar-oversize.cu", [ "--block-dim=64"; "--check=barrier-div" ], 0);
    ("named-bar-oversize.cu",
      [ "--block-dim=64"; "--check=missing-participants" ], 0);

    (* convergent-if: two lexical __syncthreads sites, one per branch of a
       tid-conditional. Each thread reaches exactly one of them — barrier-div
       fails (peers disagree on each site) and missing-participants fails
       (each site has missing threads). well-sync passes (same thread,
       same launch always lands in the same branch). *)
    ("convergent-if.cu", [ "--check=well-sync" ], 0);
    ("convergent-if.cu", [ "--check=barrier-div" ], 1);
    ("convergent-if.cu", [ "--check=missing-participants" ], 1);
    (* tid-conditional: __syncthreads inside [tid_x < 17]. Half-ish the
       threads skip it — barrier-div and missing-participants both flag. *)
    ("tid-conditional.cu", [ "--check=well-sync" ], 0);
    ("tid-conditional.cu", [ "--check=barrier-div" ], 1);
    ("tid-conditional.cu", [ "--check=missing-participants" ], 1);
    (* tid-conditional under --all-dims: blockDim.x is symbolic, so the
       analyser can no longer prove every thread reaches the barrier and
       missing-participants flags. With --assume "blockDim.x <= 17"
       injected as a kernel pre-condition, every in-block tid_x satisfies
       the guard and the property holds. Exercises the --assume CLI flag
       (and that it composes with --all-dims). *)
    ("tid-conditional.cu",
      [ "--all-dims"; "--check=missing-participants" ], 1);
    ("tid-conditional.cu",
      [ "--all-dims"; "--check=missing-participants";
        "--assume"; "blockDim.x <= 17" ], 0);
    (* tid-mod: __syncthreads inside [tid_x % 2 == 0]. Same shape as
       tid-conditional but with a different guard. *)
    ("tid-mod.cu", [ "--check=well-sync" ], 0);
    ("tid-mod.cu", [ "--check=barrier-div" ], 1);
    ("tid-mod.cu", [ "--check=missing-participants" ], 1);
    (* loop-tid-bound: barrier inside [for i = 0..tid_x). Threads with tid=0
     iterate zero times, never reaching the barrier. *)
    ("loop-tid-bound.cu", [ "--check=well-sync" ], 0);
    ("loop-tid-bound.cu", [ "--check=barrier-div" ], 1);
    ("loop-tid-bound.cu", [ "--check=missing-participants" ], 1);

    (* decl-branch: branch on a memory-loaded decl. The source array
       is read-only, so two runs of the same thread (well-sync) see
       the same value and take the same path — well-sync passes.
       Two distinct threads (barrier-div) may see different values
       at different indices, and the single-thread reachability
       check (missing-participants) still flags. *)
    ("decl-branch.cu", [ "--check=well-sync" ], 0);
    ("decl-branch.cu", [ "--check=barrier-div" ], 1);
    ("decl-branch.cu", [ "--check=missing-participants" ], 1);
    (* decl-loop-bound: loop count read from memory into a per-thread
       decl. Same read-only-source pattern as above. *)
    ("decl-loop-bound.cu", [ "--check=well-sync" ], 0);
    ("decl-loop-bound.cu", [ "--check=barrier-div" ], 1);
    ("decl-loop-bound.cu", [ "--check=missing-participants" ], 1);
    (* decl-under-tid: nested case, tid-only branch wraps a decl-derived
       branch. *)
    ("decl-under-tid.cu", [ "--check=well-sync" ], 0);
    ("decl-under-tid.cu", [ "--check=barrier-div" ], 1);
    ("decl-under-tid.cu", [ "--check=missing-participants" ], 1);
    (* clz-range-sync: the barrier's guard is [__clz(t) <= 32], which
       every thread satisfies because __clz is declared to return a
       number between 0 and 32. A goal that does not carry the
       declaration leaves the result an arbitrary integer, lets one
       thread's exceed 32 while another's does not, and reports the
       barrier divergent. *)
    ("clz-range-sync.cu", [ "--check=barrier-div" ], 0);
    (* clz-range-divergent: the companion at a bound the declared range
       does not reach, so two threads can straddle it and the
       divergence is real. *)
    ("clz-range-divergent.cu", [ "--check=barrier-div" ], 1);
  ]

(* faial-sync-sym is the symbolic-execution variant. On most kernels it
   agrees with [faial-sync --check=missing-participants], so we don't
   double-cover them here. The cases below are where the two analyses
   genuinely diverge — useful as documentation of *what makes them
   different*. *)
let sym_tests : (string * string list * int) list =
  [
    (* convergent-if: faial-sync flags both lexical __syncthreads sites
       (each has missing threads from its own perspective). faial-sync-sym
       fuses them into one phase via shared barrier id, the merged cohort
       is the full block, and no error is reported. *)
    ("convergent-if.cu", [], 0);
    (* named-bar-undersize: bar.sync expects 32 participants but the block
       has 16. faial-sync's path-condition view doesn't see the cardinality
       mismatch; faial-sync-sym's static fast-path
       (expected > threads_per_warp) does. *)
    ("named-bar-undersize.cu", [ "--block-dim=16" ], 1);
    (* named-bar-oversize: cohort of 64 arrives at a bar.sync expecting 32.
       Caught by faial-sync-sym's small-distinctness SAT — 33 distinct
       in-block tids that all satisfy the cohort. faial-sync misses it
       (path-condition view: every thread reaches the asm, fine). *)
    ("named-bar-oversize.cu", [ "--block-dim=64" ], 1);
    (* incomplete-arrivals: bar.arrive 0,32 in a 16-thread block. The
       arrival cohort is the full block (16 < 32 expected), and there
       are no waiters — the membrane stays in a half-collected state.
       Exercises the split-phase [Incomplete_arrivals] diagnostic
       (distinct from [Missing_participants], which applies only to
       sync-only barriers where [b_a = b_w]). *)
    ("incomplete-arrivals.cu", [ "--block-dim=16" ], 1);
  ]

(* These are kernels in this directory that are intentionally not
   exercised by [sync_tests] — typically because the analyser hits a
   limitation. *)
let unsupported : Fpath.t list =
  [
    (* faial-sync-sym hits "Phase 1: only constant barrier counts
       supported" because the bar.sync's id is a per-thread expression
       (count is constant 32). [sync_tests] does cover this kernel for
       the three faial-sync properties. *)
    (* No file is currently fully unsupported. *)
  ]
  |> List.map (fun x -> Fpath.(v "." / x))

(* ---- Testing-specific code ----- *)

let test_exe : Fpath.t = Fpath.(v Sys.executable_name |> normalize)
let test_dir : Fpath.t = Fpath.(test_exe |> parent)
let build_dir : Fpath.t = Fpath.(test_dir |> parent |> parent |> normalize)
let workspace_dir : Fpath.t = Fpath.(build_dir |> parent |> parent)

let faial_sync_exe : Fpath.t =
  Fpath.(build_dir / "barrier_div" / "bin" / "check.exe")

let faial_sync_sym_exe : Fpath.t =
  Fpath.(build_dir / "barrier_div" / "bin" / "participants.exe")

let make_subprocess (exe : Fpath.t) ?(args = []) (fname : Fpath.t) :
    Subprocess.t =
  Subprocess.make
    (Fpath.to_string exe)
    (args @ [ fname |> Fpath.to_string ])

let used_files : Fpath.Set.t =
  let from list =
    list
    |> List.map (fun (x, _, _) -> Fpath.(v "." / x))
    |> Fpath.Set.of_list
  in
  Fpath.Set.union (from sync_tests) (from sym_tests)

let missed_files (dir : Fpath.t) : Fpath.Set.t =
  let all_cu_files : Fpath.Set.t =
    dir |> Files.read_dir
    |> List.filter (Fpath.has_ext ".cu")
    |> Fpath.Set.of_list
  in
  let unsupported = Fpath.Set.of_list unsupported in
  Fpath.Set.diff (Fpath.Set.diff all_cu_files used_files) unsupported

let bullet_for : int -> string = function
  | 0 -> "PASS:  "
  | 1 -> "FAIL:  "
  | _ -> "?:     "

let run_one ~(label : string) ~(exe : Fpath.t) (filename : string)
    (args : string list) (expected_status : int) : unit =
  let str_args = if args = [] then "" else String.concat " " args ^ " " in
  print_string (bullet_for expected_status ^ label ^ " " ^ str_args ^ filename);
  Stdlib.flush_all ();
  let given =
    make_subprocess exe ~args (Fpath.v filename) |> Subprocess.run_split
  in
  if given.status = Unix.WEXITED expected_status then print_endline " ✔"
  else (
    let exit_code = Subprocess.exit_code given.status |> string_of_int in
    print_endline " ✘";
    print_endline "------------------------ OUTPUT ------------------------";
    print_endline given.stdout;
    print_endline given.stderr;
    print_endline
      ("ERROR: Expected return code " ^ string_of_int expected_status
     ^ " but got " ^ exit_code);
    print_endline "";
    let exe_str =
      exe
      |> Fpath.relativize ~root:workspace_dir
      |> Option.value ~default:(Fpath.v label)
      |> Fpath.to_string
    in
    let rerun_file =
      Fpath.append test_dir (Fpath.v filename)
      |> Fpath.relativize ~root:workspace_dir
      |> Option.get |> Fpath.to_string
    in
    print_endline "Re-run file:";
    print_endline (" - " ^ exe_str ^ " " ^ String.concat " " (args @ [ rerun_file ]));
    let test_exe_str =
      test_exe |> Fpath.relativize ~root:workspace_dir
      |> Option.get |> Fpath.to_string
    in
    print_endline "Re-run test:";
    print_endline (" - dune exec " ^ test_exe_str);
    exit 1);
  Stdlib.flush_all ()

let () =
  print_endline "Checking examples for synchronization properties:";
  Unix.chdir (Fpath.to_string test_dir);
  sync_tests
  |> List.iter (fun (filename, args, expected_status) ->
         run_one ~label:"faial-sync" ~exe:faial_sync_exe filename args
           expected_status);
  print_endline "";
  print_endline "Checking documented divergences with faial-sync-sym:";
  sym_tests
  |> List.iter (fun (filename, args, expected_status) ->
         run_one ~label:"faial-sync-sym" ~exe:faial_sync_sym_exe filename
           args expected_status);
  unsupported
  |> List.iter (fun f ->
         if not (Files.exists f) then (
           print_endline ("Missing unsupported file: " ^ Fpath.to_string f);
           exit 1)
         else print_endline ("TODO:  " ^ Fpath.to_string f));
  let missed = missed_files (Fpath.v ".") in
  if not (Fpath.Set.is_empty missed) then (
    let missed =
      missed |> Fpath.Set.to_list |> List.sort Fpath.compare
      |> List.map Fpath.to_string |> String.concat " "
    in
    print_endline "";
    print_endline ("ERROR: The following files are not being checked: " ^ missed);
    exit (-1))
  else ()
