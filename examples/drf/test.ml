open Stage0

(* -------- Define the actual tests: ------------- *)

let tests =
  [
    (* The example should be DRF *)
    ("parse-gv.cu", [], 0);
    (* Unless we override the parameters with something other than
     what is in the source code. *)
    ("parse-gv.cu", [ "--gridDim=3" ], 1);
    (* This is the simplest data-race. *)
    ("racy-saxpy.cu", [], 1);
    (* Sanity check, make sure that the bit-vector logic works. *)
    ("racy-saxpy.cu", [ "--logic"; "QF_AUFBV" ], 1);
    (* This is the simplest data-race free example. *)
    ("drf-saxpy.cu", [], 0);
    (* The kernel contains constraints that makes it DRF: blockDim.{y,z}=1
     and gridDim.{y,z}=1. *)
    ("drf-saxpy.cu", [ "--all-dims"; "--all-levels" ], 0);
    (* Same kernel as drf-saxpy.cu but without the in-source __assume()s:
     racy under all-dims/all-levels because blockDim.{y,z} or gridDim.{y,z}
     may exceed 1, allowing two threads to compute the same index. *)
    ("drf-assume.cu", [ "--all-dims"; "--all-levels" ], 1);
    (* Same kernel, but the missing __assume() constraints are injected via
     the --assume CLI flag. Two assumptions are passed to verify that
     --assume composes when repeated. *)
    ( "drf-assume.cu",
      [
        "--all-dims";
        "--all-levels";
        "--assume";
        "blockDim.y == 1 && blockDim.z == 1";
        "--assume";
        "gridDim.y == 1 && gridDim.z == 1";
      ],
      0 );
    (* This example is only racy at the grid-level *)
    ("racy-grid-level.cu", [], 0);
    ("racy-grid-level.cu", [ "--grid-level" ], 1);
    (* This is a data-race in a 2D shared array. *)
    ("racy-2d.cu", [], 1);
    (* This is a data-race on a shared scalar. *)
    ("racy-shared-scalar.cu", [], 1);
    (* A data-race in shared memory is invisible at the grid level. *)
    ("racy-shared-scalar.cu", [ "--grid-level" ], 0);
    (* A data-race free example that relies on top-level assignments. *)
    ("drf-toplevel.cu", [], 0);
    (* A data-race that occurs when analysis understand top-level assignments.
     We ensure it's a data-race between threads 0 and 1. *)
    ("racy-toplevel.cu", [ "--tid1"; "0"; "--tid2"; "1" ], 1);
    (* Data-race free example *)
    ("drf-shared-mem.cu", [], 0);
    (* Shared memory *)
    ("racy-shared-mem.cu", [], 1);
    (* Shared memory in a device function *)
    ("racy-shared-mem-2.cu", [], 1);
    (* Data-race free example with array aliasing *)
    ("drf-alias.cu", [], 0);
    (* Data-race free example with array aliasing *)
    ("racy-alias.cu", [], 1);
    (* Data-race with atomics. *)
    ("racy-atomics.cu", [], 1);
    (* A data-race that occurs when we have warp-concurrent semantics *)
    ("racy-reduce.cu", [], 1);
    (* A data-race free example as long as the analysis understands typedefs. *)
    ("drf-typedef.cu", [], 0);
    (* The running example of CAV21 *)
    ("racy-cav21.cu", [], 1);
    (* The fixed running example of CAV21 *)
    ("drf-cav21.cu", [], 0);
    (* A racy example *)
    ("racy-device.cu", [], 1);
    (* A data-race that uses aliasing and templated arrays *)
    ("racy-template-alias.cu", [], 1);
    (* A data-race that uses aliasing and templated arrays *)
    ("racy-template.cu", [], 1);
    (* Improve the support for creating decls due to mutation *)
    ("racy-mutation.cu", [], 1);
    (* Support for enumerates *)
    ("drf-enum.cu", [], 0);
    (* Support for anonymous enumerates named via typedef *)
    ("drf-enum-typedef.cu", [], 0);
    (* Support for enumerates *)
    ("drf-enum-constraint.cu", [], 0);
    (* Aliasing using shared memory (example 1) *)
    ("racy-alias-shmem1.cu", [], 1);
    (* Aliasing using shared memory (example 1) *)
    ("racy-alias-shmem2.cu", [], 1);
    (* Aliasing using shared memory (example 1) *)
    ("racy-alias-shmem3.cu", [], 1);
    (* Aliasing with increment *)
    ("racy-alias-assign.cu", [], 1);
    (* Array accesses of local memory should not introduce data-races. *)
    ("drf-local-array.cu", [], 0);
    (* Check support for macros *)
    ("macro.cu", [ "-DMACRO=" ], 0);
    ("macro.cu", [ "-DMACRO=+ 0" ], 0);
    ("macro.cu", [ "-DMACRO=+ 1" ], 1);
    (* data-race *)
    ("macro.cu", [ "-DMACRO" ], 2);
    (* expands to 1, which is a syntax error *)
    ("macro.cu", [], 2);
    (* syntax error if the macro is not defined *)
    (* A conditional break is inferred as an assertion *)
    ("drf-assert-loop.cu", [], 0);
    (* Bug from generating unknowns from function calls *)
    ("racy-funcion-call-unknowns.cu", [], 1);
    (* Bug from generating unknowns from a kernel call *)
    ("racy-kernel-calls-return.cu", [], 1);
    (* (int j = 0; j < n; j++) *)
    ("drf-loop1.cu", [], 0);
    (* (int j = n; j >= 0; j--) *)
    ("drf-loop2.cu", [], 0);
    (* (int i = 0; i <= 4; i++) *)
    ("drf-loop3.cu", [], 0);
    (* (int i = 4; i - k; i++) *)
    ("drf-loop4.cu", [], 0);
    (* (int j = n; j > 0; j--) *)
    ("drf-loop5.cu", [], 0);
    (* (int j = 1; j + k < n; j++) *)
    ("drf-loop6.cu", [], 0);
    (* (int j = 0; j <= n; j++) *)
    ("racy-loop1.cu", [ "-p"; "n=0"; "--index=[0]" ], 1);
    (* (int j = n; j >= 0; j--) *)
    ("racy-loop2.cu", [ "-p"; "n=1"; "--index=[1]" ], 1);
    (* the comma operator *)
    ("racy-comma.cu", [], 1);
    (* the comma operator *)
    ("drf-comma.cu", [], 0);
    (* index of a templated type *)
    ("drf-template-index.cu", [], 0);
    (* Each launch of a templated kernel produces its own specialisation
     alongside the primary template; every specialisation must be
     parsed as a separate kernel, not collapsed into the primary. *)
    ("drf-template-instances.cu", [], 0);
    (* Variadic-template kernel: the [vals...] parameter-pack expansion
     in the primary template body must be preserved through parsing
     rather than collapsed away. *)
    ("drf-template-pack.cu", [], 0);
    (* Variadic-template kernel with explicit launches generating
     [variadic<int>] and [variadic<int, int>] specialisations. The
     resolved template arguments must reach faial as a pack-shaped
     TemplateArgument whose elements are the individual concrete
     types. *)
    ("drf-template-pack-instances.cu", [], 0);
    (* Templated kernel writing [Traits<T>::value] to a single shared
     index from every thread. With no explicit launch, the primary
     template body is parsed and the qualified dependent reference
     reaches the analyser as a [DependentScopeRef] rather than
     collapsing to RecoveryExpr. *)
    ("racy-template-dep-scope.cu", [], 1);
    (* Launch metadata: one [<<<grid, block>>>] launch with host-side
     dim3 locals and a templated kernel argument. The LaunchParam node
     emitted alongside the AST must parse without disturbing the
     kernel-level DRF analysis. *)
    ("drf-launch-param.cu", [], 0);
    (* --assume-launch must rescue an under-constrained kernel that is
     racy when blockDim/gridDim's [y]/[z] axes are free: synthesising
     the launch's [dim3((n+255)/256)] / [dim3(256)] pins the unused
     axes to 1 via [assert(...)] in the pseudo-kernel body. *)
    ("drf-launch-rescue.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Two distinct launches of the same templated kernel must each
     produce their own pseudo-kernel and analyse independently with
     the launch's concrete dims. *)
    ("drf-launch-multi.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Negative control: a kernel that races regardless of launch
     dims (every thread writes [out[0]]) stays racy under
     [--assume-launch] — pinning blockDim doesn't suppress real
     races. *)
    ("racy-launch-mismatch.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 1);
    (* A scalar kernel arg supplied by a non-Ident launch-site
     expression (here [params[0]]). The launch-arg resolver folds
     the array-subscript into a fresh uniform pseudo-parameter so
     the formal stays block-uniform; analyses DRF. Without the
     resolver, this would false-positive racy because the launch
     arg surfaces as a per-thread @AccessState. *)
    ("drf-launch-complex-arg.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* A scalar kernel arg fed by a [const int N = 256] host
     variable that c-to-json const-folds to its literal value at
     the launch site. The resolver passes literals through as
     [Const] so the inliner substitutes the kernel formal with
     [256] directly. Without pass-through, the formal stays
     unbound and Z3 picks an adversarial witness, false-positive
     reporting racy on a kernel where every (blockIdx.x,
     threadIdx.x) pair writes a distinct address. *)
    ("drf-launch-const-arg.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* 2d array *)
    ("drf-2d.cu", [], 0);
    (* add support for side-effects (reads/writes) in the conditions as commas *)
    ("drf-loop-comma-in-cond.cu", [], 0);
    ("racy-loop-comma-in-cond.cu", [], 1);
    (* support for inlining functions which return values *)
    ("drf-inline-var.cu", [], 0);
    (* ensure that an aligned protocol remains aligned *)
    ("drf-loop-aligned-1.cu", [], 0);
    (* End-to-end smoke test for IntegerLiteral parsing of uint64
     sentinels that exceed OCaml's 63-bit int — they must reach the
     analyser as concrete two's-complement values, not the
     [Int.max_int] fallback. *)
    ("drf-uint64-sentinel.cu", [], 0);
    (* C++11 range-based for over a fixed-size array: the bound is
     extracted from the RangeStmt's qualType so the iteration
     variable becomes [arr[__idx]] inside a bounded foreach,
     instead of an unbounded Star. *)
    ("drf-range-for.cu", [], 0);
    (* Pointer parameter with [volatile T * const __restrict]
     qualifier stack: c_type's pointer detection must normalise the
     trailing qualifier soup so the parameter classifies as a
     global array. Without normalisation, the parameter is
     Unsupported and every access lowers to [skip], producing a
     false-negative DRF on a kernel that races on every thread. *)
    ("racy-qualified-pointer.cu", [], 1);
  ]

(* These are kernels that are being documented, but are
   not currently being checked *)
let unsupported : Fpath.t list =
  [
    "drf-warp.cu";
    "racy-warp.cu";
    "racy-device-ref.cu";
    (* example where assignment is used as an expression, rather
     than a statement *)
    "drf-assign-exp.cu";
    (* Data-race free requires understanding fields in parameters. *)
    "drf-field-in-param.cu";
    (* A racy example that uses structs *)
    "racy-struct.cu";
    (* A racy example that calls a device function without array as args *)
    "racy-device-no-args.cu";
  ]
  |> List.map (fun x -> Fpath.(v "." / x))

(* ---- Testing-specific code ----- *)

(* Get the absolute path of the test binary *)
let test_exe : Fpath.t = Fpath.(v Sys.executable_name |> normalize)

(* Get the absolute path of the test binary *)
let test_dir : Fpath.t = Fpath.(test_exe |> parent)

(* Get the absolute path of the build directory *)
let build_dir : Fpath.t = Fpath.(test_dir |> parent |> parent |> normalize)

(* Get the absolute path of the root of our project *)
let workspace_dir : Fpath.t = Fpath.(build_dir |> parent |> parent)

(* Get the path of faial-drf *)
let faial_drf_exe : Fpath.t = Fpath.(build_dir / "drf" / "bin" / "main.exe")

let faial_drf ?(args = []) (fname : Fpath.t) : Subprocess.t =
  Subprocess.make
    (Fpath.to_string faial_drf_exe)
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
    |> List.filter (Fpath.has_ext ".cu")
    |> Fpath.Set.of_list
  in
  let unsupported = Fpath.Set.of_list unsupported in
  Fpath.Set.diff (Fpath.Set.diff all_cu_files used_files) unsupported

let () =
  let open Fpath in
  print_endline "Checking examples for DRF:";
  Unix.chdir (Fpath.to_string test_dir);
  tests
  |> List.iter (fun (filename, args, expected_status) ->
      let str_args = if args = [] then "" else String.concat " " args ^ " " in
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
         (* Get the generated binary *)
         let exe =
           faial_drf_exe
           |> Fpath.relativize ~root:workspace_dir
           |> Option.value ~default:(Fpath.v "faial-drf")
           |> Fpath.to_string
         in
         (* Get the path of the test file *)
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
