open Stage0

(* -------- Define the actual tests: ------------- *)

let per_request_tests =
  [
    ("setRowReadRow.cu", [ "--only-reads" ], "1");
    ("setRowReadRow.cu", [ "--only-writes" ], "1");
    ("setColReadCol.cu", [ "--only-reads" ], "32");
    ("setColReadCol.cu", [ "--only-writes" ], "32");
    ("setRowReadCol.cu", [ "--only-reads" ], "32");
    ("setRowReadCol.cu", [ "--only-writes" ], "1");
    ("setRowReadColPad.cu", [ "--only-reads" ], "1");
    ("setRowReadColPad.cu", [ "--only-writes" ], "1");
  ]

let bc_tests =
  [
    ("2tid.cu", [ "--blockDim=1024"; "--gridDim=2" ], "1");
    ("4tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "3");
    ("6tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "1");
    ("8tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "7");
    ("10tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "1");
    ("12tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "3");
    ("14tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "1");
    ("16tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "15");
    ("18tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "1");
    ("20tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "3");
    ("22tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "1");
    ("24tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "7");
    ("26tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "1");
    ("28tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "3");
    ("30tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "1");
    ("32tid.cu", [ "--blockDim=1024"; "--gridDim=1" ], "31");
    (* [m] is all ones, so C's unsigned shift [m >> 60] is 15 and the stride
       is 16: two banks, sixteen threads each, which is 32tid's neighbour
       16tid at cost 15. The protocol models [m] as -1, a value no width
       produces, and the per-thread simulator used to shift it at OCaml's 63
       bits, answering 7 for the shift, 8 for the stride and 7 for the cost,
       which understates a cost that is meant to be an upper bound. The
       simulator now declines the shift and the analysis falls back to the
       whole warp, 31, which is above the true 15. *)
    ("rsh-unsigned-wrap.cu", [], "31");
    ("assume.cu", [], "1");
    ("tidx-tidy.cu", [ "--blockDim=[16,16]" ], "1");
    ("tidx-tidy.cu", [ "--blockDim=[32,32]" ], "0");
    ("loops-1.cu", [], "Σ_{i | 0 ≤ i ≤ (n - 1)} 1");
    ("loops-1-step-plus.cu", [], "Σ_{j | 0 ≤ j ≤ ⌊(ub - lb) / step⌋} 1");
    ("loops-1-step-minus.cu", [], "Σ_{j | 0 ≤ j ≤ ⌊(ub - lb) / step⌋} 1");
    ( "loops-1-step-minus.cu",
      [ "-p"; "lb=2"; "-p"; "ub=33"; "-p"; "step=5" ],
      "7" );
    ("loops-1-div.cu", [], "9");
    ("loops-1-bitwise.cu", [], "Σ_{i | 0 ≤ i ≤ ((n << m) - 1)} 1");
    ("loops-1-minus.cu", [], "Σ_{i | 0 ≤ i ≤ n} 1");
    ("loops-1-multiple.cu", [], "11");
    ("loops-1-pow.cu", [ "--block-dim"; "512" ], "8");
    ("loops-1-pow-2.cu", [], "9");
    ("loops-1-pow-unknown-bound.cu", [], "Σ_{i | 1 ≤ i ≤ ⌊log₂((n - 1))⌋} 1");
    ("loops-1-pow-unknown-bound.cu", [ "-p"; "n=1025" ], "10");
    ("loops-1-rsh.cu", [], "9");
    (* tid in the lower bound *)
    ("loops-1-tid-lb.cu", [], "1024");
    (* tid in the upper bound *)
    ("loops-1-tid-ub.cu", [ "--blockDim=32" ], "31");
    (* tid in the upper bound *)
    ( "loops-1-tid-ub-var.cu",
      [ "--blockDim=32" ],
      "Σ_{i | 0 ≤ i ≤ ((31 + n) - 1)} 1" );
    (* loop range is made uniform and variable used in index (variable is thread local) *)
    ("loops-1-tid-var-1.cu", [], "310");
    (* loop range is made uniform and variable used in index (variable is thread global) *)
    ("loops-1-tid-var-2.cu", [], "31744");
    (* a thread-local unknown variable being multiplied by tid is 31 *)
    ("unkn-tid.cu", [], "31");
    ( "loops-nested-2.cu",
      [],
      "Σ_{i | 0 ≤ i ≤ (n - 1)} Σ_{j | i ≤ j ≤ (n - 1)} 1" );
    ( "loops-nested-2-ind.cu",
      [],
      "Σ_{i | 0 ≤ i ≤ (n - 1)} Σ_{j | 0 ≤ j ≤ (n - 1)} 1" );
    ( "loops-nested-2-pow.cu",
      [],
      "Σ_{i | 0 ≤ i ≤ (n - 1)} Σ_{j | 1 ≤ j ≤ ⌊log₂((n - 1))⌋} 1" );
    ( "loops-nested-2-ind-step.cu",
      [],
      "Σ_{i | 0 ≤ i ≤ ⌊(n - 1) / step1⌋} Σ_{j | 0 ≤ j ≤ ⌊((m - 1) - (i * \
       step1)) / step2⌋} 1" );
    (*
    Condition: (threadIdx.x <= 16)
    - exact condition: none
    - exact non-uniform condition: (threadIdx.x <= 16)
    - approx non-uniform condition: none

    The exact non-uniform condition should yield 1 + 1 on both branches.
   *)
    ("ifs-1.cu", [], "2");
    (* (n < 4) uniform condition that needs to be preserved *)
    ("ifs-2.cu", [], "if ((n < 4)) then 1 else 3");
    (*
    Condition: (threadIdx.x <= 16 && n < 4)
    - exact uniform condition: (n < 4)
    - exact non-uniform condition (threadIdx.x <= 16),
    - approx non-uniform condition: none

    The uniform condition must be preserved, and cost of 1 should appear in
    both branches.
   *)
    ("ifs-3.cu", [], "if ((n < 4)) then 1 else 1");
    (*
    Condition: (threadIdx.x <= 16 && x[0] < 4)
    - exact uniform conditions: x[0] < 4 (substituted by an
      uninterpreted-function read with constant args)
    - exact non-uniform conditions: threadIdx.x <= 16

    The thread-uniform but value-unknown UF read keeps the if
    structure; both branches have cost 1, the conditional itself
    is preserved.
   *)
    ("ifs-4.cu", [], "if (($read_x < 4)) then 1 else 1");
  ]

let ua_tests =
  [
    ("ua-aligned-1.cu", [], "4");
    ("ua-aligned-2.cu", [], "5");
    ("ua-aligned-3.cu", [], "5");
    ("ua-aligned-4.cu", [], "2");
    ("ua-aligned-5.cu", [], "3");
    ("ua-broadcast.cu", [], "1");
    ("ua-unif-times-tid-1.cu", [], "min((4 * n), 32)");
    ("ua-unif-times-tid-2.cu", [], "32");
    ("ua-unif-times-tid-3.cu", [], "min((4 * n), 16)");
    ("ua-unif-times-tid-4.cu", [], "min((32 + (4 * n)), 16)");
  ]

(* These are kernels that are being documented, but are
   not currently being checked *)
let unsupported : Fpath.t list =
  [
    "2tid-racuda-1.cu";
    "2tid-seq-2.cu";
    "blockDim-tid.cu";
    "data-dep.cu";
    "empty.cu";
    "ifs-seq-3.cu";
    "loops-nested-3.cu";
    "loops-nested-4.cu";
    "loops-nested-5.cu";
    "loops-seq-2-same-var.cu";
    "loops-seq-2.cu";
    "loops-seq-3.cu";
    "loops-seq-4.cu";
    "loops-seq-5.cu";
    "loops-seq-6.cu";
    "nd-2.cu";
    "nd-3.cu";
    "racuda-index-analysis-bug.cu";
    "tid-tid.cu";
    "tidx-tidy-tidz.cu";
    "types-1.cu";
    "2tid-plus-const.cu";
    "2tid-racuda-1.cu";
    "2tid-racuda-2.cu";
    "2tid-racuda-3.cu";
    "loops-nested-2-tid.cu";
  ]
  |> List.map (fun x -> Fpath.(v "." / x))

let cost_exe : Fpath.t = Files.from_string "../../rel_cost/bin/cost.exe"

let cost ?(metric = "bc") ?(args = []) (fname : Fpath.t) : Subprocess.t =
  Subprocess.make (Fpath.to_string cost_exe)
    (args @ [ "--metric"; metric; fname |> Fpath.to_string ])

let used_files : Fpath.Set.t =
  bc_tests @ ua_tests @ per_request_tests
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

let run_test ~metric
    ((filename : string), (args : string list), (expected_output : string)) :
    unit =
  let str_args = if args = [] then "" else String.concat " " args ^ " " in
  let bullet = " - " in
  print_string (bullet ^ "faial-cost " ^ str_args ^ filename);
  Stdlib.flush_all ();
  let given = cost ~metric ~args (Fpath.v filename) |> Subprocess.run_split in
  let expected_output = expected_output |> String.trim in
  let given_output = given.stdout |> String.trim in
  if given.status = Unix.WEXITED 0 && expected_output = given_output then
    print_endline " ✔"
  else (
    print_endline " ✘";
    print_endline "----------------------- EXPECTED -----------------------";
    print_endline expected_output;
    print_endline "----------------------- GIVEN --------------------------";
    print_endline given_output;
    print_endline "----------------------- STDERR -------------------------";
    print_endline given.stderr;
    exit 1);
  Stdlib.flush_all ()

let run_all ~metric tests : unit =
  List.iter
    (fun (filename, args, expected_output) ->
      run_test ~metric (filename, "--only-cost" :: args, expected_output))
    tests;
  print_endline ""

let () =
  let open Fpath in
  print_endline "-=- Checking bank-conflicts examples -=-\n";
  print_endline "BC tests:";
  run_all ~metric:"bc" bc_tests;
  print_endline "UA tests:";
  run_all ~metric:"ua" ua_tests;
  print_endline "Skiped files:";
  unsupported
  |> List.iter (fun f ->
      if not (Files.exists f) then (
        print_endline
          (" ✘ ERROR: Missing unsupported file: " ^ Fpath.to_string f);
        exit 1)
      else print_endline (" - " ^ Fpath.to_string f));
  let missed = missed_files (v ".") in
  if not (Fpath.Set.is_empty missed) then (
    let missed =
      missed |> Fpath.Set.to_list |> List.sort Fpath.compare
      |> List.map Fpath.to_string |> String.concat " "
    in
    print_endline "";
    print_endline
      (" ✘ ERROR: The following files are not being checked: " ^ missed);
    exit (-1))
  else ()
