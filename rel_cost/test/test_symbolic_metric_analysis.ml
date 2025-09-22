open Protocols
open Exp
open Rel_cost
open Rel_cost.Symbolic_metric_analysis

(* Factory function for Config objects *)
let make_config (threads_per_warp : int) : Config.t =
  Config.make ~bank_count:1 (* to simplify let's assume that there's 1 bank *)
    ~threads_per_warp
    ~block_dim:(Dim3.make ~x:threads_per_warp ())
      (* Allow enough space for all threads *)
    ~grid_dim:(Dim3.make ~x:1 ()) ()

(* Utility function to create variables *)
let var_ (name : string) : nexp = Var (Variable.from_name name)

(* Helper functions for count_active_threads tests *)
let test_encode_count_active_threads (name : string) (threads_per_warp : int)
    (locals : Variable.Set.t) (cond : bexp) (expected : nexp) =
  ( name,
    `Quick,
    fun () ->
      let cfg = make_config threads_per_warp in
      let actual = encode_count_active_threads cfg locals cond in
      if actual = expected then ()
      else
        Alcotest.failf
          "encode_count_active_threads failed for %s:\n\
           \texpected != given:\n\
           %s\n\
           %s\n\
           (condition: %s, threads_per_warp: %d)"
          name (n_to_string expected) (n_to_string actual) (b_to_string cond)
          threads_per_warp )

let test_count_active_threads (name : string)
    ?(strategy = Gen_z3.Optimizer.Strategy.Maximize) (threads_per_warp : int)
    (locals : Variable.Set.t) (cond : bexp) (expected : int option) =
  ( name,
    `Quick,
    fun () ->
      let cfg = make_config threads_per_warp in
      let actual = count_active_threads ~strategy cfg locals cond in
      let int_option_testable = Alcotest.(option int) in
      Alcotest.check int_option_testable
        (Printf.sprintf
           "count_active_threads for %s (condition: %s, threads_per_warp: %d)"
           name (b_to_string cond) threads_per_warp)
        expected actual )

(* Utility function to check if an expression is even *)
let is_even (e : nexp) : bexp = n_eq (Binary (Mod, e, Num 2)) (Num 0)

(* Test-specific Alcotest testable types *)
let proof_result_testable : ProofResult.t Alcotest.testable =
  let pp fmt result = Format.fprintf fmt "%s" (ProofResult.to_string result) in
  let equal = ( = ) in
  Alcotest.testable pp equal

let replicate_result_testable : (bexp * nexp) list Alcotest.testable =
  let pp : (bexp * nexp) list Fmt.t =
   fun fmt result ->
    let pp_pair (b, n) =
      Printf.sprintf "(%s, %s)" (b_to_string b) (n_to_string n)
    in
    Format.fprintf fmt "[%s]" (String.concat "; " (List.map pp_pair result))
  in
  let equal : (bexp * nexp) list -> (bexp * nexp) list -> bool = ( = ) in
  Alcotest.testable pp equal

let nexp_testable : nexp Alcotest.testable =
  let pp : nexp Fmt.t = fun fmt n -> Format.fprintf fmt "%s" (n_to_string n) in
  let equal : nexp -> nexp -> bool = ( = ) in
  Alcotest.testable pp equal

let solver_result_testable : Gen_z3.Solver.t Alcotest.testable =
  let pp : Gen_z3.Solver.t Fmt.t =
   fun fmt -> function
     | Gen_z3.Solver.Sat _ -> Format.fprintf fmt "Sat"
     | Gen_z3.Solver.Unsat -> Format.fprintf fmt "Unsat"
     | Gen_z3.Solver.Unknown msg -> Format.fprintf fmt "Unknown(%s)" msg
  in
  let equal : Gen_z3.Solver.t -> Gen_z3.Solver.t -> bool = ( = ) in
  Alcotest.testable pp equal

(* Utility function that wraps replicate and provides better error messages *)
let assert_replicate ~(expected : (bexp * nexp) list) ~(threads_per_warp : int)
    ~(locals : Variable.Set.t) ~(cond : bexp) ~(index : nexp) : unit =
  let cfg = make_config threads_per_warp in
  let result = Proj.run_pair cfg locals cond index in
  Alcotest.check replicate_result_testable "replicate result" expected result

(* Utility function that wraps encode_ua and provides better error messages *)
let assert_encode_ua ~(expected : nexp) ~(threads_per_warp : int)
    ~(locals : Variable.Set.t) ~(cond : bexp) ~(index : nexp) : unit =
  let cfg = make_config threads_per_warp in
  let result = encode_ua cfg locals index cond in
  let detailed_msg =
    Printf.sprintf
      "encode_ua failed\n\
      \  index: %s\n\
      \  condition: %s\n\
      \  threads_per_warp: %d\n\
      \  locals: %s"
      (n_to_string index) (b_to_string cond) threads_per_warp
      (Variable.set_to_string locals)
  in
  Alcotest.check nexp_testable detailed_msg expected result

(* Utility function that wraps ua and provides better error messages *)
let assert_ua
    ?(strategy : Gen_z3.Optimizer.Strategy.t =
      Gen_z3.Optimizer.Strategy.Maximize) ~(expected : int option)
    ~(threads_per_warp : int) ~(locals : Variable.Set.t) ~(cond : bexp)
    ~(index : nexp) () : unit =
  let cfg = make_config threads_per_warp in
  let index_with_segments =
    n_mult index (Num (Config.memory_segments_bits cfg))
  in
  let result = ua ~strategy cfg locals cond index_with_segments in
  Alcotest.check Alcotest.(option int) "ua result" expected result

(* Test cases for encode_count_active_threads *)
let encode_count_active_threads_tests =
  [
    test_encode_count_active_threads "always true, 2 threads" 2
      Variable.Set.empty b_true (Num 2);
    test_encode_count_active_threads "always true, 4 threads" 4
      Variable.Set.empty b_true (Num 4);
    test_encode_count_active_threads "always false, 2 threads" 2
      Variable.Set.empty b_false (Num 0);
    test_encode_count_active_threads "with local variable x" 2
      (Variable.Set.singleton (Variable.from_name "x"))
      b_true (Num 2);
    test_encode_count_active_threads "even threadIdx.x condition" 2
      (Variable.Set.singleton Variable.tid_x)
      (is_even (Var Variable.tid_x))
      (n_plus
         (n_if (is_even (var_ "threadIdx.x$1")) (Num 1) (Num 0))
         (n_if (is_even (var_ "threadIdx.x$0")) (Num 1) (Num 0)));
    test_encode_count_active_threads "even threadIdx.x condition" 3
      (Variable.Set.singleton Variable.tid_x)
      (is_even (Var Variable.tid_x))
      (n_plus
         (n_if (is_even (var_ "threadIdx.x$2")) (Num 1) (Num 0))
         (n_plus
            (n_if (is_even (var_ "threadIdx.x$1")) (Num 1) (Num 0))
            (n_if (is_even (var_ "threadIdx.x$0")) (Num 1) (Num 0))));
  ]

(* Test cases for count_active_threads *)
let count_active_threads_tests =
  [
    test_count_active_threads "always true, 2 threads" 2 Variable.Set.empty
      b_true (Some 2);
    test_count_active_threads "always true, 4 threads" 4 Variable.Set.empty
      b_true (Some 4);
    test_count_active_threads "always false, 2 threads" 2 Variable.Set.empty
      b_false (Some 0);
    test_count_active_threads "with local variable x" 2
      (Variable.Set.singleton (Variable.from_name "x"))
      b_true (Some 2);
    test_count_active_threads "even threadIdx.x, 2 threads" 2
      (Variable.Set.singleton Variable.tid_x)
      (is_even (Var Variable.tid_x))
      (Some 1);
    test_count_active_threads "even threadIdx.x, 4 threads" 4
      (Variable.Set.singleton Variable.tid_x)
      (is_even (Var Variable.tid_x))
      (Some 2);
    test_count_active_threads "even threadIdx.x, 10 threads" 8
      (Variable.Set.singleton Variable.tid_x)
      (is_even (Var Variable.tid_x))
      (Some 4);
    test_count_active_threads "maximize strategy"
      ~strategy:Gen_z3.Optimizer.Strategy.Maximize 2 Variable.Set.empty b_true
      (Some 2);
    test_count_active_threads "minimize strategy"
      ~strategy:Gen_z3.Optimizer.Strategy.Minimize 2 Variable.Set.empty b_true
      (Some 2);
  ]

let test_replicate_empty_locals () : unit =
  (* Test with empty locals, cfg with 2 threads_per_warp *)
  assert_replicate
    ~expected:[ (b_true, Num 0); (b_true, Num 0) ]
    ~threads_per_warp:2 ~locals:Variable.Set.empty ~cond:b_true ~index:(Num 0)

let test_replicate_with_local_variable () : unit =
  (* Test with local variable x *)
  let x = Variable.from_name "x" in
  let locals = Variable.Set.singleton x in
  assert_replicate
    ~expected:[ (b_true, var_ "x$0"); (b_true, var_ "x$1") ]
    ~threads_per_warp:2 ~locals ~cond:b_true ~index:(var_ "x")

let test_encode_ua_empty_locals () : unit =
  (* Test encode_ua with empty locals, 2 threads accessing same index *)
  assert_encode_ua ~threads_per_warp:2 ~locals:Variable.Set.empty ~cond:b_true
    ~index:(Num 0) ~expected:(Num 1)

let test_encode_ua_with_local_variable () : unit =
  (* Test encode_ua with local variable x *)
  let x = Variable.from_name "x" in
  let locals = Variable.Set.singleton x in
  let expected =
    n_plus (Num 1)
      (NIf
         ( n_neq (n_div (var_ "x$1") (Num 32)) (n_div (var_ "x$0") (Num 32)),
           Num 1,
           Num 0 ))
  in
  assert_encode_ua ~expected ~threads_per_warp:2 ~locals ~cond:b_true
    ~index:(var_ "x")

let test_ua_empty_locals () : unit =
  (* Test ua with empty locals, 2 threads accessing same index *)
  assert_ua ~expected:(Some 1) ~threads_per_warp:2 ~locals:Variable.Set.empty
    ~cond:b_true ~index:(Num 0) ()

let test_ua_with_local_variable () : unit =
  (* Test ua with local variable x *)
  let x = Variable.from_name "x" in
  let locals = Variable.Set.singleton x in
  assert_ua ~expected:(Some 2) ~threads_per_warp:2 ~locals ~cond:b_true
    ~index:(var_ "x") ()

let test_ua_threadIdx_x () : unit =
  (* Test ua with both minimize and maximize strategies on threadIdx.x *)
  (* assert_ua automatically multiplies by memory segment size *)
  (* Expect 2 uncoalesced accesses since threads hit different memory segments *)

  (* Test minimize strategy *)
  assert_ua ~strategy:Gen_z3.Optimizer.Strategy.Minimize ~expected:(Some 2)
    ~threads_per_warp:2
    ~locals:(Variable.Set.singleton Variable.tid_x)
    ~cond:b_true ~index:(Var Variable.tid_x) ();

  (* Test maximize strategy *)
  assert_ua ~strategy:Gen_z3.Optimizer.Strategy.Maximize ~expected:(Some 2)
    ~threads_per_warp:2
    ~locals:(Variable.Set.singleton Variable.tid_x)
    ~cond:b_true ~index:(Var Variable.tid_x) ();

  (* Test with condition tidx % 2 == 0 (only even thread IDs) *)
  assert_ua ~strategy:Gen_z3.Optimizer.Strategy.Maximize ~expected:(Some 2)
    ~threads_per_warp:4 (* Only threadIdx.x is a thread-local variable *)
    ~locals:(Variable.Set.singleton Variable.tid_x)
    ~cond:(is_even (Var Variable.tid_x))
    ~index:(Var Variable.tid_x) ()

let test_warp_constraints_enforces_bounds_and_uniqueness () : unit =
  (* Test that warp_constraints prevents threads from having same coordinates within bounds *)
  Constraints.values
  |> List.iter (fun gen ->
         let cfg = make_config 2 in
         let c = Constraints.to_bexp cfg gen in
         (* Is it possible for 2 tids to be equal? *)
         let contradiction =
           b_and c (n_eq (var_ "threadIdx.x$0") (var_ "threadIdx.x$1"))
         in
         let open Gen_z3.IntGen in
         let result = solve contradiction in
         let test_msg =
           Printf.sprintf
             "warp_constraints should make threadIdx.x$0 = threadIdx.x$1 \
              unsatisfiable when considering block bounds (%s)"
             (Constraints.to_string gen)
         in
         Alcotest.check solver_result_testable test_msg Gen_z3.Solver.Unsat
           result)

let test_cross_warp_unsoundness_test () : unit =
  (* Test to expose unsoundness: threads from different warps should not be allowed *)
  Constraints.values
  |> List.iter (fun gen ->
         let cfg =
           Config.make ~threads_per_warp:32
             ~block_dim:(Dim3.make ~x:64 ()) (* 2 warps: 0-31 and 32-63 *)
             ~grid_dim:(Dim3.make ~x:1 ()) ()
         in
         let c = Constraints.to_bexp cfg gen in
         (* Try to assign threads from different warps *)
         let cross_warp =
           b_and_ex
             [
               c;
               n_eq (var_ "threadIdx.x$0") (Num 0);
               (* thread 0 = warp 0 *)
               n_eq (var_ "threadIdx.x$1") (Num 32);
               (* thread 32 = warp 1 *)
               n_eq (var_ "threadIdx.y$0") (Num 0);
               n_eq (var_ "threadIdx.y$1") (Num 0);
               n_eq (var_ "threadIdx.z$0") (Num 0);
               n_eq (var_ "threadIdx.z$1") (Num 0);
             ]
         in
         let open Gen_z3.IntGen in
         let result = solve cross_warp in
         let test_msg =
           Printf.sprintf
             "EXPECTED TO FAIL: Current constraints allow threads from \
              different warps (this exposes unsoundness) (%s)"
             (Constraints.to_string gen)
         in
         (* This test SHOULD fail (return Sat) with current constraints, exposing the bug *)
         Alcotest.check solver_result_testable test_msg Gen_z3.Solver.Unsat
           result)

let prove_thorem (msg : string) (thm : Theorem.t) : unit =
  print_endline (Theorem.to_string thm);
  flush stdout;
  Alcotest.check proof_result_testable msg ProofResult.Proved
    (Theorem.prove thm)

let test_theorem_prove_exact_cost () : unit =
  let open Theorem in
  let cfg = make_config 32 in
  let k = 2 in
  (* Test theorem: 2 * threadIdx.x should have exact cost 2 *)
  prove_thorem "2 * threadIdx.x should have exact cost 2"
    {
      cfg;
      locals = Variable.Set.singleton Variable.tid_x;
      local_context = b_true;
      global_context = b_true;
      index = n_mult (Num k) (Var Variable.tid_x);
      rel = N_rel.Eq;
      expected_cost = Num k;
    };
  ()

let test_constraints_bug1 () : unit =
  let cfg = make_config 4 in
  let theorem =
    {
      Theorem.cfg;
      locals = Variable.Set.empty;
      local_context = b_true;
      global_context = b_true;
      index = n_mult (Num 2) (Var Variable.tid_x);
      rel = N_rel.Eq;
      expected_cost = Num 2;
    }
  in

  (* Test all constraint versions - they should all behave consistently *)
  Constraints.values
  |> List.iter (fun v ->
         (* Check that we get a counterexample, not a proof *)
         match Theorem.prove ~generator:v theorem with
         | ProofResult.Counterexample _ -> () (* This is what we expect *)
         | p ->
             let msg =
               Printf.sprintf "Expecting counterexample from %s but got %s\n%s"
                 (Constraints.to_string v) (ProofResult.to_string p)
                 (Constraints.to_bexp cfg v |> Exp.b_and_split
                |> List.map Exp.b_to_string |> String.concat "\n&&")
             in
             Alcotest.fail msg)

let tests : unit Alcotest.test_case list =
  [
    ("replicate_empty_locals", `Quick, test_replicate_empty_locals);
    ("replicate_with_local_variable", `Quick, test_replicate_with_local_variable);
    ("encode_ua_empty_locals", `Quick, test_encode_ua_empty_locals);
    ("encode_ua_with_local_variable", `Quick, test_encode_ua_with_local_variable);
    ("ua_empty_locals", `Quick, test_ua_empty_locals);
    ("ua_with_local_variable", `Quick, test_ua_with_local_variable);
    ("ua_threadIdx.x", `Quick, test_ua_threadIdx_x);
    ( "warp_constraints_enforces_bounds_and_uniqueness",
      `Quick,
      test_warp_constraints_enforces_bounds_and_uniqueness );
    ("cross_warp_unsoundness_test", `Quick, test_cross_warp_unsoundness_test);
    ("theorem_prove_exact_cost", `Quick, test_theorem_prove_exact_cost);
    ("constraints_bug1", `Quick, test_constraints_bug1);
  ]

let all_tests =
  [
    ("encode_count_active_threads", encode_count_active_threads_tests);
    ("count_active_threads", count_active_threads_tests);
    ("legacy_tests", tests);
  ]

let () = Alcotest.run "Symbolic Metric Analysis" all_tests
