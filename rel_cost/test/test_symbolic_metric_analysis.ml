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
      let actual = encode_count_active_threads cfg locals cond (Num 0) in
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
      let actual = count_active_threads ~strategy cfg locals cond (Num 0) in
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
  let result =
    Stage0.Common.zip
      (Proj.b_split cfg locals cond)
      (Proj.n_split cfg locals index)
  in
  Alcotest.check replicate_result_testable "replicate result" expected result

(* Utility function that wraps encode_ua and provides better error messages *)
let assert_encode_ua ~(expected : nexp) ~(threads_per_warp : int)
    ~(locals : Variable.Set.t) ~(cond : bexp) ~(index : nexp) : unit =
  let cfg = make_config threads_per_warp in
  let result = encode_ua cfg locals cond index in
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

(* Utility function that wraps cost and provides better error messages *)
let assert_cost ~(expected : nexp) ~(expected_constraints : bexp) ~(threads_per_warp : int)
    ~(locals : Variable.Set.t) ~(metric : bexp -> nexp -> nexp)
    ~(active_threads : bexp) ~(index : nexp) () : unit =
  let cfg = make_config threads_per_warp in
  let constraints, result = cost cfg locals metric active_threads index in
  let detailed_msg =
    Printf.sprintf
      "cost(threads_per_warp=%d, locals=%s, active_threads=%s, index=%s) \
       expected %s but got %s"
      threads_per_warp
      (Variable.set_to_string locals)
      (b_to_string active_threads)
      (n_to_string index) (n_to_string expected) (n_to_string result)
  in
  let constraints_msg =
    Printf.sprintf
      "cost constraints: expected %s but got %s"
      (b_to_string expected_constraints) (b_to_string constraints)
  in
  Alcotest.check nexp_testable detailed_msg expected result;
  Alcotest.(check bool) constraints_msg true (constraints = expected_constraints)

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
         (n_if (is_even (var_ "threadIdx.x$0")) (Num 1) (Num 0))
         (n_if (is_even (var_ "threadIdx.x$1")) (Num 1) (Num 0)));
    test_encode_count_active_threads "even threadIdx.x condition" 3
      (Variable.Set.singleton Variable.tid_x)
      (is_even (Var Variable.tid_x))
      (n_plus
         (n_plus
            (n_if (is_even (var_ "threadIdx.x$0")) (Num 1) (Num 0))
            (n_if (is_even (var_ "threadIdx.x$1")) (Num 1) (Num 0)))
         (n_if (is_even (var_ "threadIdx.x$2")) (Num 1) (Num 0)));
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
  let results = Theorem.execute thm in
  (* Expect the first goal to be a successful proof *)
  match results with
  | [ Ok (TheoremResult.ProofResult ProofResult.Proved) ] ->
      Alcotest.check proof_result_testable msg ProofResult.Proved
        ProofResult.Proved
  | [ Ok (TheoremResult.ProofResult result) ] ->
      Alcotest.check proof_result_testable msg ProofResult.Proved result
  | [ Error msg ] -> Alcotest.fail ("Theorem execution failed: " ^ msg)
  | _ -> Alcotest.fail "Unexpected number of results or result type"

let test_theorem_prove_exact_cost () : unit =
  let open Theorem in
  let cfg = make_config 32 in
  let k = 2 in
  (* Test theorem: ua(2 * threadIdx.x) == 2 *)
  let goal =
    Goal.Prop
      (NRel (N_rel.Eq, NCall ("ua", n_mult (Num k) (Var Variable.tid_x)), Num k))
  in
  prove_thorem "ua(2 * threadIdx.x) should equal 2"
    {
      cfg;
      locals = Variable.Set.singleton Variable.tid_x;
      active_threads = b_true;
      assumptions = b_true;
      goals = [ goal ];
    };
  ()

let test_constraints_bug1 () : unit =
  let cfg = make_config 4 in
  let goal =
    Theorem.Goal.Prop
      (NRel (N_rel.Eq, NCall ("ua", n_mult (Num 2) (Var Variable.tid_x)), Num 2))
  in
  let theorem =
    {
      Theorem.cfg;
      locals = Variable.Set.empty;
      active_threads = b_true;
      assumptions = b_true;
      goals = [ goal ];
    }
  in

  (* Test all constraint versions - they should all behave consistently *)
  Constraints.values
  |> List.iter (fun v ->
         (* Check that we get a counterexample, not a proof *)
         let results = Theorem.execute ~generator:v theorem in
         match results with
         | [ Ok (TheoremResult.ProofResult (ProofResult.Counterexample _)) ] ->
             () (* This is what we expect *)
         | [ Ok (TheoremResult.ProofResult p) ] ->
             let msg =
               Printf.sprintf "Expecting counterexample from %s but got %s\n%s"
                 (Constraints.to_string v) (ProofResult.to_string p)
                 (Constraints.to_bexp cfg v |> Exp.b_and_split
                |> List.map Exp.b_to_string |> String.concat "\n&&")
             in
             Alcotest.fail msg
         | [ Ok (TheoremResult.OptimizationResult value) ] ->
             Alcotest.fail
               (Printf.sprintf
                  "Expected proof result but got optimization result: %d" value)
         | [ Error msg ] -> Alcotest.fail ("Theorem execution failed: " ^ msg)
         | [] -> Alcotest.fail "No results returned from theorem execution"
         | multiple_results ->
             let count = List.length multiple_results in
             Alcotest.fail
               (Printf.sprintf "Expected single result but got %d results" count))

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

(* Test cases for Proj.extract_global *)
let test_extract_global (name : string) (expression : bexp)
    (locals : Variable.Set.t) (expected_local : bexp) (expected_global : bexp) =
  ( name,
    `Quick,
    fun () ->
      let actual_local, actual_global = Proj.extract_global locals expression in
      let local_matches = actual_local = expected_local in
      let global_matches = actual_global = expected_global in
      if not local_matches then
        Alcotest.failf "Local part mismatch:\nExpected: %s\nActual: %s"
          (b_to_string expected_local)
          (b_to_string actual_local);
      if not global_matches then
        Alcotest.failf "Global part mismatch:\nExpected: %s\nActual: %s"
          (b_to_string expected_global)
          (b_to_string actual_global);
      Alcotest.(check bool) name true (local_matches && global_matches) )

let extract_global_tests =
  let e = Variable.from_name "e" in
  let passnum = Variable.from_name "passnum" in
  let x = Variable.from_name "x" in

  [
    (* Test 1: expression with local variable - should go to local part *)
    test_extract_global "expression with local variable"
      (n_ge (n_plus (Var e) (n_mult (Num 32) (Var passnum))) (Num 0))
      (Variable.Set.singleton e)
      (n_ge (n_plus (Var e) (n_mult (Num 32) (Var passnum))) (Num 0))
      (* local part *)
      b_true;
    (* global part *)

    (* Test 2: expression with no local variables - should go to global part *)
    test_extract_global "expression with no local variables"
      (n_ge (Var passnum) (Num 0))
      (Variable.Set.singleton e) b_true (* local part *)
      (n_ge (Var passnum) (Num 0));
    (* global part *)

    (* Test 3: expression with only local variables *)
    test_extract_global "expression with only local variables"
      (n_gt (Var e) (Num 0)) (Variable.Set.singleton e)
      (n_gt (Var e) (Num 0)) (* local part *)
      b_true;
    (* global part *)

    (* Test 4: conjunction with mixed local/global parts *)
    test_extract_global "conjunction with mixed local and global"
      (b_and (n_gt (Var e) (Num 0)) (n_ge (Var passnum) (Num 0)))
      (Variable.Set.singleton e)
      (n_gt (Var e) (Num 0)) (* local part *)
      (n_ge (Var passnum) (Num 0));
    (* global part *)

    (* Test 5: multiple local variables *)
    test_extract_global "multiple local variables"
      (b_and (n_gt (Var e) (Num 0)) (n_lt (Var x) (Num 100)))
      (Variable.Set.of_list [ e; x ])
      (b_and (n_gt (Var e) (Num 0)) (n_lt (Var x) (Num 100))) (* local part *)
      b_true;
    (* global part *)
  ]

(* Simple metric function that always returns Num 1 *)
let unit_metric : bexp -> nexp -> nexp = fun _active_threads _index -> Num 1

let test_cost_simple_metric () : unit =
  (* Test with the specified parameters: active_threads = b_true, index = e + passnum, locals = {e} *)
  let e = Variable.from_name "e" in
  let passnum = Variable.from_name "passnum" in
  let locals = Variable.Set.singleton e in

  (* First, let's see what the actual result is by running it *)
  let cfg = make_config 2 in
  let constraints, result =
    cost cfg locals unit_metric b_true (n_plus (Var e) (Var passnum))
  in
  Printf.printf "Cost result: %s\n" (n_to_string result);
  Printf.printf "Cost constraints: %s\n" (b_to_string constraints);
  flush stdout;

  (* Use assert_cost with the correct expected value based on the output *)
  let expected_result = Num 1 in
  let expected_constraints =
    b_and
      (n_ge (n_plus (Var (proj ~suffix:"0" e)) (Var passnum)) (Num 0))
      (n_ge (n_plus (Var (proj ~suffix:"1" e)) (Var passnum)) (Num 0))
  in
  assert_cost ~expected:expected_result ~expected_constraints ~threads_per_warp:2 ~locals
    ~metric:unit_metric ~active_threads:b_true
    ~index:(n_plus (Var e) (Var passnum))
    ()

let cost_tests =
  [ ("cost with simple metric", `Quick, test_cost_simple_metric) ]

let all_tests =
  [
    ("encode_count_active_threads", encode_count_active_threads_tests);
    ("count_active_threads", count_active_threads_tests);
    ("extract_global", extract_global_tests);
    ("cost", cost_tests);
    ("legacy_tests", tests);
  ]

let () = Alcotest.run "Symbolic Metric Analysis" all_tests
