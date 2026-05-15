(* Tests for [Cegar.check_three_tier].

   The three-tier predicate fires its checks in the order
   [tier1 → tier2 → drf] and short-circuits on the first [false].
   These tests pin the ordering using counter refs as side effects
   inside each closure, asserting which checks were invoked. *)

open Drf_genie

(* Run the helper with three closure counters and return the bool
   plus a triple [(t1_calls, t2_calls, drf_calls)]. *)
let run_with_counters
    (t1_result : bool) (t2_result : bool) (drf_result : bool)
    : bool * (int * int * int) =
  let t1 = ref 0 in
  let t2 = ref 0 in
  let drf = ref 0 in
  let result =
    Cegar.check_three_tier
      ~tier1:(fun () -> incr t1; t1_result)
      ~tier2:(fun () -> incr t2; t2_result)
      ~drf:(fun () -> incr drf; drf_result)
  in
  (result, (!t1, !t2, !drf))

(* Test 1. Tier 1 short-circuit: [tier1 = false] must skip both
   Tier 2 and DRF. *)
let test_tier1_short_circuits () =
  let result, (t1, t2, drf) = run_with_counters false true true in
  Alcotest.(check bool) "result is false" false result;
  Alcotest.(check int) "tier1 called once" 1 t1;
  Alcotest.(check int) "tier2 not called" 0 t2;
  Alcotest.(check int) "drf not called" 0 drf

(* Test 2. Tier 2 short-circuit: [tier1 = true, tier2 = false]
   must skip DRF. *)
let test_tier2_short_circuits () =
  let result, (t1, t2, drf) = run_with_counters true false true in
  Alcotest.(check bool) "result is false" false result;
  Alcotest.(check int) "tier1 called once" 1 t1;
  Alcotest.(check int) "tier2 called once" 1 t2;
  Alcotest.(check int) "drf not called" 0 drf

(* Test 3. All-pass: every tier fires exactly once and the result
   is [true]. *)
let test_all_pass () =
  let result, (t1, t2, drf) = run_with_counters true true true in
  Alcotest.(check bool) "result is true" true result;
  Alcotest.(check int) "tier1 called once" 1 t1;
  Alcotest.(check int) "tier2 called once" 1 t2;
  Alcotest.(check int) "drf called once" 1 drf

(* Test 4. DRF rejection: T1 and T2 pass, DRF returns false. All
   three tiers must have fired. *)
let test_drf_rejection () =
  let result, (t1, t2, drf) = run_with_counters true true false in
  Alcotest.(check bool) "result is false" false result;
  Alcotest.(check int) "tier1 called once" 1 t1;
  Alcotest.(check int) "tier2 called once" 1 t2;
  Alcotest.(check int) "drf called once" 1 drf

(* Test 5. Ordering: a closure that raises if invoked detects
   out-of-order calls. Tier 2's closure raises; with
   [tier1 = false] the helper must not invoke it. *)
let test_ordering_tier1_before_tier2 () =
  let raised = ref false in
  let result =
    Cegar.check_three_tier
      ~tier1:(fun () -> false)
      ~tier2:(fun () -> raised := true; true)
      ~drf:(fun () -> raised := true; true)
  in
  Alcotest.(check bool) "result is false" false result;
  Alcotest.(check bool) "neither tier2 nor drf invoked" false !raised

(* Test 6. Ordering: with [tier1 = true, tier2 = false] DRF must
   not be invoked. *)
let test_ordering_tier2_before_drf () =
  let drf_invoked = ref false in
  let result =
    Cegar.check_three_tier
      ~tier1:(fun () -> true)
      ~tier2:(fun () -> false)
      ~drf:(fun () -> drf_invoked := true; true)
  in
  Alcotest.(check bool) "result is false" false result;
  Alcotest.(check bool) "drf not invoked" false !drf_invoked

let tests = [
  ("1. tier1 = false short-circuits tier2 and drf",
    `Quick, test_tier1_short_circuits);
  ("2. tier2 = false short-circuits drf",
    `Quick, test_tier2_short_circuits);
  ("3. all pass: each tier fires exactly once",
    `Quick, test_all_pass);
  ("4. drf = false: all three tiers fire",
    `Quick, test_drf_rejection);
  ("5. ordering: tier1 fires before tier2",
    `Quick, test_ordering_tier1_before_tier2);
  ("6. ordering: tier2 fires before drf",
    `Quick, test_ordering_tier2_before_drf);
]

let () =
  Alcotest.run "drf_genie/cegar" [
    ("Cegar.check_three_tier", tests);
  ]
