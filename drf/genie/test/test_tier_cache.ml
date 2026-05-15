(* Tests for [Tier_cache], the per-[compute_verdict] Φ-keyed cache
   used to skip Tier 1 / Tier 2 gate re-evaluation across the
   abductive loop's accept → shrink → re-check → weaken → re-shrink
   sequences.

   The cache logic is exercised independently of Z3: a counter ref
   stands in for the underlying gate predicate. The properties
   tested are
   1. identical assumes hit the cache on the second lookup;
   2. assumes that differ in any clause produce distinct keys (miss);
   3. assumes that share the same clause set but in a different
      order share a key (normalisation).
*)

open Protocols
open Drf_genie

let var (n : string) : Exp.nexp = Exp.Var (Variable.from_name n)
let num (n : int) : Exp.nexp = Exp.Num n
let eq_b (a : Exp.nexp) (b : Exp.nexp) : Exp.bexp = Exp.NRel (Eq, a, b)

(* Each test wraps a counter around a no-op predicate so we can
   assert how many times the underlying compute fired. *)
let counted_compute () : (unit -> bool) * (unit -> int) =
  let n = ref 0 in
  let compute () = incr n; true in
  (compute, fun () -> !n)

let test_hit_on_identical_app () =
  let cache = Tier_cache.create () in
  let assumes : (string * Exp.bexp list) list =
    [ ("k", [ eq_b (var "x") (num 0) ]) ]
  in
  let compute, get_count = counted_compute () in
  let v1 = Tier_cache.lookup_or_compute cache assumes compute in
  let v2 = Tier_cache.lookup_or_compute cache assumes compute in
  Alcotest.(check bool) "first call returns compute result" true v1;
  Alcotest.(check bool) "second call returns cached result" true v2;
  Alcotest.(check int) "compute ran once" 1 (get_count ());
  Alcotest.(check int) "cache holds one entry" 1 (Tier_cache.size cache)

let test_miss_on_different_assumes () =
  let cache = Tier_cache.create () in
  let a1 : (string * Exp.bexp list) list =
    [ ("k", [ eq_b (var "x") (num 0) ]) ]
  in
  let a2 : (string * Exp.bexp list) list =
    [ ("k", [ eq_b (var "x") (num 1) ]) ]
  in
  let compute, get_count = counted_compute () in
  let _ = Tier_cache.lookup_or_compute cache a1 compute in
  let _ = Tier_cache.lookup_or_compute cache a2 compute in
  Alcotest.(check int) "both calls missed" 2 (get_count ());
  Alcotest.(check int) "cache holds two entries" 2 (Tier_cache.size cache)

let test_normalisation_clause_order () =
  let cache = Tier_cache.create () in
  let c1 = eq_b (var "x") (num 0) in
  let c2 = eq_b (var "y") (num 1) in
  let a_forward : (string * Exp.bexp list) list = [ ("k", [ c1; c2 ]) ] in
  let a_reversed : (string * Exp.bexp list) list = [ ("k", [ c2; c1 ]) ] in
  let compute, get_count = counted_compute () in
  let _ = Tier_cache.lookup_or_compute cache a_forward compute in
  let _ = Tier_cache.lookup_or_compute cache a_reversed compute in
  Alcotest.(check int) "compute ran once despite reversed order"
    1 (get_count ());
  Alcotest.(check int) "cache holds one entry" 1 (Tier_cache.size cache)

let test_normalisation_kernel_order () =
  let cache = Tier_cache.create () in
  let assumes_ab : (string * Exp.bexp list) list =
    [ ("a", [ eq_b (var "x") (num 0) ]);
      ("b", [ eq_b (var "y") (num 1) ]) ]
  in
  let assumes_ba : (string * Exp.bexp list) list =
    [ ("b", [ eq_b (var "y") (num 1) ]);
      ("a", [ eq_b (var "x") (num 0) ]) ]
  in
  let compute, get_count = counted_compute () in
  let _ = Tier_cache.lookup_or_compute cache assumes_ab compute in
  let _ = Tier_cache.lookup_or_compute cache assumes_ba compute in
  Alcotest.(check int)
    "compute ran once across kernel-order permutation"
    1 (get_count ())

let test_find_opt_and_add () =
  let cache = Tier_cache.create () in
  let assumes : (string * Exp.bexp list) list =
    [ ("k", [ eq_b (var "x") (num 0) ]) ]
  in
  Alcotest.(check (option bool)) "miss before add"
    None (Tier_cache.find_opt cache assumes);
  Tier_cache.add cache assumes false;
  Alcotest.(check (option bool)) "hit after add"
    (Some false) (Tier_cache.find_opt cache assumes);
  Tier_cache.add cache assumes true;
  Alcotest.(check (option bool)) "add replaces"
    (Some true) (Tier_cache.find_opt cache assumes)

let test_empty_assumes_share_key () =
  let cache = Tier_cache.create () in
  let empty_a : (string * Exp.bexp list) list = [ ("k", []) ] in
  let empty_b : (string * Exp.bexp list) list = [ ("k", []) ] in
  let compute, get_count = counted_compute () in
  let _ = Tier_cache.lookup_or_compute cache empty_a compute in
  let _ = Tier_cache.lookup_or_compute cache empty_b compute in
  Alcotest.(check int) "empty-assumes hits on second call" 1 (get_count ())

let tier_cache_tests = [
  ("hit on identical app",            `Quick, test_hit_on_identical_app);
  ("miss on different assumes",       `Quick, test_miss_on_different_assumes);
  ("normalises clause order",         `Quick, test_normalisation_clause_order);
  ("normalises per-kernel order",     `Quick, test_normalisation_kernel_order);
  ("find_opt / add roundtrip",        `Quick, test_find_opt_and_add);
  ("empty assumes share a key",       `Quick, test_empty_assumes_share_key);
]

let () =
  Alcotest.run "drf_genie/tier_cache" [
    ("Tier_cache", tier_cache_tests);
  ]
