open Barrier_div
open Stage0
open Protocols
open Exp

(* shared helpers *)

let v (name : string) : Variable.t = Variable.from_name name

let mk_sync ?(array = "__syncthreads") ?(index = []) ?(count = None) () : Sync.t
    =
  {
    Sync.mode = Sync.Mode.Sync;
    array = v array;
    index;
    count;
    loc = Some Location.empty;
  }

let thread (path_cond : bexp) : Thread.t =
  { Thread.path_cond; proto = Code.Skip }

let evt sync (rest : Thread.t) : Thread.sync_event = { sync; rest }

let cfg : Rel_cost.Config.t =
  let block_dim = Dim3.make ~x:32 () in
  let grid_dim = Dim3.one in
  Rel_cost.Config.make ~block_dim ~grid_dim ()

(* id_eq *)

let test_id_eq_same () =
  let s = mk_sync () in
  Alcotest.(check bool) "self equality" true (Phase.id_eq s s)

let test_id_eq_distinct_arrays () =
  let s1 = mk_sync ~array:"b1" () in
  let s2 = mk_sync ~array:"b2" () in
  Alcotest.(check bool) "distinct arrays" false (Phase.id_eq s1 s2)

let test_id_eq_distinct_indices () =
  let s1 = mk_sync ~index:[ Num 0 ] () in
  let s2 = mk_sync ~index:[ Num 1 ] () in
  Alcotest.(check bool) "distinct indices" false (Phase.id_eq s1 s2)

(* absorb *)

let test_absorb_arrive_grows_cohort () =
  let s = mk_sync () in
  let p = Phase.of_arrive cfg (evt s (thread (Bool true))) in
  let p' = Phase.absorb_arrive (Bool false) p in
  (* arrive_cohort is now (true ∨ false) = true; SMT would simplify, but
     equality is on syntactic bexp *)
  Alcotest.(check bool) "absorb produces b_or" true
    (p'.arrive_cohort = Exp.b_or (Bool true) (Bool false))

let test_absorb_wait_adds_parked () =
  let s = mk_sync () in
  let p = Phase.of_arrive cfg (evt s (thread (Bool true))) in
  let new_thread = thread (Bool false) in
  let p' = Phase.absorb_wait new_thread p in
  Alcotest.(check int) "parked grows by 1" 1 (List.length p'.parked);
  Alcotest.(check bool) "wait_cohort is b_or" true
    (p'.wait_cohort = Exp.b_or (Bool false) (Bool false))

let test_absorb_sync_does_both () =
  let s = mk_sync () in
  (* Start with a non-trivial path condition so b_or doesn't simplify to true *)
  let initial_pc = n_lt (Var Variable.tid_x) (Num 16) in
  let p = Phase.of_arrive cfg (evt s (thread initial_pc)) in
  let other_pc = n_lt (Var (v "tid")) (Num 10) in
  let new_thread = thread other_pc in
  let p' = Phase.absorb_sync new_thread p in
  Alcotest.(check int) "parked grows" 1 (List.length p'.parked);
  Alcotest.(check bool) "arrive_cohort grew" true
    (p'.arrive_cohort <> p.arrive_cohort);
  Alcotest.(check bool) "wait_cohort grew" true
    (p'.wait_cohort <> p.wait_cohort)

(* matches *)

let test_matches_same () =
  let s = mk_sync () in
  let p = Phase.of_sync cfg (evt s (thread (Bool true))) in
  Alcotest.(check bool) "matches own sync" true (Phase.matches s p)

let test_matches_different () =
  let s1 = mk_sync ~array:"b1" () in
  let s2 = mk_sync ~array:"b2" () in
  let p = Phase.of_sync cfg (evt s1 (thread (Bool true))) in
  Alcotest.(check bool) "does not match other" false (Phase.matches s2 p)

(* no_late_activity *)

let test_no_late_activity_clean () =
  let s = mk_sync () in
  let p = Phase.of_sync cfg (evt s (thread (Bool true))) in
  let other_threads = [ thread (Bool true) ] in
  Alcotest.(check bool) "no other thread references" true
    (Phase.no_late_activity other_threads p)

let test_no_late_activity_pending_arrival () =
  let s = mk_sync () in
  let p = Phase.of_sync cfg (evt s (thread (Bool true))) in
  let pending : Thread.t = { path_cond = Bool true; proto = Code.Sync s } in
  Alcotest.(check bool) "pending sync blocks fire" false
    (Phase.no_late_activity [ pending ] p)

(* no_rival_count *)

let test_no_rival_count_no_others () =
  let s = mk_sync () in
  let p = Phase.of_sync cfg (evt s (thread (Bool true))) in
  Alcotest.(check bool) "no rivals when alone" true
    (Phase.no_rival_count [ p ] p)

let test_no_rival_count_same_count_ok () =
  let s = mk_sync ~count:(Some (Num 16)) () in
  let p1 = Phase.of_sync cfg (evt s (thread (Bool true))) in
  let p2 = Phase.of_sync cfg (evt s (thread (Bool false))) in
  Alcotest.(check bool) "same count is not a rival" true
    (Phase.no_rival_count [ p1; p2 ] p1)

let test_no_rival_count_different_count_blocks () =
  let s_16 = mk_sync ~count:(Some (Num 16)) () in
  let s_32 = mk_sync ~count:(Some (Num 32)) () in
  let p_small = Phase.of_sync cfg (evt s_16 (thread (Bool true))) in
  let p_large = Phase.of_sync cfg (evt s_32 (thread (Bool false))) in
  Alcotest.(check bool) "rival with different count blocks" false
    (Phase.no_rival_count [ p_small; p_large ] p_small)

(* SMT-backed status: small cases that should resolve quickly *)

let test_is_finished_uniform_sync () =
  (* Cohort = true (all 32 threads), count = 32 → finished *)
  let s = mk_sync () in
  let p = Phase.of_sync cfg (evt s (thread (Bool true))) in
  let locals = Variable.tid_set in
  Alcotest.(check bool) "true cohort = blockDim is finished" true
    (Phase.is_finished cfg locals p)

let test_is_finished_partial () =
  (* Cohort = (tid < 17), count = 32 → not finished (only 17 threads) *)
  let s = mk_sync () in
  let pc = n_lt (Var Variable.tid_x) (Num 17) in
  let p = Phase.of_sync cfg (evt s (thread pc)) in
  let locals = Variable.tid_set in
  Alcotest.(check bool) "tid<17 cohort with count=32 is not finished" false
    (Phase.is_finished cfg locals p)

(* Direct check on Thread_count: under a 32-thread warp, the cohort
   {tid : tid_x < 17} should have exactly 17 threads (min = max = 17).
   If the SMT layer doesn't constrain tid_x to its valid range, min
   may collapse to 0 and max may inflate beyond 17 — both are
   diagnostic-corrupting and worth catching as regressions. *)
let test_cohort_size_tid_lt_17 () =
  let pc = n_lt (Var Variable.tid_x) (Num 17) in
  let locals = Variable.tid_set in
  Alcotest.(check (option int)) "max(tid_x < 17) = 17" (Some 17)
    (Thread_count.max_count cfg locals pc);
  Alcotest.(check (option int)) "min(tid_x < 17) = 17" (Some 17)
    (Thread_count.min_count cfg locals pc)

(* Regression: when [block_dim] exceeds [threads_per_warp], the
   symbolic-metric layer samples only one warp at a time. For a
   tid-bound predicate like [tid_x < 17], the per-warp cohort then
   varies — full warp 0 has 17 matching threads, warp 1+ has 0 — and
   min collapses to 0. For block-wide barrier reasoning, the binary
   must align [threads_per_warp] to the full block size; this test
   exercises that with [threads_per_warp = block_dim.x]. *)
let test_cohort_multi_warp_block () =
  let block_dim = Dim3.make ~x:64 () in
  let cfg' =
    Rel_cost.Config.make ~threads_per_warp:64 ~block_dim
      ~grid_dim:Dim3.one ()
  in
  let pc = n_lt (Var Variable.tid_x) (Num 17) in
  let locals = Variable.tid_set in
  Alcotest.(check (option int)) "max on 64-thread block = 17" (Some 17)
    (Thread_count.max_count cfg' locals pc);
  Alcotest.(check (option int)) "min on 64-thread block = 17" (Some 17)
    (Thread_count.min_count cfg' locals pc)

(* test groups *)

let id_tests =
  [
    ("id_eq self", `Quick, test_id_eq_same);
    ("id_eq distinct arrays", `Quick, test_id_eq_distinct_arrays);
    ("id_eq distinct indices", `Quick, test_id_eq_distinct_indices);
  ]

let absorb_tests =
  [
    ("absorb_arrive grows cohort", `Quick, test_absorb_arrive_grows_cohort);
    ("absorb_wait adds parked", `Quick, test_absorb_wait_adds_parked);
    ("absorb_sync does both", `Quick, test_absorb_sync_does_both);
  ]

let matches_tests =
  [
    ("matches same", `Quick, test_matches_same);
    ("matches different", `Quick, test_matches_different);
  ]

let engulfment_tests =
  [
    ("no_late_activity clean", `Quick, test_no_late_activity_clean);
    ( "no_late_activity blocks on pending arrival",
      `Quick,
      test_no_late_activity_pending_arrival );
  ]

let rival_tests =
  [
    ("no_rival_count alone", `Quick, test_no_rival_count_no_others);
    ("no_rival_count same count", `Quick, test_no_rival_count_same_count_ok);
    ( "no_rival_count different count blocks",
      `Quick,
      test_no_rival_count_different_count_blocks );
  ]

let smt_tests =
  [
    ("is_finished uniform sync", `Slow, test_is_finished_uniform_sync);
    ("is_finished partial cohort", `Slow, test_is_finished_partial);
    ("cohort size tid_x < 17", `Slow, test_cohort_size_tid_lt_17);
    ("cohort multi-warp block", `Slow, test_cohort_multi_warp_block);
  ]

let () =
  Alcotest.run "phase"
    [
      ("id", id_tests);
      ("absorb", absorb_tests);
      ("matches", matches_tests);
      ("engulfment", engulfment_tests);
      ("rivals", rival_tests);
      ("smt", smt_tests);
    ]
