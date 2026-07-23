open Stage0
open Protocols
open Exp
open Named_barrier_div

let v (name : string) : Variable.t = Variable.from_name name

let access : Code.t =
  Code.Access (Access.read (v "a") [ Num 0 ])

let sync_at ?(mode = Sync.Mode.ArriveAndWait) (label : string) : Sync.t =
  { Sync.mode; id = Var (v label); participants = None;
    loc = Some Location.empty }

let mk_sync ?mode (label : string) : Code.t = Code.Sync (sync_at ?mode label)

let empty_sigma : Sigma.t = Sigma.empty

(* Build a parked task with [residual = sync label; rest]. *)
let parked_task (label : string) (rest : Code.t) : Task.t =
  Task.make ~sigma:empty_sigma (Code.Seq (mk_sync label, rest))

(* Barriers.of_code and is_blocking *)

let test_barriers_of_skip () =
  Alcotest.(check int) "skip has no barriers" 0
    (Barriers.IdSet.cardinal (Barriers.of_code Code.Skip))

let test_barriers_of_blocking_sync () =
  let ids = Barriers.of_code (mk_sync "s1") in
  Alcotest.(check int) "blocking sync contributes one id" 1
    (Barriers.IdSet.cardinal ids)

let test_barriers_of_arrive_only () =
  let ids = Barriers.of_code (mk_sync ~mode:Sync.Mode.Arrive "s1") in
  Alcotest.(check int) "Arrive does not contribute" 0
    (Barriers.IdSet.cardinal ids)

let test_barriers_seq_union () =
  let proto = Code.Seq (mk_sync "s1", mk_sync "s2") in
  Alcotest.(check int) "two distinct barriers" 2
    (Barriers.IdSet.cardinal (Barriers.of_code proto))

(* group_at *)

let test_group_at_partitions () =
  let t1 = parked_task "s1" Code.Skip in
  let t2 = parked_task "s2" Code.Skip in
  let t3 = parked_task "s1" access in
  let id_s1 = Barriers.Id.of_sync (sync_at "s1") in
  let group, other = Tier2.group_at id_s1 [ t1; t2; t3 ] in
  Alcotest.(check int) "group has two tasks" 2 (List.length group);
  Alcotest.(check int) "other has one task"  1 (List.length other)

(* check / advance_n *)

let test_check_empty () =
  Alcotest.(check int) "empty parked → no diagnostics" 0
    (List.length (Tier2.check empty_sigma []))

let test_check_single_cohort () =
  (* Two tasks both stopped at s1 with no residual: should advance and
     drain with no diagnostics (bd stub returns Pass). *)
  let t1 = parked_task "s1" Code.Skip in
  let t2 = parked_task "s1" Code.Skip in
  Alcotest.(check int) "single cohort drains" 0
    (List.length (Tier2.check empty_sigma [ t1; t2 ]))

let test_check_two_cohorts_independent () =
  (* Two cohorts at distinct ids, no residuals — both fireable in
     either order. *)
  let t1 = parked_task "s1" Code.Skip in
  let t2 = parked_task "s2" Code.Skip in
  Alcotest.(check int) "two independent cohorts drain" 0
    (List.length (Tier2.check empty_sigma [ t1; t2 ]))

let test_check_staggered_two_cohorts () =
  (* t1: parked at s1, with s2 in its post-sync residual.
     t2: parked at s2, with no further sync.
     [Adv-Skip] for s1 fails because s2 ∈ barriers(rest of t1) — no,
     wait, for s1 advance: t2 is non-matching, and s1 is NOT in t2's
     post-sync residual, so s1 is fireable.
     For s2 advance: t1 is non-matching, and s2 IS in t1's post-sync
     residual, so s2 is NOT directly fireable.
     pick_fireable should pick s1 first; after firing, t1's residual
     is the s2 sync, which then forms a cohort with t2 at s2. *)
  let t1 = parked_task "s1" (mk_sync "s2") in
  let t2 = parked_task "s2" Code.Skip in
  Alcotest.(check int) "staggered two cohorts drain" 0
    (List.length (Tier2.check empty_sigma [ t1; t2 ]))

let test_check_stuck_when_no_id_fireable () =
  (* t1 parked at s1, with s2 in residual; t2 parked at s2, with s1 in
     residual. Neither id is fireable because each non-matcher has the
     other id reachable. Result: Stuck diagnostic. *)
  let t1 = parked_task "s1" (mk_sync "s2") in
  let t2 = parked_task "s2" (mk_sync "s1") in
  match Tier2.check empty_sigma [ t1; t2 ] with
  | [ Tier2.Stuck _ ] -> ()
  | _ -> Alcotest.fail "expected exactly one Stuck diagnostic"

(* test groups *)

let barriers_tests = [
  ("skip has no barriers",      `Quick, test_barriers_of_skip);
  ("blocking sync contributes", `Quick, test_barriers_of_blocking_sync);
  ("Arrive does not contribute",`Quick, test_barriers_of_arrive_only);
  ("Seq unions barriers",       `Quick, test_barriers_seq_union);
]

let group_tests = [
  ("group_at partitions",       `Quick, test_group_at_partitions);
]

let check_tests = [
  ("empty parked",              `Quick, test_check_empty);
  ("single cohort drains",      `Quick, test_check_single_cohort);
  ("two independent cohorts",   `Quick, test_check_two_cohorts_independent);
  ("staggered two cohorts",     `Quick, test_check_staggered_two_cohorts);
  ("stuck on cyclic future",    `Quick, test_check_stuck_when_no_id_fireable);
]

let () =
  Alcotest.run "tier2" [
    ("barriers",  barriers_tests);
    ("group_at",  group_tests);
    ("check",     check_tests);
  ]
