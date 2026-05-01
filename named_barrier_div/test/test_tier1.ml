open Stage0
open Protocols
open Exp
open Named_barrier_div

(* Convenience constructors. *)
let v (name : string) : Variable.t = Variable.from_name name
let var (name : string) : nexp = Var (v name)

let access : Code.t =
  Code.Access { array = v "a"; index = [ Num 0 ]; mode = Access.Mode.Read }

let sync_at ?(mode = Sync.Mode.Sync) (label : string) : Sync.t =
  { Sync.mode; array = v label; index = []; count = None;
    loc = Some Location.empty }

let mk_sync ?mode (label : string) : Code.t = Code.Sync (sync_at ?mode label)

let empty_sigma : Sigma.t = Sigma.empty

let task_of (residual : Code.t) : Task.t =
  Task.make ~sigma:empty_sigma residual

(* head_split *)

let test_head_skip () =
  Alcotest.(check bool) "skip yields none" true
    (Tier1.head_split Code.Skip = None)

let test_head_access_alone () =
  match Tier1.head_split access with
  | Some (Code.Access _, Code.Skip) -> ()
  | _ -> Alcotest.fail "access alone should yield (Access, Skip)"

let test_head_seq_skip_left () =
  let proto = Code.Seq (Code.Skip, access) in
  match Tier1.head_split proto with
  | Some (Code.Access _, Code.Skip) -> ()
  | _ -> Alcotest.fail "Skip; Access should yield (Access, Skip)"

let test_head_seq_access_then_sync () =
  let proto = Code.Seq (access, mk_sync "s1") in
  match Tier1.head_split proto with
  | Some (Code.Access _, Code.Sync _) -> ()
  | _ -> Alcotest.fail "Access; Sync should yield (Access, Sync)"

let test_head_decl_not_transparent () =
  (* Unlike Thread.head_of, head_split treats Decl as a head so we can
     extend Σ. *)
  let proto = Code.Decl { var = v "x"; ty = C_type.int; body = access } in
  match Tier1.head_split proto with
  | Some (Code.Decl _, Code.Skip) -> ()
  | _ -> Alcotest.fail "decl should be a head, not transparent"

(* reduce *)

let test_reduce_skip () =
  let s = Tier1.reduce (task_of Code.Skip) in
  Alcotest.(check bool) "skip yields empty state" true (State.is_empty s)

let test_reduce_access () =
  let s = Tier1.reduce (task_of access) in
  Alcotest.(check bool) "access drains" true (State.is_empty s)

let test_reduce_sync_parks () =
  let s = Tier1.reduce (task_of (mk_sync "b")) in
  Alcotest.(check bool) "live empty"  true (s.live = []);
  Alcotest.(check int)  "one parked"  1    (List.length s.parked)

let test_reduce_arrive_is_noop () =
  (* [Sync.Mode.Arrive] is non-blocking: tier 1 should treat it as
     [T-Acc] and not park anything. *)
  let s = Tier1.reduce (task_of (mk_sync ~mode:Sync.Mode.Arrive "b")) in
  Alcotest.(check bool) "arrive drains" true (State.is_empty s)

let test_reduce_wait_is_noop () =
  let s = Tier1.reduce (task_of (mk_sync ~mode:Sync.Mode.Wait "b")) in
  Alcotest.(check bool) "wait drains" true (State.is_empty s)

let test_reduce_arrive_and_drop_is_noop () =
  let s = Tier1.reduce (task_of (mk_sync ~mode:Sync.Mode.ArriveAndDrop "b")) in
  Alcotest.(check bool) "arrive_and_drop drains" true (State.is_empty s)

let test_reduce_arrive_and_wait_parks () =
  (* [ArriveAndWait] is blocking just like [Sync]. *)
  let s = Tier1.reduce (task_of (mk_sync ~mode:Sync.Mode.ArriveAndWait "b")) in
  Alcotest.(check int) "one parked" 1 (List.length s.parked)

let test_reduce_access_then_sync_parks_post () =
  (* Access; Sync — the access is consumed, the parked task's residual
     is the Sync (rest = Skip). *)
  let proto = Code.Seq (access, mk_sync "b") in
  let s = Tier1.reduce (task_of proto) in
  Alcotest.(check int) "one parked" 1 (List.length s.parked)

let test_reduce_if_forks () =
  (* if (x < 5) { skip } else { sync s1 }  →  one branch drains,
     the other parks. *)
  let cond = n_lt (var "x") (Num 5) in
  let proto = Code.If (cond, Code.Skip, mk_sync "s1") in
  let s = Tier1.reduce (task_of proto) in
  Alcotest.(check bool) "live empty (no live partition after If)"
    true (s.live = []);
  Alcotest.(check int) "one parked branch" 1 (List.length s.parked)

let test_reduce_loop_forks () =
  (* for i in [0, K) skip   →   both branches drain to skip *)
  let k = var "K" in
  let i = v "i" in
  let r = Range.{
    var = i; ty = C_type.int;
    lower_bound = Num 0; upper_bound = k;
    step = Plus (Num 1); dir = Increase;
  } in
  let proto = Code.Loop { range = r; body = Code.Skip } in
  let s = Tier1.reduce (task_of proto) in
  Alcotest.(check bool) "loop with empty body drains" true (State.is_empty s)

let test_reduce_decl_extends_sigma () =
  (* decl x; sync s1   →   parked task has x ↦ Local in its sigma *)
  let proto =
    Code.Decl { var = v "x"; ty = C_type.int; body = mk_sync "s1" }
  in
  let s = Tier1.reduce (task_of proto) in
  match s.parked with
  | [ t ] ->
      Alcotest.(check bool) "x is Local in parked task's sigma"
        true (Modifier.equal (Sigma.find (v "x") t.sigma) Modifier.Local)
  | _ -> Alcotest.fail "expected exactly one parked task"

(* test groups *)

let head_split_tests = [
  ("skip yields none",            `Quick, test_head_skip);
  ("access alone",                `Quick, test_head_access_alone);
  ("Skip; Access",                `Quick, test_head_seq_skip_left);
  ("Access; Sync",                `Quick, test_head_seq_access_then_sync);
  ("Decl is a head",              `Quick, test_head_decl_not_transparent);
]

let reduce_tests = [
  ("skip drains",                       `Quick, test_reduce_skip);
  ("access drains",                     `Quick, test_reduce_access);
  ("blocking sync parks",               `Quick, test_reduce_sync_parks);
  ("Arrive is no-op",                   `Quick, test_reduce_arrive_is_noop);
  ("Wait is no-op",                     `Quick, test_reduce_wait_is_noop);
  ("ArriveAndDrop is no-op",            `Quick, test_reduce_arrive_and_drop_is_noop);
  ("ArriveAndWait parks",               `Quick, test_reduce_arrive_and_wait_parks);
  ("Access; Sync parks post-access",    `Quick, test_reduce_access_then_sync_parks_post);
  ("If forks",                          `Quick, test_reduce_if_forks);
  ("Loop drains when body is skip",     `Quick, test_reduce_loop_forks);
  ("Decl extends sigma with Local",     `Quick, test_reduce_decl_extends_sigma);
]

let () =
  Alcotest.run "tier1" [
    ("head_split", head_split_tests);
    ("reduce", reduce_tests);
  ]
