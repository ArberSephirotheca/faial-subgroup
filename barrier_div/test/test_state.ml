open Stage0
open Protocols
open Exp
open Barrier_div

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

let cfg : Rel_cost.Config.t =
  let block_dim = Dim3.make ~x:32 () in
  let grid_dim = Dim3.one in
  Rel_cost.Config.make ~block_dim ~grid_dim ()

let locals : Variable.Set.t = Variable.tid_set

let mk_thread (path_cond : bexp) (proto : Code.t) : Thread.t =
  { Thread.path_cond; proto }

(* helpers *)

let reduce_to_normal (s : State.t) : State.t = State.reduce cfg locals s

(* tests *)

let test_empty_kernel () =
  (* No threads, no phases — already normal *)
  let s : State.t = { phases = []; threads = [] } in
  Alcotest.(check bool) "empty is normal" true (State.is_normal s)

let test_single_skip_terminates () =
  let t = mk_thread (Bool true) Code.Skip in
  let s = State.initial t in
  let s' = reduce_to_normal s in
  Alcotest.(check bool) "skip thread reduces to normal" true
    (State.is_normal s')

let test_single_sync_fires_uniform () =
  (* One class with all threads, single bar.sync — should fire and reduce
     to ⟨∅; ∅⟩ *)
  let s = mk_sync () in
  let proto = Code.Sync s in
  let t = mk_thread (Bool true) proto in
  let s' = reduce_to_normal (State.initial t) in
  Alcotest.(check bool) "uniform sync reduces to normal" true
    (State.is_normal s')

let test_partial_cohort_stuck () =
  (* tid<17 reaching sync; cohort is 17 of 32, should be stuck *)
  let s = mk_sync () in
  let pc = n_lt (Var Variable.tid_x) (Num 17) in
  let proto = Code.Sync s in
  let t = mk_thread pc proto in
  let s' = reduce_to_normal (State.initial t) in
  Alcotest.(check int) "one stuck phase remains" 1 (List.length s'.phases);
  Alcotest.(check int) "no threads left" 0 (List.length s'.threads)

let test_convergent_if_else () =
  (* if (tid<17) sync else sync — both branches reach the same lexical sync,
     should merge and fire *)
  let s = mk_sync () in
  let cond = n_lt (Var Variable.tid_x) (Num 17) in
  let proto = Code.If (cond, Code.Sync s, Code.Sync s) in
  let t = mk_thread (Bool true) proto in
  let s' = reduce_to_normal (State.initial t) in
  Alcotest.(check bool) "convergent if/else reduces to normal" true
    (State.is_normal s')

let test_missing_participants () =
  (* if (tid<17) sync — only 17 threads sync, 32 expected → stuck *)
  let s = mk_sync () in
  let cond = n_lt (Var Variable.tid_x) (Num 17) in
  let proto = Code.If (cond, Code.Sync s, Code.Skip) in
  let t = mk_thread (Bool true) proto in
  let s' = reduce_to_normal (State.initial t) in
  Alcotest.(check int) "stuck phase remains" 1 (List.length s'.phases);
  Alcotest.(check int) "no threads left" 0 (List.length s'.threads)

let test_sequential_syncs () =
  (* sync s1; sync s2 — both uniform, should fire in sequence *)
  let s1 = mk_sync ~array:"b1" () in
  let s2 = mk_sync ~array:"b2" () in
  let proto = Code.Seq (Code.Sync s1, Code.Sync s2) in
  let t = mk_thread (Bool true) proto in
  let s' = reduce_to_normal (State.initial t) in
  Alcotest.(check bool) "sequential syncs reduce to normal" true
    (State.is_normal s')

let test_engulfment_blocks_premature_fire () =
  (* Two parallel classes both targeting same sync; one arrives first
     but fire should be blocked until the second arrives. *)
  let s = mk_sync () in
  let pc1 = n_lt (Var Variable.tid_x) (Num 17) in
  let pc2 = n_le (Num 17) (Var Variable.tid_x) in
  let t1 = mk_thread pc1 (Code.Sync s) in
  let t2 = mk_thread pc2 (Code.Sync s) in
  let s' = reduce_to_normal { phases = []; threads = [ t1; t2 ] } in
  Alcotest.(check bool) "both arrive then merge then fire" true
    (State.is_normal s')

let tests =
  [
    ("empty kernel", `Quick, test_empty_kernel);
    ("single skip", `Quick, test_single_skip_terminates);
    ("uniform sync fires", `Slow, test_single_sync_fires_uniform);
    ("partial cohort stuck", `Slow, test_partial_cohort_stuck);
    ("convergent if/else", `Slow, test_convergent_if_else);
    ("missing participants", `Slow, test_missing_participants);
    ("sequential syncs", `Slow, test_sequential_syncs);
    ("engulfment blocks fire", `Slow, test_engulfment_blocks_premature_fire);
  ]

let () = Alcotest.run "state" [ ("reduce", tests) ]
