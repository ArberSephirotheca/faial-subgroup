open Stage0
open Protocols
open Exp
open Named_barrier_div

let v (name : string) : Variable.t = Variable.from_name name
let var (name : string) : nexp = Var (v name)

(* A minimal sigma in which [tidx] is Local and everything else
   defaults to Unif. *)
let sigma_with_tidx_local : Sigma.t =
  Sigma.add Variable.tid_x Modifier.Local Sigma.empty

(* Build a parked task at sync id "n" with a given pi/delta. *)
let mk_task ?(sigma = sigma_with_tidx_local) ?(pi = Bool true)
    ?(delta = Bool true) () : Task.t =
  let s : Sync.t = {
    Sync.mode = Sync.Mode.ArriveAndWait;
    id = Var (v "n");
    participants = None;
    loc = Some Location.empty;
  } in
  Task.make ~sigma ~pi ~delta (Code.Sync s)

(* bd discharge passes on an empty group. *)
let test_empty_group () =
  match Bd.discharge sigma_with_tidx_local [] with
  | Bd.Pass -> ()
  | Bd.Fail _ -> Alcotest.fail "empty group should pass"

(* bd discharge passes on a singleton (no second task to disagree). *)
let test_singleton () =
  let t = mk_task () in
  match Bd.discharge sigma_with_tidx_local [ t ] with
  | Bd.Pass -> ()
  | Bd.Fail _ -> Alcotest.fail "singleton should pass"

(* Two identical tasks (π=⊤, δ=⊤): trivial agreement, pass. *)
let test_two_trivial_tasks () =
  let t1 = mk_task () in
  let t2 = mk_task () in
  match Bd.discharge sigma_with_tidx_local [ t1; t2 ] with
  | Bd.Pass -> ()
  | Bd.Fail _ -> Alcotest.fail "two trivial tasks should pass"

(* Two tasks with disjoint π but δ=⊤: cohort filter excludes
   cross-pairs; same-task pairs trivially agree. Pass. *)
let test_disjoint_pi () =
  let pi_t1 = NRel (N_rel.Eq, n_mod (var "threadIdx.x") (Num 2), Num 0) in
  let pi_t2 = NRel (N_rel.Neq, n_mod (var "threadIdx.x") (Num 2), Num 0) in
  let t1 = mk_task ~pi:pi_t1 () in
  let t2 = mk_task ~pi:pi_t2 () in
  match Bd.discharge sigma_with_tidx_local [ t1; t2 ] with
  | Bd.Pass -> ()
  | Bd.Fail _ -> Alcotest.fail "disjoint-pi tasks should pass"

(* Two tasks with same π=⊤ but mutually-exclusive δ: bd should fail
   under the formalism — two threads in the same cohort would
   disagree on whether they reach the rendezvous via the same
   path. *)
let test_disjoint_delta_same_pi () =
  let n = var "N" in
  let delta_a = n_lt (Num 0) n in
  let delta_b = n_le n (Num 0) in
  let t1 = mk_task ~delta:delta_a () in
  let t2 = mk_task ~delta:delta_b () in
  match Bd.discharge sigma_with_tidx_local [ t1; t2 ] with
  | Bd.Fail _ -> ()
  | Bd.Pass -> Alcotest.fail "disjoint-delta tasks should fail"

(* Two-task disagreement on a Local-mentioning δ — a thread-private
   condition routed (incorrectly) into δ would break agreement.
   This catches the kind of false negative we'd see if a Local
   guard was misrouted. *)
let test_local_in_delta_disagrees () =
  let g = NRel (N_rel.Lt, var "threadIdx.x", Num 4) in
  let t1 = mk_task ~delta:g () in
  let t2 = mk_task ~delta:(b_not g) () in
  match Bd.discharge sigma_with_tidx_local [ t1; t2 ] with
  | Bd.Fail _ -> ()
  | Bd.Pass -> Alcotest.fail "local-in-delta disagreement should fail"

(* Feasibility filter — saxpy-style false positive should NOT fire.
   T_a's δ requires N>0 (loop entered: 0≤i<N), T_b's δ requires
   N≤0 (empty range: 0≤r<N ∧ N≤0 ⇒ unsat). T_b is unreachable —
   the filter drops it, leaving the cohort {T_a}, which trivially
   agrees with itself. *)
let test_feasibility_filter_drops_dead_task () =
  let n = var "N" in
  let r = var "r" in
  let i = var "i" in
  let in_range x = b_and (n_le (Num 0) x) (n_lt x n) in
  let delta_a = b_and (in_range r) (in_range i) in
  let delta_b = b_and (in_range r) (n_le n (Num 0)) in
  let t1 = mk_task ~delta:delta_a () in
  let t2 = mk_task ~delta:delta_b () in
  match Bd.discharge sigma_with_tidx_local [ t1; t2 ] with
  | Bd.Pass -> ()
  | Bd.Fail _ ->
      Alcotest.fail "feasibility filter should drop the dead empty-range task"

(* Pre that forces both deltas to the same truth value under every
   satisfying model: bd should pass because δ_a ⇔ δ_b is always
   true under the antecedent. *)
let test_pre_forces_agreement () =
  let n = var "N" in
  let delta_a = n_eq n (Num 5) in
  let delta_b = n_lt (Num 0) n in
  (* Pre fixes N=5; under that, both deltas are true. *)
  let pre = n_eq n (Num 5) in
  let t1 = mk_task ~delta:delta_a () in
  let t2 = mk_task ~delta:delta_b () in
  match Bd.discharge ~pre sigma_with_tidx_local [ t1; t2 ] with
  | Bd.Pass -> ()
  | Bd.Fail _ ->
      Alcotest.fail "pre forcing both deltas true should pass"

let bd_tests = [
  ("empty group passes",          `Quick, test_empty_group);
  ("singleton passes",            `Quick, test_singleton);
  ("two trivial tasks pass",      `Quick, test_two_trivial_tasks);
  ("disjoint pi passes",          `Quick, test_disjoint_pi);
  ("disjoint delta fails",        `Quick, test_disjoint_delta_same_pi);
  ("local-in-delta fails",        `Quick, test_local_in_delta_disagrees);
  ("feasibility filter passes",   `Quick, test_feasibility_filter_drops_dead_task);
  ("pre forces agreement",        `Quick, test_pre_forces_agreement);
]

let () = Alcotest.run "bd" [ ("discharge", bd_tests) ]
