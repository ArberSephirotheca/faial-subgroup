(* Per-kernel Z3 slot for discharging delin-emitted bounds. Holds a
   persistent solver with [kernel.pre /\ runtime] preloaded; each
   per-bound query pushes [scope /\ ~bound], checks UNSAT, pops.
   UNSAT means the bound is entailed; SAT and UNKNOWN both mean the
   caller should not rely on the bound. *)
open Stage0
open Protocols
open Exp

type t = {
  ctx : Z3.context;
  solver : Z3.Solver.solver;
}

let make ~(timeout : int) (k : Aligned.Kernel.t) : t =
  let runtime =
    Params.to_bexp (Params.union_left k.global_variables k.local_variables)
  in
  let base_goal =
    b_and k.pre runtime
    |> Predicates.b_inline
    |> Predicates.strip_cross_thread
  in
  let args =
    if timeout > 0 then [ ("timeout", string_of_int timeout) ] else []
  in
  let ctx = Z3.mk_context args in
  let solver = Z3.Solver.mk_solver ctx None in
  Z3.Solver.add solver [ Gen_z3.Bv64Gen.b_to_expr ctx base_goal ];
  { ctx; solver }

let release (t : t) : unit = Z3.Solver.reset t.solver

let with_slot ~(timeout : int) (k : Aligned.Kernel.t) (f : t -> 'a) : 'a =
  let slot = make ~timeout k in
  Fun.protect ~finally:(fun () -> release slot) (fun () -> f slot)

let entails (t : t) ~(scope : bexp list) ~(bound : bexp) : bool =
  let delta =
    b_and (b_and_ex scope) (b_not bound)
    |> Predicates.b_inline
    |> Predicates.strip_cross_thread
  in
  Z3.Solver.push t.solver;
  Z3.Solver.add t.solver [ Gen_z3.Bv64Gen.b_to_expr t.ctx delta ];
  let result =
    Phase_timer.measure "delin/verify" (fun () ->
      Z3.Solver.check t.solver [])
  in
  Z3.Solver.pop t.solver 1;
  match result with
  | Z3.Solver.UNSATISFIABLE -> true
  | Z3.Solver.SATISFIABLE -> false
  | Z3.Solver.UNKNOWN -> false

(* Anti-vacuity check, the satisfiability mirror of [entails]: pushes
   [scope /\ bound] and asks SAT. SAT means the assumed bound leaves a
   non-empty state space (safe to assume); UNSAT means it contradicts
   [kernel.pre /\ runtime /\ scope] and assuming it would erase the
   access, i.e. a vacuous delinearisation, so the caller must refuse it.
   UNKNOWN is treated as inconsistent: we never assume a bound we cannot
   confirm is consistent. *)
let consistent (t : t) ~(scope : bexp list) ~(bound : bexp) : bool =
  let delta =
    b_and (b_and_ex scope) bound
    |> Predicates.b_inline
    |> Predicates.strip_cross_thread
  in
  Z3.Solver.push t.solver;
  Z3.Solver.add t.solver [ Gen_z3.Bv64Gen.b_to_expr t.ctx delta ];
  let result =
    Phase_timer.measure "delin/consistent" (fun () ->
      Z3.Solver.check t.solver [])
  in
  Z3.Solver.pop t.solver 1;
  match result with
  | Z3.Solver.SATISFIABLE -> true
  | Z3.Solver.UNSATISFIABLE -> false
  | Z3.Solver.UNKNOWN -> false
