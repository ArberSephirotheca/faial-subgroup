(* The bd obligation discharge.

   ⊨ bd(Σ; G)  ≜  ∀ T_i, T_j ∈ G.  ∀σ_i, σ_j.
       σ_i ≈_Σ σ_j  ∧  tid(σ_i) ≠ tid(σ_j)
     ∧ pre(σ_i) ∧ pre(σ_j)
     ∧ T_i.π(σ_i) ∧ T_j.π(σ_j)            -- cohort filter
     ⟹ T_i.δ(σ_i)  ⇔  T_j.δ(σ_j)          -- per-iteration agreement

   Encoding strategy:
   - Build two thread frames [t1] and [t2] by projecting Local
     variables: [x ↦ x$t1] in frame 1, [x ↦ x$t2] in frame 2. Unif
     (and Iter) variables share names across the two frames, which is
     how [≈_Σ] gets enforced — Iter is shared by the same-iteration-
     sample convention.
   - Pre is already conjoined into each task's [delta] at task
     construction time, so [δ_i] also carries [pre(σ_i)].
   - For each pair (T_i, T_j) ∈ G × G build the disjunct
       π_i$t1 ∧ π_j$t2 ∧ ¬(δ_i$t1 ⇔ δ_j$t2)
     and OR them together with the [tid distinct] precondition.
   - SAT ⇒ Fail (some pair disagrees); UNSAT ⇒ Pass. *)

open Protocols
open Exp

type witness = {
  reason : string;
  model : string;
}

type result = Pass | Fail of witness

(* The set of variables to project across the two thread frames. A
   variable is projected if some task in the group has it as Local.
   Iter binders stay shared (they represent the same iteration sample
   across both threads). *)
let projectable_vars (group : Task.t list) : Variable.Set.t =
  List.fold_left
    (fun acc (t : Task.t) -> Variable.Set.union acc (Sigma.locals t.sigma))
    Variable.Set.empty group

(* Tag for the two frames. *)
type frame = T1 | T2

let frame_suffix : frame -> string = function T1 -> "$t1" | T2 -> "$t2"

let project_var (f : frame) (x : Variable.t) : Variable.t =
  Variable.update_name (fun n -> n ^ frame_suffix f) x

(* Build a substitution that renames each variable in [vars] by
   suffixing with the frame's tag. *)
let frame_subst (f : frame) (vars : Variable.Set.t) : Subst.SubstAssoc.t =
  Variable.Set.elements vars
  |> List.map (fun x -> (Variable.name x, Var (project_var f x)))
  |> Subst.SubstAssoc.make

module Replace = Subst.Make (Subst.SubstAssoc)

let project (f : frame) (vars : Variable.Set.t) (b : bexp) : bexp =
  let s = frame_subst f vars in
  if Subst.SubstAssoc.is_empty s then b else Replace.b_subst s b

(* tid distinct: at least one of (tidx, tidy, tidz) differs across
   the two frames. *)
let tid_distinct () : bexp =
  let one_diff (x : Variable.t) : bexp =
    n_neq (Var (project_var T1 x)) (Var (project_var T2 x))
  in
  List.fold_left
    (fun acc x -> b_or acc (one_diff x))
    (Bool false) Variable.tid_list

(* (a ⇎ b) — disagreement between two booleans. *)
let b_xor (a : bexp) (b : bexp) : bexp =
  b_or (b_and a (b_not b)) (b_and (b_not a) b)

(* Build the disjunct for a single ordered pair (T_i, T_j). *)
let pair_clause (vars : Variable.Set.t) (ti : Task.t) (tj : Task.t) : bexp =
  let pi_i  = project T1 vars ti.pi in
  let pi_j  = project T2 vars tj.pi in
  let dl_i  = project T1 vars ti.delta in
  let dl_j  = project T2 vars tj.delta in
  b_and (b_and pi_i pi_j) (b_xor dl_i dl_j)

(* Build the negated bd obligation for the entire group. *)
let goal_of_group (group : Task.t list) : bexp =
  let vars = projectable_vars group in
  let clauses =
    List.concat_map
      (fun ti -> List.map (fun tj -> pair_clause vars ti tj) group)
      group
  in
  let body =
    List.fold_left b_or (Bool false) clauses
  in
  b_and (tid_distinct ()) body

let z3_solver : (module Gen_z3.Z3_SOLVER) = (module Gen_z3.Bv64Gen)

(* Feasibility test for a single task: is there any state σ where
   pre(σ) ∧ π(σ) ∧ δ(σ) holds?

   A task that fails this test is statically unreachable — no thread
   can take this path under any σ — so it would only contribute false
   counterexamples to the bd obligation. We drop such tasks before
   the cohort comparison.

   Returns:
   - [`Sat]    : the task may be reached;
   - [`Unsat]  : the task is dead, drop it;
   - [`Unknown msg] : the solver couldn't decide; conservatively keep
                     the task (treat as potentially reachable). *)
let task_feasible ?(timeout = 0) (pre : bexp) (t : Task.t)
    : [ `Sat | `Unsat | `Unknown of string ] =
  let goal =
    b_and (b_and pre t.Task.pi) t.delta |> Predicates.b_inline
  in
  let module S = (val z3_solver) in
  match S.solve ~timeout goal with
  | Ok (Gen_z3.Solver.Sat _) -> `Sat
  | Ok Gen_z3.Solver.Unsat -> `Unsat
  | Error msg -> `Unknown msg

let feasible_group ?(timeout = 0) (pre : bexp) (group : Task.t list)
    : Task.t list =
  List.filter
    (fun t ->
      match task_feasible ~timeout pre t with
      | `Unsat -> false
      | `Sat | `Unknown _ -> true)
    group

(* Build the negated bd obligation for the entire group, with [pre]
   treated as a per-frame side condition. *)
let goal_of_group_with_pre (pre : bexp) (group : Task.t list) : bexp =
  let vars = projectable_vars group in
  let pre_t1 = project T1 vars pre in
  let pre_t2 = project T2 vars pre in
  let clauses =
    List.concat_map
      (fun ti -> List.map (fun tj -> pair_clause vars ti tj) group)
      group
  in
  let body = List.fold_left b_or (Bool false) clauses in
  b_and (b_and pre_t1 pre_t2) (b_and (tid_distinct ()) body)

let discharge ?(timeout = 0) ?(pre = Bool true) (_sigma : Sigma.t)
    (group : Task.t list) : result =
  (* Drop unreachable tasks first — they cannot contribute a real
     thread to the cohort, but their [π/δ] can still produce spurious
     counterexamples to the strict pair-by-pair bd obligation. *)
  let group = feasible_group ~timeout pre group in
  match group with
  | [] | [ _ ] ->
      (* Empty or singleton cohorts are trivially in agreement: there
         is no pair of distinct threads to disagree. (A singleton
         self-pair would still need to satisfy the obligation, but
         since there's only one task and tid_distinct forces two
         different threads, both threads must be in the same task —
         and a task's δ is deterministic in σ, so it agrees with
         itself.) *)
      Pass
  | _ ->
      let goal = goal_of_group_with_pre pre group |> Predicates.b_inline in
      let module S = (val z3_solver) in
      match S.solve ~timeout goal with
      | Ok Gen_z3.Solver.Unsat -> Pass
      | Ok (Gen_z3.Solver.Sat m) ->
          Fail {
            reason = "bd obligation fails: some pair of threads in the cohort \
                      disagrees on the rendezvous-time path condition";
            model = Z3.Model.to_string m;
          }
      | Error msg ->
          Fail {
            reason = "bd obligation could not be discharged: " ^ msg;
            model = "";
          }
