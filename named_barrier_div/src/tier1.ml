(* Tier 1 — T ▷ ⟨L; R⟩

   Reduces a single task structurally. Sync-headed tasks accumulate
   in the parked partition; everything else is consumed.

   Implements the rules [T-Done], [T-Stop], [T-Decl], [T-Acc],
   [T-If], [T-Loop].

   Invariant: every reduction lands in ⟨0; R⟩ — the live partition is
   fully drained because [T-Stop] is the only rule that produces a
   parked task, and it produces an empty live partition. *)

open Protocols
open Exp

(* head_split c  =  Some (head, rest)  if c reduces to head followed
   by rest at the leftmost reducible position; None if c is empty.

   Walks Seq trees to find the leftmost non-Skip operation. Decl is
   *not* transparent (unlike [Thread.head_of] in the old engine) — we
   need to see Decl as a head to extend Σ. *)
let rec head_split (c : Code.t) : (Code.t * Code.t) option =
  match c with
  | Skip -> None
  | (Access _ | Sync _ | If _ | Loop _ | Decl _) as op -> Some (op, Skip)
  | Seq (p, q) -> (
      match head_split p with
      | Some (op, rest_p) -> Some (op, Code.seq rest_p q)
      | None -> head_split q)

(* bind_kappa Σ b  =  the modifier to bind a loop variable, given the
   classification of the loop's range condition b:
     bind(unif) = Unif    (all fv are Unif)
     bind(div)  = Iter    (some fv is Iter or Local) *)
let bind_kappa (sigma : Sigma.t) (b : bexp) : Modifier.t =
  let fv = b_free_names b Variable.Set.empty in
  let all_unif =
    Variable.Set.for_all
      (fun v -> Modifier.equal (Sigma.find v sigma) Unif)
      fv
  in
  if all_unif then Modifier.Unif else Modifier.Iter

(* T ▷ ⟨L; R⟩ *)
let rec reduce (t : Task.t) : State.t =
  match head_split t.residual with
  | None ->
      (* [T-Done]: residual is Skip *)
      State.empty
  | Some (Sync s, rest) ->
      if Barriers.is_blocking s then
        (* [T-Stop]: blocking rendezvous — keep the original task (head +
           rest) parked. The full residual is t.residual itself, unchanged. *)
        State.parked t
      else
        (* Non-blocking modes (Arrive, Wait, ArriveAndDrop) are no-ops
           for NBD: they don't form rendezvous points, so we advance
           past them like [T-Acc]. *)
        reduce { t with residual = rest }
  | Some (Decl { var; body; _ }, rest) ->
      (* [T-Decl]: extend Σ with x:Local; advance to body; rest *)
      let sigma' = Sigma.add var Modifier.Local t.sigma in
      let residual = Code.seq body rest in
      reduce { t with sigma = sigma'; residual }
  | Some (Access _, rest) ->
      (* [T-Acc]: advance past the access *)
      reduce { t with residual = rest }
  | Some (If (b, p, q), rest) ->
      (* [T-If]: route b/¬b via the +_Σ helper, recurse on each branch *)
      let pi_p, delta_p =
        Task.route_guard t.sigma ~pi:t.pi ~delta:t.delta b
      in
      let pi_q, delta_q =
        Task.route_guard t.sigma ~pi:t.pi ~delta:t.delta (b_not b)
      in
      let task_p =
        { t with pi = pi_p; delta = delta_p; residual = Code.seq p rest }
      in
      let task_q =
        { t with pi = pi_q; delta = delta_q; residual = Code.seq q rest }
      in
      State.union (reduce task_p) (reduce task_q)
  | Some (Loop { range; body }, rest) ->
      (* [T-Loop]: classify the range, pick the binder modifier, recurse
         on body+rest under the iter convention and on rest under the
         empty-range branch. Range conditions go through the routing
         helper just like [T-If], so a range mentioning a Local lands
         in π (rare in practice). *)
      let cond_r = Range.to_cond range in
      let empty_r = Range.is_empty range in
      let kappa = bind_kappa t.sigma cond_r in
      let pi_body, delta_body =
        Task.route_guard t.sigma ~pi:t.pi ~delta:t.delta cond_r
      in
      let pi_rest, delta_rest =
        Task.route_guard t.sigma ~pi:t.pi ~delta:t.delta empty_r
      in
      let body_task =
        {
          Task.sigma = Sigma.add range.var kappa t.sigma;
          pi = pi_body;
          delta = delta_body;
          residual = Code.seq body rest;
        }
      in
      let rest_task =
        { t with pi = pi_rest; delta = delta_rest; residual = rest }
      in
      State.union (reduce body_task) (reduce rest_task)
  | Some ((Skip | Seq _), _) ->
      (* head_split never returns these as heads. *)
      assert false
