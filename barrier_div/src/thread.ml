open Protocols
open Exp

(* A thread class [b] proto: the set of concrete threads satisfying
   path condition [b], each running protocol [proto]. *)

type t = { path_cond : bexp; proto : Code.t }

(* Bundle of fields produced when a class hits a barrier. The Sync.t
   is preserved verbatim so downstream code can read its mode, count,
   array+index identity, and source location. *)
type sync_event = { sync : Sync.t; rest : t }

(* The result of one reduction step. The State scheduler routes these:

   - [Step]: thread continues running; possibly forked into multiple
     classes by branching or loop unfolding.
   - [Sync] / [Arrive] / [Wait]: a barrier event the scheduler must
     route into a Phase. The [rest] thread is what continues *after*
     the barrier from this class's point of view.
   - [Terminated]: protocol exhausted, thread can be dropped. *)
type action =
  | Step of t list
  | Sync of sync_event
  | Arrive of sync_event
  | Wait of sync_event
  | Terminated

(* Extract the leftmost reducible operation from [proto], paired with
   the rest of the protocol after it completes. [Decl] markers are
   transparent — we don't track local-variable scopes here; the caller
   relies on [Code.vars_distinct] having renamed binders so name
   clashes don't arise. *)
let rec head_of (proto : Code.t) : (Code.t * Code.t) option =
  match proto with
  | Skip -> None
  | (Access _ | Sync _ | If _ | Loop _) as op -> Some (op, Skip)
  | Decl { body; _ } -> head_of body
  | Seq (p, q) -> (
      match head_of p with
      | Some (op, rest) -> Some (op, Code.seq rest q)
      | None -> head_of q)

let mk_sync_event (sync : Sync.t) (rest : t) : sync_event = { sync; rest }

let step (t : t) : action =
  match head_of t.proto with
  | None -> Terminated
  | Some (head, rest) -> (
      match head with
      | Access _ -> Step [ { t with proto = rest } ]
      | Sync sync ->
          let evt = mk_sync_event sync { t with proto = rest } in
          (match sync.mode with
           | Sync.Mode.Sync -> Sync evt
           | Sync.Mode.Arrive -> Arrive evt
           | Sync.Mode.Wait -> Wait evt
           | Sync.Mode.ArriveAndWait -> Sync evt
           | Sync.Mode.ArriveAndDrop ->
               failwith "Phase 1: ArriveAndDrop not supported")
      | If (b, p, q) ->
          let then_pc = Exp.b_and t.path_cond b in
          let else_pc = Exp.b_and t.path_cond (Exp.b_not b) in
          Step
            [
              { path_cond = then_pc; proto = Code.seq p rest };
              { path_cond = else_pc; proto = Code.seq q rest };
            ]
      | Loop { range; body } ->
          let in_range = Range.to_cond range in
          let empty = Range.is_empty range in
          let active_pc = Exp.b_and t.path_cond in_range in
          let empty_pc = Exp.b_and t.path_cond empty in
          Step
            [
              { path_cond = active_pc; proto = Code.seq body rest };
              { path_cond = empty_pc; proto = rest };
            ]
      | Skip | Decl _ | Seq _ ->
          (* head_of never returns these *)
          assert false)

(* Engulfment helper: does this thread's protocol syntactically reference
   barrier id [sync] in any role? Used by Phase.can_fire to determine
   whether a phase has "settled" — i.e., no class in the surrounding P
   could still arrive at, wait on, or sync at this barrier. *)
let references ~(sync : Sync.t) (t : t) : bool =
  let same_id (s : Sync.t) : bool =
    Variable.equal s.array sync.array && s.index = sync.index
  in
  Code.exists (function Sync s -> same_id s | _ -> false) t.proto
