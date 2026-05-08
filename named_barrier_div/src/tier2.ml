(* Tier 2 — ⊢ ⟨0; R⟩

   Acts on the parked partition (the live partition is always 0 by the
   tier-1 invariant). Three pieces:

   - [group_at]:    G(R, n) — partition tasks by sync id.
   - [advance_n]:   R ⇒_n R' — inductive sync-advance.
   - [check]:       ⊢ ⟨0; R⟩ — the [D-Done] / [D-Sync] loop. *)

open Protocols

(* The head of a parked task is always a Sync; these helpers extract
   its id and the post-sync residual. They fail if invariants are
   violated. *)
let head_sync_id (t : Task.t) : Barriers.Id.t =
  match Tier1.head_split t.residual with
  | Some (Sync s, _) -> Barriers.Id.of_sync s
  | _ -> failwith "Tier2.head_sync_id: parked task without a sync head"

let post_sync_residual (t : Task.t) : Code.t =
  match Tier1.head_split t.residual with
  | Some (Sync _, rest) -> rest
  | _ -> failwith "Tier2.post_sync_residual: parked task without a sync head"

(* G(R, n) — the cohort at id [n] (and the rest of the parked tasks). *)
let group_at (n : Barriers.Id.t) (parked : Task.t list)
    : Task.t list * Task.t list =
  List.partition (fun t -> Barriers.Id.equal (head_sync_id t) n) parked

(* All distinct barrier ids appearing at the head of parked tasks. *)
let parked_ids (parked : Task.t list) : Barriers.IdSet.t =
  List.fold_left
    (fun acc t -> Barriers.IdSet.add (head_sync_id t) acc)
    Barriers.IdSet.empty parked

(* R ⇒_n R' — inductive sync-advance.

   Returns [None] when the rendezvous-completeness precondition fails:
   some non-matching task in [R] has [n] reachable in its post-sync
   residual, so firing [n] now would leave a future arrival at [n]
   unaccounted for. *)
let rec advance_n (n : Barriers.Id.t) (parked : Task.t list)
    : Task.t list option =
  match parked with
  | [] ->
      (* [Adv-Empty] *)
      Some []
  | t :: rest -> (
      if Barriers.Id.equal (head_sync_id t) n then
        (* [Adv-Match]: tier-1-advance the post-sync residual. *)
        let post : Task.t = { t with residual = post_sync_residual t } in
        let post_state = Tier1.reduce post in
        assert (post_state.live = []);
        match advance_n n rest with
        | None -> None
        | Some rest' -> Some (post_state.parked @ rest')
      else
        (* [Adv-Skip]: keep the task in place if [n] is not in the
           future barriers of its post-sync residual. *)
        let future = Barriers.of_code (post_sync_residual t) in
        if Barriers.IdSet.mem n future then None
        else
          match advance_n n rest with
          | None -> None
          | Some rest' -> Some (t :: rest'))

(* Pick the first id whose [D-Sync] precondition is satisfiable; also
   return the advanced parked partition so we don't recompute it. *)
let pick_fireable (parked : Task.t list)
    : (Barriers.Id.t * Task.t list) option =
  Barriers.IdSet.fold
    (fun n acc ->
      match acc with
      | Some _ -> acc
      | None -> (
          match advance_n n parked with
          | Some parked' -> Some (n, parked')
          | None -> None))
    (parked_ids parked) None

(* Outcome of a single [D-Sync] firing or the entire analysis. *)
type diagnostic =
  | Bd_failure of {
      id : Barriers.Id.t;
      group : Task.t list;
      witness : Bd.witness;
    }
  | Stuck of Task.t list
        (* No id is fireable: deadlock or imprecise [barriers] for
           symbolic ids. The remaining parked partition is captured
           verbatim so the caller can report it. *)

(* ⊢ ⟨0; R⟩ — top-level loop.

   Returns the (possibly empty) list of diagnostics; the empty list
   indicates the program type-checks. *)
let rec check ?(timeout = 0) ?(pre = Exp.Bool true) (sigma : Sigma.t)
    (parked : Task.t list) : diagnostic list =
  match parked with
  | [] -> []
  | _ -> (
      match pick_fireable parked with
      | None -> [ Stuck parked ]
      | Some (id, parked') ->
          let group, _other = group_at id parked in
          let bd_diags =
            match Bd.discharge ~timeout ~pre sigma group with
            | Bd.Pass -> []
            | Bd.Fail witness -> [ Bd_failure { id; group; witness } ]
          in
          bd_diags @ check ~timeout ~pre sigma parked')
