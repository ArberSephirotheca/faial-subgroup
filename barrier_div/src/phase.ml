open Protocols
open Exp

(* A sync episode in flight at barrier [sync] expecting [count] arrivals.
   [arrive_cohort] is the disjunction of path conditions of threads that
   have arrived; [wait_cohort] is the disjunction for waiters; [parked]
   holds the post-wait continuations of waiters.

   For bar.sync (the Phase 1 case), arrivals and waits coincide and
   [arrive_cohort = wait_cohort]. The two-cohort form is in place for
   future split-phase support. *)

type t = {
  sync : Sync.t;
  count : int;
  arrive_cohort : bexp;
  wait_cohort : bexp;
  parked : Thread.t list;
}

(* Resolve the participant count for a Sync to an int. [None] means
   __syncthreads — its implicit count is the warp size; [Some (Num k)]
   pins it explicitly. Symbolic counts are deferred to Phase 2. *)
let resolve_count (cfg : Rel_cost.Config.t) (sync : Sync.t) : int =
  match sync.count with
  | None -> cfg.threads_per_warp
  | Some (Num k) -> k
  | Some _ -> failwith "Phase 1: only constant barrier counts supported"

let id_eq (a : Sync.t) (b : Sync.t) : bool =
  Variable.equal a.array b.array && a.index = b.index

(* construction from arrival events *)

let of_sync (cfg : Rel_cost.Config.t) (evt : Thread.sync_event) : t =
  {
    sync = evt.sync;
    count = resolve_count cfg evt.sync;
    arrive_cohort = evt.rest.path_cond;
    wait_cohort = evt.rest.path_cond;
    parked = [ evt.rest ];
  }

let of_arrive (cfg : Rel_cost.Config.t) (evt : Thread.sync_event) : t =
  {
    sync = evt.sync;
    count = resolve_count cfg evt.sync;
    arrive_cohort = evt.rest.path_cond;
    wait_cohort = Bool false;
    parked = [];
  }

let of_wait (cfg : Rel_cost.Config.t) (evt : Thread.sync_event) : t =
  {
    sync = evt.sync;
    count = resolve_count cfg evt.sync;
    arrive_cohort = Bool false;
    wait_cohort = evt.rest.path_cond;
    parked = [ evt.rest ];
  }

(* incremental updates *)

let absorb_arrive (b : bexp) (p : t) : t =
  { p with arrive_cohort = Exp.b_or p.arrive_cohort b }

let absorb_wait (t : Thread.t) (p : t) : t =
  {
    p with
    wait_cohort = Exp.b_or p.wait_cohort t.path_cond;
    parked = t :: p.parked;
  }

let absorb_sync (t : Thread.t) (p : t) : t =
  p |> absorb_arrive t.path_cond |> absorb_wait t

(* identity *)

let matches (sync : Sync.t) (p : t) : bool = id_eq sync p.sync
let same_count (a : t) (b : t) : bool = a.count = b.count
let phases_share_id (a : t) (b : t) : bool = id_eq a.sync b.sync

(* SMT-backed status *)

let is_finished ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (p : t) : bool =
  Thread_count.equals ~timeout cfg locals p.arrive_cohort p.count

let can_admit ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (b : bexp) (p : t) : bool =
  let combined = Exp.b_or p.arrive_cohort b in
  Thread_count.at_most ~timeout cfg locals combined p.count

(* engulfment premise: no class in [threads] still references this phase's
   barrier id syntactically *)
let no_late_activity (threads : Thread.t list) (p : t) : bool =
  not (List.exists (Thread.references ~sync:p.sync) threads)

(* no-rival-reuse premise: no other phase shares this id with a different
   count. Excludes [p] itself by physical equality. *)
let no_rival_count (phases : t list) (p : t) : bool =
  not
    (List.exists
       (fun q -> q != p && phases_share_id p q && not (same_count p q))
       phases)

(* full Rule F premise *)
let can_fire ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (phases : t list) (threads : Thread.t list)
    (p : t) : bool =
  is_finished ~timeout cfg locals p
  && no_late_activity threads p
  && no_rival_count phases p

(* release the parked waiters; called by State when [can_fire] holds *)
let fire (p : t) : Thread.t list = p.parked
