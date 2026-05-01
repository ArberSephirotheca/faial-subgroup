open Protocols

(* The scheduler. Holds the global state ⟨M; P⟩ and routes the events
   that Thread.step emits. The scheduler's job is to:

   - apply Thread.step to a thread and route the resulting action
     (forked classes go back to threads; barrier events update phases);
   - fire any phase whose Rule F premises hold (count match, no late
     activity, no rival reuse);
   - iterate until normal form ⟨∅; ∅⟩ or a stuck residual.

   Thread step takes priority over phase fire — only when no thread
   can act do we try to fire. This conservatively approximates Rule
   F's engulfment premise and matches the conventional small-step
   reduction strategy. *)

type t = { phases : Phase.t list; threads : Thread.t list }

let initial (t : Thread.t) : t = { phases = []; threads = [ t ] }
let is_normal (s : t) : bool = s.threads = [] && s.phases = []

(* Routing helpers: try to merge an arrival event into an existing
   phase, falling back to creating a new one if no compatible phase
   has budget room. *)

type event_kind = Arrive | Wait | Sync

let absorb_into ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (kind : event_kind) (evt : Thread.sync_event)
    (phases : Phase.t list) : Phase.t list =
  let count = Phase.resolve_count cfg evt.sync in
  let mk_new () =
    match kind with
    | Arrive -> Phase.of_arrive cfg evt
    | Wait -> Phase.of_wait cfg evt
    | Sync -> Phase.of_sync cfg evt
  in
  let absorb p =
    match kind with
    | Arrive -> Phase.absorb_arrive evt.rest.path_cond p
    | Wait -> Phase.absorb_wait evt.rest p
    | Sync -> Phase.absorb_sync evt.rest p
  in
  let check_budget = match kind with Wait -> false | _ -> true in
  let compatible p =
    Phase.matches evt.sync p
    && p.count = count
    && ((not check_budget)
       || Phase.can_admit ~timeout cfg locals evt.rest.path_cond p)
  in
  let rec find = function
    | [] -> [ mk_new () ]
    | p :: rest when compatible p -> absorb p :: rest
    | p :: rest -> p :: find rest
  in
  find phases

(* Try to step any thread. Returns Some s' if any thread could act. *)
let step_thread ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (s : t) : t option =
  match s.threads with
  | [] -> None
  | t :: rest -> (
      match Thread.step t with
      | Terminated -> Some { s with threads = rest }
      | Step ts -> Some { s with threads = ts @ rest }
      | Sync evt ->
          Some
            {
              phases = absorb_into ~timeout cfg locals Sync evt s.phases;
              threads = rest;
            }
      | Arrive evt ->
          Some
            {
              phases = absorb_into ~timeout cfg locals Arrive evt s.phases;
              threads = evt.rest :: rest;
            }
      | Wait evt ->
          Some
            {
              phases = absorb_into ~timeout cfg locals Wait evt s.phases;
              threads = rest;
            })

(* Try to fire any phase. Returns Some s' if any phase could fire. *)
let step_phase ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (s : t) : t option =
  let rec scan (acc : Phase.t list) = function
    | [] -> None
    | p :: rest ->
        if Phase.can_fire ~timeout cfg locals s.phases s.threads p then
          let released = Phase.fire p in
          Some
            {
              phases = List.rev_append acc rest;
              threads = released @ s.threads;
            }
        else scan (p :: acc) rest
  in
  scan [] s.phases

let step ?(timeout = 0) (cfg : Rel_cost.Config.t) (locals : Variable.Set.t)
    (s : t) : t option =
  match step_thread ~timeout cfg locals s with
  | Some _ as r -> r
  | None -> step_phase ~timeout cfg locals s

let rec reduce ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (s : t) : t =
  match step ~timeout cfg locals s with
  | Some s' -> reduce ~timeout cfg locals s'
  | None -> s
