open Protocols

(* Classification of stuck residual states into bug categories.
   Operates outside the rewrite system: takes a normal-form State.t
   and inspects each phase against the diagnostic table from
   documentation/barrier-participants.md. *)

(* Two reporting modes for the cohort_size in single-phase diagnostics:

   - [Witness] (default): use the count value from a SAT model. Cheap
     — one SAT call per direction. The reported [cohort_size] is some
     valuation that exhibits the bug, not necessarily the worst one.
   - [Precise]: after SAT confirms divergence, additionally run the
     optimizer to refine [cohort_size] to the actual extremum (min for
     Missing_participants, max for Oversize_cohort). Falls back to the
     SAT witness if the optimizer times out, so a slow refinement
     never suppresses a diagnostic. *)
type mode = Witness | Precise

(* The witness for a Missing_participants diagnostic depends on the
   query that produced it:

   - [Failing_thread] (single-tid SAT): a concrete tid that fails the
     cohort. Used for block-wide barriers, where Missing reduces to
     "is there *any* thread that doesn't arrive?".
   - [Cohort_size n] (vector-counting SAT or optimizer): a witness
     count value showing the cohort can be smaller than expected.
     Used for sub-warp named bars, where the bug is "cohort < count"
     and existence-of-a-missing-thread isn't enough to conclude. *)
type missing_witness =
  | Failing_thread of { x : int; y : int; z : int }
  | Cohort_size of int

type t =
  | Missing_participants of {
      sync : Sync.t;
      witness : missing_witness;
      expected : int;
    }
  | Oversize_cohort of {
      sync : Sync.t;
      cohort_size : int;
      expected : int;
    }
  | Count_mismatch of { sync : Sync.t; n : int; m : int }

let sync_of : t -> Sync.t = function
  | Missing_participants { sync; _ }
  | Oversize_cohort { sync; _ }
  | Count_mismatch { sync; _ } ->
      sync

let to_string : t -> string = function
  | Missing_participants { witness; expected; _ } ->
      let what =
        match witness with
        | Failing_thread { x; y; z } ->
            Printf.sprintf "thread tid=(%d,%d,%d) does not arrive" x y z
        | Cohort_size n -> Printf.sprintf "got %d" n
      in
      Printf.sprintf "Missing participants (%s, expected %d)" what expected
  | Oversize_cohort { cohort_size; expected; _ } ->
      Printf.sprintf "Oversize cohort (got %d, expected %d)" cohort_size
        expected
  | Count_mismatch { n; m; _ } ->
      Printf.sprintf "Count mismatch on barrier id (counts %d, %d)" n m

(* Single-phase diagnosis. We split on three structural cases:

   1. [expected > threads_per_warp]. The cohort is bounded above by
      [threads_per_warp], so it can never reach [expected]. Statically
      Missing — no SMT call needed.

   2. [expected = threads_per_warp]. Block-wide barrier (the
      __syncthreads case). Missing reduces to "is there a single thread
      in the block that fails the cohort?", answered by one SAT call
      over a single tid triple. Oversize is impossible (cohort ≤
      threads_per_warp = expected).

   3. [expected < threads_per_warp]. Sub-warp named bar — the cohort
      can be too small *or* too large at a count smaller than the
      block. Cardinality matters. Falls back to the existing SAT
      below/above queries; in [Precise] mode each SAT witness is
      refined via the optimizer with witness fallback on timeout. *)

let refine_size (mode : mode) (witness : int) (refine : unit -> int option) : int
    =
  match mode with
  | Witness -> witness
  | Precise -> Option.value (refine ()) ~default:witness

let of_phase_solo ?(mode = Witness) ?(timeout = 0) ~(pre : Exp.bexp)
    (cfg : Rel_cost.Config.t) (locals : Variable.Set.t) (p : Phase.t) : t list =
  if p.count > cfg.threads_per_warp then
    [
      Missing_participants
        {
          sync = p.sync;
          witness = Cohort_size cfg.threads_per_warp;
          expected = p.count;
        };
    ]
  else if p.count = cfg.threads_per_warp then
    match Thread_count.find_missing_thread ~timeout cfg ~pre p.arrive_cohort with
    | Some (x, y, z) ->
        [
          Missing_participants
            {
              sync = p.sync;
              witness = Failing_thread { x; y; z };
              expected = p.count;
            };
        ]
    | None -> []
  else
    let missing =
      match Thread_count.below ~timeout cfg locals p.arrive_cohort p.count with
      | Sat k ->
          let cohort_size =
            refine_size mode k (fun () ->
                Thread_count.refine_min ~timeout cfg locals p.arrive_cohort)
          in
          [
            Missing_participants
              { sync = p.sync; witness = Cohort_size cohort_size; expected = p.count };
          ]
      | Unsat | Unknown -> []
    in
    let oversize =
      match Thread_count.above ~timeout cfg locals p.arrive_cohort p.count with
      | Sat k ->
          let cohort_size =
            refine_size mode k (fun () ->
                Thread_count.refine_max ~timeout cfg locals p.arrive_cohort)
          in
          [ Oversize_cohort { sync = p.sync; cohort_size; expected = p.count } ]
      | Unsat | Unknown -> []
    in
    oversize @ missing

(* Pairwise diagnosis: same id, different counts → Count_mismatch.
   Reported only once per ordered pair (later phase compared against
   earlier) to avoid duplicates. *)
let pair_diagnostics (earlier : Phase.t) (later : Phase.t) : t list =
  if Phase.phases_share_id earlier later && earlier.count <> later.count then
    [ Count_mismatch { sync = later.sync; n = earlier.count; m = later.count } ]
  else []

let of_state ?(mode = Witness) ?(timeout = 0) ~(pre : Exp.bexp)
    (cfg : Rel_cost.Config.t) (locals : Variable.Set.t) (s : State.t) :
    t list =
  let solo =
    s.phases |> List.concat_map (of_phase_solo ~mode ~timeout ~pre cfg locals)
  in
  let rec pairs (acc : t list) = function
    | [] -> acc
    | p :: rest ->
        let with_each = List.concat_map (pair_diagnostics p) rest in
        pairs (with_each @ acc) rest
  in
  let pairwise = pairs [] s.phases in
  solo @ pairwise
