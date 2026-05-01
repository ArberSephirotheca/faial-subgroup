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

(* Symmetric to [missing_witness] for Oversize: either a count value
   (from the vector / optimizer path) or a list of distinct tids that
   all satisfy the cohort (from the small-distinctness SAT, used for
   sub-warp named bars — its length is [expected + 1], proving the
   cohort can hold strictly more than [expected]). *)
type oversize_witness =
  | Cohort_size_over of int
  | Witness_threads of (int * int * int) list

(* Sync-only ([b_a = b_w]) and split-phase ([b_a ≠ b_w]) residuals share
   the same arrival-side analysis (count vs expected) but differ in how
   the bug is reported. The diagnostic table at
   documentation/barrier-participants.md lines 251-263 distinguishes:

   - Sync-only, [|b_a| < n]   → [Missing_participants]
   - Split, [|b_a| < n], [b_w = ⊥]   → [Incomplete_arrivals]
       (no waiters affected, but barrier never fires — contract
        violation; any future wait would deadlock)
   - Split, [|b_a| < n], [b_w ≠ ⊥]   → [Waiters_blocked]
       (waiters parked inside the membrane will never be released)
   - Any shape, [|b_a| > n]   → [Oversize_cohort]
   - [b_a = ⊥], [b_w ≠ ⊥]      → [Standalone_wait_deadlock]
       (no thread arrives; waiters sleep forever) *)
type t =
  | Missing_participants of {
      sync : Sync.t;
      witness : missing_witness;
      expected : int;
    }
  | Oversize_cohort of {
      sync : Sync.t;
      witness : oversize_witness;
      expected : int;
    }
  | Count_mismatch of { sync : Sync.t; n : int; m : int }
  | Incomplete_arrivals of {
      sync : Sync.t;
      witness : missing_witness;
      expected : int;
    }
  | Waiters_blocked of {
      sync : Sync.t;
      witness : missing_witness;
      expected : int;
    }
  | Standalone_wait_deadlock of { sync : Sync.t; expected : int }

let sync_of : t -> Sync.t = function
  | Missing_participants { sync; _ }
  | Oversize_cohort { sync; _ }
  | Count_mismatch { sync; _ }
  | Incomplete_arrivals { sync; _ }
  | Waiters_blocked { sync; _ }
  | Standalone_wait_deadlock { sync; _ } ->
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
  | Oversize_cohort { witness; expected; _ } ->
      let what =
        match witness with
        | Cohort_size_over n -> Printf.sprintf "got %d" n
        | Witness_threads ts ->
            let n = List.length ts in
            let sample_size = 3 in
            let sample, suffix =
              if n <= sample_size then (ts, "")
              else
                let take_n =
                  let rec aux acc k = function
                    | _ when k = 0 -> List.rev acc
                    | [] -> List.rev acc
                    | x :: rest -> aux (x :: acc) (k - 1) rest
                  in
                  aux [] sample_size ts
                in
                (take_n, ", ...")
            in
            let pretty =
              sample
              |> List.map (fun (x, y, z) -> Printf.sprintf "(%d,%d,%d)" x y z)
              |> String.concat ", "
            in
            Printf.sprintf "got at least %d distinct threads [%s%s]" n pretty
              suffix
      in
      Printf.sprintf "Oversize cohort (%s, expected %d)" what expected
  | Count_mismatch { n; m; _ } ->
      Printf.sprintf "Count mismatch on barrier id (counts %d, %d)" n m
  | Incomplete_arrivals { witness; expected; _ } ->
      let what =
        match witness with
        | Failing_thread { x; y; z } ->
            Printf.sprintf "thread tid=(%d,%d,%d) does not arrive" x y z
        | Cohort_size n -> Printf.sprintf "got %d" n
      in
      Printf.sprintf
        "Incomplete arrivals (%s, expected %d) — barrier contract \
         violated: any future wait deadlocks"
        what expected
  | Waiters_blocked { witness; expected; _ } ->
      let what =
        match witness with
        | Failing_thread { x; y; z } ->
            Printf.sprintf "thread tid=(%d,%d,%d) does not arrive" x y z
        | Cohort_size n -> Printf.sprintf "got %d" n
      in
      Printf.sprintf
        "Deadlock: waiters blocked on insufficient arrivals (%s, \
         expected %d)"
        what expected
  | Standalone_wait_deadlock { expected; _ } ->
      Printf.sprintf
        "Standalone wait deadlock (no thread arrives, expected %d)"
        expected

(* Internal arrival-side bug type, agnostic to whether the surrounding
   phase is sync-only or split-phase. The phase-shape dispatch wraps
   each value in the appropriate Diagnostic.t constructor. *)
type arrival_bug =
  | Insufficient of missing_witness
  | Excessive of oversize_witness

(* Detect bugs on the arrival cohort against the expected count. We
   split on three structural cases:

   1. [expected > threads_per_warp]. The cohort is bounded above by
      [threads_per_warp], so it can never reach [expected]. Statically
      insufficient — no SMT call needed.

   2. [expected = threads_per_warp]. Block-wide barrier (the
      __syncthreads case). Insufficient reduces to "is there a single
      thread in the block that fails the cohort?", answered by one
      SAT call over a single tid triple. Excessive is impossible
      (cohort ≤ threads_per_warp = expected).

   3. [expected < threads_per_warp]. Sub-warp named bar — cardinality
      matters. Excessive uses a small-distinctness SAT
      ([Thread_count.exceeds_cardinality]) with witness threads;
      Insufficient has no symmetric cheap encoding and is gated behind
      [--precise], which runs [refine_min]. Witness mode reports
      Excessive only. *)

let detect_arrival_bugs ?(mode = Witness) ?(timeout = 0) ~(pre : Exp.bexp)
    (cfg : Rel_cost.Config.t) (locals : Variable.Set.t) (p : Phase.t) :
    arrival_bug list =
  if p.count > cfg.threads_per_warp then
    [ Insufficient (Cohort_size cfg.threads_per_warp) ]
  else if p.count = cfg.threads_per_warp then
    match Thread_count.find_missing_thread ~timeout cfg ~pre p.arrive_cohort with
    | Some (x, y, z) -> [ Insufficient (Failing_thread { x; y; z }) ]
    | None -> []
  else
    let oversize =
      match
        Thread_count.exceeds_cardinality ~timeout cfg ~pre p.arrive_cohort
          p.count
      with
      | Some witnesses -> [ Excessive (Witness_threads witnesses) ]
      | None -> []
    in
    let missing =
      match mode with
      | Witness -> []
      | Precise -> (
          match
            Thread_count.refine_min ~timeout cfg locals p.arrive_cohort
          with
          | Some k when k < p.count -> [ Insufficient (Cohort_size k) ]
          | _ -> [])
    in
    oversize @ missing

(* Single-phase diagnosis. First decide whether the residual is
   sync-only ([b_a = b_w]), arrive-only ([b_w = ⊥]), wait-only
   ([b_a = ⊥]), or two-cohort ([b_a ≠ b_w], both non-⊥), then label the
   arrival-side bugs accordingly.

   Wait-only is its own diagnostic: no count check is meaningful when
   nothing arrives — the bug is the standalone wait, not its size. *)
let of_phase_solo ?(mode = Witness) ?(timeout = 0) ~(pre : Exp.bexp)
    (cfg : Rel_cost.Config.t) (locals : Variable.Set.t) (p : Phase.t) : t list =
  let arrive_empty = p.arrive_cohort = Exp.Bool false in
  let wait_empty = p.wait_cohort = Exp.Bool false in
  let is_split = p.arrive_cohort <> p.wait_cohort in
  if arrive_empty && not wait_empty then
    [ Standalone_wait_deadlock { sync = p.sync; expected = p.count } ]
  else
    let label : arrival_bug -> t = function
      | Excessive w ->
          Oversize_cohort
            { sync = p.sync; witness = w; expected = p.count }
      | Insufficient w ->
          if is_split then
            if wait_empty then
              Incomplete_arrivals
                { sync = p.sync; witness = w; expected = p.count }
            else
              Waiters_blocked
                { sync = p.sync; witness = w; expected = p.count }
          else
            Missing_participants
              { sync = p.sync; witness = w; expected = p.count }
    in
    detect_arrival_bugs ~mode ~timeout ~pre cfg locals p |> List.map label

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
