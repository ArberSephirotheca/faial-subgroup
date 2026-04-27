open Protocols

(* Classification of stuck residual states into bug categories.
   Operates outside the rewrite system: takes a normal-form State.t
   and inspects each phase against the diagnostic table from
   documentation/barrier-participants.md. *)

type t =
  | Missing_participants of {
      sync : Sync.t;
      cohort_size : int;
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
  | Missing_participants { cohort_size; expected; _ } ->
      Printf.sprintf "Missing participants (got %d, expected %d)" cohort_size
        expected
  | Oversize_cohort { cohort_size; expected; _ } ->
      Printf.sprintf "Oversize cohort (got %d, expected %d)" cohort_size
        expected
  | Count_mismatch { n; m; _ } ->
      Printf.sprintf "Count mismatch on barrier id (counts %d, %d)" n m

(* Single-phase diagnosis: compare arrive cohort size against expected
   count. The min and max queries are evaluated independently so that
   one timing out does not suppress the diagnostic from the other —
   [mn < count] alone is conclusive evidence of missing participants,
   and [mx > count] alone is conclusive evidence of an oversize cohort. *)
let of_phase_solo ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (p : Phase.t) : t list =
  let mx = Thread_count.max_count ~timeout cfg locals p.arrive_cohort in
  let mn = Thread_count.min_count ~timeout cfg locals p.arrive_cohort in
  let oversize =
    match mx with
    | Some mx when mx > p.count ->
        [
          Oversize_cohort
            { sync = p.sync; cohort_size = mx; expected = p.count };
        ]
    | _ -> []
  in
  let missing =
    match mn with
    | Some mn when mn < p.count ->
        [
          Missing_participants
            { sync = p.sync; cohort_size = mn; expected = p.count };
        ]
    | _ -> []
  in
  oversize @ missing

(* Pairwise diagnosis: same id, different counts → Count_mismatch.
   Reported only once per ordered pair (later phase compared against
   earlier) to avoid duplicates. *)
let pair_diagnostics (earlier : Phase.t) (later : Phase.t) : t list =
  if Phase.phases_share_id earlier later && earlier.count <> later.count then
    [ Count_mismatch { sync = later.sync; n = earlier.count; m = later.count } ]
  else []

let of_state ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (s : State.t) : t list =
  let solo =
    s.phases |> List.concat_map (of_phase_solo ~timeout cfg locals)
  in
  let rec pairs (acc : t list) = function
    | [] -> acc
    | p :: rest ->
        let with_each = List.concat_map (pair_diagnostics p) rest in
        pairs (with_each @ acc) rest
  in
  let pairwise = pairs [] s.phases in
  solo @ pairwise
