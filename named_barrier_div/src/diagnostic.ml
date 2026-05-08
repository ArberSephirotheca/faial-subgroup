(* Pretty-printing for tier 2 diagnostics. Kept separate from
   [Tier2.diagnostic] so the analysis core stays free of formatting. *)

open Protocols

let task_to_string (t : Task.t) : string =
  let pi = Exp.b_to_string t.pi in
  let delta = Exp.b_to_string t.delta in
  Printf.sprintf "{ pi=%s; delta=%s }" pi delta

let to_string (d : Tier2.diagnostic) : string =
  match d with
  | Tier2.Bd_failure { id; group; witness } ->
      Printf.sprintf
        "bd failure at barrier %s (cohort size %d): %s"
        (Barriers.Id.to_string id) (List.length group) witness.reason
  | Tier2.Stuck parked ->
      let ids =
        List.map
          (fun t ->
            match Tier1.head_split t.Task.residual with
            | Some (Code.Sync s, _) ->
                Barriers.Id.to_string (Barriers.Id.of_sync s)
            | _ -> "<not-a-sync>")
          parked
      in
      Printf.sprintf
        "stuck: no barrier id is fireable. Remaining cohorts at: %s"
        (String.concat ", " ids)
