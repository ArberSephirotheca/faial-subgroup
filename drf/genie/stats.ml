(* Integer counters for faial-genie's [--json] output.

   Phase_timer records wall-clock per phase. The questions this
   complements it on — how many CTI rounds before convergence, what
   was the abductive pool's cardinality, whether the blanket fallback
   fired — are counts, not durations. Kept as a thin Hashtbl emitted
   in lexicographic key order so byte-level JSON diffs across runs
   are stable regardless of which counter was bumped first. *)

let counts : (string, int) Hashtbl.t = Hashtbl.create 16

let set (name : string) (value : int) : unit =
  Hashtbl.replace counts name value

let incr ?(by = 1) (name : string) : unit =
  match Hashtbl.find_opt counts name with
  | None -> Hashtbl.add counts name by
  | Some v -> Hashtbl.replace counts name (v + by)

let to_json () : Yojson.Basic.t =
  `Assoc
    (Hashtbl.fold (fun n v acc -> (n, v) :: acc) counts []
     |> List.sort (fun (a, _) (b, _) -> String.compare a b)
     |> List.map (fun (n, v) -> (n, `Int v)))

let reset () : unit =
  Hashtbl.clear counts
