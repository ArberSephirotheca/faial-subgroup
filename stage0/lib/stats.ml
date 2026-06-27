(* Integer counters for the [--json] output of the analysis tools.

   Phase_timer records wall-clock per phase. The questions this
   complements it on — how many CTI rounds before convergence, what
   was the abductive pool's cardinality, which delinearization engine
   produced each committed shape — are counts, not durations. Kept as
   a thin Hashtbl emitted in lexicographic key order so byte-level JSON
   diffs across runs are stable regardless of which counter was bumped
   first. *)

let counts : (string, int) Hashtbl.t = Hashtbl.create 16

(* When [FAIAL_STATS_LOG] is set (and not "0"/empty), every [set] /
   [incr] mirrors the post-update value to stderr immediately, mirroring
   Phase_timer's FAIAL_PHASE_LOG mode. Useful when the [--json] envelope
   never gets written because the process is killed by an external
   timeout: counters that landed before the kill survive on stderr. *)
let log_enabled : bool =
  match Sys.getenv_opt "FAIAL_STATS_LOG" with
  | None | Some "" | Some "0" -> false
  | _ -> true

let log (name : string) (value : int) : unit =
  if log_enabled then begin
    Printf.eprintf "[stat] %s = %d\n" name value;
    flush stderr
  end

let set (name : string) (value : int) : unit =
  Hashtbl.replace counts name value;
  log name value

let incr ?(by = 1) (name : string) : unit =
  let v =
    match Hashtbl.find_opt counts name with
    | None -> Hashtbl.add counts name by; by
    | Some v -> let v' = v + by in Hashtbl.replace counts name v'; v'
  in
  log name v

let to_json () : Yojson.Basic.t =
  `Assoc
    (Hashtbl.fold (fun n v acc -> (n, v) :: acc) counts []
     |> List.sort (fun (a, _) (b, _) -> String.compare a b)
     |> List.map (fun (n, v) -> (n, `Int v)))

let reset () : unit =
  Hashtbl.clear counts
