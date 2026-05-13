(* Integer counters for faial-genie's [--json] output.

   Phase_timer records wall-clock per phase. The questions this
   complements it on — how many CTI rounds before convergence, what
   was the abductive pool's cardinality, whether the blanket fallback
   fired — are counts, not durations. Kept as a thin Hashtbl + insertion
   order mirror so the JSON ordering matches the order counters first
   appeared. *)

let counts : (string, int) Hashtbl.t = Hashtbl.create 16
let order : string list ref = ref []

let set (name : string) (value : int) : unit =
  if not (Hashtbl.mem counts name) then order := name :: !order;
  Hashtbl.replace counts name value

let incr ?(by = 1) (name : string) : unit =
  match Hashtbl.find_opt counts name with
  | None -> Hashtbl.add counts name by; order := name :: !order
  | Some v -> Hashtbl.replace counts name (v + by)

let to_json () : Yojson.Basic.t =
  `Assoc
    (!order
     |> List.rev
     |> List.map (fun n -> (n, `Int (Hashtbl.find counts n))))

let reset () : unit =
  Hashtbl.clear counts;
  order := []
