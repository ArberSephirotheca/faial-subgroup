(* ⟨L; R⟩ — the state pair.

   [live]:   tasks reducible by tier 1.
   [parked]: tasks blocked at a sync head, awaiting rendezvous.

   The empty state is ⟨0; 0⟩ = { live = []; parked = [] }; the
   parallel composition ‖ is list concatenation; the operations are
   commutative and associative since order is not observable. *)

type t = { live : Task.t list; parked : Task.t list }

let empty : t = { live = []; parked = [] }

let live (t : Task.t) : t = { live = [ t ]; parked = [] }
let parked (t : Task.t) : t = { live = []; parked = [ t ] }

let union (a : t) (b : t) : t =
  { live = a.live @ b.live; parked = a.parked @ b.parked }

let is_empty (s : t) : bool =
  s.live = [] && s.parked = []
