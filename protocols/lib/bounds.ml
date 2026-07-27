(* The range of an integer domain, with either end allowed to be absent.
   An absent end means the domain admits values that no OCaml [int] can
   name, so there is no number to state it with. *)

type t = { lower : int option; upper : int option }

let unbounded : t = { lower = None; upper = None }

let between (lower : int) (upper : int) : t =
  { lower = Some lower; upper = Some upper }

let at_least (lower : int) : t = { lower = Some lower; upper = None }

let contains (n : int) (x : t) : bool =
  Option.fold ~none:true ~some:(fun lo -> lo <= n) x.lower
  && Option.fold ~none:true ~some:(fun hi -> n <= hi) x.upper
