(* A type-safe way of manipulating indexes of different bases *)
type t = { base0 : int }

let zero : t = { base0 = 0 }
let from_base0 (x : int) : t = { base0 = x }
let from_base1 (x : int) : t = { base0 = x - 1 }
let to_base0 (x : t) : int = x.base0
let to_base1 (x : t) : int = x.base0 + 1
let add (x : t) (amount : int) : t = { base0 = x.base0 + amount }
let repr (x : t) : string = "{base0=" ^ string_of_int x.base0 ^ "}"
let min (x : t) (y : t) : t = { base0 = min x.base0 y.base0 }
let max (x : t) (y : t) : t = { base0 = max x.base0 y.base0 }

(* Returns the difference between both points.
    Guarantees that the value is non-negative. *)
let distance (x : t) (y : t) : int =
  Int.max x.base0 y.base0 - Int.min x.base0 y.base0
