type t = Signed | Unsigned

let is_signed : t -> bool = function Signed -> true | Unsigned -> false
let is_unsigned : t -> bool = function Unsigned -> true | Signed -> false

(* Suffix appended to an operator's textual rendering. Signed is the
   default and prints without a suffix; unsigned prints "u". *)
let suffix : t -> string = function Signed -> "" | Unsigned -> "u"
