type t = BitNot | Negate

let eval : t -> int -> int = function
  | BitNot -> Int.neg
  | Negate -> fun x -> -x

let to_string : t -> string = function BitNot -> "~" | Negate -> "-"
