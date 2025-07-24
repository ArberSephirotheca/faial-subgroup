module Size = struct
  type t =
    | Bit8 (* char *)
    | Bit16 (* short *)
    | Bit32 (* int *)
    | Bit64 (* long *)

  let to_string : t -> string = function
    | Bit8 -> "char"
    | Bit16 -> "short"
    | Bit32 -> "int"
    | Bit64 -> "long"
end

type t = { size : Size.t; signed : bool }

let to_string (x : t) : string =
  let s = if x.signed then "" else "unsigned " in
  s ^ Size.to_string x.size

let signed_char_range = (-127, 127)
let unsigned_char_range = (0, 255)
let signed_short_range = (-32767, 32767)
let unsigned_short_range = (0, 65535)
let signed_int_range = (-2147483648, 2147483647)
let unsigned_int_range = (0, 4294967295)
(* let signed_long_range = (-9223372036854775808L, 9223372036854775807L) *)
(* let unsigned_long_range = (0L, 18446744073709551615L) *)

let signed_char : t = { size = Bit8; signed = true }
let unsigned_char : t = { size = Bit8; signed = false }
let signed_short : t = { size = Bit16; signed = true }
let unsigned_short : t = { size = Bit16; signed = false }
let signed_int : t = { size = Bit32; signed = true }
let unsigned_int : t = { size = Bit32; signed = false }
let signed_long : t = { size = Bit64; signed = true }
let unsigned_long : t = { size = Bit64; signed = false }

let to_range (d : t) : int * int =
  match (d.size, d.signed) with
  | Bit8, true -> signed_char_range
  | Bit8, false -> unsigned_char_range
  | Bit16, true -> signed_short_range
  | Bit16, false -> unsigned_short_range
  | Bit32, true -> signed_int_range
  | Bit32, false -> unsigned_int_range
  | Bit64, true -> signed_int_range
  | Bit64, false -> unsigned_int_range

let to_bexp (x : Variable.t) (d : t) : Exp.bexp =
  let lb, ub = to_range d in
  let open Exp in
  b_and (n_le (Num lb) (Var x)) (n_le (Var x) (Num ub))
