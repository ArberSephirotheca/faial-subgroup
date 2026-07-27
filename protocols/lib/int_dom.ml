module Size = Size

type t = { size : Size.t; signed : bool }

let to_string (x : t) : string =
  let s = if x.signed then "" else "unsigned " in
  s ^ Size.to_string x.size

let signed_char_bounds = Bounds.between (-128) 127
let unsigned_char_bounds = Bounds.between 0 255
let signed_short_bounds = Bounds.between (-32768) 32767
let unsigned_short_bounds = Bounds.between 0 65535
let signed_int_bounds = Bounds.between (-2147483648) 2147483647
let unsigned_int_bounds = Bounds.between 0 4294967295

(* Neither 64-bit end is an OCaml [int]: [max_int] is 2^62 - 1, short of
   both 2^63 - 1 and 2^64 - 1. Every end that can be written is narrower
   than the true one, and a narrow end asserted of a variable rules out a
   value the domain admits, so these two state only what they can. *)
let signed_long_bounds = Bounds.unbounded
let unsigned_long_bounds = Bounds.at_least 0

let signed_char : t = { size = Bit8; signed = true }
let unsigned_char : t = { size = Bit8; signed = false }
let signed_short : t = { size = Bit16; signed = true }
let unsigned_short : t = { size = Bit16; signed = false }
let signed_int : t = { size = Bit32; signed = true }
let unsigned_int : t = { size = Bit32; signed = false }
let signed_long : t = { size = Bit64; signed = true }
let unsigned_long : t = { size = Bit64; signed = false }

let to_bounds (d : t) : Bounds.t =
  match (d.size, d.signed) with
  | Bit8, true -> signed_char_bounds
  | Bit8, false -> unsigned_char_bounds
  | Bit16, true -> signed_short_bounds
  | Bit16, false -> unsigned_short_bounds
  | Bit32, true -> signed_int_bounds
  | Bit32, false -> unsigned_int_bounds
  | Bit64, true -> signed_long_bounds
  | Bit64, false -> unsigned_long_bounds
