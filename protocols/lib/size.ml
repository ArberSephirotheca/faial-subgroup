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

let bytes : t -> int = function
  | Bit8 -> 1
  | Bit16 -> 2
  | Bit32 -> 4
  | Bit64 -> 8

let of_bytes : int -> t option = function
  | 1 -> Some Bit8
  | 2 -> Some Bit16
  | 4 -> Some Bit32
  | 8 -> Some Bit64
  | _ -> None
