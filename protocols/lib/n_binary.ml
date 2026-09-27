open Stage0

type t =
  | BitOr
  | BitXOr
  | BitAnd
  | LeftShift
  | RightShift of Signedness.t
  | Plus of Signedness.t
  | Minus of Signedness.t
  | Mult of Signedness.t
  | Div of Signedness.t
  | Mod of Signedness.t

(* An unsigned right shift reads its left operand at the source type's width,
   and [int] carries no width. Every width agrees with OCaml's for a
   non-negative operand, so that shift is answered; a negative one has no
   answer to give and [eval] declines rather than invent a width. *)
exception Unknown_width

exception Shift_amount_out_of_range

let eval : t -> int -> int -> int =
  let shift (f : int -> int -> int) (l : int) (r : int) : int =
    if r < 0 || r >= Sys.int_size then raise Shift_amount_out_of_range
    else f l r
  in
  function
  | BitAnd -> ( land )
  | BitXOr -> ( lxor )
  | BitOr -> ( lor )
  | Plus _ -> ( + )
  | Minus _ -> ( - )
  | Mult _ -> ( * )
  | Div _ -> ( / )
  | Mod _ -> Common.modulo
  | LeftShift -> shift ( lsl )
  | RightShift Signed -> shift ( asr )
  | RightShift Unsigned ->
      shift (fun l r -> if l < 0 then raise Unknown_width else l lsr r)

let to_string : t -> string = function
  | Plus s -> "+" ^ Signedness.suffix s
  | Minus s -> "-" ^ Signedness.suffix s
  | Mult s -> "*" ^ Signedness.suffix s
  | Div s -> "/" ^ Signedness.suffix s
  | Mod s -> "%" ^ Signedness.suffix s
  | LeftShift -> "<<"
  | RightShift s -> ">>" ^ Signedness.suffix s
  | BitXOr -> "^"
  | BitOr -> "|"
  | BitAnd -> "&"
