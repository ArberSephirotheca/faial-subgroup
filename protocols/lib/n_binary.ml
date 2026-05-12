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

let eval : t -> int -> int -> int = function
  | BitAnd -> ( land )
  | BitXOr -> ( lxor )
  | BitOr -> ( lor )
  | Plus _ -> ( + )
  | Minus _ -> ( - )
  | Mult _ -> ( * )
  | Div _ -> ( / )
  | Mod _ -> Common.modulo
  | LeftShift -> ( lsl )
  | RightShift _ -> ( lsr )

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
