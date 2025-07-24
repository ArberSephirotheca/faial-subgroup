open Stage0

type t =
  | BitOr
  | BitXOr
  | BitAnd
  | LeftShift
  | RightShift
  | Plus
  | Minus
  | Mult
  | Div
  | Mod

let eval : t -> int -> int -> int = function
  | BitAnd -> ( land )
  | BitXOr -> ( lxor )
  | BitOr -> ( lor )
  | Plus -> ( + )
  | Minus -> ( - )
  | Mult -> ( * )
  | Div -> ( / )
  | Mod -> Common.modulo
  | LeftShift -> ( lsl )
  | RightShift -> ( lsr )

let to_string : t -> string = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | LeftShift -> "<<"
  | RightShift -> ">>"
  | BitXOr -> "^"
  | BitOr -> "|"
  | BitAnd -> "&"
