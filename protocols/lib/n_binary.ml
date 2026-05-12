open Stage0

type t =
  | BitOr
  | BitXOr
  | BitAnd
  | LeftShift
  | RightShift
  | URightShift
  | Plus
  | UPlus
  | Minus
  | Mult
  | UMult
  | Div of Signedness.t
  | Mod

let eval : t -> int -> int -> int = function
  | BitAnd -> ( land )
  | BitXOr -> ( lxor )
  | BitOr -> ( lor )
  | Plus -> ( + )
  | UPlus -> ( + )
  | Minus -> ( - )
  | Mult -> ( * )
  | UMult -> ( * )
  | Div _ -> ( / )
  | Mod -> Common.modulo
  | LeftShift -> ( lsl )
  | RightShift -> ( lsr )
  | URightShift -> ( lsr )

let to_string : t -> string = function
  | Plus -> "+"
  | UPlus -> "+u"
  | Minus -> "-"
  | Mult -> "*"
  | UMult -> "*u"
  | Div s -> "/" ^ Signedness.suffix s
  | Mod -> "%"
  | LeftShift -> "<<"
  | RightShift -> ">>"
  | URightShift -> ">>u"
  | BitXOr -> "^"
  | BitOr -> "|"
  | BitAnd -> "&"
