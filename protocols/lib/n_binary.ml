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
  | Minus of Signedness.t
  | Mult
  | UMult
  | Div of Signedness.t
  | Mod of Signedness.t

let eval : t -> int -> int -> int = function
  | BitAnd -> ( land )
  | BitXOr -> ( lxor )
  | BitOr -> ( lor )
  | Plus -> ( + )
  | UPlus -> ( + )
  | Minus _ -> ( - )
  | Mult -> ( * )
  | UMult -> ( * )
  | Div _ -> ( / )
  | Mod _ -> Common.modulo
  | LeftShift -> ( lsl )
  | RightShift -> ( lsr )
  | URightShift -> ( lsr )

let to_string : t -> string = function
  | Plus -> "+"
  | UPlus -> "+u"
  | Minus s -> "-" ^ Signedness.suffix s
  | Mult -> "*"
  | UMult -> "*u"
  | Div s -> "/" ^ Signedness.suffix s
  | Mod s -> "%" ^ Signedness.suffix s
  | LeftShift -> "<<"
  | RightShift -> ">>"
  | URightShift -> ">>u"
  | BitXOr -> "^"
  | BitOr -> "|"
  | BitAnd -> "&"
