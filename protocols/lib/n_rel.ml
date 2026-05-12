type t = Eq | Neq | Lt | ULt | Le | ULe | Gt | UGt | Ge | UGe

let eval : t -> int -> int -> bool = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Le -> ( <= )
  | ULe -> ( <= )
  | Ge -> ( >= )
  | UGe -> ( >= )
  | Lt -> ( < )
  | ULt -> ( < )
  | Gt -> ( > )
  | UGt -> ( > )

let to_string : t -> string = function
  | Eq -> "=="
  | Le -> "<="
  | ULe -> "<=u"
  | Lt -> "<"
  | ULt -> "<u"
  | Ge -> ">="
  | UGe -> ">=u"
  | Gt -> ">"
  | UGt -> ">u"
  | Neq -> "!="
