type t = Eq | Neq | Lt | ULt | Le | Gt | Ge | UGe

let eval : t -> int -> int -> bool = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Le -> ( <= )
  | Ge -> ( >= )
  | UGe -> ( >= )
  | Lt -> ( < )
  | ULt -> ( < )
  | Gt -> ( > )

let to_string : t -> string = function
  | Eq -> "=="
  | Le -> "<="
  | Lt -> "<"
  | ULt -> "<u"
  | Ge -> ">="
  | UGe -> ">=u"
  | Gt -> ">"
  | Neq -> "!="
