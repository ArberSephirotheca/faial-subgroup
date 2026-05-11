type t = Eq | Neq | Lt | ULt | Le | Gt | Ge

let eval : t -> int -> int -> bool = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Le -> ( <= )
  | Ge -> ( >= )
  | Lt -> ( < )
  | ULt -> ( < )
  | Gt -> ( > )

let to_string : t -> string = function
  | Eq -> "=="
  | Le -> "<="
  | Lt -> "<"
  | ULt -> "<u"
  | Ge -> ">="
  | Gt -> ">"
  | Neq -> "!="
