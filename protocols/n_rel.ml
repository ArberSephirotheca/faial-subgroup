type t = Eq | Neq | Lt | Le | Gt | Ge

let eval : t -> int -> int -> bool = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Le -> ( <= )
  | Ge -> ( >= )
  | Lt -> ( < )
  | Gt -> ( > )

let to_string : t -> string = function
  | Eq -> "=="
  | Le -> "<="
  | Lt -> "<"
  | Ge -> ">="
  | Gt -> ">"
  | Neq -> "!="
