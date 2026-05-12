type t =
  | Eq
  | Neq
  | Lt of Signedness.t
  | Le of Signedness.t
  | Gt of Signedness.t
  | Ge of Signedness.t

let eval : t -> int -> int -> bool = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Le _ -> ( <= )
  | Ge _ -> ( >= )
  | Lt _ -> ( < )
  | Gt _ -> ( > )

let to_string : t -> string = function
  | Eq -> "=="
  | Le s -> "<=" ^ Signedness.suffix s
  | Lt s -> "<" ^ Signedness.suffix s
  | Ge s -> ">=" ^ Signedness.suffix s
  | Gt s -> ">" ^ Signedness.suffix s
  | Neq -> "!="
