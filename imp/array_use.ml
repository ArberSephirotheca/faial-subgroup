open Protocols

type t = { array : Variable.t; offset : Exp.nexp }

let make ?(offset = Exp.Num 0) (array : Variable.t) : t = { array; offset }

let to_string (l : t) : string =
  "&" ^ Variable.name l.array ^ "[" ^ Exp.n_to_string l.offset ^ "]"
