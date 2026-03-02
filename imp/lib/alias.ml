open Protocols

type t = { source : Variable.t; target : Variable.t; offset : Exp.nexp }

let is_trivial (a : t) : bool =
  Variable.equal a.source a.target && a.offset = Num 0

let to_string (l : t) : string =
  Variable.name l.target ^ " = " ^ Variable.name l.source ^ " + "
  ^ Exp.n_to_string l.offset ^ ";"
