open Protocols

type t = {
  target : Variable.t;
  ty : C_type.t;
  atomic : Exp.nexp Atomic.t;
  array : Variable.t;
  index : Exp.nexp list;
  guard : Exp.bexp option;
}

let to_access (a : t) : Access.t =
  Access.{ array = a.array; index = a.index; mode = Atomic a.atomic }
