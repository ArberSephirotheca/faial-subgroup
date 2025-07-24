open Protocols

type t = {
  target : Variable.t;
  ty : C_type.t;
  atomic : Atomic.t;
  array : Variable.t;
  index : Exp.nexp list;
}

let to_access (a : t) : Access.t =
  Access.{ array = a.array; index = a.index; mode = Atomic a.atomic }
