open Protocols

type t = {
  target : Variable.t;
  ty : Ty.t;
  atomic : Exp.nexp Atomic.t;
  array : Variable.t;
  selector : Exp.nexp list;
  index : Exp.nexp list;
  guard : Exp.bexp option;
}

let to_access (a : t) : Access.t =
  Access.make ~array:a.array ~index:a.index ~mode:(Atomic a.atomic)
