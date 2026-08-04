open Protocols

type t = {
  target : Variable.t;
  ty : Ty.t;
  atomic : Exp.nexp Atomic.t;
  path : Exp.nexp Field_path.t;
  index : Exp.nexp list;
  guard : Exp.bexp option;
}

let array (a : t) : Variable.t = Field_path.to_variable a.path

let to_access (a : t) : Access.t =
  Access.make ~array:(array a) ~index:a.index ~mode:(Atomic a.atomic)
