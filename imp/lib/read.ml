open Protocols

type t = {
  target : (Ty.t * Variable.t) option;
  array : Variable.t;
  selector : Exp.nexp list;
  index : Exp.nexp list;
  guard : Exp.bexp option;
}

let to_access (r : t) : Access.t = Access.read r.array r.index
