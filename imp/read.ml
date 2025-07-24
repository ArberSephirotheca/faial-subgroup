open Protocols

type t = {
  target : (C_type.t * Variable.t) option;
  array : Variable.t;
  index : Exp.nexp list;
}

let to_access (r : t) : Access.t = Access.read r.array r.index
