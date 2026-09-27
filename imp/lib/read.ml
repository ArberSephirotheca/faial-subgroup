open Protocols

type t = {
  target : (Ty.t * Variable.t) option;
  path : Exp.nexp Field_path.t;
  index : Exp.nexp list;
  guard : Exp.bexp option;
}

let array (r : t) : Variable.t = Field_path.to_variable r.path
let to_access (r : t) : Access.t = Access.read (array r) r.index
