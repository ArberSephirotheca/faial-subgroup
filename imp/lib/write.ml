open Protocols

type t = {
  path : Exp.nexp Field_path.t;
  index : Exp.nexp list;
  payload : int option;
  guard : Exp.bexp option;
}

let array (w : t) : Variable.t = Field_path.to_variable w.path
let to_access (w : t) : Access.t = Access.write (array w) w.index w.payload
