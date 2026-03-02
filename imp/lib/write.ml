open Protocols

type t = { array : Variable.t; index : Exp.nexp list; payload : int option }

let to_access (w : t) : Access.t = Access.write w.array w.index w.payload
