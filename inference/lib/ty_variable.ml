open Protocols

(* Typed variable *)

type t = { name : Variable.t; ty : Ty.t }

let make ~name ~ty : t = { name; ty }
let name (x : t) : Variable.t = x.name
let is_tid (x : t) : bool = x.name |> Variable.is_tid
let ty (x : t) : Ty.t = x.ty

let to_string (x : t) : string =
  Ty.to_string x.ty ^ " " ^ Variable.name x.name

let matches (pred : Ty.t -> bool) (x : t) : bool = pred x.ty
