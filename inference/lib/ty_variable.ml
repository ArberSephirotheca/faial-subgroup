open Protocols

(* Typed variable *)

type t = { name : Variable.t; ty : J_type.t }

let make ~name ~ty : t = { name; ty }
let name (x : t) : Variable.t = x.name
let is_tid (x : t) : bool = x.name |> Variable.is_tid
let ty (x : t) : J_type.t = x.ty

let to_string (x : t) : string =
  J_type.to_string x.ty ^ " " ^ Variable.name x.name

let matches (pred : C_type.t -> bool) (x : t) : bool =
  J_type.to_c_type_res x.ty |> Result.map pred |> Result.value ~default:false
