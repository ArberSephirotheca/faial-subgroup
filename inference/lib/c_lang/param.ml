open Stage0
open Protocols
open Parse_util

type t = { ty_var : Ty_variable.t; is_used : bool; is_shared : bool }

let make ~ty_var ~is_used ~is_shared : t = { ty_var; is_used; is_shared }
let ty_var (x : t) : Ty_variable.t = x.ty_var
let name (x : t) : Variable.t = x.ty_var.name

let matches (type_of : Ty.t -> bool) (x : t) : bool =
  Ty_variable.matches type_of x.ty_var

let to_string (p : t) : string =
  let used = if p.is_used then "" else " unsed" in
  let shared = if p.is_shared then "shared " else "" in
  used ^ shared ^ Ty_variable.to_string p.ty_var

let parse (j : Yojson.Basic.t) : t Rjson.j_result =
  let open Rjson in
  let* o = cast_object j in
  let* name = parse_variable j in
  let* ty = get_field "type" o in
  let* is_refed = with_field_or "isReferenced" cast_bool false o in
  let* is_used = with_field_or "isUsed" cast_bool false o in
  let* is_shared = with_field_or "shared" cast_bool false o in

  let ty_var : Ty_variable.t =
    Ty_variable.make ~ty:(J_type.parse ty) ~name
  in
  Ok (make ~is_used:(is_refed || is_used) ~ty_var ~is_shared)
