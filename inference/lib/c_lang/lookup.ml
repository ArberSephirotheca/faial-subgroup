open Stage0
open Protocols
open Parse_util
module Function_id = Imp.Function_id

type t = { qualifier : Ty.segment list; name : string; ty : string option }

let parse (j : Yojson.Basic.t) : t option j_result =
  let open Rjson in
  let* o = cast_object j in
  let* name = with_opt_field "name" cast_string o in
  match name with
  | Some name ->
      let ty =
        get_signature_type o
        |> Result.map (fun j -> Ty.to_string (J_type.parse j))
        |> Result.to_option
      in
      Ok (Some { qualifier = J_type.qualifier o; name; ty })
  | None -> Ok None

let matches (id : Function_id.t) (x : t) : bool =
  Function_id.name id = x.name
  &&
  match x.ty with
  | Some ty ->
      Function_id.ty id = ty && Function_id.qualifier id = x.qualifier
  | None -> true
