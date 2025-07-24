open Stage0
open Protocols
(*
 Represents a c-to-json serialization of a data-type.
 *)

type t = Yojson.Basic.t

let from_json (j : Yojson.Basic.t) : t = j
let from_string (name : string) : t = `Assoc [ ("qualType", `String name) ]
let from_c_type (c : C_type.t) : t = from_string (C_type.to_string c)
let int = from_string "int"
let char = from_string "char"
let bool = from_string "bool"
let float = from_string "float"
let void = from_string "void"
let unknown = from_string "?"

type 'a j_result = 'a Rjson.j_result

let to_c_type_res (j : t) : C_type.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* ty = with_field "qualType" cast_string o in
  Ok (C_type.make ty)

let to_c_type ?(default = C_type.unknown) (j : t) : C_type.t =
  j |> to_c_type_res |> Result.value ~default

let matches (f : C_type.t -> bool) (j : t) : bool =
  j |> to_c_type_res |> Result.map f |> Result.value ~default:false

let to_string (j : t) : string = j |> to_c_type |> C_type.to_string
