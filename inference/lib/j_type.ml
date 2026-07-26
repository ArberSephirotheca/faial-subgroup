open Stage0
open Protocols

(*
 Parses a c-to-json serialization of a data-type into [Ty.t].

 The [type] field takes two shapes: an object carrying [qualType] and
 optionally [desugaredQualType], or a bare string such as ["void"],
 ["uint3"] or ["T"]. Anything unrecognised reaches [Ty.Opaque], so this
 never fails.
 *)

let parse (j : Yojson.Basic.t) : Ty.t =
  let open Rjson in
  match j with
  | `String s -> Ty.of_c_string s
  | _ -> (
      let written =
        let* o = cast_object j in
        let* q = with_field "qualType" cast_string o in
        let desugared =
          with_opt_field "desugaredQualType" cast_string o
          |> Result.value ~default:None
        in
        Ok (Ty.of_c_string ?desugared q)
      in
      match written with Ok ty -> ty | Error _ -> Ty.unknown)

let of_string (name : string) : Ty.t = Ty.of_c_string name
let int : Ty.t = Ty.int
let char : Ty.t = Ty.char
let bool : Ty.t = of_string "bool"
let float : Ty.t = of_string "float"
let void : Ty.t = Ty.void
let unknown : Ty.t = Ty.unknown
