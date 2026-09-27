open Stage0
open Protocols

(*
 Parses a c-to-json serialization of a data-type into [Ty.t].

 The [type] field takes two shapes: an object carrying [qualType] and
 optionally [desugaredQualType], or a bare string such as ["void"],
 ["uint3"] or ["T"]. Anything unrecognised reaches [Ty.Opaque], so this
 never fails.
 *)

let rec parse (j : Yojson.Basic.t) : Ty.t =
  let open Rjson in
  match j with
  | `String s -> Ty.of_c_string s
  | _ -> (
      let written =
        let* o = cast_object j in
        let* q = with_field "qualType" cast_string o in
        match path o with
        | Some path -> Ok { (Ty.named path) with name = Some q }
        | None ->
            let desugared =
              with_opt_field "desugaredQualType" cast_string o
              |> Result.value ~default:None
            in
            Ok (Ty.of_c_string ?desugared q)
      in
      match written with Ok ty -> ty | Error _ -> Ty.unknown)

and path (o : (string * Yojson.Basic.t) list) : Ty.segment list option =
  let open Rjson in
  match with_opt_field "name" cast_string o with
  | Ok (Some base) ->
      Some (qualifier o @ [ { Ty.base; args = template_args o } ])
  | Ok None | Error _ -> None

and qualifier_opt (o : (string * Yojson.Basic.t) list) : Ty.segment list option =
  let open Rjson in
  let part (j : Yojson.Basic.t) : Ty.segment =
    match cast_object j with
    | Error _ -> Ty.segment "?"
    | Ok o -> (
        match with_opt_field "name" cast_string o with
        | Ok (Some base) -> { Ty.base; args = template_args o }
        | Ok None | Error _ -> Ty.segment "?")
  in
  match with_opt_field "qualifierParts" cast_list o with
  | Ok (Some l) -> Some (List.map part l)
  | Ok None | Error _ -> (
      match with_opt_field "qualifier" (cast_map cast_string) o with
      | Ok (Some q) -> Some (List.map Ty.segment q)
      | Ok None | Error _ -> None)

and qualifier (o : (string * Yojson.Basic.t) list) : Ty.segment list =
  Option.value (qualifier_opt o) ~default:[]

and template_args (o : (string * Yojson.Basic.t) list) : Ty.arg list =
  let open Rjson in
  let rec arg (j : Yojson.Basic.t) : Ty.arg =
    match cast_object j with
    | Error _ -> Ty.Unmodelled (Yojson.Basic.to_string j)
    | Ok o -> (
        let spelled (field : string) : string =
          match List.assoc_opt field o with
          | Some (`String s) -> s
          | Some (`Int i) -> string_of_int i
          | Some (`Bool b) -> string_of_bool b
          | Some j -> Yojson.Basic.to_string j
          | None -> ""
        in
        match with_opt_field "argKind" cast_string o with
        | Ok (Some "type") -> Ty.Type (parse (spelled_type o))
        | Ok (Some "integral") -> Ty.Integral (spelled "value")
        | Ok (Some "template") | Ok (Some "templateExpansion") ->
            Ty.Template (spelled "name")
        | Ok (Some "declaration") ->
            Ty.Declaration (declaration o)
        | Ok (Some "nullPtr") -> Ty.NullPtr
        | Ok (Some ("expression" | "structuralValue")) ->
            Ty.Expression (spelled "value")
        | Ok (Some "pack") -> (
            match with_opt_field "inner" cast_list o with
            | Ok (Some l) -> Ty.Pack (List.map arg l)
            | Ok None | Error _ -> Ty.Pack [])
        | Ok (Some k) -> Ty.Unmodelled (k ^ " " ^ Yojson.Basic.to_string j)
        | Ok None | Error _ -> Ty.Unmodelled (Yojson.Basic.to_string j))
  and spelled_type (o : (string * Yojson.Basic.t) list) : Yojson.Basic.t =
    List.assoc_opt "type" o |> Option.value ~default:(`String "?")
  and declaration (o : (string * Yojson.Basic.t) list) : string =
    match List.assoc_opt "decl" o with
    | Some (`Assoc d) -> (
        match List.assoc_opt "name" d with
        | Some (`String n) -> n
        | Some j -> Yojson.Basic.to_string j
        | None -> Yojson.Basic.to_string (`Assoc d))
    | Some j -> Yojson.Basic.to_string j
    | None -> "?"
  in
  match with_opt_field "templateArgs" cast_list o with
  | Ok (Some l) -> List.map arg l
  | Ok None | Error _ -> []

let of_string (name : string) : Ty.t = Ty.of_c_string name
let int : Ty.t = Ty.int
let char : Ty.t = Ty.char
let bool : Ty.t = of_string "bool"
let float : Ty.t = of_string "float"
let void : Ty.t = Ty.void
let unknown : Ty.t = Ty.unknown
