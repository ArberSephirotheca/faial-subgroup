open Stage0
open Protocols

(*
 Parses a c-to-json serialization of a data-type into [Ty.t].

 The [type] field takes two shapes: an object carrying [qualType] and
 optionally [desugaredQualType], or a bare string such as ["void"],
 ["uint3"] or ["T"]. Anything unrecognised reaches [Ty.Opaque], so this
 never fails.
 *)

let template_args (o : (string * Yojson.Basic.t) list) : string list =
  let open Rjson in
  let arg (j : Yojson.Basic.t) : string =
    match cast_object j with
    | Error _ -> "?"
    | Ok o ->
        let field (name : string) : string option =
          match List.assoc_opt name o with
          | Some (`String s) -> Some s
          | Some (`Int i) -> Some (string_of_int i)
          | Some (`Bool b) -> Some (string_of_bool b)
          | Some j -> Some (Yojson.Basic.to_string j)
          | None -> None
        in
        List.find_map field [ "type"; "value"; "name" ]
        |> Option.value ~default:"?"
  in
  match with_opt_field "templateArgs" cast_list o with
  | Ok (Some l) -> List.map arg l
  | Ok None | Error _ -> []

let specialization ~(name : string) ~(qualifier : string list)
    ~(args : string list) : string =
  String.concat "::" (qualifier @ [ name ])
  ^ if args = [] then "" else "<" ^ String.concat ", " args ^ ">"

let qualifier (o : (string * Yojson.Basic.t) list) : string list option =
  let open Rjson in
  let part (j : Yojson.Basic.t) : string =
    match cast_object j with
    | Error _ -> "?"
    | Ok o -> (
        match with_opt_field "name" cast_string o with
        | Ok (Some name) ->
            specialization ~name ~qualifier:[] ~args:(template_args o)
        | Ok None | Error _ -> "?")
  in
  match with_opt_field "qualifierParts" cast_list o with
  | Ok (Some l) -> Some (List.map part l)
  | Ok None | Error _ -> (
      match with_opt_field "qualifier" (cast_map cast_string) o with
      | Ok (Some q) -> Some q
      | Ok None | Error _ -> None)

let key (o : (string * Yojson.Basic.t) list) : string option =
  let open Rjson in
  match with_opt_field "name" cast_string o with
  | Ok (Some name) ->
      Some
        (specialization ~name
           ~qualifier:(Option.value (qualifier o) ~default:[])
           ~args:(template_args o))
  | Ok None | Error _ -> None

let parse (j : Yojson.Basic.t) : Ty.t =
  let open Rjson in
  match j with
  | `String s -> Ty.of_c_string s
  | _ -> (
      let written =
        let* o = cast_object j in
        let* q = with_field "qualType" cast_string o in
        let desugared =
          match key o with
          | Some k -> Some k
          | None ->
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
