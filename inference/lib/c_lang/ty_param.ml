open Stage0
open Protocols
open Location_parser
open Parse_util

type t =
  | TemplateType of Variable.t
  | NonTypeTemplate of { name : Variable.t; ty : Ty.t }

let to_string (p : t) : string =
  let name =
    match p with TemplateType x -> x | NonTypeTemplate x -> x.name
  in
  Variable.name name

let name : t -> Variable.t = function
  | TemplateType x -> x
  | NonTypeTemplate x -> x.name

let parse (j : Yojson.Basic.t) : t option j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  match k with
  | "TemplateTypeParmDecl" ->
      let* name = parse_variable j in
      Ok (Some (TemplateType name))
  | "NonTypeTemplateParmDecl" ->
      (* Anonymous SFINAE template parameters (e.g.
         [typename std::enable_if<...>::type = 0]) have no name field;
         synthesize one from depth/index since the parameter is never
         referenced from the function body. *)
      let* name =
        match parse_variable j with
        | Ok v -> Ok v
        | Error _ ->
            let* depth = with_field_or "depth" cast_int 0 o in
            let* index = with_field_or "index" cast_int 0 o in
            let* location = with_field "range" parse_location o in
            let name =
              Printf.sprintf "__anon_nttp_%d_%d" depth index
            in
            Ok (Variable.make ~name ~location ())
      in
      let* ty = get_field "type" o in
      Ok (Some (NonTypeTemplate { name; ty = J_type.parse ty }))
  | _ -> Ok None
