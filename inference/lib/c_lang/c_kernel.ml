open Protocols
open Stage0
open Location_parser
open Ast
open Parse_util

module KernelAttr = Kernel_attr
module TemplateArgument = Template_argument

type t = {
  name : string;
  ty : string;
  code : Stmt.t;
  type_params : Ty_param.t list;
  params : Param.t list;
  attribute : KernelAttr.t;
  template_args : TemplateArgument.t list;
  specialization_kind : Specialization_kind.t option;
  primary_template_name : string option;
  location : Location.t;
}

let make ~ty ~name ~code ~type_params ~params ~attribute
    ~template_args ~specialization_kind ~primary_template_name ~location =
  {
    name;
    ty;
    code;
    type_params;
    params;
    attribute;
    template_args;
    specialization_kind;
    primary_template_name;
    location;
  }

let name (x : t) : string = x.name
let params (x : t) : Param.t list = x.params
let type_params (x : t) : Ty_param.t list = x.type_params
let attribute (x : t) : KernelAttr.t = x.attribute
let template_args (x : t) : TemplateArgument.t list = x.template_args
let location (x : t) : Location.t = x.location

let specialization_kind (x : t) : Specialization_kind.t option =
  x.specialization_kind

let primary_template_name (x : t) : string option = x.primary_template_name
let is_specialization (x : t) : bool = x.specialization_kind <> None

let rewrite_comma (k : t) : t =
  (* Run StmtExpr hoisting before comma rewriting: by the time
     [Stmt.rewrite_comma] sees the kernel body, every [StmtExpr]
     node has been replaced by its hoisted prefix decls plus a
     residual expression, so the rest of the pipeline never has to
     know about GCC statement expressions. *)
  let code = Rewrite_stmt_expr.run k.code in
  { k with code = Stmt.rewrite_comma code }

let rewrite_barriers (k : t) : t =
  { k with code = Stmt.rewrite_barriers k.code }

let is_global (k : t) : bool = KernelAttr.is_global k.attribute

let to_s (k : t) : Indent.t list =
  let tps =
    if k.type_params <> [] then
      "[" ^ list_to_s Ty_param.to_string k.type_params ^ "]"
    else ""
  in
  let targs =
    if k.template_args <> [] then
      "<" ^ list_to_s TemplateArgument.to_string k.template_args ^ ">"
    else ""
  in
  let open Indent in
  [
    Line
      (KernelAttr.to_string k.attribute
      ^ " " ^ k.name ^ targs ^ " " ^ tps ^ "("
      ^ list_to_s Param.to_string k.params
      ^ ") {");
    Block (Stmt.to_s k.code);
    Line "}";
  ]

let wrap_error (msg : string) (j : Yojson.Basic.t) :
    'a j_result -> 'a j_result = function
  | Ok e -> Ok e
  | Error e -> Rjson.because msg j e

let parse (type_params : Ty_param.t list) (j : Yojson.Basic.t) : t j_result =
  let open Rjson in
  (let* o = cast_object j in
   let* ty = get_field "type" o |> Result.map J_type.parse in
   let ty = Ty.to_string ty in
   let* inner = with_field "inner" cast_list o in
   let attrs, inner =
     inner |> List.partition (j_filter_kind (String.ends_with ~suffix:"Attr"))
   in
   let ps, body =
     inner
     |> List.partition
          (j_filter_kind (fun k ->
               k = "ParmVarDecl" || k = "TemplateArgument"))
   in
   let* attrs = map parse_attr attrs in
   let m : KernelAttr.t =
     List.find_map KernelAttr.parse attrs |> Option.get
   in
   let* body : Stmt.t = Stmt.parse_list (`List body) in
   let* name : string = with_field "name" cast_string o in
   let ps = List.map Param.parse ps |> List.concat_map Result.to_list in
   let* template_args =
     with_field_or "templateArgs" (cast_map Parsers.parse_c_template_argument)
       [] o
   in
   let* spec_kind_str = with_opt_field "specializationKind" cast_string o in
   let specialization_kind =
     Option.bind spec_kind_str Specialization_kind.parse
   in
   let* primary_template_name =
     with_opt_field "primaryTemplate"
       (fun pj ->
         let* po = cast_object pj in
         with_field "name" cast_string po)
       o
   in
   let location =
     with_field "range" parse_location o
     |> Result.value ~default:Location.empty
   in
   Ok
     (make ~ty ~name ~code:body ~params:ps ~type_params ~attribute:m
        ~template_args ~specialization_kind ~primary_template_name
        ~location))
  |> wrap_error "Kernel" j
