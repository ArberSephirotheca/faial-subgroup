open Protocols
open Stage0
open Location_parser
open Ast
open Parse_util

module KernelAttr = Kernel_attr
module TemplateArgument = Template_argument
module Function_id = Imp.Function_id

type t = {
  name : string;
  ty : string;
  return_ty : Ty.t;
  (* Enclosing namespaces, outermost first. Two functions of the same
     signature in different namespaces are different functions, and the
     name clang reports is unqualified. *)
  qualifier : Ty.segment list;
  (* Clang's identifier for this declaration, used to resolve a call
     site to the definition it names. *)
  decl_id : string option;
  code : Stmt.t;
  (* Whether the declaration carried a body. A prototype and a function
     defined with an empty body both parse to [code = Skip], so the code
     alone cannot tell them apart. *)
  has_body : bool;
  type_params : Ty_param.t list;
  params : Param.t list;
  attribute : KernelAttr.t;
  template_args : TemplateArgument.t list;
  specialization_kind : Specialization_kind.t option;
  primary_template_name : string option;
  location : Location.t;
}

let make ~ty ~return_ty ~name ~qualifier ~decl_id ~code ~has_body ~type_params
    ~params ~attribute ~template_args ~specialization_kind
    ~primary_template_name ~location =
  {
    name;
    ty;
    return_ty;
    qualifier;
    decl_id;
    code;
    has_body;
    type_params;
    params;
    attribute;
    template_args;
    specialization_kind;
    primary_template_name;
    location;
  }

let name (x : t) : string = x.name
let decl_id (x : t) : string option = x.decl_id
let return_ty (x : t) : Ty.t = x.return_ty

(* What separates this declaration from every other function. A
   redeclaration reaches the same value as its definition, which is what
   merges a prototype with the body it declares. *)
let id (x : t) : Function_id.t =
  Function_id.make ~qualifier:x.qualifier
    ~template_args:(List.map TemplateArgument.to_string x.template_args)
    ~name:x.name ~ty:x.ty ()
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
let has_body (k : t) : bool = k.has_body

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
  let quals =
    String.concat ""
      (List.map (fun q -> Ty.segment_to_string q ^ "::") k.qualifier)
  in
  let header =
    KernelAttr.to_string k.attribute
    ^ " " ^ quals ^ k.name ^ targs ^ " " ^ tps ^ "("
    ^ list_to_s Param.to_string k.params
    ^ ")"
  in
  let open Indent in
  if k.has_body then
    [ Line (header ^ " {"); Block (Stmt.to_s k.code); Line "}" ]
  else [ Line (header ^ ";") ]

let wrap_error (msg : string) (j : Yojson.Basic.t) :
    'a j_result -> 'a j_result = function
  | Ok e -> Ok e
  | Error e -> Rjson.because msg j e

(* A static method cannot name [this] and an instance method that touches a
   member does, implicitly when the source omits it, so the presence of the
   node decides which kind this is. cu-to-json emits no storage class, and
   an instance method that never reads its object needs no parameter for
   it either. *)
let rec mentions_this (j : Yojson.Basic.t) : bool =
  match j with
  | `Assoc o ->
      (match List.assoc_opt "kind" o with
       | Some (`String "CXXThisExpr") -> true
       | _ ->
           List.assoc_opt "inner" o
           |> Option.map mentions_this
           |> Option.value ~default:false)
  | `List l -> List.exists mentions_this l
  | _ -> false

(* The record a method reads its members through is the innermost scope it
   is qualified by. Given as a by-value parameter so that it expands into
   one parameter per member, which is the shape the object at the call site
   expands into. *)
let this_param (qualifier : Ty.segment list) : Param.t option =
  match qualifier with
  | [] -> None
  | path ->
      let ty_var = Ty_variable.make ~ty:(Ty.named path) ~name:this_var in
      Some (Param.make ~ty_var ~is_used:true ~is_shared:false)

let bind_closure ~(self : Ty.segment) ~(captures : (string * Ty.t) list)
    (k : t) : t =
  let closure = Ty.named [ self ] in
  let bound = List.to_seq captures |> Hashtbl.of_seq in
  let rewrite (e : Expr.t) : Expr.t =
    Expr.Visit.map
      (function
        | Expr.Ident v as e -> (
            match Hashtbl.find_opt bound (Variable.name (Decl_expr.name v)) with
            | Some ty ->
                Expr.MemberExpr
                  { name = Variable.name (Decl_expr.name v);
                    base = Expr.Ident (Decl_expr.from_name ~ty:closure this_var);
                    ty }
            | None -> e)
        | e -> e)
      e
  in
  let this =
    Param.make
      ~ty_var:(Ty_variable.make ~ty:closure ~name:this_var)
      ~is_used:true ~is_shared:false
  in
  { k with
    params = this :: k.params;
    code = Stmt.Visit.map_expr rewrite k.code;
    qualifier = [ self ] }

let parse ?(qualifier = []) (type_params : Ty_param.t list)
    (j : Yojson.Basic.t) : t j_result =
  let open Rjson in
  (let* o = cast_object j in
   let* ty = get_signature_type o |> Result.map J_type.parse in
   let ty = Ty.to_string ty in
   let* return_ty =
     with_opt_field "returnType" (fun j -> Ok (J_type.parse j)) o
   in
   let return_ty = Option.value return_ty ~default:Ty.unknown in
   (* The scopes cu-to-json computes from the semantic declaration
      context, which hold for a definition written out of line with a
      qualified name where the enclosing [NamespaceDecl] nodes, all the
      caller can offer, do not. Absent from a [.cjson] recorded before
      the field existed, and from a function at translation-unit
      scope. *)
   let qualifier =
     match J_type.qualifier_opt o with
     | Some qs -> qs
     | None -> qualifier
   in
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
   let has_body = body <> [] in
   let* body : Stmt.t = Stmt.parse_list (`List body) in
   let* name : string = with_field "name" cast_string o in
   let ps = List.map Param.parse ps |> List.concat_map Result.to_list in
   let ps =
     if mentions_this j then
       match this_param qualifier with Some p -> p :: ps | None -> ps
     else ps
   in
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
     (make ~ty ~return_ty ~name ~qualifier ~decl_id:(parse_decl_id o)
        ~code:body ~has_body
        ~params:ps ~type_params ~attribute:m ~template_args
        ~specialization_kind ~primary_template_name ~location))
  |> wrap_error "Kernel" j
