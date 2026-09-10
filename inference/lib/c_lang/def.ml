open Stage0
open Protocols
open Location_parser
open Ast
open Parse_util
module KernelAttr = Kernel_attr
module LaunchParam = Launch_param

type t =
  | Kernel of C_kernel.t
  | Declaration of Decl.t
  | Typedef of Typedef.t
  | Enum of Imp.Enum.t
  | LaunchParam of LaunchParam.t

let remove_comma : t -> t = function
  | Kernel k -> Kernel (C_kernel.rewrite_comma k)
  | Declaration d -> Declaration (Decl.map_expr Expr.remove_comma d)
  | (Typedef _ | Enum _ | LaunchParam _) as d -> d

let rewrite_barriers : t -> t = function
  | Kernel k -> Kernel (C_kernel.rewrite_barriers k)
  | (Declaration _ | Typedef _ | Enum _ | LaunchParam _) as d -> d

let to_s (d : t) : Indent.t list =
  match d with
  | Declaration d -> Decl.to_s d
  | Kernel k -> C_kernel.to_s k
  | Typedef d -> Typedef.to_s d
  | Enum e -> Imp.Enum.to_s e
  | LaunchParam lp -> LaunchParam.to_s lp

let location : t -> Location.t = function
  | Declaration d -> Decl.location d
  | Kernel k -> C_kernel.location k
  | Typedef d -> Typedef.location d
  | Enum e -> Imp.Enum.location e
  | LaunchParam lp -> LaunchParam.location lp

let has_array_type (j : Yojson.Basic.t) : bool =
  let open Rjson in
  let is_array =
    let* o = cast_object j in
    let* ty = get_field "type" o |> Result.map J_type.from_json in
    Ok (J_type.matches C_type.is_array ty)
  in
  is_array |> Result.value ~default:false

let subgroup_method_callees =
  [
    "__ballot_sync";
    "__shfl_down_sync";
    "__shfl_sync";
    "__shfl_up_sync";
    "__shfl_xor_sync";
    "__syncwarp";
    "fill_fragment";
    "load_matrix_sync";
    "mma_sync";
    "store_matrix_sync";
    "warp_max";
    "warp_prefix_inclusive_sum";
    "warp_reduce_all";
    "warp_reduce_any";
    "warp_reduce_max";
    "warp_reduce_sum";
    "warp_sum";
  ]

let rec references_subgroup_method (j : Yojson.Basic.t) : bool =
  match j with
  | `Assoc fields ->
      List.exists
        (function
          | "name", `String name -> List.mem name subgroup_method_callees
          | _, child -> references_subgroup_method child)
        fields
  | `List children -> List.exists references_subgroup_method children
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ -> false

let supported_subgroup_method_names = [ "reduce" ]
let supported_subgroup_class_names = [ "block_reduce_policy" ]

let supported_subgroup_class (j : Yojson.Basic.t) : bool =
  match j with
  | `Assoc fields -> (
      match List.assoc_opt "name" fields with
      | Some (`String name) -> List.mem name supported_subgroup_class_names
      | Some _ | None -> false)
  | `List _ | `Null | `Bool _ | `Int _ | `Float _ | `String _ -> false

let is_kernel (j : Yojson.Basic.t) : bool =
  let open Rjson in
  let is_kernel =
    let* o = cast_object j in
    let* k = get_kind o in
    if k = "FunctionDecl" || k = "CXXMethodDecl" then
      let* inner = with_field "inner" cast_list o in
      let attrs, inner =
        inner
        |> List.partition (j_filter_kind (String.ends_with ~suffix:"Attr"))
      in
      let attrs =
        attrs
        |> List.filter_map (fun j ->
            parse_attr j
            >>= (fun a -> Ok (Some a))
            |> Result.value ~default:None)
      in
      let _params, _ =
        inner |> List.partition (j_filter_kind (fun k -> k = "ParmVarDecl"))
      in
      Ok
        (match List.find_map KernelAttr.parse attrs with
        | Some KernelAttr.Default -> true
        | None -> false
        | Some KernelAttr.Auxiliary -> true)
    else Ok false
  in
  is_kernel |> Result.value ~default:false

let subgroup_method_decl (j : Yojson.Basic.t) : bool =
  match j with
  | `Assoc fields -> (
      match List.assoc_opt "name" fields with
      | Some (`String name) ->
          List.mem name supported_subgroup_method_names
          && is_kernel j
          && references_subgroup_method j
      | Some _ | None -> false)
  | `List _ | `Null | `Bool _ | `Int _ | `Float _ | `String _ -> false

let rec contains_subgroup_method_decl (j : Yojson.Basic.t) : bool =
  match j with
  | `Assoc fields -> (
      match List.assoc_opt "kind" fields with
      | Some (`String "CXXMethodDecl") -> subgroup_method_decl j
      | Some
          (`String
             ( "ClassTemplateDecl" | "ClassTemplateSpecializationDecl"
             | "ClassTemplatePartialSpecializationDecl" | "CXXRecordDecl" )) ->
        begin
          match List.assoc_opt "inner" fields with
          | Some (`List children) ->
              List.exists contains_subgroup_method_decl children
          | Some _ | None -> false
        end
      | Some _ | None -> false)
  | `List _ | `Null | `Bool _ | `Int _ | `Float _ | `String _ -> false

let parse_constant (j : Yojson.Basic.t) : Imp.Enum.Constant.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* _ = expect_kind "EnumConstantDecl" o in
  let* var = parse_variable j in
  let rec parse_init (j : json) : int option j_result =
    let* o = cast_object j in
    let* k = get_kind o in
    if k = "ConstantExpr" then
      match with_opt_field "value" cast_string o with
      | Ok (Some s) -> (
          match int_of_string_opt s with
          | Some n -> Ok (Some n)
          | None -> root_cause ("ConstantExpr.value is not an integer: " ^ s) j)
      | _ -> root_cause "ConstantExpr without a pre-evaluated value" j
    else if List.mem k [ "ImplicitCastExpr"; "ParenExpr"; "CStyleCastExpr" ]
    then with_field "inner" (cast_list_1 parse_init) o
    else
      let* e = Expr.parse j in
      match e with
      | IntegerLiteral n -> Ok (Some n)
      | _ -> root_cause "Expecting an integer, but got something else" j
  in
  let is_doc_comment : json -> bool =
    j_filter_kind (fun k -> k = "FullComment")
  in
  let* init =
    with_field_or "inner"
      (fun j ->
        let* l = cast_list j in
        let l = List.filter (fun x -> not (is_doc_comment x)) l in
        match l with [] -> Ok None | _ -> cast_list_1 parse_init (`List l))
      None o
  in
  let open Imp.Enum.Constant in
  Ok { var; init }

let parse_enum (j : Yojson.Basic.t) : Imp.Enum.t j_result =
  let open Rjson in
  let open Imp.Enum in
  let* o = cast_object j in
  let is_constant : Yojson.Basic.t -> bool =
    j_filter_kind (fun k -> k = "EnumConstantDecl")
  in
  let* var =
    match parse_variable j with
    | Ok v -> Ok v
    | Error _ -> (
        let* location = with_field "range" parse_location o in
        let name =
          let open Yojson.Basic.Util in
          let inner =
            List.assoc_opt "inner" o |> Option.value ~default:(`List [])
          in
          let consts =
            match inner with `List l -> List.filter is_constant l | _ -> []
          in
          match consts with
          | first :: _ ->
              first |> member "type" |> J_type.from_json |> J_type.to_c_type_res
              |> Result.to_option
              |> Option.map C_type.to_string
          | [] -> None
        in
        match name with
        | Some name -> Ok (Variable.make ~name ~location)
        | None -> root_cause "Could not find enum name." j)
  in
  let* constants =
    with_field_or "inner"
      (fun j ->
        let* l = cast_list j in
        cast_map parse_constant (`List (List.filter is_constant l)))
      [] o
  in
  Ok { var; constants }

let rec parse_with_context (inherited_type_params : Ty_param.t list)
    (inherited_template_args : Template_argument.t list) (j : Yojson.Basic.t) :
    t list j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  let parse_k (type_params : Ty_param.t list) (j : Yojson.Basic.t) :
      t list j_result =
    if is_kernel j then
      let* k = C_kernel.parse type_params j in
      let k =
        if k.template_args = [] && inherited_template_args <> [] then
          { k with template_args = inherited_template_args }
        else k
      in
      if k.code = Skip then Ok [] else Ok [ Kernel k ]
    else Ok []
  in
  match k with
  | "FunctionTemplateDecl" -> (
      let rec split_params (type_params : Ty_param.t list) :
          Yojson.Basic.t list ->
          (Ty_param.t list * Yojson.Basic.t list) j_result = function
        | [] -> Ok (List.rev type_params, [])
        | j :: l -> (
            let* p = Ty_param.parse j in
            match p with
            | Some p -> split_params (p :: type_params) l
            | None -> Ok (List.rev type_params, j :: l))
      in
      let* inner = with_field "inner" cast_list o in
      let* type_params, fdecls = split_params inherited_type_params inner in
      let is_specialization (j : Yojson.Basic.t) : bool =
        match j with
        | `Assoc o' -> (
            match List.assoc_opt "templateArgs" o' with
            | Some (`List (_ :: _)) -> true
            | _ -> false)
        | _ -> false
      in
      let primaries, specs =
        List.partition (fun j -> not (is_specialization j)) fdecls
      in
      let to_parse = if specs = [] then primaries else specs in
      let rec parse_all : Yojson.Basic.t list -> t list j_result = function
        | [] -> Ok []
        | j :: rest ->
            let* ks = parse_k type_params j in
            let* rest_ks = parse_all rest in
            Ok (ks @ rest_ks)
      in
      match to_parse with
      | [] ->
          root_cause "Error parsing FunctionTemplateDecl: no FunctionDecl found"
            j
      | _ -> parse_all to_parse)
  | "FunctionDecl" -> parse_k inherited_type_params j
  | "CXXMethodDecl" ->
      if subgroup_method_decl j then parse_k inherited_type_params j else Ok []
  | "VarDecl" -> (
      match Decl.parse j with Ok (Some d) -> Ok [ Declaration d ] | _ -> Ok [])
  | "ClassTemplateDecl" ->
      if
        (not (supported_subgroup_class j))
        || not (contains_subgroup_method_decl j)
      then Ok []
      else
        let* inner = with_field_or "inner" cast_list [] o in
        let* defs =
          inner
          |> map
               (parse_with_context inherited_type_params inherited_template_args)
        in
        Ok (List.concat defs)
  | "ClassTemplateSpecializationDecl" | "ClassTemplatePartialSpecializationDecl"
    ->
      if
        (not (supported_subgroup_class j))
        || not (contains_subgroup_method_decl j)
      then Ok []
      else
        let* template_args =
          with_field_or "templateArgs"
            (cast_map Parsers.parse_c_template_argument)
            inherited_template_args o
        in
        let* defs =
          with_field_or "inner"
            (cast_map (parse_with_context inherited_type_params template_args))
            [] o
        in
        Ok (List.concat defs)
  | "CXXRecordDecl" ->
      if
        (not (supported_subgroup_class j))
        || not (contains_subgroup_method_decl j)
      then Ok []
      else
        let* defs =
          with_field_or "inner"
            (cast_map
               (parse_with_context inherited_type_params inherited_template_args))
            [] o
        in
        Ok (List.concat defs)
  | "LinkageSpecDecl" | "NamespaceDecl" ->
      let* defs =
        with_field_or "inner"
          (cast_map
             (parse_with_context inherited_type_params inherited_template_args))
          [] o
      in
      Ok (List.concat defs)
  | "TypedefDecl" | "TypeAliasDecl" -> (
      let* name = with_field "name" cast_string o in
      let* ty = get_field "type" o |> Result.map J_type.from_json in
      let location =
        with_field "range" parse_location o
        |> Result.value ~default:Location.empty
      in
      let ty = J_type.from_c_type (J_type.to_desugared_c_type ty) in
      match J_type.to_c_type_res ty with
      | Ok ty ->
          if C_type.is_struct ty || C_type.is_array ty || C_type.is_function ty
          then Ok []
          else Ok [ Typedef { name; ty; location } ]
      | Error _ -> Ok [])
  | "EnumDecl" ->
      let* e = parse_enum j in
      Ok [ Enum e ]
  | "LaunchParam" ->
      let* lp = LaunchParam.parse j in
      Ok [ LaunchParam lp ]
  | "LaunchParamWarning" ->
      let* () = LaunchParam.log_warning j in
      Ok []
  | _ -> Ok []

and parse (j : Yojson.Basic.t) : t list j_result = parse_with_context [] [] j
