open Stage0
open Protocols
open Location_parser
open Ast
open Parse_util

module KernelAttr = Kernel_attr
module LaunchParam = Launch_param

type t =
  | Kernel of C_kernel.t
  (* A function declared without a body. It is kept so that a call to it
     can be recognised as reaching code faial cannot see, rather than
     silently vanishing. Its [code] is [Skip] and must not be read. *)
  | Prototype of C_kernel.t
  | Declaration of Decl.t
  | Typedef of Typedef.t
  | Record of Record.t
  | UsingNamespace of string
  | Enum of Imp.Enum.t
  | LaunchParam of LaunchParam.t

let remove_comma : t -> t = function
  | Kernel k -> Kernel (C_kernel.rewrite_comma k)
  | Declaration d -> Declaration (Decl.map_expr Expr.remove_comma d)
  | (Prototype _ | Typedef _ | Record _ | Enum _ | LaunchParam _
    | UsingNamespace _) as d ->
      d

let rewrite_barriers : t -> t = function
  | Kernel k -> Kernel (C_kernel.rewrite_barriers k)
  | ( Prototype _ | Declaration _ | Typedef _ | Record _ | Enum _
    | LaunchParam _ | UsingNamespace _ ) as d ->
      d

let to_s (d : t) : Indent.t list =
  match d with
  | Declaration d -> Decl.to_s d
  | Kernel k | Prototype k -> C_kernel.to_s k
  | Typedef d -> Typedef.to_s d
  | Record r -> Record.to_s r
  | UsingNamespace n -> [ Line ("using namespace " ^ n ^ ";") ]
  | Enum e -> Imp.Enum.to_s e
  | LaunchParam lp -> LaunchParam.to_s lp

let location : t -> Location.t = function
  | Declaration d -> Decl.location d
  | Kernel k | Prototype k -> C_kernel.location k
  | Typedef d -> Typedef.location d
  | Record r -> Record.location r
  | UsingNamespace _ -> Location.empty
  | Enum e -> Imp.Enum.location e
  | LaunchParam lp -> LaunchParam.location lp

let has_array_type (j : Yojson.Basic.t) : bool =
  let open Rjson in
  let is_array =
    let* o = cast_object j in
    let* ty = get_field "type" o |> Result.map J_type.parse in
    Ok (Ty.is_array_or_pointer ty)
  in
  is_array |> Result.value ~default:false

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
          | None ->
              root_cause
                ("ConstantExpr.value is not an integer: " ^ s) j)
      | _ ->
          root_cause "ConstantExpr without a pre-evaluated value" j
    else if List.mem k [ "ImplicitCastExpr"; "ParenExpr"; "CStyleCastExpr" ] then
      with_field "inner" (cast_list_1 parse_init) o
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
    | Error _ ->
        let* location = with_field "range" parse_location o in
        let name =
          let open Yojson.Basic.Util in
          let inner =
            List.assoc_opt "inner" o |> Option.value ~default:(`List [])
          in
          let consts =
            match inner with
            | `List l -> List.filter is_constant l
            | _ -> []
          in
          match consts with
          | first :: _ ->
              first
              |> member "type"
              |> J_type.parse
              |> Ty.to_string
              |> Option.some
          | [] -> None
        in
        (match name with
        | Some name -> Ok (Variable.make ~name ~location ())
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

(* A lambda written in host code carries the only copy of its body, and its
   closure is a record whose fields clang leaves unnamed: field [i] holds
   capture [i], whose initialiser names the variable the body refers to. The
   body reads those names as free variables, so [operator()] takes the
   closure as [this] and each captured name becomes a member of it, which is
   the shape a call through a by-value closure argument already binds. *)
let rec closures (j : Yojson.Basic.t) : t list =
  let open Rjson in
  let lambda (o : j_object) : t list =
    let inner = with_field_or "inner" cast_list [] o |> Result.value ~default:[] in
    let record =
      List.find_map
        (fun j ->
          match cast_object j with
          | Ok o when get_kind o = Ok "CXXRecordDecl" -> Some o
          | _ -> None)
        inner
    in
    let captures =
      match inner with
      | _ :: rest when List.length rest > 1 ->
          List.filteri (fun i _ -> i < List.length rest - 1) rest
      | _ -> []
    in
    let capture_name (j : Yojson.Basic.t) : string option =
      let rec walk (j : Yojson.Basic.t) : string option =
        match cast_object j with
        | Error _ -> None
        | Ok o -> (
            match get_kind o with
            | Ok "DeclRefExpr" ->
                with_field "referencedDecl"
                  (fun d ->
                    let* d = cast_object d in
                    with_field "name" cast_string d)
                  o
                |> Result.to_option
            | _ ->
                with_field_or "inner" cast_list [] o
                |> Result.value ~default:[] |> List.find_map walk)
      in
      walk j
    in
    match record with
    | None -> []
    | Some r ->
        let members =
          with_field_or "inner" cast_list [] r |> Result.value ~default:[]
        in
        let fields =
          members
          |> List.filter (j_filter_kind (fun k -> k = "FieldDecl"))
          |> List.filter_map (fun j ->
              match cast_object j with
              | Ok o -> get_field "type" o |> Result.to_option
              | Error _ -> None)
        in
        let names = List.map capture_name captures in
        if fields = [] || List.length fields <> List.length names then []
        else
          let bound =
            List.combine names fields
            |> List.filter_map (fun (n, ty) ->
                Option.map (fun n -> (n, J_type.parse ty)) n)
          in
          if List.length bound <> List.length fields then []
          else
            let name = with_opt_field "name" cast_string r in
            let location =
              with_field "range" parse_location r
              |> Result.value ~default:Location.empty
            in
            let self =
              match name with
              | Ok (Some base) -> Some { Ty.base; args = [] }
              | Ok None | Error _ -> None
            in
            let operators =
              members
              |> List.filter (fun j ->
                  j_filter_kind (fun k -> k = "CXXMethodDecl") j && is_kernel j)
              |> List.filter_map (fun j ->
                  match (self, C_kernel.parse [] j) with
                  | Some self, Ok k when C_kernel.has_body k ->
                      Some (Kernel (C_kernel.bind_closure ~self ~captures:bound k))
                  | _ -> None)
            in
            (match self with
             | Some name ->
                 Record
                   {
                     Record.name;
                     qualifier = [];
                     bases = [];
                     fields =
                       bound
                       |> List.map (fun (name, ty) ->
                              Record.Field.make ~name ~ty ());
                     size = None;
                     align = None;
                     location;
                   }
                 :: operators
             | None -> operators)
  in
  match cast_object j with
  | Error _ -> (
      match j with `List l -> List.concat_map closures l | _ -> [])
  | Ok o -> (
      match get_kind o with
      | Ok "LambdaExpr" -> lambda o
      | _ ->
          with_field_or "inner" cast_list [] o
          |> Result.value ~default:[]
          |> List.concat_map closures)

and parse ?(qualifier = []) (j : Yojson.Basic.t) : t list j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  let parse_k (type_params : Ty_param.t list) (j : Yojson.Basic.t) :
      t list j_result =
    if is_kernel j then
      let* k = C_kernel.parse ~qualifier type_params j in
      if not (C_kernel.has_body k) then Ok [ Prototype k ] else Ok [ Kernel k ]
    else Ok (closures j)
  in
  match k with
  | "FunctionTemplateDecl" ->
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
      let* type_params, fdecls = split_params [] inner in
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
      let rec parse_all : Yojson.Basic.t list -> t list j_result =
        function
        | [] -> Ok []
        | j :: rest ->
            let* ks = parse_k type_params j in
            let* rest_ks = parse_all rest in
            Ok (ks @ rest_ks)
      in
      (match to_parse with
       | [] ->
           root_cause
             "Error parsing FunctionTemplateDecl: no FunctionDecl found" j
       | _ -> parse_all to_parse)
  | "FunctionDecl" | "CXXMethodDecl" -> parse_k [] j
  | "CXXRecordDecl" | "ClassTemplateSpecializationDecl"
  | "ClassTemplatePartialSpecializationDecl" -> (
      let name = with_opt_field "name" cast_string o in
      let qualifier =
        match J_type.qualifier_opt o with
        | Some q -> q
        | None -> qualifier
      in
      let self : Ty.segment option =
        match name with
        | Ok (Some base) -> Some { Ty.base; args = J_type.template_args o }
        | Ok None | Error _ -> None
      in
      let inner_qualifier =
        match self with Some s -> qualifier @ [ s ] | None -> qualifier
      in
      let* inner = with_field_or "inner" cast_list [] o in
      (* A nested record is already here as a child; it is dropped on our
         side rather than omitted upstream. Recursing also reaches the
         injected class name, an implicit node carrying the record's own
         name and no fields, which the [fields = []] guard below skips. *)
      let members =
        inner
        |> List.filter
             (j_filter_kind (fun k ->
                  k = "CXXMethodDecl" || k = "CXXRecordDecl"
                  || k = "FunctionTemplateDecl"))
      in
      let* defs = cast_map (parse ~qualifier:inner_qualifier) (`List members) in
      let defs = List.concat defs in
      let bases =
        with_field_or "bases" cast_list [] o
        |> Result.value ~default:[]
        |> List.filter_map (fun j ->
            let base =
              let* o = cast_object j in
              let* ty = get_field "type" o in
              Ok (J_type.parse ty)
            in
            base |> Result.to_option
            |> Fun.flip Option.bind Record.type_path)
      in
      match self with
      | Some name ->
          let fields =
            inner
            |> List.filter (j_filter_kind (fun k -> k = "FieldDecl"))
            |> List.filter_map (fun j ->
                let field =
                  let* o = cast_object j in
                  let* name = with_field "name" cast_string o in
                  let* ty = get_field "type" o in
                  let offset =
                    with_field "offsetBits" cast_int o |> Result.to_option
                  in
                  Ok (Record.Field.make ?offset ~name ~ty:(J_type.parse ty) ())
                in
                Result.to_option field)
          in
          let layout (field : string) : int option =
            with_field field cast_int o |> Result.to_option
          in
          let location =
            with_field "range" parse_location o
            |> Result.value ~default:Location.empty
          in
          if fields = [] && bases = [] then Ok defs
          else
            Ok
              (Record
                 {
                   Record.name;
                   qualifier;
                   bases;
                   fields;
                   size = layout "size";
                   align = layout "align";
                   location;
                 }
              :: defs)
      | None -> Ok defs)
  | "VarDecl" -> (
      match Decl.parse j with
      | Ok (Some d) -> Ok [ Declaration d ]
      | _ -> Ok [])
  | "LinkageSpecDecl" ->
      (* [extern "C"] changes linkage, not the qualified name. *)
      let* defs = with_field_or "inner" (cast_map (parse ~qualifier)) [] o in
      Ok (List.concat defs)
  | "NamespaceDecl" ->
      let qualifier =
        match with_opt_field "name" cast_string o with
        | Ok (Some n) -> qualifier @ [ Ty.segment n ]
        (* An anonymous namespace has no name to qualify with; its
           members are unreachable from any other namespace anyway. *)
        | Ok None | Error _ -> qualifier
      in
      let* defs = with_field_or "inner" (cast_map (parse ~qualifier)) [] o in
      Ok (List.concat defs)
  | "TypedefDecl" | "TypeAliasDecl" -> (
      let* name = with_field "name" cast_string o in
      let* ty = get_field "type" o |> Result.map J_type.parse in
      let location =
        with_field "range" parse_location o
        |> Result.value ~default:Location.empty
      in
      (* A typedef of a record is kept only when it renames one, which is
         what lets a use of the alias find the record's fields. The
         self-named form, [typedef struct X { ... } X], renames nothing and
         is how CUDA declares its vector types, whose lanes are handled
         apart from records. *)
      let self_named =
        Ty.is_struct ty && Record.type_path ty = Some [ Ty.segment name ]
      in
      if self_named || Ty.is_array_or_pointer ty || Ty.is_function ty then
        Ok []
      else Ok [ Typedef { alias = Ty.parse name; ty; location } ])
  | "ClassTemplateDecl" ->
      let* inner = with_field_or "inner" cast_list [] o in
      let records =
        inner
        |> List.filter
             (j_filter_kind (fun k ->
                  k = "CXXRecordDecl" || k = "ClassTemplateSpecializationDecl"
                  || k = "ClassTemplatePartialSpecializationDecl"))
      in
      let* defs = cast_map (parse ~qualifier) (`List records) in
      Ok (List.concat defs)
  | "UsingDirectiveDecl" -> (
      let ns =
        let* o = get_field "nominatedNamespace" o |> Result.map Fun.id in
        let* o = cast_object o in
        with_field "name" cast_string o
      in
      match ns with Ok n -> Ok [ UsingNamespace n ] | Error _ -> Ok [])
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
