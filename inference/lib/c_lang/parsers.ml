open Stage0
open Protocols
open Location_parser
open Ast
open Parse_util
module StackTrace = Stack_trace

let decl_is_valid_j : json -> bool = function
  | `Assoc o -> (
      match Rjson.get_kind o with
      | Error _ | Ok "FullComment" -> false
      | Ok _ -> true)
  | _ -> false

(* The C-AST parsers are mutually recursive. Parsing an expression may need to
   parse a statement (for [StmtExpr] and [LambdaExpr] bodies), and
   parsing a statement requires parsing expressions, declarations, and
   for-init clauses. They are defined as a single mutually-recursive
   function. Each per-category module ([Expr], [Stmt], etc.)
   re-exports the relevant entry point as a thin delegate. *)
let rec parse_expr (j : json) : c_expr j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | _ when is_invalid o ->
      (* Unknown value *)
      let* ty = get_field "type" o in
      Ok (RecoveryExpr (J_type.parse ty))
  | "ImplicitValueInitExpr" | "CXXNullPtrLiteralExpr"
  | "StringLiteral" | "PredefinedExpr" | "SizeOfPackExpr"
  | "RecoveryExpr" | "UnresolvedMemberExpr" ->
      (* Unknown value *)
      let* ty = get_field "type" o in
      Ok (RecoveryExpr (J_type.parse ty))
  | "CXXThisExpr" ->
      (* The object a non-static method reads its members through, which
         [C_kernel.parse] gives that method as a leading parameter. The
         node's type is a pointer to the record and the parameter holds
         the record itself, so that both sides expand into the same
         members. *)
      let* ty = get_field "type" o |> Result.map J_type.parse in
      let ty = match ty.inner with Ty.Pointer p -> p | _ -> ty in
      Ok
        (Ident
           { name = this_var; ty; kind = Decl_expr.Kind.ParmVar;
             decl_id = None })
  | "DependentScopeDeclRefExpr" ->
      (* Qualified dependent reference like [Traits<T>::value]. The
         JSON now carries [name] and [nestedNameSpecifier] (older
         dumpers emitted only the bare envelope, which forced a
         RecoveryExpr collapse). *)
      let* ty = get_field "type" o |> Result.map J_type.parse in
      let* name = with_field "name" cast_string o in
      let* nested_name_specifier =
        with_opt_field "nestedNameSpecifier" cast_string o
      in
      Ok (DependentScopeRef { name; nested_name_specifier; ty })
  | "ShuffleVectorExpr" | "ConvertVectorExpr" ->
      (* Clang's [__builtin_shufflevector(vec1, vec2, idx0, idx1, ...)] and
         [__builtin_convertvector(vec, type)], used by <mmintrin.h>- and
         <xmmintrin.h>-style intrinsics. Preserve the inner operands as a
         synthetic call so downstream stages still see the dependencies;
         protocol-level vector-shuffle support is TODO. *)
      let* args = with_field "inner" (cast_map parse_expr) o in
      let* ty = get_field "type" o in
      let builtin_name =
        if kind = "ShuffleVectorExpr" then "__shufflevector"
        else "__convertvector"
      in
      let func =
        Ident
          {
            name = Variable.from_name builtin_name;
            ty = J_type.parse ty;
            kind = Decl_expr.Kind.Function;
            decl_id = None;
          }
      in
      Ok (CallExpr { func; args; ty = J_type.parse ty })
  | "CharacterLiteral" ->
      let* i = with_field "value" cast_int o in
      Ok (CharacterLiteral i)
  | "CXXConstCastExpr" | "CXXReinterpretCastExpr"
  | "ImplicitCastExpr" | "CXXStaticCastExpr" | "ConstantExpr" | "ParenExpr"
  | "ExprWithCleanups" | "CStyleCastExpr" | "CXXDefaultArgExpr"
  | "CXXFunctionalCastExpr" ->
      let* arg = with_field "inner" (cast_list_1 parse_expr) o in
      (* The pure wrappers share this arm, and [convert] declines them
         without a special case, since their type equals their operand's. *)
      Ok
        (match get_field "type" o with
        | Ok ty -> convert (J_type.parse ty) arg
        | Error _ -> arg)
  | "PackExpansionExpr" ->
      (* Preserve the pack-expansion wrapper rather than collapsing to
         the bare pattern: [f(args...)] keeps the trailing [...] so
         downstream stages can tell it apart from [f(args)]. *)
      let* inner = with_field "inner" (cast_list_1 parse_expr) o in
      Ok (PackExpansion inner)
  | "SubstNonTypeTemplateParmExpr" ->
      (* Clang wraps a substituted non-type template parameter
         (e.g. [BS] becoming [128] in [reduce<float, 128>]) in this
         node; [inner] is [NonTypeTemplateParmDecl; <substituted value>].
         Drop the parameter decl and recurse into the substituted value. *)
      with_field "inner"
        (fun i ->
          let* l = cast_list i in
          match l with
          | [ _; v ] -> parse_expr v
          | _ ->
              root_cause
                ("SubstNonTypeTemplateParmExpr: expected 2 inner items, got "
                ^ (List.length l |> string_of_int))
                i)
        o
  | "CXXDependentScopeMemberExpr" ->
      let* n = with_field "member" cast_string o in
      let* b =
        with_field "inner"
          (fun i ->
            match cast_map parse_expr i with
            | Ok [ o ] -> Ok o
            | Ok l ->
                root_cause
                  ("A list of length 1, but got "
                  ^ (List.length l |> string_of_int))
                  i
            | Error e -> Error e)
          o
      in
      let* ty = get_field "type" o in
      Ok (MemberExpr { name = n; base = b; ty = J_type.parse ty })
  | "DeclRefExpr" ->
      with_field "referencedDecl"
        (fun new_j ->
          (* Propagate provenance *)
          let new_j =
            match (new_j, List.assoc_opt "range" o) with
            | `Assoc new_o, Some range -> `Assoc (("range", range) :: new_o)
            | _, _ -> new_j
          in
          parse_expr new_j)
        o
  | "FloatingLiteral" -> (
      match with_field "value" cast_int o with
      | Ok i -> Ok (FloatingLiteral (Float.of_int i))
      | _ ->
          let* f = with_field "value" cast_float o in
          Ok (FloatingLiteral f))
  | "IntegerLiteral" ->
      let* s = with_field "value" cast_string o in
      (* Try increasingly wide parses: OCaml [int] is 63-bit on 64-bit
         platforms, so [int_of_string] rejects literals in
         [2^62, 2^63) and uint64 sentinels in [2^63, 2^64).
         [Int64.of_string] covers up to [2^63-1]; the [0u]-prefixed
         form reinterprets the digits as unsigned 64-bit (two's
         complement), which is what C does for [unsigned long long]
         constants like [0xFFFFFFFFFFFFFFFFULL]. *)
      let fits_int (i64 : int64) : bool =
        i64 >= Int64.of_int Int.min_int && i64 <= Int64.of_int Int.max_int
      in
      let parsed : int option =
        match int_of_string_opt s with
        | Some i -> Some i
        | None -> (
            match Int64.of_string_opt s with
            | Some i64 when fits_int i64 -> Some (Int64.to_int i64)
            | _ -> (
                match Int64.of_string_opt ("0u" ^ s) with
                | Some i64 when fits_int i64 -> Some (Int64.to_int i64)
                | _ -> None))
      in
      let i =
        match parsed with
        | Some i -> i
        | None ->
            prerr_endline ("Could not parse long: " ^ s);
            if String.length s > 0 && String.get s 0 = '-' then Int.min_int
            else Int.max_int
      in
      Ok (IntegerLiteral i)
  | "MemberExpr" ->
      let* n = with_field "name" cast_string o in
      let* b = with_field "inner" (cast_list_1 parse_expr) o in
      let* ty = get_field "type" o in
      Ok (MemberExpr { name = n; base = b; ty = J_type.parse ty })
  | "EnumConstantDecl" ->
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name; ty = J_type.parse ty; kind = EnumConstant;
                  decl_id = None })
  | "VarDecl" | "VarTemplateSpecializationDecl" | "BindingDecl" ->
      (* [VarTemplateSpecializationDecl] is a C++14 variable-template
         instantiation (e.g. [HASHTABLE_EMPTY_VALUE<uint64, uint32>]); it
         carries the same [name]/[type] shape as a plain [VarDecl].
         [BindingDecl] is a structured-binding element ([auto [a, b] =
         ...]); as a [DeclRefExpr]'s referencedDecl it carries the same
         [name]/[type] shape. *)
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name; ty = J_type.parse ty; kind = Var; decl_id = None })
  | "FunctionDecl" ->
      let* v = parse_variable j in
      let* ty = get_signature_type o in
      Ok
        (Ident
           { name = v; ty = J_type.parse ty; kind = Function;
             decl_id = parse_decl_id o })
  | "CXXMethodDecl" | "CXXConstructorDecl" | "CXXDestructorDecl"
  | "CXXConversionDecl" ->
      let* name = parse_variable j in
      let* ty = get_signature_type o in
      Ok
        (Ident
           { name; ty = J_type.parse ty; kind = CXXMethod;
             decl_id = parse_decl_id o })
  | "ConditionalOperator" ->
      let* c, t, e =
        with_field "inner" (cast_list_3 parse_expr parse_expr parse_expr) o
      in
      let* ty = get_field "type" o in
      Ok
        (ConditionalOperator
           {
             cond = c;
             then_expr = t;
             else_expr = e;
             ty = J_type.parse ty;
           })
  | "UnaryExprOrTypeTraitExpr" ->
      let written_type (j : json) : Ty.t j_result =
        let* o = cast_object j in
        let* ty = get_field "type" o in
        Ok (J_type.parse ty)
      in
      let* result_ty = get_field "type" o in
      let ty =
        match with_opt_field "argType" (fun j -> Ok (J_type.parse j)) o with
        | Ok (Some ty) -> ty
        | _ ->
            with_field "inner" (cast_first written_type) o
            |> Result.value ~default:(J_type.parse result_ty)
      in
      Ok (SizeOfExpr ty)
  | "ParmVarDecl" ->
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name; ty = J_type.parse ty; kind = ParmVar;
                  decl_id = None })
  | "NonTypeTemplateParmDecl" ->
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok
        (Ident { name; ty = J_type.parse ty; kind = NonTypeTemplateParm;
                 decl_id = None })
  | "UnresolvedLookupExpr" ->
      let* v = parse_variable j in
      let* tys = get_field "lookups" o >>= cast_list in
      Ok
        (UnresolvedLookupExpr
           { name = v; tys = List.map J_type.parse tys })
  | "CXXNewExpr" ->
      let* arg = with_field "inner" (cast_first parse_expr) o in
      let* ty = get_field "type" o in
      Ok (CXXNewExpr { arg; ty = J_type.parse ty })
  | "CXXDeleteExpr" ->
      let* arg = with_field "inner" (cast_list_1 parse_expr) o in
      let* ty = get_field "type" o in
      Ok (CXXDeleteExpr { arg; ty = J_type.parse ty })
  | "UnaryOperator" ->
      let* op = with_field "opcode" cast_string o in
      let* c = with_field "inner" (cast_list_1 parse_expr) o in
      let* ty = get_field "type" o in
      let inc o =
        BinaryOperator
          {
            ty = J_type.parse ty;
            opcode = "=";
            lhs = c;
            rhs =
              BinaryOperator
                {
                  ty = J_type.parse ty;
                  opcode = o;
                  lhs = c;
                  rhs = IntegerLiteral 1;
                };
          }
      in
      Ok
        (match op with
        | "++" -> inc "+"
        | "--" -> inc "-"
        | "+" -> c
        | "-" -> (
            match c with
            | IntegerLiteral n -> IntegerLiteral (-n)
            | _ ->
                BinaryOperator
                  {
                    ty = J_type.parse ty;
                    opcode = op;
                    lhs = IntegerLiteral 0;
                    rhs = c;
                  })
        | "*" -> (
            match c with
            | UnaryOperator { opcode = "&"; child; _ } -> child
            | _ ->
                UnaryOperator { ty = J_type.parse ty; opcode = op; child = c })
        | "&" -> (
            match c with
            | UnaryOperator { opcode = "*"; child; _ } -> child
            | _ ->
                UnaryOperator { ty = J_type.parse ty; opcode = op; child = c })
        | _ ->
            UnaryOperator { ty = J_type.parse ty; opcode = op; child = c })
  | "CompoundAssignOperator" -> (
      (* Convert: x += e into x = x + y *)
      let* ty = get_field "computeResultType" o in
      let* lhs, rhs =
        with_field "inner" (cast_list_2 parse_expr parse_expr) o
      in
      let* opcode = with_field "opcode" cast_string o in
      match Common.rsplit '=' opcode with
      | Some (opcode, "") ->
          Ok (c_expr_compound (J_type.parse ty) lhs opcode rhs)
      | _ -> root_cause "ERROR: parse_exp" j)
  | "BinaryOperator" ->
      let ty =
        List.assoc_opt "type" o
        |> Option.map J_type.parse
        |> Option.value ~default:J_type.int
      in
      let* opcode = with_field "opcode" cast_string o in
      let* lhs, rhs =
        with_field "inner" (cast_list_2 parse_expr parse_expr) o
      in
      Ok (BinaryOperator { ty; opcode; lhs; rhs })
  | "ArraySubscriptExpr" ->
      let* ty = get_field "type" o in
      let* lhs, rhs =
        with_field "inner" (cast_list_2 parse_expr parse_expr) o
      in
      let* loc = with_field "range" parse_location o in
      Ok
        (ArraySubscriptExpr
           {
             ty = J_type.parse ty;
             lhs;
             rhs;
             location = Location.set_length (Location.length loc + 1) loc;
           })
  | "CXXMemberCallExpr" | "CXXOperatorCallExpr" ->
      let* func, args =
        with_field "inner"
          (fun j ->
            let* h, t = cast_cons j in
            let* func = wrap parse_expr (fun _ -> ("func", j)) h in
            let* args = wrap (map parse_expr) (fun _ -> ("args", j)) t in
            Ok (func, args))
          o
      in
      let* ty = get_field "type" o in
      Ok
        (match (func, args) with
        | Ident { name = n; kind = CXXMethod; _ }, [ lhs; rhs ]
          when Variable.name n = "operator=" ->
            BinaryOperator
              { lhs; opcode = "="; rhs; ty = c_expr_to_type lhs }
        | UnresolvedLookupExpr { name = n; _ }, [ lhs; rhs ]
        | Ident { name = n; kind = Function; _ }, [ lhs; rhs ] -> (
            let ty = J_type.parse ty in
            match Variable.name n with
            | "operator-=" -> c_expr_compound ty lhs "-" rhs
            | "operator+=" -> c_expr_compound ty lhs "+" rhs
            | "operator*=" -> c_expr_compound ty lhs "*" rhs
            | "operator/=" -> c_expr_compound ty lhs "/" rhs
            | "operator%=" -> c_expr_compound ty lhs "%" rhs
            | "operator^=" -> c_expr_compound ty lhs "^" rhs
            | "operator&=" -> c_expr_compound ty lhs "&" rhs
            | "operator|=" -> c_expr_compound ty lhs "|" rhs
            | "operator<<=" -> c_expr_compound ty lhs "<<" rhs
            | "operator>>=" -> c_expr_compound ty lhs ">>" rhs
            | _ -> CXXOperatorCallExpr { func; args; ty })
        | _ -> CXXOperatorCallExpr { func; args; ty = J_type.parse ty })
  | "CallExpr" ->
      let* func, args =
        with_field "inner"
          (fun j ->
            let* h, t = cast_cons j in
            let* func = wrap parse_expr (fun _ -> ("func", j)) h in
            let* args = wrap (map parse_expr) (fun _ -> ("args", j)) t in
            Ok (func, args))
          o
      in
      let* ty = get_field "type" o in
      (* CUDA bit-level reinterpret-cast intrinsics. Each is a
         bijection on the underlying bits; faial's value model
         can't represent the type punning, but downstream
         passes ([Atomic_seed_read]) need the wrapped variable
         to flow through identity in the alias graph.
         Unwrapping at the C_lang stage lets the rest of the
         pipeline see the bare argument as if the intrinsic
         were absent. *)
      let is_reinterpret_cast = function
        | "__double_as_longlong" | "__longlong_as_double"
        | "__float_as_int" | "__int_as_float"
        | "__float_as_uint" | "__uint_as_float" ->
            true
        | _ -> false
      in
      (match func, args with
       | Ident f, [ arg ] when is_reinterpret_cast (Variable.name f.name) ->
           Ok arg
       | _ -> Ok (CallExpr { func; args; ty = J_type.parse ty }))
  | "CXXBindTemporaryExpr" | "MaterializeTemporaryExpr"
  | "CompoundLiteralExpr" ->
      let* body = with_field "inner" (cast_list_1 parse_expr) o in
      Ok body
  | "StmtExpr" ->
      (* GCC statement expression [({ s1; s2; ... ; e; })]. The inner
         CompoundStmt's body is a sequence of statements with the
         trailing element being the value-yielding expression. We
         parse the body via [parse_stmt_list] (mutually recursive
         with this function), then split off the trailing [SExpr e]
         as [result] — leaving the prefix statements as [body]. The
         [rewrite_stmtexpr] pass downstream hoists [body] into the
         enclosing statement scope and replaces the StmtExpr with
         [result], faithfully preserving C semantics: the statements
         execute in the enclosing scope before the value is consumed. *)
      let* compound = with_field "inner" (cast_list_1 (fun j -> Ok j)) o in
      let* compound_o = cast_object compound in
      let* body_stmt =
        with_field "inner" (fun j -> parse_stmt_list j) compound_o
      in
      let* ty = get_field "type" o in
      let ty = J_type.parse ty in
      let rec split_trailing : c_stmt -> (c_stmt * c_expr) option = function
        | SExpr e -> Some (Skip, e)
        | Seq (s1, s2) -> (
            match split_trailing s2 with
            | Some (rest, e) -> Some (Seq (s1, rest), e)
            | None -> None)
        | _ -> None
      in
      (match split_trailing body_stmt with
      | Some (body, result) -> Ok (StmtExpr { body; result; ty })
      | None ->
          (* No trailing expression — illegal C, treat as unknown. *)
          Ok (RecoveryExpr ty))
  | "CXXTemporaryObjectExpr" | "InitListExpr" | "CXXUnresolvedConstructExpr"
  | "CXXConstructExpr" ->
      let* ty = get_field "type" o in
      let* args = with_field_or "inner" (cast_map parse_expr) [] o in
      Ok (CXXConstructExpr { args; ty = J_type.parse ty })
  | "CXXBoolLiteralExpr" ->
      let* b = with_field "value" cast_bool o in
      Ok (CXXBoolLiteralExpr b)
  | "LambdaExpr" ->
      (* Shape (verified empirically on lrn-cuda):
           LambdaExpr { inner = [closure_CXXRecordDecl; cap_init × M;
                                 redundant_CompoundStmt(body)] }
           closure_CXXRecordDecl.inner = [CXXMethodDecl operator() {
             inner = [ParmVarDecl × N; CompoundStmt body; CUDA attrs]
           }; FieldDecl × M; (implicit ctors/dtor)]
         Captures are matched 1:1 between FieldDecls (in closure
         order) and the cap_init exprs (in LambdaExpr.inner order).
         Each cap_init parses to an [Ident] referencing the captured
         outer-scope variable. *)
      let* ty = get_field "type" o in
      let ty = J_type.parse ty in
      let* inner_list = with_field "inner" cast_list o in
      let* closure_j, init_and_body =
        match inner_list with
        | h :: rest -> Ok (h, rest)
        | [] -> root_cause "LambdaExpr with empty inner" j
      in
      let* closure_o = cast_object closure_j in
      let* _ = expect_kind "CXXRecordDecl" closure_o in
      let* closure_inner = with_field "inner" cast_list closure_o in
      let is_kind k j = j_filter_kind (fun x -> x = k) j in
      let is_named_method (name : string) (item : json) : bool =
        Result.value ~default:false
          (let* o = cast_object item in
           let* k = get_kind o in
           let* nm = with_field_or "name" cast_string "" o in
           Ok (k = "CXXMethodDecl" && nm = name))
      in
      let op_method_of (item : json) : json option =
        if is_named_method "operator()" item then Some item
        else
          Result.value ~default:None
            (let* o = cast_object item in
             let* k = get_kind o in
             let* nm = with_field_or "name" cast_string "" o in
             if k = "FunctionTemplateDecl" && nm = "operator()" then
               let* tmpl_inner = with_field "inner" cast_list o in
               Ok (List.find_opt (is_named_method "operator()") tmpl_inner)
             else Ok None)
      in
      let* op_j =
        match List.find_map op_method_of closure_inner with
        | Some op -> Ok op
        | None -> root_cause "LambdaExpr: no operator() method" j
      in
      let* op_o = cast_object op_j in
      let* op_inner = with_field "inner" cast_list op_o in
      let params_j = List.filter (is_kind "ParmVarDecl") op_inner in
      let* params = map Param.parse params_j in
      let* body_j =
        match List.find_opt (is_kind "CompoundStmt") op_inner with
        | Some b -> Ok b
        | None -> root_cause "LambdaExpr: no operator() body" j
      in
      let* body = parse_stmt body_j in
      let n_captures =
        List.length (List.filter (is_kind "FieldDecl") closure_inner)
      in
      let cap_inits =
        let rec take n = function
          | _ when n = 0 -> []
          | [] -> []
          | h :: t -> h :: take (n - 1) t
        in
        take n_captures init_and_body
      in
      (* Capture init exprs are usually a [DeclRefExpr] (auto-unwrapped
         through [ImplicitCastExpr] etc. by parse), but for captures
         of non-trivially-copyable values — notably *another lambda* —
         they're wrapped in a [CXXConstructExpr] (copy constructor)
         whose single arg is the underlying [Ident]. Walk through it
         to recover both the captured outer name and the simplified
         init expression. *)
      let rec resolve_capture (e : c_expr) : (Variable.t * c_expr) option =
        match e with
        | Ident i -> Some (i.name, Ident i)
        | CXXConstructExpr { args = [ single ]; _ } ->
            resolve_capture single
        | _ -> None
      in
      let* captures =
        map
          (fun init_j ->
            let* e = parse_expr init_j in
            match resolve_capture e with
            | Some r -> Ok r
            | None ->
                root_cause
                  "LambdaExpr capture init not resolvable to a name"
                  init_j)
          cap_inits
      in
      (* Return type: the operator()'s qualType is e.g.
           "float (int64_t, int64_t, ..., int64_t) const"
         Take the prefix before " (" as the return type string. *)
      let ret_ty =
        let r =
          let* op_ty_j = get_field "type" op_o in
          let qt = Ty.to_string (J_type.parse op_ty_j) in
          match String.index_opt qt '(' with
          | Some i ->
              let s = String.trim (String.sub qt 0 i) in
              Ok (J_type.of_string s)
          | None -> Ok (J_type.parse op_ty_j)
        in
        Result.value ~default:J_type.unknown r
      in
      let _ = ty in
      Ok (LambdaExpr { captures; params; body; ret_ty })
  | _ -> root_cause "ERROR: parse_exp" j

and parse_init (j : json) : c_init j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "ParenListExpr" | "InitListExpr" ->
      let* ty = get_field "type" o in
      let* args = with_field_or "inner" (cast_map parse_expr) [] o in
      Ok (InitListExpr { ty = J_type.parse ty; args })
  | _ ->
      let* e = parse_expr j in
      Ok (IExpr e)

and parse_decl (j : json) : c_decl option j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  (* Tag declarations (anonymous structs/unions/enums introduced
     by `struct { ... } v;`) appear as siblings of the VarDecl in a
     DeclStmt's inner list. They aren't variable declarations and
     have no `name` field, so skip them rather than failing inside
     parse_variable. *)
  let is_tag_decl =
    match k with
    | "CXXRecordDecl" | "RecordDecl" | "EnumDecl" | "ClassTemplateDecl" ->
        true
    | _ -> false
  in
  (* Block-scope declaration statements that introduce no runtime value
     carry no [type] field: [using X::y;] (UsingDecl), [using namespace
     foo;] (UsingDirectiveDecl), [namespace a = b;] (NamespaceAliasDecl),
     and the like. A real variable declaration always has a [type], so
     skip any typeless decl like a tag decl. *)
  let has_no_type = not (List.mem_assoc "type" o) in
  if is_invalid o || is_tag_decl || has_no_type then Ok None
  else
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    let inner =
      List.assoc_opt "inner" o |> Option.value ~default:(`List [])
    in
    let* inner = cast_list inner in
    let inner = List.filter decl_is_valid_j inner in
    let attrs, inits =
      List.partition
        (fun j ->
          (let* o = cast_object j in
           let* k = get_kind o in
           Ok
             (match k with
             | "CUDASharedAttr" | "CUDADeviceAttr" | "CUDAConstantAttr"
             | "CUDAManagedAttr" ->
                 true
             | _ -> false))
          |> Result.value ~default:false)
        inner
    in
    let inits =
      List.filter
        (fun j -> not (j_filter_kind (String.ends_with ~suffix:"Attr") j))
        inits
    in
    let* attrs = map parse_attr attrs in
    (* cu-to-json flags a file-scope global it proves is never written as
       [mutated: false]; record that as a synthetic attr so the lowering
       may fold its initializer. An absent field defaults to mutable,
       the conservative choice. *)
    let* mutated = with_field_or "mutated" cast_bool true o in
    let attrs = if mutated then attrs else c_attr_immutable :: attrs in
    let* inits = map parse_init inits in
    (* Further enforce that there is _at most_ one init expression. *)
    let* init =
      match inits with
      | [ init ] -> Ok (Some init)
      | [] -> Ok None
      | _ ->
          (* Print out a nice error message with provenance. *)
          let i = List.length inits |> string_of_int in
          let msg = "Expecting at most one expression, but got " ^ i in
          let open StackTrace in
          Error (Because (("Field 'init'", j), RootCause (msg, `List inner)))
    in
    let ty_var = Ty_variable.make ~name ~ty:(J_type.parse ty) in
    Ok
      (Some
         {
           ty = ty_var.ty;
           var = Ty_variable.name ty_var;
           init;
           attrs;
         })

and parse_for_init (j : json) : c_for_init j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "DeclStmt" ->
      let* ds = with_field "inner" (cast_map parse_decl) o in
      Ok (Decls (Common.flatten_opt ds))
  | _ ->
      let* e = parse_expr j in
      Ok (Expr e)

and parse_stmt (j : json) : c_stmt j_result =
  let open Rjson in
  let* o = cast_object j in
  match get_kind o |> Result.to_option with
  | Some "IfStmt" ->
      with_field "inner"
        (fun j ->
          let* l = cast_list j in
          let wrap (m : string) handle_ok =
            wrap handle_ok (fun _ -> (m, j))
          in
          match l with
          | [ cond; then_stmt; else_stmt ] ->
              let* cond = wrap "cond" parse_expr cond in
              let* then_stmt = wrap "then_stmt" parse_stmt then_stmt in
              let* else_stmt = wrap "else_stmt" parse_stmt else_stmt in
              Ok (IfStmt { cond; then_stmt; else_stmt })
          | [ cond; then_stmt ] ->
              let* cond = wrap "cond" parse_expr cond in
              let* then_stmt = wrap "then_stmt" parse_stmt then_stmt in
              Ok (IfStmt { cond; then_stmt; else_stmt = Skip })
          | _ ->
              let g = List.length l |> string_of_int in
              root_cause
                ("Expecting a list of length 2 or 3, but got a length of \
                  list " ^ g)
                j)
        o
  | Some "WhileStmt" ->
      with_field "inner"
        (fun j ->
          let* l = cast_list j in
          match l with
          | [ cond; body ] ->
              let* cond = parse_expr cond in
              let* body = parse_stmt body in
              Ok (WhileStmt { cond; body })
          | [ decl; cond; body ] ->
              (* C++ [while (auto x = init) { body }]: clang emits
                 [decl; cond; body] (with [hasVar: true]). Render as
                 [for (auto x = init; cond; ) body] — [parse_for_init]
                 already lowers the [DeclStmt] node into a [Decls]
                 init, so we keep [x] bound as the loop's own variable
                 instead of hoisting it into the enclosing scope. *)
              let* init = parse_for_init decl in
              let* cond = parse_expr cond in
              let* body = parse_stmt body in
              Ok
                (ForStmt
                   { init = Some init; cond = Some cond; inc = Skip; body })
          | _ ->
              let g = List.length l |> string_of_int in
              root_cause
                ("Expecting a list of length 2 or 3, but got a length of \
                  list " ^ g)
                j)
        o
  | Some "DeclStmt" -> (
      let has_typedecl : bool =
        let has_typedecl : bool j_result =
          let* children = get_field "inner" o in
          let* l = cast_list children in
          let* o = get_index 0 l >>= cast_object in
          let* k = get_kind o in
          Ok (k = "TypedefDecl" || k = "EnumDecl" || k = "TypeAliasDecl")
        in
        Result.value ~default:false has_typedecl
      in
      if has_typedecl then Ok Skip
      else
        let static_assert : c_stmt option =
          o
          |> with_field "inner"
               (cast_list_1 (fun j ->
                    (* Ensure the expected kind *)
                    let* o = cast_object j in
                    let* _ = expect_kind "StaticAssertDecl" o in
                    (* C++17 allows omitting the assertion message; clang
                       still emits an inner slot for it (as an empty
                       object [{}] or [null]). Drop entries that have no
                       [kind] field so parse_expr only sees real
                       expressions (the condition, plus an optional
                       StringLiteral message). *)
                    let* args =
                      with_field "inner"
                        (fun j ->
                          let* l = cast_list j in
                          let has_kind = function
                            | `Assoc fs -> List.mem_assoc "kind" fs
                            | _ -> false
                          in
                          map parse_expr (List.filter has_kind l))
                        o
                    in
                    let static_assert : Decl_expr.t =
                      {
                        name = Variable.from_name "static_assert";
                        ty = J_type.void;
                        kind = Decl_expr.Kind.Function;
                        decl_id = None;
                      }
                    in
                    let func = Ident static_assert in
                    Ok (SExpr (CallExpr { func; args; ty = J_type.void }))))
          |> Result.to_option
        in
        match static_assert with
        | Some e -> Ok e
        | None ->
            let* children = with_field "inner" (cast_map parse_decl) o in
            Ok (DeclStmt (children |> Common.flatten_opt)))
  | Some "DefaultStmt" ->
      let* c = with_field "inner" (cast_list_1 parse_stmt) o in
      Ok (DefaultStmt c)
  | Some "CaseStmt" ->
      let* c, b =
        with_field "inner" (cast_list_2 parse_expr parse_stmt) o
      in
      Ok (CaseStmt { case = c; body = b })
  | Some "SwitchStmt" ->
      let* cond, body =
        with_field "inner" (cast_list_2 parse_expr parse_stmt) o
      in
      Ok (SwitchStmt { cond; body })
  | Some "CompoundStmt" ->
      let* children =
        with_field_or "inner"
          (fun (i : json) ->
            match i with
            | `Assoc _ ->
                let* o = parse_stmt i in
                Ok o
            | _ -> parse_stmt_list i)
          Skip o
      in
      Ok children
  | Some "LabelStmt" ->
      (* TODO: do not parse LabelStmt *)
      with_field "inner" (cast_list_1 parse_stmt) o
  | Some "ReturnStmt" ->
      let* e =
        with_field_or "inner"
          (cast_list_1 (fun j -> parse_expr j |> Result.map Option.some))
          None o
      in
      Ok (ReturnStmt e)
  | Some "GotoStmt" -> Ok GotoStmt
  | Some "BreakStmt" -> Ok BreakStmt
  | Some "ContinueStmt" -> Ok ContinueStmt
  | Some "DoStmt" ->
      let* inner = with_field "inner" cast_list o in
      let* b, c =
        match inner with
        | [ b; c ] ->
            let* b = parse_stmt b in
            let* c = parse_expr c in
            Ok (b, c)
        | [ b ] ->
            let* b = parse_stmt b in
            Ok (b, CXXBoolLiteralExpr true)
        | _ -> root_cause "Error parsing DoStmt" j
      in
      Ok (DoStmt { cond = c; body = b })
  | Some "AttributedStmt" ->
      let* _, stmt =
        with_field "inner" (cast_list_2 Result.ok parse_stmt) o
      in
      Ok stmt
  | Some "ForStmt" ->
      with_field "inner"
        (fun j ->
          let* l = cast_list j in
          let wrap handle_ok (m : string) =
            wrap handle_ok (fun _ -> (m, j))
          in
          let wrap_opt handle_ok (m : string) (j : Yojson.Basic.t) =
            match j with
            | `Assoc [] -> Ok None
            | _ ->
                let* r = wrap handle_ok m j in
                Ok (Some r)
          in
          match l with
          | [ init; _; cond; inc; body ] ->
              let* init = wrap_opt parse_for_init "init" init in
              let* cond = wrap_opt parse_expr "cond" cond in
              let* inc = wrap_opt parse_expr "inc" inc in
              let inc =
                inc
                |> Option.map (fun e -> SExpr e)
                |> Option.value ~default:Skip
              in
              let* body = wrap parse_stmt "body" body in
              Ok (ForStmt { init; cond; inc; body })
          | _ ->
              let g = List.length l |> string_of_int in
              root_cause
                ("Expecting a list of length 5, but got a length of list " ^ g)
                j)
        o
  | Some "CXXForRangeStmt" ->
      (* C++ range-based [for (T x : range) body]. Clang's [inner]
         is [Init?; RangeStmt; BeginStmt; EndStmt; Cond; Inc;
         LoopVar; Body] — the optional init is C++20. The RangeStmt
         is a synthetic [DeclStmt] declaring [__rangeN] whose [type]
         is the range expression's type and whose first inner is the
         range expression itself. For built-in arrays the [qualType]
         carries the bound (e.g. [int (&)[3]]), which lets us emit a
         bounded [for (int __faial_idx = 0; __faial_idx < N; ++)]
         with [LoopVarT loop_var = arr_expr[__faial_idx]] in the
         body — the analyser then sees a real bound and a concrete
         binding for [loop_var]. When the bound can't be extracted
         (containers, iterator-based ranges) we fall back to a
         [while(1)] wrapper around the original LoopVar declaration,
         which under-approximates iteration count but at least keeps
         body accesses visible. *)
      let parse_array_bound (qual_type : string) : int option =
        match
          (String.rindex_opt qual_type '[', String.rindex_opt qual_type ']')
        with
        | Some i, Some j when j > i + 1 ->
            int_of_string_opt (String.sub qual_type (i + 1) (j - i - 1))
        | _ -> None
      in
      let parse_range_stmt (range_j : json) :
          (c_expr * int * Ty.t) option =
        let extract =
          let* ro = cast_object range_j in
          let* var_decls = with_field "inner" cast_list ro in
          let* var_j =
            match var_decls with
            | x :: _ -> Ok x
            | [] -> root_cause "RangeStmt: empty inner" range_j
          in
          let* vo = cast_object var_j in
          let* ty_j = get_field "type" vo in
          let qual_type = Ty.to_string (J_type.parse ty_j) in
          let* arr_expr = with_field "inner" (cast_list_1 parse_expr) vo in
          match parse_array_bound qual_type with
          | Some n -> Ok (arr_expr, n, J_type.parse ty_j)
          | None -> root_cause ("RangeStmt: no [N] in " ^ qual_type) range_j
        in
        match extract with Ok x -> Some x | Error _ -> None
      in
      (* The RangeStmt's VarDecl name is [__rangeN]; reuse N as a
         non-clashing suffix for our synthetic index variable. *)
      let synth_index_name (range_j : json) : string =
        let base = "__faial_for_range_idx" in
        match
          let* ro = cast_object range_j in
          let* var_decls = with_field "inner" cast_list ro in
          let* var_j =
            match var_decls with
            | x :: _ -> Ok x
            | [] -> root_cause "" range_j
          in
          let* vo = cast_object var_j in
          with_field "name" cast_string vo
        with
        | Ok n when String.length n > 7 && String.sub n 0 7 = "__range" ->
            base ^ String.sub n 7 (String.length n - 7)
        | _ -> base
      in
      (* The LoopVar DeclStmt wraps a single VarDecl whose name and
         type we want; keep its original parse_stmt result for the
         fallback path, and re-extract name/type for the bounded
         path. *)
      let parse_loop_var (lv_j : json) : (Variable.t * Ty.t) option =
        let extract =
          let* lo = cast_object lv_j in
          let* decls = with_field "inner" cast_list lo in
          let* d_j =
            match decls with
            | x :: _ -> Ok x
            | [] -> root_cause "LoopVar: empty inner" lv_j
          in
          let* d_o = cast_object d_j in
          let* var = parse_variable d_j in
          let* ty_j = get_field "type" d_o in
          Ok (var, J_type.parse ty_j)
        in
        match extract with Ok x -> Some x | Error _ -> None
      in
      with_field "inner"
        (fun j ->
          let* l = cast_list j in
          let n = List.length l in
          if n < 7 then
            root_cause
              ("CXXForRangeStmt: expected at least 7 inner elements, got "
              ^ string_of_int n)
              j
          else
            let body_j = List.nth l (n - 1) in
            let loop_var_j = List.nth l (n - 2) in
            let range_stmt_j = List.nth l (n - 7) in
            let* body = parse_stmt body_j in
            match
              ( parse_range_stmt range_stmt_j,
                parse_loop_var loop_var_j )
            with
            | Some (arr_expr, bound, _arr_ty), Some (loop_var, loop_var_ty)
              ->
                let idx_var =
                  Variable.from_name (synth_index_name range_stmt_j)
                in
                let idx_ref : c_expr =
                  Ident
                    (Decl_expr.from_name ~ty:J_type.int
                       ~kind:Decl_expr.Kind.Var idx_var)
                in
                let arr_subscript : c_expr =
                  ArraySubscriptExpr
                    {
                      lhs = arr_expr;
                      rhs = idx_ref;
                      ty = loop_var_ty;
                      location = Location.empty;
                    }
                in
                let init : c_for_init =
                  Decls
                    [
                      {
                        var = idx_var;
                        ty = J_type.int;
                        init = Some (IExpr (IntegerLiteral 0));
                        attrs = [];
                      };
                    ]
                in
                let cond : c_expr =
                  BinaryOperator
                    {
                      opcode = "<";
                      lhs = idx_ref;
                      rhs = IntegerLiteral bound;
                      ty = J_type.bool;
                    }
                in
                let inc : c_stmt =
                  SExpr
                    (UnaryOperator
                       { opcode = "++"; child = idx_ref; ty = J_type.int })
                in
                let loop_var_decl : c_stmt =
                  DeclStmt
                    [
                      {
                        var = loop_var;
                        ty = loop_var_ty;
                        init = Some (IExpr arr_subscript);
                        attrs = [];
                      };
                    ]
                in
                Ok
                  (ForStmt
                     {
                       init = Some init;
                       cond = Some cond;
                       inc;
                       body = Seq (loop_var_decl, body);
                     })
            | _ ->
                let* loop_var = parse_stmt loop_var_j in
                Ok (Seq (loop_var, WhileStmt { cond = IntegerLiteral 1; body })))
        o
  | Some "FullComment" | Some "NullStmt" -> Ok Skip
  | Some "GCCAsmStmt" ->
      let* a = Asm.parse parse_expr j in
      Ok (AsmStmt a)
  | Some _ ->
      let* e = parse_expr j in
      Ok (SExpr e)
  | None -> Ok Skip

and parse_stmt_list (j : json) : c_stmt j_result =
  let open Rjson in
  let* l = cast_list j in
  let* l =
    map_all parse_stmt
      (fun idx s e ->
        StackTrace.Because
          (("error parsing statement #" ^ string_of_int (idx + 1), s), e))
      l
  in
  Ok (c_stmt_from_list l)

and parse_c_template_argument (j : json) : c_template_argument j_result =
  let open Rjson in
  let* o = cast_object j in
  let* is_expr = with_field_or "isExpr" cast_bool false o in
  let* is_pack = with_field_or "isPack" cast_bool false o in
  let* is_expansion = with_field_or "isExpansion" cast_bool false o in
  let* is_null_ptr = with_field_or "isNullPtr" cast_bool false o in
  let* is_null = with_field_or "isNull" cast_bool false o in
  if is_expr then
    let* e = with_field "inner" (cast_list_1 parse_expr) o in
    Ok (TArgExpr e)
  else if is_pack then
    let* xs = with_field "inner" (cast_map parse_c_template_argument) o in
    Ok (TArgPack xs)
  else if is_null_ptr then Ok TArgNullPtr
  else if is_null then Ok TArgNullArg
  else if List.mem_assoc "decl" o then
    let* name =
      with_field "decl"
        (fun d ->
          let* od = cast_object d in
          with_field "name" cast_string od)
        o
    in
    Ok (TArgDecl name)
  else
    let* value_opt = with_opt_field "value" cast_int o in
    let* type_opt =
      with_opt_field "type" (fun j -> Ok (J_type.parse j)) o
    in
    let* name_opt = with_opt_field "name" cast_string o in
    match (value_opt, type_opt, name_opt) with
    | Some n, _, _ -> Ok (TArgIntegral n)
    | _, Some ty, _ -> Ok (TArgType ty)
    | _, _, Some n when is_expansion -> Ok (TArgTemplateExpansion n)
    | _, _, Some n -> Ok (TArgTemplate n)
    | _ -> root_cause "TemplateArgument: unrecognized shape" j

let parse_bare_decl_ref (j : Yojson.Basic.t) : Decl_expr.t Rjson.j_result =
  let open Rjson in
  let* e = parse_expr j in
  match e with
  | Ident d -> Ok d
  | _ -> root_cause "parse_bare_decl_ref: expected an Ident" j
