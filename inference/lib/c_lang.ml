open Stage0
open Protocols
open Location_parser
module StackTrace = Stack_trace
open Exp

type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

let list_to_s (f : 'a -> string) (l : 'a list) : string =
  List.map f l |> String.concat ", "

(* Monadic let *)
let ( let* ) = Result.bind

(* Monadic pipe *)
let ( >>= ) = Result.bind

let parse_variable (j : json) : Variable.t j_result =
  (let open Rjson in
   let* o = cast_object j in
   let* name = with_field "name" cast_string o in
   match List.assoc_opt "range" o with
   | Some range ->
       let* l = parse_location range in
       let l =
         if Location.length l = 0 then
           Location.set_length (String.length name) l
         else l
       in
       Ok (Variable.make ~location:l ~name)
   | None -> Ok (Variable.from_name name))
  |> Rjson.add_reason "parse_variable" j

let is_invalid (o : j_object) : bool =
  let open Rjson in
  with_opt_field "isInvalid" cast_bool o
  |> Result.value ~default:None
  |> Option.value ~default:false

let expect_kind (k : string) (o : j_object) : unit j_result =
  let open Rjson in
  let* obtained : string = get_kind o in
  if obtained = k then Ok ()
  else
    root_cause
      ("Expecting kind '" ^ k ^ "' but got '" ^ obtained ^ "'")
      (`Assoc o)

let parse_attr (j : Yojson.Basic.t) : string j_result =
  let open Rjson in
  let* o = cast_object j in
  let* v = with_field "value" cast_string o in
  Ok (String.trim v)

let j_filter_kind (f : string -> bool) (j : Yojson.Basic.t) : bool =
  let open Rjson in
  let res =
    let* o = cast_object j in
    let* k = get_kind o in
    Ok (f k)
  in
  res |> Result.value ~default:false

module Param = struct
  type t = { ty_var : Ty_variable.t; is_used : bool; is_shared : bool }

  let make ~ty_var ~is_used ~is_shared : t = { ty_var; is_used; is_shared }
  let ty_var (x : t) : Ty_variable.t = x.ty_var
  let name (x : t) : Variable.t = x.ty_var.name

  let matches (type_of : C_type.t -> bool) (x : t) : bool =
    Ty_variable.matches type_of x.ty_var

  let to_string (p : t) : string =
    let used = if p.is_used then "" else " unsed" in
    let shared = if p.is_shared then "shared " else "" in
    used ^ shared ^ Ty_variable.to_string p.ty_var

  let parse (j : Yojson.Basic.t) : t Rjson.j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    let* is_refed = with_field_or "isReferenced" cast_bool false o in
    let* is_used = with_field_or "isUsed" cast_bool false o in
    let* is_shared = with_field_or "shared" cast_bool false o in

    let ty_var : Ty_variable.t =
      Ty_variable.make ~ty:(J_type.from_json ty) ~name
    in
    Ok (make ~is_used:(is_refed || is_used) ~ty_var ~is_shared)
end

module BarrierOp = struct
  type t = Arrive | Wait | ArriveAndWait | ArriveAndDrop

  let to_string : t -> string = function
    | Arrive -> "arrive"
    | Wait -> "wait"
    | ArriveAndWait -> "arrive_and_wait"
    | ArriveAndDrop -> "arrive_and_drop"

  let of_method_name : string -> t option = function
    | "arrive" -> Some Arrive
    | "wait" -> Some Wait
    | "arrive_and_wait" -> Some ArriveAndWait
    | "arrive_and_drop" -> Some ArriveAndDrop
    | _ -> None

  (* Strict type guard on a resolved C type. *)
  let is_barrier_c_type (ty : C_type.t) : bool =
    Common.contains ~substring:"cuda::barrier" (C_type.to_string ty)

  (* Strict type guard: the desugared base type must be cuda::barrier<_>. *)
  let is_barrier_base_type (ty : J_type.t) : bool =
    J_type.desugared_matches is_barrier_c_type ty
end

(* The C-AST types are mutually recursive: [c_expr] needs to embed
   [c_stmt] (via [StmtExpr], the GCC statement expression), and
   [c_stmt] embeds [c_expr] in conditions, returns, etc. Defining the
   types at file scope lets us hand them out from the [Expr], [Init],
   [Decl], [ForInit], and [Stmt] modules below via the [type t = origin
   = | C1 ... | Cn ...] re-export pattern, so that [Expr.t = c_expr]
   etc. and [Expr.BinaryOperator] / [Stmt.IfStmt] / etc. remain
   accessible at their existing module paths. *)
type c_expr =
  | SizeOfExpr of J_type.t
  | CXXNewExpr of { arg : c_expr; ty : J_type.t }
  | CXXDeleteExpr of { arg : c_expr; ty : J_type.t }
  | RecoveryExpr of J_type.t
  | CharacterLiteral of int
  | ArraySubscriptExpr of c_array_subscript
  | BinaryOperator of c_binary
  | CallExpr of { func : c_expr; args : c_expr list; ty : J_type.t }
  | ConditionalOperator of {
      cond : c_expr;
      then_expr : c_expr;
      else_expr : c_expr;
      ty : J_type.t;
    }
  | CXXConstructExpr of { args : c_expr list; ty : J_type.t }
  | CXXBoolLiteralExpr of bool
  | Ident of Decl_expr.t
  | CXXOperatorCallExpr of { func : c_expr; args : c_expr list; ty : J_type.t }
  | FloatingLiteral of float
  | IntegerLiteral of int
  | MemberExpr of { name : string; base : c_expr; ty : J_type.t }
  | UnaryOperator of { opcode : string; child : c_expr; ty : J_type.t }
  | UnresolvedLookupExpr of { name : Variable.t; tys : J_type.t list }
  (* GCC statement expression [({ s1; s2; ... ; e; })]. The value of
     the expression is [result] (the trailing expression of the inner
     CompoundStmt); [body] holds the prefix statements (typically
     hygiene decls from a macro like [ppcg_min]). The [rewrite_stmtexpr]
     pass hoists [body] into the enclosing statement scope and replaces
     the StmtExpr with [result], so the rest of the pipeline never sees
     this constructor. *)
  | StmtExpr of { body : c_stmt; result : c_expr; ty : J_type.t }
  (* C++ lambda expression [\[captures\](params) { body }]. C_lang
     carries this through unchanged; [D_lang.rewrite_stmt] recognises
     the [auto v = LambdaExpr {...}] singleton-DeclStmt shape and emits
     a structured [LambdaDecl] statement, which [Lift_lambdas] hoists
     into a synthetic [Auxiliary] kernel before [D_to_imp] runs. *)
  | LambdaExpr of {
      captures : (Variable.t * c_expr) list;
      params : Param.t list;
      body : c_stmt;
      ret_ty : J_type.t;
    }
  (* C++11 parameter-pack expansion [pattern...]. Wraps the pattern
     expression and marks "this expression repeats over a parameter
     pack" so downstream stages can distinguish [f(args...)] from
     [f(args)]. The wrapper is otherwise transparent: type and
     traversal delegate to the inner expression. *)
  | PackExpansion of c_expr
  (* Dependent qualified reference inside a templated context, e.g.
     [Traits<T>::value] or [is_same<T, U>::value]. Carries the unqualified
     name and the nested-name-specifier verbatim so analyses can equate
     references by syntactic identity across uses without resolving
     them semantically. *)
  | DependentScopeRef of {
      name : string;
      nested_name_specifier : string option;
      ty : J_type.t;
    }

and c_binary = { opcode : string; lhs : c_expr; rhs : c_expr; ty : J_type.t }

and c_array_subscript = {
  lhs : c_expr;
  rhs : c_expr;
  ty : J_type.t;
  location : Location.t;
}

and c_init =
  | InitListExpr of { ty : J_type.t; args : c_expr list }
  | IExpr of c_expr

and c_decl = {
  var : Variable.t;
  ty : J_type.t;
  init : c_init option;
  attrs : string list;
}

and c_for_init = Decls of c_decl list | Expr of c_expr

and 'a c_if = { cond : c_expr; then_stmt : 'a; else_stmt : 'a }
and 'a c_cond = { cond : c_expr; body : 'a }

and 'a c_for = {
  init : c_for_init option;
  cond : c_expr option;
  inc : 'a;
  body : 'a;
}

and 'a c_case = { case : c_expr; body : 'a }

and c_stmt =
  | Skip
  | BreakStmt
  | GotoStmt
  | ReturnStmt of c_expr option
  | ContinueStmt
  | IfStmt of c_stmt c_if
  | DeclStmt of c_decl list
  | WhileStmt of c_stmt c_cond
  | ForStmt of c_stmt c_for
  | DoStmt of c_stmt c_cond
  | SwitchStmt of c_stmt c_cond
  | DefaultStmt of c_stmt
  | CaseStmt of c_stmt c_case
  | SExpr of c_expr
  | AsmStmt of c_expr Asm.t
  | BarrierOp of {
      op : BarrierOp.t;
      target : c_expr;
      args : c_expr list;
      loc : Location.t option;
    }
  | Seq of c_stmt * c_stmt

(* C++ template argument: the resolved arguments of a specialisation
   like [reduce<float, 128>] (where the JSON carries [type=float] and
   [value=128]) and the explicit-template-args arrays on dependent
   scope references. Mirrors the JSON shapes emitted by clang's
   [JSONNodeDumper], with [TArgExpr] / [TArgPack] requiring mutual
   recursion through [c_expr]. *)
and c_template_argument =
  | TArgType of J_type.t
  | TArgIntegral of int
  | TArgNullArg
  | TArgNullPtr
  | TArgDecl of string
  | TArgExpr of c_expr
  | TArgPack of c_template_argument list
  | TArgTemplate of string
  | TArgTemplateExpansion of string

(* The C-AST parsers form a knot: parsing an expression may need to
   parse a statement (for [StmtExpr] and [LambdaExpr] bodies), and
   parsing a statement requires parsing expressions, declarations, and
   for-init clauses. We define them as a single mutually-recursive
   group at the top level, then each module below re-exports its
   parser as a thin delegate. The standalone helpers
   [c_stmt_seq], [c_stmt_from_list], [c_expr_to_type],
   [c_expr_compound], and [decl_is_valid_j] mirror logic that also
   appears inside the modules; the modules can safely depend on the
   top-level versions because the top-level definitions don't depend
   on the modules in turn. *)

let c_stmt_seq (s1 : c_stmt) (s2 : c_stmt) : c_stmt =
  if s1 = Skip then s2 else if s2 = Skip then s1 else Seq (s1, s2)

let c_stmt_from_list (l : c_stmt list) : c_stmt =
  List.fold_left c_stmt_seq Skip l

let rec c_expr_to_type : c_expr -> J_type.t = function
  | SizeOfExpr _ -> J_type.int
  | CXXNewExpr c -> c.ty
  | CXXDeleteExpr c -> c.ty
  | CXXConstructExpr c -> c.ty
  | CharacterLiteral _ -> J_type.char
  | ArraySubscriptExpr a -> a.ty
  | BinaryOperator a -> a.ty
  | ConditionalOperator c -> c_expr_to_type c.then_expr
  | CXXBoolLiteralExpr _ -> J_type.bool
  | FloatingLiteral _ -> J_type.float
  | Ident a -> a.ty
  | IntegerLiteral _ -> J_type.int
  | UnaryOperator a -> a.ty
  | CallExpr c -> c.ty
  | CXXOperatorCallExpr a -> a.ty
  | MemberExpr a -> a.ty
  | UnresolvedLookupExpr _ -> J_type.unknown
  | RecoveryExpr ty -> ty
  | StmtExpr e -> e.ty
  | LambdaExpr _ ->
      (* Closure type — opaque from the analyser's POV before lifting. *)
      J_type.unknown
  | PackExpansion e -> c_expr_to_type e
  | DependentScopeRef d -> d.ty

let c_expr_compound (ty : J_type.t) (lhs : c_expr) (opcode : string)
    (rhs : c_expr) : c_expr =
  BinaryOperator
    { ty; opcode = "="; lhs; rhs = BinaryOperator { ty; opcode; lhs; rhs } }

let decl_is_valid_j : json -> bool = function
  | `Assoc o -> (
      match Rjson.get_kind o with
      | Error _ | Ok "FullComment" -> false
      | Ok _ -> true)
  | _ -> false

let rec parse_expr (j : json) : c_expr j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | _ when is_invalid o ->
      (* Unknown value *)
      let* ty = get_field "type" o in
      Ok (RecoveryExpr (J_type.from_json ty))
  | "ImplicitValueInitExpr" | "CXXNullPtrLiteralExpr"
  | "StringLiteral" | "RecoveryExpr" | "CXXThisExpr" ->
      (* Unknown value *)
      let* ty = get_field "type" o in
      Ok (RecoveryExpr (J_type.from_json ty))
  | "DependentScopeDeclRefExpr" ->
      (* Qualified dependent reference like [Traits<T>::value]. The
         JSON now carries [name] and [nestedNameSpecifier] (older
         dumpers emitted only the bare envelope, which forced a
         RecoveryExpr collapse). *)
      let* ty = get_field "type" o |> Result.map J_type.from_json in
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
            ty = J_type.from_json ty;
            kind = Decl_expr.Kind.Function;
          }
      in
      Ok (CallExpr { func; args; ty = J_type.from_json ty })
  | "CharacterLiteral" ->
      let* i = with_field "value" cast_int o in
      Ok (CharacterLiteral i)
  | "CXXConstCastExpr" | "CXXReinterpretCastExpr"
  | "ImplicitCastExpr" | "CXXStaticCastExpr" | "ConstantExpr" | "ParenExpr"
  | "ExprWithCleanups" | "CStyleCastExpr" | "CXXDefaultArgExpr" ->
      with_field "inner" (cast_list_1 parse_expr) o
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
      Ok (MemberExpr { name = n; base = b; ty = J_type.from_json ty })
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
      Ok (MemberExpr { name = n; base = b; ty = J_type.from_json ty })
  | "EnumConstantDecl" ->
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name; ty = J_type.from_json ty; kind = EnumConstant })
  | "VarDecl" | "VarTemplateSpecializationDecl" ->
      (* [VarTemplateSpecializationDecl] is a C++14 variable-template
         instantiation (e.g. [HASHTABLE_EMPTY_VALUE<uint64, uint32>]); it
         carries the same [name]/[type] shape as a plain [VarDecl]. *)
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name; ty = J_type.from_json ty; kind = Var })
  | "FunctionDecl" ->
      let* v = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name = v; ty = J_type.from_json ty; kind = Function })
  | "CXXMethodDecl" | "CXXConstructorDecl" | "CXXDestructorDecl"
  | "CXXConversionDecl" ->
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name; ty = J_type.from_json ty; kind = CXXMethod })
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
             ty = J_type.from_json ty;
           })
  | "UnaryExprOrTypeTraitExpr" ->
      let* ty = get_field "type" o in
      Ok (SizeOfExpr (J_type.from_json ty))
  | "ParmVarDecl" ->
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok (Ident { name; ty = J_type.from_json ty; kind = ParmVar })
  | "NonTypeTemplateParmDecl" ->
      let* name = parse_variable j in
      let* ty = get_field "type" o in
      Ok
        (Ident { name; ty = J_type.from_json ty; kind = NonTypeTemplateParm })
  | "UnresolvedLookupExpr" ->
      let* v = parse_variable j in
      let* tys = get_field "lookups" o >>= cast_list in
      Ok
        (UnresolvedLookupExpr
           { name = v; tys = List.map J_type.from_json tys })
  | "CXXNewExpr" ->
      let* arg = with_field "inner" (cast_first parse_expr) o in
      let* ty = get_field "type" o in
      Ok (CXXNewExpr { arg; ty = J_type.from_json ty })
  | "CXXDeleteExpr" ->
      let* arg = with_field "inner" (cast_list_1 parse_expr) o in
      let* ty = get_field "type" o in
      Ok (CXXDeleteExpr { arg; ty = J_type.from_json ty })
  | "UnaryOperator" ->
      let* op = with_field "opcode" cast_string o in
      let* c = with_field "inner" (cast_list_1 parse_expr) o in
      let* ty = get_field "type" o in
      let inc o =
        BinaryOperator
          {
            ty = J_type.from_json ty;
            opcode = "=";
            lhs = c;
            rhs =
              BinaryOperator
                {
                  ty = J_type.from_json ty;
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
        | "-" ->
            BinaryOperator
              {
                ty = J_type.from_json ty;
                opcode = op;
                lhs = IntegerLiteral 0;
                rhs = c;
              }
        | _ ->
            UnaryOperator { ty = J_type.from_json ty; opcode = op; child = c })
  | "CompoundAssignOperator" -> (
      (* Convert: x += e into x = x + y *)
      let* ty = get_field "computeResultType" o in
      let* lhs, rhs =
        with_field "inner" (cast_list_2 parse_expr parse_expr) o
      in
      let* opcode = with_field "opcode" cast_string o in
      match Common.rsplit '=' opcode with
      | Some (opcode, "") ->
          Ok (c_expr_compound (J_type.from_json ty) lhs opcode rhs)
      | _ -> root_cause "ERROR: parse_exp" j)
  | "BinaryOperator" ->
      let ty =
        List.assoc_opt "type" o
        |> Option.map J_type.from_json
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
             ty = J_type.from_json ty;
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
            let ty = J_type.from_json ty in
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
        | _ -> CXXOperatorCallExpr { func; args; ty = J_type.from_json ty })
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
      Ok (CallExpr { func; args; ty = J_type.from_json ty })
  | "CXXBindTemporaryExpr" | "CXXFunctionalCastExpr"
  | "MaterializeTemporaryExpr" | "CompoundLiteralExpr" ->
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
      let ty = J_type.from_json ty in
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
      Ok (CXXConstructExpr { args; ty = J_type.from_json ty })
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
      let ty = J_type.from_json ty in
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
      let is_op_method (item : json) : bool =
        let r =
          let* o = cast_object item in
          let* k = get_kind o in
          if k <> "CXXMethodDecl" then Ok false
          else
            let* nm = with_field_or "name" cast_string "" o in
            Ok (nm = "operator()")
        in
        Result.value ~default:false r
      in
      let* op_j =
        match List.find_opt is_op_method closure_inner with
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
          let* op_ty_o = cast_object op_ty_j in
          let* qt = with_field "qualType" cast_string op_ty_o in
          match String.index_opt qt '(' with
          | Some i ->
              let s = String.trim (String.sub qt 0 i) in
              Ok (J_type.from_json (`Assoc [ ("qualType", `String s) ]))
          | None -> Ok (J_type.from_json op_ty_j)
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
      Ok (InitListExpr { ty = J_type.from_json ty; args })
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
  if is_invalid o || is_tag_decl then Ok None
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
    let* attrs = map parse_attr attrs in
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
    let ty_var = Ty_variable.make ~name ~ty:(J_type.from_json ty) in
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
          (c_expr * int * J_type.t) option =
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
          let* ty_o = cast_object ty_j in
          let* qual_type = with_field "qualType" cast_string ty_o in
          let* arr_expr = with_field "inner" (cast_list_1 parse_expr) vo in
          match parse_array_bound qual_type with
          | Some n -> Ok (arr_expr, n, J_type.from_json ty_j)
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
      let parse_loop_var (lv_j : json) : (Variable.t * J_type.t) option =
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
          Ok (var, J_type.from_json ty_j)
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
  else
    let* value_opt = with_opt_field "value" cast_int o in
    let* type_opt =
      with_opt_field "type" (fun j -> Ok (J_type.from_json j)) o
    in
    let* name_opt = with_opt_field "name" cast_string o in
    match (value_opt, type_opt, name_opt) with
    | Some n, _, _ -> Ok (TArgIntegral n)
    | _, Some ty, _ -> Ok (TArgType ty)
    | _, _, Some n when is_expansion -> Ok (TArgTemplateExpansion n)
    | _, _, Some n -> Ok (TArgTemplate n)
    | _ -> root_cause "TemplateArgument: unrecognized shape" j

module Expr = struct
  type t = c_expr =
    | SizeOfExpr of J_type.t
    | CXXNewExpr of { arg : t; ty : J_type.t }
    | CXXDeleteExpr of { arg : t; ty : J_type.t }
    | RecoveryExpr of J_type.t
    | CharacterLiteral of int
    | ArraySubscriptExpr of c_array_subscript
    | BinaryOperator of c_binary
    | CallExpr of { func : t; args : t list; ty : J_type.t }
    | ConditionalOperator of {
        cond : t;
        then_expr : t;
        else_expr : t;
        ty : J_type.t;
      }
    | CXXConstructExpr of { args : t list; ty : J_type.t }
    | CXXBoolLiteralExpr of bool
    | Ident of Decl_expr.t
    | CXXOperatorCallExpr of { func : t; args : t list; ty : J_type.t }
    | FloatingLiteral of float
    | IntegerLiteral of int
    | MemberExpr of { name : string; base : t; ty : J_type.t }
    | UnaryOperator of { opcode : string; child : t; ty : J_type.t }
    | UnresolvedLookupExpr of { name : Variable.t; tys : J_type.t list }
    | StmtExpr of { body : c_stmt; result : t; ty : J_type.t }
    | LambdaExpr of {
        captures : (Variable.t * t) list;
        params : Param.t list;
        body : c_stmt;
        ret_ty : J_type.t;
      }
    | PackExpansion of t
    | DependentScopeRef of {
        name : string;
        nested_name_specifier : string option;
        ty : J_type.t;
      }

  type nonrec c_binary = c_binary = {
    opcode : string;
    lhs : t;
    rhs : t;
    ty : J_type.t;
  }

  type nonrec c_array_subscript = c_array_subscript = {
    lhs : t;
    rhs : t;
    ty : J_type.t;
    location : Location.t;
  }

  let rec to_type : t -> J_type.t = function
    | SizeOfExpr _ -> J_type.int
    | CXXNewExpr c -> c.ty
    | CXXDeleteExpr c -> c.ty
    | CXXConstructExpr c -> c.ty
    | CharacterLiteral _ -> J_type.char
    | ArraySubscriptExpr a -> a.ty
    | BinaryOperator a -> a.ty
    | ConditionalOperator c -> to_type c.then_expr
    | CXXBoolLiteralExpr _ -> J_type.bool
    | FloatingLiteral _ -> J_type.float
    | Ident a -> a.ty
    | IntegerLiteral _ -> J_type.int
    | UnaryOperator a -> a.ty
    | CallExpr c -> c.ty
    | CXXOperatorCallExpr a -> a.ty
    | MemberExpr a -> a.ty
    | UnresolvedLookupExpr _ -> J_type.unknown
    | RecoveryExpr ty -> ty
    | StmtExpr e -> e.ty
    | LambdaExpr _ ->
        (* Closure type — opaque from the analyser's POV before lifting. *)
        J_type.unknown
    | PackExpansion e -> to_type e
    | DependentScopeRef d -> d.ty

  let to_string ?(modifier : bool = false) ?(provenance : bool = false)
      ?(types : bool = false) : t -> string =
    let attr (s : string) : string = if modifier then "@" ^ s ^ " " else "" in
    let opcode (o : string) (j : J_type.t) : string =
      if types then "(" ^ o ^ "." ^ J_type.to_string j ^ ")" else o
    in
    let var_name : Variable.t -> string =
      if provenance then Variable.name_line else Variable.name
    in
    let rec exp_to_s : t -> string =
      let par (e : t) : string =
        match e with
        | BinaryOperator _ | ConditionalOperator _ -> "(" ^ exp_to_s e ^ ")"
        | UnaryOperator _ | CXXNewExpr _ | CXXDeleteExpr _ | Ident _
        | UnresolvedLookupExpr _ | CallExpr _ | CXXOperatorCallExpr _
        | CXXConstructExpr _ | CXXBoolLiteralExpr _ | ArraySubscriptExpr _
        | MemberExpr _ | IntegerLiteral _ | CharacterLiteral _ | RecoveryExpr _
        | FloatingLiteral _ | SizeOfExpr _ | StmtExpr _ | LambdaExpr _
        | PackExpansion _ | DependentScopeRef _ ->
            exp_to_s e
      in
      function
      | SizeOfExpr ty -> "sizeof(" ^ J_type.to_string ty ^ ")"
      | CXXNewExpr c ->
          "new " ^ J_type.to_string c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
      | CXXDeleteExpr c -> "del " ^ par c.arg
      | RecoveryExpr _ -> "?"
      | FloatingLiteral f -> string_of_float f
      | CharacterLiteral i | IntegerLiteral i -> string_of_int i
      | ConditionalOperator c ->
          par c.cond ^ " ? " ^ par c.then_expr ^ " : " ^ par c.else_expr
      | BinaryOperator b ->
          par b.lhs ^ " " ^ opcode b.opcode b.ty ^ " " ^ par b.rhs
      | MemberExpr m -> par m.base ^ "." ^ m.name
      | ArraySubscriptExpr b -> par b.lhs ^ "[" ^ exp_to_s b.rhs ^ "]"
      | CXXBoolLiteralExpr b -> if b then "true" else "false"
      | CXXConstructExpr c ->
          attr "ctor" ^ J_type.to_string c.ty ^ "(" ^ list_to_s exp_to_s c.args
          ^ ")"
      | CXXOperatorCallExpr c ->
          exp_to_s c.func ^ "[" ^ list_to_s exp_to_s c.args ^ "]"
      | Ident v -> Decl_expr.to_string ~modifier v
      | CallExpr c -> par c.func ^ "(" ^ list_to_s exp_to_s c.args ^ ")"
      | UnresolvedLookupExpr v -> attr "unresolv" ^ var_name v.name
      | UnaryOperator u -> u.opcode ^ par u.child
      | StmtExpr e -> "({ ...; " ^ exp_to_s e.result ^ "; })"
      | LambdaExpr e ->
          let cap =
            e.captures
            |> List.map (fun (v, _) -> Variable.name v)
            |> String.concat ", "
          in
          let par_names =
            e.params
            |> List.map (fun (p : Param.t) -> Variable.name p.ty_var.name)
            |> String.concat ", "
          in
          "[" ^ cap ^ "](" ^ par_names ^ ") { ... }"
      | PackExpansion e -> par e ^ "..."
      | DependentScopeRef d ->
          (match d.nested_name_specifier with
           | Some nns -> nns ^ d.name
           | None -> d.name)
    in
    exp_to_s

  let unknown : t = RecoveryExpr J_type.unknown

  let opt_to_string : t option -> string = function
    | Some o -> to_string o
    | None -> ""

  (* Free variable references in [e] at the expression level only:
     every [Ident] reachable through pure [Expr.t] structure, plus the
     captures' init exprs of any [LambdaExpr] and the [result] of any
     [StmtExpr]. The nested [c_stmt] bodies of [LambdaExpr] /
     [StmtExpr] are not descended into — that's [Stmt]'s job. Returned
     as a [Decl_expr.Set.t] so callers can union / filter / iterate
     deterministically. *)
  let rec shallow_free_vars : t -> Decl_expr.Set.t =
    let union_list (xs : Decl_expr.Set.t list) : Decl_expr.Set.t =
      List.fold_left Decl_expr.Set.union Decl_expr.Set.empty xs
    in
    function
    | Ident d -> Decl_expr.Set.singleton d
    | SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _
    | CXXBoolLiteralExpr _ | FloatingLiteral _ | IntegerLiteral _
    | UnresolvedLookupExpr _ | DependentScopeRef _ ->
        Decl_expr.Set.empty
    | CXXNewExpr { arg; _ } | CXXDeleteExpr { arg; _ }
    | UnaryOperator { child = arg; _ } | MemberExpr { base = arg; _ }
    | PackExpansion arg ->
        shallow_free_vars arg
    | ArraySubscriptExpr { lhs; rhs; _ }
    | BinaryOperator { lhs; rhs; _ } ->
        Decl_expr.Set.union (shallow_free_vars lhs) (shallow_free_vars rhs)
    | CallExpr { func; args; ty = _ }
    | CXXOperatorCallExpr { func; args; ty = _ } ->
        union_list (shallow_free_vars func :: List.map shallow_free_vars args)
    | ConditionalOperator { cond; then_expr; else_expr; _ } ->
        union_list
          [ shallow_free_vars cond;
            shallow_free_vars then_expr;
            shallow_free_vars else_expr ]
    | CXXConstructExpr { args; _ } ->
        union_list (List.map shallow_free_vars args)
    | StmtExpr { result; _ } ->
        (* Nested [body] is a [c_stmt] — outside this function's
           scope. We collect the result's free vars only. *)
        shallow_free_vars result
    | LambdaExpr { captures; _ } ->
        (* Captures' init exprs evaluate in the outer scope, so their
           free vars contribute. The [body] is a [c_stmt] — skipped
           here, same as [StmtExpr]. *)
        union_list (List.map (fun (_, init) -> shallow_free_vars init) captures)

  module Visit = struct
    type expr_t = t

    type 'a t =
      | SizeOf of J_type.t
      | CXXNew of { arg : 'a; ty : J_type.t }
      | CXXDelete of { arg : 'a; ty : J_type.t }
      | Recovery of J_type.t
      | CharacterLiteral of int
      | ArraySubscript of {
          lhs : 'a;
          rhs : 'a;
          ty : J_type.t;
          location : Location.t;
        }
      | BinaryOperator of { opcode : string; lhs : 'a; rhs : 'a; ty : J_type.t }
      | Call of { func : 'a; args : 'a list; ty : J_type.t }
      | ConditionalOperator of {
          cond : 'a;
          then_expr : 'a;
          else_expr : 'a;
          ty : J_type.t;
        }
      | CXXConstruct of { args : 'a list; ty : J_type.t }
      | CXXBoolLiteral of bool
      | Ident of Decl_expr.t
      | CXXOperatorCall of { func : 'a; args : 'a list; ty : J_type.t }
      | FloatingLiteral of float
      | IntegerLiteral of int
      | Member of { name : string; base : 'a; ty : J_type.t }
      | UnaryOperator of { opcode : string; child : 'a; ty : J_type.t }
      | UnresolvedLookup of { name : Variable.t; tys : J_type.t list }
      | StmtExpr of { body : c_stmt; result : 'a; ty : J_type.t }
      | LambdaExpr of {
          captures : (Variable.t * 'a) list;
          params : Param.t list;
          body : c_stmt;
          ret_ty : J_type.t;
        }
      | PackExpansion of 'a
      | DependentScopeRef of {
          name : string;
          nested_name_specifier : string option;
          ty : J_type.t;
        }

    let rec fold (f : 'a t -> 'a) : expr_t -> 'a = function
      | SizeOfExpr e -> f (SizeOf e)
      | CXXNewExpr e -> f (CXXNew { arg = fold f e.arg; ty = e.ty })
      | CXXDeleteExpr e -> f (CXXDelete { arg = fold f e.arg; ty = e.ty })
      | RecoveryExpr e -> f (Recovery e)
      | CharacterLiteral e -> f (CharacterLiteral e)
      | ArraySubscriptExpr e ->
          f
            (ArraySubscript
               {
                 lhs = fold f e.lhs;
                 rhs = fold f e.rhs;
                 ty = e.ty;
                 location = e.location;
               })
      | BinaryOperator e ->
          f
            (BinaryOperator
               {
                 opcode = e.opcode;
                 lhs = fold f e.lhs;
                 rhs = fold f e.rhs;
                 ty = e.ty;
               })
      | CallExpr e ->
          f
            (Call
               {
                 func = fold f e.func;
                 args = List.map (fold f) e.args;
                 ty = e.ty;
               })
      | ConditionalOperator e ->
          f
            (ConditionalOperator
               {
                 cond = fold f e.cond;
                 then_expr = fold f e.then_expr;
                 else_expr = fold f e.else_expr;
                 ty = e.ty;
               })
      | CXXConstructExpr e ->
          f (CXXConstruct { args = List.map (fold f) e.args; ty = e.ty })
      | CXXBoolLiteralExpr e -> f (CXXBoolLiteral e)
      | Ident e -> f (Ident e)
      | CXXOperatorCallExpr e ->
          f
            (CXXOperatorCall
               {
                 func = fold f e.func;
                 args = List.map (fold f) e.args;
                 ty = e.ty;
               })
      | FloatingLiteral e -> f (FloatingLiteral e)
      | IntegerLiteral e -> f (IntegerLiteral e)
      | MemberExpr e ->
          f (Member { name = e.name; base = fold f e.base; ty = e.ty })
      | UnaryOperator e ->
          f
            (UnaryOperator
               { opcode = e.opcode; child = fold f e.child; ty = e.ty })
      | UnresolvedLookupExpr e ->
          f (UnresolvedLookup { name = e.name; tys = e.tys })
      | StmtExpr e ->
          f (StmtExpr { body = e.body; result = fold f e.result; ty = e.ty })
      | LambdaExpr e ->
          f
            (LambdaExpr
               {
                 captures =
                   List.map (fun (v, c) -> (v, fold f c)) e.captures;
                 params = e.params;
                 body = e.body;
                 ret_ty = e.ret_ty;
               })
      | PackExpansion e -> f (PackExpansion (fold f e))
      | DependentScopeRef d ->
          f
            (DependentScopeRef
               {
                 name = d.name;
                 nested_name_specifier = d.nested_name_specifier;
                 ty = d.ty;
               })

    let rec map (f : expr_t -> expr_t) (e : expr_t) : expr_t =
      let ret : expr_t -> expr_t = map f in
      match e with
      | FloatingLiteral _ | IntegerLiteral _ | CharacterLiteral _ | Ident _
      | RecoveryExpr _ | SizeOfExpr _ | UnresolvedLookupExpr _
      | CXXBoolLiteralExpr _ | DependentScopeRef _ ->
          f e
      | CXXNewExpr { arg = a; ty } -> f (CXXNewExpr { arg = ret a; ty })
      | CXXDeleteExpr { arg = a; ty } -> f (CXXDeleteExpr { arg = ret a; ty })
      | ArraySubscriptExpr { lhs = e1; rhs = e2; ty; location = l } ->
          f
            (ArraySubscriptExpr { lhs = ret e1; rhs = ret e2; ty; location = l })
      | BinaryOperator { opcode = o; lhs = e1; rhs = e2; ty } ->
          f (BinaryOperator { opcode = o; lhs = ret e1; rhs = ret e2; ty })
      | CallExpr { func = e; args = l; ty } ->
          f (CallExpr { func = f e; args = List.map ret l; ty })
      | ConditionalOperator { cond = e1; then_expr = e2; else_expr = e3; ty } ->
          f
            (ConditionalOperator
               { cond = f e1; then_expr = ret e2; else_expr = ret e3; ty })
      | CXXConstructExpr { args = l; ty } ->
          f (CXXConstructExpr { args = List.map ret l; ty })
      | CXXOperatorCallExpr { func = e; args = l; ty } ->
          f (CXXOperatorCallExpr { func = ret e; args = List.map ret l; ty })
      | MemberExpr { name = x; base = e; ty } ->
          f (MemberExpr { name = x; base = ret e; ty })
      | UnaryOperator { opcode = o; child = e; ty } ->
          f (UnaryOperator { opcode = o; child = ret e; ty })
      | StmtExpr { body; result; ty } ->
          f (StmtExpr { body; result = ret result; ty })
      | LambdaExpr { captures; params; body; ret_ty } ->
          f
            (LambdaExpr
               {
                 captures = List.map (fun (v, c) -> (v, ret c)) captures;
                 params;
                 body;
                 ret_ty;
               })
      | PackExpansion e -> f (PackExpansion (ret e))
  end

  (* DependentScopeRef is a leaf — handled by the literal-and-leaf
     OR-pattern above. *)

  (** Remove comma operator *)
  let rec remove_comma : t -> t = function
    (* the main case is handling the comma operator *)
    | BinaryOperator { opcode = ","; rhs; _ } ->
        (* we discard the previous elements *)
        rhs
    | CXXNewExpr { arg; ty } -> CXXNewExpr { arg = remove_comma arg; ty }
    | CXXDeleteExpr { arg; ty } -> CXXDeleteExpr { arg = remove_comma arg; ty }
    | ArraySubscriptExpr { lhs; rhs; ty; location } ->
        ArraySubscriptExpr
          { lhs = remove_comma lhs; rhs = remove_comma rhs; ty; location }
    | BinaryOperator { opcode; lhs; rhs; ty } ->
        BinaryOperator
          { lhs = remove_comma lhs; rhs = remove_comma rhs; opcode; ty }
    | CallExpr { func; args; ty } ->
        CallExpr
          { func = remove_comma func; args = List.map remove_comma args; ty }
    | ConditionalOperator { cond; then_expr; else_expr; ty } ->
        ConditionalOperator
          {
            cond = remove_comma cond;
            then_expr = remove_comma then_expr;
            else_expr = remove_comma else_expr;
            ty;
          }
    | CXXConstructExpr { args; ty } ->
        CXXConstructExpr { args = List.map remove_comma args; ty }
    | CXXOperatorCallExpr { func; args; ty } ->
        CXXOperatorCallExpr
          { func = remove_comma func; args = List.map remove_comma args; ty }
    | MemberExpr { name; base; ty } ->
        MemberExpr { base = remove_comma base; ty; name }
    | UnaryOperator { opcode; child; ty } ->
        UnaryOperator { opcode; child = remove_comma child; ty }
    | StmtExpr { body; result; ty } ->
        StmtExpr { body; result = remove_comma result; ty }
    | LambdaExpr { captures; params; body; ret_ty } ->
        LambdaExpr
          {
            captures =
              List.map (fun (v, c) -> (v, remove_comma c)) captures;
            params;
            body;
            ret_ty;
          }
    | PackExpansion e -> PackExpansion (remove_comma e)
    | ( SizeOfExpr _ | FloatingLiteral _ | CXXBoolLiteralExpr _
      | UnresolvedLookupExpr _ | RecoveryExpr _ | CharacterLiteral _ | Ident _
      | IntegerLiteral _ | DependentScopeRef _ ) as e ->
        e

  (** Rewrites a comma operator, by returning the last expression and a list of
      side-effects. For instance, if you have i++, i < n this function would
      return i < n, [i++]

      Note that this will recursively iterate over all sub-comma expressions, so
      the output expressions will all be absent of a comma operator. *)
  let rewrite_comma : t -> t list * t =
    let open State.Syntax in
    let add (e : t) : (t list, unit) State.t =
      State.update (fun st -> e :: st)
    in
    let rec rw : t -> (t list, t) State.t = function
      (* the main case is handling the comma operator *)
      | BinaryOperator { opcode = ","; lhs; rhs; ty = _ } ->
          (* we hoist the left-hand side expression to the state,
           and we return the right-hand side expression *)
          let* lhs = rw lhs in
          let* rhs = rw rhs in
          let* () = add lhs in
          return rhs
      | SizeOfExpr j -> return (SizeOfExpr j)
      | CXXNewExpr { arg; ty } ->
          let* arg = rw arg in
          return (CXXNewExpr { arg; ty })
      | CXXDeleteExpr { arg; ty } ->
          let* arg = rw arg in
          return (CXXDeleteExpr { arg; ty })
      | RecoveryExpr ty -> return (RecoveryExpr ty)
      | CharacterLiteral l -> return (CharacterLiteral l)
      | ArraySubscriptExpr { lhs; rhs; ty; location } ->
          let* lhs = rw lhs in
          let* rhs = rw rhs in
          return (ArraySubscriptExpr { lhs; rhs; ty; location })
      | BinaryOperator { opcode; lhs; rhs; ty } ->
          let* lhs = rw lhs in
          let* rhs = rw rhs in
          return (BinaryOperator { opcode; lhs; rhs; ty })
      | CallExpr { func; args; ty } ->
          let* func = rw func in
          let* args = State.list_map rw args in
          return (CallExpr { func; args; ty })
      | ConditionalOperator { cond; then_expr; else_expr; ty } ->
          let* cond = rw cond in
          let* then_expr = rw then_expr in
          let* else_expr = rw else_expr in
          return (ConditionalOperator { cond; then_expr; else_expr; ty })
      | CXXConstructExpr { args; ty } ->
          let* args = State.list_map rw args in
          return (CXXConstructExpr { args; ty })
      | CXXBoolLiteralExpr b -> return (CXXBoolLiteralExpr b)
      | Ident i -> return (Ident i)
      | CXXOperatorCallExpr { func; args; ty } ->
          let* func = rw func in
          let* args = State.list_map rw args in
          return (CXXOperatorCallExpr { func; args; ty })
      | FloatingLiteral l -> return (FloatingLiteral l)
      | IntegerLiteral l -> return (IntegerLiteral l)
      | MemberExpr { name; base; ty } ->
          let* base = rw base in
          return (MemberExpr { name; base; ty })
      | UnaryOperator { opcode; child; ty } ->
          let* child = rw child in
          return (UnaryOperator { opcode; child; ty })
      | UnresolvedLookupExpr { name; tys } ->
          return (UnresolvedLookupExpr { name; tys })
      | StmtExpr { body; result; ty } ->
          let* result = rw result in
          return (StmtExpr { body; result; ty })
      | LambdaExpr { captures; params; body; ret_ty } ->
          let* captures =
            State.list_map
              (fun (v, c) ->
                let* c = rw c in
                return (v, c))
              captures
          in
          return (LambdaExpr { captures; params; body; ret_ty })
      | PackExpansion e ->
          let* e = rw e in
          return (PackExpansion e)
      | DependentScopeRef d -> return (DependentScopeRef d)
    in
    fun e ->
      let st, e = State.run (rw e) [] in
      (List.rev st, e)

  let compound (ty : J_type.t) (lhs : t) (opcode : string) (rhs : t) : t =
    BinaryOperator
      { ty; opcode = "="; lhs; rhs = BinaryOperator { ty; opcode; lhs; rhs } }

  let parse : json -> t j_result = parse_expr
end

module Init = struct
  type t = c_init =
    | InitListExpr of { ty : J_type.t; args : Expr.t list }
    | IExpr of Expr.t

  let map_expr (f : Expr.t -> Expr.t) : t -> t = function
    | InitListExpr { ty; args = l } -> InitListExpr { ty; args = List.map f l }
    | IExpr e -> IExpr (f e)

  let to_expr_seq : t -> Expr.t Seq.t = function
    | InitListExpr l -> List.to_seq l.args
    | IExpr e -> Seq.return e

  let to_string : t -> string = function
    | InitListExpr i -> list_to_s Expr.to_string i.args
    | IExpr i -> Expr.to_string i

  let parse : json -> t j_result = parse_init
end

let c_attr (k : string) : string = "__attribute__((" ^ k ^ "))"
let c_attr_shared = c_attr "shared"
let c_attr_global = c_attr "global"
let c_attr_device = c_attr "device"
let c_attr_constant = c_attr "constant"
let c_attr_managed = c_attr "managed"

module Decl : sig
  type t = c_decl = {
    var : Variable.t;
    ty : J_type.t;
    init : Init.t option;
    attrs : string list;
  }

  (* Constructor *)
  val make :
    ty_var:Ty_variable.t -> init:Init.t option -> attrs:string list -> t

  (* Accessors *)
  val attrs : t -> string list
  val init : t -> Init.t option

  (* Expression iterator *)
  val to_expr_seq : t -> Expr.t Seq.t

  (* Update contained expressions *)
  val map_expr : (Expr.t -> Expr.t) -> t -> t

  (* Show its contents *)
  val to_string : t -> string

  (* Convinience *)
  val is_shared : t -> bool
  val matches : (C_type.t -> bool) -> t -> bool
  val var : t -> Variable.t
  val ty : t -> J_type.t
  val to_s : t -> Indent.t list
  val parse : Yojson.Basic.t -> t option j_result
end = struct
  type t = c_decl = {
    var : Variable.t;
    ty : J_type.t;
    init : Init.t option;
    attrs : string list;
  }

  let make ~ty_var ~init ~attrs : t =
    { ty = ty_var.ty; var = Ty_variable.name ty_var; init; attrs }

  let init (x : t) : Init.t option = x.init
  let attrs (x : t) : string list = x.attrs
  let var (x : t) : Variable.t = x.var
  let ty (x : t) : J_type.t = x.ty
  let matches pred (x : t) = J_type.matches pred x.ty
  let is_shared (x : t) : bool = List.mem c_attr_shared x.attrs

  let to_expr_seq (x : t) : Expr.t Seq.t =
    match x.init with Some i -> Init.to_expr_seq i | None -> Seq.empty

  let map_expr (f : Expr.t -> Expr.t) (x : t) : t =
    { x with init = x.init |> Option.map (Init.map_expr f) }

  let to_string (d : t) : string =
    let i =
      match d.init with Some e -> " = " ^ Init.to_string e | None -> ""
    in
    let attr =
      if d.attrs = [] then ""
      else
        let attrs = String.concat " " d.attrs |> String.trim in
        attrs ^ " "
    in
    attr ^ J_type.to_string d.ty ^ " " ^ Variable.name d.var ^ i

  let to_s (d : t) : Indent.t list = [ Line (to_string d ^ ";") ]

  let parse : json -> t option j_result = parse_decl
end

module ForInit = struct
  type t = c_for_init = Decls of Decl.t list | Expr of Expr.t

  (* Iterate over the expressions contained in a for-init *)
  let to_expr_seq : t -> Expr.t Seq.t = function
    | Decls l -> List.to_seq l |> Seq.concat_map Decl.to_expr_seq
    | Expr e -> Seq.return e

  (* Returns the binders of a for statement *)
  let loop_vars : t -> Variable.t list =
    let rec exp_var (e : Expr.t) : Variable.t list =
      match e with
      | BinaryOperator { lhs = l; opcode = ","; rhs = r; _ } ->
          exp_var l |> Common.append_rev1 (exp_var r)
      | BinaryOperator { lhs = Ident l; opcode = "="; _ } -> [ l.name ]
      | _ -> []
    in
    function Decls l -> List.map Decl.var l | Expr e -> exp_var e

  let to_string : t -> string = function
    | Decls d -> list_to_s Decl.to_string d
    | Expr e -> Expr.to_string e

  let opt_to_string : t option -> string = function
    | Some o -> to_string o
    | None -> ""

  let parse : json -> t j_result = parse_for_init
end

module Stmt = struct
  type 'a if_t = 'a c_if = {
    cond : Expr.t;
    then_stmt : 'a;
    else_stmt : 'a;
  }

  type 'a cond_t = 'a c_cond = { cond : Expr.t; body : 'a }

  type 'a for_t = 'a c_for = {
    init : ForInit.t option;
    cond : Expr.t option;
    inc : 'a;
    body : 'a;
  }

  type 'a case_t = 'a c_case = { case : Expr.t; body : 'a }

  type t = c_stmt =
    | Skip
    | BreakStmt
    | GotoStmt
    | ReturnStmt of Expr.t option
    | ContinueStmt
    | IfStmt of t if_t
    | DeclStmt of Decl.t list
    | WhileStmt of t cond_t
    | ForStmt of t for_t
    | DoStmt of t cond_t
    | SwitchStmt of t cond_t
    | DefaultStmt of t
    | CaseStmt of t case_t
    | SExpr of Expr.t
    | AsmStmt of Expr.t Asm.t
    | BarrierOp of {
        op : BarrierOp.t;
        target : Expr.t;
        args : Expr.t list;
        loc : Location.t option;
      }
    | Seq of t * t

  type if_stmt = t if_t
  type cond_stmt = t cond_t
  type for_stmt = t for_t
  type case_stmt = t for_t

  let expr (e : Expr.t) : t = SExpr e

  let rec to_s : t -> Indent.t list = function
    | ReturnStmt None -> [ Line "return;" ]
    | ReturnStmt (Some e) -> [ Line ("return " ^ Expr.to_string e ^ ";") ]
    | GotoStmt -> [ Line "goto" ]
    | BreakStmt -> [ Line "break" ]
    | ContinueStmt -> [ Line "continue" ]
    | ForStmt f ->
        let inc = to_string ~inline:true f.inc in
        let open Indent in
        [
          Line
            ("for ("
            ^ ForInit.opt_to_string f.init
            ^ "; " ^ Expr.opt_to_string f.cond ^ "; " ^ inc ^ ") {");
          Block (to_s f.body);
          Line "}";
        ]
    | WhileStmt { cond = b; body = s } ->
        [
          Line ("while (" ^ Expr.to_string b ^ ") {"); Block (to_s s); Line "}";
        ]
    | DoStmt { cond = b; body = s } ->
        [ Line "}"; Block (to_s s); Line ("do (" ^ Expr.to_string b ^ ") {") ]
    | SwitchStmt { cond = b; body = s } ->
        [ Line ("switch " ^ Expr.to_string b ^ " {"); Block (to_s s); Line "}" ]
    | CaseStmt c ->
        [ Line ("case " ^ Expr.to_string c.case ^ ":"); Block (to_s c.body) ]
    | DefaultStmt d -> [ Line "default:"; Block (to_s d) ]
    | IfStmt { then_stmt = Skip; else_stmt = Skip; _ } -> [ Line ";" ]
    | IfStmt { cond = b; then_stmt = s1; else_stmt = s2 } ->
        let s1 = to_s s1 in
        let s2 =
          let open Indent in
          if s2 = Skip then [] else [ Line "} else {"; Block (to_s s2) ]
        in
        let open Indent in
        [ Line ("if (" ^ Expr.to_string b ^ ") {"); Block s1 ]
        @ s2 @ [ Line "}" ]
    | DeclStmt [] -> []
    | DeclStmt [ d ] -> [ Line ("decl " ^ Decl.to_string d) ]
    | DeclStmt d ->
        let open Indent in
        [
          Line "decl {";
          Block (List.map (fun e -> Line (Decl.to_string e)) d);
          Line "}";
        ]
    | SExpr e -> [ Line (Expr.to_string e ^ ";") ]
    | AsmStmt a -> [ Line (Asm.to_string Expr.to_string a ^ ";") ]
    | BarrierOp { op; target; args; _ } ->
        let args_s =
          if args = [] then ""
          else "(" ^ list_to_s Expr.to_string args ^ ")"
        in
        [
          Line
            (Expr.to_string target
            ^ "." ^ BarrierOp.to_string op ^ args_s ^ ";");
        ]
    | Seq (s1, s2) -> to_s s1 @ to_s s2
    | Skip -> [ Line ";" ]

  and to_string ?(inline = false) (s : t) : string =
    s |> to_s |> Indent.to_string |> fun s ->
    if inline then s |> Common.replace ~substring:"\n" ~by:" " |> String.trim
    else s

  module Visit = struct
    type c_stmt = t

    type 'a t =
      | Break
      | Goto
      | Return of Expr.t option
      | Continue
      | If of 'a if_t
      | Decl of Decl.t list
      | While of 'a cond_t
      | For of 'a for_t
      | Do of 'a cond_t
      | Switch of 'a cond_t
      | Default of 'a
      | Case of 'a case_t
      | SExpr of Expr.t
      | Asm of Expr.t Asm.t
      | Barrier of {
          op : BarrierOp.t;
          target : Expr.t;
          args : Expr.t list;
          loc : Location.t option;
        }
      | Seq of ('a * 'a)
      | Skip

    let rec fold (f : 'a t -> 'a) : c_stmt -> 'a = function
      | BreakStmt -> f Break
      | GotoStmt -> f Goto
      | ReturnStmt e -> f (Return e)
      | ContinueStmt -> f Continue
      | IfStmt c ->
          f
            (If
               {
                 cond = c.cond;
                 then_stmt = fold f c.then_stmt;
                 else_stmt = fold f c.else_stmt;
               })
      | DeclStmt l -> f (Decl l)
      | WhileStmt w -> f (While { cond = w.cond; body = fold f w.body })
      | ForStmt s ->
          f
            (For
               {
                 init = s.init;
                 cond = s.cond;
                 inc = fold f s.inc;
                 body = fold f s.body;
               })
      | DoStmt s -> f (Do { cond = s.cond; body = fold f s.body })
      | SwitchStmt s -> f (Switch { cond = s.cond; body = fold f s.body })
      | DefaultStmt s -> f (Default (fold f s))
      | CaseStmt s -> f (Case { case = s.case; body = fold f s.body })
      | SExpr e -> f (SExpr e)
      | AsmStmt a -> f (Asm a)
      | BarrierOp { op; target; args; loc } ->
          f (Barrier { op; target; args; loc })
      | Seq (s1, s2) -> f (Seq (fold f s1, fold f s2))
      | Skip -> f Skip

    let map (f : c_stmt -> c_stmt) : c_stmt -> c_stmt =
      fold (function
        | Break -> f BreakStmt
        | Goto -> f GotoStmt
        | Return e -> f (ReturnStmt e)
        | Continue -> f ContinueStmt
        | If c -> f (IfStmt c)
        | Decl l -> f (DeclStmt l)
        | While c -> f (WhileStmt c)
        | For c -> f (ForStmt c)
        | Do c -> f (DoStmt c)
        | Switch c -> f (SwitchStmt c)
        | Default c -> f (DefaultStmt c)
        | Case c -> f (CaseStmt c)
        | SExpr e -> f (SExpr e)
        | Asm a -> f (AsmStmt a)
        | Barrier { op; target; args; loc } ->
            f (BarrierOp { op; target; args; loc })
        | Seq (s1, s2) -> f (Seq (s1, s2))
        | Skip -> f Skip)

    let to_expr_seq : c_stmt -> Expr.t Seq.t =
      fold (function
        | Skip | Break | Goto | Return None | Continue -> Seq.empty
        | Return (Some e) -> Seq.return e
        | If { cond = c; then_stmt = s1; else_stmt = s2 } ->
            Seq.return c |> Seq.append s1 |> Seq.append s2
        | Decl d ->
            List.to_seq d
            |> Seq.concat_map (fun d ->
                Decl.init d |> Option.to_seq |> Seq.concat_map Init.to_expr_seq)
        | While { cond = c; body = b }
        | Do { cond = c; body = b }
        | Switch { cond = c; body = b }
        | Case { case = c; body = b } ->
            Seq.cons c b
        | For s -> Option.to_seq s.init |> Seq.concat_map ForInit.to_expr_seq
        | Default s -> s
        | SExpr e -> Seq.return e
        | Asm a ->
            let operand_exprs os = List.to_seq os |> Seq.map (fun o -> o.Asm.expr) in
            Seq.append (operand_exprs a.Asm.outputs) (operand_exprs a.Asm.inputs)
        | Barrier { target; args; _ } ->
            Seq.cons target (List.to_seq args)
        | Seq (s1, s2) -> Seq.append s1 s2)
  end

  let rec find (f : t -> bool) (s : t) : t option =
    if f s then Some s
    else
      match s with
      | Skip | BreakStmt | GotoStmt | ReturnStmt _ | ContinueStmt | DeclStmt _
      | SExpr _ | AsmStmt _ | BarrierOp _ ->
          None
      | Seq (s1, s2) | IfStmt { then_stmt = s1; else_stmt = s2; _ } -> (
          match find f s1 with Some s -> Some s | None -> find f s2)
      | WhileStmt { body = s; _ }
      | DoStmt { body = s; _ }
      | SwitchStmt { body = s; _ }
      | CaseStmt { body = s; _ }
      | DefaultStmt s
      | ForStmt { body = s; _ } ->
          find f s

  let member (f : t -> bool) (s : t) : bool = find f s |> Option.is_some

  let rec fold : 'a. (t -> 'a -> 'a) -> t -> 'a -> 'a =
   fun f (s : t) (init : 'a) ->
    let init : 'a = f s init in
    match s with
    | Skip | BreakStmt | GotoStmt | ReturnStmt _ | ContinueStmt | DeclStmt _
    | SExpr _ | AsmStmt _ | BarrierOp _ ->
        init
    | IfStmt { then_stmt = s1; else_stmt = s2; _ } ->
        let init : 'a = fold f s1 init in
        fold f s2 init
    | WhileStmt { body = s; _ }
    | DoStmt { body = s; _ }
    | SwitchStmt { body = s; _ }
    | CaseStmt { body = s; _ }
    | DefaultStmt s
    | ForStmt { body = s; _ } ->
        fold f s init
    | Seq (s1, s2) -> fold f s1 init |> fold f s2

  (* Returns all elements that match a given predicate *)
  let find_all_map (f : t -> 'a option) (s : t) : 'a Seq.t =
    let g (e : t) (r : 'a Seq.t) : 'a Seq.t =
      match f e with Some x -> Seq.cons x r | None -> r
    in
    fold g s Seq.empty

  let seq (s1 : t) (s2 : t) : t =
    if s1 = Skip then s2 else if s2 = Skip then s1 else Seq (s1, s2)

  let find_all (f : t -> bool) : t -> t Seq.t =
    find_all_map (fun x -> if f x then Some x else None)

  let from_list (l : t list) : t = List.fold_left seq Skip l

  let rewrite_comma : t -> t =
    let open State.Syntax in
    let to_stmt (l : Expr.t list) : t =
      match l with
      | x :: l -> List.fold_left (fun s e -> Seq (SExpr e, s)) (SExpr x) l
      | [] -> Skip
    in

    let add (s : t) : (t, unit) State.t =
      if s = Skip then return () else State.update (fun s' -> seq s' s)
    in

    let rewrite_expr (e : Expr.t) : (t, Expr.t) State.t =
      let st, e = Expr.rewrite_comma e in
      let* () = add (to_stmt st) in
      return e
    in

    let opt_rewrite_comma (o : Expr.t option) : Expr.t list * Expr.t option =
      match o with
      | Some e ->
          let st, e = Expr.rewrite_comma e in
          (st, Some e)
      | None -> ([], None)
    in

    let run (m : (t, unit) State.t) : t = State.run_update m Skip in

    let rec rw : t -> t = function
      | ReturnStmt None -> ReturnStmt None
      | ReturnStmt (Some e) ->
          run
            (let* e = rewrite_expr e in
             add (ReturnStmt (Some e)))
      | IfStmt { cond; then_stmt; else_stmt } ->
          run
            (let* cond = rewrite_expr cond in
             let then_stmt = rw then_stmt in
             let else_stmt = rw else_stmt in
             add (IfStmt { cond; then_stmt; else_stmt }))
      | WhileStmt { cond; body } ->
          let st, cond = Expr.rewrite_comma cond in
          if st = [] then
            (* easy case, no commas in the condition *)
            WhileStmt { cond; body = rw body }
          else
            let s = to_stmt st in
            (* when there are commas in the condition, we need to
           append the commas to the end of the loop body, and before
           the loop too *)
            let body = Seq (rw body, s) in
            Seq (s, WhileStmt { cond; body })
      | DoStmt { cond; body } ->
          let st, cond = Expr.rewrite_comma cond in
          let body = seq (to_stmt st) (rw body) in
          DoStmt { cond; body }
      | ForStmt { init; cond; inc; body } ->
          (* this works as a combination of a while and a do-loop *)
          let st_cond, cond = opt_rewrite_comma cond in
          let inc = rw inc in
          if st_cond = [] then ForStmt { init; cond; inc; body = rw body }
          else
            let st_cond = to_stmt st_cond in
            (* add commas at the end of the body *)
            let body = Seq (rw body, st_cond) in
            Seq
              ( (* pre-pend the commas of the condition *)
                st_cond,
                ForStmt { init; cond; inc; body } )
      (* simple propagation *)
      | CaseStmt { case; body } ->
          run
            (let* case = rewrite_expr case in
             add (CaseStmt { case; body = rw body }))
      | DefaultStmt s -> DefaultStmt (rw s)
      | SwitchStmt { cond; body } ->
          run
            (let* cond = rewrite_expr cond in
             add (SwitchStmt { cond; body = rw body }))
      | Seq (s1, s2) -> Seq (rw s1, rw s2)
      | SExpr e ->
          run
            (let* e = rewrite_expr e in
             add (SExpr e))
      | s -> s
    in
    rw

  (* Recognize barrier method calls and lift them out of expression position
     into [BarrierOp] stmt nodes. Runs after comma rewriting so barrier args
     have already been normalized. Strict type guard: only fires when the
     method's receiver has desugared type [cuda::barrier<_>]. *)
  let rewrite_barriers : t -> t =
    let try_lift ?(loc : Location.t option = None) (e : Expr.t) : t option =
      match e with
      | CXXOperatorCallExpr
          { func = MemberExpr { base; name; _ }; args; _ } -> (
          match BarrierOp.of_method_name name with
          | Some op when BarrierOp.is_barrier_base_type (Expr.to_type base) ->
              Some (BarrierOp { op; target = base; args; loc })
          | _ -> None)
      | _ -> None
    in
    let rewrite_decl (d : Decl.t) : t =
      match Decl.init d with
      | Some (IExpr e) -> (
          match try_lift e with
          | Some b_stmt ->
              (* Drop the token-binding decl; the [BarrierOp] replaces it. *)
              b_stmt
          | None -> DeclStmt [ d ])
      | _ -> DeclStmt [ d ]
    in
    let rec rw : t -> t = function
      | SExpr e -> (
          match try_lift e with Some b -> b | None -> SExpr e)
      | DeclStmt l ->
          l |> List.map rewrite_decl |> from_list
      | IfStmt { cond; then_stmt; else_stmt } ->
          IfStmt { cond; then_stmt = rw then_stmt; else_stmt = rw else_stmt }
      | WhileStmt { cond; body } -> WhileStmt { cond; body = rw body }
      | DoStmt { cond; body } -> DoStmt { cond; body = rw body }
      | ForStmt { init; cond; inc; body } ->
          ForStmt { init; cond; inc = rw inc; body = rw body }
      | SwitchStmt { cond; body } -> SwitchStmt { cond; body = rw body }
      | CaseStmt { case; body } -> CaseStmt { case; body = rw body }
      | DefaultStmt s -> DefaultStmt (rw s)
      | Seq (s1, s2) -> Seq (rw s1, rw s2)
      | s -> s
    in
    rw

  let parse : json -> t j_result = parse_stmt
  let parse_list : json -> t j_result = parse_stmt_list
end

(* ──────────────────────────────────────────────────────────────────
   StmtExpr hoisting pass
   ──────────────────────────────────────────────────────────────────

   GCC statement expressions [({ s1; ... ; e; })] embed statements
   inside expressions. They are eliminated here, before the rest of
   the pipeline (notably [Stmt.rewrite_comma] and [D_lang.rewrite_*])
   sees the AST. The pass is the StmtExpr analogue of [rewrite_comma]:
   it pulls statement-shaped side effects out of expressions and
   stitches them into the enclosing statement scope.

   - [rewrite_expr_stmtexpr e] returns a pair [(prefix, residual)]
     such that evaluating [e] is semantically equivalent to executing
     [prefix] then evaluating [residual]. Walking down the expression
     tree, every [StmtExpr { body; result; _ }] contributes its body
     to the prefix and continues recursion on [result].

   - [rewrite_stmt_stmtexpr s] walks each statement context, calls
     [rewrite_expr_stmtexpr] on every contained expression, and
     prepends the prefix statements at the right point. For loop
     conditions we duplicate the prefix at top-and-tail (mirroring
     [rewrite_comma]) so that side-effects re-execute on every
     iteration along with the condition.

   Caveat: hoisting unconditionally past a [ConditionalOperator] is
   unsound for [StmtExpr]s nested inside a conditional branch, since
   the prefix would execute regardless of the branch taken. We
   currently treat both branches as always-executed; this matches
   what [rewrite_comma] does for commas in conditionals and is sound
   for the macro-hygiene patterns ([ppcg_min] and friends) that
   dominate real-world StmtExpr usage. If a kernel exercises the
   nested-in-branch case we can refine later. *)

let rec rewrite_expr_stmtexpr (e : c_expr) : c_stmt * c_expr =
  match e with
  | StmtExpr { body; result; _ } ->
      let body' = rewrite_stmt_stmtexpr body in
      let prefix_inner, residual = rewrite_expr_stmtexpr result in
      (Stmt.seq body' prefix_inner, residual)
  | SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _ | CXXBoolLiteralExpr _
  | FloatingLiteral _ | IntegerLiteral _ | Ident _ | UnresolvedLookupExpr _
  | DependentScopeRef _ ->
      (Skip, e)
  | CXXNewExpr { arg; ty } ->
      let s, arg = rewrite_expr_stmtexpr arg in
      (s, CXXNewExpr { arg; ty })
  | CXXDeleteExpr { arg; ty } ->
      let s, arg = rewrite_expr_stmtexpr arg in
      (s, CXXDeleteExpr { arg; ty })
  | ArraySubscriptExpr { lhs; rhs; ty; location } ->
      let s1, lhs = rewrite_expr_stmtexpr lhs in
      let s2, rhs = rewrite_expr_stmtexpr rhs in
      (Stmt.seq s1 s2, ArraySubscriptExpr { lhs; rhs; ty; location })
  | BinaryOperator { opcode; lhs; rhs; ty } ->
      let s1, lhs = rewrite_expr_stmtexpr lhs in
      let s2, rhs = rewrite_expr_stmtexpr rhs in
      (Stmt.seq s1 s2, BinaryOperator { opcode; lhs; rhs; ty })
  | CallExpr { func; args; ty } ->
      let sf, func = rewrite_expr_stmtexpr func in
      let sa, args = rewrite_expr_list_stmtexpr args in
      (Stmt.seq sf sa, CallExpr { func; args; ty })
  | ConditionalOperator { cond; then_expr; else_expr; ty } ->
      let sc, cond = rewrite_expr_stmtexpr cond in
      let st, then_expr = rewrite_expr_stmtexpr then_expr in
      let se, else_expr = rewrite_expr_stmtexpr else_expr in
      ( Stmt.seq sc (Stmt.seq st se),
        ConditionalOperator { cond; then_expr; else_expr; ty } )
  | CXXConstructExpr { args; ty } ->
      let s, args = rewrite_expr_list_stmtexpr args in
      (s, CXXConstructExpr { args; ty })
  | CXXOperatorCallExpr { func; args; ty } ->
      let sf, func = rewrite_expr_stmtexpr func in
      let sa, args = rewrite_expr_list_stmtexpr args in
      (Stmt.seq sf sa, CXXOperatorCallExpr { func; args; ty })
  | MemberExpr { name; base; ty } ->
      let s, base = rewrite_expr_stmtexpr base in
      (s, MemberExpr { name; base; ty })
  | UnaryOperator { opcode; child; ty } ->
      let s, child = rewrite_expr_stmtexpr child in
      (s, UnaryOperator { opcode; child; ty })
  | LambdaExpr { captures; params; body; ret_ty } ->
      (* Capture initializers are evaluated at the lambda's declaration
         site; lift any StmtExprs in them out into the enclosing scope.
         The body is opaque here — when [Lambda_lift] hoists it to a
         synthetic function, [rewrite_stmt_stmtexpr] runs on that
         function's body independently. *)
      let s, captures =
        List.fold_left
          (fun (prefix, acc) (v, c) ->
            let s, c = rewrite_expr_stmtexpr c in
            (Stmt.seq prefix s, (v, c) :: acc))
          (Skip, []) captures
      in
      ( s,
        LambdaExpr
          {
            captures = List.rev captures;
            params;
            body;
            ret_ty;
          } )
  | PackExpansion e ->
      let s, e = rewrite_expr_stmtexpr e in
      (s, PackExpansion e)

and rewrite_expr_list_stmtexpr (es : c_expr list) : c_stmt * c_expr list =
  let prefix, residuals =
    List.fold_left
      (fun (prefix, acc) e ->
        let s, e = rewrite_expr_stmtexpr e in
        (Stmt.seq prefix s, e :: acc))
      (Skip, []) es
  in
  (prefix, List.rev residuals)

and rewrite_expr_opt_stmtexpr : c_expr option -> c_stmt * c_expr option =
  function
  | None -> (Skip, None)
  | Some e ->
      let s, e = rewrite_expr_stmtexpr e in
      (s, Some e)

and rewrite_init_stmtexpr (i : c_init) : c_stmt * c_init =
  match i with
  | IExpr e ->
      let s, e = rewrite_expr_stmtexpr e in
      (s, IExpr e)
  | InitListExpr { ty; args } ->
      let s, args = rewrite_expr_list_stmtexpr args in
      (s, InitListExpr { ty; args })

and rewrite_decl_stmtexpr (d : c_decl) : c_stmt * c_decl =
  match d.init with
  | None -> (Skip, d)
  | Some i ->
      let s, i = rewrite_init_stmtexpr i in
      (s, { d with init = Some i })

and rewrite_decls_stmtexpr (ds : c_decl list) : c_stmt * c_decl list =
  let prefix, residuals =
    List.fold_left
      (fun (prefix, acc) d ->
        let s, d = rewrite_decl_stmtexpr d in
        (Stmt.seq prefix s, d :: acc))
      (Skip, []) ds
  in
  (prefix, List.rev residuals)

and rewrite_for_init_stmtexpr (f : c_for_init) : c_stmt * c_for_init =
  match f with
  | Decls ds ->
      let s, ds = rewrite_decls_stmtexpr ds in
      (s, Decls ds)
  | Expr e ->
      let s, e = rewrite_expr_stmtexpr e in
      (s, Expr e)

and rewrite_stmt_stmtexpr (s : c_stmt) : c_stmt =
  match s with
  | Skip | BreakStmt | GotoStmt | ContinueStmt | ReturnStmt None -> s
  | ReturnStmt (Some e) ->
      let prefix, e = rewrite_expr_stmtexpr e in
      Stmt.seq prefix (ReturnStmt (Some e))
  | IfStmt { cond; then_stmt; else_stmt } ->
      let prefix, cond = rewrite_expr_stmtexpr cond in
      Stmt.seq prefix
        (IfStmt
           {
             cond;
             then_stmt = rewrite_stmt_stmtexpr then_stmt;
             else_stmt = rewrite_stmt_stmtexpr else_stmt;
           })
  | DeclStmt ds ->
      let prefix, ds = rewrite_decls_stmtexpr ds in
      Stmt.seq prefix (DeclStmt ds)
  | WhileStmt { cond; body } ->
      let prefix, cond = rewrite_expr_stmtexpr cond in
      let body = rewrite_stmt_stmtexpr body in
      if prefix = Skip then WhileStmt { cond; body }
      else
        (* Prefix re-runs each iteration: prepend before the loop AND
           append at the end of the body so the next iteration's
           condition evaluates with fresh side-effects. Mirrors
           [rewrite_comma]'s loop-cond duplication. *)
        Stmt.seq prefix
          (WhileStmt { cond; body = Stmt.seq body prefix })
  | DoStmt { cond; body } ->
      let prefix, cond = rewrite_expr_stmtexpr cond in
      let body = rewrite_stmt_stmtexpr body in
      DoStmt { cond; body = Stmt.seq body prefix }
  | ForStmt { init; cond; inc; body } ->
      let s_init, init =
        match init with
        | None -> (Skip, None)
        | Some f ->
            let s, f = rewrite_for_init_stmtexpr f in
            (s, Some f)
      in
      let s_cond, cond = rewrite_expr_opt_stmtexpr cond in
      let inc = rewrite_stmt_stmtexpr inc in
      let body = rewrite_stmt_stmtexpr body in
      let body =
        if s_cond = Skip then body else Stmt.seq body s_cond
      in
      Stmt.seq s_init
        (Stmt.seq s_cond (ForStmt { init; cond; inc; body }))
  | SwitchStmt { cond; body } ->
      let prefix, cond = rewrite_expr_stmtexpr cond in
      Stmt.seq prefix
        (SwitchStmt { cond; body = rewrite_stmt_stmtexpr body })
  | CaseStmt { case; body } ->
      let prefix, case = rewrite_expr_stmtexpr case in
      Stmt.seq prefix
        (CaseStmt { case; body = rewrite_stmt_stmtexpr body })
  | DefaultStmt s -> DefaultStmt (rewrite_stmt_stmtexpr s)
  | SExpr e ->
      let prefix, e = rewrite_expr_stmtexpr e in
      Stmt.seq prefix (SExpr e)
  | AsmStmt a ->
      let rewrite_operand (op : c_expr Asm.operand) :
          c_stmt * c_expr Asm.operand =
        let s, expr = rewrite_expr_stmtexpr op.expr in
        (s, { Asm.constr = op.constr; expr })
      in
      let s_outs, outputs =
        List.fold_left
          (fun (prefix, acc) op ->
            let s, op = rewrite_operand op in
            (Stmt.seq prefix s, op :: acc))
          (Skip, []) a.outputs
      in
      let s_ins, inputs =
        List.fold_left
          (fun (prefix, acc) op ->
            let s, op = rewrite_operand op in
            (Stmt.seq prefix s, op :: acc))
          (Skip, []) a.inputs
      in
      Stmt.seq (Stmt.seq s_outs s_ins)
        (AsmStmt { a with outputs = List.rev outputs; inputs = List.rev inputs })
  | BarrierOp { op; target; args; loc } ->
      let st, target = rewrite_expr_stmtexpr target in
      let sa, args = rewrite_expr_list_stmtexpr args in
      Stmt.seq (Stmt.seq st sa) (BarrierOp { op; target; args; loc })
  | Seq (s1, s2) ->
      Stmt.seq (rewrite_stmt_stmtexpr s1) (rewrite_stmt_stmtexpr s2)

module KernelAttr = struct
  type t = Default | Auxiliary

  let to_string : t -> string = function
    | Default -> "__global__"
    | Auxiliary -> "__device__"

  let is_global : t -> bool = function Default -> true | Auxiliary -> false
  let is_device : t -> bool = function Default -> false | Auxiliary -> true

  let parse (x : string) : t option =
    if x = c_attr_global then Some Default
    else if x = c_attr_device then Some Auxiliary
    else None

  let can_parse (x : string) : bool = parse x |> Option.is_some
end

module Ty_param = struct
  type t =
    | TemplateType of Variable.t
    | NonTypeTemplate of { name : Variable.t; ty : J_type.t }

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
              Ok (Variable.make ~name ~location)
        in
        let* ty = get_field "type" o in
        Ok (Some (NonTypeTemplate { name; ty = J_type.from_json ty }))
    | _ -> Ok None
end

module TemplateArgument = struct
  type t = c_template_argument =
    | TArgType of J_type.t
    | TArgIntegral of int
    | TArgNullArg
    | TArgNullPtr
    | TArgDecl of string
    | TArgExpr of c_expr
    | TArgPack of t list
    | TArgTemplate of string
    | TArgTemplateExpansion of string

  let parse = parse_c_template_argument

  let rec to_string : t -> string = function
    | TArgType ty -> J_type.to_string ty
    | TArgIntegral n -> string_of_int n
    | TArgNullArg -> "<null>"
    | TArgNullPtr -> "nullptr"
    | TArgDecl n -> n
    | TArgExpr _ -> "<expr>"
    | TArgPack xs -> "{" ^ list_to_s to_string xs ^ "}"
    | TArgTemplate n -> n
    | TArgTemplateExpansion n -> n ^ "..."
end

module Specialization_kind = struct
  type t =
    | Undeclared
    | ImplicitInstantiation
    | ExplicitSpecialization
    | ExplicitInstantiationDeclaration
    | ExplicitInstantiationDefinition

  let parse (s : string) : t option =
    match s with
    | "Undeclared" -> Some Undeclared
    | "ImplicitInstantiation" -> Some ImplicitInstantiation
    | "ExplicitSpecialization" -> Some ExplicitSpecialization
    | "ExplicitInstantiationDeclaration" ->
        Some ExplicitInstantiationDeclaration
    | "ExplicitInstantiationDefinition" ->
        Some ExplicitInstantiationDefinition
    | _ -> None

  let to_string : t -> string = function
    | Undeclared -> "Undeclared"
    | ImplicitInstantiation -> "ImplicitInstantiation"
    | ExplicitSpecialization -> "ExplicitSpecialization"
    | ExplicitInstantiationDeclaration -> "ExplicitInstantiationDeclaration"
    | ExplicitInstantiationDefinition -> "ExplicitInstantiationDefinition"
end

module Kernel = struct
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
  }

  let make ~ty ~name ~code ~type_params ~params ~attribute
      ~template_args ~specialization_kind ~primary_template_name =
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
    }

  let name (x : t) : string = x.name
  let params (x : t) : Param.t list = x.params
  let type_params (x : t) : Ty_param.t list = x.type_params
  let attribute (x : t) : KernelAttr.t = x.attribute
  let template_args (x : t) : TemplateArgument.t list = x.template_args

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
    let code = rewrite_stmt_stmtexpr k.code in
    { k with code = Stmt.rewrite_comma code }

  let rewrite_barriers (k : t) : t =
    { k with code = Stmt.rewrite_barriers k.code }

  (* Returns whether the kernel has a __global__ modifier *)
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
        ^ ")");
    ]
    @ Stmt.to_s k.code

  let wrap_error (msg : string) (j : Yojson.Basic.t) :
      'a j_result -> 'a j_result = function
    | Ok e -> Ok e
    | Error e -> Rjson.because msg j e

  let parse (type_params : Ty_param.t list) (j : Yojson.Basic.t) : t j_result =
    let open Rjson in
    (let* o = cast_object j in
     let* ty = get_field "type" o |> Result.map J_type.from_json in
     let ty = J_type.to_string ty in
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
     (* we can safely convert the option with Option.get because parse_kernel
        is only invoked when we are able to parse *)
     let m : KernelAttr.t =
       List.find_map KernelAttr.parse attrs |> Option.get
     in
     let* body : Stmt.t = Stmt.parse_list (`List body) in
     let* name : string = with_field "name" cast_string o in
     (* Parameters may be faulty, recover: *)
     let ps = List.map Param.parse ps |> List.concat_map Result.to_list in
     (* Phase-2 metadata (specialisations only): the resolved template
        arguments, the kind of specialisation, and the primary
        template's name. Absent on the primary template — those fields
        are emitted only on instantiation FunctionDecls. *)
     let* template_args =
       with_field_or "templateArgs" (cast_map parse_c_template_argument) [] o
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
     Ok
       (make ~ty ~name ~code:body ~params:ps ~type_params ~attribute:m
          ~template_args ~specialization_kind ~primary_template_name))
    |> wrap_error "Kernel" j
end

(* Bare-decl-ref shape (e.g. [{kind: "FunctionDecl", name, type}]) used
   as the [kernel] / [host_function] / [referencedDecl] fields on
   c-to-json's launch metadata. parse_expr already produces an [Ident]
   for these shapes, so we just unwrap. *)
let parse_bare_decl_ref (j : Yojson.Basic.t) : Decl_expr.t Rjson.j_result =
  let open Rjson in
  let* e = parse_expr j in
  match e with
  | Ident d -> Ok d
  | _ -> root_cause "parse_bare_decl_ref: expected an Ident" j

(* One [ConstBinding] entry inside [LaunchParam.const_bindings]. c-to-json
   surfaces every host-local [const]-qualified variable reachable from the
   launch's emitted expressions paired with its initialiser, so equalities
   like [inum == numk * 1024] reach faial without the emitter rewriting use
   sites: [inum] stays a named [DeclRefExpr] in the grid expression, the
   path condition, and any kernel arg, and the equality travels alongside.

   c-to-json filters by type-system const + no address-taken — the strongest
   immutability guarantee available without value reconstruction — so each
   binding is sound as a launch-time hypothesis. The init expression is
   already resolved (const-fold + trivial-init substitution + pure-helper
   inlining), so it round-trips through [parse_expr] like any other slot. *)
module ConstBinding = struct
  type t = {
    name : Variable.t;
    ty : J_type.t;
    init : c_expr;
  }

  let parse (j : Yojson.Basic.t) : t Rjson.j_result =
    let open Rjson in
    (let* o = cast_object j in
     let* () = expect_kind "ConstBinding" o in
     let* name_str = with_field "name" cast_string o in
     let* ty = get_field "type" o in
     let* inner = with_field "inner" (cast_map parse_expr) o in
     let* init =
       match inner with
       | [ e ] -> Ok e
       | _ ->
           root_cause
             "ConstBinding: expected exactly one init Expr in inner[]" j
     in
     Ok
       {
         name = Variable.from_name name_str;
         ty = J_type.from_json ty;
         init;
       })
    |> Rjson.add_reason "ConstBinding" j

  let to_string (b : t) : string =
    Variable.name b.name ^ " = " ^ Expr.to_string b.init
end

(* Parse the [const_bindings] wrapper:
     {kind: "ConstBindings", inner: [<ConstBinding>...]}
   c-to-json wraps the list in a single named slot for streamer-layout
   reasons (two labeled-array slots at the same level malform the JSON);
   on this side we just project [inner]. *)
let parse_const_bindings (j : Yojson.Basic.t) : ConstBinding.t list Rjson.j_result =
  let open Rjson in
  (let* o = cast_object j in
   let* () = expect_kind "ConstBindings" o in
   with_field "inner" (cast_map ConstBinding.parse) o)
  |> Rjson.add_reason "ConstBindings" j

module LaunchParam = struct
  (* One CUDA launch site, populated from c-to-json's [LaunchParam] node
     in TranslationUnitDecl.inner[]. The expression slots (grid / block
     / shared_mem / stream / args) are real AST subtrees that round-trip
     through [parse_expr]: c-to-json's resolution policy const-folds
     where possible and emits the original AST otherwise, but every
     shape parses with the existing [c_expr] arms. *)
  type t = {
    loc : Location.t;
    kernel : Decl_expr.t;
    host_function : Decl_expr.t option;
    template_args : TemplateArgument.t list;
    launch_api : string option;
    grid : c_expr;
    block : c_expr;
    shared_mem : c_expr;
    stream : c_expr;
    args : c_expr list;
    (* Sound conjunction of host-side guards (from enclosing
       [if]/[while]/[for]) that hold whenever this launch executes,
       as emitted by c-to-json's [path_condition] slot. The dropper
       on the c-to-json side excludes anything potentially mutated
       between the guard's branch entry and the launch — calls,
       members, escaped locals, side effects — so what survives is
       always pure arithmetic / boolean over [Ident]s and literals
       that [Launch_arg.lift_pure] handles directly. Absent when no
       conjunct survives the soundness check. *)
    path_condition : c_expr option;
    (* Host-local [const]-qualified variables reachable from the
       launch's emitted expressions, paired with their initialisers.
       Surfaces equalities like [inum == numk * 1024] so a downstream
       consumer can conjoin them to the wrapper invariant without
       rewriting use sites — [inum] stays a named identifier in the
       grid expression, the path condition, and any kernel arg.
       Empty when c-to-json's BFS admits no bindings. *)
    const_bindings : ConstBinding.t list;
    notes : string option;
  }

  let parse (j : Yojson.Basic.t) : t Rjson.j_result =
    let open Rjson in
    (let* o = cast_object j in
     let* loc = with_field "range" parse_location o in
     let* kernel = with_field "kernel" parse_bare_decl_ref o in
     let* host_function =
       with_opt_field "host_function" parse_bare_decl_ref o
     in
     let* template_args =
       with_field_or "template_args"
         (cast_map parse_c_template_argument) [] o
     in
     let* launch_api = with_opt_field "launch_api" cast_string o in
     let* grid = with_field "grid" parse_expr o in
     let* block = with_field "block" parse_expr o in
     let* shared_mem = with_field "shared_mem" parse_expr o in
     let* stream = with_field "stream" parse_expr o in
     let* args = with_field_or "args" (cast_map parse_expr) [] o in
     let* path_condition = with_opt_field "path_condition" parse_expr o in
     let* const_bindings =
       with_field_or "const_bindings" parse_const_bindings [] o
     in
     let* notes = with_opt_field "notes" cast_string o in
     Ok
       {
         loc;
         kernel;
         host_function;
         template_args;
         launch_api;
         grid;
         block;
         shared_mem;
         stream;
         args;
         path_condition;
         const_bindings;
         notes;
       })
    |> Rjson.add_reason "LaunchParam" j

  let to_s (lp : t) : Indent.t list =
    let targs =
      if lp.template_args <> [] then
        "<" ^ list_to_s TemplateArgument.to_string lp.template_args ^ ">"
      else ""
    in
    let host =
      match lp.host_function with
      | Some h -> " in " ^ Variable.name h.name
      | None -> ""
    in
    let pc =
      match lp.path_condition with
      | Some e -> " when " ^ Expr.to_string e
      | None -> ""
    in
    let cb =
      if lp.const_bindings = [] then ""
      else " where " ^ list_to_s ConstBinding.to_string lp.const_bindings
    in
    [
      Indent.Line
        ("<<<launch>>> "
        ^ Variable.name lp.kernel.name
        ^ targs ^ host ^ "(" ^ list_to_s Expr.to_string lp.args ^ ")"
        ^ pc ^ cb);
    ]

  (* Free variables referenced anywhere in the launch's expression
     slots — grid / block / shared_mem / stream / args / path_condition
     / each const binding's init. The binding's [name] (LHS) is
     already covered by the other slot walks (c-to-json's BFS only
     admits a binding when its name is reachable from a slot Expr);
     walking inits surfaces vars referenced *inside* an init (e.g.
     [numk] in [inum = numk * 1024]). All ident kinds are returned;
     callers filter (e.g. by [Decl_expr.is_runtime_value]) as needed. *)
  let free_vars (lp : t) : Decl_expr.Set.t =
    let exprs =
      [ lp.grid; lp.block; lp.shared_mem; lp.stream ]
      @ lp.args
      @ Option.to_list lp.path_condition
      @ List.map (fun (b : ConstBinding.t) -> b.init) lp.const_bindings
    in
    List.fold_left
      (fun acc e -> Decl_expr.Set.union acc (Expr.shallow_free_vars e))
      Decl_expr.Set.empty exprs
end

(* c-to-json emits a [LaunchParamWarning] node for every [<<<>>>] /
   [cudaLaunchKernel] site whose callee can't be resolved to a
   [FunctionDecl] — function-pointer kernels, dependent
   unresolved-lookups, helper-wrapped launches. Faial doesn't carry
   these in the AST: we have no consumer for them, and the textual
   warning at parse time is sufficient observability. If a downstream
   stage ever needs to enumerate or count unresolvable launches, this
   should grow back into a [Def.t] variant. *)
let log_launch_param_warning (j : Yojson.Basic.t) : unit Rjson.j_result =
  let open Rjson in
  let* o = cast_object j in
  (* c-to-json emits [range] for resolvable LaunchParam nodes but
     sometimes only [loc] for the warning variant; accept either. *)
  let* loc =
    match List.assoc_opt "range" o with
    | Some r -> parse_location r
    | None -> with_field "loc" (parse_position ?filename:None) o
  in
  let* reason = with_field "reason" cast_string o in
  let* host_function =
    with_opt_field "host_function" parse_bare_decl_ref o
  in
  let host =
    match host_function with
    | Some h -> " in " ^ Variable.name h.name
    | None -> ""
  in
  prerr_endline
    ("WARNING: unresolved launch at " ^ Location.to_string loc ^ host
    ^ ": " ^ reason);
  Ok ()

module Def = struct
  type t =
    | Kernel of Kernel.t
    | Declaration of Decl.t
    | Typedef of Typedef.t
    | Enum of Imp.Enum.t
    | LaunchParam of LaunchParam.t

  let remove_comma : t -> t = function
    | Kernel k -> Kernel (Kernel.rewrite_comma k)
    | Declaration d -> Declaration (Decl.map_expr Expr.remove_comma d)
    | (Typedef _ | Enum _ | LaunchParam _) as d -> d

  let rewrite_barriers : t -> t = function
    | Kernel k -> Kernel (Kernel.rewrite_barriers k)
    | (Declaration _ | Typedef _ | Enum _ | LaunchParam _) as d -> d

  let to_s (d : t) : Indent.t list =
    match d with
    | Declaration d -> Decl.to_s d
    | Kernel k -> Kernel.to_s k
    | Typedef d -> Typedef.to_s d
    | Enum e -> Imp.Enum.to_s e
    | LaunchParam lp -> LaunchParam.to_s lp

  (* Function that checks if a variable is of type array and is being used *)
  let has_array_type (j : Yojson.Basic.t) : bool =
    let open Rjson in
    let is_array =
      let* o = cast_object j in
      let* ty = get_field "type" o |> Result.map J_type.from_json in
      Ok (J_type.matches C_type.is_array ty)
    in
    is_array |> Result.value ~default:false

  let is_kernel (j : Yojson.Basic.t) : bool =
    let open Rjson in
    let is_kernel =
      let* o = cast_object j in
      let* k = get_kind o in
      if k = "FunctionDecl" then
        let* inner = with_field "inner" cast_list o in
        let attrs, inner =
          inner
          |> List.partition (j_filter_kind (String.ends_with ~suffix:"Attr"))
        in
        (* Try to parse attrs *)
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
          | Some KernelAttr.Auxiliary ->
              true
              (* We only care about __device__ functions that manipulate arrays *)
              (*         List.exists has_array_type params *))
      else Ok false
    in
    is_kernel |> Result.value ~default:false

  let parse_constant (j : Yojson.Basic.t) : Imp.Enum.Constant.t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* _ = expect_kind "EnumConstantDecl" o in
    let* var = parse_variable j in
    (* The init expression may be a bare [IntegerLiteral] or a clang
       [ConstantExpr] wrapper carrying the already-evaluated value as
       a string (used for any non-trivial init like [-1] or [1 << 3]).
       Prefer the pre-evaluated [value] field when present. *)
    let parse_init (j : json) : int option j_result =
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
      else
        let* e = Expr.parse j in
        match e with
        | IntegerLiteral n -> Ok (Some n)
        | _ -> root_cause "Expecting an integer, but got something else" j
    in
    (* Skip non-init siblings (notably trailing FullComment doc
       comments — clang attaches them to the EnumConstantDecl when
       the enumerator has an inline `/** ... */`). *)
    let is_doc_comment : json -> bool =
      j_filter_kind (fun k -> k = "FullComment")
    in
    let* init =
      with_field_or "inner"
        (fun j ->
          let* l = cast_list j in
          let l = List.filter (fun x -> not (is_doc_comment x)) l in
          (* No init expression — the enumerator inherits [previous + 1]
             (or 0 when first). We don't model that here; just record
             [None] so the caller skips the binding. *)
          match l with [] -> Ok None | _ -> cast_list_1 parse_init (`List l))
        None o
    in
    let open Imp.Enum.Constant in
    Ok { var; init }

  let parse_enum (j : Yojson.Basic.t) : Imp.Enum.t j_result =
    let open Rjson in
    let open Imp.Enum in
    let* o = cast_object j in
    (* Skip non-EnumConstantDecl children (notably FullComment doc
       comments interspersed between enumerators). *)
    let is_constant : Yojson.Basic.t -> bool =
      j_filter_kind (fun k -> k = "EnumConstantDecl")
    in
    let* var =
      match parse_variable j with
      | Ok v -> Ok v
      | Error _ ->
          (* Anonymous enum: derive a name from the first
             EnumConstantDecl's type qualType (clang renders these as
             e.g. "Matrix::(unnamed enum at .../main.cu:92:3)"). Skip
             non-EnumConstantDecl children first so a leading FullComment
             can't trip the lookup. *)
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
                first |> member "type" |> member "qualType"
                |> to_string_option
            | [] -> None
          in
          (match name with
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

  let rec parse (j : Yojson.Basic.t) : t list j_result =
    let open Rjson in
    let* o = cast_object j in
    let* k = get_kind o in
    let parse_k (type_params : Ty_param.t list) (j : Yojson.Basic.t) :
        t list j_result =
      if is_kernel j then
        let* k = Kernel.parse type_params j in
        if k.code = Skip then Ok [] else Ok [ Kernel k ]
      else Ok []
    in
    match k with
    | "FunctionTemplateDecl" ->
        (* [inner] holds the template parameters first
           (TemplateTypeParmDecl / NonTypeTemplateParmDecl), then the
           primary FunctionDecl, then any implicit/explicit
           specialisations emitted by writeTemplateDecl
           (JSONNodeDumper.h). The primary carries dependent types
           (T *, etc.); specialisations carry concrete substituted
           types and bodies. When specialisations exist they
           supersede the primary for analysis. *)
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
        (* Specialisation FunctionDecls carry [templateArgs] (and a
           [specializationKind]) — the primary template doesn't. Use
           that as the discriminator rather than position; it survives
           reorderings and is the same predicate the JSON itself
           guarantees. When any specialisation is present, drop the
           primary; otherwise keep the primary as the analysable
           kernel. *)
        let is_specialization (j : Yojson.Basic.t) : bool =
          match j with
          | `Assoc o' -> (
              match List.assoc_opt "templateArgs" o' with
              | Some (`List (_ :: _)) -> true
              | _ -> false)
          | _ -> false
        in
        let primaries, specs = List.partition (fun j -> not (is_specialization j)) fdecls in
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
    | "FunctionDecl" -> parse_k [] j
    | "VarDecl" -> (
        match Decl.parse j with
        | Ok (Some d) -> Ok [ Declaration d ]
        | _ -> Ok [])
    | "LinkageSpecDecl" | "NamespaceDecl" ->
        let* defs = with_field_or "inner" (cast_map parse) [] o in
        Ok (List.concat defs)
    | "TypedefDecl" | "TypeAliasDecl" -> (
        let* name = with_field "name" cast_string o in
        let* ty = get_field "type" o |> Result.map J_type.from_json in
        (* Prefer the desugared form so aliases like
           [using barrier_t = cuda::barrier<...>] resolve all the way. *)
        let ty = J_type.from_c_type (J_type.to_desugared_c_type ty) in
        match J_type.to_c_type_res ty with
        | Ok ty ->
            if
              C_type.is_struct ty || C_type.is_array ty || C_type.is_function ty
            then Ok []
            else Ok [ Typedef { name; ty } ]
        | Error _ -> Ok [])
    | "EnumDecl" ->
        let* e = parse_enum j in
        Ok [ Enum e ]
    | "LaunchParam" ->
        let* lp = LaunchParam.parse j in
        Ok [ LaunchParam lp ]
    | "LaunchParamWarning" ->
        let* () = log_launch_param_warning j in
        Ok []
    | _ -> Ok []
end

module Program = struct
  open Stage0

  type t = Def.t list
  type 'a state = (Variable.Set.t, 'a) State.t

  let rewrite_shared_arrays : t -> t =
    let open Stage0.State.Syntax in
    (* Rewrites expressions: when it finds a variable that has been defined as
      a shared variable, we replace that by an array subscript:
      x becomes x[0] *)
    let rw_exp (vars : Variable.Set.t) (e : Expr.t) : Expr.t =
      if Variable.Set.is_empty vars then e
      else
        e
        |> Expr.Visit.map (fun e ->
            match e with
            | Ident x ->
                if Variable.Set.mem x.name vars then
                  ArraySubscriptExpr
                    {
                      lhs = Ident x;
                      rhs = IntegerLiteral 0;
                      ty = x.ty;
                      location = Variable.location x.name;
                    }
                else e
            | _ -> e)
    in
    (* When rewriting a variable declaration, we must return as the side-effect
      the shadowing of the available variables when it makes sense *)
    let rw_decl (d : Decl.t) : Decl.t state =
      State.update_return (fun vars ->
          let vars =
            let name = Decl.var d in
            if Decl.is_shared d && not (Decl.matches C_type.is_array d) then
              Variable.Set.add name vars
            else Variable.Set.remove name vars
          in
          (vars, Decl.map_expr (rw_exp vars) d))
    in

    (* We declare a scope where side effects (variable declarations) are
       contained *)
    let scope (m : 'a state) : 'a state =
      State.update_return (fun s -> (s, State.run_result m s))
    in

    (* We now rewrite statements *)
    let rw_stmt (vars : Variable.Set.t) : Stmt.t -> Stmt.t =
      let rw_e (e : Expr.t) : Expr.t state =
        State.get_return (fun vars -> rw_exp vars e)
      in
      let rec rw_s : Stmt.t -> Stmt.t state =
        let open Stmt in
        function
        | (Skip | BreakStmt | GotoStmt | ReturnStmt None | ContinueStmt) as s ->
            State.return s
        | ReturnStmt (Some e) ->
            let* e = rw_e e in
            return (ReturnStmt (Some e))
        | IfStmt { cond; then_stmt; else_stmt } ->
            let* cond = rw_e cond in
            let* then_stmt = scope (rw_s then_stmt) in
            let* else_stmt = scope (rw_s else_stmt) in
            return (IfStmt { cond; then_stmt; else_stmt })
        | Seq (s1, s2) ->
            let* s1 = rw_s s1 in
            let* s2 = rw_s s2 in
            return (Seq (s1, s2))
        | DeclStmt l ->
            (* Variable declaration introduces a scope *)
            let* l = State.list_map rw_decl l in
            return (DeclStmt l)
        | WhileStmt { cond; body } ->
            let* cond = rw_e cond in
            let* body = scope (rw_s body) in
            return (WhileStmt { cond; body })
        | ForStmt { init; cond; inc; body } ->
            (* Since the init may declare a scope, we must contain
             the side-effects *)
            scope
              (let* init =
                 match init with
                 | None -> return None
                 | Some d ->
                     let* d =
                       match d with
                       | ForInit.Decls l ->
                           let* l = State.list_map rw_decl l in
                           return (ForInit.Decls l)
                       | ForInit.Expr e ->
                           let* e = rw_e e in
                           return (ForInit.Expr e)
                     in
                     return (Some d)
               in
               let* cond = State.option_map rw_e cond in
               let* inc = rw_s inc in
               let* body = rw_s body in
               return (ForStmt { init; cond; inc; body }))
        | DoStmt { cond; body } ->
            scope
              (let* cond = rw_e cond in
               let* body = rw_s body in
               return (DoStmt { cond; body }))
        | SwitchStmt { cond; body } ->
            scope
              (let* cond = rw_e cond in
               let* body = rw_s body in
               return (SwitchStmt { cond; body }))
        | DefaultStmt s ->
            scope
              (let* s = rw_s s in
               return (DefaultStmt s))
        | CaseStmt { case; body } ->
            let* case = rw_e case in
            let* body = rw_s body in
            return (CaseStmt { case; body })
        | SExpr e ->
            let* e = rw_e e in
            return (SExpr e)
        | AsmStmt a ->
            let rw_op (op : Expr.t Asm.operand) : Expr.t Asm.operand state =
              let* expr = rw_e op.expr in
              return { Asm.constr = op.constr; expr }
            in
            let* outputs = State.list_map rw_op a.outputs in
            let* inputs = State.list_map rw_op a.inputs in
            return (AsmStmt { a with outputs; inputs })
        | BarrierOp { op; target; args; loc } ->
            let* target = rw_e target in
            let* args = State.list_map rw_e args in
            return (BarrierOp { op; target; args; loc })
      in
      fun s -> State.run_result (rw_s s) vars
    in
    let rec rw_p (vars : Variable.Set.t) : t -> t = function
      | Declaration d :: p ->
          let vars =
            if Decl.is_shared d && not (Decl.matches C_type.is_array d) then
              Variable.Set.add (Decl.var d) vars
            else vars
          in
          Declaration d :: rw_p vars p
      | Kernel k :: p ->
          Kernel { k with code = rw_stmt vars k.code } :: rw_p vars p
      | Typedef d :: p -> Typedef d :: rw_p vars p
      | Enum e :: p -> Enum e :: rw_p vars p
      | LaunchParam lp :: p -> LaunchParam lp :: rw_p vars p
      | [] -> []
    in
    rw_p Variable.Set.empty

  let remove_comma : t -> t = List.map Def.remove_comma
  let rewrite_barriers : t -> t = List.map Def.rewrite_barriers

  let to_s (p : t) : Indent.t list =
    List.concat_map (fun k -> Def.to_s k @ [ Line "" ]) p

  let print (p : t) : unit = Indent.print (to_s p)
  let filter (pred : Def.t -> bool) (p : t) : t = List.filter pred p

  let parse ?(remove_commas = true) ?(rewrite_shared_variables = true)
      (j : Yojson.Basic.t) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* inner = with_field "inner" (cast_map Def.parse) o in
    let p = List.concat inner in
    let p = if rewrite_shared_variables then rewrite_shared_arrays p else p in
    let p = if remove_commas then remove_comma p else p in
    let p = rewrite_barriers p in
    Ok p
end

(* ------------------------------------------------- *)

(* ------------------------------------------------------------------------ *)
