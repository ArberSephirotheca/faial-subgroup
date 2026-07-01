open Stage0
open Protocols

let list_to_s (f : 'a -> string) (l : 'a list) : string =
  List.map f l |> String.concat ", "

(* The C-AST types are mutually recursive: [c_expr] needs to embed
   [c_stmt] (via [StmtExpr], the GCC statement expression), and
   [c_stmt] embeds [c_expr] in conditions, returns, etc. Defining the
   types here lets us hand them out from the [Expr], [Init], [Decl],
   [ForInit], and [Stmt] modules via the [type t = origin = | C1 ... |
   Cn ...] re-export pattern, so that [Expr.t = c_expr] etc. and
   [Expr.BinaryOperator] / [Stmt.IfStmt] / etc. remain accessible at
   their existing module paths. *)
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
      op : Barrier_op.t;
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

let c_attr (k : string) : string = "__attribute__((" ^ k ^ "))"
let c_attr_shared = c_attr "shared"
let c_attr_global = c_attr "global"
let c_attr_device = c_attr "device"
let c_attr_constant = c_attr "constant"
let c_attr_managed = c_attr "managed"

(* Synthetic tag, not a source attribute: recorded when cu-to-json marks
   a file-scope variable [mutated: false], i.e. it proved the global is
   never written after initialization. The lowering folds the
   initializer of a global only when it carries this tag (or is
   [const]); absence means treat the global as mutable. *)
let c_attr_immutable = c_attr "faial_immutable"
