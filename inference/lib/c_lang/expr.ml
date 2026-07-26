open Stage0
open Protocols
open Ast

type t = c_expr =
  | SizeOfExpr of Ty.t
  | CXXNewExpr of { arg : t; ty : Ty.t }
  | CXXDeleteExpr of { arg : t; ty : Ty.t }
  | RecoveryExpr of Ty.t
  | CharacterLiteral of int
  | ArraySubscriptExpr of c_array_subscript
  | BinaryOperator of c_binary
  | CallExpr of { func : t; args : t list; ty : Ty.t }
  | ConditionalOperator of {
      cond : t;
      then_expr : t;
      else_expr : t;
      ty : Ty.t;
    }
  | CXXConstructExpr of { args : t list; ty : Ty.t }
  | CXXBoolLiteralExpr of bool
  | Ident of Decl_expr.t
  | CXXOperatorCallExpr of { func : t; args : t list; ty : Ty.t }
  | FloatingLiteral of float
  | IntegerLiteral of int
  | MemberExpr of { name : string; base : t; ty : Ty.t }
  | UnaryOperator of { opcode : string; child : t; ty : Ty.t }
  | UnresolvedLookupExpr of { name : Variable.t; tys : Ty.t list }
  | StmtExpr of { body : c_stmt; result : t; ty : Ty.t }
  | LambdaExpr of {
      captures : (Variable.t * t) list;
      params : Param.t list;
      body : c_stmt;
      ret_ty : Ty.t;
    }
  | PackExpansion of t
  | DependentScopeRef of {
      name : string;
      nested_name_specifier : string option;
      ty : Ty.t;
    }

type nonrec c_binary = c_binary = {
  opcode : string;
  lhs : t;
  rhs : t;
  ty : Ty.t;
}

type nonrec c_array_subscript = c_array_subscript = {
  lhs : t;
  rhs : t;
  ty : Ty.t;
  location : Location.t;
}

let rec to_type : t -> Ty.t = function
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
  let opcode (o : string) (j : Ty.t) : string =
    if types then "(" ^ o ^ "." ^ Ty.to_string j ^ ")" else o
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
    | SizeOfExpr ty -> "sizeof(" ^ Ty.to_string ty ^ ")"
    | CXXNewExpr c ->
        "new " ^ Ty.to_string c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
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
        attr "ctor" ^ Ty.to_string c.ty ^ "(" ^ list_to_s exp_to_s c.args
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
    | SizeOf of Ty.t
    | CXXNew of { arg : 'a; ty : Ty.t }
    | CXXDelete of { arg : 'a; ty : Ty.t }
    | Recovery of Ty.t
    | CharacterLiteral of int
    | ArraySubscript of {
        lhs : 'a;
        rhs : 'a;
        ty : Ty.t;
        location : Location.t;
      }
    | BinaryOperator of { opcode : string; lhs : 'a; rhs : 'a; ty : Ty.t }
    | Call of { func : 'a; args : 'a list; ty : Ty.t }
    | ConditionalOperator of {
        cond : 'a;
        then_expr : 'a;
        else_expr : 'a;
        ty : Ty.t;
      }
    | CXXConstruct of { args : 'a list; ty : Ty.t }
    | CXXBoolLiteral of bool
    | Ident of Decl_expr.t
    | CXXOperatorCall of { func : 'a; args : 'a list; ty : Ty.t }
    | FloatingLiteral of float
    | IntegerLiteral of int
    | Member of { name : string; base : 'a; ty : Ty.t }
    | UnaryOperator of { opcode : string; child : 'a; ty : Ty.t }
    | UnresolvedLookup of { name : Variable.t; tys : Ty.t list }
    | StmtExpr of { body : c_stmt; result : 'a; ty : Ty.t }
    | LambdaExpr of {
        captures : (Variable.t * 'a) list;
        params : Param.t list;
        body : c_stmt;
        ret_ty : Ty.t;
      }
    | PackExpansion of 'a
    | DependentScopeRef of {
        name : string;
        nested_name_specifier : string option;
        ty : Ty.t;
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

let compound (ty : Ty.t) (lhs : t) (opcode : string) (rhs : t) : t =
  BinaryOperator
    { ty; opcode = "="; lhs; rhs = BinaryOperator { ty; opcode; lhs; rhs } }

let parse : Parse_util.json -> t Parse_util.j_result = Parsers.parse_expr
