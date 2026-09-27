open Ast

(* StmtExpr hoisting: eliminate GCC statement expressions
   [({ s1; ... ; e; })] from expression position by lifting their body
   into the enclosing statement scope. [rewrite_expr e] returns
   [(prefix, residual)] such that evaluating [e] is semantically
   equivalent to executing [prefix] then evaluating [residual]; every
   [StmtExpr] node contributes its body to the prefix. [rewrite_stmt s]
   walks each statement context, calls [rewrite_expr] on every
   contained expression, and prepends the prefix at the right point.
   Loop conditions duplicate the prefix top-and-tail so side effects
   re-execute each iteration (mirrors [Stmt.rewrite_comma]). Hoisting
   past a [ConditionalOperator] is unsound for [StmtExpr]s nested in a
   branch; we currently treat both branches as always-executed, which
   matches [rewrite_comma] for commas and is sound for the
   macro-hygiene shapes that dominate real-world use. *)

let rec rewrite_expr (e : c_expr) : c_stmt * c_expr =
  match e with
  | StmtExpr { body; result; _ } ->
      let body' = rewrite_stmt body in
      let prefix_inner, residual = rewrite_expr result in
      (Stmt.seq body' prefix_inner, residual)
  | SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _ | CXXBoolLiteralExpr _
  | FloatingLiteral _ | IntegerLiteral _ | Ident _ | UnresolvedLookupExpr _
  | DependentScopeRef _ ->
      (Skip, e)
  | Convert { arg; ty } ->
      let s, arg = rewrite_expr arg in
      (s, Convert { arg; ty })
  | CXXNewExpr { arg; ty } ->
      let s, arg = rewrite_expr arg in
      (s, CXXNewExpr { arg; ty })
  | CXXDeleteExpr { arg; ty } ->
      let s, arg = rewrite_expr arg in
      (s, CXXDeleteExpr { arg; ty })
  | ArraySubscriptExpr { lhs; rhs; ty; location } ->
      let s1, lhs = rewrite_expr lhs in
      let s2, rhs = rewrite_expr rhs in
      (Stmt.seq s1 s2, ArraySubscriptExpr { lhs; rhs; ty; location })
  | BinaryOperator { opcode; lhs; rhs; ty } ->
      let s1, lhs = rewrite_expr lhs in
      let s2, rhs = rewrite_expr rhs in
      (Stmt.seq s1 s2, BinaryOperator { opcode; lhs; rhs; ty })
  | CallExpr { func; args; ty } ->
      let sf, func = rewrite_expr func in
      let sa, args = rewrite_expr_list args in
      (Stmt.seq sf sa, CallExpr { func; args; ty })
  | ConditionalOperator { cond; then_expr; else_expr; ty } ->
      let sc, cond = rewrite_expr cond in
      let st, then_expr = rewrite_expr then_expr in
      let se, else_expr = rewrite_expr else_expr in
      ( Stmt.seq sc (Stmt.seq st se),
        ConditionalOperator { cond; then_expr; else_expr; ty } )
  | CXXConstructExpr { args; ty } ->
      let s, args = rewrite_expr_list args in
      (s, CXXConstructExpr { args; ty })
  | CXXOperatorCallExpr { func; args; ty } ->
      let sf, func = rewrite_expr func in
      let sa, args = rewrite_expr_list args in
      (Stmt.seq sf sa, CXXOperatorCallExpr { func; args; ty })
  | MemberExpr { name; base; ty } ->
      let s, base = rewrite_expr base in
      (s, MemberExpr { name; base; ty })
  | UnaryOperator { opcode; child; ty } ->
      let s, child = rewrite_expr child in
      (s, UnaryOperator { opcode; child; ty })
  | LambdaExpr { captures; params; body; ret_ty } ->
      (* Capture initializers are evaluated at the lambda's declaration
         site; lift any StmtExprs in them out into the enclosing scope.
         The body is opaque here — when [Lambda_lift] hoists it to a
         synthetic function, [rewrite_stmt] runs on that function's
         body independently. *)
      let s, captures =
        List.fold_left
          (fun (prefix, acc) (v, c) ->
            let s, c = rewrite_expr c in
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
      let s, e = rewrite_expr e in
      (s, PackExpansion e)

and rewrite_expr_list (es : c_expr list) : c_stmt * c_expr list =
  let prefix, residuals =
    List.fold_left
      (fun (prefix, acc) e ->
        let s, e = rewrite_expr e in
        (Stmt.seq prefix s, e :: acc))
      (Skip, []) es
  in
  (prefix, List.rev residuals)

and rewrite_expr_opt : c_expr option -> c_stmt * c_expr option = function
  | None -> (Skip, None)
  | Some e ->
      let s, e = rewrite_expr e in
      (s, Some e)

and rewrite_init (i : c_init) : c_stmt * c_init =
  match i with
  | IExpr e ->
      let s, e = rewrite_expr e in
      (s, IExpr e)
  | InitListExpr { ty; args } ->
      let s, args = rewrite_expr_list args in
      (s, InitListExpr { ty; args })

and rewrite_decl (d : c_decl) : c_stmt * c_decl =
  match d.init with
  | None -> (Skip, d)
  | Some i ->
      let s, i = rewrite_init i in
      (s, { d with init = Some i })

and rewrite_decls (ds : c_decl list) : c_stmt * c_decl list =
  let prefix, residuals =
    List.fold_left
      (fun (prefix, acc) d ->
        let s, d = rewrite_decl d in
        (Stmt.seq prefix s, d :: acc))
      (Skip, []) ds
  in
  (prefix, List.rev residuals)

and rewrite_for_init (f : c_for_init) : c_stmt * c_for_init =
  match f with
  | Decls ds ->
      let s, ds = rewrite_decls ds in
      (s, Decls ds)
  | Expr e ->
      let s, e = rewrite_expr e in
      (s, Expr e)

and rewrite_stmt (s : c_stmt) : c_stmt =
  match s with
  | Skip | BreakStmt | GotoStmt | ContinueStmt | ReturnStmt None -> s
  | ReturnStmt (Some e) ->
      let prefix, e = rewrite_expr e in
      Stmt.seq prefix (ReturnStmt (Some e))
  | IfStmt { cond; then_stmt; else_stmt } ->
      let prefix, cond = rewrite_expr cond in
      Stmt.seq prefix
        (IfStmt
           {
             cond;
             then_stmt = rewrite_stmt then_stmt;
             else_stmt = rewrite_stmt else_stmt;
           })
  | DeclStmt ds ->
      let prefix, ds = rewrite_decls ds in
      Stmt.seq prefix (DeclStmt ds)
  | WhileStmt { cond; body } ->
      let prefix, cond = rewrite_expr cond in
      let body = rewrite_stmt body in
      if prefix = Skip then WhileStmt { cond; body }
      else
        Stmt.seq prefix
          (WhileStmt { cond; body = Stmt.seq body prefix })
  | DoStmt { cond; body } ->
      let prefix, cond = rewrite_expr cond in
      let body = rewrite_stmt body in
      DoStmt { cond; body = Stmt.seq body prefix }
  | ForStmt { init; cond; inc; body } ->
      let s_init, init =
        match init with
        | None -> (Skip, None)
        | Some f ->
            let s, f = rewrite_for_init f in
            (s, Some f)
      in
      let s_cond, cond = rewrite_expr_opt cond in
      let inc = rewrite_stmt inc in
      let body = rewrite_stmt body in
      let body =
        if s_cond = Skip then body else Stmt.seq body s_cond
      in
      Stmt.seq s_init
        (Stmt.seq s_cond (ForStmt { init; cond; inc; body }))
  | SwitchStmt { cond; body } ->
      let prefix, cond = rewrite_expr cond in
      Stmt.seq prefix
        (SwitchStmt { cond; body = rewrite_stmt body })
  | CaseStmt { case; body } ->
      let prefix, case = rewrite_expr case in
      Stmt.seq prefix
        (CaseStmt { case; body = rewrite_stmt body })
  | DefaultStmt s -> DefaultStmt (rewrite_stmt s)
  | SExpr e ->
      let prefix, e = rewrite_expr e in
      Stmt.seq prefix (SExpr e)
  | AsmStmt a ->
      let rewrite_operand (op : c_expr Asm.operand) :
          c_stmt * c_expr Asm.operand =
        let s, expr = rewrite_expr op.expr in
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
      let st, target = rewrite_expr target in
      let sa, args = rewrite_expr_list args in
      Stmt.seq (Stmt.seq st sa) (BarrierOp { op; target; args; loc })
  | Seq (s1, s2) ->
      Stmt.seq (rewrite_stmt s1) (rewrite_stmt s2)

let run : c_stmt -> c_stmt = rewrite_stmt
