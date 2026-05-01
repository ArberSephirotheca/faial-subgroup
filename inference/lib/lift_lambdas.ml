open Protocols
open D_lang

(* Eliminates [LambdaDecl] from the D_lang AST by hoisting each lambda
   body to a fresh top-level [Auxiliary] [D_lang.Kernel.t] and rewriting
   [v(args)] call sites to [__lambda_<id>(captures @ args)]. After this
   pass runs, [D_to_imp] never sees a [LambdaDecl]; the synthetic
   kernels become [Imp.Kernel.t] with [Visibility.Device] and are
   inlined by [Imp.Inline_calls] exactly like any hand-written
   [__device__] helper.

   Lambda-of-lambda is handled by *capture splicing*: when an enclosing
   lambda captures another lambda binding, the captures of that binding
   are spliced into the enclosing lambda's effective capture list, and
   calls inside the enclosing body have already been rewritten to
   direct calls to the synthetic. So lambda-typed parameters never
   appear in the generated kernels.

   State is threaded explicitly (no refs): a fresh-name counter and an
   accumulator of synthetic kernels flow through the recursive
   traversal. *)

module Param = C_lang.Param

type binding = {
  fname : Variable.t;
  (* Effective captures (after lambda-of-lambda splicing): names that
     appear in the synthetic kernel's parameter list, paired with the
     init expression spliced as an argument at every call site. *)
  captures : (Variable.t * Expr.t) list;
}

type state = {
  next : int;
  bindings : binding Variable.Map.t;
  (* Synthetic kernels in reverse order of synthesis. *)
  synthetics : Kernel.t list;
}

let mk_param ~(name : Variable.t) ~(ty : J_type.t) : Param.t =
  Param.make
    ~ty_var:(Ty_variable.make ~ty ~name)
    ~is_used:true ~is_shared:false

(* Walk a D_lang.Expr.t with [env], replacing any lambda call sites. *)
let rec rewrite_expr (env : binding Variable.Map.t) (e : Expr.t) : Expr.t =
  let r = rewrite_expr env in
  match e with
  | CallExpr { func = Ident { name = v; _ }; args; ty }
    when Variable.Map.mem v env ->
      let b = Variable.Map.find v env in
      let cap_args = List.map snd b.captures in
      let func' =
        Expr.Ident
          (Decl_expr.from_name ~ty:J_type.unknown
             ~kind:Decl_expr.Kind.Function b.fname)
      in
      let args = List.map r args in
      CallExpr { func = func'; args = cap_args @ args; ty }
  | CallExpr { func; args; ty } ->
      CallExpr { func = r func; args = List.map r args; ty }
  | CXXOperatorCallExpr { func; args; ty } ->
      CXXOperatorCallExpr { func = r func; args = List.map r args; ty }
  | BinaryOperator { lhs; rhs; opcode; ty } ->
      BinaryOperator { lhs = r lhs; rhs = r rhs; opcode; ty }
  | UnaryOperator { child; opcode; ty } ->
      UnaryOperator { child = r child; opcode; ty }
  | ConditionalOperator { cond; then_expr; else_expr; ty } ->
      ConditionalOperator
        {
          cond = r cond;
          then_expr = r then_expr;
          else_expr = r else_expr;
          ty;
        }
  | CXXNewExpr { arg; ty } -> CXXNewExpr { arg = r arg; ty }
  | CXXDeleteExpr { arg; ty } -> CXXDeleteExpr { arg = r arg; ty }
  | CXXConstructExpr { args; ty } ->
      CXXConstructExpr { args = List.map r args; ty }
  | MemberExpr { name; base; ty } -> MemberExpr { name; base = r base; ty }
  | (SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _
    | CXXBoolLiteralExpr _ | FloatingLiteral _ | IntegerLiteral _ | Ident _
    | UnresolvedLookupExpr _) as e ->
      e

let rewrite_init (env : binding Variable.Map.t) (i : Init.t) : Init.t =
  let r = rewrite_expr env in
  match i with
  | IExpr e -> IExpr (r e)
  | InitListExpr { ty; args } -> InitListExpr { ty; args = List.map r args }
  | CXXConstructExpr _ -> i

let rewrite_decl (env : binding Variable.Map.t) (d : Decl.t) : Decl.t =
  { d with init = Option.map (rewrite_init env) d.init }

let rewrite_for_init (env : binding Variable.Map.t) (f : ForInit.t) :
    ForInit.t =
  match f with
  | Decls ds -> Decls (List.map (rewrite_decl env) ds)
  | Expr e -> Expr (rewrite_expr env e)

let rewrite_subscript (env : binding Variable.Map.t) (s : d_subscript) :
    d_subscript =
  { s with index = List.map (rewrite_expr env) s.index }

(* Resolve lambda-of-lambda captures: replace a capture whose name is
   itself a lambda binding with the lambda's effective captures. Then
   dedupe (a single name might be reachable through multiple paths). *)
let splice_captures (env : binding Variable.Map.t)
    (caps : (Variable.t * Expr.t) list) : (Variable.t * Expr.t) list =
  let expanded =
    List.concat_map
      (fun (name, init_expr) ->
        match Variable.Map.find_opt name env with
        | Some b -> b.captures
        | None -> [ (name, init_expr) ])
      caps
  in
  let rec dedupe seen = function
    | [] -> []
    | (n, e) :: t ->
        if Variable.Set.mem n seen then dedupe seen t
        else (n, e) :: dedupe (Variable.Set.add n seen) t
  in
  dedupe Variable.Set.empty expanded

let fresh_name (s : state) (label : string) : state * Variable.t =
  let name = Printf.sprintf "__lambda_%s_%d" label s.next in
  ({ s with next = s.next + 1 }, Variable.from_name name)

(* Walk a D_lang.Stmt.t threading [s], lifting any [LambdaDecl] into
   [s.synthetics] and rewriting its call sites in subsequent siblings. *)
let rec rewrite_stmt (s : state) (st : Stmt.t) : state * Stmt.t =
  let r_e = rewrite_expr s.bindings in
  let r_sub = rewrite_subscript s.bindings in
  match st with
  | Skip | BreakStmt | GotoStmt | ContinueStmt -> (s, st)
  | Seq (a, b) ->
      let s, a = rewrite_stmt s a in
      let s, b = rewrite_stmt s b in
      (s, Stmt.seq a b)
  | WriteAccessStmt w ->
      ( s,
        WriteAccessStmt
          { w with target = r_sub w.target; source = r_e w.source } )
  | ReadAccessStmt r -> (s, ReadAccessStmt { r with source = r_sub r.source })
  | AtomicAccessStmt a ->
      (s, AtomicAccessStmt { a with source = r_sub a.source })
  | ReturnStmt e -> (s, ReturnStmt (Option.map r_e e))
  | IfStmt { cond; then_stmt; else_stmt } ->
      let s, t = rewrite_stmt s then_stmt in
      let s, e = rewrite_stmt s else_stmt in
      (s, IfStmt { cond = r_e cond; then_stmt = t; else_stmt = e })
  | DeclStmt ds -> (s, DeclStmt (List.map (rewrite_decl s.bindings) ds))
  | WhileStmt { cond; body } ->
      let s, body = rewrite_stmt s body in
      (s, WhileStmt { cond = r_e cond; body })
  | DoStmt { cond; body } ->
      let s, body = rewrite_stmt s body in
      (s, DoStmt { cond = r_e cond; body })
  | ForStmt { init; cond; inc; body } ->
      let init = Option.map (rewrite_for_init s.bindings) init in
      let cond = Option.map r_e cond in
      let s, inc = rewrite_stmt s inc in
      let s, body = rewrite_stmt s body in
      (s, ForStmt { init; cond; inc; body })
  | SwitchStmt { cond; body } ->
      let s, body = rewrite_stmt s body in
      (s, SwitchStmt { cond = r_e cond; body })
  | DefaultStmt body ->
      let s, body = rewrite_stmt s body in
      (s, DefaultStmt body)
  | CaseStmt { case; body } ->
      let s, body = rewrite_stmt s body in
      (s, CaseStmt { case = r_e case; body })
  | SExpr e -> (s, SExpr (r_e e))
  | AsmStmt a ->
      let r_op (op : Expr.t Asm.operand) : Expr.t Asm.operand =
        { Asm.constr = op.constr; expr = r_e op.expr }
      in
      ( s,
        AsmStmt
          {
            a with
            outputs = List.map r_op a.outputs;
            inputs = List.map r_op a.inputs;
          } )
  | BarrierOp { op; target; args; loc } ->
      ( s,
        BarrierOp
          { op; target = r_sub target; args = List.map r_e args; loc } )
  | LambdaDecl { var; captures; params; body; ret_ty } ->
      (* Step 1: rewrite captures' init exprs in the *outer* env. *)
      let captures = List.map (fun (n, e) -> (n, r_e e)) captures in
      (* Step 2: splice lambda-of-lambda captures so the synthetic
         kernel takes only first-class parameters. *)
      let effective = splice_captures s.bindings captures in
      (* Step 3: rewrite the lambda body using the *current* env so
         calls to in-scope sibling lambdas are inlined. *)
      let s_inner, body = rewrite_stmt s body in
      (* Step 4: emit the synthetic kernel. Param list = capture-params
         followed by explicit-params. *)
      let s, fname = fresh_name s_inner (Variable.name var) in
      let cap_params =
        List.map
          (fun (n, _) -> mk_param ~name:n ~ty:J_type.unknown)
          effective
      in
      let synth : Kernel.t =
        {
          ty = J_type.to_string ret_ty;
          name = Variable.name fname;
          code = body;
          type_params = [];
          params = cap_params @ params;
          attribute = KernelAttr.Auxiliary;
        }
      in
      let s =
        {
          s with
          bindings =
            Variable.Map.add var { fname; captures = effective } s.bindings;
          synthetics = synth :: s.synthetics;
        }
      in
      (* The original LambdaDecl has no runtime equivalent — drop it. *)
      (s, Skip)

let lift_kernel (next : int) (k : Kernel.t) : int * Kernel.t list * Kernel.t =
  let s = { next; bindings = Variable.Map.empty; synthetics = [] } in
  let s, code = rewrite_stmt s k.code in
  (s.next, List.rev s.synthetics, { k with code })

(* Lift every [Def.Kernel] in [p], producing a new program where each
   kernel's synthetic auxiliaries appear *before* the kernel itself.
   Earlier ordering matters because [D_to_imp.parse_p] threads its
   [Context.t] through the program in order, so callees must be parsed
   before callers (their shared-array decls flow into the caller's
   context). *)
let lift_program (p : Program.t) : Program.t =
  let lift_def (next : int) (def : Def.t) : int * Def.t list =
    match def with
    | Kernel k ->
        let next, synths, k' = lift_kernel next k in
        (next, List.map (fun s -> Def.Kernel s) synths @ [ Def.Kernel k' ])
    | other -> (next, [ other ])
  in
  let _, rev =
    List.fold_left
      (fun (next, acc) def ->
        let next, defs = lift_def next def in
        (next, List.rev_append defs acc))
      (0, []) p
  in
  List.rev rev
