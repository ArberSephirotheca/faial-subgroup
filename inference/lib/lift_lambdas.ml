open Stage0
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

   State is threaded through the [State] monad: a fresh-name counter
   and an accumulator of synthetic kernels flow through [Context.t]. *)

module Param = C_lang.Param

let mk_param ~(name : Variable.t) ~(ty : J_type.t) : Param.t =
  Param.make
    ~ty_var:(Ty_variable.make ~ty ~name)
    ~is_used:true ~is_shared:false

module Context = struct
  type binding = {
    fname : Variable.t;
    (* Effective captures (after lambda-of-lambda splicing): names that
       appear in the synthetic kernel's parameter list, paired with the
       init expression spliced as an argument at every call site. *)
    captures : (Variable.t * Expr.t) list;
  }

  type t = {
    next : int;
    bindings : binding Variable.Map.t;
    (* Synthetic kernels in reverse order of synthesis. *)
    synthetics : Kernel.t list;
  }

  let make (next : int) : t =
    { next; bindings = Variable.Map.empty; synthetics = [] }

  let bindings : (t, binding Variable.Map.t) State.t =
    State.get_return (fun s -> s.bindings)

  let lookup (v : Variable.t) : (t, binding option) State.t =
    State.get_return (fun s -> Variable.Map.find_opt v s.bindings)

  let fresh_name (label : string) : (t, Variable.t) State.t =
    State.update_return (fun s ->
        let name = Printf.sprintf "__lambda_%s_%d" label s.next in
        ({ s with next = s.next + 1 }, Variable.from_name name))

  let add_binding (var : Variable.t) (b : binding) : (t, unit) State.t =
    State.update (fun s ->
        { s with bindings = Variable.Map.add var b s.bindings })

  let add_synthetic (k : Kernel.t) : (t, unit) State.t =
    State.update (fun s -> { s with synthetics = k :: s.synthetics })

  (* Resolve lambda-of-lambda captures: replace a capture whose name is
     itself a lambda binding with the lambda's effective captures. Then
     dedupe (a single name might be reachable through multiple paths). *)
  let splice_captures (caps : (Variable.t * Expr.t) list) :
      (t, (Variable.t * Expr.t) list) State.t =
    let open State.Syntax in
    let* env = bindings in
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
    return (dedupe Variable.Set.empty expanded)
end

type 'a state = (Context.t, 'a) State.t

open State.Syntax

(* Walk a D_lang.Expr.t, replacing any lambda call sites. The bindings map
   is stable for the duration of an expression walk (only [rewrite_stmt]
   ever adds bindings), so we snapshot it once and let [Expr.st_map] handle
   the recursion. *)
let rewrite_expr (e : Expr.t) : Expr.t state =
  let* env = Context.bindings in
  Expr.st_map
    (function
      | CallExpr { func = Ident { name = v; _ }; args; ty }
        when Variable.Map.mem v env ->
          let b = Variable.Map.find v env in
          let cap_args = List.map snd b.captures in
          let func =
            Expr.Ident
              (Decl_expr.from_name ~ty:J_type.unknown
                 ~kind:Decl_expr.Kind.Function b.fname)
          in
          return (Expr.CallExpr { func; args = cap_args @ args; ty })
      | e -> return e)
    e

let rewrite_init (i : Init.t) : Init.t state =
  match i with
  | IExpr e ->
      let* e = rewrite_expr e in
      return (Init.IExpr e)
  | InitListExpr { ty; args } ->
      let* args = State.list_map rewrite_expr args in
      return (Init.InitListExpr { ty; args })
  | CXXConstructExpr _ -> return i

let rewrite_decl (d : Decl.t) : Decl.t state =
  let* init = State.option_map rewrite_init d.init in
  return { d with init }

let rewrite_for_init (f : ForInit.t) : ForInit.t state =
  match f with
  | Decls ds ->
      let* ds = State.list_map rewrite_decl ds in
      return (ForInit.Decls ds)
  | Expr e ->
      let* e = rewrite_expr e in
      return (ForInit.Expr e)

let rewrite_subscript (s : d_subscript) : d_subscript state =
  let* index = State.list_map rewrite_expr s.index in
  return { s with index }

(* Walk a D_lang.Stmt.t, lifting any [LambdaDecl] into [Context.synthetics]
   and rewriting its call sites in subsequent siblings. [Stmt.st_map]
   handles child-stmt recursion post-order, so by the time the algebra
   sees a node, its child statements have already been rewritten. *)
let rewrite_stmt (st : Stmt.t) : Stmt.t state =
  Stmt.st_map
    (fun st ->
      match st with
      | Skip | BreakStmt | GotoStmt | ContinueStmt | Seq _ | DefaultStmt _ ->
          return st
      | WriteAccessStmt w ->
          let* target = rewrite_subscript w.target in
          let* source = rewrite_expr w.source in
          return (Stmt.WriteAccessStmt { w with target; source })
      | ReadAccessStmt r ->
          let* source = rewrite_subscript r.source in
          return (Stmt.ReadAccessStmt { r with source })
      | AtomicAccessStmt a ->
          let* source = rewrite_subscript a.source in
          return (Stmt.AtomicAccessStmt { a with source })
      | ReturnStmt e ->
          let* e = State.option_map rewrite_expr e in
          return (Stmt.ReturnStmt e)
      | IfStmt { cond; then_stmt; else_stmt } ->
          let* cond = rewrite_expr cond in
          return (Stmt.IfStmt { cond; then_stmt; else_stmt })
      | DeclStmt ds ->
          let* ds = State.list_map rewrite_decl ds in
          return (Stmt.DeclStmt ds)
      | WhileStmt { cond; body } ->
          let* cond = rewrite_expr cond in
          return (Stmt.WhileStmt { cond; body })
      | DoStmt { cond; body } ->
          let* cond = rewrite_expr cond in
          return (Stmt.DoStmt { cond; body })
      | ForStmt { init; cond; inc; body } ->
          let* init = State.option_map rewrite_for_init init in
          let* cond = State.option_map rewrite_expr cond in
          return (Stmt.ForStmt { init; cond; inc; body })
      | SwitchStmt { cond; body } ->
          let* cond = rewrite_expr cond in
          return (Stmt.SwitchStmt { cond; body })
      | CaseStmt { case; body } ->
          let* case = rewrite_expr case in
          return (Stmt.CaseStmt { case; body })
      | SExpr e ->
          let* e = rewrite_expr e in
          return (Stmt.SExpr e)
      | AsmStmt a ->
          let r_op (op : Expr.t Asm.operand) : Expr.t Asm.operand state =
            let* expr = rewrite_expr op.expr in
            return { Asm.constr = op.constr; expr }
          in
          let* outputs = State.list_map r_op a.outputs in
          let* inputs = State.list_map r_op a.inputs in
          return (Stmt.AsmStmt { a with outputs; inputs })
      | BarrierOp { op; target; args; loc } ->
          let* target = rewrite_subscript target in
          let* args = State.list_map rewrite_expr args in
          return (Stmt.BarrierOp { op; target; args; loc })
      | LambdaDecl { var; captures; params; body; ret_ty } ->
          (* Captures' init exprs are rewritten in the outer env
             (st_map didn't recurse into them — only body). The body
             above has already been rewritten in the env that was
             current when we entered this LambdaDecl, so calls to
             in-scope sibling lambdas were inlined. *)
          let* captures =
            State.list_map
              (fun (n, e) ->
                let* e = rewrite_expr e in
                return (n, e))
              captures
          in
          let* effective = Context.splice_captures captures in
          let* fname = Context.fresh_name (Variable.name var) in
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
          let* () =
            Context.add_binding var { fname; captures = effective }
          in
          let* () = Context.add_synthetic synth in
          return Stmt.Skip)
    st

let lift_kernel (next : int) (k : Kernel.t) : int * Kernel.t list * Kernel.t =
  let s, code = State.run (rewrite_stmt k.code) (Context.make next) in
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
