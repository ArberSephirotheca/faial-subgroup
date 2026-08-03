open Stage0
open Protocols
open Ast
open Parse_util

module ForInit = For_init

type t = Def.t list
type 'a state = (Variable.Set.t, 'a) State.t

let rewrite_shared_arrays (p : t) : t =
  let open Stage0.State.Syntax in
  let records =
    p
    |> List.filter_map (function
        | Def.Record r -> Some (Variable.from_name (Record.qualified_name r))
        | _ -> None)
    |> Variable.Set.of_list
  in
  let is_record (ty : Ty.t) : bool =
    match Record.type_name ty with
    | Some n -> Variable.Set.mem (Variable.from_name n) records
    | None -> false
  in
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
  let rw_decl (d : Decl.t) : Decl.t state =
    State.update_return (fun vars ->
        let vars =
          let name = Decl.var d in
          if
            Decl.is_shared d
            && (not (Decl.matches Ty.is_array_or_pointer d))
            && not (Decl.matches is_record d)
          then Variable.Set.add name vars
          else Variable.Set.remove name vars
        in
        (vars, Decl.map_expr (rw_exp vars) d))
  in

  let scope (m : 'a state) : 'a state =
    State.update_return (fun s -> (s, State.run_result m s))
  in

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
          let* l = State.list_map rw_decl l in
          return (DeclStmt l)
      | WhileStmt { cond; body } ->
          let* cond = rw_e cond in
          let* body = scope (rw_s body) in
          return (WhileStmt { cond; body })
      | ForStmt { init; cond; inc; body } ->
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
    | Def.Declaration d :: p ->
        let vars =
          if
            Decl.is_shared d
            && (not (Decl.matches Ty.is_array_or_pointer d))
            && not (Decl.matches is_record d)
          then Variable.Set.add (Decl.var d) vars
          else vars
        in
        Def.Declaration d :: rw_p vars p
    | Def.Kernel k :: p ->
        Def.Kernel { k with code = rw_stmt vars k.code } :: rw_p vars p
    | Def.Prototype k :: p -> Def.Prototype k :: rw_p vars p
    | Def.Typedef d :: p -> Def.Typedef d :: rw_p vars p
    | Def.Record r :: p -> Def.Record r :: rw_p vars p
    | Def.Enum e :: p -> Def.Enum e :: rw_p vars p
    | Def.LaunchParam lp :: p -> Def.LaunchParam lp :: rw_p vars p
    | [] -> []
  in
  rw_p Variable.Set.empty p

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
