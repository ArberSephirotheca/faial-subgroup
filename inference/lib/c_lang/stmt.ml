open Stage0
open Ast

module BarrierOp = Barrier_op
module ForInit = For_init

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

  let map_expr (f : Expr.t -> Expr.t) : c_stmt -> c_stmt =
    let for_init : ForInit.t -> ForInit.t = function
      | Decls l -> Decls (List.map (Decl.map_expr f) l)
      | Expr e -> Expr (f e)
    in
    map (function
      | ReturnStmt e -> ReturnStmt (Option.map f e)
      | IfStmt c -> IfStmt { c with cond = f c.cond }
      | DeclStmt l -> DeclStmt (List.map (Decl.map_expr f) l)
      | WhileStmt w -> WhileStmt { w with cond = f w.cond }
      | DoStmt w -> DoStmt { w with cond = f w.cond }
      | SwitchStmt w -> SwitchStmt { w with cond = f w.cond }
      | CaseStmt c -> CaseStmt { c with case = f c.case }
      | ForStmt r ->
          ForStmt
            { r with
              init = Option.map for_init r.init;
              cond = Option.map f r.cond }
      | SExpr e -> SExpr (f e)
      | AsmStmt a -> AsmStmt (Asm.map_expr f a)
      | BarrierOp b ->
          BarrierOp { b with target = f b.target; args = List.map f b.args }
      | (Skip | BreakStmt | GotoStmt | ContinueStmt | DefaultStmt _ | Seq _) as s
        ->
          s)

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
          WhileStmt { cond; body = rw body }
        else
          let s = to_stmt st in
          (* when there are commas in the condition, we need to
             append the commas to the end of the loop body, and
             before the loop too *)
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

let parse : Parse_util.json -> t Parse_util.j_result = Parsers.parse_stmt
let parse_list : Parse_util.json -> t Parse_util.j_result = Parsers.parse_stmt_list
