open Stage0
open Protocols
open Logger
open Imp
module StackTrace = Stack_trace
module KernelAttr = C_lang.KernelAttr
module StringMap = Common.StringMap
module Param = C_lang.Param
module Ty_param = C_lang.Ty_param

let ( @ ) = Common.append_tr

open Exp

type d_error = string StackTrace.t

let error_to_buffer (e : d_error) : Buffer.t =
  let b = Buffer.create 512 in
  StackTrace.iter (Buffer.add_string b) e;
  b

type 'a d_result = ('a, d_error) Result.t

let unwrap : 'a d_result -> 'a = function
  | Ok a -> a
  | Error e -> failwith (error_to_buffer e |> Buffer.contents)

let parse_var : D_lang.Expr.t -> Variable.t = function
  | Ident v -> v.name
  | e ->
      failwith ("parse_var: unexpected expression: " ^ D_lang.Expr.to_string e)

type d_access = {
  location : Variable.t;
  mode : Access.Mode.t;
  index : D_lang.Expr.t list;
}

type d_location_alias = {
  source : D_lang.Expr.t;
  target : D_lang.Expr.t;
  offset : D_lang.Expr.t;
}

module TypeAlias = struct
  type t = C_type.t StringMap.t

  let empty : t = StringMap.empty

  (* Resolve a type according to the alias in the database *)
  let resolve (ty : C_type.t) (db : t) : C_type.t =
    StringMap.find_opt (C_type.to_string ty) db |> Option.value ~default:ty

  (* Add a new type alias to the data-base *)
  let add (x : Typedef.t) (db : t) : t =
    (* Resolve the type so that there are no indirect alias *)
    StringMap.add x.name (resolve x.ty db) db
end

module Make (L : Logger) = struct
  let parse_bin (op : string) (l : Imp.Infer_exp.t) (r : Infer_exp.t) :
      Infer_exp.t =
    match op with
    (* bool -> bool -> bool *)
    | "||" -> BExp (BRel (BOr, l, r))
    | "&&" -> BExp (BRel (BAnd, l, r))
    (* int -> int -> bool *)
    | "==" -> BExp (NRel (Eq, l, r))
    | "!=" -> BExp (NRel (Neq, l, r))
    | "<=" -> BExp (NRel (Le, l, r))
    | "<" -> BExp (NRel (Lt, l, r))
    | ">=" -> BExp (NRel (Ge, l, r))
    | ">" -> BExp (NRel (Gt, l, r))
    (* int -> int -> int *)
    | "+" -> NExp (Binary (Plus, l, r))
    | "-" -> NExp (Binary (Minus, l, r))
    | "*" -> NExp (Binary (Mult, l, r))
    | "/" -> NExp (Binary (Div, l, r))
    | "%" -> NExp (Binary (Mod, l, r))
    | ">>" -> NExp (Binary (RightShift, l, r))
    | "<<" -> NExp (Binary (LeftShift, l, r))
    | "^" -> NExp (Binary (BitXOr, l, r))
    | "|" -> NExp (Binary (BitOr, l, r))
    | "&" -> NExp (Binary (BitAnd, l, r))
    | _ ->
        L.warning ("parse_bin: rewriting to unknown binary operator: " ^ op);
        let lbl =
          "(" ^ Infer_exp.to_string l ^ ") " ^ op ^ " " ^ "("
          ^ Infer_exp.to_string r ^ ")"
        in
        Unknown lbl

  let rec infer_expr (e : D_lang.Expr.t) : Infer_exp.t =
    match e with
    (* ---------------- CUDA SPECIFIC ----------- *)
    | MemberExpr { base = Ident base; name = field; _ } ->
        let v = base.name |> Variable.update_name (fun n -> n ^ "." ^ field) in
        NExp (Var v)
    (* ------------------ nexp ------------------------ *)
    | Ident d -> NExp (Var d.name)
    | SizeOfExpr ty -> (
        match J_type.to_c_type_res ty with
        | Ok ty ->
            let size = C_type.sizeof ty |> Option.value ~default:4 in
            L.warning
              ("sizeof(" ^ C_type.to_string ty ^ ") = " ^ string_of_int size);
            NExp (Num size)
        | Error _ ->
            let lbl = "sizeof(" ^ J_type.to_string ty ^ ")" in
            L.warning ("could not parse type: " ^ lbl ^ " = ?");
            Unknown lbl)
    | IntegerLiteral n | CharacterLiteral n -> NExp (Num n)
    | FloatingLiteral n ->
        L.warning
          ("parse_nexp: converting float '" ^ Float.to_string n ^ "' to integer");
        NExp (Num (Float.to_int n))
    | ConditionalOperator o ->
        let b = infer_expr o.cond in
        let n1 = infer_expr o.then_expr in
        let n2 = infer_expr o.else_expr in
        NExp (NIf (b, n1, n2))
    | UnaryOperator { opcode = "~"; child = e; _ } ->
        let n = infer_expr e in
        NExp (Unary (BitNot, n))
    | CallExpr
        { func = Ident { name = n; kind = Function; _ }; args = [ n1; n2 ]; _ }
      when Variable.name n = "divUp" ->
        let n1 = infer_expr n1 in
        let n2 = infer_expr n2 in
        (*  (n1 + n2 - 1)/n2 *)
        let n2_minus_1 : Infer_exp.n = Binary (Minus, n2, NExp (Num 1)) in
        let n1_plus_n2_minus_1 : Infer_exp.n =
          Binary (Plus, n1, NExp n2_minus_1)
        in
        NExp (Binary (Div, NExp n1_plus_n2_minus_1, n2))
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args = [ n ]; _ }
      when Variable.name f = "__other_int" ->
        let n = infer_expr n in
        NExp (Other n)
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args = [ n ]; _ }
      when Variable.name f = "__uniform_int" ->
        let n = infer_expr n in
        BExp (Infer_exp.thread_equal n)
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args = [ n ]; _ }
      when Variable.name f = "__distinct_int" ->
        let n = infer_expr n in
        BExp (Infer_exp.thread_distinct n)
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args = [ n ]; _ }
      when Variable.name f = "__is_pow2" ->
        let n = infer_expr n in
        BExp (Pred ("pow2", n))
    | CallExpr
        { func = Ident { name = n; kind = Function; _ }; args = [ n1; n2 ]; _ }
      when Variable.name n = "min" ->
        let n1 = infer_expr n1 in
        let n2 = infer_expr n2 in
        NExp (NIf (BExp (NRel (Lt, n1, n2)), n1, n2))
    | CallExpr
        { func = Ident { name = n; kind = Function; _ }; args = [ n1; n2 ]; _ }
      when Variable.name n = "max" ->
        let n1 = infer_expr n1 in
        let n2 = infer_expr n2 in
        NExp (NIf (BExp (NRel (Gt, n1, n2)), n1, n2))
    | BinaryOperator { lhs = l; opcode = "&"; rhs = IntegerLiteral 1; _ } ->
        let n = infer_expr l in
        BExp (NRel (Eq, NExp (Binary (Mod, n, NExp (Num 2))), NExp (Num 0)))
    | BinaryOperator
        {
          opcode = "==";
          lhs =
            BinaryOperator
              {
                opcode = "&";
                lhs = Ident n1 as e;
                rhs =
                  BinaryOperator
                    { opcode = "-"; lhs = Ident n2; rhs = IntegerLiteral 1; _ };
                _;
              };
          rhs = IntegerLiteral 0;
          _;
        }
      when Decl_expr.equal n1 n2 ->
        let n = infer_expr e in
        BExp
          (Infer_exp.or_
             (BExp (Pred ("pow2", n)))
             (BExp (Infer_exp.n_eq n (NExp (Num 0)))))
    | BinaryOperator { opcode = ","; lhs = _; rhs = e; _ } -> infer_expr e
    | BinaryOperator { opcode = o; lhs = n1; rhs = n2; _ } ->
        let n1 = infer_expr n1 in
        let n2 = infer_expr n2 in
        parse_bin o n1 n2
    | CXXBoolLiteralExpr b -> BExp (Bool b)
    | UnaryOperator u when u.opcode = "!" ->
        let b = infer_expr u.child in
        BExp (BNot b)
    | RecoveryExpr _ | CXXConstructExpr _ | MemberExpr _ | CallExpr _
    | UnaryOperator _ | CXXOperatorCallExpr _ ->
        let lbl = D_lang.Expr.to_string e in
        L.warning ("parse_exp: rewriting to unknown: " ^ lbl);
        Unknown lbl
    | _ ->
        failwith
          ("WARNING: parse_nexp: unsupported expression " ^ D_lang.Expr.name e
         ^ " : " ^ D_lang.Expr.to_string e)

  let to_nexp (e : D_lang.Expr.t) : Exp.nexp Infer_exp.state =
    Infer_exp.to_nexp (infer_expr e)

  let try_to_nexp (e : D_lang.Expr.t) : Exp.nexp option =
    e |> infer_expr |> Infer_exp.to_nexp |> Infer_exp.no_unknowns

  let parse_type (e : J_type.t) : C_type.t =
    e |> J_type.to_c_type_res
    |> Result.map_error (fun e ->
        Stack_trace.RootCause (Rjson.error_to_string e))
    |> unwrap

  let infer_arg (e : D_lang.Expr.t) : Infer_stmt.Arg.t =
    let rec to_array_use : D_lang.Expr.t -> Infer_stmt.Array_use.t option =
      function
      | Ident v -> Some (Infer_stmt.Array_use.make v.name)
      | UnaryOperator { opcode; child = Ident _ as v; _ } when opcode = "&" ->
          to_array_use v
      | BinaryOperator o when o.opcode = "+" ->
          let lhs_ty = D_lang.Expr.to_type o.lhs |> parse_type in
          let address, offset =
            if C_type.is_array lhs_ty then (o.lhs, o.rhs) else (o.rhs, o.lhs)
          in
          address
          (* try to parse array use *)
          |> to_array_use
          (* add offset *)
          |> Option.map (fun arr ->
              let offset = infer_expr offset in
              Infer_stmt.Array_use.add offset arr)
      | _ -> None
    in
    let ty = D_lang.Expr.to_type e |> parse_type in
    if C_type.is_array ty then
      e |> to_array_use
      (* If we have an array, wrap it under Array *)
      |> Option.map (fun o -> Infer_stmt.Arg.Array o)
      (* Otherwise, return unsupported *)
      |> Option.value ~default:Infer_stmt.Arg.Unsupported
    else if C_type.is_int ty then
      (* Handle scalars *)
      Scalar (infer_expr e)
    else Unsupported

  (* -------------------------------------------------------------- *)

  module Context = struct
    type t = {
      sigs : D_lang.SignatureDB.t;
      arrays : Memory.t Variable.Map.t;
      globals : Params.t;
      assigns : (Variable.t * nexp) list;
      typedefs : TypeAlias.t;
      enums : Enum.t Variable.Map.t;
    }

    let to_string (ctx : t) : string =
      [
        "sigs: " ^ D_lang.SignatureDB.to_string ctx.sigs;
        "arrays: ["
        ^ (ctx.arrays |> Variable.Map.to_list |> List.map fst
         |> List.map Variable.name |> String.concat ", ")
        ^ "]";
      ]
      |> String.concat "\n"

    let from_signature_db (sigs : D_lang.SignatureDB.t) : t =
      {
        sigs;
        arrays = Variable.Map.empty;
        globals = Params.empty;
        assigns = [];
        typedefs = TypeAlias.empty;
        enums = Variable.Map.empty;
      }

    let resolve (ty : C_type.t) (b : t) : C_type.t =
      TypeAlias.resolve ty b.typedefs

    let lookup_sig (e : D_lang.Expr.t) (arg_count : int) (db : t) :
        D_lang.SignatureDB.Signature.t option =
      D_lang.SignatureDB.lookup e arg_count db.sigs

    let is_enum (ty : C_type.t) (ctx : t) : bool =
      let name = C_type.to_string ty |> Variable.from_name in
      Variable.Map.mem name ctx.enums

    let is_int (ty : C_type.t) (ctx : t) : bool =
      C_type.is_int ty || is_enum ty ctx

    let get_enum (ty : C_type.t) (ctx : t) : Enum.t =
      let name = C_type.to_string ty |> Variable.from_name in
      Variable.Map.find name ctx.enums

    let add_array (var : Variable.t) (m : Memory.t) (b : t) : t =
      { b with arrays = Variable.Map.add var m b.arrays }

    let add_assign (var : Variable.t) (n : Exp.nexp) (b : t) : t =
      { b with assigns = (var, n) :: b.assigns }

    let add_global (var : Variable.t) (ty : C_type.t) (b : t) : t =
      { b with globals = Params.add var ty b.globals }

    let add_typedef (d : Typedef.t) (b : t) : t =
      { b with typedefs = TypeAlias.add d b.typedefs }

    let add_enum (e : Enum.t) (b : t) : t =
      let assigns =
        if Enum.ignore e then b.assigns else Enum.to_assigns e @ b.assigns
      in
      { b with assigns; enums = Variable.Map.add e.var e b.enums }

    (* Generate the preamble *)
    let gen_preamble (c : t) : Imp.Stmt.t =
      c.assigns
      |> List.map (fun (k, v) -> Imp.Stmt.decl_set k v)
      |> Stmt.from_list
  end

  let rec infer_load_expr (target : D_lang.Expr.t) (exp : D_lang.Expr.t) :
      d_location_alias option =
    let ( let* ) = Option.bind in
    match exp with
    | Ident { ty; _ }
      when J_type.matches C_type.is_pointer ty
           || J_type.matches C_type.is_array ty ->
        Some { target; source = exp; offset = IntegerLiteral 0 }
    | CXXOperatorCallExpr
        { func = UnresolvedLookupExpr { name = n; _ }; args = [ lhs; rhs ]; ty }
    | CXXOperatorCallExpr
        { func = Ident { name = n; _ }; args = [ lhs; rhs ]; ty }
      when Variable.name n = "operator+" ->
        let* l = infer_load_expr target lhs in
        let offset : D_lang.Expr.t =
          BinaryOperator { opcode = "+"; lhs = l.offset; rhs; ty }
        in
        Some { l with offset }
    | CXXOperatorCallExpr _ -> None
    | BinaryOperator ({ lhs = l; _ } as b) ->
        let* l = infer_load_expr target l in
        let offset : D_lang.Expr.t = BinaryOperator { b with lhs = l.offset } in
        Some { l with offset }
    | _ -> None

  let asserts : Variable.Set.t =
    Variable.Set.of_list
      [
        Variable.from_name "assert";
        Variable.from_name "static_assert";
        Variable.from_name "__requires";
      ]

  let infer_stmt (ctx : Context.t) : D_lang.Stmt.t -> Imp.Infer_stmt.t =
    let resolve ty = Context.resolve ty ctx in

    let infer_type (ty : J_type.t) : C_type.t =
      Context.resolve (J_type.to_c_type ty) ctx
    in

    let infer_location_alias (s : d_location_alias) : Imp.Infer_stmt.t =
      let source = parse_var s.source in
      let target = parse_var s.target in
      let offset = infer_expr s.offset in
      Infer_stmt.LocationAlias { target; source; offset }
    in

    let infer_decl (d : D_lang.Decl.t) : Infer_stmt.t =
      let x = d.var in
      match
        D_lang.Decl.types d
        |> List.map (fun ty -> Context.resolve (J_type.to_c_type ty) ctx)
        |> List.find_opt (fun ty -> Context.is_int ty ctx)
      with
      | Some ty ->
          let init : Infer_exp.t option =
            match d.init with
            | Some (IExpr n) -> Some (infer_expr n)
            | _ -> None
          in
          let d : Infer_stmt.t =
            match init with
            | Some n -> Infer_stmt.decl_set ~ty x n
            | None -> Infer_stmt.decl_unset ~ty x
          in
          d
      | None ->
          let x = Variable.name x in
          let ty = J_type.to_string d.ty in
          L.warning
            ("parse_decl: skipping non-int local variable '" ^ x ^ "' "
           ^ "type: " ^ ty);
          Skip
    in

    let infer_call ?(result = None) (func : D_lang.Expr.t)
        (args : D_lang.Expr.t list) : Infer_stmt.t =
      let arg_count = List.length args in
      match Context.lookup_sig func arg_count ctx with
      | Some s ->
          if List.length s.params <> arg_count then
            let e : D_lang.Expr.t =
              CallExpr { func; args; ty = J_type.unknown }
            in
            failwith
              ("infer_call: CallExpr args mismatch: " ^ D_lang.Expr.to_string e)
          else
            let open Imp.Infer_stmt in
            Call
              {
                result;
                kernel = s.kernel;
                ty = s.ty;
                args = List.map infer_arg args;
              }
      | None -> Skip
    in

    let rec infer : D_lang.Stmt.t -> Imp.Infer_stmt.t = function
      | Skip -> Skip
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = []; _ })
        when Variable.name n = "__syncthreads" ->
          Sync n.location
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = [ _ ]; _ })
        when Variable.name n = "sync" ->
          Sync n.location
          (* Static assert may have a message as second argument *)
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = b :: _; _ })
        when Variable.Set.mem n asserts ->
          Infer_stmt.Assert (infer_expr b)
      | SExpr (CallExpr { func; args; _ }) -> infer_call func args
      | WriteAccessStmt w ->
          let array =
            w.target.name |> Variable.set_location w.target.location
          in
          let index = List.map infer_expr w.target.index in
          Infer_stmt.Write { array; index; payload = w.payload }
      | ReadAccessStmt r ->
          let array =
            r.source.name |> Variable.set_location r.source.location
          in
          let index = List.map infer_expr r.source.index in
          let ty = r.ty |> resolve |> C_type.strip_array in
          Infer_stmt.Read { target = Some (ty, r.target); array; index }
      | AtomicAccessStmt r ->
          let array =
            r.source.name |> Variable.set_location r.source.location
          in
          let index = List.map infer_expr r.source.index in
          let ty = r.ty |> resolve |> C_type.strip_array in
          Infer_stmt.Atomic
            { target = r.target; atomic = r.atomic; array; index; ty }
      | IfStmt { cond; then_stmt; else_stmt } ->
          Imp.Infer_stmt.If (infer_expr cond, infer then_stmt, infer else_stmt)
      (* Support for location aliasing that declares a new variable *)
      | DeclStmt [ d ] -> (
          let ( let* ) = Option.bind in
          (* Detect non-standard declarations: *)
          let s =
            match d with
            (* Detect kernel-calls: *)
            | { init = Some (IExpr (CallExpr { func; args; _ })); _ } ->
                (* Found a call, so extract the call and parse the rest
            of the declaration yet unsetting the first decl *)
                let ty = infer_type d.ty in
                Some (infer_call ~result:(Some (d.var, ty)) func args)
            (* Detect array alias: *)
            | { ty; init = Some (IExpr rhs); _ }
              when J_type.matches
                     (fun x -> C_type.is_pointer x || C_type.is_auto x)
                     ty ->
                let d_ty =
                  d.ty
                  |> J_type.to_c_type ~default:C_type.int
                  |> resolve |> J_type.from_c_type
                in
                let lhs : D_lang.Expr.t =
                  Ident (Decl_expr.from_name ~ty:d_ty d.var)
                in
                let* a = infer_load_expr lhs rhs in
                Some (infer_location_alias a)
            (* Otherwise, nothing found *)
            | _ -> None
          in
          match s with
          | Some s -> s
          | None ->
              (* fall back to the default parsing of decls *)
              infer_decl d)
      | DeclStmt (d :: l) ->
          Infer_stmt.seq (infer (DeclStmt [ d ])) (infer (DeclStmt l))
      | DeclStmt [] -> Skip
      | SExpr
          (BinaryOperator { opcode = "="; lhs = Ident { ty; _ } as lhs; rhs; _ })
        when J_type.matches C_type.is_pointer ty ->
          infer_load_expr lhs rhs
          |> Option.map infer_location_alias
          |> Option.value ~default:Infer_stmt.Skip
      | SExpr
          (BinaryOperator
             { opcode = "="; lhs = Ident { name = var; _ }; rhs; ty; _ }) ->
          let rhs = infer_expr rhs in
          let ty = J_type.to_c_type ~default:C_type.int ty |> resolve in
          Infer_stmt.Assign { var; ty; data = rhs }
      | ContinueStmt -> Continue
      | BreakStmt -> Break
      | GotoStmt -> Skip
      | ReturnStmt e -> Return (Option.map infer_expr e)
      | SExpr _ -> Skip
      | ForStmt s ->
          let init : Infer_stmt.t =
            s.init
            |> Option.map (fun (f : D_lang.ForInit.t) : Infer_stmt.t ->
                let s : D_lang.Stmt.t =
                  match f with
                  | Decls d -> D_lang.Stmt.DeclStmt d
                  | Expr e -> SExpr e
                in
                infer s)
            |> Option.value ~default:Infer_stmt.Skip
          in
          let cond =
            s.cond |> Option.map infer_expr
            |> Option.value ~default:Infer_exp.true_
          in
          let body = infer s.body in
          let inc = infer s.inc in
          Infer_stmt.For { cond; init; body; inc }
      | DoStmt w ->
          let body = infer w.body in
          let cond = infer_expr w.cond in
          DoWhile (cond, body)
      | WhileStmt w ->
          let body = infer w.body in
          let cond = infer_expr w.cond in
          While (cond, body)
      | SwitchStmt { body = s; _ } | CaseStmt { body = s; _ } | DefaultStmt s ->
          infer s
      | Seq (s1, s2) -> Seq (infer s1, infer s2)
    in
    infer

  type param = (Variable.t * C_type.t, Variable.t * Memory.t) Either.t

  let from_j_error (e : Rjson.j_error) : d_error =
    RootCause (Rjson.error_to_string e)

  let parse_param (ctx : Context.t) (p : Param.t) : Kernel.Parameter.t =
    let mk_array (h : Mem_hierarchy.t) (ty : C_type.t) : Memory.t =
      {
        hierarchy = h;
        size = C_type.get_array_length ty;
        data_type = C_type.get_array_type ty;
      }
    in
    let ty =
      p.ty_var.ty |> J_type.to_c_type_res
      |> Result.map_error from_j_error
      |> Result.map (fun x -> Context.resolve x ctx)
      |> unwrap
    in
    let x = p.ty_var.name in
    if Context.is_enum ty ctx then
      Kernel.Parameter.enum x (Context.get_enum ty ctx)
    else if Context.is_int ty ctx then Kernel.Parameter.scalar x ty
    else if C_type.is_array ty then
      let h =
        if p.is_shared then Mem_hierarchy.SharedMemory
        else Mem_hierarchy.GlobalMemory
      in
      Kernel.Parameter.array x (mk_array h ty)
    else Kernel.Parameter.unsupported x

  let parse_shared (s : D_lang.Stmt.t) : (Variable.t * Memory.t) list =
    let open D_lang in
    let rec find_shared (arrays : (Variable.t * array_t) list) (s : Stmt.t) :
        (Variable.t * array_t) list =
      match s with
      | DeclStmt l ->
          List.filter_map
            (fun (d : Decl.t) ->
              Decl.get_shared d |> Option.map (fun a -> (d.var, a)))
            l
          |> Common.append_tr arrays
      | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _ | GotoStmt
      | ReturnStmt _ | ContinueStmt | BreakStmt | SExpr _ | Skip ->
          arrays
      | Seq (s1, s2) | IfStmt { then_stmt = s1; else_stmt = s2; _ } ->
          let arrays = find_shared arrays s1 in
          find_shared arrays s2
      | ForStmt { body = d; _ }
      | WhileStmt { body = d; _ }
      | DoStmt { body = d; _ }
      | SwitchStmt { body = d; _ }
      | DefaultStmt d
      | CaseStmt { body = d; _ } ->
          find_shared arrays d
    in
    find_shared [] s

  let parse_kernel (ctx : Context.t) (k : D_lang.Kernel.t) :
      Context.t * Imp.Kernel.t =
    let code, return = Infer_stmt.infer (infer_stmt ctx k.code) in
    (* Add inferred shared arrays to global context *)
    let ctx =
      List.fold_left
        (fun ctx (x, m) -> Context.add_array x m ctx)
        ctx (parse_shared k.code)
    in
    (* Parse kernel parameters *)
    let parameters = List.map (parse_param ctx) k.params in
    (* type parameters become global variables because c-t-j doesn't represent
     type instantiations.
    *)
    let rec add_type_params (params : Params.t) : Ty_param.t list -> Params.t =
      function
      | [] -> params
      | TemplateType _ :: l -> add_type_params params l
      | NonTypeTemplate x :: l ->
          let params =
            match J_type.to_c_type_res x.ty with
            | Ok ty when C_type.is_int ty -> Params.add x.name ty params
            | _ -> params
          in
          add_type_params params l
    in
    let global_variables = add_type_params ctx.globals k.type_params in
    let open Imp.Stmt in
    let code = Seq (Context.gen_preamble ctx, code) in
    let open Imp.Kernel in
    ( ctx,
      {
        name = k.name;
        ty = k.ty;
        code;
        parameters;
        global_arrays = ctx.arrays;
        global_variables;
        visibility =
          (match k.attribute with
          | Default -> Visibility.Global
          | Auxiliary -> Visibility.Device);
        block_dim = None;
        grid_dim = None;
        return;
      } )

  let parse_program (p : D_lang.Program.t) : Imp.Kernel.t list =
    let rec parse_p (ctx : Context.t) (p : D_lang.Program.t) : Imp.Kernel.t list
        =
      match p with
      | Declaration v :: l ->
          let b =
            match J_type.to_c_type_res v.ty with
            | Ok ty ->
                (* make sure we resolve the type before we query it *)
                let ty = Context.resolve ty ctx in
                let is_mut = not (C_type.is_const ty) in
                if is_mut && List.mem C_lang.c_attr_shared v.attrs then
                  Context.add_array v.var (Memory.from_type SharedMemory ty) ctx
                else if is_mut && List.mem C_lang.c_attr_device v.attrs then
                  Context.add_array v.var (Memory.from_type GlobalMemory ty) ctx
                else if Context.is_int ty ctx then
                  let g =
                    match v.init with
                    | Some (IExpr n) -> try_to_nexp n
                    | _ -> None
                  in
                  match g with
                  | Some g -> Context.add_assign v.var g ctx
                  | None -> Context.add_global v.var ty ctx
                else ctx
            | Error _ -> ctx
          in
          parse_p b l
      | Kernel k :: l ->
          let ctx, k = parse_kernel ctx k in
          let ks = parse_p ctx l in
          k :: ks
      | Typedef d :: l -> parse_p (Context.add_typedef d ctx) l
      | Enum e :: l -> parse_p (Context.add_enum e ctx) l
      | [] -> []
    in
    let sigs = D_lang.SignatureDB.from_program p in
    parse_p (Context.from_signature_db sigs) p
end

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
