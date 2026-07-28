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
  type t = Ty.t StringMap.t

  let empty : t = StringMap.empty

  (* Resolve a type according to the alias in the database *)
  let resolve (ty : Ty.t) (db : t) : Ty.t =
    StringMap.find_opt (Ty.to_string ty) db |> Option.value ~default:ty

  (* Add a new type alias to the data-base *)
  let add (x : Typedef.t) (db : t) : t =
    (* Resolve the type so that there are no indirect alias *)
    StringMap.add x.name (resolve x.ty db) db
end

module Make (L : Logger) = struct
  let parse_bin ?(sign = Signedness.Signed) (op : string)
      (l : Imp.Infer_exp.t) (r : Infer_exp.t) : Infer_exp.t =
    match op with
    (* bool -> bool -> bool *)
    | "||" -> BExp (BRel (BOr, l, r))
    | "&&" -> BExp (BRel (BAnd, l, r))
    (* int -> int -> bool *)
    | "==" -> BExp (NRel (Eq, l, r))
    | "!=" -> BExp (NRel (Neq, l, r))
    | "<=" -> BExp (NRel (Le sign, l, r))
    | "<" -> BExp (NRel (Lt sign, l, r))
    | ">=" -> BExp (NRel (Ge sign, l, r))
    | ">" -> BExp (NRel (Gt sign, l, r))
    (* int -> int -> int *)
    | "+" -> NExp (Binary (Plus sign, l, r))
    | "-" -> NExp (Binary (Minus sign, l, r))
    | "*" -> NExp (Binary (Mult sign, l, r))
    | "/" -> NExp (Binary (Div sign, l, r))
    | "%" -> NExp (Binary (Mod sign, l, r))
    | ">>" -> NExp (Binary (RightShift sign, l, r))
    | "<<" -> NExp (Binary (LeftShift, l, r))
    | "^" -> NExp (Binary (BitXOr, l, r))
    | "|" -> NExp (Binary (BitOr, l, r))
    | "&" -> NExp (Binary (BitAnd, l, r))
    | _ ->
        L.warning (fun () -> "parse_bin: rewriting to unknown binary operator: " ^ op);
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
    | SizeOfExpr ty ->
        let size = Ty.sizeof ty |> Option.value ~default:4 in
        L.warning (fun () ->
          "sizeof(" ^ Ty.to_string ty ^ ") = " ^ string_of_int size);
        NExp (Num size)
    | IntegerLiteral n | CharacterLiteral n -> NExp (Num n)
    | FloatingLiteral n ->
        L.warning (fun () ->
          "parse_nexp: converting float '" ^ Float.to_string n ^ "' to integer");
        NExp (Num (Float.to_int n))
    | ConditionalOperator o ->
        let b = infer_expr o.cond in
        let n1 = infer_expr o.then_expr in
        let n2 = infer_expr o.else_expr in
        NExp (NIf (b, n1, n2))
    | UnaryOperator { opcode = "~"; child = e; _ } ->
        let n = infer_expr e in
        NExp (Unary (BitNot, n))
    (* The thread-uniformity intrinsics are the surface syntax for the
       cross-thread primitive, so they become [IsThreadUnif] here rather
       than a [Pred] the inliner would have to lower later. *)
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args = [ arg ]; _ }
      when Exp.is_uniformity_intrinsic (Variable.name f) ->
        let n = infer_expr arg in
        BExp
          (if String.equal (Variable.name f) Exp.is_thread_unif_name then
             Infer_exp.is_thread_unif n
           else Infer_exp.is_thread_distinct n)
    (* Whitelisted pure functions / predicates are lifted to [NCall]
       / [Pred] nodes; their lowering bodies live in [Functions] /
       [Predicates] and run at [Constfold] / [Predicates.b_inline]
       time. [Functions.supported] covers [divUp] / [min] / [max] /
       [log2] / [log] / [sqrt] / [__ffs] / [__clz]; [Predicates.supported]
       covers [__is_pow2]. *)
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args; _ }
      when Functions.supported (Variable.name f) ->
        let args = List.map infer_expr args in
        NExp (NCall (Variable.name f, args))
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args; _ }
      when Predicates.supported (Variable.name f) ->
        let args = List.map infer_expr args in
        BExp (Pred (Variable.name f, args))
    | BinaryOperator { lhs = l; opcode = "&"; rhs = IntegerLiteral 1; _ } ->
        let n = infer_expr l in
        BExp
          (Infer_exp.n_neq
             (NExp (Binary (Mod Signedness.Signed, n, NExp (Num 2))))
             (NExp (Num 0)))
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
             (BExp (Pred ("pow2", [ n ])))
             (BExp (Infer_exp.n_eq n (NExp (Num 0)))))
    | BinaryOperator { opcode = ","; lhs = _; rhs = e; _ } -> infer_expr e
    | BinaryOperator { opcode = o; lhs = n1; rhs = n2; ty } ->
        (* Clang applies C's usual arithmetic conversions and types an
           arithmetic operator with their result, so the operator already
           carries the answer. A comparison does not: its type is the result
           type, bool, so its signedness has to come from the operands, which
           clang has promoted for the same reason. *)
        let sign : Signedness.t =
          let unsigned =
            match o with
            | "<" | "<=" | ">" | ">=" ->
                Ty.is_unsigned (D_lang.Expr.to_type n1)
                || Ty.is_unsigned (D_lang.Expr.to_type n2)
            | _ -> Ty.is_unsigned ty
          in
          if unsigned then Unsigned else Signed
        in
        let n1 = infer_expr n1 in
        let n2 = infer_expr n2 in
        parse_bin ~sign o n1 n2
    | Convert c -> (
        let arg = infer_expr c.arg in
        match Ty.to_scalar c.ty with
        | Some ty -> NExp (Convert { ty; arg })
        | None -> arg)
    | CXXBoolLiteralExpr b -> BExp (Bool b)
    | UnaryOperator u when u.opcode = "!" ->
        let b = infer_expr u.child in
        BExp (BNot b)
    (* [&x] denotes the same location as [x], and the argument classifier
       reads the base variable out of the expression, so the address-of
       must not reach the unknown arm below. Only a bare variable is
       unwrapped: [&a[i]] would sequence a read of [a] that the source
       never performs. *)
    | UnaryOperator { opcode = "&"; child = Ident _ as child; _ } ->
        infer_expr child
    | CXXConstructExpr { args = [ Ident v ]; ty }
      when Ty.vector_lanes ty |> Option.is_some ->
        NExp (Var v.name)
    | RecoveryExpr _ | CXXConstructExpr _ | MemberExpr _ | CallExpr _
    | UnaryOperator _ | CXXOperatorCallExpr _ | UnresolvedLookupExpr _ ->
        let lbl = D_lang.Expr.to_string e in
        L.warning (fun () -> "parse_exp: rewriting to unknown: " ^ lbl);
        Unknown lbl
    | _ ->
        failwith
          ("WARNING: parse_nexp: unsupported expression " ^ D_lang.Expr.name e
         ^ " : " ^ D_lang.Expr.to_string e)

  let to_nexp (e : D_lang.Expr.t) : Exp.nexp Infer_exp.state =
    Infer_exp.to_nexp (infer_expr e)

  let try_to_nexp (e : D_lang.Expr.t) : Exp.nexp option =
    e |> infer_expr |> Infer_exp.to_nexp |> Infer_exp.no_unknowns

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

    let resolve (ty : Ty.t) (b : t) : Ty.t =
      TypeAlias.resolve ty b.typedefs

    let lookup_sig (e : D_lang.Expr.t) (arg_count : int) (db : t) :
        D_lang.SignatureDB.Signature.t option =
      D_lang.SignatureDB.lookup e arg_count db.sigs

    let is_enum (ty : Ty.t) (ctx : t) : bool =
      let name = Ty.to_string ty |> Variable.from_name in
      Variable.Map.mem name ctx.enums

    let is_int (ty : Ty.t) (ctx : t) : bool =
      Ty.is_int ty || is_enum ty ctx

    let get_enum (ty : Ty.t) (ctx : t) : Enum.t =
      let name = Ty.to_string ty |> Variable.from_name in
      Variable.Map.find name ctx.enums

    let add_array (var : Variable.t) (m : Memory.t) (b : t) : t =
      { b with arrays = Variable.Map.add var m b.arrays }

    let add_assign (var : Variable.t) (n : Exp.nexp) (b : t) : t =
      { b with assigns = (var, n) :: b.assigns }

    let add_global (var : Variable.t) (ty : Ty.t) (b : t) : t =
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
      when Ty.is_pointer ty
           || Ty.is_array_or_pointer ty ->
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
        Variable.from_name "__builtin_assume";
      ]

  (* CUDA vector lanes are named [x], [y], [z], [w] in argument order. *)
  let axes_of_arity : int -> string list option = function
    | 1 -> Some [ "x" ]
    | 2 -> Some [ "x"; "y" ]
    | 3 -> Some [ "x"; "y"; "z" ]
    | 4 -> Some [ "x"; "y"; "z"; "w" ]
    | _ -> None

  (* A CUDA vector constructor [make_<type><N>] initialises components
     [x]..[w] from its [N] arguments in order. *)
  let vector_ctor_fields (name : string) : string list option =
    if String.starts_with ~prefix:"make_" name then
      axes_of_arity (Char.code name.[String.length name - 1] - Char.code '0')
    else None

  (* Lane axes of a CUDA vector type ([uint2], [const uint3], ...). *)
  let vector_type_axes (ty : Ty.t) : string list option = Ty.vector_lanes ty

  let infer_stmt (ctx : Context.t) : D_lang.Stmt.t -> Imp.Infer_stmt.t =
    let resolve ty = Context.resolve ty ctx in

    let infer_type (ty : Ty.t) : Ty.t = Context.resolve ty ctx in

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
        |> List.map (fun ty -> Context.resolve ty ctx)
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
          let ty = Ty.to_string d.ty in
          L.warning (fun () ->
            "parse_decl: skipping non-int local variable '" ^ x ^ "' "
           ^ "type: " ^ ty);
          Skip
    in

    let infer_call ?(result = None) (func : D_lang.Expr.t)
        (args : D_lang.Expr.t list) : Infer_stmt.t =
      let arg_count = List.length args in
      match (func, result) with
      (* Model [v = make_uintN(a, ...)] as per-component assignments
         [v.x := a; ...] so downstream member reads [v.x] resolve,
         instead of leaving [v] an opaque call result. *)
      | Ident { name = f; _ }, Some (var, _)
        when (match vector_ctor_fields (Variable.name f) with
              | Some fields -> List.length fields = arg_count
              | None -> false) ->
          let fields = Option.get (vector_ctor_fields (Variable.name f)) in
          List.map2
            (fun field arg ->
              let member =
                Variable.update_name (fun n -> n ^ "." ^ field) var
              in
              Infer_stmt.Assign
                { var = member; ty = Ty.int; data = infer_expr arg })
            fields args
          |> Infer_stmt.from_list
      | _ -> (
          match Context.lookup_sig func arg_count ctx with
          | Some s when List.length s.params = arg_count ->
              let open Imp.Infer_stmt in
              Call
                {
                  result;
                  kernel = s.kernel;
                  ty = s.ty;
                  args = List.map infer_expr args;
                }
          (* Either no signature found, or the matched signature has a
             different param count — happens with variadic-template /
             pack-expansion specialisations whose ty-string aliases a
             stored entry. Skip rather than abort the whole analysis. *)
          | Some _ | None -> Skip)
    in

    let rec infer : D_lang.Stmt.t -> Imp.Infer_stmt.t = function
      | Skip -> Skip
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = []; _ })
        when Variable.name n = "__syncthreads" ->
          Sync (Sync.syncthreads ?loc:n.location ())
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = [ _ ]; _ })
        when Variable.name n = "sync" ->
          Sync (Sync.syncthreads ?loc:n.location ())
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
          let guard = Option.map infer_expr w.guard in
          Infer_stmt.Write { array; index; payload = w.payload; guard }
      | ReadAccessStmt r ->
          let array =
            r.source.name |> Variable.set_location r.source.location
          in
          let index = List.map infer_expr r.source.index in
          let ty = r.ty |> resolve |> Ty.strip_array in
          let guard = Option.map infer_expr r.guard in
          Infer_stmt.Read { target = Some (ty, r.target); array; index; guard }
      | AtomicAccessStmt r ->
          let array =
            r.source.name |> Variable.set_location r.source.location
          in
          let index = List.map infer_expr r.source.index in
          let ty = r.ty |> resolve |> Ty.strip_array in
          let atomic = Atomic.map infer_expr r.atomic in
          let guard = Option.map infer_expr r.guard in
          Infer_stmt.Atomic
            {
              target = r.target;
              atomic;
              array;
              index;
              ty;
              guard;
            }
      | IfStmt { cond; then_stmt; else_stmt } ->
          Imp.Infer_stmt.If (infer_expr cond, infer then_stmt, infer else_stmt)
      (* Support for location aliasing that declares a new variable *)
      | DeclStmt [ d ] -> (
          let ( let* ) = Option.bind in
          (* Detect non-standard declarations: *)
          let s =
            match d with
            (* Detect kernel-calls: *)
            | { init = Some (IExpr (CallExpr { func; args; _ })); _ }
              when Context.lookup_sig func (List.length args) ctx
                   |> Option.is_some ->
                (* Found a kernel call, so extract the call and parse
                   the rest of the declaration yet unsetting the first
                   decl. The signature lookup gates this arm so calls
                   to pure functions (e.g. [log2]) fall through to
                   [infer_decl], which lifts them via the [Functions]
                   registry into an [NCall]-init decl. *)
                let ty = infer_type d.ty in
                Some (infer_call ~result:(Some (d.var, ty)) func args)
            (* Detect a by-value vector copy [uintN v = w]: bind each
               lane [v.x := w.x; ...] so the caller's component reads
               resolve through the temporary the compiler introduces
               for a by-value struct result. *)
            | { ty; init = Some (IExpr (Ident src)); _ }
              when vector_type_axes ty |> Option.is_some ->
                let axes = Option.get (vector_type_axes ty) in
                Some
                  (List.map
                     (fun axis ->
                       let lane = Variable.update_name (fun n -> n ^ "." ^ axis) in
                       Infer_stmt.Assign
                         {
                           var = lane d.var;
                           ty = Ty.int;
                           data = NExp (Var (lane src.name));
                         })
                     axes
                  |> Infer_stmt.from_list)
            (* Detect array alias: *)
            | { ty; init = Some (IExpr rhs); _ }
              when Ty.is_pointer ty || Ty.is_auto ty ->
                let d_ty = resolve d.ty in
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
        when Ty.is_pointer ty ->
          infer_load_expr lhs rhs
          |> Option.map infer_location_alias
          |> Option.value ~default:Infer_stmt.Skip
      | SExpr
          (BinaryOperator
             { opcode = "="; lhs = Ident { name = var; _ }; rhs; ty; _ }) ->
          let rhs = infer_expr rhs in
          let ty = ty |> resolve in
          Infer_stmt.Assign { var; ty; data = rhs }
      | SExpr
          (BinaryOperator
             { opcode = "=";
               lhs = MemberExpr { base = Ident base; name = field; _ };
               rhs; ty; _ }) ->
          let var =
            base.name |> Variable.update_name (fun n -> n ^ "." ^ field)
          in
          let rhs = infer_expr rhs in
          let ty = ty |> resolve in
          Infer_stmt.Assign { var; ty; data = rhs }
      (* [++x] / [--x] / [x++] / [x--] as a statement-expression. Clang
         normalises these to [x = x + 1] inside [for]-loop inc slots
         before c-to-json sees them, but they survive in other
         positions (e.g. statement-expressions, synthesised increments
         from CXXForRangeStmt lowering). Lower them to the same
         Assign shape so the loop-inference in [imp/lib/for.ml]
         recognises them as well-formed increments and avoids
         falling back to an unbounded [Star]. *)
      | SExpr
          (UnaryOperator
             { opcode = ("++" | "--") as opcode;
               child = Ident { name = var; _ };
               ty;
             }) ->
          let op : N_binary.t =
            if opcode = "++" then Plus Signedness.Signed
            else Minus Signedness.Signed
          in
          let data : Infer_exp.t =
            NExp (Binary (op, NExp (Var var), NExp (Num 1)))
          in
          let ty = ty |> resolve in
          Infer_stmt.Assign { var; ty; data }
      | ContinueStmt -> Continue
      | BreakStmt -> Break
      | GotoStmt -> Skip
      (* A vector-constructor return [return make_uintN(a, ...)] cannot
         be a single scalar return value, so lower it to per-lane
         assignments on a synthetic return variable and return that
         variable; the inliner then binds the caller's lanes from it. *)
      | ReturnStmt
          (Some (CallExpr { func = Ident { name = f; _ }; args; _ }))
        when (match vector_ctor_fields (Variable.name f) with
              | Some fields -> List.length fields = List.length args
              | None -> false) ->
          let fields = Option.get (vector_ctor_fields (Variable.name f)) in
          let retvar = Variable.from_name "@vec_return" in
          List.map2
            (fun field arg ->
              let lane = Variable.update_name (fun n -> n ^ "." ^ field) retvar in
              Infer_stmt.Assign
                { var = lane; ty = Ty.int; data = infer_expr arg })
            fields args
          @ [ Infer_stmt.Return (Some (NExp (Var retvar))) ]
          |> Infer_stmt.from_list
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
      | AsmStmt a ->
          (* Outputs precede inputs in %N indexing, per GCC inline-asm. *)
          let operands : Exp.nexp option list =
            (a.outputs @ a.inputs)
            |> List.map (fun (o : D_lang.Expr.t Asm.operand) ->
                   try_to_nexp o.expr)
          in
          (match Ptx.parse ?loc:a.loc ~operands a.asm_string with
           | Some s -> Infer_stmt.Sync s
           | None ->
               L.warning (fun () ->
                 "asm: dropping (unrecognized PTX template): " ^ a.asm_string);
               Skip)
      | BarrierOp { op; target; args = _; loc } ->
          let mode : Sync.Mode.t =
            match op with
            | Arrive -> Sync.Mode.Arrive
            | Wait -> Sync.Mode.Wait
            | ArriveAndWait -> Sync.Mode.ArriveAndWait
            | ArriveAndDrop -> Sync.Mode.ArriveAndDrop
          in
          let index = List.map infer_expr target.index in
          Infer_stmt.SyncOp { mode; array = target.name; index; loc }
      | Seq (s1, s2) -> Seq (infer s1, infer s2)
      | LambdaDecl _ ->
          (* [Lift_lambdas.lift_program] runs at the start of
             [parse_program] and removes every [LambdaDecl]. *)
          failwith
            "D_to_imp.infer: LambdaDecl leaked past Lift_lambdas — \
             pass not run?"
    in
    infer

  type param = (Variable.t * Ty.t, Variable.t * Memory.t) Either.t

  let parse_param ~(expand_vectors : bool) (ctx : Context.t) (p : Param.t) :
      Kernel.Parameter.t list =
    let mk_array (h : Mem_hierarchy.t) (ty : Ty.t) : Memory.t =
      {
        hierarchy = h;
        size = Ty.get_array_length ty;
        data_type = Ty.get_array_type ty;
      }
    in
    let ty = Context.resolve p.ty_var.ty ctx in
    (* A const reference cannot be assigned through, so the parameter
       binds the referent's value and is classified as the referent. A
       mutable reference names storage the callee can write, which the
       substrate has no term for, so it stays unsupported. *)
    let ty =
      Ty.deref_const ty
      |> Option.map (fun ty -> Context.resolve ty ctx)
      |> Option.value ~default:ty
    in
    let x = p.ty_var.name in
    if Context.is_enum ty ctx then
      [ Kernel.Parameter.enum x (Context.get_enum ty ctx) ]
    else if Context.is_int ty ctx then [ Kernel.Parameter.scalar x ty ]
    else if Ty.is_array_or_pointer ty then
      let h =
        if p.is_shared then Mem_hierarchy.SharedMemory
        else Mem_hierarchy.GlobalMemory
      in
      [ Kernel.Parameter.array x (mk_array h ty) ]
    else
      match (if expand_vectors then vector_type_axes p.ty_var.ty else None) with
      (* A vector param [uintN v] exposes each lane [v.x], [v.y], ... as
         a uniform scalar parameter, so component reads resolve to a
         per-launch value rather than a thread-divergent free var. Only
         top-level kernels expand: a device function's vector params are
         bound through inlining, where lane-splitting would break the
         call's argument arity. *)
      | Some axes ->
          List.map
            (fun axis ->
              let lane = Variable.update_name (fun n -> n ^ "." ^ axis) x in
              Kernel.Parameter.scalar lane Ty.int)
            axes
      | None -> [ Kernel.Parameter.unsupported x ty ]

  let parse_shared (ctx : Context.t) (s : D_lang.Stmt.t) :
      (Variable.t * Memory.t) list =
    let open D_lang in
    (* A decl declares an array of barriers iff its element type, after
       typedef resolution, is [cuda::barrier<_>]. Such decls are identity-only
       (the array names a set of named barriers) and must not be added to
       data-memory tracking. *)
    let is_barrier_decl (d : Decl.t) : bool =
      let resolved = Context.resolve (Ty.strip_array d.ty) ctx in
      C_lang.BarrierOp.is_barrier_c_type resolved
      || C_lang.BarrierOp.is_barrier_base_type d.ty
    in
    let rec find_shared (arrays : (Variable.t * array_t) list) (s : Stmt.t) :
        (Variable.t * array_t) list =
      match s with
      | DeclStmt l ->
          List.filter_map
            (fun (d : Decl.t) ->
              if is_barrier_decl d then None
              else Decl.get_shared d |> Option.map (fun a -> (d.var, a)))
            l
          |> Common.append_tr arrays
      | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _ | GotoStmt
      | ReturnStmt _ | ContinueStmt | BreakStmt | SExpr _ | AsmStmt _
      | BarrierOp _ | Skip | LambdaDecl _ ->
          (* [LambdaDecl] is removed by [Lift_lambdas.lift_program]
             before [parse_kernel] runs; if one survives here, it
             carries no shared declarations the caller could see. *)
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
    let code, return =
      infer_stmt ctx k.code
      |> Imp.Atomic_seed_read.rewrite
      |> Infer_stmt.infer
    in
    (* Add inferred shared arrays to global context *)
    let ctx =
      List.fold_left
        (fun ctx (x, m) -> Context.add_array x m ctx)
        ctx (parse_shared ctx k.code)
    in
    (* Parse kernel parameters *)
    let expand_vectors =
      match k.attribute with KernelAttr.Default -> true | _ -> false
    in
    let parameters =
      List.concat_map (parse_param ~expand_vectors ctx) k.params
    in
    (* type parameters become global variables because c-t-j doesn't represent
     type instantiations.
    *)
    let rec add_type_params (params : Params.t) : Ty_param.t list -> Params.t =
      function
      | [] -> params
      | TemplateType _ :: l -> add_type_params params l
      | NonTypeTemplate x :: l ->
          let params =
            if Ty.is_int x.ty then Params.add x.name x.ty params else params
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
    (* Hoist C++ lambdas into synthetic [D_lang.Kernel.t] entries with
       [Auxiliary] visibility before parsing. The synthetic kernels
       become regular [Imp.Kernel.t] with [Visibility.Device] and are
       inlined by [Imp.Inline_calls]. *)
    let p = Lift_lambdas.lift_program p in
    let rec parse_p (ctx : Context.t) (p : D_lang.Program.t) : Imp.Kernel.t list
        =
      match p with
      | Declaration v :: l ->
          let b =
            (* Skip arrays of barriers: they're identity-only, not data memory. *)
            if C_lang.BarrierOp.is_barrier_base_type v.ty then ctx
            else
              (* make sure we resolve the type before we query it *)
              let ty = Context.resolve v.ty ctx in
              let is_mut = not (Ty.is_const ty) in
              if is_mut && List.mem C_lang.c_attr_shared v.attrs then
                Context.add_array v.var
                  (Memory.from_type SharedMemory ty) ctx
              else if is_mut && List.mem C_lang.c_attr_device v.attrs then
                Context.add_array v.var
                  (Memory.from_type GlobalMemory ty) ctx
              else if Context.is_int ty ctx then
                (* Fold a global's initializer into a constant only
                   when it is immutable: a C-level [const], or a
                   global cu-to-json proved is never written (the
                   [c_attr_immutable] tag). A mutable global, e.g. one
                   accumulated at runtime before a launch, would
                   otherwise be pinned to its stale initializer;
                   without proof of immutability, keep it symbolic. *)
                let immutable =
                  (not is_mut)
                  || List.mem C_lang.c_attr_immutable v.attrs
                in
                let g =
                  if immutable then
                    (match v.init with
                     | Some (IExpr n) -> try_to_nexp n
                     | _ -> None)
                  else None
                in
                match g with
                | Some g -> Context.add_assign v.var g ctx
                | None -> Context.add_global v.var ty ctx
              else ctx
          in
          parse_p b l
      | Kernel k :: l ->
          let ctx, k = parse_kernel ctx k in
          let ks = parse_p ctx l in
          k :: ks
      | Typedef d :: l -> parse_p (Context.add_typedef d ctx) l
      | Enum e :: l -> parse_p (Context.add_enum e ctx) l
      | LaunchParam _ :: l ->
          (* Launch metadata flows through the pipeline as data only;
             d_to_imp produces Imp.Kernel.t which has no slot for
             launches. Drop here until a downstream stage consumes. *)
          parse_p ctx l
      | [] -> []
    in
    let sigs = D_lang.SignatureDB.from_program p in
    parse_p (Context.from_signature_db sigs) p
end

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
