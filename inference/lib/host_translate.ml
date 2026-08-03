open Stage0
open Protocols

(** Host-to-kernel expression translation.

    Launch-site expressions are host-side and broadcast as uniform
    constants to every thread; routing them through the in-kernel
    rewriter [D_lang.rewrite_exp] surfaces false-positive races on
    non-[Ident] args. This module rewrites each host expression into
    a [D_lang.Expr.t] usable directly by the synthesiser, abstracting
    opaque sub-expressions behind fresh variables (deduped per
    launch). *)

(** Translator state: a per-launch dedup cache keyed structurally by
    [D_lang.Expr.compare], plus the running list of fresh params minted
    for this launch, newest-first. *)
type t = {
  cache : Variable.t D_lang.Expr.Map.t;
  fresh : Ty_variable.t list;
}

let empty : t = { cache = D_lang.Expr.Map.empty; fresh = [] }

let to_string (st : t) : string =
  st.cache
  |> D_lang.Expr.Map.bindings
  |> List.map (fun (e, v) ->
         Printf.sprintf "%s = %s" (Variable.name v) (D_lang.Expr.to_string e))
  |> String.concat "\n"

let fresh_params (st : t) : C_lang.Param.t list =
  st.fresh
  |> List.rev_map (fun ty_var ->
         C_lang.Param.make ~ty_var ~is_used:true ~is_shared:false)

(** Build a [$read_<arr>(idx...)] call from a read. *)
let read_to_call (r : D_lang.d_read) : D_lang.Expr.t =
  let read_name = Variable.update_name (fun x -> "$read_" ^ x) r.source.name in
  CallExpr {
    func = D_lang.Expr.ident read_name;
    args = r.source.index;
    ty = r.ty;
  }

(** A simplistic substitution function for statements that is only
    enough to handle the result of D_lang.rewrite_expr. *)
let rec subst_in_stmt (var : Variable.t) (e_new : D_lang.Expr.t)
    (stmt : D_lang.Stmt.t) : D_lang.Stmt.t =
  let subst = D_lang.Expr.subst var e_new in
  let subst_subscript (s : D_lang.d_subscript) : D_lang.d_subscript =
    { s with selector = List.map subst s.selector;
             index = List.map subst s.index }
  in
  let subst_decl : D_lang.Decl.t -> D_lang.Decl.t = D_lang.Decl.map subst in
  match stmt with
  | Seq (a, b) ->
      Seq (subst_in_stmt var e_new a, subst_in_stmt var e_new b)
  | ReadAccessStmt r ->
      ReadAccessStmt
        { r with source = subst_subscript r.source;
                 guard = Option.map subst r.guard }
  | WriteAccessStmt w ->
      WriteAccessStmt
        { w with target = subst_subscript w.target; source = subst w.source;
                 guard = Option.map subst w.guard }
  | AtomicAccessStmt a ->
      AtomicAccessStmt
        { a with source = subst_subscript a.source;
                 guard = Option.map subst a.guard }
  | DeclStmt ds -> DeclStmt (List.map subst_decl ds)
  | SExpr e -> SExpr (subst e)
  | _ -> stmt

let rec inline_reads ((stmt, e) : D_lang.Stmt.t * D_lang.Expr.t) : D_lang.Expr.t =
  match stmt with
  | Seq (DeclStmt [{var; init = Some (IExpr v); _}], stmt) ->
    inline_reads (subst_in_stmt var v stmt, D_lang.Expr.subst var v e)
  | Seq (DeclStmt [{init = Some (InitListExpr _); _}], stmt)
  | Seq (DeclStmt [{init = Some (CXXConstructExpr _); _}], stmt)
  | Seq (DeclStmt [{init = None; _}], stmt)
  | Seq (DeclStmt [], stmt) ->
    inline_reads (stmt, e)
  | Seq (DeclStmt (d :: l), stmt) ->
    inline_reads (Seq (DeclStmt [d], Seq (DeclStmt l, stmt)), e)
  | Seq (ReadAccessStmt ({ target; _ } as r), rest) ->
      let call = read_to_call r in
      let rest = subst_in_stmt target call rest in
      let e = D_lang.Expr.subst target call e in
      inline_reads (rest, e)
  | Seq (stmt1, stmt2) ->
      let e = inline_reads (stmt1, e) in
      inline_reads (stmt2, e)
  | DeclStmt _ | ReadAccessStmt _ -> inline_reads (Seq (stmt, Skip), e)
  | _ -> e

let make_var ?label ?location (st : t) : Variable.t =
  let count = D_lang.Expr.Map.cardinal st.cache in
  Variable.make
    ~name:("@Launch" ^ string_of_int count)
    ?label ?location ~kind:LaunchParameter ()

let abstract (e : D_lang.Expr.t) : (t, D_lang.Expr.t) State.t =
  let ty = D_lang.Expr.to_type e in
  State.update_return (fun st ->
    let (st, name) =
      match D_lang.Expr.Map.find_opt e st.cache with
      | Some name -> (st, name)
      | None ->
        let name = make_var st in
        ({
              cache = D_lang.Expr.Map.add e name st.cache;
              fresh = Ty_variable.make ~name ~ty :: st.fresh;
            }, name)
    in
      (st, D_lang.Expr.ident ~ty name)
  )


(** A pointer-valued launch argument spelled [arr[i]], where [arr] is a
    host-side array, names one of the pointers stored in [arr]. Reading
    that element would abstract the argument into an opaque scalar, and
    a kernel parameter bound to a scalar has no location for its
    accesses to land on, so every access through the parameter would be
    dropped. The argument aliases [arr] itself instead. This conflates
    the elements of [arr]: two arguments taken from the same array are
    seen as one location, so their accesses are compared rather than
    ignored. *)
let pointer_array_base (e : C_lang.Expr.t) : Decl_expr.t option =
  let rec base : C_lang.Expr.t -> Decl_expr.t option = function
    | ArraySubscriptExpr { lhs = Ident d; _ } when Ty.is_array d.ty -> Some d
    | ArraySubscriptExpr { lhs; _ } -> base lhs
    | _ -> None
  in
  if Ty.is_array_or_pointer (C_lang.Expr.to_type e) then base e else None

let rewrite_expr (e : C_lang.Expr.t) : (t, D_lang.Expr.t) State.t =
  match pointer_array_base e with
  | Some d -> State.return (D_lang.Expr.Ident d)
  | None ->
      e
      (* convert from C_lang.Expr.t to D_lang.Expr.t *)
      |> D_lang.rewrite_exp
      (* unpack from the monadic result *)
      |> D_lang.run0
      (* apply rewrites of reads *)
      |> inline_reads
      (* abstract these following operations *)
      |> D_lang.Expr.st_map (fun e ->
          match e with
          (* Calls to whitelisted pure functions / predicates survive into
             [D_lang.Expr] so [d_to_imp] can lift them to [NCall] / [Pred].
             The Z3 encoder then treats matching names as the same UF
             symbol across launches, preserving cross-call-site sharing
             that an opaque [@LaunchN] abstraction would lose. *)
          | CallExpr { func = Ident { name = f; _ }; _ }
            when Functions.supported (Variable.name f)
                 || Predicates.supported (Variable.name f)
                 || Exp.is_uniformity_intrinsic (Variable.name f) ->
              State.return e
          | CXXNewExpr _
          | CXXDeleteExpr _
          | CallExpr _
          | CXXConstructExpr _
          | MemberExpr _ -> abstract e
          | _ ->
          (* Otherwise, leave intact *)
          State.return e
        )


let unpack_dim3 (e : C_lang.Expr.t) :
    C_lang.Expr.t option * C_lang.Expr.t option * C_lang.Expr.t option =
  let one : C_lang.Expr.t = IntegerLiteral 1 in
  let is_int_arg (a : C_lang.Expr.t) : bool =
    Ty.is_int (Ty.strip_reference (C_lang.Expr.to_type a))
  in
  match e with
  | CXXConstructExpr { args = [ x; y; z ]; _ }
    when is_int_arg x && is_int_arg y && is_int_arg z ->
      (Some x, Some y, Some z)
  | CXXConstructExpr { args = [ x; y ]; _ }
    when is_int_arg x && is_int_arg y ->
      (Some x, Some y, Some one)
  | CXXConstructExpr { args = [ x ]; _ } when is_int_arg x ->
      (Some x, Some one, Some one)
  | _ -> (None, None, None)

(** Rewrites a [gridDim]/[blockDim] dim3 expression into its x/y/z
    axes. [None] propagates per axis from [dim3_axes] when that slot
    can't be decomposed. *)
let rewrite_dim3 (e : C_lang.Expr.t) :
    (t, D_lang.Expr.t option * D_lang.Expr.t option * D_lang.Expr.t option)
    State.t =
  let open State.Syntax in
  let xe, ye, ze = unpack_dim3 e in
  let one = State.option_map rewrite_expr in
  let* rx = one xe in
  let* ry = one ye in
  let* rz = one ze in
  return (rx, ry, rz)
