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

(* TODO: replace the string-keyed dedup with an E-graph so equivalence
   classes are captured structurally rather than by stringifying every
   expression we look up. *)

(** Translator state: a per-launch dedup cache keyed by canonical
    (location-stripped) stringification, plus the running list of
    fresh params minted for this launch, newest-first. *)
type t = {
  cache : Variable.t Common.StringMap.t;
  fresh : Ty_variable.t list;
}

let empty : t = { cache = Common.StringMap.empty; fresh = [] }

let fresh_params (st : t) : C_lang.Param.t list =
  st.fresh
  |> List.rev_map (fun ty_var ->
         C_lang.Param.make ~ty_var ~is_used:true ~is_shared:false)

(** Returns a fresh name for [e], reused for equivalent expressions
    in the same launch. *)
let intern (e : C_lang.Expr.t) ~(name : Variable.t) :
    (t, Variable.t) State.t =
  let key = C_lang.Expr.to_string e in
  let ty = C_lang.Expr.to_type e in
  State.update_return (fun st ->
    match Common.StringMap.find_opt key st.cache with
    | Some existing -> (st, existing)
    | None ->
        ( {
            cache = Common.StringMap.add key name st.cache;
            fresh = Ty_variable.make ~name ~ty :: st.fresh;
          },
          name ))

(** Abstracts [e] behind a fresh variable, shared with equivalent
    expressions in the same launch. *)
let abstract (e : C_lang.Expr.t) ~(name : Variable.t) :
    (t, D_lang.Expr.t) State.t =
  let open State.Syntax in
  let ty = C_lang.Expr.to_type e in
  let* name = intern e ~name in
  let d = Decl_expr.from_name ~ty ~kind:Decl_expr.Kind.Var name in
  return (D_lang.Expr.Ident d)

(** Lifts [e] to a [D_lang.Expr.t] if it's side-effect-free —
    literals, idents, and pure arithmetic / conditional / unary
    combinations. Returns [None] on calls, members, subscripts, and
    other impure shapes. *)
let rec lift_pure (e : C_lang.Expr.t) : D_lang.Expr.t option =
  let open D_lang.Expr in
  let ( let* ) = Option.bind in
  match e with
  | Ident d -> Some (Ident d)
  | IntegerLiteral n -> Some (IntegerLiteral n)
  | FloatingLiteral f -> Some (FloatingLiteral f)
  | CharacterLiteral c -> Some (CharacterLiteral c)
  | CXXBoolLiteralExpr b -> Some (CXXBoolLiteralExpr b)
  | BinaryOperator { opcode; lhs; rhs; ty }
    when not
           (List.mem opcode
              [ "="; "+="; "-="; "*="; "/="; "%=";
                "&="; "|="; "^="; "<<="; ">>=" ]) ->
      let* lhs = lift_pure lhs in
      let* rhs = lift_pure rhs in
      Some (BinaryOperator { opcode; lhs; rhs; ty })
  | UnaryOperator { opcode; child; ty }
    when List.mem opcode [ "-"; "+"; "!"; "~" ] ->
      let* child = lift_pure child in
      Some (UnaryOperator { opcode; child; ty })
  | ConditionalOperator { cond; then_expr; else_expr; ty } ->
      let* cond = lift_pure cond in
      let* then_expr = lift_pure then_expr in
      let* else_expr = lift_pure else_expr in
      Some (ConditionalOperator { cond; then_expr; else_expr; ty })
  | _ -> None

let mk_arg_name (idx : int) : Variable.t =
  Variable.from_name (Printf.sprintf "__faial_launch_arg_%d" idx)

let mk_axis_name (base : string) (axis : string) : Variable.t =
  Variable.from_name (Printf.sprintf "__faial_launch_%s_%s" base axis)

(** Decomposition of a pointer-shaped launch argument. *)
type pointer =
  | Pointer of Decl_expr.t
  | Indexed of { base : Decl_expr.t; offset : C_lang.Expr.t }

(** Matches [a], [a + offset], and [offset + a]; otherwise [None]. *)
let rec strip_pointer_offset (e : C_lang.Expr.t) : pointer option =
  let ( let* ) = Option.bind in
  match e with
  | Ident d -> Some (Pointer d)
  | BinaryOperator { opcode = "+"; lhs; rhs; _ } -> (
      match (lhs, rhs) with
      | Ident d, offset | offset, Ident d ->
          Some (Indexed { base = d; offset })
      | _ ->
          let* p = strip_pointer_offset lhs in
          (match p with
           | Pointer d -> Some (Indexed { base = d; offset = rhs })
           (* Limitation: nested offsets like [(a + b) + c] return
              [None] instead of [Indexed { base = a; offset = b + c }]. *)
           | Indexed _ -> None))
  | _ -> None

let rewrite_offset (proposed_name : Variable.t) (off : C_lang.Expr.t) :
    (t, Decl_expr.t) State.t =
  let open State.Syntax in
  match off with
  | Ident d -> return d
  | _ ->
      let ty = C_lang.Expr.to_type off in
      let* name = intern off ~name:proposed_name in
      return (Decl_expr.from_name ~ty ~kind:Decl_expr.Kind.Var name)

(** Rewrites [e] verbatim when possible; defers to [opaque] otherwise.
    Shared between [rewrite_arg] and [rewrite_axis]. *)
let rewrite_pure_or
    (opaque : C_lang.Expr.t -> (t, D_lang.Expr.t) State.t)
    (e : C_lang.Expr.t) : (t, D_lang.Expr.t) State.t =
  let open State.Syntax in
  match lift_pure e with
  | Some pure -> return pure
  | None -> opaque e

(** Rewrites the launch-site argument at position [idx]. *)
let rewrite_arg (idx : int) :
    C_lang.Expr.t -> (t, D_lang.Expr.t) State.t =
  let open State.Syntax in
  rewrite_pure_or (fun e ->
      let ty = C_lang.Expr.to_type e in
      if J_type.matches C_type.is_array ty then
        match strip_pointer_offset e with
        | Some (Indexed { base; offset }) ->
            let off_name =
              Variable.from_name
                (Printf.sprintf "__faial_launch_arg_%d_off" idx)
            in
            let* off_decl = rewrite_offset off_name offset in
            return
              D_lang.Expr.(
                BinaryOperator
                  { opcode = "+"; lhs = Ident base; rhs = Ident off_decl; ty })
        | _ -> abstract e ~name:(mk_arg_name idx)
      else abstract e ~name:(mk_arg_name idx))

(** Given a dim3 expression, extract each axis independently. [None]
    on an axis signals that no static information is available and no
    per-axis constraint should be emitted; fabricating
    [IntegerLiteral 1] for an unknown axis would be unsound. For
    [CXXConstructExpr], unspecified trailing args are CUDA-semantically
    [1] (e.g. [dim3(32)] means [(32, 1, 1)]) and stay [Some].

    Arms are gated by "all args integer-typed" — Clang sometimes
    materialises a 3-arg [CXXConstructExpr] whose first arg is itself
    a dim3-typed copy/move construct (when the launch slot receives an
    existing dim3 value like a function return or struct field). The
    value ctor [dim3(unsigned int, ...)] only applies when each arg is
    integer; the dim3-typed shape falls through to [None]. *)
let dim3_axes (e : C_lang.Expr.t) :
    C_lang.Expr.t option * C_lang.Expr.t option * C_lang.Expr.t option =
  let one : C_lang.Expr.t = IntegerLiteral 1 in
  let is_int_arg (a : C_lang.Expr.t) : bool =
    J_type.matches C_type.is_int (C_lang.Expr.to_type a)
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

(** Rewrites the x/y/z axes of a [gridDim]/[blockDim] expression.
    [None] propagates per axis from [dim3_axes] when that slot can't
    be decomposed. *)
let rewrite_axis (base : string) (e : C_lang.Expr.t) :
    (t, D_lang.Expr.t option * D_lang.Expr.t option * D_lang.Expr.t option)
    State.t =
  let open State.Syntax in
  let xe, ye, ze = dim3_axes e in
  let one (axis : string) (e_opt : C_lang.Expr.t option) =
    match e_opt with
    | None -> return None
    | Some e ->
        let* d =
          rewrite_pure_or
            (fun e -> abstract e ~name:(mk_axis_name base axis))
            e
        in
        return (Some d)
  in
  let* rx = one "x" xe in
  let* ry = one "y" ye in
  let* rz = one "z" ze in
  return (rx, ry, rz)
