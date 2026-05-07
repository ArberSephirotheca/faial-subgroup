open Stage0
open Protocols

(** Launch-site argument resolution.

    Launch-site expressions are host-side and broadcast as uniform
    constants to every thread; routing them through the in-kernel
    rewriter [D_lang.rewrite_exp] surfaces false-positive races on
    non-[Ident] args. This module resolves each launch arg to a
    [D_lang.Expr.t] usable directly by the synthesiser, abstracting
    opaque sub-expressions behind fresh variables (deduped per
    launch). *)

module Equiv = struct
  (* TODO: replace the string-keyed dedup with an E-graph so equivalence
     classes are captured structurally rather than by stringifying every
     expression we look up. *)

  (** Resolver state: a per-launch dedup cache keyed by canonical
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
end

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

let resolve_offset (proposed_name : Variable.t) (off : C_lang.Expr.t) :
    (Equiv.t, Decl_expr.t) State.t =
  let open State.Syntax in
  match off with
  | Ident d -> return d
  | _ ->
      let ty = C_lang.Expr.to_type off in
      let* name = Equiv.intern off ~name:proposed_name in
      return (Decl_expr.from_name ~ty ~kind:Decl_expr.Kind.Var name)

(** Resolves [e] verbatim when possible; defers to [opaque] otherwise.
    Shared between [resolve] and [resolve_axis]. *)
let resolve_pure_or
    (opaque : C_lang.Expr.t -> (Equiv.t, D_lang.Expr.t) State.t)
    (e : C_lang.Expr.t) : (Equiv.t, D_lang.Expr.t) State.t =
  let open State.Syntax in
  match lift_pure e with
  | Some pure -> return pure
  | None -> opaque e

(** Resolves the launch-site argument at position [idx]. *)
let resolve (idx : int) :
    C_lang.Expr.t -> (Equiv.t, D_lang.Expr.t) State.t =
  let open State.Syntax in
  resolve_pure_or (fun e ->
      let ty = C_lang.Expr.to_type e in
      if J_type.matches C_type.is_array ty then
        match strip_pointer_offset e with
        | Some (Indexed { base; offset }) ->
            let off_name =
              Variable.from_name
                (Printf.sprintf "__faial_launch_arg_%d_off" idx)
            in
            let* off_decl = resolve_offset off_name offset in
            return
              D_lang.Expr.(
                BinaryOperator
                  { opcode = "+"; lhs = Ident base; rhs = Ident off_decl; ty })
        | _ -> Equiv.abstract e ~name:(mk_arg_name idx)
      else Equiv.abstract e ~name:(mk_arg_name idx))

(** Resolves one of x/y/z of [gridDim]/[blockDim]. *)
let resolve_axis (base : string) (axis : string) :
    C_lang.Expr.t -> (Equiv.t, D_lang.Expr.t) State.t =
  resolve_pure_or (fun e ->
      Equiv.abstract e ~name:(mk_axis_name base axis))
