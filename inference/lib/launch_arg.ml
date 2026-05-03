open Protocols

(* Launch-site argument resolution.

   A launch site is host code: it executes once before kernel dispatch,
   and the value of each launch-site expression is broadcast to every
   thread of the launch as a uniform constant. Crucially, this is
   *not* the same as in-kernel C-to-D rewriting, which sequences reads
   through [@AccessState] decls and treats unknown calls as
   per-thread side effects.

   Routing launch args through the kernel-code rewriter (i.e.
   [D_lang.rewrite_exp]) introduces [@AccessState] decls whose
   [CallExpr] initialisers get dropped by [D_to_imp.infer_call] when
   the callee isn't a known kernel; the inliner then substitutes the
   formal with that bare decl, which surfaces as a free per-thread
   variable in the SMT model. The result is false-positive races on
   launches whose args aren't already bare [Ident]s.

   This module abstracts each launch-site expression to one of:
     - a host-side [Ident] reused as-is ([Direct]),
     - a fresh pseudo-parameter (uniform across threads by
       construction) ([Uniform]),
     - an array identity carrying an optional [Ident]-shaped offset
       ([ArrayId]).

   Non-recoverable structure (function calls, struct-field accesses,
   array subscripts, etc.) folds into [Uniform]: sound (no false
   positives), at the cost of losing the alias to the originating
   host-side variable name.

   Identical non-[Ident] expressions reaching the resolver multiple
   times within one launch (e.g. [seq_len] surfacing as both
   [gridDim.y] and a scalar arg, after c-to-json folds them both to
   [atoi(argv[2])]) collapse onto a single fresh uniform via the
   per-launch [cache]. The first slot to see a given expression
   names it; later slots reuse the same name. This restores the
   structural equality the program had before c-to-json's folding
   and gives Z3 a single uniform to constrain instead of two
   unrelated symbols. *)

type resolved =
  | Direct of Decl_expr.t
  | Const of D_lang.Expr.t
    (* A literal launch-site argument ([IntegerLiteral],
       [FloatingLiteral], [CharacterLiteral], [CXXBoolLiteralExpr]):
       trivially uniform across threads by construction, so the
       resolver passes it through to the kernel call as-is. The
       inliner then substitutes the kernel formal with the literal
       throughout the body, which collapses constant-folded
       launch-site values (e.g. c-to-json folding a [const int N =
       256] reference to [256]) into concrete index expressions. *)
  | Uniform of {
      name : Variable.t;
      ty : J_type.t;
    }
  | ArrayId of {
      base : Decl_expr.t;
      offset : Decl_expr.t option;
      ty : J_type.t;
    }

(* A pseudo-parameter the resolver minted for a non-[Ident] launch
   arg or for an extracted offset. The caller threads these into the
   synthesised pseudo-kernel's [params] list so the analysis knows to
   treat them as uniform globals. *)
type fresh_param = { name : Variable.t; ty : J_type.t }

(* Per-launch dedup map: keys are canonical-stringified launch
   expressions, values are the fresh uniform that already represents
   them. The key uses [C_lang.Expr.to_string] with default options,
   which prints bare [Variable.name] (no source locations) — so two
   structurally-identical expressions at different launch slots key
   the same way. *)
module ExprMap = Map.Make (String)

type cache = Variable.t ExprMap.t

let cache_empty : cache = ExprMap.empty

(* If [e] is a C-AST literal whose value is trivially uniform across
   threads, lift it to its [D_lang.Expr.t] counterpart so it can be
   passed straight through into the synthesised kernel call. Returns
   [None] for everything else (function calls, identifiers, struct
   accesses, etc.) — those go through the resolver's general
   path. *)
let lift_literal (e : C_lang.Expr.t) : D_lang.Expr.t option =
  match e with
  | IntegerLiteral n -> Some (IntegerLiteral n)
  | FloatingLiteral f -> Some (FloatingLiteral f)
  | CharacterLiteral c -> Some (CharacterLiteral c)
  | CXXBoolLiteralExpr b -> Some (CXXBoolLiteralExpr b)
  | _ -> None

let mk_arg_name (idx : int) : Variable.t =
  Variable.from_name (Printf.sprintf "__faial_launch_arg_%d" idx)

let mk_axis_name (base : string) (axis : string) : Variable.t =
  Variable.from_name (Printf.sprintf "__faial_launch_%s_%s" base axis)

let is_pointer_like (ty : J_type.t) : bool =
  J_type.matches (fun ct -> C_type.is_pointer ct || C_type.is_array ct) ty

(* Look up [key] in [cache]. On hit, return the existing name with no
   fresh param. On miss, allocate [name] for [key] and emit a single
   fresh-param entry — the caller adds it to the pseudo-kernel's
   parameter list. *)
let intern (cache : cache) ~(key : string) ~(name : Variable.t)
    ~(ty : J_type.t) : cache * Variable.t * fresh_param list =
  match ExprMap.find_opt key cache with
  | Some existing -> (cache, existing, [])
  | None -> (ExprMap.add key name cache, name, [ { name; ty } ])

(* If [e] is [a + offset] (or [offset + a]) where [a] is a bare
   [Ident], return [(a, Some offset)]. If [e] is itself a bare
   [Ident], return [(e, None)]. Otherwise [None] — the caller treats
   the whole expression as opaque. *)
let rec strip_pointer_offset (e : C_lang.Expr.t) :
    (Decl_expr.t * C_lang.Expr.t option) option =
  match e with
  | Ident d -> Some (d, None)
  | BinaryOperator { opcode = "+"; lhs; rhs; _ } -> (
      match (lhs, rhs) with
      | Ident d, off -> Some (d, Some off)
      | off, Ident d -> Some (d, Some off)
      | _ -> (
          (* Recurse left only — pointer arithmetic associates left
             and the array identity is on the LHS in practice. *)
          match strip_pointer_offset lhs with
          | Some (d, None) -> Some (d, Some rhs)
          | _ -> None))
  | _ -> None

(* If the offset is already an [Ident], reuse it via [free_vars_of_launch]'s
   existing capture path; otherwise mint or reuse a fresh uniform via
   [cache]. *)
let resolve_offset (cache : cache) (proposed_name : Variable.t)
    (off : C_lang.Expr.t) : cache * Decl_expr.t * fresh_param list =
  match off with
  | Ident d -> (cache, d, [])
  | _ ->
      let ty = C_lang.Expr.to_type off in
      let key = C_lang.Expr.to_string off in
      let cache, name, fresh = intern cache ~key ~name:proposed_name ~ty in
      let d = Decl_expr.from_name ~ty ~kind:Decl_expr.Kind.Var name in
      (cache, d, fresh)

(* Resolve a launch-site argument at position [idx] within the
   call. *)
let resolve (cache : cache) (idx : int) (e : C_lang.Expr.t) :
    cache * resolved * fresh_param list =
  match e with
  | Ident d -> (cache, Direct d, [])
  | _ when Option.is_some (lift_literal e) ->
      (cache, Const (Option.get (lift_literal e)), [])
  | _ ->
      let ty = C_lang.Expr.to_type e in
      if is_pointer_like ty then
        match strip_pointer_offset e with
        | Some (base, None) -> (cache, Direct base, [])
        | Some (base, Some off) ->
            let off_name =
              Variable.from_name
                (Printf.sprintf "__faial_launch_arg_%d_off" idx)
            in
            let cache, off_decl, fresh =
              resolve_offset cache off_name off
            in
            (cache, ArrayId { base; offset = Some off_decl; ty }, fresh)
        | None ->
            let key = C_lang.Expr.to_string e in
            let cache, name, fresh =
              intern cache ~key ~name:(mk_arg_name idx) ~ty
            in
            (cache, Uniform { name; ty }, fresh)
      else
        let key = C_lang.Expr.to_string e in
        let cache, name, fresh =
          intern cache ~key ~name:(mk_arg_name idx) ~ty
        in
        (cache, Uniform { name; ty }, fresh)

(* Resolve a single dim-axis (one of x/y/z of [gridDim]/[blockDim]).
   Same shape as [resolve], but uses a stable axis-based name when
   minting a fresh uniform so the pseudo-kernel's parameter list is
   deterministic across runs. The cache still applies — if the same
   axis expression already appeared elsewhere in this launch, it
   reuses the existing name regardless of axis. *)
let resolve_axis (cache : cache) (base : string) (axis : string)
    (e : C_lang.Expr.t) : cache * resolved * fresh_param list =
  match e with
  | Ident d -> (cache, Direct d, [])
  | _ when Option.is_some (lift_literal e) ->
      (cache, Const (Option.get (lift_literal e)), [])
  | _ ->
      let ty = C_lang.Expr.to_type e in
      let key = C_lang.Expr.to_string e in
      let cache, name, fresh =
        intern cache ~key ~name:(mk_axis_name base axis) ~ty
      in
      (cache, Uniform { name; ty }, fresh)

(* Convert a [resolved] handle back into a [D_lang.Expr.t] for use as
   a CallExpr argument or as the RHS of a dim-axis [assert]. *)
let to_d_expr (r : resolved) : D_lang.Expr.t =
  match r with
  | Direct d -> Ident d
  | Const e -> e
  | Uniform { name; ty } ->
      Ident (Decl_expr.from_name ~ty ~kind:Decl_expr.Kind.Var name)
  | ArrayId { base; offset = None; _ } -> Ident base
  | ArrayId { base; offset = Some off; ty } ->
      BinaryOperator
        { opcode = "+"; lhs = Ident base; rhs = Ident off; ty }

(* Convert a fresh param into the [C_lang.Param.t] shape that
   [synth_kernel] folds into the pseudo-kernel's [params] list.
   Mirrors [param_of_free_var] but takes a (name, ty) pair instead of
   a [Decl_expr.t]. *)
let fresh_to_param (p : fresh_param) : C_lang.Param.t =
  let ty_var = Ty_variable.make ~ty:p.ty ~name:p.name in
  C_lang.Param.make ~ty_var ~is_used:true ~is_shared:false
