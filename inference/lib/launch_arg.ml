open Stage0
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
   per-launch dedup cache. The first slot to see a given expression
   names it; later slots reuse the same name. This restores the
   structural equality the program had before c-to-json's folding
   and gives Z3 a single uniform to constrain instead of two
   unrelated symbols. *)

module Context = struct
  (* Per-launch dedup map: keys are canonical-stringified launch
     expressions (via [C_lang.Expr.to_string] with default options,
     which prints bare [Variable.name] (no source locations) — so two
     structurally-identical expressions at different launch slots key
     the same way), values are the fresh uniform already representing
     them.

     Resolver state: the dedup cache plus the running list of fresh
     params minted for this launch, accumulated newest-first. The
     resolver functions are state monads over [t]; [synth_kernel] in
     [synthesise_launches.ml] runs them once per pseudo-kernel and
     pulls the final fresh-param list via [fresh_params]. *)
  type t = {
    cache : Variable.t Common.StringMap.t;
    fresh : Ty_variable.t list;
  }

  let empty : t = { cache = Common.StringMap.empty; fresh = [] }

  (* Fresh wrapper-kernel params minted during the resolver run, in
     encounter order, ready to splice into [synth_kernel]'s [params]
     list. *)
  let fresh_params (st : t) : C_lang.Param.t list =
    st.fresh
    |> List.rev_map (fun ty_var ->
           C_lang.Param.make ~ty_var ~is_used:true ~is_shared:false)

  (* Look up [e] in the cache. On hit, return the existing name. On
     miss, mint a new uniform under [name]: extend the cache and push
     a fresh entry that the caller pulls from [fresh_params] after
     running the resolver. The cache key is the canonical
     stringification of [e] (location-stripped, so two structurally
     identical exprs at different launch slots collide); the
     accompanying [Ty_variable.t] picks up its type from [e] directly. *)
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

end

type t =
  | Direct of Decl_expr.t
  | Const of D_lang.Expr.t
    (* A side-effect-free launch-site expression — a literal, an
       [Ident], or arithmetic / conditional / unary operations
       composing the two. Trivially uniform across threads by
       construction, so the resolver passes it through to the
       kernel call verbatim. Two consequences:

       1. The inliner substitutes the kernel formal with the
          expression throughout the body, so constant-folded
          launch-site values (e.g. c-to-json folding [const int N =
          256] to [256]) collapse into concrete index expressions.

       2. When the expression appears in [gridDim] / [blockDim]
          assertions, its structure reaches Z3 (e.g.
          [gridDim.x == imageW / 128]). Combined with the existing
          [gridDim.x >= 1] preamble, Z3 derives lower bounds on the
          contained kernel args transitively (here, [imageW >=
          128]) without needing a dedicated grid-arithmetic
          inversion pass. *)
  | Uniform of {
      name : Variable.t;
      ty : J_type.t;
    }
  | ArrayId of {
      base : Decl_expr.t;
      offset : Decl_expr.t option;
      ty : J_type.t;
    }

(* If [e] is a pure expression over [Ident]s and integer / float /
   bool / character literals — i.e. has no host-side side effects,
   no function calls, no struct or array reads — lift it to its
   [D_lang.Expr.t] counterpart so the launch's structure flows
   through to the synthesised kernel verbatim. The captured
   [Ident]s still surface via [free_vars_of_launch] as block-uniform
   pseudo-parameters; preserving the surrounding arithmetic lets
   Z3 reason transitively over the launch's relations (e.g.
   deriving [imageW >= 128] from [gridDim.x == imageW / 128] and
   the existing [gridDim.x >= 1] preamble).

   Excludes [CallExpr], [ArraySubscriptExpr], [MemberExpr], etc. —
   those carry host-side memory effects whose results c-to-json may
   have folded but whose relations the analyser shouldn't try to
   reason about; those still go through the [Uniform] / [ArrayId]
   abstraction path. *)
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

(* If the offset is already an [Ident], reuse it; otherwise mint or
   reuse a fresh uniform via the cache. *)
let resolve_offset (proposed_name : Variable.t) (off : C_lang.Expr.t) :
    (Context.t, Decl_expr.t) State.t =
  let open State.Syntax in
  match off with
  | Ident d -> return d
  | _ ->
      let ty = C_lang.Expr.to_type off in
      let* name = Context.intern off ~name:proposed_name in
      return (Decl_expr.from_name ~ty ~kind:Decl_expr.Kind.Var name)

(* Resolve a launch-site argument at position [idx] within the
   call. *)
let resolve (idx : int) (e : C_lang.Expr.t) : (Context.t, t) State.t =
  let open State.Syntax in
  match e with
  | Ident d -> return (Direct d)
  | _ -> (
      match lift_pure e with
      | Some pure -> return (Const pure)
      | None ->
          let ty = C_lang.Expr.to_type e in
          let mint_uniform proposed_name =
            let* name = Context.intern e ~name:proposed_name in
            return (Uniform { name; ty })
          in
          if J_type.matches C_type.is_array ty then
            match strip_pointer_offset e with
            | Some (base, None) -> return (Direct base)
            | Some (base, Some off) ->
                let off_name =
                  Variable.from_name
                    (Printf.sprintf "__faial_launch_arg_%d_off" idx)
                in
                let* off_decl = resolve_offset off_name off in
                return (ArrayId { base; offset = Some off_decl; ty })
            | None -> mint_uniform (mk_arg_name idx)
          else mint_uniform (mk_arg_name idx))

(* Resolve a single dim-axis (one of x/y/z of [gridDim]/[blockDim]).
   Same shape as [resolve], but uses a stable axis-based name when
   minting a fresh uniform so the pseudo-kernel's parameter list is
   deterministic across runs. The cache still applies — if the same
   axis expression already appeared elsewhere in this launch, it
   reuses the existing name regardless of axis. *)
let resolve_axis (base : string) (axis : string) (e : C_lang.Expr.t) :
    (Context.t, t) State.t =
  let open State.Syntax in
  match e with
  | Ident d -> return (Direct d)
  | _ -> (
      match lift_pure e with
      | Some pure -> return (Const pure)
      | None ->
          let ty = C_lang.Expr.to_type e in
          let* name = Context.intern e ~name:(mk_axis_name base axis) in
          return (Uniform { name; ty }))

(* Convert a resolved launch arg back into a [D_lang.Expr.t] for use
   as a CallExpr argument or as the RHS of a dim-axis [assert]. *)
let to_d_expr (r : t) : D_lang.Expr.t =
  match r with
  | Direct d -> Ident d
  | Const e -> e
  | Uniform { name; ty } ->
      Ident (Decl_expr.from_name ~ty ~kind:Decl_expr.Kind.Var name)
  | ArrayId { base; offset = None; _ } -> Ident base
  | ArrayId { base; offset = Some off; ty } ->
      BinaryOperator
        { opcode = "+"; lhs = Ident base; rhs = Ident off; ty }
