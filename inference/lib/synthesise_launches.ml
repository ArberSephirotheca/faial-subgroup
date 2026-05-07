open Stage0
open Protocols
open D_lang
open State.Syntax

(* For every [Def.LaunchParam lp] in the program, synthesise a fresh
   [__global__] [Def.Kernel] whose body:

     assert(gridDim.x == <lp.grid axis x>);  ... y ... z ...
     assert(blockDim.x == <lp.block axis x>); ... y ... z ...
     <lp.kernel.name>(<lp.args>...);

   The free host-side variables referenced inside the launch slots
   become parameters of the synthesised kernel; faial then treats
   them as universally-quantified ints. Asserts (Global visibility)
   lift to SMT hypotheses guarding all subsequent accesses, which
   pins gridDim/blockDim across both the auto-generated invariant
   and the inlined kernel body in the proof obligation.

   Kernels that are the target of any [LaunchParam] get demoted from
   [Default] (__global__) to [Auxiliary] (__device__) so the existing
   call-inliner picks them up. Kernels that are never launched in this
   TU are left as [Default] — analysis falls back to today's behaviour.

   This pass is opt-in via [--assume-launch] on faial-drf; the original
   [Def.LaunchParam] entries are consumed and removed from the program
   regardless. *)

(* ---------- Free-variable extraction ---------- *)

(* Union of free vars referenced anywhere in a [LaunchParam]'s slots —
   grid / block / shared_mem / stream / args / path_condition / each
   const binding's init — restricted to [Var] / [ParmVar] kinds. The
   binding's [name] (LHS) is already covered by the other slot walks
   (c-to-json's BFS only admits a binding when its name is reachable
   from a slot Expr); walking inits surfaces vars referenced *inside*
   an init (e.g. [numk] in [inum = numk * 1024]) so they become
   synth-kernel parameters. *)
let free_vars_of_launch (lp : C_lang.LaunchParam.t) : Decl_expr.Set.t =
  let binding_inits =
    List.map (fun (b : C_lang.ConstBinding.t) -> b.init) lp.const_bindings
  in
  let exprs =
    [ lp.grid; lp.block; lp.shared_mem; lp.stream ]
    @ lp.args
    @ Option.to_list lp.path_condition
    @ binding_inits
  in
  exprs
  |> List.fold_left
       (fun acc e ->
         Decl_expr.Set.union acc (C_lang.Expr.shallow_free_vars e))
       Decl_expr.Set.empty
  |> Decl_expr.Set.filter Decl_expr.is_runtime_value

(* ---------- Pseudo-kernel synthesis ---------- *)

(* The three axes of a synthesised [dim3(...)] (the [grid] / [block]
   slot of a [LaunchParam]). c-to-json always wraps the resolved per-
   axis expressions in a [CXXConstructExpr]; pad missing axes with
   [IntegerLiteral 1]. If the slot is anything else (defensive), treat
   the whole expression as the x-axis. *)
let dim3_axes (e : C_lang.Expr.t) : C_lang.Expr.t * C_lang.Expr.t * C_lang.Expr.t =
  let one : C_lang.Expr.t = IntegerLiteral 1 in
  match e with
  | CXXConstructExpr { args; _ } -> (
      match args with
      | [ x; y; z ] -> (x, y, z)
      | [ x; y ] -> (x, y, one)
      | [ x ] -> (x, one, one)
      | _ -> (e, one, one))
  | _ -> (e, one, one)

(* Build [assert(<base>.<axis> == <rhs>);] as a D_lang.Stmt.t. The LHS
   of the equality uses the flat name [Ident "<base>.<axis>"] (matching
   how [MemberExpr { base; name }] is rendered downstream as [Var
   "base.name"]), so the assert references the same SMT variable that
   the auto-generated invariant uses. [d_to_imp] recognises calls to
   [assert]/[static_assert]/[__requires] and lifts them to
   [Imp.Stmt.Assert] with [Global] visibility, which becomes an SMT
   hypothesis on every subsequent access. *)
let assert_axis_eq (base : string) (axis : string) (rhs : Expr.t) : Stmt.t =
  let var = Variable.from_name (base ^ "." ^ axis) in
  let lhs : Expr.t =
    Ident (Decl_expr.from_name ~ty:J_type.int ~kind:Decl_expr.Kind.Var var)
  in
  let cond : Expr.t =
    BinaryOperator { opcode = "=="; lhs; rhs; ty = J_type.bool }
  in
  Stmt.assert_stmt cond

(* Build the dim-axis assertions for one of [gridDim] or [blockDim].
   Each axis expression goes through [Launch_arg.resolve_axis]: an
   [Ident] is reused, an [IntegerLiteral] becomes a literal RHS, and
   anything else folds into a fresh per-axis pseudo-parameter
   ([__faial_launch_<base>_<axis>]) — or reuses a name already minted
   for an identical expression elsewhere in this launch (via
   [cache]). The host-side analysis domain matters here —
   [D_lang.rewrite_exp] would introduce [@AccessState] decls whose
   [CallExpr] inits get dropped by [d_to_imp.infer_call], producing
   free per-thread variables. *)
let dim_asserts (base : string) (e : C_lang.Expr.t) :
    (Launch_arg.Equiv.t, Stmt.t) State.t =
  let xe, ye, ze = dim3_axes e in
  let axis_rhs (axis : string) (e : C_lang.Expr.t) :
      (Launch_arg.Equiv.t, Expr.t) State.t =
    let* resolved = Launch_arg.resolve_axis base axis e in
    return (Launch_arg.to_d_expr resolved)
  in
  let* rhs_x = axis_rhs "x" xe in
  let* rhs_y = axis_rhs "y" ye in
  let* rhs_z = axis_rhs "z" ze in
  return
    (Stmt.from_list
       [
         assert_axis_eq base "x" rhs_x;
         assert_axis_eq base "y" rhs_y;
         assert_axis_eq base "z" rhs_z;
       ])

(* Build [kernel(args...);] as a D_lang.Stmt.t. The function reference
   carries the kernel's full type string so the SignatureDB lookup hits
   the right specialisation when multiple specialisations share a name.

   Each launch arg goes through [Launch_arg.resolve]: bare [Ident]s
   are reused as-is; pointer expressions of shape [a + offset]
   surface as [a + ident_offset]; everything else collapses into a
   fresh uniform parameter — or, when the same expression already
   appeared in [cache] (e.g. as a dim-axis), the existing uniform is
   reused so the analyser sees one symbol instead of two. *)
let call_stmt (kernel : Decl_expr.t) (args : C_lang.Expr.t list) :
    (Launch_arg.Equiv.t, Stmt.t) State.t =
  let* rs =
    args
    |> List.mapi (fun i a -> (i, a))
    |> State.list_map (fun (i, a) -> Launch_arg.resolve i a)
  in
  let args = List.map Launch_arg.to_d_expr rs in
  let func : Expr.t =
    Ident
      (Decl_expr.from_name ~ty:kernel.ty ~kind:Decl_expr.Kind.Function
         kernel.name)
  in
  return (Stmt.SExpr (CallExpr { func; args; ty = kernel.ty }))

(* Stable name for the synthesised kernel. The launch's source location
   is unique per call site within a translation unit; combine with the
   target kernel name to keep the identifier readable. *)
let synth_name (lp : C_lang.LaunchParam.t) : string =
  let kernel_name = Variable.name lp.kernel.name in
  let file = Filename.basename (Location.filename lp.loc) in
  let file =
    try Filename.chop_extension file with Invalid_argument _ -> file
  in
  let file =
    String.map (fun c -> if c = '.' || c = '-' then '_' else c) file
  in
  let line = Index.to_base1 (Location.line lp.loc) in
  Printf.sprintf "%s__launch_%s_%d" kernel_name file line

(* Convert a free-variable [Decl_expr.t] into a kernel parameter. Drop
   structs defensively — c-to-json's resolution policy const-folds
   [dim3] constructors inline, so launch slots see only scalar host
   vars in practice; on the rare occasion a struct survives we skip
   it. The analysis still proceeds (the slot stays opaque). *)
let param_of_free_var (d : Decl_expr.t) : C_lang.Param.t option =
  if J_type.matches C_type.is_struct d.ty then None
  else
    let ty_var = Ty_variable.make ~ty:d.ty ~name:d.name in
    Some (C_lang.Param.make ~ty_var ~is_used:true ~is_shared:false)

(* Lift c-to-json's [path_condition] (a sound conjunction of
   enclosing [if]/[while]/[for] guards that hold when the launch
   executes) into an [assert(...)] in the pseudo-kernel body. The
   c-to-json drop rule excludes anything potentially mutated between
   the guard's branch entry and the launch (calls, members, escaped
   locals, side effects), so what survives is always pure
   arithmetic / boolean over [Ident]s and literals — exactly the
   shapes [Launch_arg.lift_pure] handles. If lift_pure declines
   (defensive — shouldn't fire given the contract), the assert is
   skipped silently. *)
let path_cond_asserts (lp : C_lang.LaunchParam.t) : Stmt.t =
  let ( let* ) = Option.bind in
  (let* e = lp.path_condition in
   let* d_expr = Launch_arg.lift_pure e in
   Some (Stmt.assert_stmt d_expr))
  |> Option.value ~default:Stmt.Skip

(* Lift c-to-json's [const_bindings] (host-local [const]-qualified
   variables paired with their initialisers) into local [DeclStmt]s
   at the top of the synth kernel body. Each binding becomes
   [<ty> <name> = <init>;] — [d_to_imp.infer_decl] lowers the
   declaration to [Imp.Stmt.decl_set], which gives Imp the
   definitional binding [name = init]. The names are filtered out of
   the synth kernel's parameter list (see [synth_kernel]) so they're
   bound exactly once, as locals.

   c-to-json's BFS only admits bindings whose name is reachable from
   another slot Expr, and the init expression has already been
   resolved (const-fold + trivial-init substitution + pure-helper
   inlining) by the emitter, so it normally lifts cleanly via
   [lift_pure]. If [lift_pure] declines (defensive — shouldn't fire
   given the contract), the binding is skipped and its name stays as
   a parameter. *)
let const_binding_decl (b : C_lang.ConstBinding.t) : Stmt.t option =
  let ( let* ) = Option.bind in
  let* rhs = Launch_arg.lift_pure b.init in
  let ty_var = Ty_variable.make ~ty:b.ty ~name:b.name in
  let d = D_lang.Decl.from_expr ty_var rhs in
  Some (Stmt.DeclStmt [ d ])

let const_binding_decls (lp : C_lang.LaunchParam.t) : Stmt.t =
  lp.const_bindings |> List.filter_map const_binding_decl |> Stmt.from_list

(* Names of const-bindings whose decl emission succeeded. Dropping
   them from [direct_params] avoids declaring the same name as both
   a synth-kernel parameter and a local. *)
let bound_names_emitted (lp : C_lang.LaunchParam.t) : Variable.Set.t =
  lp.const_bindings
  |> List.filter_map (fun (b : C_lang.ConstBinding.t) ->
         Option.map (fun _ -> b.name) (Launch_arg.lift_pure b.init))
  |> Variable.Set.of_list

(* De-dup [xs] by the [Variable.t] returned by [name_of], keeping
   first-seen order. *)
let dedup_by_name (type a) ~(name_of : a -> Variable.t) (xs : a list) : a list =
  let step (seen, acc) x =
    let n = name_of x in
    if Variable.Set.mem n seen then (seen, acc)
    else (Variable.Set.add n seen, x :: acc)
  in
  List.fold_left step (Variable.Set.empty, []) xs |> snd |> List.rev

let synth_kernel (lp : C_lang.LaunchParam.t) : Kernel.t =
  (* One resolver state per pseudo-kernel — gridDim, then blockDim,
     then args. First slot to see a non-Ident expression names it;
     later slots reuse the same uniform. This catches the pattern
     where a host-side variable gets c-to-json-folded to the same
     expression at multiple launch slots (e.g. [seq_len] both as
     [gridDim.y] and as a scalar arg, both folded to
     [atoi(argv[2])]). *)
  let m =
    let* body_grid = dim_asserts "gridDim" lp.grid in
    let* body_block = dim_asserts "blockDim" lp.block in
    let* body_call = call_stmt lp.kernel lp.args in
    return (body_grid, body_block, body_call)
  in
  let final, (body_grid, body_block, body_call) =
    State.run m Launch_arg.Equiv.empty
  in
  let body_path_cond = path_cond_asserts lp in
  let body_const_bindings = const_binding_decls lp in
  (* shared_mem: skipped intentionally. Static [__shared__] arrays
     declare their own sizes inline; only [extern __shared__] consumes
     the launch's dynamic shared-mem arg, and faial doesn't yet model
     that binding. *)
  (* stream: not relevant to data-race analysis. *)
  (* Const-binding decls come first so the locals they introduce are
     in scope for the dim asserts, the path-condition assert, and the
     kernel call — any of those slots may reference a binding's
     [name] as a [DeclRefExpr]. *)
  let body =
    Stmt.from_list
      [ body_const_bindings; body_grid; body_block; body_path_cond; body_call ]
  in
  (* Free-var capture still drives the [Direct] path: any [Ident]
     surfaced by [Launch_arg.resolve] surfaces here as a parameter,
     same as before. The fresh-param list adds the [Uniform]/[ArrayId]
     uniforms minted for non-[Ident] launch args and dim-axis
     expressions. Names that became local const-binding decls are
     dropped — they're bound by [body_const_bindings] now, not by
     parameter passing. Dedup by variable name in case a fresh name
     collides with a captured free var (shouldn't happen in practice
     given the [__faial_launch_*] prefix, but be defensive). *)
  let bound = bound_names_emitted lp in
  let direct_params =
    free_vars_of_launch lp
    |> Decl_expr.Set.filter (fun (d : Decl_expr.t) ->
           not (Variable.Set.mem d.name bound))
    |> Decl_expr.Set.elements
    |> List.filter_map param_of_free_var
  in
  let fresh_params = Launch_arg.Equiv.fresh_params final in
  let params =
    direct_params @ fresh_params
    |> dedup_by_name ~name_of:C_lang.Param.name
  in
  let name = synth_name lp in
  let ty = J_type.to_string lp.kernel.ty in
  {
    Kernel.ty;
    name;
    code = body;
    type_params = [];
    params;
    attribute = C_lang.KernelAttr.Default;
  }

(* ---------- Demote launched kernels ---------- *)

(* Collect the names of every kernel that is the target of at least one
   [LaunchParam] in the program. *)
let launched_kernel_names (p : Program.t) : Variable.Set.t =
  List.fold_left
    (fun acc def ->
      match def with
      | Def.LaunchParam lp -> Variable.Set.add lp.kernel.name acc
      | _ -> acc)
    Variable.Set.empty p

let demote_if_launched (launched : Variable.Set.t) (k : Kernel.t) : Kernel.t =
  let n = Variable.from_name k.name in
  if Variable.Set.mem n launched && k.attribute = C_lang.KernelAttr.Default
  then { k with attribute = C_lang.KernelAttr.Auxiliary }
  else k

(* ---------- Top-level transform ---------- *)

(* Replace every [Def.LaunchParam] in [p] with a synthesised [Def.Kernel],
   and demote any kernel that's the target of a launch from [Default] to
   [Auxiliary]. The synthesised kernels are ordered before the demoted
   originals so the call-inliner sees callees before callers. *)
let rewrite_program (p : Program.t) : Program.t =
  let launched = launched_kernel_names p in
  let push_synth def = State.update (fun synth -> def :: synth) in
  let m =
    State.list_fold_left
      (fun rest def ->
        match def with
        | Def.LaunchParam lp ->
            let* () = push_synth (Def.Kernel (synth_kernel lp)) in
            return rest
        | Def.Kernel k ->
            return (Def.Kernel (demote_if_launched launched k) :: rest)
        | other -> return (other :: rest))
      [] p
  in
  let synth, rest_rev = State.run m [] in
  List.rev rest_rev @ List.rev synth
