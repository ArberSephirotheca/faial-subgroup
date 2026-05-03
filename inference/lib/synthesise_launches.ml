open Stage0
open Protocols
open D_lang

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

(* Walk a [C_lang.Expr.t] collecting [Decl_expr.t] for every identifier
   reference whose [kind] is [Var] or [ParmVar]. De-dup by name in
   first-seen order. Function/method/enum/template-parm references are
   skipped — they don't carry runtime values. *)
let free_vars_of_c_expr (e : C_lang.Expr.t) : Decl_expr.t list =
  let seen : Variable.Set.t ref = ref Variable.Set.empty in
  let acc : Decl_expr.t list ref = ref [] in
  let push (d : Decl_expr.t) : unit =
    let n = d.name in
    if not (Variable.Set.mem n !seen) then (
      seen := Variable.Set.add n !seen;
      acc := d :: !acc)
  in
  let rec walk (e : C_lang.Expr.t) : unit =
    match e with
    | Ident d -> (
        match d.kind with
        | Decl_expr.Kind.Var | Decl_expr.Kind.ParmVar -> push d
        | Decl_expr.Kind.Function
        | Decl_expr.Kind.CXXMethod
        | Decl_expr.Kind.NonTypeTemplateParm
        | Decl_expr.Kind.EnumConstant ->
            ())
    | BinaryOperator b ->
        walk b.lhs;
        walk b.rhs
    | UnaryOperator u -> walk u.child
    | CallExpr { func; args; _ } ->
        walk func;
        List.iter walk args
    | CXXOperatorCallExpr { func; args; _ } ->
        walk func;
        List.iter walk args
    | CXXConstructExpr { args; _ } -> List.iter walk args
    | CXXNewExpr { arg; _ } -> walk arg
    | CXXDeleteExpr { arg; _ } -> walk arg
    | ConditionalOperator c ->
        walk c.cond;
        walk c.then_expr;
        walk c.else_expr
    | MemberExpr { base; _ } -> walk base
    | ArraySubscriptExpr a ->
        walk a.lhs;
        walk a.rhs
    | StmtExpr e -> walk e.result
    | PackExpansion e -> walk e
    | LambdaExpr _
    (* Lambdas in launch slots aren't expected — be defensive: skip. *)
    | DependentScopeRef _
    (* Dependent references in resolved-launch slots shouldn't survive
       past Phase-2; defensive skip if they do. *)
    | UnresolvedLookupExpr _
    | RecoveryExpr _ | SizeOfExpr _ | CharacterLiteral _ | IntegerLiteral _
    | FloatingLiteral _ | CXXBoolLiteralExpr _ ->
        ()
  in
  walk e;
  List.rev !acc

(* Union of free vars referenced anywhere in a [LaunchParam]'s slots. *)
let free_vars_of_launch (lp : C_lang.LaunchParam.t) : Decl_expr.t list =
  let seen : Variable.Set.t ref = ref Variable.Set.empty in
  let acc : Decl_expr.t list ref = ref [] in
  let consider (d : Decl_expr.t) : unit =
    let n = d.name in
    if not (Variable.Set.mem n !seen) then (
      seen := Variable.Set.add n !seen;
      acc := d :: !acc)
  in
  let from_one (e : C_lang.Expr.t) : unit =
    List.iter consider (free_vars_of_c_expr e)
  in
  from_one lp.grid;
  from_one lp.block;
  from_one lp.shared_mem;
  from_one lp.stream;
  List.iter from_one lp.args;
  Option.iter from_one lp.path_condition;
  List.rev !acc

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

let int_ty : J_type.t = J_type.int

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
    Ident (Decl_expr.from_name ~ty:int_ty ~kind:Decl_expr.Kind.Var var)
  in
  let cond : Expr.t =
    BinaryOperator { opcode = "=="; lhs; rhs; ty = J_type.bool }
  in
  let assert_func : Expr.t =
    Ident
      (Decl_expr.from_name ~ty:int_ty ~kind:Decl_expr.Kind.Function
         (Variable.from_name "assert"))
  in
  SExpr (CallExpr { func = assert_func; args = [ cond ]; ty = int_ty })

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
let dim_asserts (cache : Launch_arg.cache) (base : string)
    (e : C_lang.Expr.t) :
    Launch_arg.cache * Stmt.t * Launch_arg.fresh_param list =
  let xe, ye, ze = dim3_axes e in
  let axis_rhs (cache : Launch_arg.cache) (axis : string) (e : C_lang.Expr.t)
      : Launch_arg.cache * Expr.t * Launch_arg.fresh_param list =
    let cache, resolved, fresh =
      Launch_arg.resolve_axis cache base axis e
    in
    (cache, Launch_arg.to_d_expr resolved, fresh)
  in
  let cache, rhs_x, fx = axis_rhs cache "x" xe in
  let cache, rhs_y, fy = axis_rhs cache "y" ye in
  let cache, rhs_z, fz = axis_rhs cache "z" ze in
  let body =
    Stmt.from_list
      [
        assert_axis_eq base "x" rhs_x;
        assert_axis_eq base "y" rhs_y;
        assert_axis_eq base "z" rhs_z;
      ]
  in
  (cache, body, fx @ fy @ fz)

(* Build [kernel(args...);] as a D_lang.Stmt.t. The function reference
   carries the kernel's full type string so the SignatureDB lookup hits
   the right specialisation when multiple specialisations share a name.

   Each launch arg goes through [Launch_arg.resolve]: bare [Ident]s
   are reused as-is; pointer expressions of shape [a + offset]
   surface as [a + ident_offset]; everything else collapses into a
   fresh uniform parameter — or, when the same expression already
   appeared in [cache] (e.g. as a dim-axis), the existing uniform is
   reused so the analyser sees one symbol instead of two. *)
let call_stmt (cache : Launch_arg.cache) (kernel : Decl_expr.t)
    (args : C_lang.Expr.t list) :
    Launch_arg.cache * Stmt.t * Launch_arg.fresh_param list =
  let cache, rs_rev, fresh =
    args
    |> List.mapi (fun i a -> (i, a))
    |> List.fold_left
         (fun (cache, rs, fs) (i, a) ->
           let cache, r, f = Launch_arg.resolve cache i a in
           (cache, r :: rs, fs @ f))
         (cache, [], [])
  in
  let args = rs_rev |> List.rev |> List.map Launch_arg.to_d_expr in
  let func : Expr.t =
    Ident
      (Decl_expr.from_name ~ty:kernel.ty ~kind:Decl_expr.Kind.Function
         kernel.name)
  in
  let call : Expr.t = CallExpr { func; args; ty = kernel.ty } in
  (cache, SExpr call, fresh)

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

(* Build [assert(<cond>);] as a D_lang.Stmt.t. [d_to_imp] recognises
   calls to [assert] and lifts them to [Imp.Stmt.Assert] with
   [Global] visibility — the same machinery that pins gridDim /
   blockDim in [dim_asserts] — so the body of the synth kernel can
   carry arbitrary host-side hypotheses as SMT preconditions. *)
let assert_stmt (cond : Expr.t) : Stmt.t =
  let assert_func : Expr.t =
    Ident
      (Decl_expr.from_name ~ty:int_ty ~kind:Decl_expr.Kind.Function
         (Variable.from_name "assert"))
  in
  SExpr (CallExpr { func = assert_func; args = [ cond ]; ty = int_ty })

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
  match lp.path_condition with
  | None -> Stmt.Skip
  | Some e -> (
      match Launch_arg.lift_pure e with
      | Some d_expr -> assert_stmt d_expr
      | None -> Stmt.Skip)

let synth_kernel (lp : C_lang.LaunchParam.t) : Kernel.t =
  (* One [Launch_arg.cache] per pseudo-kernel — gridDim, then
     blockDim, then args. First slot to see a non-Ident expression
     names it; later slots reuse the same uniform. This catches the
     pattern where a host-side variable gets c-to-json-folded to the
     same expression at multiple launch slots (e.g. [seq_len] both as
     [gridDim.y] and as a scalar arg, both folded to
     [atoi(argv[2])]). *)
  let cache = Launch_arg.cache_empty in
  let cache, body_grid, fresh_grid = dim_asserts cache "gridDim" lp.grid in
  let cache, body_block, fresh_block = dim_asserts cache "blockDim" lp.block in
  let _cache, body_call, fresh_args = call_stmt cache lp.kernel lp.args in
  let body_path_cond = path_cond_asserts lp in
  (* shared_mem: skipped intentionally. Static [__shared__] arrays
     declare their own sizes inline; only [extern __shared__] consumes
     the launch's dynamic shared-mem arg, and faial doesn't yet model
     that binding. *)
  (* stream: not relevant to data-race analysis. *)
  let body =
    Stmt.from_list [ body_grid; body_block; body_path_cond; body_call ]
  in
  (* Free-var capture still drives the [Direct] path: any [Ident]
     surfaced by [Launch_arg.resolve] surfaces here as a parameter,
     same as before. The fresh-param list adds the [Uniform]/[ArrayId]
     uniforms minted for non-[Ident] launch args and dim-axis
     expressions. Dedup by variable name in case a fresh name
     collides with a captured free var (shouldn't happen in practice
     given the [__faial_launch_*] prefix, but be defensive). *)
  let direct_params =
    free_vars_of_launch lp |> List.filter_map param_of_free_var
  in
  let fresh_params =
    fresh_grid @ fresh_block @ fresh_args |> List.map Launch_arg.fresh_to_param
  in
  let seen : Variable.Set.t ref = ref Variable.Set.empty in
  let dedup (acc : C_lang.Param.t list) (p : C_lang.Param.t) :
      C_lang.Param.t list =
    let n = C_lang.Param.name p in
    if Variable.Set.mem n !seen then acc
    else (
      seen := Variable.Set.add n !seen;
      p :: acc)
  in
  let params =
    List.fold_left dedup [] (direct_params @ fresh_params) |> List.rev
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
  let synthesised : Def.t list ref = ref [] in
  let rest =
    List.filter_map
      (fun def ->
        match def with
        | Def.LaunchParam lp ->
            synthesised := Def.Kernel (synth_kernel lp) :: !synthesised;
            None
        | Def.Kernel k -> Some (Def.Kernel (demote_if_launched launched k))
        | other -> Some other)
      p
  in
  rest @ List.rev !synthesised
