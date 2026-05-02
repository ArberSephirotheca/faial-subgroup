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

let dim_asserts (base : string) (e : C_lang.Expr.t) : Stmt.t * Stmt.t =
  let x, y, z = dim3_axes e in
  let prefix_x, x = run0 (rewrite_exp x) in
  let prefix_y, y = run0 (rewrite_exp y) in
  let prefix_z, z = run0 (rewrite_exp z) in
  let body =
    Stmt.from_list
      [
        assert_axis_eq base "x" x;
        assert_axis_eq base "y" y;
        assert_axis_eq base "z" z;
      ]
  in
  (Stmt.from_list [ prefix_x; prefix_y; prefix_z ], body)

(* Build [kernel(args...);] as a D_lang.Stmt.t. The function reference
   carries the kernel's full type string so the SignatureDB lookup hits
   the right specialisation when multiple specialisations share a name. *)
let call_stmt (kernel : Decl_expr.t) (args : C_lang.Expr.t list) : Stmt.t * Stmt.t =
  let preludes_and_args : Stmt.t list * Expr.t list =
    List.fold_left
      (fun (preludes, exprs) a ->
        let prelude, e = run0 (rewrite_exp a) in
        (prelude :: preludes, e :: exprs))
      ([], []) args
  in
  let preludes, args =
    let p, a = preludes_and_args in
    (List.rev p, List.rev a)
  in
  let func : Expr.t =
    Ident
      (Decl_expr.from_name ~ty:kernel.ty ~kind:Decl_expr.Kind.Function
         kernel.name)
  in
  let call : Expr.t =
    CallExpr { func; args; ty = kernel.ty }
  in
  (Stmt.from_list preludes, SExpr call)

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

let synth_kernel (lp : C_lang.LaunchParam.t) : Kernel.t =
  let prelude_grid, body_grid = dim_asserts "gridDim" lp.grid in
  let prelude_block, body_block = dim_asserts "blockDim" lp.block in
  let prelude_call, body_call = call_stmt lp.kernel lp.args in
  (* shared_mem: skipped intentionally. Static [__shared__] arrays
     declare their own sizes inline; only [extern __shared__] consumes
     the launch's dynamic shared-mem arg, and faial doesn't yet model
     that binding. *)
  (* stream: not relevant to data-race analysis. *)
  let body =
    Stmt.from_list
      [
        prelude_grid;
        body_grid;
        prelude_block;
        body_block;
        prelude_call;
        body_call;
      ]
  in
  let params =
    free_vars_of_launch lp |> List.filter_map param_of_free_var
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
