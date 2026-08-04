open Stage0
open Protocols
open D_lang
open State.Syntax

(** Replaces each [Def.LaunchParam lp] with a synthesised [__global__]
    pseudo-kernel that pins gridDim/blockDim/path-condition via
    [assert]s and then calls the original kernel; the original is
    demoted to [Auxiliary] so the call-inliner picks it up. Opt-in
    via [--assume-launch] on faial-drf. *)

(** {1 Pseudo-kernel synthesis} *)

(** [d_to_imp] lifts these asserts to [Global]-visibility SMT
    hypotheses on every subsequent access. *)
let assert_axis_eq (base : string) (axis : string) (rhs : Expr.t) : Stmt.t =
  let var = Variable.from_name (base ^ "." ^ axis) in
  let lhs : Expr.t =
    Ident (Decl_expr.from_name ~ty:J_type.int ~kind:Decl_expr.Kind.Var var)
  in
  let cond : Expr.t =
    BinaryOperator { opcode = "=="; lhs; rhs; ty = J_type.bool }
  in
  Stmt.assert_stmt cond

(** One assert per axis; non-[Ident] axis expressions fold into a
    fresh per-axis pseudo-parameter, deduplicated against earlier
    slots through the resolver cache. An axis that [rewrite_dim3]
    returns as [None] emits [Skip] instead — under [--all-dims] that
    dim then ranges freely, which is sound but imprecise. *)
let dim_asserts (base : string) (e : C_lang.Expr.t) :
    (Host_translate.t, Stmt.t) State.t =
  let* rhs_x, rhs_y, rhs_z = Host_translate.rewrite_dim3 e in
  let mk axis = function
    | None -> Stmt.Skip
    | Some rhs -> assert_axis_eq base axis rhs
  in
  return (Stmt.from_list [ mk "x" rhs_x; mk "y" rhs_y; mk "z" rhs_z ])

let call_stmt (kernel : Decl_expr.t) (args : C_lang.Expr.t list) :
    (Host_translate.t, Stmt.t) State.t =
  let* args =
    args
    |> State.list_map Host_translate.rewrite_expr
  in
  (* Keep the launch site's [decl_id]: it is what resolves the call to
     the launched instantiation rather than to a same-named sibling. *)
  let func : Expr.t = Ident { kernel with kind = Decl_expr.Kind.Function } in
  return (Stmt.SExpr (CallExpr { func; args; ty = kernel.ty }))

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
  Printf.sprintf "%s@%s_%d" kernel_name file line

(** Converts a free variable into a kernel parameter. *)
let param_of_free_var (d : Decl_expr.t) : C_lang.Param.t option =
  if Ty.is_struct d.ty then None
  else
    let ty_var = Ty_variable.make ~ty:(Ty.strip_reference d.ty) ~name:d.name in
    Some (C_lang.Param.make ~ty_var ~is_used:true ~is_shared:false)

(** Lifts a path_condition into an assert. *)
let path_cond_asserts (lp : C_lang.LaunchParam.t) :
    (Host_translate.t, Stmt.t) State.t =
  match lp.path_condition with
  | None -> State.return Stmt.Skip
  | Some e ->
      let* d_expr = Host_translate.rewrite_expr e in
      State.return (Stmt.assert_stmt d_expr)

(** Each binding becomes a local [DeclStmt], giving Imp a definitional
    binding instead of a universally-quantified parameter. *)
let const_binding_decl (b : C_lang.ConstBinding.t) :
    (Host_translate.t, Stmt.t) State.t =
  let* rhs = Host_translate.rewrite_expr b.init in
  let ty_var = Ty_variable.make ~ty:b.ty ~name:b.name in
  let d = D_lang.Decl.from_expr ty_var rhs in
  State.return (Stmt.DeclStmt [ d ])

let rec binds_closure (e : C_lang.Expr.t) : bool =
  match e with
  | LambdaExpr _ -> true
  | Convert { arg; _ } -> binds_closure arg
  | CXXConstructExpr { args = [ arg ]; _ } -> binds_closure arg
  | _ -> false

let bindable (lp : C_lang.LaunchParam.t) : C_lang.ConstBinding.t list =
  lp.const_bindings
  |> List.filter (fun (b : C_lang.ConstBinding.t) -> not (binds_closure b.init))

let const_binding_decls (lp : C_lang.LaunchParam.t) :
    (Host_translate.t, Stmt.t) State.t =
  let* decls = State.list_map const_binding_decl (bindable lp) in
  State.return (Stmt.from_list decls)

(** Names emitted as decls must be filtered out of the parameter list
    to avoid double-binding. Every binding now emits a decl via
    [rewrite_expr] (which always succeeds), so this is just the names
    of every binding. *)
let bound_names_emitted (lp : C_lang.LaunchParam.t) : Variable.Set.t =
  bindable lp
  |> List.map (fun (b : C_lang.ConstBinding.t) -> b.name)
  |> Variable.Set.of_list

let dedup_by_name (type a) ~(name_of : a -> Variable.t) (xs : a list) : a list =
  let step (seen, acc) x =
    let n = name_of x in
    if Variable.Set.mem n seen then (seen, acc)
    else (Variable.Set.add n seen, x :: acc)
  in
  List.fold_left step (Variable.Set.empty, []) xs |> snd |> List.rev

let synth_kernel (lp : C_lang.LaunchParam.t) : Kernel.t =
  (* Shared resolver state across const bindings, grid, block,
     path_condition, and args so duplicate expressions across slots
     collapse to one uniform symbol. Const-binding decls are emitted
     first so their names are in scope for the asserts and the kernel
     call. *)
  let ctx, body =
    Host_translate.empty
    |> State.run (
      let* body_const_bindings = const_binding_decls lp in
      let* body_grid = dim_asserts "gridDim" lp.grid in
      let* body_block = dim_asserts "blockDim" lp.block in
      let* body_path_cond = path_cond_asserts lp in
      let* body_call = call_stmt lp.kernel lp.args in
      return (Stmt.from_list
        [ body_const_bindings; body_grid; body_block; body_path_cond; body_call ])
    )
  in
  let bound = bound_names_emitted lp in
  let direct_params =
    C_lang.LaunchParam.free_vars lp
    |> Decl_expr.Set.filter Decl_expr.is_runtime_value
    |> Decl_expr.Set.filter (fun (d : Decl_expr.t) ->
           not (Variable.Set.mem d.name bound))
    |> Decl_expr.Set.elements
    |> List.filter_map param_of_free_var
  in
  let fresh_params = Host_translate.fresh_params ctx in
  let params =
    direct_params @ fresh_params |> dedup_by_name ~name_of:C_lang.Param.name
  in
  let name = synth_name lp in
  let ty = Ty.to_string lp.kernel.ty in
  {
    Kernel.id = Imp.Function_id.make ~name ~ty ();
    decl_id = None;
    code = body;
    type_params = [];
    params;
    attribute = C_lang.KernelAttr.Default;
    returns_location = false;
  }

let rec fold_int (e : Expr.t) : int option =
  match e with
  | Expr.IntegerLiteral n -> Some n
  | Expr.Convert { arg; _ } -> fold_int arg
  | _ -> None

let rec fold_bool (e : Expr.t) : bool option =
  match e with
  | Expr.CXXBoolLiteralExpr b -> Some b
  | Expr.Convert { arg; _ } -> fold_bool arg
  | Expr.UnaryOperator { opcode = "!"; child; _ } ->
      fold_bool child |> Option.map not
  | Expr.BinaryOperator { opcode = "&&"; lhs; rhs; _ } -> (
      match (fold_bool lhs, fold_bool rhs) with
      | Some false, _ | _, Some false -> Some false
      | Some true, Some true -> Some true
      | _ -> None)
  | Expr.BinaryOperator { opcode = "||"; lhs; rhs; _ } -> (
      match (fold_bool lhs, fold_bool rhs) with
      | Some true, _ | _, Some true -> Some true
      | Some false, Some false -> Some false
      | _ -> None)
  | Expr.BinaryOperator { opcode; lhs; rhs; _ } -> (
      match (fold_int lhs, fold_int rhs) with
      | Some l, Some r -> (
          match opcode with
          | "==" -> Some (l = r)
          | "!=" -> Some (l <> r)
          | "<" -> Some (l < r)
          | "<=" -> Some (l <= r)
          | ">" -> Some (l > r)
          | ">=" -> Some (l >= r)
          | _ -> None)
      | _, _ -> None)
  | _ -> None

(** A path condition that folds to false describes a launch that no run
    performs, so it gets no pseudo-kernel: one is a kernel reported to have
    no accesses, which reads as a kernel that touches nothing. An
    instantiation of an enclosing template is where a constant condition
    comes from. *)
let is_dead_launch (lp : C_lang.LaunchParam.t) : bool =
  match lp.path_condition with
  | None -> false
  | Some e -> (
      Host_translate.empty
      |> State.run (Host_translate.rewrite_expr e)
      |> snd
      |> fold_bool
      |> function
      | Some false -> true
      | Some true | None -> false)

(** The name a pseudo-kernel gets is its callee and its source line, which
    two records share whenever one line is reached twice: from two
    instantiations of an enclosing template, or from two launches written
    side by side. Records that synthesise the same body and call the same
    declaration are one launch and merge; the rest keep their own identity
    and are numbered, since a shared name would make the last one written
    overwrite the others in the kernel map. *)
let synth_kernels (p : Program.t) : Kernel.t list =
  let key ((lp, k) : C_lang.LaunchParam.t * Kernel.t) : string =
    (Kernel.to_s k |> Indent.to_string)
    ^ "|"
    ^ Option.value ~default:"" lp.kernel.decl_id
  in
  let dedup (seen, acc) (x : C_lang.LaunchParam.t * Kernel.t) =
    let k = key x in
    if Common.StringSet.mem k seen then (seen, acc)
    else (Common.StringSet.add k seen, snd x :: acc)
  in
  let number (seen, acc) (k : Kernel.t) =
    let name = Imp.Function_id.name k.id in
    let n = Common.StringMap.find_opt name seen |> Option.value ~default:0 in
    let k =
      if n = 0 then k
      else
        let id =
          Imp.Function_id.make
            ~name:(Printf.sprintf "%s#%d" name (n + 1))
            ~ty:(Imp.Function_id.ty k.id) ()
        in
        { k with id }
    in
    (Common.StringMap.add name (n + 1) seen, k :: acc)
  in
  p
  |> List.filter_map (function
      | Def.LaunchParam lp when not (is_dead_launch lp) ->
          Some (lp, synth_kernel lp)
      | _ -> None)
  |> List.fold_left dedup (Common.StringSet.empty, [])
  |> snd |> List.rev
  |> List.fold_left number (Common.StringMap.empty, [])
  |> snd |> List.rev

(** {1 Demote launched kernels} *)

(** Resolves every launch target the same way the synthesised call
    will, so a kernel is demoted exactly when a wrapper stands in for
    it. Matching on the bare name instead demotes every same-named
    sibling, which leaves an unlaunched overload or namespace member
    with neither an entry point nor a wrapper, and therefore no
    verdict at all. *)
let launched_ids (p : Program.t) : Imp.Function_id.Set.t =
  let db = SignatureDB.from_program p in
  List.fold_left
    (fun acc def ->
      match def with
      | Def.LaunchParam lp when not (is_dead_launch lp) -> (
          let func : Expr.t =
            Ident { lp.kernel with kind = Decl_expr.Kind.Function }
          in
          match SignatureDB.lookup func (List.length lp.args) db with
          | Some s -> Imp.Function_id.Set.add s.id acc
          | None -> acc)
      | _ -> acc)
    Imp.Function_id.Set.empty p

let demote_if_launched (launched : Imp.Function_id.Set.t) (k : Kernel.t) :
    Kernel.t =
  if
    Imp.Function_id.Set.mem k.id launched
    && k.attribute = C_lang.KernelAttr.Default
  then { k with attribute = C_lang.KernelAttr.Auxiliary }
  else k

(** {1 Top-level transform} *)

(** Top-level entry point of the launch-synthesis pass. *)
let rewrite_program (p : Program.t) : Program.t =
  (* Synth kernels are emitted before demoted originals so the
     call-inliner sees callees before callers. *)
  let launched = launched_ids p in
  let synth = synth_kernels p |> List.map (fun k -> Def.Kernel k) in
  let rest =
    p
    |> List.filter_map (function
        | Def.LaunchParam _ -> None
        | Def.Kernel k -> Some (Def.Kernel (demote_if_launched launched k))
        | other -> Some other)
  in
  rest @ synth
