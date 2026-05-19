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
    slots through the resolver cache. An axis that [rewrite_axis]
    returns as [None] emits [Skip] instead — under [--all-dims] that
    dim then ranges freely, which is sound but imprecise. *)
let dim_asserts (base : string) (e : C_lang.Expr.t) :
    (Host_translate.t, Stmt.t) State.t =
  let* rhs_x, rhs_y, rhs_z = Host_translate.rewrite_axis base e in
  let mk axis = function
    | None -> Stmt.Skip
    | Some rhs -> assert_axis_eq base axis rhs
  in
  return (Stmt.from_list [ mk "x" rhs_x; mk "y" rhs_y; mk "z" rhs_z ])

let call_stmt (kernel : Decl_expr.t) (args : C_lang.Expr.t list) :
    (Host_translate.t, Stmt.t) State.t =
  let* args =
    args
    |> List.mapi (fun i a -> (i, a))
    |> State.list_map (fun (i, a) -> Host_translate.rewrite_arg i a)
  in
  let func : Expr.t =
    Ident
      (Decl_expr.from_name ~ty:kernel.ty ~kind:Decl_expr.Kind.Function
         kernel.name)
  in
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
  Printf.sprintf "%s__launch_%s_%d" kernel_name file line

(** Converts a free variable into a kernel parameter. *)
let param_of_free_var (d : Decl_expr.t) : C_lang.Param.t option =
  if J_type.matches C_type.is_struct d.ty then None
  else
    let ty_var = Ty_variable.make ~ty:d.ty ~name:d.name in
    Some (C_lang.Param.make ~ty_var ~is_used:true ~is_shared:false)

(** Lifts a path_condition into an assert. *)
let path_cond_asserts (lp : C_lang.LaunchParam.t) : Stmt.t =
  let ( let* ) = Option.bind in
  (let* e = lp.path_condition in
   let* d_expr = Host_translate.lift_pure e in
   Some (Stmt.assert_stmt d_expr))
  |> Option.value ~default:Stmt.Skip

(** Each binding becomes a local [DeclStmt], giving Imp a definitional
    binding instead of a universally-quantified parameter. *)
let const_binding_decl (b : C_lang.ConstBinding.t) : Stmt.t option =
  let ( let* ) = Option.bind in
  let* rhs = Host_translate.lift_pure b.init in
  let ty_var = Ty_variable.make ~ty:b.ty ~name:b.name in
  let d = D_lang.Decl.from_expr ty_var rhs in
  Some (Stmt.DeclStmt [ d ])

let const_binding_decls (lp : C_lang.LaunchParam.t) : Stmt.t =
  lp.const_bindings |> List.filter_map const_binding_decl |> Stmt.from_list

(** Names emitted as decls must be filtered out of the parameter list
    to avoid double-binding. *)
let bound_names_emitted (lp : C_lang.LaunchParam.t) : Variable.Set.t =
  lp.const_bindings
  |> List.filter_map (fun (b : C_lang.ConstBinding.t) ->
         Option.map (fun _ -> b.name) (Host_translate.lift_pure b.init))
  |> Variable.Set.of_list

let dedup_by_name (type a) ~(name_of : a -> Variable.t) (xs : a list) : a list =
  let step (seen, acc) x =
    let n = name_of x in
    if Variable.Set.mem n seen then (seen, acc)
    else (Variable.Set.add n seen, x :: acc)
  in
  List.fold_left step (Variable.Set.empty, []) xs |> snd |> List.rev

(** Rewrites arg slots whose top-level expression equals a
    [const_bindings] init back to [Ident <binding-name>], so a host
    [const] that cu-to-json inlined here re-converges with its named
    uses elsewhere in the launch instead of being abstracted. *)
let rebind_args_to_const_names
    (bindings : C_lang.ConstBinding.t list) (args : C_lang.Expr.t list) :
    C_lang.Expr.t list * C_lang.ConstBinding.t list =
  let table : (string, C_lang.ConstBinding.t) Hashtbl.t =
    Hashtbl.create (List.length bindings)
  in
  List.iter
    (fun (b : C_lang.ConstBinding.t) ->
      Hashtbl.replace table (C_lang.Expr.to_string b.init) b)
    bindings;
  let used = ref [] in
  let rewrite (a : C_lang.Expr.t) : C_lang.Expr.t =
    match Hashtbl.find_opt table (C_lang.Expr.to_string a) with
    | Some b ->
        used := b :: !used;
        C_lang.Expr.Ident
          (Decl_expr.from_name ~ty:b.ty ~kind:Decl_expr.Kind.Var b.name)
    | None -> a
  in
  let args = List.map rewrite args in
  let used =
    !used
    |> dedup_by_name ~name_of:(fun (b : C_lang.ConstBinding.t) -> b.name)
  in
  (args, used)

let synth_kernel (lp : C_lang.LaunchParam.t) : Kernel.t =
  let args, rebound_bindings =
    rebind_args_to_const_names lp.const_bindings lp.args
  in
  (* Shared resolver state across grid, block, and args so duplicate
     expressions across slots collapse to one uniform symbol. *)
  let ctx, (body_grid, body_block, body_call) =
    Host_translate.empty
    |> State.run (
      let* body_grid = dim_asserts "gridDim" lp.grid in
      let* body_block = dim_asserts "blockDim" lp.block in
      let* body_call = call_stmt lp.kernel args in
      return (body_grid, body_block, body_call)
    )
  in
  let body_path_cond = path_cond_asserts lp in
  let body_const_bindings = const_binding_decls lp in
  (* Const-binding decls first so their names are in scope for the
     asserts and the kernel call. *)
  let body =
    Stmt.from_list
      [ body_const_bindings; body_grid; body_block; body_path_cond; body_call ]
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
  (* Binding names introduced by the arg-slot rebind. Already in
     [direct_params] when the same binding is also referenced
     elsewhere (grid / block / path_condition); excluded from [bound]
     because impure-init bindings emit no [DeclStmt]. The trailing
     [dedup_by_name] reconciles the overlap. *)
  let rebound_params =
    rebound_bindings
    |> List.filter_map (fun (b : C_lang.ConstBinding.t) ->
           if Variable.Set.mem b.name bound then None
           else
             let d =
               Decl_expr.from_name ~ty:b.ty ~kind:Decl_expr.Kind.Var b.name
             in
             param_of_free_var d)
  in
  let fresh_params = Host_translate.fresh_params ctx in
  let params =
    direct_params @ rebound_params @ fresh_params
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

(** {1 Demote launched kernels} *)

let demote_if_launched (launched : Variable.Set.t) (k : Kernel.t) : Kernel.t =
  let n = Variable.from_name k.name in
  if Variable.Set.mem n launched && k.attribute = C_lang.KernelAttr.Default
  then { k with attribute = C_lang.KernelAttr.Auxiliary }
  else k

(** {1 Top-level transform} *)

(** Top-level entry point of the launch-synthesis pass. *)
let rewrite_program (p : Program.t) : Program.t =
  (* Synth kernels are emitted before demoted originals so the
     call-inliner sees callees before callers. *)
  let launched = Program.launched_kernel_names p in
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
