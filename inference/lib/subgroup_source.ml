open Protocols
module SM = Subgroup_matrix
module StringMap = Stage0.Common.StringMap
module StringSet = Stage0.Common.StringSet
module IntMap = Stage0.Common.IntMap

type error =
  | Missing_subgroup_config of { kernel : string }
  | Unsupported_expression of { context : string; expr : string }
  | Unsupported_participation_control of { kernel : string; control : string }
  | Unsupported_matrix_call of { op : string; reason : string; expr : string }
  | Subgroup_callee_requires_inlining of { kernel : string; callee : string }
  | Launch_wrapper_inlining_error of {
      kernel : string;
      callee : string option;
      reason : string;
    }
  | Conflicting_launch_dimension of {
      kernel : string;
      dimension : string;
      previous : Exp.nexp;
      next : Exp.nexp;
    }

type routed_kernel =
  | Ordinary_source of D_lang.Kernel.t
  | Subgroup_matrix of subgroup_kernel

and ordinary_memory_kind = Ordinary_read | Ordinary_write | Ordinary_atomic

and ordinary_memory_site = {
  id : int;
  source_order : int;
  label : string;
  location : Stage0.Location.t option;
}

and ordinary_memory_phase = { workgroup : int; subgroup : SM.Site.id list }

and ordinary_memory_effect = {
  kind : ordinary_memory_kind;
  site : ordinary_memory_site;
  access : Access.t;
  source_conditions : Exp.bexp list;
  runtime_condition : Exp.bexp option;
  phase : ordinary_memory_phase;
  target_config : SM.Target_config.t;
}

and site_control = {
  site_id : SM.Site.id;
  source_order : int;
  conditions : Exp.bexp list;
  memory_conditions : Exp.bexp list;
  uniform_vars : Variable.Set.t;
  numeric_aliases : Exp.nexp Variable.Map.t;
}

and subgroup_kernel = {
  matrix_kernel : SM.Kernel.t;
  site_controls : site_control list;
  uniform_vars : Variable.Set.t;
  memory_globals : Variable.Set.t;
  ordinary_memory_effects : ordinary_memory_effect list;
  launch_precondition : Exp.bexp;
  launch_dimensions : Exp.nexp Variable.Map.t;
}

let error_to_string : error -> string = function
  | Missing_subgroup_config { kernel } ->
      Printf.sprintf
        "kernel '%s' contains subgroup/matrix source operations but no \
         explicit subgroup configuration was provided"
        kernel
  | Unsupported_expression { context; expr } ->
      Printf.sprintf "unsupported expression in %s: %s" context expr
  | Unsupported_participation_control { kernel; control } ->
      Printf.sprintf "kernel '%s' uses unsupported participation control: %s"
        kernel control
  | Unsupported_matrix_call { op; reason; expr } ->
      Printf.sprintf "unsupported matrix call '%s': %s in %s" op reason expr
  | Subgroup_callee_requires_inlining { kernel; callee } ->
      Printf.sprintf
        "kernel '%s' calls subgroup kernel '%s'; subgroup launch-wrapper \
         inlining is required to preserve launch assertions and arguments"
        kernel callee
  | Launch_wrapper_inlining_error { kernel; callee; reason } ->
      let callee =
        callee
        |> Option.map (Printf.sprintf " and callee '%s'")
        |> Option.value ~default:""
      in
      Printf.sprintf "cannot inline launch wrapper '%s'%s: %s" kernel callee
        reason
  | Conflicting_launch_dimension { kernel; dimension; previous; next } ->
      Printf.sprintf
        "kernel '%s' has conflicting launch facts for %s: %s and %s" kernel
        dimension (Exp.n_to_string previous) (Exp.n_to_string next)

let call_name : D_lang.Expr.t -> string option = function
  | Ident { name; kind = Function | CXXMethod; _ }
  | UnresolvedLookupExpr { name; _ } ->
      Some (Variable.name name)
  | _ -> None

let call_location : D_lang.Expr.t -> Stage0.Location.t option = function
  | Ident { name; _ } | UnresolvedLookupExpr { name; _ } ->
      Variable.location_opt name
  | _ -> None

let expr_to_string = D_lang.Expr.to_string

let option_exists (pred : 'a -> bool) : 'a option -> bool = function
  | Some value -> pred value
  | None -> false

let j_type_is_pointer_like (ty : Ty.t) : bool =
  ty |> fun ty -> Ty.is_pointer ty || Ty.is_array ty || Ty.is_auto ty

let j_type_is_pointer_decl (ty : Ty.t) : bool =
  ty |> fun ty -> Ty.is_pointer ty || Ty.is_auto ty

let expr_is_pointer_like (expr : D_lang.Expr.t) : bool =
  D_lang.Expr.to_type expr |> j_type_is_pointer_like

let unsupported_expr ~(context : string) (expr : D_lang.Expr.t) :
    ('a, error) result =
  Error (Unsupported_expression { context; expr = expr_to_string expr })

let map_matrix_error ~(op : string) ~(expr : D_lang.Expr.t) :
    (SM.Matrix.collective, string) result ->
    (SM.Matrix.collective, error) result = function
  | Ok value -> Ok value
  | Error reason ->
      Error (Unsupported_matrix_call { op; reason; expr = expr_to_string expr })

let n_rel_of_opcode : string -> N_rel.t option = function
  | "==" -> Some N_rel.Eq
  | "!=" -> Some N_rel.Neq
  | "<" -> Some (N_rel.Lt Signedness.Signed)
  | "<=" -> Some (N_rel.Le Signedness.Signed)
  | ">" -> Some (N_rel.Gt Signedness.Signed)
  | ">=" -> Some (N_rel.Ge Signedness.Signed)
  | _ -> None

let b_rel_of_opcode : string -> B_rel.t option = function
  | "&&" -> Some B_rel.BAnd
  | "||" -> Some B_rel.BOr
  | _ -> None

let rec nexp_of_expr ~(context : string) (expr : D_lang.Expr.t) :
    (Exp.nexp, error) result =
  let ( let* ) = Result.bind in
  let binary op lhs rhs =
    let* lhs = nexp_of_expr ~context lhs in
    let* rhs = nexp_of_expr ~context rhs in
    Ok (Exp.n_bin op lhs rhs)
  in
  match expr with
  | Ident decl -> Ok (Exp.Var decl.name)
  | Convert { arg; ty } -> (
      let* arg = nexp_of_expr ~context arg in
      match Ty.to_scalar ty with
      | Some scalar -> Ok (Exp.convert scalar arg)
      | None -> unsupported_expr ~context expr)
  | CXXBoolLiteralExpr value -> Ok (Exp.Num (if value then 1 else 0))
  | IntegerLiteral n | CharacterLiteral n -> Ok (Exp.Num n)
  | FloatingLiteral n -> Ok (Exp.Num (Float.to_int n))
  | SizeOfExpr ty -> (
      match Ty.sizeof ty with
      | Some size -> Ok (Exp.Num size)
      | None -> unsupported_expr ~context expr)
  | MemberExpr _ -> Ok (Exp.Var (Variable.from_name (expr_to_string expr)))
  | BinaryOperator { opcode = "+"; lhs; rhs; _ } ->
      binary (Plus Signedness.Signed) lhs rhs
  | BinaryOperator { opcode = "-"; lhs; rhs; _ } ->
      binary (Minus Signedness.Signed) lhs rhs
  | BinaryOperator { opcode = "*"; lhs; rhs; _ } ->
      binary (Mult Signedness.Signed) lhs rhs
  | BinaryOperator { opcode = "/"; lhs; rhs; _ } ->
      binary (Div Signedness.Signed) lhs rhs
  | BinaryOperator { opcode = "%"; lhs; rhs; _ } ->
      binary (Mod Signedness.Signed) lhs rhs
  | BinaryOperator { opcode = "<<"; lhs; rhs; _ } -> binary LeftShift lhs rhs
  | BinaryOperator { opcode = ">>"; lhs; rhs; _ } ->
      binary (RightShift Signedness.Signed) lhs rhs
  | BinaryOperator { opcode = "&"; lhs; rhs; _ } -> binary BitAnd lhs rhs
  | BinaryOperator { opcode = "|"; lhs; rhs; _ } -> binary BitOr lhs rhs
  | BinaryOperator { opcode = "^"; lhs; rhs; _ } -> binary BitXOr lhs rhs
  | CallExpr { func; args = [ lhs; rhs ]; _ }
    when Option.equal String.equal (call_name func) (Some "min")
         || Option.equal String.equal (call_name func) (Some "fminf") ->
      let* lhs = nexp_of_expr ~context lhs in
      let* rhs = nexp_of_expr ~context rhs in
      Ok (Exp.n_if (Exp.n_lt lhs rhs) lhs rhs)
  | CallExpr { func; args = [ lhs; rhs ]; _ }
    when Option.equal String.equal (call_name func) (Some "max")
         || Option.equal String.equal (call_name func) (Some "fmaxf") ->
      let* lhs = nexp_of_expr ~context lhs in
      let* rhs = nexp_of_expr ~context rhs in
      Ok (Exp.n_if (Exp.n_gt lhs rhs) lhs rhs)
  | CallExpr { func; args = [ value ]; _ }
    when Option.equal String.equal (call_name func) (Some "__half2float")
         || Option.equal String.equal (call_name func) (Some "__float2half_rn")
    ->
      let* value = nexp_of_expr ~context value in
      Ok (Exp.NCall (Option.get (call_name func), [ value ]))
  | UnaryOperator { opcode = "-"; child; _ } ->
      let* child = nexp_of_expr ~context child in
      Ok (Exp.n_uminus child)
  | ConditionalOperator { cond; then_expr; else_expr; _ } ->
      let* cond = bexp_of_expr ~context cond in
      let* then_expr = nexp_of_expr ~context then_expr in
      let* else_expr = nexp_of_expr ~context else_expr in
      Ok (Exp.n_if cond then_expr else_expr)
  | BinaryOperator { opcode = ","; rhs; _ } -> nexp_of_expr ~context rhs
  | _ -> unsupported_expr ~context expr

and bexp_of_expr ~(context : string) (expr : D_lang.Expr.t) :
    (Exp.bexp, error) result =
  let ( let* ) = Result.bind in
  match expr with
  | CXXBoolLiteralExpr value -> Ok (Exp.Bool value)
  | BinaryOperator { opcode = ","; rhs; _ } -> bexp_of_expr ~context rhs
  | BinaryOperator { opcode; lhs; rhs; _ } -> (
      match b_rel_of_opcode opcode with
      | Some rel ->
          let* lhs = bexp_of_expr ~context lhs in
          let* rhs = bexp_of_expr ~context rhs in
          Ok (Exp.b_rel rel lhs rhs)
      | None -> (
          match n_rel_of_opcode opcode with
          | Some rel ->
              let* lhs = nexp_of_expr ~context lhs in
              let* rhs = nexp_of_expr ~context rhs in
              Ok (Exp.n_rel rel lhs rhs)
          | None ->
              let* expr = nexp_of_expr ~context expr in
              Ok (Exp.cast_bool expr)))
  | UnaryOperator { opcode = "!"; child; _ } ->
      let* child = bexp_of_expr ~context child in
      Ok (Exp.b_not child)
  | _ ->
      let* expr = nexp_of_expr ~context expr in
      Ok (Exp.cast_bool expr)

type pointer_alias = {
  source : Variable.t;
  offset : Exp.nexp;
  required_controls : Exp.bexp list;
}

type aggregate_alias = {
  aggregate_array : Variable.t;
  aggregate_index : Exp.nexp list;
}

let pointer_alias_equal (lhs : pointer_alias) (rhs : pointer_alias) : bool =
  Variable.equal lhs.source rhs.source
  && Exp.n_equal lhs.offset rhs.offset
  && List.equal Exp.b_equal lhs.required_controls rhs.required_controls

type collect_state = {
  kernel_name : string;
  next_site_id : int;
  next_memory_site_id : int;
  next_source_order : int;
  stmts_rev : SM.Stmt.t list;
  site_controls_rev : site_control list;
  ordinary_memory_effects_rev : ordinary_memory_effect list;
  site_source_orders : int IntMap.t;
  control_stack : Exp.bexp list;
  workgroup_phase : int;
  subgroup_phase : SM.Site.id list;
  target_config : SM.Target_config.t;
  uniform_vars : Variable.Set.t;
  memory_globals : Variable.Set.t;
  thread_x_coordinate_vars : Variable.Set.t;
  constant_values : int Variable.Map.t;
  numeric_aliases : Exp.nexp Variable.Map.t;
  pointer_aliases : pointer_alias Variable.Map.t;
  aggregate_aliases : aggregate_alias Variable.Map.t;
  invalid_pointer_aliases : Variable.Set.t;
  private_arrays : Variable.Set.t;
  uniform_preserving_calls : StringSet.t;
  launch_preconditions_rev : Exp.bexp list;
  launch_dimensions : Exp.nexp Variable.Map.t;
  type_aliases : Ty.t StringMap.t;
  helper_kernels : D_lang.Kernel.t list StringMap.t;
  subgroup_helpers : StringSet.t;
  active_helpers : StringSet.t;
  current_template_args : C_lang.TemplateArgument.t list;
  call_result : Variable.t option;
}

type scalar_facts = {
  fact_uniform_vars : Variable.Set.t;
  fact_memory_globals : Variable.Set.t;
  fact_thread_x_coordinate_vars : Variable.Set.t;
  fact_constant_values : int Variable.Map.t;
  fact_numeric_aliases : Exp.nexp Variable.Map.t;
  fact_pointer_aliases : pointer_alias Variable.Map.t;
  fact_aggregate_aliases : aggregate_alias Variable.Map.t;
  fact_invalid_pointer_aliases : Variable.Set.t;
}

let scalar_facts_of (state : collect_state) : scalar_facts =
  {
    fact_uniform_vars = state.uniform_vars;
    fact_memory_globals = state.memory_globals;
    fact_thread_x_coordinate_vars = state.thread_x_coordinate_vars;
    fact_constant_values = state.constant_values;
    fact_numeric_aliases = state.numeric_aliases;
    fact_pointer_aliases = state.pointer_aliases;
    fact_aggregate_aliases = state.aggregate_aliases;
    fact_invalid_pointer_aliases = state.invalid_pointer_aliases;
  }

let restore_scalar_facts (state : collect_state) (facts : scalar_facts) :
    collect_state =
  {
    state with
    uniform_vars = facts.fact_uniform_vars;
    memory_globals = facts.fact_memory_globals;
    thread_x_coordinate_vars = facts.fact_thread_x_coordinate_vars;
    constant_values = facts.fact_constant_values;
    numeric_aliases = facts.fact_numeric_aliases;
    pointer_aliases = facts.fact_pointer_aliases;
    aggregate_aliases = facts.fact_aggregate_aliases;
    invalid_pointer_aliases = facts.fact_invalid_pointer_aliases;
  }

let pointer_fact_candidates (states : collect_state list) : Variable.Set.t =
  List.fold_left
    (fun vars (state : collect_state) ->
      let vars =
        Variable.Map.fold
          (fun var _ vars -> Variable.Set.add var vars)
          state.pointer_aliases vars
      in
      Variable.Set.union vars state.invalid_pointer_aliases)
    Variable.Set.empty states

let join_pointer_facts (states : collect_state list) :
    pointer_alias Variable.Map.t * Variable.Set.t =
  let aliases, candidates =
    match states with
    | [] -> failwith "join_pointer_facts requires at least one state"
    | _ :: _ ->
        let candidates = pointer_fact_candidates states in
        let aliases =
          Variable.Set.fold
            (fun var aliases ->
              let present =
                List.filter_map
                  (fun (state : collect_state) ->
                    Variable.Map.find_opt var state.pointer_aliases)
                  states
              in
              match present with
              | alias :: others
                when List.for_all (pointer_alias_equal alias) others
                     && (List.length present = List.length states
                        || alias.required_controls <> []) ->
                  Variable.Map.add var alias aliases
              | _ -> aliases)
            candidates Variable.Map.empty
        in
        (aliases, candidates)
  in
  let invalid =
    Variable.Set.filter
      (fun var -> not (Variable.Map.mem var aliases))
      candidates
  in
  (aliases, invalid)

let join_scalar_facts (states : collect_state list) : scalar_facts =
  match states with
  | [] -> failwith "join_scalar_facts requires at least one state"
  | state :: rest ->
      let uniform_vars =
        List.fold_left
          (fun vars (state : collect_state) ->
            Variable.Set.inter vars state.uniform_vars)
          state.uniform_vars rest
      in
      let thread_x_coordinate_vars =
        List.fold_left
          (fun vars (state : collect_state) ->
            Variable.Set.inter vars state.thread_x_coordinate_vars)
          state.thread_x_coordinate_vars rest
      in
      let memory_globals =
        List.fold_left
          (fun vars (state : collect_state) ->
            Variable.Set.inter vars state.memory_globals)
          state.memory_globals rest
      in
      let constant_values =
        Variable.Map.filter
          (fun var value ->
            List.for_all
              (fun (state : collect_state) ->
                Option.equal Int.equal
                  (Variable.Map.find_opt var state.constant_values)
                  (Some value))
              rest)
          state.constant_values
      in
      let numeric_aliases =
        Variable.Map.filter
          (fun var expr ->
            List.for_all
              (fun (state : collect_state) ->
                Option.equal ( = )
                  (Variable.Map.find_opt var state.numeric_aliases)
                  (Some expr))
              rest)
          state.numeric_aliases
      in
      let aggregate_aliases =
        Variable.Map.filter
          (fun var alias ->
            List.for_all
              (fun (state : collect_state) ->
                Option.equal ( = )
                  (Variable.Map.find_opt var state.aggregate_aliases)
                  (Some alias))
              rest)
          state.aggregate_aliases
      in
      let pointer_aliases, invalid_pointer_aliases =
        join_pointer_facts states
      in
      {
        fact_uniform_vars = uniform_vars;
        fact_memory_globals = memory_globals;
        fact_thread_x_coordinate_vars = thread_x_coordinate_vars;
        fact_constant_values = constant_values;
        fact_numeric_aliases = numeric_aliases;
        fact_pointer_aliases = pointer_aliases;
        fact_aggregate_aliases = aggregate_aliases;
        fact_invalid_pointer_aliases = invalid_pointer_aliases;
      }

let join_if_pointer_facts (guard : Exp.bexp) (then_state : collect_state)
    (else_state : collect_state) : pointer_alias Variable.Map.t * Variable.Set.t
    =
  let candidates = pointer_fact_candidates [ then_state; else_state ] in
  let else_guard = Exp.b_not guard in
  let alias_requires condition alias =
    List.exists (Exp.b_equal condition) alias.required_controls
  in
  let aliases =
    Variable.Set.fold
      (fun var aliases ->
        let then_alias = Variable.Map.find_opt var then_state.pointer_aliases in
        let else_alias = Variable.Map.find_opt var else_state.pointer_aliases in
        match (then_alias, else_alias) with
        | Some lhs, Some rhs when pointer_alias_equal lhs rhs ->
            Variable.Map.add var lhs aliases
        | Some lhs, rhs
          when alias_requires guard lhs
               && Option.fold ~none:true ~some:(alias_requires guard) rhs ->
            Variable.Map.add var lhs aliases
        | lhs, Some rhs
          when alias_requires else_guard rhs
               && Option.fold ~none:true ~some:(alias_requires else_guard) lhs
          ->
            Variable.Map.add var rhs aliases
        | _ -> aliases)
      candidates Variable.Map.empty
  in
  let invalid =
    Variable.Set.filter
      (fun var -> not (Variable.Map.mem var aliases))
      candidates
  in
  (aliases, invalid)

let join_if_scalar_facts (guard : Exp.bexp) (then_state : collect_state)
    (else_state : collect_state) : scalar_facts =
  let facts = join_scalar_facts [ then_state; else_state ] in
  let pointer_aliases, invalid_pointer_aliases =
    join_if_pointer_facts guard then_state else_state
  in
  {
    facts with
    fact_pointer_aliases = pointer_aliases;
    fact_invalid_pointer_aliases = invalid_pointer_aliases;
  }

let ordinary_memory_kind_to_string : ordinary_memory_kind -> string = function
  | Ordinary_read -> "read"
  | Ordinary_write -> "write"
  | Ordinary_atomic -> "atomic"

let ordinary_memory_effect_to_string (memory_effect : ordinary_memory_effect) :
    string =
  ordinary_memory_kind_to_string memory_effect.kind
  ^ " "
  ^ Variable.name memory_effect.access.array
  ^ Access.index_to_string memory_effect.access.index

let location_opt (location : Stage0.Location.t) : Stage0.Location.t option =
  if String.equal (Stage0.Location.filename location) "" then None
  else Some location

let ordinary_memory_site_to_string (site : ordinary_memory_site) : string =
  let location =
    site.location
    |> Option.map (fun location -> "@" ^ Stage0.Location.to_string location)
    |> Option.value ~default:""
  in
  Printf.sprintf "ordinary#%d/order#%d[%s]%s" site.id site.source_order
    site.label location

let ordinary_memory_phase_to_string (phase : ordinary_memory_phase) : string =
  let subgroup =
    match phase.subgroup with
    | [] -> "S[]"
    | ids -> "S[" ^ (ids |> List.map string_of_int |> String.concat ";") ^ "]"
  in
  Printf.sprintf "W%d/%s" phase.workgroup subgroup

let ordinary_memory_conditions_to_string (conditions : Exp.bexp list) : string =
  conditions |> Exp.b_and_ex |> Exp.b_to_string

let ordinary_memory_effect_summary (memory_effect : ordinary_memory_effect) :
    string =
  let runtime_condition =
    memory_effect.runtime_condition |> Option.map Exp.b_to_string
    |> Option.value ~default:"true"
  in
  Printf.sprintf
    "%s effect=%s access=%s phase=%s control=%s runtime=%s target=%s"
    (ordinary_memory_site_to_string memory_effect.site)
    (ordinary_memory_kind_to_string memory_effect.kind)
    (Access.to_string memory_effect.access)
    (ordinary_memory_phase_to_string memory_effect.phase)
    (ordinary_memory_conditions_to_string memory_effect.source_conditions)
    runtime_condition
    (SM.Target_config.to_string memory_effect.target_config)

let site_control_memory_conditions (control : site_control) : Exp.bexp list =
  control.memory_conditions

let is_source_uniform_builtin (x : Variable.t) : bool =
  List.exists (Variable.equal x)
    (Variable.bid_list @ Variable.bdim_list @ Variable.gdim_list)

let is_memory_global_builtin (x : Variable.t) : bool =
  List.exists (Variable.equal x)
    (Variable.bid_list @ Variable.bdim_list @ Variable.gdim_list)

let target_subgroup_size_value (state : collect_state) : int option =
  SM.Target_config.cuda_x_contiguous_subgroup_size state.target_config
  |> Option.map SM.Target_config.subgroup_size_value

let rec int_constant_of_expr (state : collect_state) :
    D_lang.Expr.t -> int option = function
  | CharacterLiteral n | IntegerLiteral n -> Some n
  | CXXBoolLiteralExpr value -> Some (if value then 1 else 0)
  | Ident decl -> Variable.Map.find_opt decl.name state.constant_values
  | Convert { arg; ty } ->
      Option.bind (int_constant_of_expr state arg) (fun value ->
          Option.bind (Ty.to_scalar ty) (fun scalar ->
              Scalar.reduce value scalar))
  | CallExpr { func; args = []; _ }
    when Option.equal String.equal (call_name func)
           (Some "ggml_cuda_get_physical_warp_size") ->
      target_subgroup_size_value state
  | UnaryOperator { opcode = "-"; child; _ } ->
      int_constant_of_expr state child |> Option.map Int.neg
  | UnaryOperator { opcode = "!"; child; _ } ->
      int_constant_of_expr state child
      |> Option.map (fun value -> if value = 0 then 1 else 0)
  | UnaryOperator { opcode = "~"; child; _ } ->
      int_constant_of_expr state child |> Option.map lnot
  | BinaryOperator { opcode; lhs; rhs; _ } ->
      let binary f =
        match
          (int_constant_of_expr state lhs, int_constant_of_expr state rhs)
        with
        | Some lhs, Some rhs -> f lhs rhs
        | _ -> None
      in
      begin match opcode with
      | "+" -> binary (fun lhs rhs -> Some (lhs + rhs))
      | "-" -> binary (fun lhs rhs -> Some (lhs - rhs))
      | "*" -> binary (fun lhs rhs -> Some (lhs * rhs))
      | "/" ->
          binary (fun lhs rhs -> if rhs = 0 then None else Some (lhs / rhs))
      | "%" ->
          binary (fun lhs rhs -> if rhs = 0 then None else Some (lhs mod rhs))
      | "==" -> binary (fun lhs rhs -> Some (if lhs = rhs then 1 else 0))
      | "!=" -> binary (fun lhs rhs -> Some (if lhs <> rhs then 1 else 0))
      | "<" -> binary (fun lhs rhs -> Some (if lhs < rhs then 1 else 0))
      | "<=" -> binary (fun lhs rhs -> Some (if lhs <= rhs then 1 else 0))
      | ">" -> binary (fun lhs rhs -> Some (if lhs > rhs then 1 else 0))
      | ">=" -> binary (fun lhs rhs -> Some (if lhs >= rhs then 1 else 0))
      | "&&" ->
          binary (fun lhs rhs -> Some (if lhs <> 0 && rhs <> 0 then 1 else 0))
      | "||" ->
          binary (fun lhs rhs -> Some (if lhs <> 0 || rhs <> 0 then 1 else 0))
      | "&" -> binary (fun lhs rhs -> Some (lhs land rhs))
      | "|" -> binary (fun lhs rhs -> Some (lhs lor rhs))
      | "^" -> binary (fun lhs rhs -> Some (lhs lxor rhs))
      | _ -> None
      end
  | SizeOfExpr _ | CXXNewExpr _ | CXXDeleteExpr _ | RecoveryExpr _
  | FloatingLiteral _ | MemberExpr _ | CallExpr _ | ConditionalOperator _
  | CXXConstructExpr _ | CXXOperatorCallExpr _ | UnaryOperator _
  | UnresolvedLookupExpr _ ->
      None

let variable_of_ident_or_member : D_lang.Expr.t -> Variable.t option = function
  | Ident decl -> Some decl.name
  | MemberExpr { base = Ident base; name = field; _ } ->
      Some (Variable.update_name (fun name -> name ^ "." ^ field) base.name)
  | _ -> None

let expr_is_thread_x_coordinate (state : collect_state) (expr : D_lang.Expr.t) :
    bool =
  match variable_of_ident_or_member expr with
  | Some var ->
      Variable.equal var Variable.tid_x
      || Variable.Set.mem var state.thread_x_coordinate_vars
  | None -> false

let expr_is_configured_subgroup_id (state : collect_state)
    (expr : D_lang.Expr.t) : bool =
  match expr with
  | BinaryOperator { opcode = "/"; lhs; rhs; _ } ->
      expr_is_thread_x_coordinate state lhs
      && Option.equal Int.equal
           (target_subgroup_size_value state)
           (int_constant_of_expr state rhs)
  | _ -> false

let expr_is_configured_thread_uniform_coordinate (state : collect_state)
    (expr : D_lang.Expr.t) : bool =
  match variable_of_ident_or_member expr with
  | Some var ->
      Option.is_some (target_subgroup_size_value state)
      && (Variable.equal var Variable.tid_y || Variable.equal var Variable.tid_z)
  | None -> false

let is_explicit_source_uniform_var (state : collect_state) (var : Variable.t) :
    bool =
  Variable.Set.mem var state.uniform_vars
  || Variable.Set.exists
       (fun root ->
         String.starts_with
           ~prefix:(Variable.name root ^ ".")
           (Variable.name var))
       state.uniform_vars

let is_explicit_memory_global_var (state : collect_state) (var : Variable.t) :
    bool =
  Variable.Set.mem var state.memory_globals
  || Variable.Set.exists
       (fun root ->
         String.starts_with
           ~prefix:(Variable.name root ^ ".")
           (Variable.name var))
       state.memory_globals

let call_is_uniform_preserving (state : collect_state) (func : D_lang.Expr.t) :
    bool =
  match call_name func with
  | Some name ->
      StringSet.mem name state.uniform_preserving_calls
      || Functions.supported name || Predicates.supported name
  | None -> false

let call_returns_subgroup_uniform_value (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : bool =
  match (call_name func, args) with
  | ( Some
        ( "warp_sum" | "warp_max" | "warp_reduce_sum" | "warp_reduce_max"
        | "warp_reduce_all" | "warp_reduce_any" ),
      [ _ ] ) ->
      true
  | _ -> false

let rec expr_is_source_uniform (state : collect_state) : D_lang.Expr.t -> bool =
  function
  | CharacterLiteral _ | CXXBoolLiteralExpr _ | FloatingLiteral _
  | IntegerLiteral _ | SizeOfExpr _ ->
      true
  | expr when expr_is_configured_subgroup_id state expr -> true
  | expr when expr_is_configured_thread_uniform_coordinate state expr -> true
  | Ident decl ->
      is_source_uniform_builtin decl.name
      || is_explicit_source_uniform_var state decl.name
  | MemberExpr { base; _ } as expr -> (
      match variable_of_ident_or_member expr with
      | Some var ->
          is_source_uniform_builtin var
          || is_explicit_source_uniform_var state var
      | None -> expr_is_source_uniform state base)
  | BinaryOperator { lhs; rhs; _ }
  | CXXOperatorCallExpr { args = [ lhs; rhs ]; _ } ->
      expr_is_source_uniform state lhs && expr_is_source_uniform state rhs
  | ConditionalOperator { cond; then_expr; else_expr; _ } ->
      expr_is_source_uniform state cond
      && expr_is_source_uniform state then_expr
      && expr_is_source_uniform state else_expr
  | UnaryOperator { child; _ }
  | Convert { arg = child; _ }
  | CXXNewExpr { arg = child; _ }
  | CXXDeleteExpr { arg = child; _ } ->
      expr_is_source_uniform state child
  | CXXConstructExpr { args; _ } ->
      List.for_all (expr_is_source_uniform state) args
  | CallExpr { func; args; _ }
    when call_returns_subgroup_uniform_value func args ->
      true
  | CallExpr { func; args; _ } when call_is_uniform_preserving state func ->
      List.for_all (expr_is_source_uniform state) args
  | CallExpr _ | CXXOperatorCallExpr _ | RecoveryExpr _ | UnresolvedLookupExpr _
    ->
      false

let rec nexp_is_source_uniform (state : collect_state) : Exp.nexp -> bool =
  function
  | Num _ -> true
  | Var x
    when Variable.equal x Variable.tid_x
         || Variable.Set.mem x state.thread_x_coordinate_vars ->
      false
  | Var x
    when Variable.equal x Variable.tid_y || Variable.equal x Variable.tid_z ->
      Option.is_some (target_subgroup_size_value state)
  | Var x ->
      is_source_uniform_builtin x || is_explicit_source_uniform_var state x
  | Binary (Div _, Var x, Num size)
    when Variable.equal x Variable.tid_x
         || Variable.Set.mem x state.thread_x_coordinate_vars ->
      Option.equal Int.equal (target_subgroup_size_value state) (Some size)
  | Binary (Mod _, Var x, _)
    when Variable.equal x Variable.tid_x
         || Variable.Set.mem x state.thread_x_coordinate_vars ->
      false
  | Binary (_, lhs, rhs) ->
      nexp_is_source_uniform state lhs && nexp_is_source_uniform state rhs
  | Unary (_, expr) -> nexp_is_source_uniform state expr
  | NIf (cond, then_expr, else_expr) ->
      bexp_is_source_uniform state cond
      && nexp_is_source_uniform state then_expr
      && nexp_is_source_uniform state else_expr
  | NCall _ | ReadResult _ -> false
  | Convert conversion -> nexp_is_source_uniform state conversion.arg
  | CastInt cond -> bexp_is_source_uniform state cond

and bexp_is_source_uniform (state : collect_state) : Exp.bexp -> bool = function
  | Bool _ -> true
  | NRel (_, lhs, rhs) ->
      nexp_is_source_uniform state lhs && nexp_is_source_uniform state rhs
  | BRel (_, lhs, rhs) ->
      bexp_is_source_uniform state lhs && bexp_is_source_uniform state rhs
  | BNot cond -> bexp_is_source_uniform state cond
  | Pred _ -> false
  | CastBool expr -> nexp_is_source_uniform state expr
  | Distinct exprs -> List.for_all (nexp_is_source_uniform state) exprs
  | AtomicResult _ | IsThreadUnif _ -> false

let control_stack_is_source_uniform (state : collect_state) : bool =
  List.for_all (bexp_is_source_uniform state) state.control_stack

let rec expr_is_memory_global (state : collect_state) : D_lang.Expr.t -> bool =
  function
  | Convert { arg; _ } -> expr_is_memory_global state arg
  | CharacterLiteral _ | CXXBoolLiteralExpr _ | FloatingLiteral _
  | IntegerLiteral _ | SizeOfExpr _ ->
      true
  | Ident decl ->
      is_memory_global_builtin decl.name
      || is_explicit_memory_global_var state decl.name
  | MemberExpr { base; _ } as expr -> (
      match variable_of_ident_or_member expr with
      | Some var ->
          is_memory_global_builtin var
          || is_explicit_memory_global_var state var
      | None -> expr_is_memory_global state base)
  | BinaryOperator { lhs; rhs; _ }
  | CXXOperatorCallExpr { args = [ lhs; rhs ]; _ } ->
      expr_is_memory_global state lhs && expr_is_memory_global state rhs
  | ConditionalOperator { cond; then_expr; else_expr; _ } ->
      expr_is_memory_global state cond
      && expr_is_memory_global state then_expr
      && expr_is_memory_global state else_expr
  | UnaryOperator { child; _ }
  | CXXNewExpr { arg = child; _ }
  | CXXDeleteExpr { arg = child; _ } ->
      expr_is_memory_global state child
  | CXXConstructExpr { args; _ } ->
      List.for_all (expr_is_memory_global state) args
  | CallExpr { func; args; _ } when call_is_uniform_preserving state func ->
      List.for_all (expr_is_memory_global state) args
  | CallExpr _ | CXXOperatorCallExpr _ | RecoveryExpr _ | UnresolvedLookupExpr _
    ->
      false

let init_is_source_uniform (state : collect_state) (init : D_lang.Init.t) : bool
    =
  D_lang.Init.to_exp init |> List.for_all (expr_is_source_uniform state)

let init_is_memory_global (state : collect_state) (init : D_lang.Init.t) : bool
    =
  D_lang.Init.to_exp init |> List.for_all (expr_is_memory_global state)

let add_uniform_var (var : Variable.t) (state : collect_state) : collect_state =
  { state with uniform_vars = Variable.Set.add var state.uniform_vars }

let add_memory_global_var (var : Variable.t) (state : collect_state) :
    collect_state =
  { state with memory_globals = Variable.Set.add var state.memory_globals }

let add_thread_x_coordinate_var (var : Variable.t) (state : collect_state) :
    collect_state =
  {
    state with
    thread_x_coordinate_vars =
      Variable.Set.add var state.thread_x_coordinate_vars;
  }

let add_template_argument_facts (state : collect_state)
    (type_params : D_lang.Ty_param.t list)
    (template_args : C_lang.TemplateArgument.t list) : collect_state =
  let rec add (state : collect_state) params args =
    match (params, args) with
    | ( D_lang.Ty_param.NonTypeTemplate { name; _ } :: params,
        C_lang.TemplateArgument.TArgIntegral value :: args ) ->
        add
          {
            state with
            uniform_vars = Variable.Set.add name state.uniform_vars;
            memory_globals = Variable.Set.add name state.memory_globals;
            constant_values = Variable.Map.add name value state.constant_values;
          }
          params args
    | _ :: params, _ :: args -> add state params args
    | [], _ | _, [] -> state
  in
  add state type_params template_args

let add_constant_value (var : Variable.t) (value : int) (state : collect_state)
    : collect_state =
  {
    state with
    constant_values = Variable.Map.add var value state.constant_values;
  }

let add_numeric_alias (var : Variable.t) (expr : Exp.nexp)
    (state : collect_state) : collect_state =
  {
    state with
    numeric_aliases = Variable.Map.add var expr state.numeric_aliases;
  }

let names_intersect (lhs : Variable.Set.t) (rhs : Variable.Set.t) : bool =
  Variable.Set.exists (fun var -> Variable.Set.mem var rhs) lhs

let pointer_alias_depends_on ~(active_controls : Exp.bexp list)
    (vars : Variable.Set.t) (alias : pointer_alias) : bool =
  let offset_depends =
    Exp.n_free_names alias.offset Variable.Set.empty |> names_intersect vars
  in
  let unprotected_control_depends =
    List.exists
      (fun control ->
        let depends =
          Exp.b_free_names control Variable.Set.empty |> names_intersect vars
        in
        depends && not (List.exists (Exp.b_equal control) active_controls))
      alias.required_controls
  in
  offset_depends || unprotected_control_depends

let pointer_alias_is_available (state : collect_state) (alias : pointer_alias) :
    bool =
  List.for_all
    (fun required -> List.exists (Exp.b_equal required) state.control_stack)
    alias.required_controls

let pointer_aliases_depending_on ~(active_controls : Exp.bexp list)
    (vars : Variable.Set.t) (aliases : pointer_alias Variable.Map.t) :
    Variable.Set.t =
  Variable.Map.fold
    (fun target alias invalidated ->
      if pointer_alias_depends_on ~active_controls vars alias then
        Variable.Set.add target invalidated
      else invalidated)
    aliases Variable.Set.empty

let invalidate_pointer_aliases (invalidated : Variable.Set.t)
    (state : collect_state) : collect_state =
  {
    state with
    pointer_aliases =
      Variable.Map.filter
        (fun alias _ -> not (Variable.Set.mem alias invalidated))
        state.pointer_aliases;
    invalid_pointer_aliases =
      Variable.Set.union invalidated state.invalid_pointer_aliases;
  }

let numeric_aliases_depending_on (var : Variable.t)
    (aliases : Exp.nexp Variable.Map.t) : Variable.Set.t =
  let rec loop invalidated =
    let next =
      Variable.Map.fold
        (fun alias expr invalidated ->
          if Variable.Set.mem alias invalidated then invalidated
          else
            let deps = Exp.n_free_names expr Variable.Set.empty in
            if
              Variable.Set.exists
                (fun dep -> Variable.Set.mem dep invalidated)
                deps
            then Variable.Set.add alias invalidated
            else invalidated)
        aliases invalidated
    in
    if Variable.Set.equal invalidated next then invalidated else loop next
  in
  loop (Variable.Set.singleton var)

let remove_dependent_numeric_aliases (var : Variable.t) (state : collect_state)
    : collect_state =
  let invalidated = numeric_aliases_depending_on var state.numeric_aliases in
  let invalidated_pointer_aliases =
    pointer_aliases_depending_on ~active_controls:state.control_stack
      invalidated state.pointer_aliases
  in
  {
    state with
    numeric_aliases =
      Variable.Map.filter
        (fun alias _ -> not (Variable.Set.mem alias invalidated))
        state.numeric_aliases;
  }
  |> invalidate_pointer_aliases invalidated_pointer_aliases

let remove_numeric_alias (var : Variable.t) (state : collect_state) :
    collect_state =
  remove_dependent_numeric_aliases var state

let remove_scalar_facts (var : Variable.t) (state : collect_state) :
    collect_state =
  let state = remove_dependent_numeric_aliases var state in
  {
    state with
    uniform_vars = Variable.Set.remove var state.uniform_vars;
    memory_globals = Variable.Set.remove var state.memory_globals;
    thread_x_coordinate_vars =
      Variable.Set.remove var state.thread_x_coordinate_vars;
    constant_values = Variable.Map.remove var state.constant_values;
    aggregate_aliases = Variable.Map.remove var state.aggregate_aliases;
  }

let expr_contains_var (var : Variable.t) (expr : Exp.nexp) : bool =
  Exp.n_free_names expr Variable.Set.empty |> Variable.Set.mem var

let numeric_alias_from_expr (var : Variable.t) (rhs : D_lang.Expr.t) :
    Exp.nexp option =
  match nexp_of_expr ~context:"scalar numeric alias" rhs with
  | Ok expr when not (expr_contains_var var expr) -> Some expr
  | Ok _ | Error _ -> None

let update_scalar_facts_from_expr (state : collect_state) (var : Variable.t)
    (rhs : D_lang.Expr.t) : collect_state =
  let control_is_uniform = control_stack_is_source_uniform state in
  let is_thread_x = expr_is_thread_x_coordinate state rhs in
  let constant_value = int_constant_of_expr state rhs in
  let is_uniform = expr_is_source_uniform state rhs in
  let is_memory_global = expr_is_memory_global state rhs in
  let numeric_alias =
    match numeric_alias_from_expr var rhs with
    | Some expr -> Some expr
    | None -> Option.map (fun value -> Exp.Num value) constant_value
  in
  let state = remove_scalar_facts var state in
  let state =
    match numeric_alias with
    | Some expr -> add_numeric_alias var expr state
    | None -> state
  in
  let state =
    if is_memory_global then add_memory_global_var var state else state
  in
  if not control_is_uniform then state
  else
    let state =
      if is_thread_x then add_thread_x_coordinate_var var state else state
    in
    let state =
      match constant_value with
      | Some value -> add_constant_value var value state
      | None -> state
    in
    let state = if is_uniform then add_uniform_var var state else state in
    state

let update_scalar_facts_from_init (state : collect_state) (decl : D_lang.Decl.t)
    : collect_state =
  let control_is_uniform = control_stack_is_source_uniform state in
  let is_uniform =
    match decl.init with
    | Some init -> init_is_source_uniform state init
    | None -> false
  in
  let is_memory_global =
    match decl.init with
    | Some init -> init_is_memory_global state init
    | None -> false
  in
  match decl.init with
  | Some (IExpr rhs) -> update_scalar_facts_from_expr state decl.var rhs
  | Some _ ->
      let state = remove_scalar_facts decl.var state in
      let state =
        if is_memory_global then add_memory_global_var decl.var state else state
      in
      let state =
        if control_is_uniform && is_uniform then add_uniform_var decl.var state
        else state
      in
      state
  | None -> remove_scalar_facts decl.var state

let add_decl_facts (state : collect_state) (decl : D_lang.Decl.t) :
    collect_state =
  update_scalar_facts_from_init state decl

let decl_is_thread_private_array (decl : D_lang.Decl.t) : bool =
  let ty = decl.ty in
  Ty.is_array ty
  && (not (List.mem C_lang.c_attr_shared decl.attrs))
  && Option.is_none (Variable.label_opt decl.var)

let add_local_decl_facts (state : collect_state) (decl : D_lang.Decl.t) :
    collect_state =
  let state = add_decl_facts state decl in
  if decl_is_thread_private_array decl then
    {
      state with
      private_arrays = Variable.Set.add decl.var state.private_arrays;
    }
  else state

let access_array_is_private (state : collect_state) (array : Variable.t) : bool
    =
  Variable.Set.mem array state.private_arrays
  || Variable.Set.exists
       (fun root ->
         String.starts_with
           ~prefix:(Variable.name root ^ ".")
           (Variable.name array))
       state.private_arrays

let resolve_type_alias (state : collect_state) (ty : Ty.t) : Ty.t =
  StringMap.find_opt (Ty.to_string ty) state.type_aliases
  |> Option.value ~default:ty

let expr_type_to_string (state : collect_state) (expr : D_lang.Expr.t) : string
    =
  D_lang.Expr.to_type expr |> resolve_type_alias state |> Ty.unnamed
  |> Ty.to_string

let add_pointer_alias (target : Variable.t) (alias : pointer_alias)
    (state : collect_state) : collect_state =
  {
    state with
    pointer_aliases = Variable.Map.add target alias state.pointer_aliases;
    invalid_pointer_aliases =
      Variable.Set.remove target state.invalid_pointer_aliases;
  }

let add_aggregate_alias (target : Variable.t) (alias : aggregate_alias)
    (state : collect_state) : collect_state =
  {
    state with
    aggregate_aliases = Variable.Map.add target alias state.aggregate_aliases;
  }

let qualify_aggregate_field (field : string) (alias : aggregate_alias) :
    aggregate_alias =
  {
    alias with
    aggregate_array =
      Variable.update_name
        (fun name -> name ^ "." ^ field)
        alias.aggregate_array;
  }

let add_aggregate_member_alias_from_decl (state : collect_state)
    (decl : D_lang.Decl.t) : collect_state =
  match decl.init with
  | Some (IExpr (MemberExpr { base = Ident base; name = field; _ })) -> (
      match Variable.Map.find_opt base.name state.aggregate_aliases with
      | Some alias ->
          add_aggregate_alias decl.var
            (qualify_aggregate_field field alias)
            state
      | None -> state)
  | Some _ | None -> state

let rec pointer_base_offset (state : collect_state) ~(context : string)
    (expr : D_lang.Expr.t) : (Variable.t * Exp.nexp, error) result =
  let ( let* ) = Result.bind in
  let pointer_variable var =
    if Variable.Set.mem var state.invalid_pointer_aliases then
      Error
        (Unsupported_expression
           {
             context = context ^ " invalidated pointer alias";
             expr = expr_to_string expr;
           })
    else
      match Variable.Map.find_opt var state.pointer_aliases with
      | Some alias when pointer_alias_is_available state alias ->
          Ok (alias.source, alias.offset)
      | Some _ ->
          Error
            (Unsupported_expression
               {
                 context = context ^ " invalidated pointer alias";
                 expr = expr_to_string expr;
               })
      | None -> Ok (var, Exp.Num 0)
  in
  match expr with
  | Ident decl -> pointer_variable decl.name
  | MemberExpr _ -> (
      match variable_of_ident_or_member expr with
      | Some var -> pointer_variable var
      | None -> unsupported_expr ~context expr)
  | UnaryOperator { opcode = "&"; child = Ident decl; _ } ->
      Ok (decl.name, Exp.Num 0)
  | BinaryOperator { opcode = "+"; lhs; rhs; _ } ->
      if expr_is_pointer_like lhs then
        let* base, offset = pointer_base_offset state ~context lhs in
        let* rhs = nexp_of_expr ~context rhs in
        Ok (base, Exp.n_plus offset rhs)
      else if expr_is_pointer_like rhs then
        let* base, offset = pointer_base_offset state ~context rhs in
        let* lhs = nexp_of_expr ~context lhs in
        Ok (base, Exp.n_plus lhs offset)
      else unsupported_expr ~context expr
  | BinaryOperator { opcode = "-"; lhs; rhs; _ } ->
      if expr_is_pointer_like lhs then
        let* base, offset = pointer_base_offset state ~context lhs in
        let* rhs = nexp_of_expr ~context rhs in
        Ok (base, Exp.n_minus offset rhs)
      else unsupported_expr ~context expr
  | CXXOperatorCallExpr { func; args = [ lhs; rhs ]; _ }
    when Option.equal String.equal (call_name func) (Some "operator+") ->
      pointer_base_offset state ~context
        (BinaryOperator
           { opcode = "+"; lhs; rhs; ty = D_lang.Expr.to_type expr })
  | ConditionalOperator _ ->
      Error
        (Unsupported_expression
           { context = context ^ " pointer alias"; expr = expr_to_string expr })
  | _ -> unsupported_expr ~context expr

let access_of_pointer (state : collect_state) ~(mode : Access.Mode.t)
    ~(context : string) (expr : D_lang.Expr.t) : (Access.t, error) result =
  let ( let* ) = Result.bind in
  let* base, offset = pointer_base_offset state ~context expr in
  let index = match offset with Exp.Num 0 -> [] | offset -> [ offset ] in
  match mode with
  | Read -> Ok (Access.read base index)
  | Write payload -> Ok (Access.write base index payload)
  | Atomic _ -> unsupported_expr ~context expr

let find_substring ~(needle : string) (text : string) : int option =
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec loop offset =
    if offset + needle_len > text_len then None
    else if String.sub text offset needle_len = needle then Some offset
    else loop (offset + 1)
  in
  loop 0

let fragment_args (state : collect_state) (expr : D_lang.Expr.t) :
    (string list, error) result =
  let ty = expr_type_to_string state expr in
  match find_substring ~needle:"fragment<" ty with
  | None ->
      Error
        (Unsupported_matrix_call
           {
             op = "wmma";
             reason = "expected an nvcuda::wmma::fragment argument";
             expr = expr_to_string expr;
           })
  | Some start -> (
      let args_start = start + String.length "fragment<" in
      try
        let args_end = String.rindex ty '>' in
        let args = String.sub ty args_start (args_end - args_start) in
        Ok (args |> String.split_on_char ',' |> List.map String.trim)
      with Invalid_argument _ | Not_found ->
        Error
          (Unsupported_matrix_call
             {
               op = "wmma";
               reason = "could not parse fragment template arguments";
               expr = expr_to_string expr;
             }))

let fragment_dim_to_expr (dim : string) : Exp.nexp =
  match int_of_string_opt dim with
  | Some value -> Exp.Num value
  | None -> Exp.Var (Variable.from_name dim)

let parse_fragment_shape (state : collect_state) (expr : D_lang.Expr.t) :
    (Exp.nexp * Exp.nexp, error) result =
  let ( let* ) = Result.bind in
  let* args = fragment_args state expr in
  match args with
  | _role :: rows :: cols :: _ ->
      Ok (fragment_dim_to_expr rows, fragment_dim_to_expr cols)
  | _ ->
      Error
        (Unsupported_matrix_call
           {
             op = "wmma";
             reason = "fragment type does not expose row/column dimensions";
             expr = expr_to_string expr;
           })

let matrix_layout (state : collect_state) (layout_sources : D_lang.Expr.t list)
    (fragment : D_lang.Expr.t) : (SM.Matrix.layout, error) result =
  let text =
    expr_type_to_string state fragment :: List.map expr_to_string layout_sources
    |> String.concat " "
  in
  if Stage0.Common.contains ~substring:"col_major" text then
    Ok SM.Matrix.Col_major
  else if Stage0.Common.contains ~substring:"row_major" text then
    Ok SM.Matrix.Row_major
  else
    Error
      (Unsupported_matrix_call
         {
           op = "wmma";
           reason =
             "matrix layout must be explicit in the fragment type or call";
           expr = expr_to_string fragment;
         })

let matrix_footprint (state : collect_state) ~(site : SM.Site.t)
    ~(mode : Access.Mode.t) ~(fragment : D_lang.Expr.t)
    ~(pointer : D_lang.Expr.t) ~(leading_dimension : D_lang.Expr.t)
    ~(layout_sources : D_lang.Expr.t list) : (SM.Matrix.footprint, error) result
    =
  let ( let* ) = Result.bind in
  let* rows, cols = parse_fragment_shape state fragment in
  let* leading_dimension =
    nexp_of_expr ~context:"matrix leading dimension" leading_dimension
  in
  let* layout = matrix_layout state layout_sources fragment in
  let* base = access_of_pointer state ~mode ~context:"matrix pointer" pointer in
  SM.Matrix.rectangular ~base ~rows ~cols ~leading_dimension ~layout
    ~row:(Variable.from_name ("matrix_row_" ^ string_of_int (SM.Site.id site)))
    ~col:(Variable.from_name ("matrix_col_" ^ string_of_int (SM.Site.id site)))
  |> Result.map_error (fun reason ->
      Unsupported_matrix_call
        { op = "wmma"; reason; expr = expr_to_string pointer })

let empty_collect_state ~(kernel_name : string)
    ~(target_config : SM.Target_config.t) ~(uniform_vars : Variable.Set.t)
    ~(uniform_preserving_calls : StringSet.t)
    ~(helper_kernels : D_lang.Kernel.t list StringMap.t)
    ~(subgroup_helpers : StringSet.t)
    ~(template_args : C_lang.TemplateArgument.t list)
    (type_aliases : Ty.t StringMap.t) : collect_state =
  {
    kernel_name;
    next_site_id = 0;
    next_memory_site_id = 0;
    next_source_order = 0;
    stmts_rev = [];
    site_controls_rev = [];
    ordinary_memory_effects_rev = [];
    site_source_orders = IntMap.empty;
    control_stack = [];
    workgroup_phase = 0;
    subgroup_phase = [];
    target_config;
    uniform_vars;
    memory_globals = uniform_vars;
    thread_x_coordinate_vars = Variable.Set.singleton Variable.tid_x;
    constant_values = Variable.Map.empty;
    numeric_aliases = Variable.Map.empty;
    pointer_aliases = Variable.Map.empty;
    aggregate_aliases = Variable.Map.empty;
    invalid_pointer_aliases = Variable.Set.empty;
    private_arrays = Variable.Set.empty;
    uniform_preserving_calls;
    launch_preconditions_rev = [];
    launch_dimensions = Variable.Map.empty;
    type_aliases;
    helper_kernels;
    subgroup_helpers;
    active_helpers = StringSet.empty;
    current_template_args = template_args;
    call_result = None;
  }

let make_site (state : collect_state) ?location ~(label : string) () :
    SM.Site.t * collect_state =
  let source_order = state.next_source_order in
  let site = SM.Site.make ?location ~label state.next_site_id in
  ( site,
    {
      state with
      next_site_id = state.next_site_id + 1;
      next_source_order = source_order + 1;
      site_source_orders =
        IntMap.add (SM.Site.id site) source_order state.site_source_orders;
    } )

let add_stmt (stmt : SM.Stmt.t) (state : collect_state) : collect_state =
  { state with stmts_rev = stmt :: state.stmts_rev }

let enter_workgroup_phase (state : collect_state) : collect_state =
  { state with workgroup_phase = state.workgroup_phase + 1 }

let enter_subgroup_phase (site : SM.Site.t) (state : collect_state) :
    collect_state =
  { state with subgroup_phase = state.subgroup_phase @ [ SM.Site.id site ] }

let record_site_control ?memory_conditions (site : SM.Site.t)
    (state : collect_state) : collect_state =
  let site_id = SM.Site.id site in
  let source_order =
    match IntMap.find_opt site_id state.site_source_orders with
    | Some source_order -> source_order
    | None -> failwith "missing source order for subgroup/matrix site"
  in
  let conditions = List.rev state.control_stack in
  let memory_conditions =
    List.rev state.launch_preconditions_rev
    @ Option.value memory_conditions ~default:conditions
  in
  {
    state with
    site_controls_rev =
      {
        site_id;
        source_order;
        conditions;
        memory_conditions;
        uniform_vars = state.uniform_vars;
        numeric_aliases = state.numeric_aliases;
      }
      :: state.site_controls_rev;
  }

let make_ordinary_memory_site (state : collect_state)
    ~(kind : ordinary_memory_kind) ~(location : Stage0.Location.t) :
    ordinary_memory_site * collect_state =
  let source_order = state.next_source_order in
  let site =
    {
      id = state.next_memory_site_id;
      source_order;
      label = "ordinary_" ^ ordinary_memory_kind_to_string kind;
      location = location_opt location;
    }
  in
  ( site,
    {
      state with
      next_memory_site_id = state.next_memory_site_id + 1;
      next_source_order = source_order + 1;
    } )

let map_result_list (f : 'a -> ('b, 'e) result) (values : 'a list) :
    ('b list, 'e) result =
  List.fold_right
    (fun value values ->
      let ( let* ) = Result.bind in
      let* value = f value in
      let* values = values in
      Ok (value :: values))
    values (Ok [])

let rec constant_nexp_value (state : collect_state) (expr : Exp.nexp) :
    int option =
  let binary f lhs rhs =
    match (constant_nexp_value state lhs, constant_nexp_value state rhs) with
    | Some lhs, Some rhs -> f lhs rhs
    | _ -> None
  in
  match expr with
  | Exp.Num value -> Some value
  | Exp.Var var -> Variable.Map.find_opt var state.constant_values
  | Exp.Unary (N_unary.Negate, expr) ->
      constant_nexp_value state expr |> Option.map Int.neg
  | Exp.Unary _ -> None
  | Exp.Binary (N_binary.Plus _, lhs, rhs) ->
      binary (fun lhs rhs -> Some (lhs + rhs)) lhs rhs
  | Exp.Binary (N_binary.Minus _, lhs, rhs) ->
      binary (fun lhs rhs -> Some (lhs - rhs)) lhs rhs
  | Exp.Binary (N_binary.Mult _, lhs, rhs) ->
      binary (fun lhs rhs -> Some (lhs * rhs)) lhs rhs
  | Exp.Binary (N_binary.Div _, lhs, rhs) ->
      binary (fun lhs rhs -> if rhs = 0 then None else Some (lhs / rhs)) lhs rhs
  | Exp.Binary (N_binary.Mod _, lhs, rhs) ->
      binary
        (fun lhs rhs ->
          if rhs = 0 then None else Some (Stage0.Common.modulo lhs rhs))
        lhs rhs
  | Exp.Convert { arg; ty } ->
      Option.bind (constant_nexp_value state arg) (fun value ->
          Scalar.reduce value ty)
  | Exp.Binary _ | Exp.NIf _ | Exp.NCall _ | Exp.CastInt _ | Exp.ReadResult _ ->
      None

let low_bit_mask_modulus (value : int) : int option =
  if value < 0 || value = Int.max_int then None
  else
    let modulus = value + 1 in
    if modulus land value = 0 then Some modulus else None

let rec normalize_solver_nexp (state : collect_state) (expr : Exp.nexp) :
    Exp.nexp =
  let normalize = normalize_solver_nexp state in
  match expr with
  | Exp.Num _ | Exp.Var _ -> expr
  | Exp.Unary (op, expr) -> Exp.Unary (op, normalize expr)
  | Exp.Binary (N_binary.BitAnd, lhs, rhs) ->
      let lhs = normalize lhs in
      let rhs = normalize rhs in
      begin match
        ( Option.bind (constant_nexp_value state lhs) low_bit_mask_modulus,
          Option.bind (constant_nexp_value state rhs) low_bit_mask_modulus )
      with
      | _, Some modulus -> Exp.n_mod lhs (Exp.Num modulus)
      | Some modulus, None -> Exp.n_mod rhs (Exp.Num modulus)
      | None, None -> Exp.Binary (N_binary.BitAnd, lhs, rhs)
      end
  | Exp.Binary (op, lhs, rhs) -> Exp.Binary (op, normalize lhs, normalize rhs)
  | Exp.NIf (cond, then_expr, else_expr) ->
      Exp.NIf
        ( normalize_solver_bexp state cond,
          normalize then_expr,
          normalize else_expr )
  | Exp.NCall (name, args) -> Exp.NCall (name, List.map normalize args)
  | Exp.ReadResult read ->
      Exp.ReadResult { read with args = List.map normalize read.args }
  | Exp.Convert conversion ->
      Exp.Convert { conversion with arg = normalize conversion.arg }
  | Exp.CastInt cond -> Exp.CastInt (normalize_solver_bexp state cond)

and normalize_solver_bexp (state : collect_state) (condition : Exp.bexp) :
    Exp.bexp =
  let normalize_n = normalize_solver_nexp state in
  let normalize_b = normalize_solver_bexp state in
  match condition with
  | Exp.Bool _ -> condition
  | Exp.NRel (op, lhs, rhs) -> Exp.NRel (op, normalize_n lhs, normalize_n rhs)
  | Exp.BRel (op, lhs, rhs) -> Exp.BRel (op, normalize_b lhs, normalize_b rhs)
  | Exp.BNot condition -> Exp.BNot (normalize_b condition)
  | Exp.Pred (name, args) -> Exp.Pred (name, List.map normalize_n args)
  | Exp.CastBool expr -> Exp.CastBool (normalize_n expr)
  | Exp.Distinct exprs -> Exp.Distinct (List.map normalize_n exprs)
  | Exp.AtomicResult { target; array; index; operation } ->
      Exp.AtomicResult
        {
          target;
          array;
          index = List.map normalize_n index;
          operation = Atomic.Operation.map normalize_n operation;
        }
  | Exp.IsThreadUnif expr -> Exp.IsThreadUnif (normalize_n expr)

let access_of_subscript (state : collect_state) ~(mode : Access.Mode.t)
    ~(context : string) (subscript : D_lang.d_subscript) :
    (Access.t, error) result =
  let ( let* ) = Result.bind in
  let* index =
    map_result_list (nexp_of_expr ~context) (D_lang.subscript_index subscript)
  in
  let index = List.map (normalize_solver_nexp state) index in
  match
    Variable.Map.find_opt
      (D_lang.subscript_name subscript)
      state.aggregate_aliases
  with
  | Some alias ->
      Ok
        {
          Access.array = alias.aggregate_array;
          id = Access.Id.unstamped;
          index = alias.aggregate_index @ index;
          mode;
        }
  | None -> (
      if
        Variable.Set.mem
          (D_lang.subscript_name subscript)
          state.invalid_pointer_aliases
      then
        Error
          (Unsupported_expression
             {
               context;
               expr =
                 D_lang.subscript_to_s subscript
                 ^ " (pointer alias is path-dependent or invalidated)";
             })
      else
        match
          Variable.Map.find_opt
            (D_lang.subscript_name subscript)
            state.pointer_aliases
        with
        | None ->
            Ok
              (Access.make
                 ~array:(D_lang.subscript_name subscript)
                 ~index ~mode)
        | Some alias when not (pointer_alias_is_available state alias) ->
            Error
              (Unsupported_expression
                 {
                   context;
                   expr =
                     D_lang.subscript_to_s subscript
                     ^ " (pointer alias is path-dependent or invalidated)";
                 })
        | Some alias -> (
            match (alias.offset, index) with
            | Exp.Num 0, _ -> Ok (Access.make ~array:alias.source ~index ~mode)
            | offset, [ linear ] ->
                Ok
                  {
                    Access.array = alias.source;
                    id = Access.Id.unstamped;
                    index = [ Exp.n_plus linear offset ];
                    mode;
                  }
            | _, _ ->
                Error
                  (Unsupported_expression
                     {
                       context;
                       expr =
                         D_lang.subscript_to_s subscript
                         ^ " (non-zero pointer offset on a non-linear \
                            subscript)";
                     })))

let condition_free_names (conditions : Exp.bexp list) : Variable.Set.t =
  List.fold_left
    (fun names condition -> Exp.b_free_names condition names)
    Variable.Set.empty conditions

let access_definedness_conditions (access : Access.t) : Exp.bexp list =
  List.concat_map Exp.n_definedness_conditions access.index

let conditions_definedness_conditions (conditions : Exp.bexp list) :
    Exp.bexp list =
  List.concat_map Exp.b_definedness_conditions conditions

let relevant_numeric_alias_conditions (state : collect_state)
    (access : Access.t) (conditions : Exp.bexp list) : Exp.bexp list =
  let initial_names =
    Access.free_names access Variable.Set.empty
    |> Variable.Set.union (condition_free_names conditions)
  in
  let rec collect seen pending conditions =
    match pending with
    | [] -> List.rev conditions
    | var :: rest when Variable.Set.mem var seen -> collect seen rest conditions
    | var :: rest -> (
        let seen = Variable.Set.add var seen in
        match Variable.Map.find_opt var state.numeric_aliases with
        | None -> collect seen rest conditions
        | Some expr ->
            let dependencies =
              Exp.n_free_names expr Variable.Set.empty |> Variable.Set.elements
            in
            let facts =
              Exp.n_eq (Exp.Var var) expr :: Exp.n_definedness_conditions expr
            in
            collect seen (dependencies @ rest)
              (List.rev_append facts conditions))
  in
  collect Variable.Set.empty (Variable.Set.elements initial_names) []
  |> Exp.dedup_conditions

let record_ordinary_memory_effect ~(kind : ordinary_memory_kind)
    ~(mode : Access.Mode.t) ~(context : string) ?guard
    (subscript : D_lang.d_subscript) (state : collect_state) :
    (collect_state, error) result =
  let ( let* ) = Result.bind in
  let* access = access_of_subscript state ~mode ~context subscript in
  let* access_guard =
    match guard with
    | None -> Ok []
    | Some guard ->
        let* guard = bexp_of_expr ~context:(context ^ " guard") guard in
        Ok [ normalize_solver_bexp state guard ]
  in
  if access_array_is_private state access.array then Ok state
  else
    let memory_conditions =
      access_guard
      @ (List.rev state.launch_preconditions_rev @ List.rev state.control_stack
        |> List.map (normalize_solver_bexp state))
    in
    let alias_conditions =
      relevant_numeric_alias_conditions state access memory_conditions
      |> List.map (normalize_solver_bexp state)
    in
    let definedness_conditions =
      access_definedness_conditions access
      @ conditions_definedness_conditions memory_conditions
    in
    let site, state =
      make_ordinary_memory_site state ~kind ~location:subscript.location
    in
    let memory_effect =
      {
        kind;
        site;
        access;
        source_conditions =
          Exp.dedup_conditions
            (alias_conditions @ definedness_conditions @ memory_conditions);
        runtime_condition = None;
        phase =
          { workgroup = state.workgroup_phase; subgroup = state.subgroup_phase };
        target_config = state.target_config;
      }
    in
    Ok
      {
        state with
        ordinary_memory_effects_rev =
          memory_effect :: state.ordinary_memory_effects_rev;
      }

let atomic_operation_of_source ~(context : string)
    (atomic : D_lang.Expr.t Atomic.t) : (Exp.nexp Atomic.t, error) result =
  let ( let* ) = Result.bind in
  let map_operand = function
    | None -> Ok None
    | Some expr ->
        let* expr = nexp_of_expr ~context expr in
        Ok (Some expr)
  in
  let* operation =
    match atomic.operation with
    | Atomic.Operation.Add expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Add expr)
    | Atomic.Operation.Sub expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Sub expr)
    | Atomic.Operation.Inc expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Inc expr)
    | Atomic.Operation.Dec expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Dec expr)
    | Atomic.Operation.And expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.And expr)
    | Atomic.Operation.Or expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Or expr)
    | Atomic.Operation.Xor expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Xor expr)
    | Atomic.Operation.Min expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Min expr)
    | Atomic.Operation.Max expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Max expr)
    | Atomic.Operation.Exch expr ->
        let* expr = map_operand expr in
        Ok (Atomic.Operation.Exch expr)
    | Atomic.Operation.CAS { expected; new_val } ->
        let* expected = map_operand expected in
        let* new_val = map_operand new_val in
        Ok (Atomic.Operation.CAS { expected; new_val })
  in
  Ok { atomic with operation }

let wmma_stmt (state : collect_state) (kind : D_lang.Wmma_call.kind)
    (func : D_lang.Expr.t) (args : D_lang.Expr.t list) :
    (collect_state, error) result =
  let ( let* ) = Result.bind in
  let label = D_lang.Wmma_call.to_string kind in
  let site, state = make_site state ?location:(call_location func) ~label () in
  let expr = D_lang.Expr.CallExpr { func; args; ty = J_type.void } in
  let require_fragment op fragment =
    fragment_args state fragment
    |> Result.map (fun _ -> ())
    |> Result.map_error (fun error ->
        match error with
        | Unsupported_matrix_call data ->
            Unsupported_matrix_call { data with op; expr = expr_to_string expr }
        | error -> error)
  in
  let* collective =
    match (kind, args) with
    | Fill_fragment, [ fragment; _value ] ->
        let* () = require_fragment label fragment in
        Ok (SM.Matrix.fill_fragment site)
    | Mma_sync, [ dst; left; right; accum ] ->
        let* () = require_fragment label dst in
        let* () = require_fragment label left in
        let* () = require_fragment label right in
        let* () = require_fragment label accum in
        Ok (SM.Matrix.mma_sync site)
    | Load_matrix_sync, [ fragment; pointer; leading_dimension ] ->
        let* footprint =
          matrix_footprint state ~site ~mode:Read ~fragment ~pointer
            ~leading_dimension ~layout_sources:[]
        in
        SM.Matrix.load_matrix_sync site footprint
        |> map_matrix_error ~op:label ~expr
    | Store_matrix_sync, [ pointer; fragment; leading_dimension; layout ] ->
        let* footprint =
          matrix_footprint state ~site ~mode:(Write None) ~fragment ~pointer
            ~leading_dimension ~layout_sources:[ layout ]
        in
        SM.Matrix.store_matrix_sync site footprint
        |> map_matrix_error ~op:label ~expr
    | _ ->
        Error
          (Unsupported_matrix_call
             {
               op = label;
               reason = "unexpected WMMA argument shape";
               expr = expr_to_string expr;
             })
  in
  let memory_conditions =
    match collective.memory with
    | None -> None
    | Some memory ->
        let access =
          memory |> SM.Matrix.memory_effect_footprint
          |> SM.Matrix.indexed_access
        in
        let memory_conditions =
          List.rev state.launch_preconditions_rev @ List.rev state.control_stack
        in
        let definedness_conditions =
          access_definedness_conditions access
          @ conditions_definedness_conditions memory_conditions
        in
        Some
          (Exp.dedup_conditions
             (relevant_numeric_alias_conditions state access memory_conditions
             @ definedness_conditions @ memory_conditions))
  in
  let state = record_site_control ?memory_conditions site state in
  Ok (add_stmt (SM.Stmt.Matrix_collective collective) state)

let full_subgroup_mask (state : collect_state) (expr : D_lang.Expr.t) : bool =
  match (target_subgroup_size_value state, int_constant_of_expr state expr) with
  | Some size, Some mask when size < Sys.int_size - 1 ->
      Int.equal mask (-1) || Int.equal mask ((1 lsl size) - 1)
  | Some _, Some -1 -> true
  | Some _, Some _ | Some _, None | None, _ -> false

let subgroup_barrier_stmt (state : collect_state) (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : (collect_state, error) result =
  let expr = D_lang.Expr.CallExpr { func; args; ty = J_type.void } in
  match args with
  | [] ->
      let site, state =
        make_site state ?location:(call_location func) ~label:"__syncwarp" ()
      in
      let state = record_site_control site state in
      Ok
        (state
        |> add_stmt (SM.Stmt.subgroup_barrier site)
        |> enter_subgroup_phase site)
  | [ mask ] when full_subgroup_mask state mask ->
      let site, state =
        make_site state ?location:(call_location func) ~label:"__syncwarp" ()
      in
      let state = record_site_control site state in
      Ok
        (state
        |> add_stmt (SM.Stmt.subgroup_barrier site)
        |> enter_subgroup_phase site)
  | [ _ ] ->
      Error
        (Unsupported_matrix_call
           {
             op = "__syncwarp";
             reason =
               "subgroup ordering requires a statically full participation mask";
             expr = expr_to_string expr;
           })
  | _ ->
      Error
        (Unsupported_matrix_call
           {
             op = "__syncwarp";
             reason = "unexpected subgroup barrier argument shape";
             expr = expr_to_string expr;
           })

type subgroup_collective_call =
  | Warp_sum
  | Warp_max
  | Warp_reduce_sum
  | Warp_reduce_max
  | Warp_reduce_all
  | Warp_reduce_any
  | Shfl_sync
  | Shfl_down_sync
  | Shfl_up_sync
  | Shfl_xor_sync

let subgroup_collective_call_name : subgroup_collective_call -> string =
  function
  | Warp_sum -> "warp_sum"
  | Warp_max -> "warp_max"
  | Warp_reduce_sum -> "warp_reduce_sum"
  | Warp_reduce_max -> "warp_reduce_max"
  | Warp_reduce_all -> "warp_reduce_all"
  | Warp_reduce_any -> "warp_reduce_any"
  | Shfl_sync -> "__shfl_sync"
  | Shfl_down_sync -> "__shfl_down_sync"
  | Shfl_up_sync -> "__shfl_up_sync"
  | Shfl_xor_sync -> "__shfl_xor_sync"

let subgroup_collective_call_of_call (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : subgroup_collective_call option =
  match (call_name func, List.length args) with
  | Some "warp_sum", 1 -> Some Warp_sum
  | Some "warp_max", 1 -> Some Warp_max
  | Some "warp_reduce_sum", 1 -> Some Warp_reduce_sum
  | Some "warp_reduce_max", 1 -> Some Warp_reduce_max
  | Some "warp_reduce_all", 1 -> Some Warp_reduce_all
  | Some "warp_reduce_any", 1 -> Some Warp_reduce_any
  | Some "__shfl_sync", (3 | 4) -> Some Shfl_sync
  | Some "__shfl_down_sync", (3 | 4) -> Some Shfl_down_sync
  | Some "__shfl_up_sync", (3 | 4) -> Some Shfl_up_sync
  | Some "__shfl_xor_sync", (3 | 4) -> Some Shfl_xor_sync
  | _ -> None

let subgroup_collective_result (site : SM.Site.t) : Variable.t =
  Variable.from_name
    ("__subgroup_collective_result_" ^ string_of_int (SM.Site.id site))

let subgroup_collective_operand ~(context : string) (expr : D_lang.Expr.t) :
    (SM.Collective.operand, error) result =
  nexp_of_expr ~context expr
  |> Result.map (fun expr -> SM.Collective.Numeric expr)

let subgroup_collective_predicate_operand ~(context : string)
    (expr : D_lang.Expr.t) : (SM.Collective.operand, error) result =
  bexp_of_expr ~context expr |> Result.map (fun expr -> SM.Collective.Bool expr)

let shuffle_operands (state : collect_state) ~(label : string)
    ~(rendered_expr : string) (args : D_lang.Expr.t list) :
    (D_lang.Expr.t * D_lang.Expr.t, error) result =
  let fail reason =
    Error (Unsupported_matrix_call { op = label; reason; expr = rendered_expr })
  in
  match args with
  | [ mask; value; selector ] | [ mask; value; selector; _ ] ->
      if full_subgroup_mask state mask then Ok (value, selector)
      else
        fail "subgroup ordering requires a statically full participation mask"
  | _ -> fail "unexpected subgroup shuffle argument shape"

let subgroup_collective_stmt ?result (state : collect_state)
    (op : subgroup_collective_call) (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : (collect_state, error) result =
  let ( let* ) = Result.bind in
  let label = subgroup_collective_call_name op in
  let expr = D_lang.Expr.CallExpr { func; args; ty = J_type.void } in
  let site, state = make_site state ?location:(call_location func) ~label () in
  let result = Option.value result ~default:(subgroup_collective_result site) in
  let unexpected_shape () =
    Error
      (Unsupported_matrix_call
         {
           op = label;
           reason = "unexpected subgroup collective argument shape";
           expr = expr_to_string expr;
         })
  in
  let* payload =
    match (op, args) with
    | (Warp_sum | Warp_reduce_sum), [ value ] ->
        let* argument =
          subgroup_collective_operand ~context:(label ^ " operand") value
        in
        Ok
          (SM.Collective.Operation_payload
             {
               op = SM.Collective.Add;
               collective_op = SM.Collective.Reduce;
               argument;
               result;
             })
    | (Warp_max | Warp_reduce_max), [ value ] ->
        let* argument =
          subgroup_collective_operand ~context:(label ^ " operand") value
        in
        Ok
          (SM.Collective.Operation_payload
             {
               op = SM.Collective.Max;
               collective_op = SM.Collective.Reduce;
               argument;
               result;
             })
    | (Warp_reduce_all | Warp_reduce_any), [ value ] ->
        let* argument =
          subgroup_collective_predicate_operand ~context:(label ^ " operand")
            value
        in
        let op =
          match op with
          | Warp_reduce_all -> SM.Collective.All
          | Warp_reduce_any -> SM.Collective.Any
          | _ -> failwith "subgroup reduction operation accounting"
        in
        Ok
          (SM.Collective.Operation_payload
             { op; collective_op = SM.Collective.Reduce; argument; result })
    | Shfl_sync, args ->
        let* value, lane =
          shuffle_operands state ~label ~rendered_expr:(expr_to_string expr)
            args
        in
        let* lane = nexp_of_expr ~context:"__shfl_sync lane" lane in
        let* argument =
          subgroup_collective_operand ~context:"__shfl_sync operand" value
        in
        Ok
          (SM.Collective.Gather_payload
             { mode = SM.Collective.Broadcast lane; argument; result })
    | Shfl_down_sync, args ->
        let* value, delta =
          shuffle_operands state ~label ~rendered_expr:(expr_to_string expr)
            args
        in
        let* delta = nexp_of_expr ~context:"__shfl_down_sync delta" delta in
        let* argument =
          subgroup_collective_operand ~context:"__shfl_down_sync operand" value
        in
        Ok
          (SM.Collective.Gather_payload
             { mode = SM.Collective.Shuffle_down delta; argument; result })
    | Shfl_up_sync, args ->
        let* value, delta =
          shuffle_operands state ~label ~rendered_expr:(expr_to_string expr)
            args
        in
        let* delta = nexp_of_expr ~context:"__shfl_up_sync delta" delta in
        let* argument =
          subgroup_collective_operand ~context:"__shfl_up_sync operand" value
        in
        Ok
          (SM.Collective.Gather_payload
             { mode = SM.Collective.Shuffle_up delta; argument; result })
    | Shfl_xor_sync, args ->
        let* value, lane_mask =
          shuffle_operands state ~label ~rendered_expr:(expr_to_string expr)
            args
        in
        let* lane_mask =
          nexp_of_expr ~context:"__shfl_xor_sync lane mask" lane_mask
        in
        let* argument =
          subgroup_collective_operand ~context:"__shfl_xor_sync operand" value
        in
        Ok
          (SM.Collective.Gather_payload
             { mode = SM.Collective.Shuffle_xor lane_mask; argument; result })
    | _ -> unexpected_shape ()
  in
  let state = record_site_control site state in
  let state =
    add_stmt
      (SM.Stmt.Subgroup_collective (SM.Collective.make site payload))
      state
  in
  let state =
    match op with
    | Warp_sum | Warp_max | Warp_reduce_sum | Warp_reduce_max | Warp_reduce_all
    | Warp_reduce_any ->
        add_uniform_var result state
    | Shfl_sync | Shfl_down_sync | Shfl_up_sync | Shfl_xor_sync -> state
  in
  Ok state

let workgroup_barrier_stmt (state : collect_state) (func : D_lang.Expr.t) :
    collect_state =
  let site, state =
    make_site state ?location:(call_location func) ~label:"__syncthreads" ()
  in
  state |> add_stmt (SM.Stmt.workgroup_barrier site) |> enter_workgroup_phase

let call_requires_subgroup (func : D_lang.Expr.t) (args : D_lang.Expr.t list) :
    bool =
  Option.is_some (D_lang.Wmma_call.classify func args)
  || Option.is_some (subgroup_collective_call_of_call func args)
  || Option.equal String.equal (call_name func) (Some "__syncwarp")

let is_assert_call (func : D_lang.Expr.t) : bool =
  match call_name func with
  | Some ("assert" | "static_assert") -> true
  | Some _ | None -> false

let launch_dimension_var (var : Variable.t) : bool =
  List.exists (Variable.equal var) (Variable.bdim_list @ Variable.gdim_list)

let record_launch_dimension (state : collect_state) (dimension : Variable.t)
    (value : Exp.nexp) : (collect_state, error) result =
  match Variable.Map.find_opt dimension state.launch_dimensions with
  | None ->
      Ok
        {
          state with
          launch_dimensions =
            Variable.Map.add dimension value state.launch_dimensions;
        }
  | Some previous when previous = value -> Ok state
  | Some previous ->
      Error
        (Conflicting_launch_dimension
           {
             kernel = state.kernel_name;
             dimension = Variable.name dimension;
             previous;
             next = value;
           })

let record_launch_dimension_from_condition (state : collect_state)
    (condition : Exp.bexp) : (collect_state, error) result =
  match condition with
  | Exp.NRel (N_rel.Eq, Exp.Var dimension, value)
    when launch_dimension_var dimension ->
      record_launch_dimension state dimension value
  | Exp.NRel (N_rel.Eq, value, Exp.Var dimension)
    when launch_dimension_var dimension ->
      record_launch_dimension state dimension value
  | _ -> Ok state

let record_assertion (state : collect_state) (args : D_lang.Expr.t list) :
    (collect_state, error) result =
  match args with
  | condition :: _ when state.control_stack = [] ->
      let ( let* ) = Result.bind in
      let* condition =
        bexp_of_expr ~context:"launch/assert precondition" condition
      in
      let* state = record_launch_dimension_from_condition state condition in
      let conditions = condition :: Exp.b_definedness_conditions condition in
      Ok
        {
          state with
          launch_preconditions_rev =
            List.rev_append conditions state.launch_preconditions_rev;
        }
  | _ -> Ok state

let classify_call ?result (state : collect_state) (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : (collect_state, error) result =
  if is_assert_call func then record_assertion state args
  else
    match D_lang.Wmma_call.classify func args with
    | Some kind -> wmma_stmt state kind func args
    | None -> (
        match subgroup_collective_call_of_call func args with
        | Some op -> subgroup_collective_stmt ?result state op func args
        | None -> (
            match call_name func with
            | Some "__syncwarp" -> subgroup_barrier_stmt state func args
            | Some "__syncthreads" -> Ok (workgroup_barrier_stmt state func)
            | _ -> Ok state))

let rec expr_requires_subgroup : D_lang.Expr.t -> bool = function
  | Convert { arg; _ } -> expr_requires_subgroup arg
  | CallExpr { func; args; _ } ->
      call_requires_subgroup func args
      || List.exists expr_requires_subgroup args
  | BinaryOperator { lhs; rhs; _ }
  | CXXOperatorCallExpr { args = [ lhs; rhs ]; _ } ->
      expr_requires_subgroup lhs || expr_requires_subgroup rhs
  | ConditionalOperator { cond; then_expr; else_expr; _ } ->
      expr_requires_subgroup cond
      || expr_requires_subgroup then_expr
      || expr_requires_subgroup else_expr
  | UnaryOperator { child; _ } | MemberExpr { base = child; _ } ->
      expr_requires_subgroup child
  | CXXConstructExpr { args; _ } -> List.exists expr_requires_subgroup args
  | SizeOfExpr _ | CXXNewExpr _ | CXXDeleteExpr _ | RecoveryExpr _
  | CharacterLiteral _ | CXXBoolLiteralExpr _ | FloatingLiteral _
  | IntegerLiteral _ | Ident _ | UnresolvedLookupExpr _ | CXXOperatorCallExpr _
    ->
      false

let decl_requires_subgroup (decl : D_lang.Decl.t) : bool =
  match decl.init with
  | Some init -> List.exists expr_requires_subgroup (D_lang.Init.to_exp init)
  | None -> false

let rec stmt_requires_subgroup : D_lang.Stmt.t -> bool = function
  | SExpr expr -> expr_requires_subgroup expr
  | DeclStmt decls -> List.exists decl_requires_subgroup decls
  | Seq (left, right) ->
      stmt_requires_subgroup left || stmt_requires_subgroup right
  | IfStmt { cond; then_stmt; else_stmt } ->
      expr_requires_subgroup cond
      || stmt_requires_subgroup then_stmt
      || stmt_requires_subgroup else_stmt
  | ForStmt { init; cond; inc; body } ->
      option_exists
        (function
          | D_lang.ForInit.Decls decls ->
              List.exists decl_requires_subgroup decls
          | Expr expr -> expr_requires_subgroup expr)
        init
      || option_exists expr_requires_subgroup cond
      || stmt_requires_subgroup inc
      || stmt_requires_subgroup body
  | WhileStmt { cond; body } | DoStmt { cond; body } | SwitchStmt { cond; body }
    ->
      expr_requires_subgroup cond || stmt_requires_subgroup body
  | CaseStmt { case; body } ->
      expr_requires_subgroup case || stmt_requires_subgroup body
  | DefaultStmt body -> stmt_requires_subgroup body
  | WriteAccessStmt write ->
      List.exists expr_requires_subgroup (write.source :: write.target.index)
  | ReadAccessStmt read -> List.exists expr_requires_subgroup read.source.index
  | AtomicAccessStmt atomic ->
      List.exists expr_requires_subgroup atomic.source.index
  | LambdaDecl { captures; body; _ } ->
      List.exists (fun (_, expr) -> expr_requires_subgroup expr) captures
      || stmt_requires_subgroup body
  | Skip | BreakStmt | GotoStmt
  | ReturnStmt None
  | ContinueStmt | AsmStmt _ | BarrierOp _ ->
      false
  | ReturnStmt (Some expr) -> expr_requires_subgroup expr

let rec first_map (f : 'a -> 'b option) : 'a list -> 'b option = function
  | [] -> None
  | value :: rest -> (
      match f value with Some _ as found -> found | None -> first_map f rest)

let first_some (left : 'a option) (right : 'a option) : 'a option =
  match left with Some _ -> left | None -> right

let rec stmt_records_ordinary_memory_effect : D_lang.Stmt.t -> bool = function
  | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _ -> true
  | Seq (left, right) ->
      stmt_records_ordinary_memory_effect left
      || stmt_records_ordinary_memory_effect right
  | IfStmt { then_stmt; else_stmt; _ } ->
      stmt_records_ordinary_memory_effect then_stmt
      || stmt_records_ordinary_memory_effect else_stmt
  | ForStmt { init; inc; body; _ } ->
      option_exists
        (function D_lang.ForInit.Decls _ -> false | Expr _ -> false)
        init
      || stmt_records_ordinary_memory_effect inc
      || stmt_records_ordinary_memory_effect body
  | WhileStmt { body; _ }
  | DoStmt { body; _ }
  | SwitchStmt { body; _ }
  | CaseStmt { body; _ }
  | DefaultStmt body ->
      stmt_records_ordinary_memory_effect body
  | SExpr _ | DeclStmt _ | Skip | BreakStmt | GotoStmt | ReturnStmt _
  | ContinueStmt | AsmStmt _ | BarrierOp _ | LambdaDecl _ ->
      false

let expr_updates_scalar_facts : D_lang.Expr.t -> bool = function
  | BinaryOperator { opcode = "="; lhs = Ident { ty; _ }; _ } ->
      not (j_type_is_pointer_like ty)
  | _ -> false

let rec stmt_updates_scalar_facts : D_lang.Stmt.t -> bool = function
  | DeclStmt decls ->
      List.exists
        (fun (decl : D_lang.Decl.t) -> not (j_type_is_pointer_like decl.ty))
        decls
  | SExpr expr -> expr_updates_scalar_facts expr
  | Seq (left, right) ->
      stmt_updates_scalar_facts left || stmt_updates_scalar_facts right
  | IfStmt { then_stmt; else_stmt; _ } ->
      stmt_updates_scalar_facts then_stmt || stmt_updates_scalar_facts else_stmt
  | ForStmt { init; inc; body; _ } ->
      option_exists
        (function
          | D_lang.ForInit.Decls decls ->
              List.exists
                (fun (decl : D_lang.Decl.t) ->
                  not (j_type_is_pointer_like decl.ty))
                decls
          | Expr expr -> expr_updates_scalar_facts expr)
        init
      || stmt_updates_scalar_facts inc
      || stmt_updates_scalar_facts body
  | WhileStmt { body; _ }
  | DoStmt { body; _ }
  | SwitchStmt { body; _ }
  | CaseStmt { body; _ }
  | DefaultStmt body ->
      stmt_updates_scalar_facts body
  | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _ | Skip | BreakStmt
  | GotoStmt | ReturnStmt _ | ContinueStmt | AsmStmt _ | BarrierOp _
  | LambdaDecl _ ->
      false

let expr_updates_pointer_facts : D_lang.Expr.t -> bool = function
  | BinaryOperator { opcode = "="; lhs = Ident { ty; _ }; _ } ->
      j_type_is_pointer_like ty
  | _ -> false

let decl_updates_pointer_facts (decl : D_lang.Decl.t) : bool =
  j_type_is_pointer_decl decl.ty

let rec stmt_updates_pointer_facts : D_lang.Stmt.t -> bool = function
  | DeclStmt decls -> List.exists decl_updates_pointer_facts decls
  | SExpr expr -> expr_updates_pointer_facts expr
  | Seq (left, right) ->
      stmt_updates_pointer_facts left || stmt_updates_pointer_facts right
  | IfStmt { then_stmt; else_stmt; _ } ->
      stmt_updates_pointer_facts then_stmt
      || stmt_updates_pointer_facts else_stmt
  | ForStmt { init; inc; body; _ } ->
      option_exists
        (function
          | D_lang.ForInit.Decls decls ->
              List.exists decl_updates_pointer_facts decls
          | Expr expr -> expr_updates_pointer_facts expr)
        init
      || stmt_updates_pointer_facts inc
      || stmt_updates_pointer_facts body
  | WhileStmt { body; _ }
  | DoStmt { body; _ }
  | SwitchStmt { body; _ }
  | CaseStmt { body; _ }
  | DefaultStmt body ->
      stmt_updates_pointer_facts body
  | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _ | Skip | BreakStmt
  | GotoStmt | ReturnStmt _ | ContinueStmt | AsmStmt _ | BarrierOp _
  | LambdaDecl _ ->
      false

let stmt_updates_source_facts (stmt : D_lang.Stmt.t) : bool =
  stmt_updates_scalar_facts stmt || stmt_updates_pointer_facts stmt

let rec stmt_has_participation_control_transfer : D_lang.Stmt.t -> bool =
  function
  | BreakStmt | ContinueStmt | GotoStmt -> true
  | Seq (left, right) ->
      stmt_has_participation_control_transfer left
      || stmt_has_participation_control_transfer right
  | IfStmt { then_stmt; else_stmt; _ } ->
      stmt_has_participation_control_transfer then_stmt
      || stmt_has_participation_control_transfer else_stmt
  | ForStmt { inc; body; _ } ->
      stmt_has_participation_control_transfer inc
      || stmt_has_participation_control_transfer body
  | WhileStmt { body; _ }
  | DoStmt { body; _ }
  | SwitchStmt { body; _ }
  | CaseStmt { body; _ }
  | DefaultStmt body ->
      stmt_has_participation_control_transfer body
  | DeclStmt _ | SExpr _ | WriteAccessStmt _ | ReadAccessStmt _
  | AtomicAccessStmt _ | Skip | ReturnStmt _ | AsmStmt _ | BarrierOp _
  | LambdaDecl _ ->
      false

let stmt_requires_source_effect_metadata (stmt : D_lang.Stmt.t) : bool =
  stmt_requires_subgroup stmt
  || stmt_records_ordinary_memory_effect stmt
  || stmt_updates_source_facts stmt
  || stmt_has_participation_control_transfer stmt

let assignment_expr : D_lang.Expr.t -> (Variable.t * D_lang.Expr.t) option =
  function
  | BinaryOperator { opcode = "="; lhs = Ident { name; _ }; rhs; _ } ->
      Some (name, rhs)
  | _ -> None

let for_init_assignment :
    D_lang.ForInit.t option -> (Variable.t * D_lang.Expr.t) option = function
  | Some (D_lang.ForInit.Decls [ { var; init = Some (IExpr rhs); _ } ]) ->
      Some (var, rhs)
  | Some (Expr expr) -> assignment_expr expr
  | Some (Decls _) | None -> None

let inc_step_expr (var : Variable.t) : D_lang.Stmt.t -> D_lang.Expr.t option =
  let same_var = Variable.equal var in
  function
  | SExpr
      (BinaryOperator
         {
           opcode = "=";
           lhs = Ident { name = target; _ };
           rhs = BinaryOperator { opcode = "+"; lhs = Ident lhs_decl; rhs; _ };
           _;
         })
    when same_var target && same_var (Decl_expr.name lhs_decl) ->
      Some rhs
  | SExpr
      (BinaryOperator
         {
           opcode = "=";
           lhs = Ident { name = target; _ };
           rhs = BinaryOperator { opcode = "+"; lhs; rhs = Ident right_decl; _ };
           _;
         })
    when same_var target && same_var (Decl_expr.name right_decl) ->
      Some lhs
  | _ -> None

let step_is_positive_constant (state : collect_state) (expr : D_lang.Expr.t) :
    bool =
  match int_constant_of_expr state expr with
  | Some value -> value > 0
  | None -> false

let loop_induction_facts (state : collect_state)
    (init : D_lang.ForInit.t option) (inc : D_lang.Stmt.t) : Exp.bexp list =
  match for_init_assignment init with
  | None -> []
  | Some (var, initial_expr) -> (
      match inc_step_expr var inc with
      | None -> []
      | Some step_expr when not (step_is_positive_constant state step_expr) ->
          []
      | Some step_expr -> (
          match
            ( nexp_of_expr ~context:"for-loop initial value" initial_expr,
              nexp_of_expr ~context:"for-loop positive stride" step_expr )
          with
          | Ok initial, Ok step ->
              let index = Exp.Var var in
              [
                Exp.n_eq
                  (Exp.n_mod (Exp.n_minus index initial) step)
                  (Exp.Num 0);
                Exp.n_le initial index;
              ]
          | Error _, _ | _, Error _ -> []))

let loop_numeric_alias_state (state : collect_state)
    (init : D_lang.ForInit.t option) (inc : D_lang.Stmt.t) : collect_state =
  match for_init_assignment init with
  | Some (var, _) when Option.is_some (inc_step_expr var inc) ->
      remove_numeric_alias var state
  | Some _ | None -> state

let template_context_score (context : C_lang.TemplateArgument.t list)
    (candidate : D_lang.Kernel.t) : int =
  List.fold_left
    (fun score argument ->
      if List.mem argument context then score + 1 else score)
    0 candidate.template_args

let helper_identity (kernel : D_lang.Kernel.t) : string =
  String.concat "\000"
    [
      D_lang.Kernel.label kernel;
      kernel.id.ty;
      String.concat ","
        (List.map C_lang.TemplateArgument.to_string kernel.template_args);
    ]

let uniquely_best_template_match (context : C_lang.TemplateArgument.t list)
    (candidates : D_lang.Kernel.t list) : D_lang.Kernel.t option =
  let scored =
    List.map
      (fun kernel -> (template_context_score context kernel, kernel))
      candidates
  in
  let best = List.fold_left (fun best (score, _) -> max best score) 0 scored in
  match List.filter (fun (score, _) -> score = best && score > 0) scored with
  | [ (_, kernel) ] -> Some kernel
  | _ -> None

let helper_candidates (helper_kernels : D_lang.Kernel.t list StringMap.t)
    (func : D_lang.Expr.t) (args : D_lang.Expr.t list) :
    D_lang.Kernel.t list * D_lang.Kernel.t list =
  match call_name func with
  | None -> ([], [])
  | Some name ->
      let candidates =
        StringMap.find_opt name helper_kernels
        |> Option.value ~default:[]
        |> List.filter (fun (kernel : D_lang.Kernel.t) ->
            List.length kernel.params = List.length args)
      in
      let resolved =
        match func with
        | D_lang.Expr.Ident { decl_id = Some id; _ } ->
              List.filter
                (fun (k : D_lang.Kernel.t) -> k.decl_id = Some id)
                candidates
        | _ -> []
      in
      let candidates =
        (* Declaration identity survives differences in qualifier spelling,
           such as an enum template argument versus its integer value. *)
        if resolved <> [] then resolved else
        match func with
        | D_lang.Expr.Ident { qualifier = _ :: _ as qualifier; _ } ->
            List.filter
              (fun (k : D_lang.Kernel.t) ->
                List.map Ty.segment_to_string k.id.qualifier = qualifier)
              candidates
        | _ -> candidates
      in
      let call_ty = Ty.to_string (D_lang.Expr.to_type func) in
      let exact =
        List.filter
          (fun (kernel : D_lang.Kernel.t) -> String.equal kernel.id.ty call_ty)
          candidates
      in
      (candidates, exact)

let select_helper_kernel ~(template_args : C_lang.TemplateArgument.t list)
    (helper_kernels : D_lang.Kernel.t list StringMap.t) (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : D_lang.Kernel.t option =
  let candidates, exact = helper_candidates helper_kernels func args in
  match uniquely_best_template_match template_args exact with
  | Some kernel -> Some kernel
  | None -> (
      match (exact, candidates) with
      | [ kernel ], _ | [], [ kernel ] -> Some kernel
      | _ -> None)

let resolve_subgroup_helper (state : collect_state) (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : (D_lang.Kernel.t option, error) result =
  match call_name func with
  | None -> Ok None
  | Some name ->
      let candidates, exact =
        helper_candidates state.helper_kernels func args
      in
      let selected =
        select_helper_kernel ~template_args:state.current_template_args
          state.helper_kernels func args
      in
      begin match selected with
      | Some kernel
        when StringSet.mem (helper_identity kernel) state.subgroup_helpers ->
          Ok (Some kernel)
      | Some _ -> Ok None
      | None ->
          if
            List.exists
              (fun kernel ->
                StringSet.mem (helper_identity kernel) state.subgroup_helpers)
              candidates
          then
            Error
              (Launch_wrapper_inlining_error
                 {
                   kernel = state.kernel_name;
                   callee = Some name;
                   reason =
                     Printf.sprintf
                       "subgroup helper resolution is %s (%d arity matches, %d \
                        type matches, %d template-context matches)"
                       (if candidates = [] then "missing" else "ambiguous")
                       (List.length candidates) (List.length exact)
                       (List.length
                          (List.filter
                             (fun kernel ->
                               template_context_score
                                 state.current_template_args kernel
                               > 0)
                             exact));
                 })
          else Ok None
      end

let call_is_directly_handled (func : D_lang.Expr.t) (args : D_lang.Expr.t list)
    : bool =
  is_assert_call func
  || call_requires_subgroup func args
  || Option.equal String.equal (call_name func) (Some "__syncthreads")

let rec collect_expr ?result (state : collect_state) (expr : D_lang.Expr.t) :
    (collect_state, error) result =
  let ( let* ) = Result.bind in
  match expr with
  | Convert { arg; _ } -> collect_expr ?result state arg
  | BinaryOperator
      { opcode = "="; lhs = Ident { name = target; ty; _ }; rhs; _ }
    when not (j_type_is_pointer_like ty) ->
      let state = update_scalar_facts_from_expr state target rhs in
      collect_expr ~result:target state rhs
  | CallExpr { func; args; _ } ->
      let* state =
        if call_is_directly_handled func args then
          classify_call ?result state func args
        else collect_subgroup_helper ?result state func args
      in
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state) args
  | BinaryOperator { lhs; rhs; _ }
  | CXXOperatorCallExpr { args = [ lhs; rhs ]; _ } ->
      let* state = collect_expr state lhs in
      collect_expr state rhs
  | ConditionalOperator { cond; then_expr; else_expr; _ } ->
      let* state = collect_expr state cond in
      let* state = collect_expr state then_expr in
      collect_expr state else_expr
  | UnaryOperator { child; _ } | MemberExpr { base = child; _ } ->
      collect_expr state child
  | CXXConstructExpr { args; _ } ->
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state) args
  | Ident decl when not (Decl_expr.is_runtime_value decl) ->
      Ok (state |> add_uniform_var decl.name |> add_memory_global_var decl.name)
  | SizeOfExpr _ | CXXNewExpr _ | CXXDeleteExpr _ | RecoveryExpr _
  | CharacterLiteral _ | CXXBoolLiteralExpr _ | FloatingLiteral _
  | IntegerLiteral _ | Ident _ | UnresolvedLookupExpr _ | CXXOperatorCallExpr _
    ->
      Ok state

and collect_subgroup_helper ?result (state : collect_state)
    (func : D_lang.Expr.t) (args : D_lang.Expr.t list) :
    (collect_state, error) result =
  let ( let* ) = Result.bind in
  let* callee = resolve_subgroup_helper state func args in
  match callee with
  | None -> Ok state
  | Some callee when StringSet.mem (helper_identity callee) state.active_helpers
    ->
      Error
        (Launch_wrapper_inlining_error
           {
             kernel = state.kernel_name;
             callee = Some callee.id.name;
             reason = "recursive subgroup helper calls are unsupported";
           })
  | Some callee ->
      let caller_active_helpers = state.active_helpers in
      let caller_template_args = state.current_template_args in
      let caller_result = state.call_result in
      let state =
        {
          state with
          active_helpers =
            StringSet.add (helper_identity callee) state.active_helpers;
          current_template_args = callee.template_args;
          call_result = result;
        }
        |> fun state ->
        add_template_argument_facts state callee.type_params
          callee.template_args
      in
      let* state =
        List.fold_left2
          (fun state param actual ->
            let* state = state in
            let decl =
              D_lang.Decl.from_expr (D_lang.Param.ty_var param) actual
            in
            let* state = collect_decl state decl in
            Ok (add_local_decl_facts state decl))
          (Ok state) callee.params args
      in
      let* state = collect_stmt state callee.code in
      Ok
        {
          state with
          active_helpers = caller_active_helpers;
          current_template_args = caller_template_args;
          call_result = caller_result;
        }

and collect_decl (state : collect_state) (decl : D_lang.Decl.t) :
    (collect_state, error) result =
  match decl.init with
  | Some (IExpr rhs) when j_type_is_pointer_decl decl.ty ->
      (* A pointer expression that this source collector cannot resolve must not
         prevent analysis of unrelated warp operations.  Keep it invalid so a
         later access still fails explicitly instead of inventing an alias. *)
      begin match
        pointer_base_offset state ~context:"pointer alias initializer" rhs
      with
      | Ok (source, offset) ->
          Ok
            (add_pointer_alias decl.var
               { source; offset; required_controls = [] }
               state)
      | Error _ ->
          Ok
            (invalidate_pointer_aliases (Variable.Set.singleton decl.var) state)
      end
  | None when j_type_is_pointer_decl decl.ty ->
      Ok (invalidate_pointer_aliases (Variable.Set.singleton decl.var) state)
  | Some (IExpr rhs) -> collect_expr ~result:decl.var state rhs
  | Some init ->
      List.fold_left
        (fun state expr ->
          let ( let* ) = Result.bind in
          let* state = state in
          collect_expr state expr)
        (Ok state) (D_lang.Init.to_exp init)
  | None -> Ok state

and collect_stmt (state : collect_state) (stmt : D_lang.Stmt.t) :
    (collect_state, error) result =
  let ( let* ) = Result.bind in
  let rec unconditionally_returns : D_lang.Stmt.t -> bool = function
    | ReturnStmt _ -> true
    | Seq (_, right) -> unconditionally_returns right
    | _ -> false
  in
  let rec early_return_continuation_guard :
      D_lang.Stmt.t -> (Exp.bexp option, error) result = function
    | IfStmt { cond; then_stmt; else_stmt = Skip }
      when unconditionally_returns then_stmt ->
        let* guard =
          bexp_of_expr ~context:"subgroup early-return condition" cond
        in
        Ok (Some (Exp.b_not guard))
    | IfStmt { cond; then_stmt = Skip; else_stmt }
      when unconditionally_returns else_stmt ->
        let* guard =
          bexp_of_expr ~context:"subgroup early-return condition" cond
        in
        Ok (Some guard)
    | Seq (left, right) ->
        let* left = early_return_continuation_guard left in
        let* right = early_return_continuation_guard right in
        begin match (left, right) with
        | None, None -> Ok None
        | Some guard, None | None, Some guard -> Ok (Some guard)
        | Some left, Some right -> Ok (Some (Exp.b_and left right))
        end
    | _ -> Ok None
  in
  let collect_with_control state guard stmt =
    let control_stack = state.control_stack in
    let* state =
      collect_stmt { state with control_stack = guard :: control_stack } stmt
    in
    Ok { state with control_stack }
  in
  let collect_stmt_guarded state ~(context : string)
      (guard_expr : D_lang.Expr.t) (stmt : D_lang.Stmt.t) :
      (collect_state, error) result =
    if stmt_requires_source_effect_metadata stmt then
      let* guard = bexp_of_expr ~context guard_expr in
      let* exit_state = collect_with_control state guard stmt in
      if stmt_updates_source_facts stmt then
        Ok
          (restore_scalar_facts exit_state
             (join_scalar_facts [ state; exit_state ]))
      else Ok exit_state
    else collect_stmt state stmt
  in
  match stmt with
  | SExpr
      (BinaryOperator
         { opcode = "="; lhs = Ident { name = target; ty; _ }; rhs; _ })
    when j_type_is_pointer_like ty -> begin
      match
        pointer_base_offset state ~context:"pointer alias assignment" rhs
      with
      | Ok (source, offset) ->
          Ok
            (add_pointer_alias target
               { source; offset; required_controls = state.control_stack }
               state)
      | Error _ ->
          Ok (invalidate_pointer_aliases (Variable.Set.singleton target) state)
    end
  | SExpr
      (BinaryOperator
         { opcode = "="; lhs = Ident { name = target; ty; _ }; rhs; _ })
    when not (j_type_is_pointer_like ty) ->
      let state = update_scalar_facts_from_expr state target rhs in
      collect_expr ~result:target state rhs
  | SExpr expr -> collect_expr state expr
  | DeclStmt decls ->
      List.fold_left
        (fun state decl ->
          let* state = state in
          let* state = collect_decl state decl in
          let state = add_local_decl_facts state decl in
          Ok (add_aggregate_member_alias_from_decl state decl))
        (Ok state) decls
  | Seq (left, right) ->
      let* state = collect_stmt state left in
      let* continuation_guard = early_return_continuation_guard left in
      begin match continuation_guard with
      | Some guard when stmt_requires_source_effect_metadata right ->
          collect_with_control state guard right
      | Some _ | None -> collect_stmt state right
      end
  | IfStmt { cond; then_stmt; else_stmt } ->
      let* state = collect_expr state cond in
      begin match int_constant_of_expr state cond with
      | Some 0 -> collect_stmt state else_stmt
      | Some _ -> collect_stmt state then_stmt
      | None ->
          let* guard =
            if
              stmt_requires_source_effect_metadata then_stmt
              || stmt_requires_source_effect_metadata else_stmt
            then
              bexp_of_expr ~context:"subgroup if control condition" cond
              |> Result.map (fun condition -> Some condition)
            else Ok None
          in
          let collect_branch state guard stmt =
            match guard with
            | Some guard when stmt_requires_source_effect_metadata stmt ->
                collect_with_control state guard stmt
            | _ -> collect_stmt state stmt
          in
          let* then_exit = collect_branch state guard then_stmt in
          let else_guard = Option.map Exp.b_not guard in
          let* else_exit_for_join = collect_branch state else_guard else_stmt in
          let else_entry =
            restore_scalar_facts then_exit (scalar_facts_of state)
          in
          let* state = collect_branch else_entry else_guard else_stmt in
          let joined_facts =
            match guard with
            | Some guard ->
                join_if_scalar_facts guard then_exit else_exit_for_join
            | None -> join_scalar_facts [ then_exit; else_exit_for_join ]
          in
          Ok (restore_scalar_facts state joined_facts)
      end
  | ForStmt { init; cond; inc; body } ->
      let* state =
        match init with
        | Some (D_lang.ForInit.Decls decls) ->
            collect_stmt state (D_lang.Stmt.DeclStmt decls)
        | Some (Expr expr) -> collect_expr state expr
        | None -> Ok state
      in
      let* state =
        match cond with
        | Some cond -> collect_expr state cond
        | None -> Ok state
      in
      let loop_facts = loop_induction_facts state init inc in
      let state = loop_numeric_alias_state state init inc in
      let loop_entry = state in
      let* body_exit =
        match cond with
        | Some cond
          when stmt_requires_source_effect_metadata body
               || stmt_requires_source_effect_metadata inc ->
            let* guard =
              bexp_of_expr ~context:"subgroup for control condition" cond
            in
            let guard = Exp.b_and_ex (guard :: loop_facts) in
            collect_with_control state guard body
        | _ -> collect_stmt state body
      in
      let* state =
        match cond with
        | Some cond when stmt_requires_source_effect_metadata inc ->
            let* guard =
              bexp_of_expr ~context:"subgroup for control condition" cond
            in
            let guard = Exp.b_and_ex (guard :: loop_facts) in
            collect_with_control body_exit guard inc
        | _ -> collect_stmt body_exit inc
      in
      if stmt_updates_source_facts body || stmt_updates_source_facts inc then
        Ok
          (restore_scalar_facts state
             (join_scalar_facts [ loop_entry; body_exit; state ]))
      else Ok state
  | WhileStmt { cond; body } ->
      let* state = collect_expr state cond in
      collect_stmt_guarded state ~context:"subgroup while control condition"
        cond body
  | DoStmt { cond; body } ->
      let* state = collect_expr state cond in
      collect_stmt_guarded state ~context:"subgroup do-while control condition"
        cond body
  | SwitchStmt { cond; body } ->
      let* state = collect_expr state cond in
      collect_stmt_guarded state ~context:"subgroup switch control condition"
        cond body
  | CaseStmt { case; body } ->
      let* state = collect_expr state case in
      collect_stmt_guarded state ~context:"subgroup case control condition" case
        body
  | DefaultStmt body -> collect_stmt state body
  | WriteAccessStmt ({ guard = Some guard; _ } as write) ->
      let* guard = bexp_of_expr ~context:"ordinary write guard" guard in
      collect_with_control state guard
        (WriteAccessStmt { write with guard = None })
  | ReadAccessStmt ({ guard = Some guard; _ } as read) ->
      let* guard = bexp_of_expr ~context:"ordinary read guard" guard in
      collect_with_control state guard
        (ReadAccessStmt { read with guard = None })
  | WriteAccessStmt write ->
      let* state =
        record_ordinary_memory_effect ~kind:Ordinary_write
          ~mode:(Write write.payload) ~context:"ordinary write access index"
          ?guard:write.guard write.target state
      in
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state)
        (write.source :: write.target.index)
  | ReadAccessStmt read ->
      let* aggregate_access =
        access_of_subscript state ~mode:Read
          ~context:"ordinary read aggregate projection" read.source
      in
      let* state =
        record_ordinary_memory_effect ~kind:Ordinary_read ~mode:Read
          ~context:"ordinary read access index" ?guard:read.guard read.source
          state
      in
      let state =
        add_aggregate_alias read.target
          {
            aggregate_array = aggregate_access.array;
            aggregate_index = aggregate_access.index;
          }
          state
      in
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state) read.source.index
  | AtomicAccessStmt atomic ->
      let* atomic_op =
        atomic_operation_of_source ~context:"ordinary atomic operation"
          atomic.atomic
      in
      let* state =
        record_ordinary_memory_effect ~kind:Ordinary_atomic
          ~mode:(Atomic atomic_op) ~context:"ordinary atomic access index"
          ?guard:atomic.guard atomic.source state
      in
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state) atomic.source.index
  | BreakStmt ->
      if control_stack_is_source_uniform state then Ok state
      else
        let control =
          state.control_stack |> List.map Exp.b_to_string
          |> String.concat " && "
        in
        Error
          (Unsupported_participation_control
             {
               kernel = state.kernel_name;
               control = "non-uniform break under " ^ control;
             })
  | ContinueStmt ->
      if control_stack_is_source_uniform state then Ok state
      else
        let control =
          state.control_stack |> List.map Exp.b_to_string
          |> String.concat " && "
        in
        Error
          (Unsupported_participation_control
             {
               kernel = state.kernel_name;
               control = "non-uniform continue under " ^ control;
             })
  | GotoStmt ->
      Error
        (Unsupported_participation_control
           { kernel = state.kernel_name; control = "goto" })
  | Skip | ReturnStmt None | AsmStmt _ | BarrierOp _ | LambdaDecl _ -> Ok state
  | ReturnStmt (Some expr) ->
      let state =
        match state.call_result with
        | Some result -> update_scalar_facts_from_expr state result expr
        | None -> state
      in
      collect_expr ?result:state.call_result state expr

let type_aliases_of_defs (context_defs : D_lang.Def.t list) : Ty.t StringMap.t =
  List.fold_left
    (fun aliases -> function
      | D_lang.Def.Typedef typedef ->
          StringMap.add (Ty.to_string typedef.alias) typedef.ty aliases
      | D_lang.Def.Declaration _ | D_lang.Def.Kernel _ | D_lang.Def.Enum _
      | D_lang.Def.LaunchParam _ | Prototype _ | Record _ | UsingNamespace _ ->
          aliases)
    StringMap.empty context_defs

let uniform_vars_of_kernel_params (kernel : D_lang.Kernel.t) : Variable.Set.t =
  let vars =
    List.fold_left
      (fun vars param -> Variable.Set.add (D_lang.Param.name param) vars)
      Variable.Set.empty kernel.params
  in
  List.fold_left
    (fun vars -> function
      | D_lang.Ty_param.NonTypeTemplate { name; _ } ->
          Variable.Set.add name vars
      | D_lang.Ty_param.TemplateType _ -> vars)
    vars kernel.type_params

let seed_context_declaration_facts (state : collect_state)
    (context_defs : D_lang.Def.t list) : collect_state =
  List.fold_left
    (fun state -> function
      | D_lang.Def.Declaration decl -> add_decl_facts state decl
      | Typedef _ | Enum _ | Kernel _ | LaunchParam _ | Prototype _ | Record _
      | UsingNamespace _ ->
          state)
    state context_defs

let helper_kernel_table (defs : D_lang.Def.t list) :
    D_lang.Kernel.t list StringMap.t =
  List.fold_left
    (fun table -> function
      | D_lang.Def.Kernel kernel
        when D_lang.KernelAttr.is_device kernel.attribute ->
          let kernels =
            StringMap.find_opt kernel.id.name table |> Option.value ~default:[]
          in
          StringMap.add kernel.id.name (kernels @ [ kernel ]) table
      | D_lang.Def.Kernel _ | Declaration _ | Typedef _ | Enum _ | LaunchParam _
      | Prototype _ | Record _ | UsingNamespace _ ->
          table)
    StringMap.empty defs

let rec resolved_expr_subgroup_callee helper_kernels subgroup_helpers
    template_args : D_lang.Expr.t -> string option = function
  | CallExpr { func; args; _ } | CXXOperatorCallExpr { func; args; _ } -> (
      match select_helper_kernel ~template_args helper_kernels func args with
      | Some helper when StringSet.mem (helper_identity helper) subgroup_helpers
        ->
          Some helper.id.name
      | Some _ | None ->
          first_some
            (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
               template_args func)
            (first_map
               (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
                  template_args)
               args))
  | BinaryOperator { lhs; rhs; _ } ->
      first_some
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args lhs)
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args rhs)
  | ConditionalOperator { cond; then_expr; else_expr; _ } ->
      first_some
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args cond)
        (first_some
           (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
              template_args then_expr)
           (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
              template_args else_expr))
  | UnaryOperator { child; _ }
  | Convert { arg = child; _ }
  | MemberExpr { base = child; _ }
  | CXXNewExpr { arg = child; _ }
  | CXXDeleteExpr { arg = child; _ } ->
      resolved_expr_subgroup_callee helper_kernels subgroup_helpers
        template_args child
  | CXXConstructExpr { args; _ } ->
      first_map
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args)
        args
  | SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _ | CXXBoolLiteralExpr _
  | FloatingLiteral _ | IntegerLiteral _ | Ident _ | UnresolvedLookupExpr _ ->
      None

let resolved_decl_subgroup_callee helper_kernels subgroup_helpers template_args
    (decl : D_lang.Decl.t) : string option =
  match decl.init with
  | Some init ->
      first_map
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args)
        (D_lang.Init.to_exp init)
  | None -> None

let rec resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
    template_args : D_lang.Stmt.t -> string option = function
  | SExpr expr | ReturnStmt (Some expr) ->
      resolved_expr_subgroup_callee helper_kernels subgroup_helpers
        template_args expr
  | DeclStmt decls ->
      first_map
        (resolved_decl_subgroup_callee helper_kernels subgroup_helpers
           template_args)
        decls
  | Seq (left, right) ->
      first_some
        (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
           template_args left)
        (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
           template_args right)
  | IfStmt { cond; then_stmt; else_stmt } ->
      first_some
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args cond)
        (first_some
           (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
              template_args then_stmt)
           (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
              template_args else_stmt))
  | ForStmt { init; cond; inc; body } ->
      let init_callee =
        match init with
        | Some (D_lang.ForInit.Decls decls) ->
            first_map
              (resolved_decl_subgroup_callee helper_kernels subgroup_helpers
                 template_args)
              decls
        | Some (Expr expr) ->
            resolved_expr_subgroup_callee helper_kernels subgroup_helpers
              template_args expr
        | None -> None
      in
      first_some init_callee
        (first_some
           (Option.bind cond
              (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
                 template_args))
           (first_some
              (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
                 template_args inc)
              (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
                 template_args body)))
  | WhileStmt { cond; body } | DoStmt { cond; body } | SwitchStmt { cond; body }
    ->
      first_some
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args cond)
        (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
           template_args body)
  | CaseStmt { case; body } ->
      first_some
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args case)
        (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
           template_args body)
  | DefaultStmt body ->
      resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
        template_args body
  | WriteAccessStmt write ->
      first_map
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args)
        (write.source :: write.target.index)
  | ReadAccessStmt read ->
      first_map
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args)
        read.source.index
  | AtomicAccessStmt atomic ->
      first_map
        (resolved_expr_subgroup_callee helper_kernels subgroup_helpers
           template_args)
        atomic.source.index
  | LambdaDecl { captures; body; _ } ->
      first_some
        (first_map
           (fun (_, expr) ->
             resolved_expr_subgroup_callee helper_kernels subgroup_helpers
               template_args expr)
           captures)
        (resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
           template_args body)
  | Skip | BreakStmt | GotoStmt
  | ReturnStmt None
  | ContinueStmt | AsmStmt _ | BarrierOp _ ->
      None

let subgroup_kernel_of_kernel (context_defs : D_lang.Def.t list)
    (target_config : SM.Target_config.t)
    ~(uniform_preserving_calls : StringSet.t) ~(subgroup_helpers : StringSet.t)
    (kernel : D_lang.Kernel.t) : (subgroup_kernel, error) result =
  let ( let* ) = Result.bind in
  match SM.Target_config.cuda_x_contiguous_subgroup_size target_config with
  | None ->
      Error (Missing_subgroup_config { kernel = D_lang.Kernel.label kernel })
  | Some _ ->
      let state =
        empty_collect_state
          ~kernel_name:(D_lang.Kernel.label kernel)
          ~target_config
          ~uniform_vars:(uniform_vars_of_kernel_params kernel)
          ~uniform_preserving_calls
          ~helper_kernels:(helper_kernel_table context_defs)
          ~subgroup_helpers ~template_args:kernel.template_args
          (type_aliases_of_defs context_defs)
        |> fun state ->
        seed_context_declaration_facts state context_defs |> fun state ->
        add_template_argument_facts state kernel.type_params
          kernel.template_args
      in
      let* state = collect_stmt state kernel.code in
      let launch_precondition =
        state.launch_preconditions_rev |> List.rev |> Exp.dedup_conditions
        |> Exp.b_and_ex
      in
      let memory_globals =
        Exp.b_free_names launch_precondition state.memory_globals
      in
      Ok
        {
          matrix_kernel =
            SM.Kernel.make ~target_config
              ~name:(D_lang.Kernel.label kernel)
              (List.rev state.stmts_rev);
          site_controls = List.rev state.site_controls_rev;
          uniform_vars = state.uniform_vars;
          memory_globals;
          ordinary_memory_effects = List.rev state.ordinary_memory_effects_rev;
          launch_precondition;
          launch_dimensions = state.launch_dimensions;
        }

module Launch_wrapper_link = struct
  type terminal_call = {
    callee_name : string;
    callee_ty : string;
    args : D_lang.Expr.t list;
  }

  let terminal_call_of_kernel (kernel : D_lang.Kernel.t) : terminal_call option
      =
    match D_lang.Stmt.last kernel.code with
    | D_lang.Stmt.SExpr (D_lang.Expr.CallExpr { func; args; _ }) ->
        call_name func
        |> Option.map (fun callee_name ->
            {
              callee_name;
              callee_ty = Ty.to_string (D_lang.Expr.to_type func);
              args;
            })
    | _ -> None

  let add_binder ~(inline_id : int) (bindings : Variable.t Variable.Map.t)
      (var : Variable.t) : Variable.t Variable.Map.t =
    if Variable.Map.mem var bindings then bindings
    else
      let fresh =
        Variable.set_name
          (Printf.sprintf "@faial_inline_%d:%s" inline_id (Variable.name var))
          var
      in
      Variable.Map.add var fresh bindings

  let add_decl_binder ~(inline_id : int) (bindings : Variable.t Variable.Map.t)
      (decl : D_lang.Decl.t) : Variable.t Variable.Map.t =
    add_binder ~inline_id bindings decl.var

  let add_param_binder ~(inline_id : int) (bindings : Variable.t Variable.Map.t)
      (param : D_lang.Param.t) : Variable.t Variable.Map.t =
    add_binder ~inline_id bindings (D_lang.Param.name param)

  let add_type_param_binder ~(inline_id : int)
      (bindings : Variable.t Variable.Map.t) (param : D_lang.Ty_param.t) :
      Variable.t Variable.Map.t =
    add_binder ~inline_id bindings (D_lang.Ty_param.name param)

  let rec collect_stmt_binders ~(inline_id : int)
      (bindings : Variable.t Variable.Map.t) (stmt : D_lang.Stmt.t) :
      Variable.t Variable.Map.t =
    let collect = collect_stmt_binders ~inline_id in
    match stmt with
    | D_lang.Stmt.DeclStmt decls ->
        List.fold_left (add_decl_binder ~inline_id) bindings decls
    | Seq (left, right) -> collect (collect bindings left) right
    | IfStmt { then_stmt; else_stmt; _ } ->
        collect (collect bindings then_stmt) else_stmt
    | ForStmt { init; inc; body; _ } ->
        let bindings =
          match init with
          | Some (D_lang.ForInit.Decls decls) ->
              List.fold_left (add_decl_binder ~inline_id) bindings decls
          | Some (D_lang.ForInit.Expr _) | None -> bindings
        in
        collect (collect bindings inc) body
    | WhileStmt { body; _ }
    | DoStmt { body; _ }
    | SwitchStmt { body; _ }
    | DefaultStmt body
    | CaseStmt { body; _ } ->
        collect bindings body
    | LambdaDecl { var; params; body; _ } ->
        let bindings = add_binder ~inline_id bindings var in
        let bindings =
          List.fold_left (add_param_binder ~inline_id) bindings params
        in
        collect bindings body
    | Skip | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _
    | BreakStmt | GotoStmt | ReturnStmt _ | ContinueStmt | SExpr _ | AsmStmt _
    | BarrierOp _ ->
        bindings

  let bindings_for_kernel ~(inline_id : int) (kernel : D_lang.Kernel.t) :
      Variable.t Variable.Map.t =
    let bindings =
      List.fold_left
        (add_param_binder ~inline_id)
        Variable.Map.empty kernel.params
    in
    let bindings =
      List.fold_left
        (add_type_param_binder ~inline_id)
        bindings kernel.type_params
    in
    collect_stmt_binders ~inline_id bindings kernel.code

  let rename_var (bindings : Variable.t Variable.Map.t) (var : Variable.t) :
      Variable.t =
    Variable.Map.find_opt var bindings |> Option.value ~default:var

  let rename_expr (bindings : Variable.t Variable.Map.t) (expr : D_lang.Expr.t)
      : D_lang.Expr.t =
    D_lang.Expr.map
      (function
        | D_lang.Expr.Ident decl
          when Decl_expr.Kind.is_runtime_value decl.kind
               || decl.kind = Decl_expr.Kind.NonTypeTemplateParm ->
            D_lang.Expr.Ident { decl with name = rename_var bindings decl.name }
        | expr -> expr)
      expr

  let rename_init bindings (init : D_lang.Init.t) : D_lang.Init.t =
    D_lang.Init.map (rename_expr bindings) init

  let rename_decl bindings (decl : D_lang.Decl.t) : D_lang.Decl.t =
    {
      decl with
      var = rename_var bindings decl.var;
      init = Option.map (rename_init bindings) decl.init;
    }

  let rename_subscript bindings (subscript : D_lang.d_subscript) :
      D_lang.d_subscript =
    {
      subscript with
      path =
        {
          (Field_path.map (rename_expr bindings) subscript.path) with
          root = rename_var bindings (Field_path.base subscript.path);
        };
      index = List.map (rename_expr bindings) subscript.index;
    }

  let rename_for_init bindings (init : D_lang.ForInit.t) : D_lang.ForInit.t =
    match init with
    | D_lang.ForInit.Decls decls ->
        D_lang.ForInit.Decls (List.map (rename_decl bindings) decls)
    | Expr expr -> D_lang.ForInit.Expr (rename_expr bindings expr)

  let rec rename_stmt bindings (stmt : D_lang.Stmt.t) : D_lang.Stmt.t =
    let expr = rename_expr bindings in
    let recurse = rename_stmt bindings in
    match stmt with
    | D_lang.Stmt.Skip -> Skip
    | Seq (left, right) -> D_lang.Stmt.seq (recurse left) (recurse right)
    | WriteAccessStmt write ->
        WriteAccessStmt
          {
            write with
            target = rename_subscript bindings write.target;
            source = expr write.source;
            guard = Option.map expr write.guard;
          }
    | ReadAccessStmt read ->
        ReadAccessStmt
          {
            read with
            target = rename_var bindings read.target;
            source = rename_subscript bindings read.source;
            guard = Option.map expr read.guard;
          }
    | AtomicAccessStmt atomic ->
        AtomicAccessStmt
          {
            atomic with
            target = rename_var bindings atomic.target;
            source = rename_subscript bindings atomic.source;
            atomic = Atomic.map expr atomic.atomic;
            guard = Option.map expr atomic.guard;
          }
    | BreakStmt -> BreakStmt
    | GotoStmt -> GotoStmt
    | ReturnStmt value -> ReturnStmt (Option.map expr value)
    | ContinueStmt -> ContinueStmt
    | IfStmt { cond; then_stmt; else_stmt } ->
        IfStmt
          {
            cond = expr cond;
            then_stmt = recurse then_stmt;
            else_stmt = recurse else_stmt;
          }
    | DeclStmt decls -> DeclStmt (List.map (rename_decl bindings) decls)
    | WhileStmt { cond; body } ->
        WhileStmt { cond = expr cond; body = recurse body }
    | ForStmt { init; cond; inc; body } ->
        ForStmt
          {
            init = Option.map (rename_for_init bindings) init;
            cond = Option.map expr cond;
            inc = recurse inc;
            body = recurse body;
          }
    | DoStmt { cond; body } -> DoStmt { cond = expr cond; body = recurse body }
    | SwitchStmt { cond; body } ->
        SwitchStmt { cond = expr cond; body = recurse body }
    | DefaultStmt body -> DefaultStmt (recurse body)
    | CaseStmt { case; body } ->
        CaseStmt { case = expr case; body = recurse body }
    | SExpr value -> SExpr (expr value)
    | AsmStmt asm -> AsmStmt (Asm.map_expr expr asm)
    | BarrierOp { op; target; args; loc } ->
        BarrierOp
          {
            op;
            target = rename_subscript bindings target;
            args = List.map expr args;
            loc;
          }
    | LambdaDecl { var; captures; params; body; ret_ty } ->
        let params =
          List.map
            (fun (param : D_lang.Param.t) ->
              let ty_var = D_lang.Param.ty_var param in
              {
                param with
                ty_var =
                  Ty_variable.make ~ty:(Ty_variable.ty ty_var)
                    ~name:(rename_var bindings (Ty_variable.name ty_var));
              })
            params
        in
        LambdaDecl
          {
            var = rename_var bindings var;
            captures =
              List.map
                (fun (name, value) -> (rename_var bindings name, expr value))
                captures;
            params;
            body = recurse body;
            ret_ty;
          }

  let parameter_binding bindings (param : D_lang.Param.t)
      (actual : D_lang.Expr.t) : D_lang.Decl.t =
    let ty_var = D_lang.Param.ty_var param in
    D_lang.Decl.from_expr
      (Ty_variable.make ~ty:(Ty_variable.ty ty_var)
         ~name:(rename_var bindings (Ty_variable.name ty_var)))
      actual

  let expr_mentions_var (var : Variable.t) (expr : D_lang.Expr.t) : bool =
    let found = ref false in
    let _ =
      D_lang.Expr.map
        (fun expr ->
          (match expr with
          | D_lang.Expr.Ident decl when Variable.equal (Decl_expr.name decl) var
            ->
              found := true
          | _ -> ());
          expr)
        expr
    in
    !found

  let subscript_mentions_var (var : Variable.t) (subscript : D_lang.d_subscript)
      : bool =
    Variable.equal (D_lang.subscript_name subscript) var
    || List.exists (expr_mentions_var var) subscript.index

  let init_mentions_var (var : Variable.t) (init : D_lang.Init.t) : bool =
    D_lang.Init.to_exp init |> List.exists (expr_mentions_var var)

  let decl_mentions_var (var : Variable.t) (decl : D_lang.Decl.t) : bool =
    Option.fold ~none:false ~some:(init_mentions_var var) decl.init

  let for_init_mentions_var (var : Variable.t) (init : D_lang.ForInit.t) : bool
      =
    match init with
    | D_lang.ForInit.Decls decls -> List.exists (decl_mentions_var var) decls
    | Expr expr -> expr_mentions_var var expr

  let asm_mentions_var (var : Variable.t) (asm : D_lang.Expr.t Asm.t) : bool =
    let operand_mentions (operand : D_lang.Expr.t Asm.operand) =
      expr_mentions_var var operand.expr
    in
    List.exists operand_mentions asm.outputs
    || List.exists operand_mentions asm.inputs

  let rec stmt_mentions_var (var : Variable.t) (stmt : D_lang.Stmt.t) : bool =
    let expr = expr_mentions_var var in
    let subscript = subscript_mentions_var var in
    let recurse = stmt_mentions_var var in
    match stmt with
    | D_lang.Stmt.Skip | BreakStmt | GotoStmt | ContinueStmt -> false
    | Seq (left, right) -> recurse left || recurse right
    | WriteAccessStmt write ->
        subscript write.target || expr write.source
        || Option.fold ~none:false ~some:expr write.guard
    | ReadAccessStmt read ->
        subscript read.source || Option.fold ~none:false ~some:expr read.guard
    | AtomicAccessStmt atomic ->
        subscript atomic.source
        || Atomic.Operation.exists expr atomic.atomic.operation
        || Option.fold ~none:false ~some:expr atomic.guard
    | ReturnStmt value -> Option.fold ~none:false ~some:expr value
    | IfStmt { cond; then_stmt; else_stmt } ->
        expr cond || recurse then_stmt || recurse else_stmt
    | DeclStmt decls -> List.exists (decl_mentions_var var) decls
    | WhileStmt { cond; body }
    | DoStmt { cond; body }
    | SwitchStmt { cond; body } ->
        expr cond || recurse body
    | ForStmt { init; cond; inc; body } ->
        Option.fold ~none:false ~some:(for_init_mentions_var var) init
        || Option.fold ~none:false ~some:expr cond
        || recurse inc || recurse body
    | DefaultStmt body -> recurse body
    | CaseStmt { case; body } -> expr case || recurse body
    | SExpr value -> expr value
    | AsmStmt asm -> asm_mentions_var var asm
    | BarrierOp { target; args; _ } -> subscript target || List.exists expr args
    | LambdaDecl { captures; body; _ } ->
        List.exists (fun (_, value) -> expr value) captures || recurse body

  let rec is_type_template_argument (argument : C_lang.TemplateArgument.t) :
      bool =
    match argument with
    | C_lang.TemplateArgument.TArgType _ -> true
    | TArgPack arguments -> List.for_all is_type_template_argument arguments
    | TArgNullArg | TArgNullPtr | TArgDecl _ | TArgExpr _ | TArgTemplate _
    | TArgTemplateExpansion _ | TArgIntegral _ ->
        false

  let template_bindings ~(caller : D_lang.Kernel.t) ~(callee : D_lang.Kernel.t)
      bindings : (D_lang.Decl.t list, error) result =
    let fail reason =
      Error
        (Launch_wrapper_inlining_error
           { kernel = caller.id.name; callee = Some callee.id.name; reason })
    in
    if callee.type_params = [] then Ok []
    else if List.length callee.type_params <> List.length callee.template_args
    then
      fail "specialization template parameter and argument counts do not match"
    else
      List.fold_left2
        (fun result param argument ->
          let ( let* ) = Result.bind in
          let* bindings_rev = result in
          match (param, argument) with
          | D_lang.Ty_param.TemplateType _, argument
            when is_type_template_argument argument ->
              Ok bindings_rev
          | ( D_lang.Ty_param.NonTypeTemplate { name; ty },
              C_lang.TemplateArgument.TArgIntegral value ) ->
              let binding =
                D_lang.Decl.from_expr
                  (Ty_variable.make ~ty ~name:(rename_var bindings name))
                  (D_lang.Expr.IntegerLiteral value)
              in
              Ok (binding :: bindings_rev)
          | D_lang.Ty_param.NonTypeTemplate { name; _ }, _
            when not (stmt_mentions_var name callee.code) ->
              (* Clang substitutes declaration-valued non-type template
                 arguments in specialized bodies. Their identity already
                 selected [callee], so no runtime declaration is needed once
                 the original template parameter has disappeared. *)
              Ok bindings_rev
          | _ ->
              fail
                ("unresolved specialization binding for template argument "
                ^ C_lang.TemplateArgument.to_string argument))
        (Ok []) callee.type_params callee.template_args
      |> Result.map List.rev

  let kernel_table (program : D_lang.Program.t) :
      D_lang.Kernel.t list StringMap.t =
    List.fold_left
      (fun table -> function
        | D_lang.Def.Kernel kernel ->
            let kernels =
              StringMap.find_opt kernel.id.name table
              |> Option.value ~default:[]
            in
            StringMap.add kernel.id.name (kernels @ [ kernel ]) table
        | Declaration _ | Typedef _ | Enum _ | LaunchParam _ | Prototype _
        | Record _ | UsingNamespace _ ->
            table)
      StringMap.empty program

  let rec template_argument_key (argument : C_lang.TemplateArgument.t) : string
      =
    let open C_lang.TemplateArgument in
    match argument with
    | TArgType ty -> "type:" ^ Ty.to_string ty
    | TArgIntegral value -> "int:" ^ string_of_int value
    | TArgNullArg -> "null-arg"
    | TArgNullPtr -> "nullptr"
    | TArgDecl name -> "decl:" ^ name
    | TArgExpr expr -> "expr:" ^ C_lang.Expr.to_string expr
    | TArgPack arguments ->
        "pack:["
        ^ String.concat ";" (List.map template_argument_key arguments)
        ^ "]"
    | TArgTemplate name -> "template:" ^ name
    | TArgTemplateExpansion name -> "template-expansion:" ^ name

  let template_arguments_key (arguments : C_lang.TemplateArgument.t list) :
      string =
    arguments |> List.map template_argument_key |> String.concat ","

  let resolve_callee ~(caller : D_lang.Kernel.t)
      (table : D_lang.Kernel.t list StringMap.t) (call : terminal_call) :
      (D_lang.Kernel.t, error) result =
    let candidates =
      StringMap.find_opt call.callee_name table
      |> Option.value ~default:[]
      |> List.filter (fun (kernel : D_lang.Kernel.t) ->
          List.length kernel.params = List.length call.args)
    in
    let specialized =
      if caller.template_args = [] then candidates
      else
        let expected = template_arguments_key caller.template_args in
        List.filter
          (fun (kernel : D_lang.Kernel.t) ->
            String.equal expected (template_arguments_key kernel.template_args))
          candidates
    in
    let exact =
      List.filter
        (fun (kernel : D_lang.Kernel.t) ->
          String.equal kernel.id.ty call.callee_ty)
        specialized
    in
    match (exact, specialized, candidates) with
    | [ kernel ], _, _ | [], [ kernel ], _ -> Ok kernel
    | [], [], [] ->
        Error
          (Launch_wrapper_inlining_error
             {
               kernel = caller.id.name;
               callee = Some call.callee_name;
               reason = "no uniquely matching kernel definition";
             })
    | [], [], _ ->
        Error
          (Launch_wrapper_inlining_error
             {
               kernel = caller.id.name;
               callee = Some call.callee_name;
               reason =
                 "no kernel specialization matches launch template args <"
                 ^ template_arguments_key caller.template_args
                 ^ ">";
             })
    | _ ->
        Error
          (Launch_wrapper_inlining_error
             {
               kernel = caller.id.name;
               callee = Some call.callee_name;
               reason = "callee resolution is ambiguous";
             })

  let inline_wrapper ~(inline_id : int)
      (table : D_lang.Kernel.t list StringMap.t) (wrapper : D_lang.Kernel.t) :
      (D_lang.Kernel.t, error) result =
    let ( let* ) = Result.bind in
    let* call =
      match terminal_call_of_kernel wrapper with
      | Some call -> Ok call
      | None ->
          Error
            (Launch_wrapper_inlining_error
               {
                 kernel = wrapper.id.name;
                 callee = None;
                 reason = "expected a terminal direct kernel call";
               })
    in
    let* callee = resolve_callee ~caller:wrapper table call in
    let bindings = bindings_for_kernel ~inline_id callee in
    let* template_bindings =
      template_bindings ~caller:wrapper ~callee bindings
    in
    let parameter_bindings =
      List.map2 (parameter_binding bindings) callee.params call.args
    in
    let code =
      D_lang.Stmt.from_list
        [
          D_lang.Stmt.skip_last wrapper.code;
          D_lang.Stmt.DeclStmt (template_bindings @ parameter_bindings);
          rename_stmt bindings callee.code;
        ]
    in
    Ok { wrapper with code; type_params = wrapper.type_params }

  let rewrite_program ~(launch_wrappers : StringSet.t)
      (program : D_lang.Program.t) : (D_lang.Program.t, error) result =
    let table = kernel_table program in
    program
    |> List.fold_left
         (fun result def ->
           let ( let* ) = Result.bind in
           let* inline_id, defs = result in
           match def with
           | D_lang.Def.Kernel kernel
             when StringSet.mem kernel.id.name launch_wrappers ->
               let* kernel = inline_wrapper ~inline_id table kernel in
               Ok (inline_id + 1, D_lang.Def.Kernel kernel :: defs)
           | _ -> Ok (inline_id, def :: defs))
         (Ok (0, []))
    |> Result.map (fun (_, defs) -> List.rev defs)
end

let uniquify_global_kernel_names ~(launch_wrappers : StringSet.t)
    (program : D_lang.Program.t) : D_lang.Program.t * StringSet.t =
  let initial =
    List.fold_left
      (fun names -> function
        | D_lang.Def.Kernel kernel when D_lang.Kernel.is_global kernel ->
            StringSet.add (D_lang.Kernel.label kernel) names
        | D_lang.Def.Kernel _ | Declaration _ | Typedef _ | Enum _
        | LaunchParam _ | Prototype _ | Record _ | UsingNamespace _ ->
            names)
      StringSet.empty program
  in
  let used = ref StringSet.empty in
  let renamed_wrappers = ref StringSet.empty in
  let program =
    List.map
      (function
        | D_lang.Def.Kernel kernel when D_lang.Kernel.is_global kernel ->
            let original = D_lang.Kernel.label kernel in
            let name =
              if not (StringSet.mem original !used) then original
              else
                let rec fresh suffix =
                  let candidate = Printf.sprintf "%s_%d" original suffix in
                  if
                    StringSet.mem candidate !used
                    || StringSet.mem candidate initial
                  then fresh (suffix + 1)
                  else candidate
                in
                fresh 2
            in
            used := StringSet.add name !used;
            if StringSet.mem original launch_wrappers then
              renamed_wrappers := StringSet.add name !renamed_wrappers;
            if name = original then D_lang.Def.Kernel kernel
            else
              D_lang.Def.Kernel
                {
                  kernel with
                  id =
                    { kernel.id with name; qualifier = []; template_args = [] };
                }
        | def -> def)
      program
  in
  (program, !renamed_wrappers)

let uniform_preserving_intrinsics : StringSet.t =
  [ "__umulhi"; "make_uint2" ]
  |> List.fold_left (fun names name -> StringSet.add name names) StringSet.empty

let expression_uses_thread_coordinate (expr : D_lang.Expr.t) : bool =
  match variable_of_ident_or_member expr with
  | Some var -> List.exists (Variable.equal var) Variable.tid_list
  | None -> false

let rec expr_is_uniform_preserving_with (calls : StringSet.t)
    (expr : D_lang.Expr.t) : bool =
  let recurse = expr_is_uniform_preserving_with calls in
  if expression_uses_thread_coordinate expr then false
  else
    match expr with
    | SizeOfExpr _ | CharacterLiteral _ | CXXBoolLiteralExpr _
    | FloatingLiteral _ | IntegerLiteral _ | Ident _ ->
        true
    | BinaryOperator { lhs; rhs; _ }
    | CXXOperatorCallExpr { args = [ lhs; rhs ]; _ } ->
        recurse lhs && recurse rhs
    | ConditionalOperator { cond; then_expr; else_expr; _ } ->
        recurse cond && recurse then_expr && recurse else_expr
    | UnaryOperator { child; _ }
    | MemberExpr { base = child; _ }
    | Convert { arg = child; _ } ->
        recurse child
    | CXXConstructExpr { args; _ } -> List.for_all recurse args
    | CallExpr { func; args; _ } -> (
        match call_name func with
        | Some name ->
            (StringSet.mem name calls || Functions.supported name
           || Predicates.supported name)
            && List.for_all recurse args
        | None -> false)
    | CXXNewExpr _ | CXXDeleteExpr _ | RecoveryExpr _ | CXXOperatorCallExpr _
    | UnresolvedLookupExpr _ ->
        false

let init_is_uniform_preserving_with (calls : StringSet.t) (init : D_lang.Init.t)
    : bool =
  D_lang.Init.to_exp init
  |> List.for_all (expr_is_uniform_preserving_with calls)

let rec stmt_is_uniform_preserving_with (calls : StringSet.t)
    (stmt : D_lang.Stmt.t) : bool =
  let expr = expr_is_uniform_preserving_with calls in
  let recurse = stmt_is_uniform_preserving_with calls in
  match stmt with
  | D_lang.Stmt.Skip | BreakStmt | GotoStmt | ContinueStmt -> true
  | Seq (left, right) -> recurse left && recurse right
  | ReturnStmt value -> Option.fold ~none:true ~some:expr value
  | IfStmt { cond; then_stmt; else_stmt } ->
      expr cond && recurse then_stmt && recurse else_stmt
  | DeclStmt decls ->
      List.for_all
        (fun (decl : D_lang.Decl.t) ->
          Option.fold ~none:true
            ~some:(init_is_uniform_preserving_with calls)
            decl.init)
        decls
  | WhileStmt { cond; body } | DoStmt { cond; body } | SwitchStmt { cond; body }
    ->
      expr cond && recurse body
  | ForStmt { init; cond; inc; body } ->
      let init_is_uniform =
        match init with
        | None -> true
        | Some (D_lang.ForInit.Expr value) -> expr value
        | Some (D_lang.ForInit.Decls decls) ->
            List.for_all
              (fun (decl : D_lang.Decl.t) ->
                Option.fold ~none:true
                  ~some:(init_is_uniform_preserving_with calls)
                  decl.init)
              decls
      in
      init_is_uniform
      && Option.fold ~none:true ~some:expr cond
      && recurse inc && recurse body
  | DefaultStmt body -> recurse body
  | CaseStmt { case; body } -> expr case && recurse body
  | SExpr value -> expr value
  | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _ | AsmStmt _
  | BarrierOp _ | LambdaDecl _ ->
      false

let uniform_preserving_device_calls (program : D_lang.Program.t) : StringSet.t =
  let candidates =
    List.fold_left
      (fun candidates -> function
        | D_lang.Def.Kernel kernel
          when D_lang.KernelAttr.is_device kernel.attribute ->
            let overloads =
              StringMap.find_opt kernel.id.name candidates
              |> Option.value ~default:[]
            in
            StringMap.add kernel.id.name (kernel :: overloads) candidates
        | D_lang.Def.Kernel _ | Declaration _ | Typedef _ | Enum _
        | LaunchParam _ | Prototype _ | Record _ | UsingNamespace _ ->
            candidates)
      StringMap.empty program
  in
  let rec close calls =
    let next =
      StringMap.fold
        (fun name kernels calls ->
          if
            StringSet.mem name calls
            || not
                 (List.for_all
                    (fun (kernel : D_lang.Kernel.t) ->
                      stmt_is_uniform_preserving_with calls kernel.code)
                    kernels)
          then calls
          else StringSet.add name calls)
        candidates calls
    in
    if StringSet.equal calls next then calls else close next
  in
  close uniform_preserving_intrinsics

let route_kernel_with_uniform_calls
    ?(target_config = SM.Target_config.missing_cuda) ?(context_defs = [])
    ?(uniform_preserving_calls = StringSet.empty)
    ?(subgroup_helpers = StringSet.empty) (kernel : D_lang.Kernel.t) :
    (routed_kernel, error) result =
  let helper_kernels = helper_kernel_table context_defs in
  let subgroup_callee =
    resolved_stmt_subgroup_callee helper_kernels subgroup_helpers
      kernel.template_args kernel.code
  in
  let has_direct_subgroup = stmt_requires_subgroup kernel.code in
  if has_direct_subgroup || Option.is_some subgroup_callee then
    let subgroup_helpers =
      if has_direct_subgroup then StringSet.empty else subgroup_helpers
    in
    subgroup_kernel_of_kernel context_defs target_config
      ~uniform_preserving_calls ~subgroup_helpers kernel
    |> Result.map (fun kernel -> Subgroup_matrix kernel)
  else Ok (Ordinary_source kernel)

let route_kernel ?(target_config = SM.Target_config.missing_cuda)
    ?(context_defs = []) (kernel : D_lang.Kernel.t) :
    (routed_kernel, error) result =
  route_kernel_with_uniform_calls ~target_config ~context_defs kernel

let route_program ?target_config ?only_kernel
    ?(launch_wrappers = StringSet.empty) (program : D_lang.Program.t) :
    (routed_kernel list, error) result =
  let ( let* ) = Result.bind in
  let program, launch_wrappers =
    uniquify_global_kernel_names ~launch_wrappers program
  in
  let launch_wrappers =
    match only_kernel with
    | Some kernel when StringSet.mem kernel launch_wrappers ->
        StringSet.singleton kernel
    | Some _ -> StringSet.empty
    | None -> launch_wrappers
  in
  let* program = Launch_wrapper_link.rewrite_program ~launch_wrappers program in
  let helper_kernels = helper_kernel_table program in
  let direct_subgroup_kernels =
    List.fold_left
      (fun names -> function
        | D_lang.Def.Kernel kernel when stmt_requires_subgroup kernel.code ->
            StringSet.add (helper_identity kernel) names
        | D_lang.Def.Kernel _ | Declaration _ | Typedef _ | Enum _
        | LaunchParam _ | Prototype _ | Record _ | UsingNamespace _ ->
            names)
      StringSet.empty program
  in
  let rec close_subgroup_dependencies names =
    let expanded =
      List.fold_left
        (fun names -> function
          | D_lang.Def.Kernel kernel
            when Option.is_some
                   (resolved_stmt_subgroup_callee helper_kernels names
                      kernel.template_args kernel.code) ->
              StringSet.add (helper_identity kernel) names
          | D_lang.Def.Kernel _ | Declaration _ | Typedef _ | Enum _
          | LaunchParam _ | Prototype _ | Record _ | UsingNamespace _ ->
              names)
        names program
    in
    if StringSet.equal names expanded then names
    else close_subgroup_dependencies expanded
  in
  let subgroup_kernels = close_subgroup_dependencies direct_subgroup_kernels in
  let routes_to_subgroup (kernel : D_lang.Kernel.t) : bool =
    stmt_requires_subgroup kernel.code
    || Option.is_some
         (resolved_stmt_subgroup_callee helper_kernels subgroup_kernels
            kernel.template_args kernel.code)
  in
  let selected_subgroup_kernel =
    List.exists
      (function
        | D_lang.Def.Kernel kernel ->
            D_lang.Kernel.is_global kernel
            && Option.fold ~none:true
                 ~some:(String.equal (D_lang.Kernel.label kernel))
                 only_kernel
            && routes_to_subgroup kernel
        | Declaration _ | Typedef _ | Enum _ | LaunchParam _ | Prototype _
        | Record _ | UsingNamespace _ ->
            false)
      program
  in
  let uniform_preserving_calls =
    if selected_subgroup_kernel then uniform_preserving_device_calls program
    else StringSet.empty
  in
  let context_defs = program in
  program
  |> List.fold_left
       (fun routed def ->
         let* routed = routed in
         match def with
         | D_lang.Def.Kernel kernel
           when D_lang.Kernel.is_global kernel
                && Option.fold ~none:true
                     ~some:(String.equal (D_lang.Kernel.label kernel))
                     only_kernel -> (
             match
               resolved_stmt_subgroup_callee helper_kernels subgroup_kernels
                 kernel.template_args kernel.code
             with
             | Some callee
               when StringSet.mem kernel.id.name launch_wrappers
                    && not (stmt_requires_subgroup kernel.code) ->
                 if
                   Option.is_some
                     (Launch_wrapper_link.terminal_call_of_kernel kernel)
                 then
                   Error
                     (Subgroup_callee_requires_inlining
                        { kernel = kernel.id.name; callee })
                 else
                   let* kernel =
                     route_kernel_with_uniform_calls ?target_config
                       ~context_defs ~uniform_preserving_calls
                       ~subgroup_helpers:subgroup_kernels kernel
                   in
                   Ok (kernel :: routed)
             | Some _ | None ->
                 let* kernel =
                   route_kernel_with_uniform_calls ?target_config ~context_defs
                     ~uniform_preserving_calls
                     ~subgroup_helpers:subgroup_kernels kernel
                 in
                 Ok (kernel :: routed))
         | D_lang.Def.Kernel _ -> Ok routed
         | Declaration _ | Typedef _ | Enum _ | LaunchParam _ | Prototype _
         | Record _ | UsingNamespace _ ->
             Ok routed)
       (Ok [])
  |> Result.map List.rev

let kernel_site_summary (kernel : SM.Kernel.t) : string list =
  Printf.sprintf "kernel %s target=%s" kernel.name
    (SM.Target_config.to_string kernel.target_config)
  :: List.map SM.Stmt.to_string kernel.body

let matrix_memory_effect_mode : SM.Matrix.memory_effect -> string = function
  | Read _ -> "read"
  | Write _ -> "write"

let matrix_footprint_summary_of_stmt (stmt : SM.Stmt.t) : string option =
  match stmt with
  | Matrix_collective collective -> (
      match collective.memory with
      | None -> None
      | Some memory ->
          let footprint = SM.Matrix.memory_effect_footprint memory in
          let indexed_access = SM.Matrix.indexed_access footprint in
          let bounds = SM.Matrix.bounds_condition footprint in
          Some
            (Printf.sprintf
               "%s matrix<%s> effect=%s base=%s indexed=%s bounds=%s"
               (SM.Site.to_string collective.site)
               (SM.Matrix.collective_kind_to_string collective.kind)
               (matrix_memory_effect_mode memory)
               (SM.Matrix.footprint_to_string footprint)
               (Access.to_string indexed_access)
               (Exp.b_to_string bounds)))
  | Workgroup_barrier _ | Subgroup_barrier _ | Subgroup_collective _ -> None

let kernel_matrix_footprint_summary (kernel : SM.Kernel.t) : string list =
  List.filter_map matrix_footprint_summary_of_stmt kernel.body

let variable_set_to_sorted_names (vars : Variable.Set.t) : string list =
  vars |> Variable.Set.elements |> List.map Variable.name
  |> List.sort String.compare

let uniform_var_summary (vars : Variable.Set.t) : string list =
  match variable_set_to_sorted_names vars with
  | [] -> [ "uniform_vars: <none>" ]
  | names -> [ "uniform_vars: " ^ String.concat ", " names ]

let memory_global_summary (vars : Variable.Set.t) : string list =
  match variable_set_to_sorted_names vars with
  | [] -> [ "memory_globals: <none>" ]
  | names -> [ "memory_globals: " ^ String.concat ", " names ]

let site_control_summary (controls : site_control list) : string list =
  let control_to_string (control : site_control) =
    let rendered =
      match control.conditions with
      | [] -> "true"
      | conditions -> Exp.b_and_ex conditions |> Exp.b_to_string
    in
    let memory_rendered =
      if control.memory_conditions = control.conditions then ""
      else
        let memory_control =
          match control.memory_conditions with
          | [] -> "true"
          | conditions -> Exp.b_and_ex conditions |> Exp.b_to_string
        in
        " memory_control=" ^ memory_control
    in
    let uniform_vars =
      match variable_set_to_sorted_names control.uniform_vars with
      | [] -> "<none>"
      | names -> String.concat ", " names
    in
    Printf.sprintf
      "site#%d order#%d control=%s%s uniform_vars=%s numeric_aliases=%d"
      control.site_id control.source_order rendered memory_rendered uniform_vars
      (Variable.Map.cardinal control.numeric_aliases)
  in
  match controls with
  | [] -> [ "site_controls: <none>" ]
  | controls -> "site_controls:" :: List.map control_to_string controls

let kernel_artifact_summary (kernel : SM.Kernel.t) : string list =
  [ "artifact: ocaml-subgroup-matrix-v1" ]
  @ kernel_site_summary kernel
  @ ("matrix_footprints:" :: kernel_matrix_footprint_summary kernel)

let subgroup_kernel_artifact_summary (subgroup : subgroup_kernel) : string list
    =
  kernel_artifact_summary subgroup.matrix_kernel
  @ uniform_var_summary subgroup.uniform_vars
  @ memory_global_summary subgroup.memory_globals
  @ site_control_summary subgroup.site_controls
  @ "ordinary_memory_effects:"
    :: List.map ordinary_memory_effect_summary subgroup.ordinary_memory_effects
