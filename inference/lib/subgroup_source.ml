open Protocols
module SM = Subgroup_matrix
module StringMap = Stage0.Common.StringMap
module IntMap = Stage0.Common.IntMap

type error =
  | Missing_subgroup_config of { kernel : string }
  | Unsupported_expression of { context : string; expr : string }
  | Unsupported_matrix_call of { op : string; reason : string; expr : string }
  | Ordinary_imp_error of string

type routed_kernel =
  | Ordinary_imp of Imp.Kernel.t
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
}

and subgroup_kernel = {
  matrix_kernel : SM.Kernel.t;
  site_controls : site_control list;
  uniform_vars : Variable.Set.t;
  memory_globals : Variable.Set.t;
  ordinary_memory_effects : ordinary_memory_effect list;
}

let error_to_string : error -> string = function
  | Missing_subgroup_config { kernel } ->
      Printf.sprintf
        "kernel '%s' contains subgroup/matrix source operations but no \
         explicit subgroup configuration was provided"
        kernel
  | Unsupported_expression { context; expr } ->
      Printf.sprintf "unsupported expression in %s: %s" context expr
  | Unsupported_matrix_call { op; reason; expr } ->
      Printf.sprintf "unsupported matrix call '%s': %s in %s" op reason expr
  | Ordinary_imp_error msg -> msg

let call_name : D_lang.Expr.t -> string option = function
  | Ident { name; kind = Function; _ } | UnresolvedLookupExpr { name; _ } ->
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

let j_type_is_pointer_like (ty : J_type.t) : bool =
  J_type.to_desugared_c_type ty |> fun ty ->
  C_type.is_pointer ty || C_type.is_array ty || C_type.is_auto ty

let j_type_is_pointer_decl (ty : J_type.t) : bool =
  J_type.to_desugared_c_type ty |> fun ty ->
  C_type.is_pointer ty || C_type.is_auto ty

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
  | CXXBoolLiteralExpr value -> Ok (Exp.Num (if value then 1 else 0))
  | IntegerLiteral n | CharacterLiteral n -> Ok (Exp.Num n)
  | FloatingLiteral n -> Ok (Exp.Num (Float.to_int n))
  | MemberExpr _ -> Ok (Exp.Var (Variable.from_name (expr_to_string expr)))
  | BinaryOperator { opcode = "+"; lhs; rhs; _ } -> binary Plus lhs rhs
  | BinaryOperator { opcode = "-"; lhs; rhs; _ } -> binary Minus lhs rhs
  | BinaryOperator { opcode = "*"; lhs; rhs; _ } -> binary Mult lhs rhs
  | BinaryOperator { opcode = "/"; lhs; rhs; _ } -> binary Div lhs rhs
  | BinaryOperator { opcode = "%"; lhs; rhs; _ } -> binary Mod lhs rhs
  | BinaryOperator { opcode = "<<"; lhs; rhs; _ } -> binary LeftShift lhs rhs
  | BinaryOperator { opcode = ">>"; lhs; rhs; _ } -> binary RightShift lhs rhs
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
      Ok (Exp.NCall (Option.get (call_name func), value))
  | UnaryOperator { opcode = "-"; child; _ } ->
      let* child = nexp_of_expr ~context child in
      Ok (Exp.n_uminus child)
  | BinaryOperator { opcode = ","; rhs; _ } -> nexp_of_expr ~context rhs
  | _ -> unsupported_expr ~context expr

let n_rel_of_opcode : string -> N_rel.t option = function
  | "==" -> Some N_rel.Eq
  | "!=" -> Some N_rel.Neq
  | "<" -> Some N_rel.Lt
  | "<=" -> Some N_rel.Le
  | ">" -> Some N_rel.Gt
  | ">=" -> Some N_rel.Ge
  | _ -> None

let b_rel_of_opcode : string -> B_rel.t option = function
  | "&&" -> Some B_rel.BAnd
  | "||" -> Some B_rel.BOr
  | _ -> None

let rec bexp_of_expr ~(context : string) (expr : D_lang.Expr.t) :
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

type pointer_alias = { source : Variable.t; offset : Exp.nexp }

let pointer_alias_equal (lhs : pointer_alias) (rhs : pointer_alias) : bool =
  Variable.equal lhs.source rhs.source && lhs.offset = rhs.offset

type collect_state = {
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
  invalid_pointer_aliases : Variable.Set.t;
  type_aliases : C_type.t StringMap.t;
}

type scalar_facts = {
  fact_uniform_vars : Variable.Set.t;
  fact_memory_globals : Variable.Set.t;
  fact_thread_x_coordinate_vars : Variable.Set.t;
  fact_constant_values : int Variable.Map.t;
  fact_numeric_aliases : Exp.nexp Variable.Map.t;
  fact_pointer_aliases : pointer_alias Variable.Map.t;
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
    | first :: rest ->
        let candidates = pointer_fact_candidates states in
        let aliases =
          Variable.Set.fold
            (fun var aliases ->
              match Variable.Map.find_opt var first.pointer_aliases with
              | Some alias
                when List.for_all
                       (fun (state : collect_state) ->
                         match
                           Variable.Map.find_opt var state.pointer_aliases
                         with
                         | Some other -> pointer_alias_equal alias other
                         | None -> false)
                       rest ->
                  Variable.Map.add var alias aliases
              | Some _ | None -> aliases)
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
  | Ident decl -> Variable.Map.find_opt decl.name state.constant_values
  | UnaryOperator { opcode = "-"; child; _ } ->
      int_constant_of_expr state child |> Option.map Int.neg
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
      | _ -> None
      end
  | SizeOfExpr _ | CXXNewExpr _ | CXXDeleteExpr _ | RecoveryExpr _
  | FloatingLiteral _ | MemberExpr _ | CallExpr _ | ConditionalOperator _
  | CXXConstructExpr _ | CXXBoolLiteralExpr _ | CXXOperatorCallExpr _
  | UnaryOperator _ | UnresolvedLookupExpr _ ->
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
  | CXXNewExpr { arg = child; _ }
  | CXXDeleteExpr { arg = child; _ } ->
      expr_is_source_uniform state child
  | CXXConstructExpr { args; _ } ->
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
  | Binary (Div, Var x, Num size)
    when Variable.equal x Variable.tid_x
         || Variable.Set.mem x state.thread_x_coordinate_vars ->
      Option.equal Int.equal (target_subgroup_size_value state) (Some size)
  | Binary (Mod, Var x, _)
    when Variable.equal x Variable.tid_x
         || Variable.Set.mem x state.thread_x_coordinate_vars ->
      false
  | Binary (_, lhs, rhs) ->
      nexp_is_source_uniform state lhs && nexp_is_source_uniform state rhs
  | Unary (_, expr) | Other expr -> nexp_is_source_uniform state expr
  | NIf (cond, then_expr, else_expr) ->
      bexp_is_source_uniform state cond
      && nexp_is_source_uniform state then_expr
      && nexp_is_source_uniform state else_expr
  | NCall _ -> false
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

let control_stack_is_source_uniform (state : collect_state) : bool =
  List.for_all (bexp_is_source_uniform state) state.control_stack

let rec expr_is_memory_global (state : collect_state) : D_lang.Expr.t -> bool =
  function
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
  | CallExpr _ | CXXOperatorCallExpr _ | RecoveryExpr _ | UnresolvedLookupExpr _
    ->
      false

let rec nexp_is_memory_global (state : collect_state) : Exp.nexp -> bool =
  function
  | Num _ -> true
  | Var x -> is_memory_global_builtin x || is_explicit_memory_global_var state x
  | Binary (_, lhs, rhs) ->
      nexp_is_memory_global state lhs && nexp_is_memory_global state rhs
  | Unary (_, expr) | Other expr -> nexp_is_memory_global state expr
  | NIf (cond, then_expr, else_expr) ->
      bexp_is_memory_global state cond
      && nexp_is_memory_global state then_expr
      && nexp_is_memory_global state else_expr
  | NCall _ -> false
  | CastInt cond -> bexp_is_memory_global state cond

and bexp_is_memory_global (state : collect_state) : Exp.bexp -> bool = function
  | Bool _ -> true
  | NRel (_, lhs, rhs) ->
      nexp_is_memory_global state lhs && nexp_is_memory_global state rhs
  | BRel (_, lhs, rhs) ->
      bexp_is_memory_global state lhs && bexp_is_memory_global state rhs
  | BNot cond -> bexp_is_memory_global state cond
  | Pred _ -> false
  | CastBool expr -> nexp_is_memory_global state expr
  | Distinct exprs -> List.for_all (nexp_is_memory_global state) exprs

let control_stack_is_memory_global (state : collect_state) : bool =
  List.for_all (bexp_is_memory_global state) state.control_stack

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

let pointer_alias_depends_on (vars : Variable.Set.t) (alias : pointer_alias) :
    bool =
  Exp.n_free_names alias.offset Variable.Set.empty
  |> Variable.Set.exists (fun var -> Variable.Set.mem var vars)

let pointer_aliases_depending_on (vars : Variable.Set.t)
    (aliases : pointer_alias Variable.Map.t) : Variable.Set.t =
  Variable.Map.fold
    (fun target alias invalidated ->
      if pointer_alias_depends_on vars alias then
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
    pointer_aliases_depending_on invalidated state.pointer_aliases
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
  let control_is_memory_global = control_stack_is_memory_global state in
  let is_thread_x = expr_is_thread_x_coordinate state rhs in
  let constant_value = int_constant_of_expr state rhs in
  let is_uniform = expr_is_source_uniform state rhs in
  let is_memory_global = expr_is_memory_global state rhs in
  let numeric_alias = numeric_alias_from_expr var rhs in
  let state = remove_scalar_facts var state in
  let state =
    match numeric_alias with
    | Some expr -> add_numeric_alias var expr state
    | None -> state
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
    if control_is_memory_global && is_memory_global then
      add_memory_global_var var state
    else state

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
        if control_is_uniform && is_uniform then add_uniform_var decl.var state
        else state
      in
      if control_is_uniform && is_memory_global then
        add_memory_global_var decl.var state
      else state
  | None -> remove_scalar_facts decl.var state

let add_decl_facts (state : collect_state) (decl : D_lang.Decl.t) :
    collect_state =
  update_scalar_facts_from_init state decl

let resolve_type_alias (state : collect_state) (ty : C_type.t) : C_type.t =
  StringMap.find_opt (C_type.to_string ty) state.type_aliases
  |> Option.value ~default:ty

let expr_type_to_string (state : collect_state) (expr : D_lang.Expr.t) : string
    =
  D_lang.Expr.to_type expr |> J_type.to_desugared_c_type
  |> resolve_type_alias state |> C_type.to_string

let add_pointer_alias (target : Variable.t) (alias : pointer_alias)
    (state : collect_state) : collect_state =
  {
    state with
    pointer_aliases = Variable.Map.add target alias state.pointer_aliases;
    invalid_pointer_aliases =
      Variable.Set.remove target state.invalid_pointer_aliases;
  }

let rec pointer_base_offset (state : collect_state) ~(context : string)
    (expr : D_lang.Expr.t) : (Variable.t * Exp.nexp, error) result =
  let ( let* ) = Result.bind in
  match expr with
  | Ident decl -> (
      if Variable.Set.mem decl.name state.invalid_pointer_aliases then
        Error
          (Unsupported_expression
             {
               context = context ^ " invalidated pointer alias";
               expr = expr_to_string expr;
             })
      else
        match Variable.Map.find_opt decl.name state.pointer_aliases with
        | Some alias -> Ok (alias.source, alias.offset)
        | None -> Ok (decl.name, Exp.Num 0))
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

let empty_collect_state ~(target_config : SM.Target_config.t)
    ~(uniform_vars : Variable.Set.t) (type_aliases : C_type.t StringMap.t) :
    collect_state =
  {
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
    invalid_pointer_aliases = Variable.Set.empty;
    type_aliases;
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
  let memory_conditions = Option.value memory_conditions ~default:conditions in
  {
    state with
    site_controls_rev =
      {
        site_id;
        source_order;
        conditions;
        memory_conditions;
        uniform_vars = state.uniform_vars;
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

let access_of_subscript ~(mode : Access.Mode.t) ~(context : string)
    (subscript : D_lang.d_subscript) : (Access.t, error) result =
  let ( let* ) = Result.bind in
  let* index = map_result_list (nexp_of_expr ~context) subscript.index in
  Ok { Access.array = subscript.name; index; mode }

let condition_free_names (conditions : Exp.bexp list) : Variable.Set.t =
  List.fold_left
    (fun names condition -> Exp.b_free_names condition names)
    Variable.Set.empty conditions

let rec nexp_definedness_conditions (expr : Exp.nexp) : Exp.bexp list =
  match expr with
  | Num _ | Var _ -> []
  | Binary ((Div | Mod), lhs, rhs) ->
      nexp_definedness_conditions lhs
      @ nexp_definedness_conditions rhs
      @ [ Exp.n_neq rhs (Exp.Num 0) ]
  | Binary (_, lhs, rhs) ->
      nexp_definedness_conditions lhs @ nexp_definedness_conditions rhs
  | Unary (_, expr) | Other expr -> nexp_definedness_conditions expr
  | NIf (cond, then_expr, else_expr) ->
      bexp_definedness_conditions cond
      @ nexp_definedness_conditions then_expr
      @ nexp_definedness_conditions else_expr
  | NCall (_, expr) -> nexp_definedness_conditions expr
  | CastInt cond -> bexp_definedness_conditions cond

and bexp_definedness_conditions (condition : Exp.bexp) : Exp.bexp list =
  match condition with
  | Bool _ -> []
  | NRel (_, lhs, rhs) ->
      nexp_definedness_conditions lhs @ nexp_definedness_conditions rhs
  | BRel (_, lhs, rhs) ->
      bexp_definedness_conditions lhs @ bexp_definedness_conditions rhs
  | BNot condition -> bexp_definedness_conditions condition
  | Pred (_, expr) | CastBool expr -> nexp_definedness_conditions expr
  | Distinct exprs -> List.concat_map nexp_definedness_conditions exprs

let access_definedness_conditions (access : Access.t) : Exp.bexp list =
  List.concat_map nexp_definedness_conditions access.index

let conditions_definedness_conditions (conditions : Exp.bexp list) :
    Exp.bexp list =
  List.concat_map bexp_definedness_conditions conditions

let dedup_conditions (conditions : Exp.bexp list) : Exp.bexp list =
  let rec loop seen kept = function
    | [] -> List.rev kept
    | condition :: rest ->
        if List.exists (fun existing -> existing = condition) seen then
          loop seen kept rest
        else loop (condition :: seen) (condition :: kept) rest
  in
  loop [] [] conditions

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
              Exp.n_eq (Exp.Var var) expr :: nexp_definedness_conditions expr
            in
            collect seen (dependencies @ rest)
              (List.rev_append facts conditions))
  in
  collect Variable.Set.empty (Variable.Set.elements initial_names) []
  |> dedup_conditions

let record_ordinary_memory_effect ~(kind : ordinary_memory_kind)
    ~(mode : Access.Mode.t) ~(context : string) (subscript : D_lang.d_subscript)
    (state : collect_state) : (collect_state, error) result =
  let ( let* ) = Result.bind in
  let* access = access_of_subscript ~mode ~context subscript in
  let control_conditions = List.rev state.control_stack in
  let alias_conditions =
    relevant_numeric_alias_conditions state access control_conditions
  in
  let definedness_conditions =
    access_definedness_conditions access
    @ conditions_definedness_conditions control_conditions
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
        dedup_conditions
          (alias_conditions @ definedness_conditions @ control_conditions);
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
        let control_conditions = List.rev state.control_stack in
        let definedness_conditions =
          access_definedness_conditions access
          @ conditions_definedness_conditions control_conditions
        in
        Some
          (dedup_conditions
             (relevant_numeric_alias_conditions state access control_conditions
             @ definedness_conditions @ control_conditions))
  in
  let state = record_site_control ?memory_conditions site state in
  Ok
    (state
    |> add_stmt (SM.Stmt.Matrix_collective collective)
    |> enter_subgroup_phase site)

let subgroup_barrier_stmt (state : collect_state) (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : (collect_state, error) result =
  let expr = D_lang.Expr.CallExpr { func; args; ty = J_type.void } in
  match args with
  | [] | [ _ ] ->
      let site, state =
        make_site state ?location:(call_location func) ~label:"__syncwarp" ()
      in
      let state = record_site_control site state in
      Ok
        (state
        |> add_stmt (SM.Stmt.subgroup_barrier site)
        |> enter_subgroup_phase site)
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
  | Shfl_sync
  | Shfl_down_sync

let subgroup_collective_call_name : subgroup_collective_call -> string =
  function
  | Warp_sum -> "warp_sum"
  | Warp_max -> "warp_max"
  | Shfl_sync -> "__shfl_sync"
  | Shfl_down_sync -> "__shfl_down_sync"

let subgroup_collective_call_of_call (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : subgroup_collective_call option =
  match (call_name func, List.length args) with
  | Some "warp_sum", 1 -> Some Warp_sum
  | Some "warp_max", 1 -> Some Warp_max
  | Some "__shfl_sync", 3 -> Some Shfl_sync
  | Some "__shfl_down_sync", 3 -> Some Shfl_down_sync
  | _ -> None

let subgroup_collective_result (site : SM.Site.t) : Variable.t =
  Variable.from_name
    ("__subgroup_collective_result_" ^ string_of_int (SM.Site.id site))

let subgroup_collective_operand ~(context : string) (expr : D_lang.Expr.t) :
    (SM.Collective.operand, error) result =
  nexp_of_expr ~context expr
  |> Result.map (fun expr -> SM.Collective.Numeric expr)

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
    | Warp_sum, [ value ] ->
        let* argument =
          subgroup_collective_operand ~context:"warp_sum operand" value
        in
        Ok
          (SM.Collective.Operation_payload
             {
               op = SM.Collective.Add;
               collective_op = SM.Collective.Reduce;
               argument;
               result;
             })
    | Warp_max, [ value ] ->
        let* argument =
          subgroup_collective_operand ~context:"warp_max operand" value
        in
        Ok
          (SM.Collective.Operation_payload
             {
               op = SM.Collective.Max;
               collective_op = SM.Collective.Reduce;
               argument;
               result;
             })
    | Shfl_sync, [ _mask; value; lane ] ->
        let* lane = nexp_of_expr ~context:"__shfl_sync lane" lane in
        let* argument =
          subgroup_collective_operand ~context:"__shfl_sync operand" value
        in
        Ok
          (SM.Collective.Gather_payload
             { mode = SM.Collective.Broadcast lane; argument; result })
    | Shfl_down_sync, [ _mask; value; delta ] ->
        let* delta = nexp_of_expr ~context:"__shfl_down_sync delta" delta in
        let* argument =
          subgroup_collective_operand ~context:"__shfl_down_sync operand" value
        in
        Ok
          (SM.Collective.Gather_payload
             { mode = SM.Collective.Shuffle_down delta; argument; result })
    | _ -> unexpected_shape ()
  in
  let state = record_site_control site state in
  Ok
    (state
    |> add_stmt (SM.Stmt.Subgroup_collective (SM.Collective.make site payload))
    |> enter_subgroup_phase site)

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

let classify_call ?result (state : collect_state) (func : D_lang.Expr.t)
    (args : D_lang.Expr.t list) : (collect_state, error) result =
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
  | Skip | BreakStmt | GotoStmt | ReturnStmt None | ContinueStmt -> false
  | ReturnStmt (Some expr) -> expr_requires_subgroup expr

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
  | ContinueStmt ->
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
  | GotoStmt | ReturnStmt _ | ContinueStmt ->
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
  | GotoStmt | ReturnStmt _ | ContinueStmt ->
      false

let stmt_updates_source_facts (stmt : D_lang.Stmt.t) : bool =
  stmt_updates_scalar_facts stmt || stmt_updates_pointer_facts stmt

let stmt_requires_source_effect_metadata (stmt : D_lang.Stmt.t) : bool =
  stmt_requires_subgroup stmt
  || stmt_records_ordinary_memory_effect stmt
  || stmt_updates_source_facts stmt

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

let rec collect_expr ?result (state : collect_state) (expr : D_lang.Expr.t) :
    (collect_state, error) result =
  let ( let* ) = Result.bind in
  match expr with
  | BinaryOperator
      { opcode = "="; lhs = Ident { name = target; ty; _ }; rhs; _ }
    when not (j_type_is_pointer_like ty) ->
      let state = update_scalar_facts_from_expr state target rhs in
      collect_expr ~result:target state rhs
  | CallExpr { func; args; _ } ->
      let* state = classify_call ?result state func args in
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
  | SizeOfExpr _ | CXXNewExpr _ | CXXDeleteExpr _ | RecoveryExpr _
  | CharacterLiteral _ | CXXBoolLiteralExpr _ | FloatingLiteral _
  | IntegerLiteral _ | Ident _ | UnresolvedLookupExpr _ | CXXOperatorCallExpr _
    ->
      Ok state

let collect_decl (state : collect_state) (decl : D_lang.Decl.t) :
    (collect_state, error) result =
  match decl.init with
  | Some (IExpr rhs) when j_type_is_pointer_like decl.ty ->
      let ( let* ) = Result.bind in
      let* source, offset =
        pointer_base_offset state ~context:"pointer alias initializer" rhs
      in
      Ok (add_pointer_alias decl.var { source; offset } state)
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

let rec collect_stmt (state : collect_state) (stmt : D_lang.Stmt.t) :
    (collect_state, error) result =
  let ( let* ) = Result.bind in
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
    when j_type_is_pointer_like ty ->
      let* source, offset =
        pointer_base_offset state ~context:"pointer alias assignment" rhs
      in
      Ok (add_pointer_alias target { source; offset } state)
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
          Ok (add_decl_facts state decl))
        (Ok state) decls
  | Seq (left, right) ->
      let* state = collect_stmt state left in
      collect_stmt state right
  | IfStmt { cond; then_stmt; else_stmt } ->
      let* state = collect_expr state cond in
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
      let else_entry = restore_scalar_facts then_exit (scalar_facts_of state) in
      let* state = collect_branch else_entry else_guard else_stmt in
      Ok
        (restore_scalar_facts state
           (join_scalar_facts [ then_exit; else_exit_for_join ]))
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
  | WriteAccessStmt write ->
      let* state =
        record_ordinary_memory_effect ~kind:Ordinary_write
          ~mode:(Write write.payload) ~context:"ordinary write access index"
          write.target state
      in
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state)
        (write.source :: write.target.index)
  | ReadAccessStmt read ->
      let* state =
        record_ordinary_memory_effect ~kind:Ordinary_read ~mode:Read
          ~context:"ordinary read access index" read.source state
      in
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state) read.source.index
  | AtomicAccessStmt atomic ->
      let* state =
        record_ordinary_memory_effect ~kind:Ordinary_atomic
          ~mode:(Atomic atomic.atomic) ~context:"ordinary atomic access index"
          atomic.source state
      in
      List.fold_left
        (fun state expr ->
          let* state = state in
          collect_expr state expr)
        (Ok state) atomic.source.index
  | Skip | BreakStmt | GotoStmt | ReturnStmt None | ContinueStmt -> Ok state
  | ReturnStmt (Some expr) -> collect_expr state expr

let ordinary_imp_of_kernel (context_defs : D_lang.Def.t list)
    (kernel : D_lang.Kernel.t) : (Imp.Kernel.t, error) result =
  try
    match
      D_to_imp.Silent.parse_program (context_defs @ [ D_lang.Def.Kernel kernel ])
    with
    | [ kernel ] -> Ok kernel
    | kernels ->
        Error
          (Ordinary_imp_error
             (Printf.sprintf "expected one ordinary Imp kernel, got %d"
                (List.length kernels)))
  with D_to_imp.Unsupported_source msg -> Error (Ordinary_imp_error msg)

let type_aliases_of_defs (context_defs : D_lang.Def.t list) :
    C_type.t StringMap.t =
  List.fold_left
    (fun aliases -> function
      | D_lang.Def.Typedef typedef ->
          StringMap.add typedef.name typedef.ty aliases
      | D_lang.Def.Declaration _ | D_lang.Def.Kernel _ | D_lang.Def.Enum _ ->
          aliases)
    StringMap.empty context_defs

let uniform_vars_of_kernel_params (kernel : D_lang.Kernel.t) : Variable.Set.t =
  List.fold_left
    (fun vars param -> Variable.Set.add (D_lang.Param.name param) vars)
    Variable.Set.empty kernel.params

let seed_context_declaration_facts (state : collect_state)
    (context_defs : D_lang.Def.t list) : collect_state =
  List.fold_left
    (fun state -> function
      | D_lang.Def.Declaration decl -> add_decl_facts state decl
      | Typedef _ | Enum _ | Kernel _ -> state)
    state context_defs

let subgroup_kernel_of_kernel (context_defs : D_lang.Def.t list)
    (target_config : SM.Target_config.t) (kernel : D_lang.Kernel.t) :
    (subgroup_kernel, error) result =
  let ( let* ) = Result.bind in
  match SM.Target_config.cuda_x_contiguous_subgroup_size target_config with
  | None -> Error (Missing_subgroup_config { kernel = kernel.name })
  | Some _ ->
      let state =
        empty_collect_state ~target_config
          ~uniform_vars:(uniform_vars_of_kernel_params kernel)
          (type_aliases_of_defs context_defs)
        |> fun state -> seed_context_declaration_facts state context_defs
      in
      let* state = collect_stmt state kernel.code in
      Ok
        {
          matrix_kernel =
            SM.Kernel.make ~target_config ~name:kernel.name
              (List.rev state.stmts_rev);
          site_controls = List.rev state.site_controls_rev;
          uniform_vars = state.uniform_vars;
          memory_globals = state.memory_globals;
          ordinary_memory_effects = List.rev state.ordinary_memory_effects_rev;
        }

let route_kernel ?(target_config = SM.Target_config.missing_cuda)
    ?(context_defs = []) (kernel : D_lang.Kernel.t) :
    (routed_kernel, error) result =
  if stmt_requires_subgroup kernel.code then
    subgroup_kernel_of_kernel context_defs target_config kernel
    |> Result.map (fun kernel -> Subgroup_matrix kernel)
  else
    ordinary_imp_of_kernel context_defs kernel
    |> Result.map (fun kernel -> Ordinary_imp kernel)

let route_program ?target_config (program : D_lang.Program.t) :
    (routed_kernel list, error) result =
  let ( let* ) = Result.bind in
  let context_defs =
    List.filter
      (function
        | D_lang.Def.Kernel _ -> false
        | Declaration _ | Typedef _ | Enum _ -> true)
      program
  in
  program
  |> List.fold_left
       (fun routed def ->
         let* routed = routed in
         match def with
         | D_lang.Def.Kernel kernel ->
             let* kernel = route_kernel ?target_config ~context_defs kernel in
             Ok (kernel :: routed)
         | Declaration _ | Typedef _ | Enum _ -> Ok routed)
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
    Printf.sprintf "site#%d order#%d control=%s%s uniform_vars=%s"
      control.site_id control.source_order rendered memory_rendered uniform_vars
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
