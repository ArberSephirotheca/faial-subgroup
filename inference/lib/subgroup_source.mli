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

and ordinary_memory_phase = {
  workgroup : int;
  subgroup : Subgroup_matrix.Site.id list;
}

and ordinary_memory_effect = {
  kind : ordinary_memory_kind;
  site : ordinary_memory_site;
  access : Protocols.Access.t;
  source_conditions : Protocols.Exp.bexp list;
  runtime_condition : Protocols.Exp.bexp option;
  phase : ordinary_memory_phase;
  target_config : Subgroup_matrix.Target_config.t;
}

and site_control = {
  site_id : Subgroup_matrix.Site.id;
  source_order : int;
  conditions : Protocols.Exp.bexp list;
  memory_conditions : Protocols.Exp.bexp list;
  uniform_vars : Protocols.Variable.Set.t;
}

and subgroup_kernel = {
  matrix_kernel : Subgroup_matrix.Kernel.t;
  site_controls : site_control list;
  uniform_vars : Protocols.Variable.Set.t;
  memory_globals : Protocols.Variable.Set.t;
  ordinary_memory_effects : ordinary_memory_effect list;
}

val error_to_string : error -> string
val ordinary_memory_effect_to_string : ordinary_memory_effect -> string
val ordinary_memory_effect_summary : ordinary_memory_effect -> string
val site_control_memory_conditions : site_control -> Protocols.Exp.bexp list

val route_kernel :
  ?target_config:Subgroup_matrix.Target_config.t ->
  ?context_defs:D_lang.Def.t list ->
  D_lang.Kernel.t ->
  (routed_kernel, error) result

val route_program :
  ?target_config:Subgroup_matrix.Target_config.t ->
  D_lang.Program.t ->
  (routed_kernel list, error) result

val kernel_site_summary : Subgroup_matrix.Kernel.t -> string list
val kernel_matrix_footprint_summary : Subgroup_matrix.Kernel.t -> string list
val kernel_artifact_summary : Subgroup_matrix.Kernel.t -> string list
val subgroup_kernel_artifact_summary : subgroup_kernel -> string list
