type row_seed = {
  row_id : string;
  template_arg : string;
  template_value : int;
  source_branch : source_branch;
  evidence_artifact_key : string;
  timeout_ms : int;
}

and source_branch =
  | Guarded_if_branch of { condition : string }
  | Else_branch_after of { if_condition : string }

type launch_builtin_dim =
  | Block_dim_x
  | Block_dim_y
  | Block_dim_z
  | Grid_dim_x
  | Grid_dim_y
  | Grid_dim_z

type launch_nexp =
  | Launch_num of int
  | Launch_var of string
  | Launch_builtin of launch_builtin_dim
  | Launch_div of launch_nexp * launch_nexp
  | Launch_mul of launch_nexp * launch_nexp

type shape_fact =
  | Shape_eq of launch_nexp * launch_nexp
  | Shape_gt of launch_nexp * launch_nexp
  | Shape_ge of launch_nexp * launch_nexp
  | Shape_le of launch_nexp * launch_nexp

type shape_contract = {
  shape_global_ints : string list;
  shape_facts : shape_fact list;
  shape_subgroup_size : int option;
}

type contract_family =
  | Gla
  | Wkv
  | Wkv7
  | Solve_tri_fast
  | Finite_type_template

type template_domain =
  | Int_template_domain of { parameter : string; value : int }
  | Finite_type_domain of { parameter : string; values : string list }

type family_seed = {
  family : Launch_contract_rows.family;
  source_file : string;
  preprocessing_profile : string;
  extraction_fixture : string;
  block_dim_source : string;
  grid_dim_source : string;
  dynamic_shared_memory : string;
  feature_class : string;
  required_semantics : string list;
  rows : row_seed list;
}

type t = {
  contract : Launch_contract_rows.t;
  source_branch : source_branch;
  row_shape_preconditions : string list;
  source_file : string;
  preprocessing_profile : string;
  extraction_fixture : string;
  block_dim_source : string;
  grid_dim_source : string;
  dynamic_shared_memory : string;
  feature_class : string;
  required_semantics : string list;
  evidence_artifact_key : string;
  timeout_ms : int;
  shape_contract : shape_contract;
}

type manifest_facts = {
  row_id : string;
  family_candidates : Launch_contract_rows.family list;
  manifest_kernel : string option;
  parsed_kernel : string option;
  template_arg : string option;
  template_value : int option;
  block_dim_source : string option;
  grid_dim_source : string option;
  source_branch : source_branch option;
  dynamic_shared_memory : string option;
  evidence_artifact_key : string option;
  timeout_ms : int option;
}

type selected_family = Solve_tri_fast

type finite_type_domain = {
  finite_type_parameter : string;
  finite_type_values : string list;
  finite_type_source : string;
}

type finite_type_row = {
  finite_type_row_id : string;
  finite_type_family_key : string;
  finite_type_source_file : string;
  finite_type_manifest_kernel : string;
  finite_type_source_kernel_family : string;
  finite_type_parsed_kernel : string;
  finite_type_template_arg : string;
  finite_type_domains : finite_type_domain list;
  finite_type_block_dim : int list;
  finite_type_grid_dim : int list option;
  finite_type_block_dim_source : string;
  finite_type_grid_dim_source : string;
  finite_type_dynamic_shared_memory : string;
  finite_type_positive_int_params : string list;
  finite_type_positive_shape_guards : string list;
  finite_type_memory_effects : string list;
  finite_type_preprocessing_profile : string;
  finite_type_extraction_fixture : string;
  finite_type_evidence_artifact_key : string;
  finite_type_timeout_ms : int;
}

type launch_value_role =
  | Fixed_by_launch of { value : string; evidence : string }
  | Fixed_by_model_or_template of { value : string; evidence : string }
  | User_symbolic of { domain : string; evidence : string }
  | Derived of {
      expression : string;
      dependencies : string list;
      evidence : string;
    }
  | Equal_to of { variable : string; evidence : string }
  | Profile_bounded of { bound : string; evidence : string }
  | Unknown_blocker of { reason : string }

type context_dim3 = {
  context_dim3_value : int list option;
  context_dim3_source : string;
  context_dim3_role : launch_value_role;
}

type context_positive_param = {
  context_positive_name : string;
  context_positive_guard : string;
  context_positive_role : launch_value_role;
}

type profile_launch_context = {
  context_id : string;
  context_row : finite_type_row;
  context_template_role : launch_value_role;
  context_block_dim : context_dim3;
  context_grid_dim : context_dim3;
  context_dynamic_shared_memory_role : launch_value_role;
  context_positive_params : context_positive_param list;
  context_memory_effect_roles : launch_value_role list;
  context_soundness_boundary : string;
}

type selected_row = {
  selected_row_id : string;
  selected_family : selected_family;
  selected_source_file : string;
  selected_manifest_kernel : string;
  selected_source_kernel_family : string;
  selected_parsed_kernel : string;
  selected_template_arg : string;
  selected_template_bindings : (string * int) list;
  selected_source_branch_conditions : string list;
  selected_preprocessing_profile : string;
  selected_extraction_fixture : string;
  selected_block_dim_source : string;
  selected_grid_dim_source : string;
  selected_concrete_block_dim : int list;
  selected_dynamic_shared_memory : string;
  selected_feature_class : string;
  selected_required_semantics : string list;
  selected_subgroup_helper : string;
  selected_subgroup_size : int;
  selected_evidence_artifact_key : string;
  selected_timeout_ms : int;
}

type solve_tri_fast_row_seed = {
  solve_tri_row_id : string;
  solve_tri_manifest_kernel : string;
  solve_tri_parsed_kernel : string;
  solve_tri_k_template : int;
  solve_tri_branch_case : string;
  solve_tri_preprocessing_profile : string;
  solve_tri_extraction_fixture : string;
  solve_tri_evidence_artifact_key : string;
  solve_tri_timeout_ms : int;
}

type solve_tri_fast_symbolic_row_kind =
  | Symbolic_lookup_anchor
  | Symbolic_unpromoted_candidate

type solve_tri_fast_symbolic_row = {
  solve_tri_symbolic_row_id : string;
  solve_tri_symbolic_manifest_kernel : string;
  solve_tri_symbolic_k_template : int;
  solve_tri_symbolic_branch_case : string;
  solve_tri_symbolic_row_kind : solve_tri_fast_symbolic_row_kind;
}

type solve_tri_fast_family_contract = {
  solve_tri_family : selected_family;
  solve_tri_source_file : string;
  solve_tri_source_kernel_family : string;
  solve_tri_n_template : int;
  solve_tri_block_dim_x : int;
  solve_tri_block_dim_source : string;
  solve_tri_grid_dim_source : string;
  solve_tri_dynamic_shared_memory : string;
  solve_tri_feature_class : string;
  solve_tri_required_semantics : string list;
  solve_tri_subgroup_helper : string;
  solve_tri_subgroup_size : int;
  solve_tri_lookup_rows : solve_tri_fast_row_seed list;
  solve_tri_symbolic_k_rows : solve_tri_fast_symbolic_row list;
  solve_tri_unpromoted_row_ids : string list;
  solve_tri_excluded_row_ids : string list;
}

type solve_tri_symbolic_k_guard = {
  symbolic_guard_family : selected_family;
  symbolic_guard_source_file : string;
  symbolic_guard_source_kernel_family : string;
  symbolic_guard_n_template : int;
  symbolic_guard_k_parameter : string;
  symbolic_guard_k_source : string;
  symbolic_guard_block_dim_relation : string;
  symbolic_guard_dynamic_shared_memory : string;
  symbolic_guard_subgroup_helper : string;
  symbolic_guard_subgroup_size : int;
  symbolic_guard_route_owner : string;
  symbolic_guard_candidate_rows : solve_tri_fast_symbolic_row list;
  symbolic_guard_lookup_anchor_row_ids : string list;
  symbolic_guard_unpromoted_row_ids : string list;
  symbolic_guard_excluded_row_ids : string list;
}

type symbolic_dimension =
  | Concrete_dimension of {
      concrete_dimension_value : int;
      concrete_dimension_source : string;
    }
  | Symbolic_dimension of {
      symbolic_dimension_parameter : string;
      symbolic_dimension_source : string;
      symbolic_dimension_candidate_values : int list;
      symbolic_dimension_positive_guard : string;
    }

type symbolic_dim3 = {
  symbolic_dim_x : symbolic_dimension;
  symbolic_dim_y : symbolic_dimension;
  symbolic_dim_z : symbolic_dimension;
}

type solve_tri_symbolic_dimension_carrier = {
  carrier_family : selected_family;
  carrier_source_file : string;
  carrier_source_kernel_family : string;
  carrier_source_width_variable : string;
  carrier_source_width_relation : string;
  carrier_template_dimensions : (string * symbolic_dimension) list;
  carrier_block_dim : symbolic_dim3;
  carrier_grid_dim_source : string;
  carrier_dynamic_shared_memory : string;
  carrier_launch_branch_conditions : string list;
  carrier_positive_shape_guards : string list;
  carrier_subgroup_size : int;
  carrier_route_owner : string;
  carrier_candidate_rows : solve_tri_fast_symbolic_row list;
  carrier_lookup_anchor_row_ids : string list;
  carrier_unpromoted_row_ids : string list;
  carrier_excluded_row_ids : string list;
}

type launch_contract_row = {
  launch_row_id : string;
  launch_family : contract_family;
  launch_manifest_kernel : string;
  launch_parsed_kernel : string;
  launch_template_arg : string;
  launch_template_param : string option;
  launch_template_value : int option;
  launch_template_bindings : (string * int) list;
  launch_template_domains : template_domain list;
  launch_shape_contract : shape_contract;
  launch_block_dim : int list option;
  launch_symbolic_dimension_carrier :
    solve_tri_symbolic_dimension_carrier option;
}

type guarded_candidate_carrier = {
  candidate_carrier_id : string;
  candidate_priority_bucket : string;
  candidate_first_blocker : string;
  candidate_proof_ladder_stage : string;
  candidate_source_ledger : string;
  candidate_affected_family_count : int;
  candidate_required_fact_keys : string list;
  candidate_route_owner : string;
  candidate_solver_policy : string;
  candidate_admission_status : string;
  candidate_next_support_step : string;
}

type template_argument_resolution_carrier = {
  template_resolution_carrier_id : string;
  template_resolution_source_ledger : string;
  template_resolution_input_blocker_ledger : string;
  template_resolution_proof_ladder_stage : string;
  template_resolution_families_attempted : int;
  template_resolution_rows_attempted : int;
  template_resolution_rows_known : int;
  template_resolution_rows_blocked : int;
  template_resolution_families_known : int;
  template_resolution_families_blocked : int;
  template_resolution_family_resolution_counts : (string * int) list;
  template_resolution_row_resolution_counts : (string * int) list;
  template_resolution_route_owner : string;
  template_resolution_solver_policy : string;
  template_resolution_admission_status : string;
  template_resolution_next_support_step : string;
}

type launch_branch_frontier_carrier = {
  launch_branch_carrier_id : string;
  launch_branch_source_ledger : string;
  launch_branch_input_frontier_ledger : string;
  launch_branch_proof_ladder_stage : string;
  launch_branch_families_attempted : int;
  launch_branch_rows_attempted : int;
  launch_branch_rows_known : int;
  launch_branch_rows_blocked : int;
  launch_branch_families_known : int;
  launch_branch_families_blocked : int;
  launch_branch_row_resolution_counts : (string * int) list;
  launch_branch_family_status_counts : (string * int) list;
  launch_branch_next_blocker_counts : (string * int) list;
  launch_branch_remaining_blocker_counts : (string * int) list;
  launch_branch_route_owner : string;
  launch_branch_solver_policy : string;
  launch_branch_admission_status : string;
  launch_branch_next_support_step : string;
}

type positive_shape_guard_family = {
  positive_shape_family_id : string;
  positive_shape_family_key : string;
  positive_shape_rows : string list;
  positive_shape_origin : string;
  positive_shape_first_blocker : string;
}

type positive_shape_guard_carrier = {
  positive_shape_carrier_id : string;
  positive_shape_source_ledger : string;
  positive_shape_input_frontier_ledger : string;
  positive_shape_proof_ladder_stage : string;
  positive_shape_families_attempted : int;
  positive_shape_rows_attempted : int;
  positive_shape_families_from_s477 : int;
  positive_shape_rows_from_s477 : int;
  positive_shape_preexisting_families : int;
  positive_shape_preexisting_rows : int;
  positive_shape_required_fact_keys : string list;
  positive_shape_candidate_families : positive_shape_guard_family list;
  positive_shape_next_blocker_counts : (string * int) list;
  positive_shape_route_owner : string;
  positive_shape_solver_policy : string;
  positive_shape_admission_status : string;
  positive_shape_next_support_step : string;
}

type positive_shape_verification_carrier = {
  positive_shape_verification_carrier_id : string;
  positive_shape_verification_source_ledger : string;
  positive_shape_verification_family_ledger : string;
  positive_shape_verification_artifact_dir : string;
  positive_shape_verification_families_attempted : int;
  positive_shape_verification_rows_attempted : int;
  positive_shape_verification_source_slice_verified_families : int;
  positive_shape_verification_existing_guarded_families : int;
  positive_shape_verification_blocked_families : int;
  positive_shape_verification_source_slice_artifacts : int;
  positive_shape_verification_exact_production_promotions : int;
  positive_shape_verification_manifest_verdict_fields_changed : bool;
  positive_shape_verification_family_status_counts : (string * int) list;
  positive_shape_verification_row_status_counts : (string * int) list;
  positive_shape_verification_candidate_families :
    positive_shape_guard_family list;
  positive_shape_verification_evidence_policy : string;
  positive_shape_verification_admission_status : string;
  positive_shape_verification_next_support_step : string;
}

type positive_shape_production_promotion_carrier = {
  positive_shape_production_carrier_id : string;
  positive_shape_production_source_ledger : string;
  positive_shape_production_input_verification_ledger : string;
  positive_shape_production_artifact_dir : string;
  positive_shape_production_families_attempted : int;
  positive_shape_production_rows_attempted : int;
  positive_shape_production_extraction_verified_families : int;
  positive_shape_production_profile_verified_families : int;
  positive_shape_production_source_slice_only_remaining : int;
  positive_shape_production_existing_guarded_families_preserved : int;
  positive_shape_production_exact_manifest_promotions : int;
  positive_shape_production_manifest_verdict_fields_changed : bool;
  positive_shape_production_status_counts : (string * int) list;
  positive_shape_production_family_specs : string list;
  positive_shape_production_evidence_policy : string;
  positive_shape_production_admission_status : string;
  positive_shape_production_next_support_step : string;
}

type exact_evidence_manifest_promotion_policy_carrier = {
  exact_policy_carrier_id : string;
  exact_policy_input_ledgers : string list;
  exact_policy_admissible_evidence_classes : string list;
  exact_policy_non_promoting_evidence_classes : string list;
  exact_policy_required_row_fact_keys : string list;
  exact_policy_row_local_manifest_status : string;
  exact_policy_guarded_family_manifest_status : string;
  exact_policy_source_slice_only_status : string;
  exact_policy_profile_only_status : string;
  exact_policy_exact_row_ids : string list;
  exact_policy_exact_row_count : int;
  exact_policy_guarded_family_ids : string list;
  exact_policy_guarded_family_count : int;
  exact_policy_manifest_status_counts : (string * int) list;
  exact_policy_blocked_boundary_counts : (string * int) list;
  exact_policy_manifest_verdict_fields_changed : bool;
  exact_policy_soundness_boundary : string;
  exact_policy_admission_status : string;
  exact_policy_next_support_step : string;
}

type symbolic_obligation_blocker = {
  blocker_route_owner : string;
  blocker_symbolic_parameter : string;
  blocker_reason : string;
  blocker_zero_obligation_cause : string;
  blocker_next_step : string;
}

type selected_manifest_facts = {
  selected_fact_row_id : string;
  selected_family_candidates : selected_family list;
  selected_fact_manifest_kernel : string option;
  selected_fact_source_file : string option;
  selected_fact_source_kernel_family : string option;
  selected_fact_parsed_kernel : string option;
  selected_fact_template_arg : string option;
  selected_fact_block_dim_source : string option;
  selected_fact_grid_dim_source : string option;
  selected_fact_source_branch_conditions : string list option;
  selected_fact_dynamic_shared_memory : string option;
  selected_fact_feature_class : string option;
  selected_fact_required_semantics : string list option;
  selected_fact_drf_status : string option;
  selected_fact_artifact_status : string option;
  selected_fact_preprocessing_profile : string option;
  selected_fact_extraction_fixture : string option;
  selected_fact_subgroup_helper : string option;
  selected_fact_subgroup_size : int option;
  selected_fact_concrete_block_dim : int list option;
  selected_fact_evidence_artifact_key : string option;
  selected_fact_timeout_ms : int option;
}

type solve_tri_symbolic_k_guard_facts = {
  symbolic_fact_family_candidates : selected_family list;
  symbolic_fact_source_file : string option;
  symbolic_fact_source_kernel_family : string option;
  symbolic_fact_n_template : int option;
  symbolic_fact_k_parameter : string option;
  symbolic_fact_k_source : string option;
  symbolic_fact_candidate_rows : string list option;
  symbolic_fact_lookup_anchor_row_ids : string list option;
  symbolic_fact_unpromoted_row_ids : string list option;
  symbolic_fact_excluded_row_ids : string list option;
  symbolic_fact_block_dim_relation : string option;
  symbolic_fact_dynamic_shared_memory : string option;
  symbolic_fact_subgroup_helper : string option;
  symbolic_fact_subgroup_size : int option;
  symbolic_fact_route_owner : string option;
}

type solve_tri_symbolic_dimension_carrier_facts = {
  carrier_fact_family_candidates : selected_family list;
  carrier_fact_source_file : string option;
  carrier_fact_source_kernel_family : string option;
  carrier_fact_source_width_variable : string option;
  carrier_fact_source_width_relation : string option;
  carrier_fact_template_dimensions : string list option;
  carrier_fact_symbolic_parameter : string option;
  carrier_fact_symbolic_candidate_values : int list option;
  carrier_fact_block_dim : string option;
  carrier_fact_grid_dim_source : string option;
  carrier_fact_dynamic_shared_memory : string option;
  carrier_fact_launch_branch_conditions : string list option;
  carrier_fact_positive_shape_guards : string list option;
  carrier_fact_subgroup_size : int option;
  carrier_fact_route_owner : string option;
  carrier_fact_candidate_rows : string list option;
  carrier_fact_lookup_anchor_row_ids : string list option;
  carrier_fact_unpromoted_row_ids : string list option;
  carrier_fact_excluded_row_ids : string list option;
}

type guarded_candidate_carrier_facts = {
  candidate_fact_carrier_id : string;
  candidate_fact_priority_bucket : string option;
  candidate_fact_first_blocker : string option;
  candidate_fact_proof_ladder_stage : string option;
  candidate_fact_source_ledger : string option;
  candidate_fact_affected_family_count : int option;
  candidate_fact_required_fact_keys : string list option;
  candidate_fact_route_owner : string option;
  candidate_fact_solver_policy : string option;
  candidate_fact_admission_status : string option;
  candidate_fact_next_support_step : string option;
}

type template_argument_resolution_facts = {
  template_resolution_fact_carrier_id : string;
  template_resolution_fact_source_ledger : string option;
  template_resolution_fact_input_blocker_ledger : string option;
  template_resolution_fact_proof_ladder_stage : string option;
  template_resolution_fact_families_attempted : int option;
  template_resolution_fact_rows_attempted : int option;
  template_resolution_fact_rows_known : int option;
  template_resolution_fact_rows_blocked : int option;
  template_resolution_fact_families_known : int option;
  template_resolution_fact_families_blocked : int option;
  template_resolution_fact_family_resolution_counts :
    (string * int) list option;
  template_resolution_fact_row_resolution_counts : (string * int) list option;
  template_resolution_fact_route_owner : string option;
  template_resolution_fact_solver_policy : string option;
  template_resolution_fact_admission_status : string option;
  template_resolution_fact_next_support_step : string option;
}

type launch_branch_frontier_facts = {
  launch_branch_fact_carrier_id : string;
  launch_branch_fact_source_ledger : string option;
  launch_branch_fact_input_frontier_ledger : string option;
  launch_branch_fact_proof_ladder_stage : string option;
  launch_branch_fact_families_attempted : int option;
  launch_branch_fact_rows_attempted : int option;
  launch_branch_fact_rows_known : int option;
  launch_branch_fact_rows_blocked : int option;
  launch_branch_fact_families_known : int option;
  launch_branch_fact_families_blocked : int option;
  launch_branch_fact_row_resolution_counts : (string * int) list option;
  launch_branch_fact_family_status_counts : (string * int) list option;
  launch_branch_fact_next_blocker_counts : (string * int) list option;
  launch_branch_fact_remaining_blocker_counts : (string * int) list option;
  launch_branch_fact_route_owner : string option;
  launch_branch_fact_solver_policy : string option;
  launch_branch_fact_admission_status : string option;
  launch_branch_fact_next_support_step : string option;
}

type positive_shape_guard_facts = {
  positive_shape_fact_carrier_id : string;
  positive_shape_fact_source_ledger : string option;
  positive_shape_fact_input_frontier_ledger : string option;
  positive_shape_fact_proof_ladder_stage : string option;
  positive_shape_fact_families_attempted : int option;
  positive_shape_fact_rows_attempted : int option;
  positive_shape_fact_families_from_s477 : int option;
  positive_shape_fact_rows_from_s477 : int option;
  positive_shape_fact_preexisting_families : int option;
  positive_shape_fact_preexisting_rows : int option;
  positive_shape_fact_required_fact_keys : string list option;
  positive_shape_fact_candidate_family_specs : string list option;
  positive_shape_fact_next_blocker_counts : (string * int) list option;
  positive_shape_fact_route_owner : string option;
  positive_shape_fact_solver_policy : string option;
  positive_shape_fact_admission_status : string option;
  positive_shape_fact_next_support_step : string option;
}

type positive_shape_verification_facts = {
  positive_shape_verification_fact_carrier_id : string;
  positive_shape_verification_fact_source_ledger : string option;
  positive_shape_verification_fact_family_ledger : string option;
  positive_shape_verification_fact_artifact_dir : string option;
  positive_shape_verification_fact_families_attempted : int option;
  positive_shape_verification_fact_rows_attempted : int option;
  positive_shape_verification_fact_source_slice_verified_families : int option;
  positive_shape_verification_fact_existing_guarded_families : int option;
  positive_shape_verification_fact_blocked_families : int option;
  positive_shape_verification_fact_source_slice_artifacts : int option;
  positive_shape_verification_fact_exact_production_promotions : int option;
  positive_shape_verification_fact_manifest_verdict_fields_changed :
    bool option;
  positive_shape_verification_fact_family_status_counts :
    (string * int) list option;
  positive_shape_verification_fact_row_status_counts :
    (string * int) list option;
  positive_shape_verification_fact_candidate_family_specs : string list option;
  positive_shape_verification_fact_evidence_policy : string option;
  positive_shape_verification_fact_admission_status : string option;
  positive_shape_verification_fact_next_support_step : string option;
}

type positive_shape_production_promotion_facts = {
  positive_shape_production_fact_carrier_id : string;
  positive_shape_production_fact_source_ledger : string option;
  positive_shape_production_fact_input_verification_ledger : string option;
  positive_shape_production_fact_artifact_dir : string option;
  positive_shape_production_fact_families_attempted : int option;
  positive_shape_production_fact_rows_attempted : int option;
  positive_shape_production_fact_extraction_verified_families : int option;
  positive_shape_production_fact_profile_verified_families : int option;
  positive_shape_production_fact_source_slice_only_remaining : int option;
  positive_shape_production_fact_existing_guarded_families_preserved :
    int option;
  positive_shape_production_fact_exact_manifest_promotions : int option;
  positive_shape_production_fact_manifest_verdict_fields_changed : bool option;
  positive_shape_production_fact_status_counts : (string * int) list option;
  positive_shape_production_fact_family_specs : string list option;
  positive_shape_production_fact_evidence_policy : string option;
  positive_shape_production_fact_admission_status : string option;
  positive_shape_production_fact_next_support_step : string option;
}

type exact_evidence_manifest_promotion_policy_facts = {
  exact_policy_fact_carrier_id : string;
  exact_policy_fact_input_ledgers : string list option;
  exact_policy_fact_admissible_evidence_classes : string list option;
  exact_policy_fact_non_promoting_evidence_classes : string list option;
  exact_policy_fact_required_row_fact_keys : string list option;
  exact_policy_fact_row_local_manifest_status : string option;
  exact_policy_fact_guarded_family_manifest_status : string option;
  exact_policy_fact_source_slice_only_status : string option;
  exact_policy_fact_profile_only_status : string option;
  exact_policy_fact_exact_row_ids : string list option;
  exact_policy_fact_exact_row_count : int option;
  exact_policy_fact_guarded_family_ids : string list option;
  exact_policy_fact_guarded_family_count : int option;
  exact_policy_fact_manifest_status_counts : (string * int) list option;
  exact_policy_fact_blocked_boundary_counts : (string * int) list option;
  exact_policy_fact_manifest_verdict_fields_changed : bool option;
  exact_policy_fact_soundness_boundary : string option;
  exact_policy_fact_admission_status : string option;
  exact_policy_fact_next_support_step : string option;
}

type guarded_candidate_row_facts = {
  candidate_row_fact_carrier_id : string;
  candidate_row_fact_row_id : string;
  candidate_row_fact_family_key : string option;
  candidate_row_fact_source_file : string option;
  candidate_row_fact_kernel_or_template : string option;
  candidate_row_fact_first_blocker : string option;
  candidate_row_fact_required_fact_statuses : (string * string) list option;
  candidate_row_fact_carrier_readiness_status : string option;
  candidate_row_fact_missing_fact_if_any : string list option;
  candidate_row_fact_solver_policy : string option;
  candidate_row_fact_admission_status : string option;
  candidate_row_fact_fresh_obligation_artifacts : int option;
  candidate_row_fact_solver_runs : int option;
  candidate_row_fact_pre_solver_runs : int option;
  candidate_row_fact_new_guarded_family_admissions : int option;
  candidate_row_fact_manifest_verdict_fields_changed : bool option;
  candidate_row_fact_lookup_rows_added : bool option;
  candidate_row_fact_shortcut_keying_used : bool option;
}

type error =
  | Unknown_generated_row of string
  | Duplicate_generated_row of string

type validation_error =
  | Missing_field of { row_id : string; field : string }
  | Ambiguous_field of { row_id : string; field : string; values : string list }
  | Field_mismatch of {
      row_id : string;
      field : string;
      expected : string;
      actual : string;
    }

let error_to_string = function
  | Unknown_generated_row row_id ->
      "unknown generated launch-contract row " ^ row_id
  | Duplicate_generated_row row_id ->
      "duplicate generated launch-contract row " ^ row_id

let family_to_string = function
  | Launch_contract_rows.Gla -> "gla"
  | Wkv -> "wkv"
  | Wkv7 -> "wkv7"

let selected_family_to_string (family : selected_family) =
  match family with Solve_tri_fast -> "solve_tri_fast"

let solve_tri_symbolic_row_kind_to_string = function
  | Symbolic_lookup_anchor -> "lookup_anchor"
  | Symbolic_unpromoted_candidate -> "unpromoted_candidate"

let source_branch_to_string = function
  | Guarded_if_branch { condition } -> "if(" ^ condition ^ ")"
  | Else_branch_after { if_condition } -> "else after if(" ^ if_condition ^ ")"

let validation_error_to_string = function
  | Missing_field { row_id; field } ->
      "generated row " ^ row_id ^ " is missing " ^ field
  | Ambiguous_field { row_id; field; values } ->
      "generated row " ^ row_id ^ " has ambiguous " ^ field ^ ": "
      ^ String.concat ", " values
  | Field_mismatch { row_id; field; expected; actual } ->
      "generated row " ^ row_id ^ " has " ^ field ^ " " ^ actual ^ ", expected "
      ^ expected

let starts_with ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal (String.sub value 0 prefix_len) prefix

let family_candidates_of_manifest_kernel manifest_kernel =
  [
    (Launch_contract_rows.Gla, "gated_linear_attn_f32<");
    (Wkv, "rwkv_wkv_f32<");
    (Wkv7, "rwkv_wkv7_f32<");
  ]
  |> List.filter_map (fun (family, prefix) ->
      if starts_with ~prefix manifest_kernel then Some family else None)

let selected_family_candidates_of_manifest_kernel manifest_kernel =
  [ ((Solve_tri_fast : selected_family), "solve_tri_f32_fast<") ]
  |> List.filter_map (fun (family, prefix) ->
      if starts_with ~prefix manifest_kernel then Some family else None)

let shared_memory_required_semantics =
  [
    "ordinary_memory_effects";
    "shared_memory_address_space";
    "workgroup_barrier_phase_splitting";
    "dynamic_shared_memory_accounting_when_nonzero";
  ]

let solve_tri_fast_required_semantics =
  shared_memory_required_semantics
  @ [
      "explicit_subgroup_target_config";
      "subgroup_sync_or_shuffle_semantics";
      "separate_workgroup_and_subgroup_phases";
      "subgroup_uniform_participation_check";
    ]

let gla_seed =
  {
    family = Gla;
    source_file = "llama.cpp/ggml/src/ggml-cuda/gla.cu";
    preprocessing_profile =
      "agent_results/rewrite/component_summaries/G504R/preprocessing_profile.md";
    extraction_fixture =
      "agent_results/rewrite/component_summaries/G504R/overlay/gla.cu";
    block_dim_source = "C / H";
    grid_dim_source = "B * H";
    dynamic_shared_memory = "0";
    feature_class = "shared_memory_syncthreads";
    required_semantics = shared_memory_required_semantics;
    rows =
      [
        {
          row_id = "L072";
          template_arg = "64";
          template_value = 64;
          source_branch = Guarded_if_branch { condition = "C / H == 64" };
          evidence_artifact_key = "g504r_launch_contract";
          timeout_ms = 1000;
        };
        {
          row_id = "L073";
          template_arg = "128";
          template_value = 128;
          source_branch = Else_branch_after { if_condition = "C / H == 64" };
          evidence_artifact_key = "h501_launch_contract";
          timeout_ms = 10000;
        };
      ];
  }

let wkv_seed =
  {
    family = Wkv;
    source_file = "llama.cpp/ggml/src/ggml-cuda/wkv.cu";
    preprocessing_profile =
      "agent_results/rewrite/component_summaries/H502/preprocessing_profile.md";
    extraction_fixture =
      "agent_results/rewrite/component_summaries/G504/overlay/wkv.cu";
    block_dim_source = "C / H";
    grid_dim_source = "B * H";
    dynamic_shared_memory = "0";
    feature_class = "shared_memory_syncthreads";
    required_semantics = shared_memory_required_semantics;
    rows =
      [
        {
          row_id = "L143";
          template_arg = "CUDA_WKV_BLOCK_SIZE";
          template_value = 64;
          source_branch =
            Guarded_if_branch { condition = "C / H == CUDA_WKV_BLOCK_SIZE" };
          evidence_artifact_key = "h502_launch_contract";
          timeout_ms = 10000;
        };
        {
          row_id = "L144";
          template_arg = "CUDA_WKV_BLOCK_SIZE * 2";
          template_value = 128;
          source_branch =
            Else_branch_after { if_condition = "C / H == CUDA_WKV_BLOCK_SIZE" };
          evidence_artifact_key = "h506_launch_contract";
          timeout_ms = 10000;
        };
      ];
  }

let wkv7_seed =
  {
    family = Wkv7;
    source_file = "llama.cpp/ggml/src/ggml-cuda/wkv.cu";
    preprocessing_profile =
      "agent_results/rewrite/component_summaries/H502/preprocessing_profile.md";
    extraction_fixture =
      "agent_results/rewrite/component_summaries/G504/overlay/wkv.cu";
    block_dim_source = "C / H";
    grid_dim_source = "B * H";
    dynamic_shared_memory = "0";
    feature_class = "shared_memory_syncthreads";
    required_semantics = shared_memory_required_semantics;
    rows =
      [
        {
          row_id = "L145";
          template_arg = "CUDA_WKV_BLOCK_SIZE";
          template_value = 64;
          source_branch =
            Guarded_if_branch { condition = "C / H == CUDA_WKV_BLOCK_SIZE" };
          evidence_artifact_key = "h507_launch_contract";
          timeout_ms = 10000;
        };
        {
          row_id = "L146";
          template_arg = "CUDA_WKV_BLOCK_SIZE * 2";
          template_value = 128;
          source_branch =
            Else_branch_after { if_condition = "C / H == CUDA_WKV_BLOCK_SIZE" };
          evidence_artifact_key = "h508_launch_contract";
          timeout_ms = 10000;
        };
      ];
  }

let family_seeds = [ gla_seed; wkv_seed; wkv7_seed ]

let int_template_domain parameter value =
  Int_template_domain { parameter; value }

let finite_type_template_domain domain =
  Finite_type_domain
    {
      parameter = domain.finite_type_parameter;
      values = domain.finite_type_values;
    }

let launch_value_role_blocks_exact = function
  | Unknown_blocker _ | Profile_bounded _ -> true
  | Fixed_by_launch _ | Fixed_by_model_or_template _ | User_symbolic _
  | Derived _ | Equal_to _ ->
      false

let launch_value_role_to_string = function
  | Fixed_by_launch { value; evidence } ->
      "fixed_by_launch(" ^ value ^ ") from " ^ evidence
  | Fixed_by_model_or_template { value; evidence } ->
      "fixed_by_model_or_template(" ^ value ^ ") from " ^ evidence
  | User_symbolic { domain; evidence } ->
      "user_symbolic(" ^ domain ^ ") from " ^ evidence
  | Derived { expression; dependencies; evidence } ->
      "derived(" ^ expression ^ "; deps="
      ^ String.concat "," dependencies
      ^ ") from " ^ evidence
  | Equal_to { variable; evidence } ->
      "equal_to(" ^ variable ^ ") from " ^ evidence
  | Profile_bounded { bound; evidence } ->
      "profile_bounded(" ^ bound ^ ") from " ^ evidence
  | Unknown_blocker { reason } -> "unknown_blocker(" ^ reason ^ ")"

let profile_launch_context_roles context =
  [
    ("template", context.context_template_role);
    ("block_dim", context.context_block_dim.context_dim3_role);
    ("grid_dim", context.context_grid_dim.context_dim3_role);
    ("dynamic_shared_memory", context.context_dynamic_shared_memory_role);
  ]
  @ List.map
      (fun param ->
        ("positive:" ^ param.context_positive_name, param.context_positive_role))
      context.context_positive_params
  @ List.mapi
      (fun index role -> ("memory_effect:" ^ string_of_int index, role))
      context.context_memory_effect_roles

let profile_launch_context_blockers context =
  profile_launch_context_roles context
  |> List.filter_map (fun (field, role) ->
      if launch_value_role_blocks_exact role then
        Some (field ^ "=" ^ launch_value_role_to_string role)
      else None)

let require_exact_profile_launch_context context =
  match profile_launch_context_blockers context with
  | [] -> ()
  | blockers ->
      invalid_arg
        ("profile launch context " ^ context.context_id
       ^ " is not exact-launch-contract ready: "
        ^ String.concat "; " blockers)

let finite_type_row_of_profile_launch_context context =
  require_exact_profile_launch_context context;
  context.context_row

let profile_launch_context_role_lines context =
  ("context_id: " ^ context.context_id)
  :: ("row_id: " ^ context.context_row.finite_type_row_id)
  :: ("soundness_boundary: " ^ context.context_soundness_boundary)
  :: (profile_launch_context_roles context
     |> List.map (fun (field, role) ->
         field ^ ": " ^ launch_value_role_to_string role))

let eq lhs rhs = Shape_eq (lhs, rhs)
let gt lhs rhs = Shape_gt (lhs, rhs)
let ge lhs rhs = Shape_ge (lhs, rhs)
let le lhs rhs = Shape_le (lhs, rhs)
let num value = Launch_num value
let named name = Launch_var name
let dim dim = Launch_builtin dim
let positive name = gt (named name) (num 0)

let dim3_eq_facts = function
  | [ x; y; z ] ->
      [
        eq (dim Block_dim_x) (num x);
        eq (dim Block_dim_y) (num y);
        eq (dim Block_dim_z) (num z);
      ]
  | _ -> invalid_arg "launch shape requires a three-element dim3"

let grid_dim3_eq_facts = function
  | [ x; y; z ] ->
      [
        eq (dim Grid_dim_x) (num x);
        eq (dim Grid_dim_y) (num y);
        eq (dim Grid_dim_z) (num z);
      ]
  | _ -> invalid_arg "launch shape requires a three-element grid dim3"

let symbolic_grid_facts =
  [
    gt (dim Grid_dim_x) (num 0);
    eq (dim Grid_dim_y) (num 1);
    eq (dim Grid_dim_z) (num 1);
  ]

let standard_row_shape_contract (contract : Launch_contract_rows.t) =
  {
    shape_global_ints = [ "B"; "T"; "C"; "H"; contract.template_param ];
    shape_facts =
      [
        eq (named contract.template_param) (num contract.template_value);
        eq (dim Block_dim_x) (num contract.template_value);
        eq (dim Block_dim_y) (num 1);
        eq (dim Block_dim_z) (num 1);
        eq (Launch_div (named "C", named "H")) (num contract.template_value);
        positive "B";
        positive "T";
        positive "C";
        positive "H";
        eq (dim Grid_dim_x) (Launch_mul (named "B", named "H"));
        eq (dim Grid_dim_y) (num 1);
        eq (dim Grid_dim_z) (num 1);
      ];
    shape_subgroup_size = None;
  }

let selected_row_shape_contract (selected : selected_row) =
  {
    shape_global_ints = List.map fst selected.selected_template_bindings;
    shape_facts =
      (selected.selected_template_bindings
      |> List.concat_map (fun (name, value) ->
          [ eq (named name) (num value); gt (named name) (num 0) ]))
      @ dim3_eq_facts selected.selected_concrete_block_dim
      @ symbolic_grid_facts;
    shape_subgroup_size = Some selected.selected_subgroup_size;
  }

let finite_type_shape_contract (row : finite_type_row) =
  let grid_facts =
    match row.finite_type_grid_dim with
    | Some grid_dim -> grid_dim3_eq_facts grid_dim
    | None -> symbolic_grid_facts
  in
  {
    shape_global_ints = [];
    shape_facts =
      dim3_eq_facts row.finite_type_block_dim
      @ grid_facts
      @ List.map positive row.finite_type_positive_int_params;
    shape_subgroup_size = None;
  }

let finite_type_domain_to_string domain =
  domain.finite_type_parameter ^ "={"
  ^ String.concat "," domain.finite_type_values
  ^ "}"

let l012_clamp_type_domain =
  {
    finite_type_parameter = "T";
    finite_type_values = [ "half"; "float" ];
    finite_type_source =
      "ggml_cuda_op_clamp dispatches GGML_TYPE_F16 to half and the fallback \
       branch to float";
  }

let l012_clamp_finite_type_row =
  {
    finite_type_row_id = "L012";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/clamp.cu :: op_clamp_kernel";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/clamp.cu";
    finite_type_manifest_kernel = "op_clamp_kernel<T>";
    finite_type_source_kernel_family = "op_clamp_kernel";
    finite_type_parsed_kernel = "op_clamp_kernel_l012_float";
    finite_type_template_arg = "T={half,float}";
    finite_type_domains = [ l012_clamp_type_domain ];
    finite_type_block_dim = [ 256; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = "CUDA_CLAMP_BLOCK_SIZE";
    finite_type_grid_dim_source =
      "(k + CUDA_CLAMP_BLOCK_SIZE - 1) / CUDA_CLAMP_BLOCK_SIZE";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "k" ];
    finite_type_positive_shape_guards = [ "k > 0" ];
    finite_type_memory_effects =
      [ "read x[i] under i < k"; "write dst[i] under i < k" ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S480/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S480/artifacts/L012_op_clamp_kernel_production_backed.cu";
    finite_type_evidence_artifact_key = "s480_l012_production_backed_extraction";
    finite_type_timeout_ms = 10000;
  }

let l067_fill_float_domain =
  {
    finite_type_parameter = "T";
    finite_type_values = [ "float" ];
    finite_type_source =
      "ggml_cuda_op_fill dispatches GGML_TYPE_F32 to fill_kernel with float \
       storage";
  }

let l068_fill_half_domain =
  {
    finite_type_parameter = "T";
    finite_type_values = [ "half" ];
    finite_type_source =
      "ggml_cuda_op_fill dispatches GGML_TYPE_F16 to fill_kernel with half \
       storage; the S482 extraction preserves DRF address ownership while \
       abstracting element bitwidth";
  }

let l067_fill_float_finite_type_row =
  {
    finite_type_row_id = "L067";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/fill.cu :: fill_kernel";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/fill.cu";
    finite_type_manifest_kernel = "fill_kernel<T>";
    finite_type_source_kernel_family = "fill_kernel";
    finite_type_parsed_kernel = "fill_kernel_l067_float";
    finite_type_template_arg = "T=float";
    finite_type_domains = [ l067_fill_float_domain ];
    finite_type_block_dim = [ 256; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = "CUDA_FILL_BLOCK_SIZE";
    finite_type_grid_dim_source =
      "(k + CUDA_FILL_BLOCK_SIZE - 1) / CUDA_FILL_BLOCK_SIZE";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "k" ];
    finite_type_positive_shape_guards = [ "k > 0" ];
    finite_type_memory_effects = [ "write dst[i] under i < k" ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S482/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S482/artifacts/L067_fill_kernel_float_production_backed.cu";
    finite_type_evidence_artifact_key = "s482_l067_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let l068_fill_half_finite_type_row =
  {
    finite_type_row_id = "L068";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/fill.cu :: fill_kernel";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/fill.cu";
    finite_type_manifest_kernel = "fill_kernel<T>";
    finite_type_source_kernel_family = "fill_kernel";
    finite_type_parsed_kernel = "fill_kernel_l068_half_profile";
    finite_type_template_arg = "T=half";
    finite_type_domains = [ l068_fill_half_domain ];
    finite_type_block_dim = [ 256; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = "CUDA_FILL_BLOCK_SIZE";
    finite_type_grid_dim_source =
      "(k + CUDA_FILL_BLOCK_SIZE - 1) / CUDA_FILL_BLOCK_SIZE";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "k" ];
    finite_type_positive_shape_guards = [ "k > 0" ];
    finite_type_memory_effects = [ "write dst[i] under i < k" ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S482/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S482/artifacts/L068_fill_kernel_half_profile_backed.cu";
    finite_type_evidence_artifact_key = "s482_l068_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let l076_divide_by_count_float_domain =
  {
    finite_type_parameter = "T";
    finite_type_values = [ "float" ];
    finite_type_source =
      "ggml_cuda_op_mean launches divide_by_count<float> after the CUB \
       reduction path";
  }

let unary_float_domain source =
  {
    finite_type_parameter = "T";
    finite_type_values = [ "float" ];
    finite_type_source = source;
  }

let unary_half_float_domain source =
  {
    finite_type_parameter = "T";
    finite_type_values = [ "half"; "float" ];
    finite_type_source = source;
  }

let l076_divide_by_count_finite_type_row =
  {
    finite_type_row_id = "L076";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/mean.cu :: divide_by_count";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/mean.cu";
    finite_type_manifest_kernel = "divide_by_count<float>";
    finite_type_source_kernel_family = "divide_by_count";
    finite_type_parsed_kernel = "divide_by_count_l076_float";
    finite_type_template_arg = "T=float";
    finite_type_domains = [ l076_divide_by_count_float_domain ];
    finite_type_block_dim = [ 1; 1; 1 ];
    finite_type_grid_dim = Some [ 1; 1; 1 ];
    finite_type_block_dim_source = "1";
    finite_type_grid_dim_source = "1";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "count" ];
    finite_type_positive_shape_guards = [ "count > 0" ];
    finite_type_memory_effects =
      [ "read result[0]"; "write result[0]"; "single launched thread" ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S483/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S483/artifacts/L076_divide_by_count_float_profile_backed.cu";
    finite_type_evidence_artifact_key = "s483_l076_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let l135_swiglu_oai_finite_type_row =
  {
    finite_type_row_id = "L135";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/unary.cu :: swiglu_oai_kernel";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/unary.cu";
    finite_type_manifest_kernel = "swiglu_oai_kernel<T>";
    finite_type_source_kernel_family = "swiglu_oai_kernel";
    finite_type_parsed_kernel = "swiglu_oai_kernel_l135_profile";
    finite_type_template_arg = "T=float";
    finite_type_domains =
      [
        unary_float_domain
          "ggml_cuda_op_swiglu_oai requires F32 inputs and launches the float \
           swiglu_oai_kernel specialization";
      ];
    finite_type_block_dim = [ 256; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = "CUDA_GLU_BLOCK_SIZE";
    finite_type_grid_dim_source =
      "(k + CUDA_GLU_BLOCK_SIZE - 1) / CUDA_GLU_BLOCK_SIZE";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "k" ];
    finite_type_positive_shape_guards = [ "k > 0" ];
    finite_type_memory_effects =
      [
        "read x[i] under i < k";
        "read g[i] under i < k";
        "write dst[i] under i < k";
      ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S483/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S483/artifacts/L135_swiglu_oai_kernel_profile_backed.cu";
    finite_type_evidence_artifact_key = "s483_l135_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let l136_xielu_finite_type_row =
  {
    finite_type_row_id = "L136";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/unary.cu :: xielu_kernel";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/unary.cu";
    finite_type_manifest_kernel = "xielu_kernel<T>";
    finite_type_source_kernel_family = "xielu_kernel";
    finite_type_parsed_kernel = "xielu_kernel_l136_profile";
    finite_type_template_arg = "T={half,float}";
    finite_type_domains =
      [
        unary_half_float_domain
          "ggml_cuda_op_xielu dispatches F16 and F32 to the same \
           one-dimensional xielu_kernel ownership shape";
      ];
    finite_type_block_dim = [ 256; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = "CUDA_XIELU_BLOCK_SIZE";
    finite_type_grid_dim_source =
      "(k + CUDA_XIELU_BLOCK_SIZE) / CUDA_XIELU_BLOCK_SIZE";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "k" ];
    finite_type_positive_shape_guards = [ "k > 0" ];
    finite_type_memory_effects =
      [ "read src[i] under i < k"; "write dst[i] under i < k" ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S483/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S483/artifacts/L136_xielu_kernel_profile_backed.cu";
    finite_type_evidence_artifact_key = "s483_l136_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let l137_silu_back_finite_type_row =
  {
    finite_type_row_id = "L137";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/unary.cu :: silu_back_kernel";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/unary.cu";
    finite_type_manifest_kernel = "silu_back_kernel<T>";
    finite_type_source_kernel_family = "silu_back_kernel";
    finite_type_parsed_kernel = "silu_back_kernel_l137_profile";
    finite_type_template_arg = "T={half,float}";
    finite_type_domains =
      [
        unary_half_float_domain
          "ggml_cuda_op_silu_back dispatches F16 and F32 to the same \
           one-dimensional silu_back_kernel ownership shape";
      ];
    finite_type_block_dim = [ 256; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = "CUDA_SILU_BACK_BLOCK_SIZE";
    finite_type_grid_dim_source =
      "(k + CUDA_SILU_BACK_BLOCK_SIZE - 1) / CUDA_SILU_BLOCK_SIZE";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "k" ];
    finite_type_positive_shape_guards = [ "k > 0" ];
    finite_type_memory_effects =
      [
        "read x[i] under i < k";
        "read g[i] under i < k";
        "write dst[i] under i < k";
      ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S483/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S483/artifacts/L137_silu_back_kernel_profile_backed.cu";
    finite_type_evidence_artifact_key = "s483_l137_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let l138_leaky_relu_finite_type_row =
  {
    finite_type_row_id = "L138";
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/unary.cu :: leaky_relu_kernel";
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/unary.cu";
    finite_type_manifest_kernel = "leaky_relu_kernel<T>";
    finite_type_source_kernel_family = "leaky_relu_kernel";
    finite_type_parsed_kernel = "leaky_relu_kernel_l138_profile";
    finite_type_template_arg = "T={half,float}";
    finite_type_domains =
      [
        unary_half_float_domain
          "ggml_cuda_op_leaky_relu dispatches F16 and F32 to the same \
           one-dimensional leaky_relu_kernel ownership shape";
      ];
    finite_type_block_dim = [ 256; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = "CUDA_RELU_BLOCK_SIZE";
    finite_type_grid_dim_source =
      "(k + CUDA_RELU_BLOCK_SIZE - 1) / CUDA_RELU_BLOCK_SIZE";
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "k" ];
    finite_type_positive_shape_guards = [ "k > 0" ];
    finite_type_memory_effects =
      [ "read src[i] under i < k"; "write dst[i] under i < k" ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S483/README.md";
    finite_type_extraction_fixture =
      "agent_results/rewrite/component_summaries/S483/artifacts/L138_leaky_relu_kernel_profile_backed.cu";
    finite_type_evidence_artifact_key = "s483_l138_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let dequantize_profile_fixture family_id kernel =
  "agent_results/rewrite/component_summaries/S479/artifacts/" ^ family_id ^ "_"
  ^ kernel ^ "_s479.cu"

let dequantize_need_check_domain value source =
  {
    finite_type_parameter = "need_check";
    finite_type_values = [ value ];
    finite_type_source = source;
  }

let dequantize_finite_type_row ?(domains = []) ?(manifest_kernel = None)
    ?(template_arg = "none") ?(block_dim_source = None)
    ?(grid_dim_source = "nb") ?(evidence_task = "s485") row_id family_id kernel
    block_size =
  {
    finite_type_row_id = row_id;
    finite_type_family_key =
      "llama.cpp/ggml/src/ggml-cuda/convert.cu :: " ^ kernel;
    finite_type_source_file = "llama.cpp/ggml/src/ggml-cuda/convert.cu";
    finite_type_manifest_kernel = Option.value manifest_kernel ~default:kernel;
    finite_type_source_kernel_family = kernel;
    finite_type_parsed_kernel = kernel;
    finite_type_template_arg = template_arg;
    finite_type_domains = domains;
    finite_type_block_dim = [ block_size; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source =
      Option.value block_dim_source ~default:(string_of_int block_size);
    finite_type_grid_dim_source = grid_dim_source;
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ "nblocks" ];
    finite_type_positive_shape_guards = [ "nblocks > 0" ];
    finite_type_memory_effects =
      [
        "read src[block] under block < nblocks";
        "write dst[block * blockDim.x + lane] under block < nblocks and lane < "
        ^ string_of_int block_size;
      ];
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S481/README.md";
    finite_type_extraction_fixture = dequantize_profile_fixture family_id kernel;
    finite_type_evidence_artifact_key =
      evidence_task ^ "_"
      ^ String.lowercase_ascii row_id
      ^ "_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let profile_fixture family_id kernel =
  "agent_results/rewrite/component_summaries/S479/artifacts/" ^ family_id ^ "_"
  ^ kernel ^ "_s479.cu"

let finite_domain parameter value source =
  {
    finite_type_parameter = parameter;
    finite_type_values = [ value ];
    finite_type_source = source;
  }

let profile_finite_type_row ~row_id ~family_id ~family_key ~source_file
    ~manifest_kernel ~source_kernel_family ~parsed_kernel ~template_arg ~domains
    ~block_dim ~block_dim_source ~grid_dim_source ~positive_param
    ~memory_effects ~evidence_task =
  {
    finite_type_row_id = row_id;
    finite_type_family_key = family_key;
    finite_type_source_file = source_file;
    finite_type_manifest_kernel = manifest_kernel;
    finite_type_source_kernel_family = source_kernel_family;
    finite_type_parsed_kernel = parsed_kernel;
    finite_type_template_arg = template_arg;
    finite_type_domains = domains;
    finite_type_block_dim = [ block_dim; 1; 1 ];
    finite_type_grid_dim = None;
    finite_type_block_dim_source = block_dim_source;
    finite_type_grid_dim_source = grid_dim_source;
    finite_type_dynamic_shared_memory = "0";
    finite_type_positive_int_params = [ positive_param ];
    finite_type_positive_shape_guards = [ positive_param ^ " > 0" ];
    finite_type_memory_effects = memory_effects;
    finite_type_preprocessing_profile =
      "agent_results/rewrite/component_summaries/S481/README.md";
    finite_type_extraction_fixture = profile_fixture family_id parsed_kernel;
    finite_type_evidence_artifact_key =
      evidence_task ^ "_"
      ^ String.lowercase_ascii row_id
      ^ "_exact_launch_contract";
    finite_type_timeout_ms = 10000;
  }

let conv2d_dw_profile_row row_id layout =
  profile_finite_type_row ~row_id ~family_id:"F016"
    ~family_key:"llama.cpp/ggml/src/ggml-cuda/conv2d-dw.cu :: conv2d_dw_kernel"
    ~source_file:"llama.cpp/ggml/src/ggml-cuda/conv2d-dw.cu"
    ~manifest_kernel:("conv2d_dw_kernel<float, " ^ layout ^ ">")
    ~source_kernel_family:"conv2d_dw_kernel" ~parsed_kernel:"conv2d_dw_kernel"
    ~template_arg:("T=float, layout=" ^ layout)
    ~domains:
      [
        finite_domain "T" "float"
          ("conv2d_dw " ^ layout ^ " host dispatch fixes T=float");
        finite_domain "layout" layout
          ("conv2d_dw launch branch fixes layout=" ^ layout);
      ]
    ~block_dim:256 ~block_dim_source:"CUDA_CONV2D_DW_BLOCK_SIZE"
    ~grid_dim_source:"blocks" ~positive_param:"total"
    ~memory_effects:
      [
        "read input[global_idx] under global_idx < total";
        "read weight[global_idx] under global_idx < total";
        "write output[global_idx] under global_idx < total";
      ]
    ~evidence_task:"s486b"

let conv2d_transpose_profile_row row_id element_type =
  profile_finite_type_row ~row_id ~family_id:"F017"
    ~family_key:
      "llama.cpp/ggml/src/ggml-cuda/conv2d-transpose.cu :: \
       conv2d_transpose_kernel"
    ~source_file:"llama.cpp/ggml/src/ggml-cuda/conv2d-transpose.cu"
    ~manifest_kernel:("conv2d_transpose_kernel<" ^ element_type ^ ">")
    ~source_kernel_family:"conv2d_transpose_kernel"
    ~parsed_kernel:"conv2d_transpose_kernel" ~template_arg:("T=" ^ element_type)
    ~domains:
      [
        finite_domain "T" element_type
          ("conv2d_transpose host dispatch fixes T=" ^ element_type);
      ]
    ~block_dim:256 ~block_dim_source:"CUDA_CONV2D_TRANSPOSE_BLOCK_SIZE"
    ~grid_dim_source:"blocks" ~positive_param:"total"
    ~memory_effects:
      [
        "read input[global_idx] under global_idx < total";
        "read weight[global_idx] under global_idx < total";
        "write output[global_idx] under global_idx < total";
      ]
    ~evidence_task:"s486b"

let cpy_profile_row ~row_id ~family_id ~kernel ~helper ~qk =
  let family_key = "llama.cpp/ggml/src/ggml-cuda/cpy.cu :: " ^ kernel in
  profile_finite_type_row ~row_id ~family_id ~family_key
    ~source_file:"llama.cpp/ggml/src/ggml-cuda/cpy.cu"
    ~manifest_kernel:(kernel ^ "<" ^ helper ^ ", " ^ qk ^ ">")
    ~source_kernel_family:kernel ~parsed_kernel:kernel
    ~template_arg:(helper ^ ", " ^ qk)
    ~domains:
      [
        finite_domain "copy_helper" helper
          ("cpy.cu launch fixes copy helper " ^ helper);
        finite_domain "QK" qk ("cpy.cu launch fixes quantization block " ^ qk);
      ]
    ~block_dim:1 ~block_dim_source:"1" ~grid_dim_source:"num_blocks"
    ~positive_param:"num_blocks"
    ~memory_effects:
      [
        "read source block[block] under block < num_blocks";
        "write destination block[block] under block < num_blocks";
      ]
    ~evidence_task:"s486b"

let conv_profile_rows =
  [
    conv2d_dw_profile_row "L018" "whcn_layout";
    conv2d_dw_profile_row "L019" "cwhn_layout";
    conv2d_transpose_profile_row "L020" "half";
    conv2d_transpose_profile_row "L021" "float";
  ]

let cpy_profile_rows =
  [
    cpy_profile_row ~row_id:"L046" ~family_id:"F041" ~kernel:"cpy_f32_q"
      ~helper:"cpy_blck_f32_q8_0" ~qk:"QK8_0";
    cpy_profile_row ~row_id:"L048" ~family_id:"F041" ~kernel:"cpy_f32_q"
      ~helper:"cpy_blck_f32_q4_0" ~qk:"QK4_0";
    cpy_profile_row ~row_id:"L050" ~family_id:"F041" ~kernel:"cpy_f32_q"
      ~helper:"cpy_blck_f32_q4_1" ~qk:"QK4_1";
    cpy_profile_row ~row_id:"L052" ~family_id:"F041" ~kernel:"cpy_f32_q"
      ~helper:"cpy_blck_f32_q5_0" ~qk:"QK5_0";
    cpy_profile_row ~row_id:"L054" ~family_id:"F041" ~kernel:"cpy_f32_q"
      ~helper:"cpy_blck_f32_q5_1" ~qk:"QK5_1";
    cpy_profile_row ~row_id:"L056" ~family_id:"F041" ~kernel:"cpy_f32_q"
      ~helper:"cpy_blck_f32_iq4_nl" ~qk:"QK4_NL";
    cpy_profile_row ~row_id:"L047" ~family_id:"F042" ~kernel:"cpy_q_f32"
      ~helper:"cpy_blck_q8_0_f32" ~qk:"QK8_0";
    cpy_profile_row ~row_id:"L049" ~family_id:"F042" ~kernel:"cpy_q_f32"
      ~helper:"cpy_blck_q_f32<dequantize_q4_0, QK4_0>" ~qk:"QK4_0";
    cpy_profile_row ~row_id:"L051" ~family_id:"F042" ~kernel:"cpy_q_f32"
      ~helper:"cpy_blck_q_f32<dequantize_q4_1, QK4_1>" ~qk:"QK4_1";
    cpy_profile_row ~row_id:"L053" ~family_id:"F042" ~kernel:"cpy_q_f32"
      ~helper:"cpy_blck_q_f32<dequantize_q5_0, QK5_0>" ~qk:"QK5_0";
    cpy_profile_row ~row_id:"L055" ~family_id:"F042" ~kernel:"cpy_q_f32"
      ~helper:"cpy_blck_q_f32<dequantize_q5_1, QK5_1>" ~qk:"QK5_1";
  ]

let im2col_type_domain =
  Finite_type_domain { parameter = "T"; values = [ "half"; "float" ] }

let im2col_bounded_block_shape_contract =
  {
    shape_global_ints = [ "local_extent"; "CUDA_IM2COL_BLOCK_SIZE" ];
    shape_facts =
      [
        eq (named "CUDA_IM2COL_BLOCK_SIZE") (num 256);
        positive "local_extent";
        gt (dim Block_dim_x) (num 0);
        le (dim Block_dim_x) (named "local_extent");
        le (dim Block_dim_x) (named "CUDA_IM2COL_BLOCK_SIZE");
        eq (dim Block_dim_y) (num 1);
        eq (dim Block_dim_z) (num 1);
        gt (dim Grid_dim_x) (num 0);
        gt (dim Grid_dim_y) (num 0);
        gt (dim Grid_dim_z) (num 0);
      ];
    shape_subgroup_size = None;
  }

let im2col_symbolic_block_launch_contract_row ~row_id ~manifest_kernel
    ~parsed_kernel ~block_dim_source ~grid_dim_source =
  {
    launch_row_id = row_id;
    launch_family = Finite_type_template;
    launch_manifest_kernel = manifest_kernel;
    launch_parsed_kernel = parsed_kernel;
    launch_template_arg =
      "T={half,float}; blockDim.x=" ^ block_dim_source ^ "; gridDim="
      ^ grid_dim_source;
    launch_template_param = None;
    launch_template_value = None;
    launch_template_bindings = [];
    launch_template_domains = [ im2col_type_domain ];
    launch_shape_contract = im2col_bounded_block_shape_contract;
    launch_block_dim = None;
    launch_symbolic_dimension_carrier = None;
  }

let im2col_symbolic_block_launch_contract_rows =
  [
    im2col_symbolic_block_launch_contract_row ~row_id:"L074"
      ~manifest_kernel:"im2col_kernel<T>" ~parsed_kernel:"im2col_kernel"
      ~block_dim_source:"MIN(IC_KH_KW, CUDA_IM2COL_BLOCK_SIZE)"
      ~grid_dim_source:"block_nums";
    im2col_symbolic_block_launch_contract_row ~row_id:"L075"
      ~manifest_kernel:"im2col_3d_kernel<T>" ~parsed_kernel:"im2col_3d_kernel"
      ~block_dim_source:"MIN(IC_KD_KH_KW, CUDA_IM2COL_BLOCK_SIZE)"
      ~grid_dim_source:"block_nums";
  ]

let dequantize_need_check_finite_type_rows =
  [
    dequantize_finite_type_row "L024" "F020" "dequantize_block_q8_0_f16" 32
      ~manifest_kernel:(Some "dequantize_block_q8_0_f16<need_check>")
      ~template_arg:"need_check=false" ~block_dim_source:(Some "WARP_SIZE")
      ~grid_dim_source:"num_blocks" ~evidence_task:"s486a"
      ~domains:
        [
          dequantize_need_check_domain "false"
            "convert.cu line 508 branch-local const bool need_check = false";
        ];
    dequantize_finite_type_row "L025" "F020" "dequantize_block_q8_0_f16" 32
      ~manifest_kernel:(Some "dequantize_block_q8_0_f16<need_check>")
      ~template_arg:"need_check=true" ~block_dim_source:(Some "WARP_SIZE")
      ~grid_dim_source:"num_blocks" ~evidence_task:"s486a"
      ~domains:
        [
          dequantize_need_check_domain "true"
            "convert.cu line 511 branch-local const bool need_check = true";
        ];
  ]

let dequantize_finite_type_rows =
  [
    dequantize_finite_type_row "L026" "F021" "dequantize_block_q2_K" 64;
    dequantize_finite_type_row "L027" "F022" "dequantize_block_q3_K" 64;
    dequantize_finite_type_row "L028" "F023" "dequantize_block_q4_0" 32;
    dequantize_finite_type_row "L029" "F024" "dequantize_block_q4_1" 32;
    dequantize_finite_type_row "L030" "F025" "dequantize_block_q4_K" 32;
    dequantize_finite_type_row "L031" "F026" "dequantize_block_q5_K" 64;
    dequantize_finite_type_row "L032" "F027" "dequantize_block_q6_K" 64;
    dequantize_finite_type_row "L033" "F028" "dequantize_block_iq2_xxs" 32;
    dequantize_finite_type_row "L034" "F029" "dequantize_block_iq2_xs" 32;
    dequantize_finite_type_row "L035" "F030" "dequantize_block_iq2_s" 32;
    dequantize_finite_type_row "L036" "F031" "dequantize_block_iq3_xxs" 32;
    dequantize_finite_type_row "L037" "F032" "dequantize_block_iq3_s" 32;
    dequantize_finite_type_row "L038" "F033" "dequantize_block_iq1_s" 32;
    dequantize_finite_type_row "L039" "F034" "dequantize_block_iq4_nl" 32;
    dequantize_finite_type_row "L040" "F035" "dequantize_block_iq1_m" 32;
    dequantize_finite_type_row "L041" "F036" "dequantize_block_iq4_xs" 32;
    dequantize_finite_type_row "L042" "F037" "dequantize_block_mxfp4" 32;
    dequantize_finite_type_row "L043" "F038" "dequantize_block_nvfp4" 32;
  ]

let fixed_by_launch value evidence = Fixed_by_launch { value; evidence }

let fixed_by_model_or_template value evidence =
  Fixed_by_model_or_template { value; evidence }

let user_symbolic domain evidence = User_symbolic { domain; evidence }

let derived expression dependencies evidence =
  Derived { expression; dependencies; evidence }

let finite_type_context context_id row ~template_role ~block_dim_role
    ~grid_dim_role ~positive_param_roles ~memory_effect_roles
    ~soundness_boundary =
  let positive_param_role name =
    match List.assoc_opt name positive_param_roles with
    | Some role -> role
    | None ->
        Unknown_blocker
          { reason = "missing role for positive parameter " ^ name }
  in
  {
    context_id;
    context_row = row;
    context_template_role = template_role;
    context_block_dim =
      {
        context_dim3_value = Some row.finite_type_block_dim;
        context_dim3_source = row.finite_type_block_dim_source;
        context_dim3_role = block_dim_role;
      };
    context_grid_dim =
      {
        context_dim3_value = row.finite_type_grid_dim;
        context_dim3_source = row.finite_type_grid_dim_source;
        context_dim3_role = grid_dim_role;
      };
    context_dynamic_shared_memory_role =
      fixed_by_launch row.finite_type_dynamic_shared_memory
        ("dynamic shared-memory source for " ^ row.finite_type_row_id);
    context_positive_params =
      List.map
        (fun name ->
          {
            context_positive_name = name;
            context_positive_guard = name ^ " > 0";
            context_positive_role = positive_param_role name;
          })
        row.finite_type_positive_int_params;
    context_memory_effect_roles = memory_effect_roles;
    context_soundness_boundary = soundness_boundary;
  }

let one_dim_grid_context row =
  derived row.finite_type_grid_dim_source
    [ "k"; row.finite_type_block_dim_source ]
    ("production launch expression for " ^ row.finite_type_row_id)

let one_dim_memory_context row =
  let dependencies = [ "i"; "blockIdx.x"; "blockDim.x"; "threadIdx.x" ] in
  let evidence = "S481/S48x ownership profile for " ^ row.finite_type_row_id in
  let role memory_effect = derived memory_effect dependencies evidence in
  List.map role row.finite_type_memory_effects

let dequantize_memory_context row =
  let dependencies =
    [ "block"; "lane"; "blockIdx.x"; "threadIdx.x"; "blockDim.x" ]
  in
  let evidence =
    "dequantize production profile for " ^ row.finite_type_row_id
  in
  let role memory_effect = derived memory_effect dependencies evidence in
  List.map role row.finite_type_memory_effects

let dequantize_need_check_profile_context row =
  finite_type_context
    ("S486A-" ^ row.finite_type_row_id)
    row
    ~template_role:
      (fixed_by_model_or_template row.finite_type_template_arg
         "convert.cu branch-local need_check const bool")
    ~block_dim_role:
      (fixed_by_launch "WARP_SIZE = 32" "convert.cu q8_0_f16 launch block size")
    ~grid_dim_role:
      (derived row.finite_type_grid_dim_source [ "nblocks" ]
         ("convert.cu q8_0_f16 launch grid for " ^ row.finite_type_row_id))
    ~positive_param_roles:
      [
        ( "nblocks",
          user_symbolic "positive dequantize block count"
            "S481 dequantize q8_0_f16 production profile" );
      ]
    ~memory_effect_roles:(dequantize_memory_context row)
    ~soundness_boundary:
      "S486-A exact q8_0_f16 need_check launch-contract evidence for DRF \
       block/lane ownership; not numeric dequantization equivalence or full \
       host translation-unit parsing"

let dequantize_profile_context row =
  finite_type_context
    ("S485-" ^ row.finite_type_row_id)
    row
    ~template_role:
      (fixed_by_model_or_template "no runtime template parameter"
         "convert.cu dequantize kernel launch")
    ~block_dim_role:
      (fixed_by_launch row.finite_type_block_dim_source
         "convert.cu dequantize launch block size")
    ~grid_dim_role:
      (derived row.finite_type_grid_dim_source [ "nblocks" ]
         ("convert.cu dequantize launch grid for " ^ row.finite_type_row_id))
    ~positive_param_roles:
      [
        ( "nblocks",
          user_symbolic "positive dequantize block count"
            "S481 dequantize production profile" );
      ]
    ~memory_effect_roles:(dequantize_memory_context row)
    ~soundness_boundary:
      "S485 exact dequantize launch-contract evidence for DRF block/lane \
       ownership; not numeric dequantization equivalence or full host \
       translation-unit parsing"

let conv_memory_context row =
  let dependencies =
    [ "global_idx"; "blockIdx.x"; "blockDim.x"; "threadIdx.x" ]
  in
  let evidence =
    "S486-B conv production profile for " ^ row.finite_type_row_id
  in
  let role memory_effect = derived memory_effect dependencies evidence in
  List.map role row.finite_type_memory_effects

let conv_profile_context row =
  finite_type_context
    ("S486B-" ^ row.finite_type_row_id)
    row
    ~template_role:
      (fixed_by_model_or_template row.finite_type_template_arg
         "conv2d host dispatch finite template/layout branch")
    ~block_dim_role:
      (fixed_by_launch row.finite_type_block_dim_source
         "conv2d production launch block size")
    ~grid_dim_role:
      (derived row.finite_type_grid_dim_source [ "total" ]
         ("conv2d production launch grid for " ^ row.finite_type_row_id))
    ~positive_param_roles:
      [
        ( "total",
          user_symbolic "positive linearized output extent"
            "S481 conv production profile" );
      ]
    ~memory_effect_roles:(conv_memory_context row)
    ~soundness_boundary:
      "S486-B exact conv launch-contract evidence for DRF linearized output \
       ownership; not numeric convolution equivalence or full host \
       translation-unit parsing"

let cpy_memory_context row =
  let dependencies = [ "block"; "blockIdx.x"; "blockDim.x"; "threadIdx.x" ] in
  let evidence =
    "S486-B cpy production profile for " ^ row.finite_type_row_id
  in
  let role memory_effect = derived memory_effect dependencies evidence in
  List.map role row.finite_type_memory_effects

let cpy_profile_context row =
  finite_type_context
    ("S486B-" ^ row.finite_type_row_id)
    row
    ~template_role:
      (fixed_by_model_or_template row.finite_type_template_arg
         "cpy.cu finite helper/QK launch branch")
    ~block_dim_role:
      (fixed_by_launch "1" "cpy.cu single-thread-per-block launch")
    ~grid_dim_role:
      (derived row.finite_type_grid_dim_source [ "num_blocks" ]
         ("cpy.cu production launch grid for " ^ row.finite_type_row_id))
    ~positive_param_roles:
      [
        ( "num_blocks",
          user_symbolic "positive copy block count"
            "S481 cpy production profile" );
      ]
    ~memory_effect_roles:(cpy_memory_context row)
    ~soundness_boundary:
      "S486-B exact cpy launch-contract evidence for DRF block ownership; not \
       numeric quantization/dequantization equivalence or full host \
       translation-unit parsing"

let profile_launch_contexts =
  [
    finite_type_context "S480-L012" l012_clamp_finite_type_row
      ~template_role:
        (fixed_by_model_or_template "T in {half,float}"
           "ggml_cuda_op_clamp host dispatch")
      ~block_dim_role:
        (fixed_by_launch "CUDA_CLAMP_BLOCK_SIZE = 256" "clamp.cu launch profile")
      ~grid_dim_role:(one_dim_grid_context l012_clamp_finite_type_row)
      ~positive_param_roles:
        [
          ( "k",
            user_symbolic "positive element count"
              "ggml_cuda_op_clamp launch shape" );
        ]
      ~memory_effect_roles:(one_dim_memory_context l012_clamp_finite_type_row)
      ~soundness_boundary:
        "production-backed extraction evidence for DRF ownership, not full \
         host translation-unit parsing or numeric equivalence";
    finite_type_context "S482-L067" l067_fill_float_finite_type_row
      ~template_role:(fixed_by_launch "T=float" "GGML_TYPE_F32 fill dispatch")
      ~block_dim_role:
        (fixed_by_launch "CUDA_FILL_BLOCK_SIZE = 256" "fill.cu launch profile")
      ~grid_dim_role:(one_dim_grid_context l067_fill_float_finite_type_row)
      ~positive_param_roles:
        [
          ( "k",
            user_symbolic "positive element count"
              "ggml_cuda_op_fill launch shape" );
        ]
      ~memory_effect_roles:
        (one_dim_memory_context l067_fill_float_finite_type_row)
      ~soundness_boundary:
        "S482 profile-backed exact launch-contract evidence for DRF ownership";
    finite_type_context "S482-L068" l068_fill_half_finite_type_row
      ~template_role:(fixed_by_launch "T=half" "GGML_TYPE_F16 fill dispatch")
      ~block_dim_role:
        (fixed_by_launch "CUDA_FILL_BLOCK_SIZE = 256" "fill.cu launch profile")
      ~grid_dim_role:(one_dim_grid_context l068_fill_half_finite_type_row)
      ~positive_param_roles:
        [
          ( "k",
            user_symbolic "positive element count"
              "ggml_cuda_op_fill launch shape" );
        ]
      ~memory_effect_roles:
        (one_dim_memory_context l068_fill_half_finite_type_row)
      ~soundness_boundary:
        "S482 profile-backed exact launch-contract evidence for DRF ownership";
    finite_type_context "S483-L076" l076_divide_by_count_finite_type_row
      ~template_role:(fixed_by_launch "T=float" "divide_by_count<float> launch")
      ~block_dim_role:(fixed_by_launch "[1,1,1]" "mean.cu launch profile")
      ~grid_dim_role:(fixed_by_launch "[1,1,1]" "mean.cu launch profile")
      ~positive_param_roles:
        [
          ( "count",
            user_symbolic "positive reduction divisor" "divide_by_count profile"
          );
        ]
      ~memory_effect_roles:
        (List.map
           (fun memory_effect ->
             derived memory_effect [ "gridDim"; "blockDim" ]
               "single-thread scalar ownership profile")
           l076_divide_by_count_finite_type_row.finite_type_memory_effects)
      ~soundness_boundary:
        "S483 exact single-thread scalar DRF profile; exact grid/block facts \
         are required";
    finite_type_context "S483-L135" l135_swiglu_oai_finite_type_row
      ~template_role:
        (fixed_by_model_or_template "T=float"
           "ggml_cuda_op_swiglu_oai F32-only host assertions")
      ~block_dim_role:
        (fixed_by_launch "CUDA_GLU_BLOCK_SIZE = 256" "unary.cu launch profile")
      ~grid_dim_role:(one_dim_grid_context l135_swiglu_oai_finite_type_row)
      ~positive_param_roles:
        [
          ( "k",
            user_symbolic "positive element count" "swiglu_oai launch profile"
          );
        ]
      ~memory_effect_roles:
        (one_dim_memory_context l135_swiglu_oai_finite_type_row)
      ~soundness_boundary:
        "S483 profile-backed exact launch-contract evidence for DRF ownership";
    finite_type_context "S483-L136" l136_xielu_finite_type_row
      ~template_role:
        (fixed_by_model_or_template "T in {half,float}"
           "xielu host dispatch finite element types")
      ~block_dim_role:
        (fixed_by_launch "CUDA_XIELU_BLOCK_SIZE = 256" "unary.cu launch profile")
      ~grid_dim_role:(one_dim_grid_context l136_xielu_finite_type_row)
      ~positive_param_roles:
        [ ("k", user_symbolic "positive element count" "xielu launch profile") ]
      ~memory_effect_roles:(one_dim_memory_context l136_xielu_finite_type_row)
      ~soundness_boundary:
        "S483 profile-backed exact launch-contract evidence for DRF ownership";
    finite_type_context "S483-L137" l137_silu_back_finite_type_row
      ~template_role:
        (fixed_by_model_or_template "T in {half,float}"
           "silu_back host dispatch finite element types")
      ~block_dim_role:
        (fixed_by_launch "CUDA_SILU_BACK_BLOCK_SIZE = 256"
           "unary.cu launch profile")
      ~grid_dim_role:(one_dim_grid_context l137_silu_back_finite_type_row)
      ~positive_param_roles:
        [
          ( "k",
            user_symbolic "positive element count" "silu_back launch profile" );
        ]
      ~memory_effect_roles:
        (one_dim_memory_context l137_silu_back_finite_type_row)
      ~soundness_boundary:
        "S483 profile-backed exact launch-contract evidence for DRF ownership";
    finite_type_context "S483-L138" l138_leaky_relu_finite_type_row
      ~template_role:
        (fixed_by_model_or_template "T in {half,float}"
           "leaky_relu host dispatch finite element types")
      ~block_dim_role:
        (fixed_by_launch "CUDA_RELU_BLOCK_SIZE = 256" "unary.cu launch profile")
      ~grid_dim_role:(one_dim_grid_context l138_leaky_relu_finite_type_row)
      ~positive_param_roles:
        [
          ( "k",
            user_symbolic "positive element count" "leaky_relu launch profile"
          );
        ]
      ~memory_effect_roles:
        (one_dim_memory_context l138_leaky_relu_finite_type_row)
      ~soundness_boundary:
        "S483 profile-backed exact launch-contract evidence for DRF ownership";
  ]
  @ List.map dequantize_need_check_profile_context
      dequantize_need_check_finite_type_rows
  @ List.map conv_profile_context conv_profile_rows
  @ List.map cpy_profile_context cpy_profile_rows
  @ List.map dequantize_profile_context dequantize_finite_type_rows

let finite_type_rows =
  List.map finite_type_row_of_profile_launch_context profile_launch_contexts

let profile_launch_context_of_row_id row_id =
  match
    List.filter
      (fun context ->
        String.equal context.context_row.finite_type_row_id row_id)
      profile_launch_contexts
  with
  | [ context ] -> Ok context
  | [] -> Error (Unknown_generated_row row_id)
  | _ -> Error (Duplicate_generated_row row_id)

let solve_tri_fast_symbolic_k_rows =
  [
    {
      solve_tri_symbolic_row_id = "L117";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 32>";
      solve_tri_symbolic_k_template = 32;
      solve_tri_symbolic_branch_case = "32";
      solve_tri_symbolic_row_kind = Symbolic_lookup_anchor;
    };
    {
      solve_tri_symbolic_row_id = "L118";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 16>";
      solve_tri_symbolic_k_template = 16;
      solve_tri_symbolic_branch_case = "16";
      solve_tri_symbolic_row_kind = Symbolic_lookup_anchor;
    };
    {
      solve_tri_symbolic_row_id = "L119";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 14>";
      solve_tri_symbolic_k_template = 14;
      solve_tri_symbolic_branch_case = "14";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
    {
      solve_tri_symbolic_row_id = "L120";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 12>";
      solve_tri_symbolic_k_template = 12;
      solve_tri_symbolic_branch_case = "12";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
    {
      solve_tri_symbolic_row_id = "L121";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 10>";
      solve_tri_symbolic_k_template = 10;
      solve_tri_symbolic_branch_case = "10";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
    {
      solve_tri_symbolic_row_id = "L122";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 8>";
      solve_tri_symbolic_k_template = 8;
      solve_tri_symbolic_branch_case = "8";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
    {
      solve_tri_symbolic_row_id = "L123";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 6>";
      solve_tri_symbolic_k_template = 6;
      solve_tri_symbolic_branch_case = "6";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
    {
      solve_tri_symbolic_row_id = "L124";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 4>";
      solve_tri_symbolic_k_template = 4;
      solve_tri_symbolic_branch_case = "4";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
    {
      solve_tri_symbolic_row_id = "L125";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 2>";
      solve_tri_symbolic_k_template = 2;
      solve_tri_symbolic_branch_case = "2";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
    {
      solve_tri_symbolic_row_id = "L126";
      solve_tri_symbolic_manifest_kernel = "solve_tri_f32_fast<64, 1>";
      solve_tri_symbolic_k_template = 1;
      solve_tri_symbolic_branch_case = "1";
      solve_tri_symbolic_row_kind = Symbolic_unpromoted_candidate;
    };
  ]

let solve_tri_fast_family =
  {
    solve_tri_family = Solve_tri_fast;
    solve_tri_source_file = "llama.cpp/ggml/src/ggml-cuda/solve_tri.cu";
    solve_tri_source_kernel_family = "solve_tri_f32_fast";
    solve_tri_n_template = 64;
    solve_tri_block_dim_x = 32;
    solve_tri_block_dim_source = "threads";
    solve_tri_grid_dim_source = "grid";
    solve_tri_dynamic_shared_memory = "0";
    solve_tri_feature_class = "shared_memory_syncthreads";
    solve_tri_required_semantics = solve_tri_fast_required_semantics;
    solve_tri_subgroup_helper = "warp_reduce_sum";
    solve_tri_subgroup_size = 32;
    solve_tri_lookup_rows =
      [
        {
          solve_tri_row_id = "L117";
          solve_tri_manifest_kernel = "solve_tri_f32_fast<64, 32>";
          solve_tri_parsed_kernel = "solve_tri_f32_fast_l117";
          solve_tri_k_template = 32;
          solve_tri_branch_case = "32";
          solve_tri_preprocessing_profile =
            "agent_results/rewrite/component_summaries/H516/preprocessing_profile.md";
          solve_tri_extraction_fixture =
            "agent_results/rewrite/component_summaries/H516/artifacts/L117_solve_tri_f32_fast_l117_source_slice.cu";
          solve_tri_evidence_artifact_key = "h516_source_intake";
          solve_tri_timeout_ms = 1000;
        };
        {
          solve_tri_row_id = "L118";
          solve_tri_manifest_kernel = "solve_tri_f32_fast<64, 16>";
          solve_tri_parsed_kernel = "solve_tri_f32_fast_l118";
          solve_tri_k_template = 16;
          solve_tri_branch_case = "16";
          solve_tri_preprocessing_profile =
            "agent_results/rewrite/component_summaries/S408/preprocessing_profile.md";
          solve_tri_extraction_fixture =
            "agent_results/rewrite/component_summaries/S408/artifacts/L118_solve_tri_f32_fast_l118_source_slice.cu";
          solve_tri_evidence_artifact_key = "s408_source_intake";
          solve_tri_timeout_ms = 10000;
        };
      ];
    solve_tri_symbolic_k_rows = solve_tri_fast_symbolic_k_rows;
    solve_tri_unpromoted_row_ids =
      [ "L119"; "L120"; "L121"; "L122"; "L123"; "L124"; "L125"; "L126" ];
    solve_tri_excluded_row_ids = [ "L116"; "L127"; "L128" ];
  }

let solve_tri_fast_selected_row
    (family_contract : solve_tri_fast_family_contract)
    (row_seed : solve_tri_fast_row_seed) =
  let n_template = family_contract.solve_tri_n_template in
  let k_template = row_seed.solve_tri_k_template in
  {
    selected_row_id = row_seed.solve_tri_row_id;
    selected_family = family_contract.solve_tri_family;
    selected_source_file = family_contract.solve_tri_source_file;
    selected_manifest_kernel = row_seed.solve_tri_manifest_kernel;
    selected_source_kernel_family =
      family_contract.solve_tri_source_kernel_family;
    selected_parsed_kernel = row_seed.solve_tri_parsed_kernel;
    selected_template_arg =
      string_of_int n_template ^ ", " ^ string_of_int k_template;
    selected_template_bindings =
      [ ("n_template", n_template); ("k_template", k_template) ];
    selected_source_branch_conditions =
      [
        "n == " ^ string_of_int n_template;
        "case " ^ row_seed.solve_tri_branch_case;
      ];
    selected_preprocessing_profile = row_seed.solve_tri_preprocessing_profile;
    selected_extraction_fixture = row_seed.solve_tri_extraction_fixture;
    selected_block_dim_source = family_contract.solve_tri_block_dim_source;
    selected_grid_dim_source = family_contract.solve_tri_grid_dim_source;
    selected_concrete_block_dim =
      [ family_contract.solve_tri_block_dim_x; k_template; 1 ];
    selected_dynamic_shared_memory =
      family_contract.solve_tri_dynamic_shared_memory;
    selected_feature_class = family_contract.solve_tri_feature_class;
    selected_required_semantics = family_contract.solve_tri_required_semantics;
    selected_subgroup_helper = family_contract.solve_tri_subgroup_helper;
    selected_subgroup_size = family_contract.solve_tri_subgroup_size;
    selected_evidence_artifact_key = row_seed.solve_tri_evidence_artifact_key;
    selected_timeout_ms = row_seed.solve_tri_timeout_ms;
  }

let selected_rows =
  List.map
    (solve_tri_fast_selected_row solve_tri_fast_family)
    solve_tri_fast_family.solve_tri_lookup_rows

let solve_tri_symbolic_row_spec row =
  row.solve_tri_symbolic_row_id ^ ":K="
  ^ string_of_int row.solve_tri_symbolic_k_template
  ^ ":"
  ^ solve_tri_symbolic_row_kind_to_string row.solve_tri_symbolic_row_kind

let symbolic_dimension_to_string = function
  | Concrete_dimension { concrete_dimension_value; _ } ->
      string_of_int concrete_dimension_value
  | Symbolic_dimension { symbolic_dimension_parameter; _ } ->
      symbolic_dimension_parameter

let symbolic_dimension_candidate_values = function
  | Concrete_dimension { concrete_dimension_value; _ } ->
      [ concrete_dimension_value ]
  | Symbolic_dimension { symbolic_dimension_candidate_values; _ } ->
      symbolic_dimension_candidate_values

let symbolic_dim3_to_string dim =
  "["
  ^ String.concat ", "
      [
        symbolic_dimension_to_string dim.symbolic_dim_x;
        symbolic_dimension_to_string dim.symbolic_dim_y;
        symbolic_dimension_to_string dim.symbolic_dim_z;
      ]
  ^ "]"

let symbolic_template_dimension_to_string (name, dimension) =
  name ^ "=" ^ symbolic_dimension_to_string dimension

let solve_tri_symbolic_k_guard =
  {
    symbolic_guard_family = solve_tri_fast_family.solve_tri_family;
    symbolic_guard_source_file = solve_tri_fast_family.solve_tri_source_file;
    symbolic_guard_source_kernel_family =
      solve_tri_fast_family.solve_tri_source_kernel_family;
    symbolic_guard_n_template = solve_tri_fast_family.solve_tri_n_template;
    symbolic_guard_k_parameter = "K";
    symbolic_guard_k_source =
      "manifest/source launch cases for solve_tri_f32_fast<64,K>";
    symbolic_guard_block_dim_relation = "blockDim = [32, K, 1]";
    symbolic_guard_dynamic_shared_memory =
      solve_tri_fast_family.solve_tri_dynamic_shared_memory;
    symbolic_guard_subgroup_helper =
      solve_tri_fast_family.solve_tri_subgroup_helper;
    symbolic_guard_subgroup_size = solve_tri_fast_family.solve_tri_subgroup_size;
    symbolic_guard_route_owner = "Memory_event.Subgroup_obligation";
    symbolic_guard_candidate_rows =
      solve_tri_fast_family.solve_tri_symbolic_k_rows;
    symbolic_guard_lookup_anchor_row_ids =
      List.map
        (fun row -> row.solve_tri_row_id)
        solve_tri_fast_family.solve_tri_lookup_rows;
    symbolic_guard_unpromoted_row_ids =
      solve_tri_fast_family.solve_tri_unpromoted_row_ids;
    symbolic_guard_excluded_row_ids =
      solve_tri_fast_family.solve_tri_excluded_row_ids;
  }

let solve_tri_symbolic_k_candidate_values =
  List.map
    (fun row -> row.solve_tri_symbolic_k_template)
    solve_tri_fast_family.solve_tri_symbolic_k_rows

let solve_tri_symbolic_dimension_carrier =
  let k_dimension =
    Symbolic_dimension
      {
        symbolic_dimension_parameter =
          solve_tri_symbolic_k_guard.symbolic_guard_k_parameter;
        symbolic_dimension_source =
          solve_tri_symbolic_k_guard.symbolic_guard_k_source;
        symbolic_dimension_candidate_values =
          solve_tri_symbolic_k_candidate_values;
        symbolic_dimension_positive_guard = "K > 0";
      }
  in
  {
    carrier_family = solve_tri_fast_family.solve_tri_family;
    carrier_source_file = solve_tri_fast_family.solve_tri_source_file;
    carrier_source_kernel_family =
      solve_tri_fast_family.solve_tri_source_kernel_family;
    carrier_source_width_variable = "k";
    carrier_source_width_relation = "k == K";
    carrier_template_dimensions =
      [
        ( "n_template",
          Concrete_dimension
            {
              concrete_dimension_value =
                solve_tri_fast_family.solve_tri_n_template;
              concrete_dimension_source = "manifest/source launch n == 64";
            } );
        ("k_template", k_dimension);
      ];
    carrier_block_dim =
      {
        symbolic_dim_x =
          Concrete_dimension
            {
              concrete_dimension_value =
                solve_tri_fast_family.solve_tri_block_dim_x;
              concrete_dimension_source =
                solve_tri_fast_family.solve_tri_block_dim_source;
            };
        symbolic_dim_y = k_dimension;
        symbolic_dim_z =
          Concrete_dimension
            { concrete_dimension_value = 1; concrete_dimension_source = "z" };
      };
    carrier_grid_dim_source = solve_tri_fast_family.solve_tri_grid_dim_source;
    carrier_dynamic_shared_memory =
      solve_tri_fast_family.solve_tri_dynamic_shared_memory;
    carrier_launch_branch_conditions =
      [
        "n == " ^ string_of_int solve_tri_fast_family.solve_tri_n_template;
        "case K in {"
        ^ String.concat ", "
            (List.map string_of_int solve_tri_symbolic_k_candidate_values)
        ^ "}";
      ];
    carrier_positive_shape_guards =
      [
        "n_template > 0";
        "K > 0";
        "blockDim.x == "
        ^ string_of_int solve_tri_fast_family.solve_tri_block_dim_x;
        "blockDim.y == K";
        "blockDim.z == 1";
        "gridDim.x > 0";
        "gridDim.y == 1";
        "gridDim.z == 1";
      ];
    carrier_subgroup_size = solve_tri_fast_family.solve_tri_subgroup_size;
    carrier_route_owner = solve_tri_symbolic_k_guard.symbolic_guard_route_owner;
    carrier_candidate_rows = solve_tri_fast_family.solve_tri_symbolic_k_rows;
    carrier_lookup_anchor_row_ids =
      solve_tri_symbolic_k_guard.symbolic_guard_lookup_anchor_row_ids;
    carrier_unpromoted_row_ids =
      solve_tri_fast_family.solve_tri_unpromoted_row_ids;
    carrier_excluded_row_ids = solve_tri_fast_family.solve_tri_excluded_row_ids;
  }

let launch_family_of_catalog_family = function
  | Launch_contract_rows.Gla -> Gla
  | Launch_contract_rows.Wkv -> Wkv
  | Launch_contract_rows.Wkv7 -> Wkv7

let launch_contract_row_of_generated (row : t) =
  let contract = row.contract in
  {
    launch_row_id = contract.row_id;
    launch_family = launch_family_of_catalog_family contract.family;
    launch_manifest_kernel = contract.manifest_kernel;
    launch_parsed_kernel = contract.parsed_kernel;
    launch_template_arg = contract.template_arg;
    launch_template_param = Some contract.template_param;
    launch_template_value = Some contract.template_value;
    launch_template_bindings =
      [ (contract.template_param, contract.template_value) ];
    launch_template_domains =
      [ int_template_domain contract.template_param contract.template_value ];
    launch_shape_contract = row.shape_contract;
    launch_block_dim = Some [ contract.template_value; 1; 1 ];
    launch_symbolic_dimension_carrier = None;
  }

let launch_contract_row_of_selected (selected : selected_row) =
  let n_template =
    match List.assoc_opt "n_template" selected.selected_template_bindings with
    | Some value -> value
    | None ->
        invalid_arg
          ("solve-tri selected row " ^ selected.selected_row_id
         ^ " is missing n_template")
  in
  {
    launch_row_id = selected.selected_row_id;
    launch_family = Solve_tri_fast;
    launch_manifest_kernel = selected.selected_manifest_kernel;
    launch_parsed_kernel = selected.selected_parsed_kernel;
    launch_template_arg = selected.selected_template_arg;
    launch_template_param = Some "n_template";
    launch_template_value = Some n_template;
    launch_template_bindings = selected.selected_template_bindings;
    launch_template_domains =
      List.map
        (fun (parameter, value) -> int_template_domain parameter value)
        selected.selected_template_bindings;
    launch_shape_contract = selected_row_shape_contract selected;
    launch_block_dim = Some selected.selected_concrete_block_dim;
    launch_symbolic_dimension_carrier =
      Some solve_tri_symbolic_dimension_carrier;
  }

let launch_contract_row_of_finite_type (row : finite_type_row) =
  {
    launch_row_id = row.finite_type_row_id;
    launch_family = Finite_type_template;
    launch_manifest_kernel = row.finite_type_manifest_kernel;
    launch_parsed_kernel = row.finite_type_parsed_kernel;
    launch_template_arg = row.finite_type_template_arg;
    launch_template_param = None;
    launch_template_value = None;
    launch_template_bindings = [];
    launch_template_domains =
      List.map finite_type_template_domain row.finite_type_domains;
    launch_shape_contract = finite_type_shape_contract row;
    launch_block_dim = Some row.finite_type_block_dim;
    launch_symbolic_dimension_carrier = None;
  }

let solve_tri_symbolic_k_obligation_blocker =
  {
    blocker_route_owner = solve_tri_symbolic_k_guard.symbolic_guard_route_owner;
    blocker_symbolic_parameter =
      solve_tri_symbolic_k_guard.symbolic_guard_k_parameter;
    blocker_reason =
      "symbolic K is represented in the launch-contract guard and typed \
       dimension carrier; S437 consumes that carrier as symbolic checked block \
       dimensions in Memory_event.Subgroup_obligation, but the launch-contract \
       guard itself is still provenance rather than solver input";
    blocker_zero_obligation_cause =
      "the launch-contract guard/carrier alone is not a solver obligation; \
       proof remains blocked until a later task consumes fresh symbolic \
       Subgroup_obligation artifacts containing K";
    blocker_next_step =
      "read back fresh S437 symbolic checked-domain obligations before running \
       any solver or pre-solver proof over K";
  }

let host_template_specialization_candidate_carrier =
  {
    candidate_carrier_id = "s447_host_template_specialization";
    candidate_priority_bucket = "host_or_template_resolution_schema_candidate";
    candidate_first_blocker = "template_args_unresolved_or_conflicting";
    candidate_proof_ladder_stage = "blocked_at_host_or_template_specialization";
    candidate_source_ledger =
      "agent_results/rewrite/component_summaries/S445/guarded_expansion_sweep.json";
    candidate_affected_family_count = 54;
    candidate_required_fact_keys =
      [
        "source_file";
        "kernel_or_template";
        "concrete_template_args";
        "launch_site";
        "preprocessing_profile";
        "extraction_fixture";
        "selected_include_order";
        "macro_profile";
        "block_dim_source";
        "grid_dim_source";
        "dynamic_shared_memory";
      ];
    candidate_route_owner =
      "Launch_contract_generator.guarded_candidate_carrier";
    candidate_solver_policy = "not_solver_input";
    candidate_admission_status = "blocked_no_fresh_obligation";
    candidate_next_support_step =
      "derive row-owned host/template specialization facts before source or \
       proof work";
  }

let template_argument_resolution_carrier =
  {
    template_resolution_carrier_id = "s475_template_argument_resolution";
    template_resolution_source_ledger =
      "agent_results/rewrite/component_summaries/S475/template_arg_resolution_ledger.json";
    template_resolution_input_blocker_ledger =
      "agent_results/rewrite/component_summaries/S470/launch_template_carrier_blockers.json";
    template_resolution_proof_ladder_stage =
      "blocked_after_template_argument_resolution";
    template_resolution_families_attempted = 54;
    template_resolution_rows_attempted = 79;
    template_resolution_rows_known = 48;
    template_resolution_rows_blocked = 31;
    template_resolution_families_known = 33;
    template_resolution_families_blocked = 21;
    template_resolution_family_resolution_counts =
      [
        ("all_rows_template_args_known", 6);
        ("not_applicable_no_template_args", 27);
        ("some_rows_still_dependent_or_unresolved", 21);
      ];
    template_resolution_row_resolution_counts =
      [
        ("concrete_from_launch_expression", 18);
        ("concrete_from_local_const", 2);
        ("dependent_or_unresolved_template_args", 31);
        ("not_applicable_no_template_args", 28);
      ];
    template_resolution_route_owner =
      "Launch_contract_generator.template_argument_resolution_carrier";
    template_resolution_solver_policy = "not_solver_input";
    template_resolution_admission_status = "blocked_no_fresh_obligation";
    template_resolution_next_support_step =
      "consume S475-known families in the proof-input frontier; extract host \
       template domains for S475-blocked families";
  }

let launch_branch_frontier_carrier =
  {
    launch_branch_carrier_id = "s477_launch_branch_frontier";
    launch_branch_source_ledger =
      "agent_results/rewrite/component_summaries/S477/launch_branch_frontier_ledger.json";
    launch_branch_input_frontier_ledger =
      "agent_results/rewrite/component_summaries/S476/s475_consumed_frontier_ledger.json";
    launch_branch_proof_ladder_stage = "blocked_after_launch_branch_frontier";
    launch_branch_families_attempted = 33;
    launch_branch_rows_attempted = 46;
    launch_branch_rows_known = 45;
    launch_branch_rows_blocked = 1;
    launch_branch_families_known = 32;
    launch_branch_families_blocked = 1;
    launch_branch_row_resolution_counts =
      [
        ("indirect_kernel_parameter", 1);
        ("selected_source_launch_branch_profile", 45);
      ];
    launch_branch_family_status_counts = [ ("blocked", 1); ("known", 32) ];
    launch_branch_next_blocker_counts = [ ("positive_shape_guard_status", 32) ];
    launch_branch_remaining_blocker_counts =
      [ ("launch_branch_status", 1); ("positive_shape_guard_status", 33) ];
    launch_branch_route_owner =
      "Launch_contract_generator.launch_branch_frontier_carrier";
    launch_branch_solver_policy = "not_solver_input";
    launch_branch_admission_status = "blocked_no_fresh_obligation";
    launch_branch_next_support_step =
      "consume S477 positive-shape families in the proof-input frontier; \
       extract indirect kernel specialization for the one S477-blocked helper";
  }

let positive_shape_required_fact_keys =
  [
    "row_domain";
    "block_dim_positive_domain";
    "grid_dim_positive_domain";
    "dynamic_shared_memory_domain";
    "zero_work_exclusion";
    "covered_rows";
    "excluded_rows";
    "source_profile_or_extraction_fixture";
    "memory_event_list";
  ]

let positive_shape_family ~family_id ~family_key ~rows ~origin =
  {
    positive_shape_family_id = family_id;
    positive_shape_family_key = family_key;
    positive_shape_rows = rows;
    positive_shape_origin = origin;
    positive_shape_first_blocker = "positive_shape_guard_status";
  }

let s477_positive_shape_origin = "s477_selected_launch_branch_profile"
let preexisting_positive_shape_origin = "preexisting_guarded_family_boundary"

let positive_shape_candidate_families =
  [
    positive_shape_family ~family_id:"F011"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/clamp.cu :: op_clamp_kernel"
      ~rows:[ "L012" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F016"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/conv2d-dw.cu :: conv2d_dw_kernel"
      ~rows:[ "L018"; "L019" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F017"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/conv2d-transpose.cu :: \
         conv2d_transpose_kernel"
      ~rows:[ "L020"; "L021" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F020"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q8_0_f16"
      ~rows:[ "L024"; "L025" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F021"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q2_K"
      ~rows:[ "L026" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F022"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q3_K"
      ~rows:[ "L027" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F023"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q4_0"
      ~rows:[ "L028" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F024"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q4_1"
      ~rows:[ "L029" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F025"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q4_K"
      ~rows:[ "L030" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F026"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q5_K"
      ~rows:[ "L031" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F027"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q6_K"
      ~rows:[ "L032" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F028"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq2_xxs"
      ~rows:[ "L033" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F029"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq2_xs"
      ~rows:[ "L034" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F030"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq2_s"
      ~rows:[ "L035" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F031"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq3_xxs"
      ~rows:[ "L036" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F032"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq3_s"
      ~rows:[ "L037" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F033"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq1_s"
      ~rows:[ "L038" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F034"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq4_nl"
      ~rows:[ "L039" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F035"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq1_m"
      ~rows:[ "L040" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F036"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq4_xs"
      ~rows:[ "L041" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F037"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_mxfp4"
      ~rows:[ "L042" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F038"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_nvfp4"
      ~rows:[ "L043" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F041"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/cpy.cu :: cpy_f32_q"
      ~rows:[ "L046"; "L048"; "L050"; "L052"; "L054"; "L056" ]
      ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F042"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/cpy.cu :: cpy_q_f32"
      ~rows:[ "L047"; "L049"; "L051"; "L053"; "L055" ]
      ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F050"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/fill.cu :: fill_kernel"
      ~rows:[ "L067"; "L068" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F055"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/im2col.cu :: im2col_kernel"
      ~rows:[ "L074" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F056"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/im2col.cu :: im2col_3d_kernel"
      ~rows:[ "L075" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F057"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/mean.cu :: divide_by_count"
      ~rows:[ "L076" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F082"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/solve_tri.cu :: solve_tri_f32_fast"
      ~rows:
        [
          "L117";
          "L118";
          "L119";
          "L120";
          "L121";
          "L122";
          "L123";
          "L124";
          "L125";
          "L126";
          "L127";
          "L128";
        ]
      ~origin:preexisting_positive_shape_origin;
    positive_shape_family ~family_id:"F086"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: swiglu_oai_kernel"
      ~rows:[ "L135" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F087"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: xielu_kernel"
      ~rows:[ "L136" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F088"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: silu_back_kernel"
      ~rows:[ "L137" ] ~origin:s477_positive_shape_origin;
    positive_shape_family ~family_id:"F089"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: leaky_relu_kernel"
      ~rows:[ "L138" ] ~origin:s477_positive_shape_origin;
  ]

type all_family_frontier_status = All_family_blocked | All_family_unsupported

type all_family_frontier_family = {
  all_family_id : string;
  all_family_key : string;
  all_family_rows : string list;
  all_family_baseline_status : all_family_frontier_status;
  all_family_attempted_stage : string;
  all_family_first_blocker : string;
  all_family_unsupported_category : string option;
}

let all_family_frontier_status_to_string = function
  | All_family_blocked -> "blocked"
  | All_family_unsupported -> "unsupported"

let all_family_frontier_family ?unsupported_category ~family_id ~family_key
    ~rows ~baseline_status ~attempted_stage ~first_blocker () =
  {
    all_family_id = family_id;
    all_family_key = family_key;
    all_family_rows = rows;
    all_family_baseline_status = baseline_status;
    all_family_attempted_stage = attempted_stage;
    all_family_first_blocker = first_blocker;
    all_family_unsupported_category = unsupported_category;
  }

let all_family_frontier_families =
  [
    all_family_frontier_family ~family_id:"F001"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/acc.cu :: acc_f32"
      ~rows:[ "L001" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F002"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/add-id.cu :: add_id_kernel"
      ~rows:[ "L002" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F003"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/allreduce.cu :: ggml_cuda_ar_add_kernel"
      ~rows:[ "L003" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F004"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/allreduce.cu :: ggml_cuda_ar_kernel"
      ~rows:[ "L004" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F005"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/arange.cu :: arange_f32"
      ~rows:[ "L005" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F006"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/argmax.cu :: argmax_f32"
      ~rows:[ "L006" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary"
      ~first_blocker:"subgroup_shuffle_reduction"
      ~unsupported_category:"subgroup_shuffle_reduction" ();
    all_family_frontier_family ~family_id:"F007"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/argsort.cu :: init_indices"
      ~rows:[ "L007" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary"
      ~first_blocker:"cub_or_cooperative_helper"
      ~unsupported_category:"cub_or_cooperative_helper" ();
    all_family_frontier_family ~family_id:"F008"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/argsort.cu :: init_offsets"
      ~rows:[ "L008" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary"
      ~first_blocker:"cub_or_cooperative_helper"
      ~unsupported_category:"cub_or_cooperative_helper" ();
    all_family_frontier_family ~family_id:"F009"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/argsort.cu :: k_argsort_f32_i32"
      ~rows:[ "L009"; "L010" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary"
      ~first_blocker:"cub_or_cooperative_helper"
      ~unsupported_category:"cub_or_cooperative_helper" ();
    all_family_frontier_family ~family_id:"F010"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/binbcast.cu :: k_repeat_back"
      ~rows:[ "L011" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F011"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/clamp.cu :: op_clamp_kernel"
      ~rows:[ "L012" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F012"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/common.cuh :: kernel"
      ~rows:[ "L013" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F013"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/concat.cu :: concat_f32_cont"
      ~rows:[ "L014"; "L015" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F014"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/concat.cu :: concat_f32_non_cont"
      ~rows:[ "L016" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F015"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/conv-transpose-1d.cu :: \
         conv_transpose_1d_kernel"
      ~rows:[ "L017" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F016"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/conv2d-dw.cu :: conv2d_dw_kernel"
      ~rows:[ "L018"; "L019" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F017"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/conv2d-transpose.cu :: \
         conv2d_transpose_kernel"
      ~rows:[ "L020"; "L021" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F018"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/conv2d.cu :: conv2d_kernel"
      ~rows:[ "L022" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F019"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block"
      ~rows:[ "L023" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F020"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q8_0_f16"
      ~rows:[ "L024"; "L025" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F021"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q2_K"
      ~rows:[ "L026" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F022"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q3_K"
      ~rows:[ "L027" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F023"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q4_0"
      ~rows:[ "L028" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F024"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q4_1"
      ~rows:[ "L029" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F025"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q4_K"
      ~rows:[ "L030" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F026"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q5_K"
      ~rows:[ "L031" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F027"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_q6_K"
      ~rows:[ "L032" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F028"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq2_xxs"
      ~rows:[ "L033" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F029"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq2_xs"
      ~rows:[ "L034" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F030"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq2_s"
      ~rows:[ "L035" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F031"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq3_xxs"
      ~rows:[ "L036" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F032"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq3_s"
      ~rows:[ "L037" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F033"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq1_s"
      ~rows:[ "L038" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F034"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq4_nl"
      ~rows:[ "L039" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F035"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq1_m"
      ~rows:[ "L040" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F036"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_iq4_xs"
      ~rows:[ "L041" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F037"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_mxfp4"
      ~rows:[ "L042" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F038"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/convert.cu :: dequantize_block_nvfp4"
      ~rows:[ "L043" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F039"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/convert.cu :: convert_unary"
      ~rows:[ "L044" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F040"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/count-equal.cu :: count_equal"
      ~rows:[ "L045" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary" ~first_blocker:"atomic"
      ~unsupported_category:"atomic" ();
    all_family_frontier_family ~family_id:"F041"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/cpy.cu :: cpy_f32_q"
      ~rows:[ "L046"; "L048"; "L050"; "L052"; "L054"; "L056" ]
      ~baseline_status:All_family_blocked ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F042"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/cpy.cu :: cpy_q_f32"
      ~rows:[ "L047"; "L049"; "L051"; "L053"; "L055" ]
      ~baseline_status:All_family_blocked ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F043"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/cross-entropy-loss.cu :: \
         cross_entropy_loss_f32"
      ~rows:[ "L057"; "L058" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"subgroup_config" ~first_blocker:"subgroup_target_config"
      ();
    all_family_frontier_family ~family_id:"F044"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/cross-entropy-loss.cu :: \
         cross_entropy_loss_back_f32"
      ~rows:[ "L059"; "L060" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F045"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/cumsum.cu :: cumsum_cub_kernel"
      ~rows:[ "L061" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F046"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/cumsum.cu :: cumsum_kernel"
      ~rows:[ "L062" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary"
      ~first_blocker:"cub_or_cooperative_helper"
      ~unsupported_category:"cub_or_cooperative_helper" ();
    all_family_frontier_family ~family_id:"F047"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/diag.cu :: diag_kernel"
      ~rows:[ "L063"; "L064" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F048"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/diagmask.cu :: diag_mask_inf_f32"
      ~rows:[ "L065" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F049"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/fattn-common.cuh :: \
         flash_attn_mask_to_KV_max"
      ~rows:[ "L066" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F050"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/fill.cu :: fill_kernel"
      ~rows:[ "L067"; "L068" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F051"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/getrows.cu :: k_get_rows"
      ~rows:[ "L069" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F052"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/getrows.cu :: k_get_rows_back_float"
      ~rows:[ "L070" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F053"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/ggml-cuda.cu :: k_compute_batched_ptrs"
      ~rows:[ "L071" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F054"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/gla.cu :: gated_linear_attn_f32"
      ~rows:[ "L072"; "L073" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"row_derived_symbolic_family_guard"
      ~first_blocker:"missing_executable_symbolic_family_guard" ();
    all_family_frontier_family ~family_id:"F055"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/im2col.cu :: im2col_kernel"
      ~rows:[ "L074" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F056"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/im2col.cu :: im2col_3d_kernel"
      ~rows:[ "L075" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F057"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/mean.cu :: divide_by_count"
      ~rows:[ "L076" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F058"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/mmf.cuh :: mul_mat_f_ids"
      ~rows:[ "L077" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F059"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/mmf.cuh :: mul_mat_f"
      ~rows:[ "L078"; "L079" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F060"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/mmid.cu :: mm_ids_helper"
      ~rows:[ "L080" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F061"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/mmq.cuh :: mul_mat_q"
      ~rows:[ "L081"; "L082"; "L083"; "L085" ]
      ~baseline_status:All_family_blocked ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F062"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/mmq.cuh :: mul_mat_q_stream_k_fixup"
      ~rows:[ "L084"; "L086" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F063"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/norm.cu :: norm_f32"
      ~rows:[ "L087"; "L088" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F064"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/norm.cu :: group_norm_f32"
      ~rows:[ "L089"; "L090" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F065"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/norm.cu :: rms_norm_back_f32"
      ~rows:[ "L091"; "L092" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F066"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/opt-step-adamw.cu :: opt_step_adamw_f32"
      ~rows:[ "L093" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F067"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/opt-step-sgd.cu :: opt_step_sgd_f32"
      ~rows:[ "L094" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F068"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/pad.cu :: pad_f32"
      ~rows:[ "L095" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F069"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/pad_reflect_1d.cu :: \
         pad_reflect_1d_kernel_f32"
      ~rows:[ "L096" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F070"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/pool2d.cu :: pool2d_nchw_kernel"
      ~rows:[ "L097" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F071"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/quantize.cu :: quantize_mmq_q8_1"
      ~rows:[ "L098"; "L099"; "L100" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary"
      ~first_blocker:"subgroup_shuffle_reduction"
      ~unsupported_category:"subgroup_shuffle_reduction" ();
    all_family_frontier_family ~family_id:"F072"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/quantize.cu :: quantize_mmq_nvfp4"
      ~rows:[ "L101" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F073"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/quantize.cu :: quantize_mmq_mxfp4"
      ~rows:[ "L102" ] ~baseline_status:All_family_unsupported
      ~attempted_stage:"unsupported_operation_boundary"
      ~first_blocker:"subgroup_shuffle_reduction"
      ~unsupported_category:"subgroup_shuffle_reduction" ();
    all_family_frontier_family ~family_id:"F074"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/roll.cu :: roll_f32_cuda"
      ~rows:[ "L103" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F075"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/rope.cu :: rope_norm"
      ~rows:[ "L104"; "L105" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F076"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/rope.cu :: rope_vision"
      ~rows:[ "L106"; "L107" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F077"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/set-rows.cu :: k_set_rows_quant"
      ~rows:[ "L108" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F078"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/snake.cu :: snake_kernel"
      ~rows:[ "L109"; "L110"; "L111" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F079"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/softmax.cu :: soft_max_f32"
      ~rows:[ "L112"; "L113"; "L114" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F080"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/softmax.cu :: soft_max_back_f32"
      ~rows:[ "L115" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F081"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/solve_tri.cu :: get_batch_pointers"
      ~rows:[ "L116" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F082"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/solve_tri.cu :: solve_tri_f32_fast"
      ~rows:
        [
          "L117";
          "L118";
          "L119";
          "L120";
          "L121";
          "L122";
          "L123";
          "L124";
          "L125";
          "L126";
          "L127";
          "L128";
        ]
      ~baseline_status:All_family_blocked ~attempted_stage:"block_grid_shape"
      ~first_blocker:"positive_shape_guard_status" ();
    all_family_frontier_family ~family_id:"F083"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/ssm-conv.cu :: ssm_conv_long_token_f32"
      ~rows:[ "L129" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F084"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/tri.cu :: tri_kernel"
      ~rows:[ "L130"; "L131"; "L132"; "L133" ]
      ~baseline_status:All_family_blocked ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F085"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/tsembd.cu :: timestep_embedding_f32"
      ~rows:[ "L134" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F086"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: swiglu_oai_kernel"
      ~rows:[ "L135" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F087"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: xielu_kernel"
      ~rows:[ "L136" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F088"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: silu_back_kernel"
      ~rows:[ "L137" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F089"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/unary.cu :: leaky_relu_kernel"
      ~rows:[ "L138" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"launch_template"
      ~first_blocker:"template_argument_status" ();
    all_family_frontier_family ~family_id:"F090"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/upscale.cu :: upscale_f32"
      ~rows:[ "L139" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F091"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/upscale.cu :: \
         upscale_f32_bilinear_antialias"
      ~rows:[ "L140" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F092"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/upscale.cu :: upscale_f32_bilinear"
      ~rows:[ "L141" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F093"
      ~family_key:
        "llama.cpp/ggml/src/ggml-cuda/upscale.cu :: upscale_f32_bicubic"
      ~rows:[ "L142" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"preprocessing" ~first_blocker:"preprocessing_profile" ();
    all_family_frontier_family ~family_id:"F094"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/wkv.cu :: rwkv_wkv_f32"
      ~rows:[ "L143"; "L144" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"row_derived_symbolic_family_guard"
      ~first_blocker:"missing_executable_symbolic_family_guard" ();
    all_family_frontier_family ~family_id:"F095"
      ~family_key:"llama.cpp/ggml/src/ggml-cuda/wkv.cu :: rwkv_wkv7_f32"
      ~rows:[ "L145"; "L146" ] ~baseline_status:All_family_blocked
      ~attempted_stage:"row_derived_symbolic_family_guard"
      ~first_blocker:"missing_executable_symbolic_family_guard" ();
  ]

let all_family_frontier_family_count = List.length all_family_frontier_families

let all_family_frontier_row_count =
  all_family_frontier_families
  |> List.fold_left
       (fun total family -> total + List.length family.all_family_rows)
       0

let positive_shape_family_row_count family =
  List.length family.positive_shape_rows

let positive_shape_family_spec family =
  family.positive_shape_family_id ^ ":rows="
  ^ String.concat "," family.positive_shape_rows
  ^ ":origin=" ^ family.positive_shape_origin ^ ":family_key="
  ^ family.positive_shape_family_key

let positive_shape_guard_carrier =
  {
    positive_shape_carrier_id = "s478_positive_shape_guard_frontier";
    positive_shape_source_ledger =
      "agent_results/rewrite/component_summaries/S477/launch_branch_frontier_ledger.json";
    positive_shape_input_frontier_ledger =
      "agent_results/rewrite/component_summaries/S477/launch_branch_frontier_families.tsv";
    positive_shape_proof_ladder_stage =
      "blocked_at_positive_shape_guard_construction";
    positive_shape_families_attempted = 33;
    positive_shape_rows_attempted = 57;
    positive_shape_families_from_s477 = 32;
    positive_shape_rows_from_s477 = 45;
    positive_shape_preexisting_families = 1;
    positive_shape_preexisting_rows = 12;
    positive_shape_required_fact_keys;
    positive_shape_candidate_families;
    positive_shape_next_blocker_counts =
      [ ("positive_block_grid_shape_guard", 33) ];
    positive_shape_route_owner =
      "Launch_contract_generator.positive_shape_guard_carrier";
    positive_shape_solver_policy = "not_solver_input";
    positive_shape_admission_status = "blocked_no_fresh_obligation";
    positive_shape_next_support_step =
      "derive executable positive block/grid/dynamic-shared-memory guards and \
       zero-work exclusions per family before memory-event obligation \
       construction";
  }

let positive_shape_verification_carrier =
  {
    positive_shape_verification_carrier_id = "s479_positive_shape_verification";
    positive_shape_verification_source_ledger =
      "agent_results/rewrite/component_summaries/S479/positive_shape_verification_ledger.json";
    positive_shape_verification_family_ledger =
      "agent_results/rewrite/component_summaries/S479/positive_shape_verification_families.tsv";
    positive_shape_verification_artifact_dir =
      "agent_results/rewrite/component_summaries/S479/artifacts";
    positive_shape_verification_families_attempted = 33;
    positive_shape_verification_rows_attempted = 57;
    positive_shape_verification_source_slice_verified_families = 32;
    positive_shape_verification_existing_guarded_families = 1;
    positive_shape_verification_blocked_families = 0;
    positive_shape_verification_source_slice_artifacts = 32;
    positive_shape_verification_exact_production_promotions = 0;
    positive_shape_verification_manifest_verdict_fields_changed = false;
    positive_shape_verification_family_status_counts =
      [
        ("source_slice_verified", 32);
        ("verified_existing_guarded_symbolic_family", 1);
      ];
    positive_shape_verification_row_status_counts =
      [
        ("source_slice_verified", 45);
        ("verified_existing_guarded_symbolic_family", 12);
      ];
    positive_shape_verification_candidate_families =
      positive_shape_candidate_families;
    positive_shape_verification_evidence_policy =
      "source_slice_or_existing_guarded_family_evidence";
    positive_shape_verification_admission_status =
      "not_exact_production_promotion";
    positive_shape_verification_next_support_step =
      "replace generated source-slice evidence with row-owned extraction \
       profiles or exact launch-contract rows before manifest promotion";
  }

let l012_clamp_production_family_spec =
  "F011:rows=L012:family_key=llama.cpp/ggml/src/ggml-cuda/clamp.cu :: \
   op_clamp_kernel:template_domain=T={half,float}:blockDim=[256,1,1]:gridDim=(k+255)/256:guard=k>0:artifact=agent_results/rewrite/component_summaries/S480/artifacts/L012_op_clamp_kernel_production_backed.cu"

let s481_profile_family_specs =
  [
    "F016:rows=L018,L019:family_key=llama.cpp/ggml/src/ggml-cuda/conv2d-dw.cu \
     :: \
     conv2d_dw_kernel:kernel=conv2d_dw_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F016_conv2d_dw_kernel_profile.json";
    "F017:rows=L020,L021:family_key=llama.cpp/ggml/src/ggml-cuda/conv2d-transpose.cu \
     :: \
     conv2d_transpose_kernel:kernel=conv2d_transpose_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F017_conv2d_transpose_kernel_profile.json";
    "F020:rows=L024,L025:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q8_0_f16:kernel=dequantize_block_q8_0_f16:profile=agent_results/rewrite/component_summaries/S481/profiles/F020_dequantize_block_q8_0_f16_profile.json";
    "F021:rows=L026:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q2_K:kernel=dequantize_block_q2_K:profile=agent_results/rewrite/component_summaries/S481/profiles/F021_dequantize_block_q2_K_profile.json";
    "F022:rows=L027:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q3_K:kernel=dequantize_block_q3_K:profile=agent_results/rewrite/component_summaries/S481/profiles/F022_dequantize_block_q3_K_profile.json";
    "F023:rows=L028:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q4_0:kernel=dequantize_block_q4_0:profile=agent_results/rewrite/component_summaries/S481/profiles/F023_dequantize_block_q4_0_profile.json";
    "F024:rows=L029:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q4_1:kernel=dequantize_block_q4_1:profile=agent_results/rewrite/component_summaries/S481/profiles/F024_dequantize_block_q4_1_profile.json";
    "F025:rows=L030:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q4_K:kernel=dequantize_block_q4_K:profile=agent_results/rewrite/component_summaries/S481/profiles/F025_dequantize_block_q4_K_profile.json";
    "F026:rows=L031:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q5_K:kernel=dequantize_block_q5_K:profile=agent_results/rewrite/component_summaries/S481/profiles/F026_dequantize_block_q5_K_profile.json";
    "F027:rows=L032:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_q6_K:kernel=dequantize_block_q6_K:profile=agent_results/rewrite/component_summaries/S481/profiles/F027_dequantize_block_q6_K_profile.json";
    "F028:rows=L033:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq2_xxs:kernel=dequantize_block_iq2_xxs:profile=agent_results/rewrite/component_summaries/S481/profiles/F028_dequantize_block_iq2_xxs_profile.json";
    "F029:rows=L034:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq2_xs:kernel=dequantize_block_iq2_xs:profile=agent_results/rewrite/component_summaries/S481/profiles/F029_dequantize_block_iq2_xs_profile.json";
    "F030:rows=L035:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq2_s:kernel=dequantize_block_iq2_s:profile=agent_results/rewrite/component_summaries/S481/profiles/F030_dequantize_block_iq2_s_profile.json";
    "F031:rows=L036:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq3_xxs:kernel=dequantize_block_iq3_xxs:profile=agent_results/rewrite/component_summaries/S481/profiles/F031_dequantize_block_iq3_xxs_profile.json";
    "F032:rows=L037:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq3_s:kernel=dequantize_block_iq3_s:profile=agent_results/rewrite/component_summaries/S481/profiles/F032_dequantize_block_iq3_s_profile.json";
    "F033:rows=L038:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq1_s:kernel=dequantize_block_iq1_s:profile=agent_results/rewrite/component_summaries/S481/profiles/F033_dequantize_block_iq1_s_profile.json";
    "F034:rows=L039:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq4_nl:kernel=dequantize_block_iq4_nl:profile=agent_results/rewrite/component_summaries/S481/profiles/F034_dequantize_block_iq4_nl_profile.json";
    "F035:rows=L040:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq1_m:kernel=dequantize_block_iq1_m:profile=agent_results/rewrite/component_summaries/S481/profiles/F035_dequantize_block_iq1_m_profile.json";
    "F036:rows=L041:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_iq4_xs:kernel=dequantize_block_iq4_xs:profile=agent_results/rewrite/component_summaries/S481/profiles/F036_dequantize_block_iq4_xs_profile.json";
    "F037:rows=L042:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_mxfp4:kernel=dequantize_block_mxfp4:profile=agent_results/rewrite/component_summaries/S481/profiles/F037_dequantize_block_mxfp4_profile.json";
    "F038:rows=L043:family_key=llama.cpp/ggml/src/ggml-cuda/convert.cu :: \
     dequantize_block_nvfp4:kernel=dequantize_block_nvfp4:profile=agent_results/rewrite/component_summaries/S481/profiles/F038_dequantize_block_nvfp4_profile.json";
    "F041:rows=L046,L048,L050,L052,L054,L056:family_key=llama.cpp/ggml/src/ggml-cuda/cpy.cu \
     :: \
     cpy_f32_q:kernel=cpy_f32_q:profile=agent_results/rewrite/component_summaries/S481/profiles/F041_cpy_f32_q_profile.json";
    "F042:rows=L047,L049,L051,L053,L055:family_key=llama.cpp/ggml/src/ggml-cuda/cpy.cu \
     :: \
     cpy_q_f32:kernel=cpy_q_f32:profile=agent_results/rewrite/component_summaries/S481/profiles/F042_cpy_q_f32_profile.json";
    "F050:rows=L067,L068:family_key=llama.cpp/ggml/src/ggml-cuda/fill.cu :: \
     fill_kernel:kernel=fill_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F050_fill_kernel_profile.json";
    "F055:rows=L074:family_key=llama.cpp/ggml/src/ggml-cuda/im2col.cu :: \
     im2col_kernel:kernel=im2col_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F055_im2col_kernel_profile.json";
    "F056:rows=L075:family_key=llama.cpp/ggml/src/ggml-cuda/im2col.cu :: \
     im2col_3d_kernel:kernel=im2col_3d_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F056_im2col_3d_kernel_profile.json";
    "F057:rows=L076:family_key=llama.cpp/ggml/src/ggml-cuda/mean.cu :: \
     divide_by_count:kernel=divide_by_count:profile=agent_results/rewrite/component_summaries/S481/profiles/F057_divide_by_count_profile.json";
    "F086:rows=L135:family_key=llama.cpp/ggml/src/ggml-cuda/unary.cu :: \
     swiglu_oai_kernel:kernel=swiglu_oai_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F086_swiglu_oai_kernel_profile.json";
    "F087:rows=L136:family_key=llama.cpp/ggml/src/ggml-cuda/unary.cu :: \
     xielu_kernel:kernel=xielu_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F087_xielu_kernel_profile.json";
    "F088:rows=L137:family_key=llama.cpp/ggml/src/ggml-cuda/unary.cu :: \
     silu_back_kernel:kernel=silu_back_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F088_silu_back_kernel_profile.json";
    "F089:rows=L138:family_key=llama.cpp/ggml/src/ggml-cuda/unary.cu :: \
     leaky_relu_kernel:kernel=leaky_relu_kernel:profile=agent_results/rewrite/component_summaries/S481/profiles/F089_leaky_relu_kernel_profile.json";
  ]

let positive_shape_production_promotion_carrier =
  {
    positive_shape_production_carrier_id =
      "s481_positive_shape_production_profile";
    positive_shape_production_source_ledger =
      "agent_results/rewrite/component_summaries/S481/production_profile_ledger.json";
    positive_shape_production_input_verification_ledger =
      positive_shape_verification_carrier
        .positive_shape_verification_source_ledger;
    positive_shape_production_artifact_dir =
      "agent_results/rewrite/component_summaries/S481";
    positive_shape_production_families_attempted = 31;
    positive_shape_production_rows_attempted = 44;
    positive_shape_production_extraction_verified_families = 1;
    positive_shape_production_profile_verified_families = 31;
    positive_shape_production_source_slice_only_remaining = 0;
    positive_shape_production_existing_guarded_families_preserved = 1;
    positive_shape_production_exact_manifest_promotions = 0;
    positive_shape_production_manifest_verdict_fields_changed = false;
    positive_shape_production_status_counts =
      [
        ("production_backed_extraction_verified", 1);
        ("production_backed_profile_verified", 31);
        ("blocked", 0);
      ];
    positive_shape_production_family_specs =
      l012_clamp_production_family_spec :: s481_profile_family_specs;
    positive_shape_production_evidence_policy =
      "production_backed_extraction_or_profile_not_full_host_header_parse";
    positive_shape_production_admission_status =
      "production_backed_profiles_verified_no_manifest_promotion";
    positive_shape_production_next_support_step =
      "consume S481 production-backed profiles as exact launch-contract rows \
       or manifest-promotion inputs before changing manifest verdict fields";
  }

let symbolic_obligation_blocker_lines (blocker : symbolic_obligation_blocker) =
  [
    "route_owner: " ^ blocker.blocker_route_owner;
    "symbolic_parameter: " ^ blocker.blocker_symbolic_parameter;
    "reason: " ^ blocker.blocker_reason;
    "zero_obligation_cause: " ^ blocker.blocker_zero_obligation_cause;
    "next_step: " ^ blocker.blocker_next_step;
  ]

let guarded_candidate_carrier_lines (carrier : guarded_candidate_carrier) =
  [
    "carrier_id: " ^ carrier.candidate_carrier_id;
    "priority_bucket: " ^ carrier.candidate_priority_bucket;
    "first_blocker: " ^ carrier.candidate_first_blocker;
    "proof_ladder_stage: " ^ carrier.candidate_proof_ladder_stage;
    "source_ledger: " ^ carrier.candidate_source_ledger;
    "affected_family_count: "
    ^ string_of_int carrier.candidate_affected_family_count;
    "required_fact_keys: "
    ^ String.concat ", " carrier.candidate_required_fact_keys;
    "route_owner: " ^ carrier.candidate_route_owner;
    "solver_policy: " ^ carrier.candidate_solver_policy;
    "admission_status: " ^ carrier.candidate_admission_status;
    "next_support_step: " ^ carrier.candidate_next_support_step;
  ]

let string_int_pairs_to_string pairs =
  pairs
  |> List.map (fun (key, value) -> key ^ "=" ^ string_of_int value)
  |> String.concat ", "

let template_argument_resolution_carrier_lines
    (carrier : template_argument_resolution_carrier) =
  [
    "carrier_id: " ^ carrier.template_resolution_carrier_id;
    "source_ledger: " ^ carrier.template_resolution_source_ledger;
    "input_blocker_ledger: " ^ carrier.template_resolution_input_blocker_ledger;
    "proof_ladder_stage: " ^ carrier.template_resolution_proof_ladder_stage;
    "families_attempted: "
    ^ string_of_int carrier.template_resolution_families_attempted;
    "rows_attempted: "
    ^ string_of_int carrier.template_resolution_rows_attempted;
    "rows_known: " ^ string_of_int carrier.template_resolution_rows_known;
    "rows_blocked: " ^ string_of_int carrier.template_resolution_rows_blocked;
    "families_known: "
    ^ string_of_int carrier.template_resolution_families_known;
    "families_blocked: "
    ^ string_of_int carrier.template_resolution_families_blocked;
    "family_resolution_counts: "
    ^ string_int_pairs_to_string
        carrier.template_resolution_family_resolution_counts;
    "row_resolution_counts: "
    ^ string_int_pairs_to_string
        carrier.template_resolution_row_resolution_counts;
    "route_owner: " ^ carrier.template_resolution_route_owner;
    "solver_policy: " ^ carrier.template_resolution_solver_policy;
    "admission_status: " ^ carrier.template_resolution_admission_status;
    "next_support_step: " ^ carrier.template_resolution_next_support_step;
  ]

let launch_branch_frontier_carrier_lines
    (carrier : launch_branch_frontier_carrier) =
  [
    "carrier_id: " ^ carrier.launch_branch_carrier_id;
    "source_ledger: " ^ carrier.launch_branch_source_ledger;
    "input_frontier_ledger: " ^ carrier.launch_branch_input_frontier_ledger;
    "proof_ladder_stage: " ^ carrier.launch_branch_proof_ladder_stage;
    "families_attempted: "
    ^ string_of_int carrier.launch_branch_families_attempted;
    "rows_attempted: " ^ string_of_int carrier.launch_branch_rows_attempted;
    "rows_known: " ^ string_of_int carrier.launch_branch_rows_known;
    "rows_blocked: " ^ string_of_int carrier.launch_branch_rows_blocked;
    "families_known: " ^ string_of_int carrier.launch_branch_families_known;
    "families_blocked: " ^ string_of_int carrier.launch_branch_families_blocked;
    "row_resolution_counts: "
    ^ string_int_pairs_to_string carrier.launch_branch_row_resolution_counts;
    "family_status_counts: "
    ^ string_int_pairs_to_string carrier.launch_branch_family_status_counts;
    "next_blocker_counts: "
    ^ string_int_pairs_to_string carrier.launch_branch_next_blocker_counts;
    "remaining_blocker_counts: "
    ^ string_int_pairs_to_string carrier.launch_branch_remaining_blocker_counts;
    "route_owner: " ^ carrier.launch_branch_route_owner;
    "solver_policy: " ^ carrier.launch_branch_solver_policy;
    "admission_status: " ^ carrier.launch_branch_admission_status;
    "next_support_step: " ^ carrier.launch_branch_next_support_step;
  ]

let positive_shape_guard_carrier_lines (carrier : positive_shape_guard_carrier)
    =
  [
    "carrier_id: " ^ carrier.positive_shape_carrier_id;
    "source_ledger: " ^ carrier.positive_shape_source_ledger;
    "input_frontier_ledger: " ^ carrier.positive_shape_input_frontier_ledger;
    "proof_ladder_stage: " ^ carrier.positive_shape_proof_ladder_stage;
    "families_attempted: "
    ^ string_of_int carrier.positive_shape_families_attempted;
    "rows_attempted: " ^ string_of_int carrier.positive_shape_rows_attempted;
    "families_from_s477: "
    ^ string_of_int carrier.positive_shape_families_from_s477;
    "rows_from_s477: " ^ string_of_int carrier.positive_shape_rows_from_s477;
    "preexisting_families: "
    ^ string_of_int carrier.positive_shape_preexisting_families;
    "preexisting_rows: " ^ string_of_int carrier.positive_shape_preexisting_rows;
    "required_fact_keys: "
    ^ String.concat ", " carrier.positive_shape_required_fact_keys;
    "candidate_family_count: "
    ^ string_of_int (List.length carrier.positive_shape_candidate_families);
    "candidate_row_count: "
    ^ string_of_int
        (List.fold_left
           (fun total family -> total + positive_shape_family_row_count family)
           0 carrier.positive_shape_candidate_families);
    "next_blocker_counts: "
    ^ string_int_pairs_to_string carrier.positive_shape_next_blocker_counts;
    "route_owner: " ^ carrier.positive_shape_route_owner;
    "solver_policy: " ^ carrier.positive_shape_solver_policy;
    "admission_status: " ^ carrier.positive_shape_admission_status;
    "next_support_step: " ^ carrier.positive_shape_next_support_step;
  ]

let positive_shape_verification_carrier_lines
    (carrier : positive_shape_verification_carrier) =
  [
    "carrier_id: " ^ carrier.positive_shape_verification_carrier_id;
    "source_ledger: " ^ carrier.positive_shape_verification_source_ledger;
    "family_ledger: " ^ carrier.positive_shape_verification_family_ledger;
    "artifact_dir: " ^ carrier.positive_shape_verification_artifact_dir;
    "families_attempted: "
    ^ string_of_int carrier.positive_shape_verification_families_attempted;
    "rows_attempted: "
    ^ string_of_int carrier.positive_shape_verification_rows_attempted;
    "source_slice_verified_families: "
    ^ string_of_int
        carrier.positive_shape_verification_source_slice_verified_families;
    "existing_guarded_families: "
    ^ string_of_int
        carrier.positive_shape_verification_existing_guarded_families;
    "blocked_families: "
    ^ string_of_int carrier.positive_shape_verification_blocked_families;
    "source_slice_artifacts: "
    ^ string_of_int carrier.positive_shape_verification_source_slice_artifacts;
    "exact_production_promotions: "
    ^ string_of_int
        carrier.positive_shape_verification_exact_production_promotions;
    "manifest_verdict_fields_changed: "
    ^ string_of_bool
        carrier.positive_shape_verification_manifest_verdict_fields_changed;
    "family_status_counts: "
    ^ string_int_pairs_to_string
        carrier.positive_shape_verification_family_status_counts;
    "row_status_counts: "
    ^ string_int_pairs_to_string
        carrier.positive_shape_verification_row_status_counts;
    "candidate_family_count: "
    ^ string_of_int
        (List.length carrier.positive_shape_verification_candidate_families);
    "candidate_row_count: "
    ^ string_of_int
        (List.fold_left
           (fun total family -> total + positive_shape_family_row_count family)
           0 carrier.positive_shape_verification_candidate_families);
    "evidence_policy: " ^ carrier.positive_shape_verification_evidence_policy;
    "admission_status: " ^ carrier.positive_shape_verification_admission_status;
    "next_support_step: "
    ^ carrier.positive_shape_verification_next_support_step;
  ]

let positive_shape_production_promotion_carrier_lines
    (carrier : positive_shape_production_promotion_carrier) =
  [
    "carrier_id: " ^ carrier.positive_shape_production_carrier_id;
    "source_ledger: " ^ carrier.positive_shape_production_source_ledger;
    "input_verification_ledger: "
    ^ carrier.positive_shape_production_input_verification_ledger;
    "artifact_dir: " ^ carrier.positive_shape_production_artifact_dir;
    "families_attempted: "
    ^ string_of_int carrier.positive_shape_production_families_attempted;
    "rows_attempted: "
    ^ string_of_int carrier.positive_shape_production_rows_attempted;
    "extraction_verified_families: "
    ^ string_of_int
        carrier.positive_shape_production_extraction_verified_families;
    "profile_verified_families: "
    ^ string_of_int carrier.positive_shape_production_profile_verified_families;
    "source_slice_only_remaining: "
    ^ string_of_int
        carrier.positive_shape_production_source_slice_only_remaining;
    "existing_guarded_families_preserved: "
    ^ string_of_int
        carrier.positive_shape_production_existing_guarded_families_preserved;
    "exact_manifest_promotions: "
    ^ string_of_int carrier.positive_shape_production_exact_manifest_promotions;
    "manifest_verdict_fields_changed: "
    ^ string_of_bool
        carrier.positive_shape_production_manifest_verdict_fields_changed;
    "status_counts: "
    ^ string_int_pairs_to_string carrier.positive_shape_production_status_counts;
    "family_specs: "
    ^ String.concat " | " carrier.positive_shape_production_family_specs;
    "evidence_policy: " ^ carrier.positive_shape_production_evidence_policy;
    "admission_status: " ^ carrier.positive_shape_production_admission_status;
    "next_support_step: " ^ carrier.positive_shape_production_next_support_step;
  ]

let contract_of_seed (family : Launch_contract_rows.family) (row : row_seed) =
  match family with
  | Launch_contract_rows.Gla ->
      Launch_contract_rows.gla ~row_id:row.row_id ~head_size:row.template_value
  | Wkv ->
      Launch_contract_rows.wkv ~row_id:row.row_id ~template_arg:row.template_arg
        ~block_size:row.template_value
  | Wkv7 ->
      Launch_contract_rows.wkv7 ~row_id:row.row_id
        ~template_arg:row.template_arg ~block_size:row.template_value

let row_shape_preconditions (contract : Launch_contract_rows.t) =
  let value = string_of_int contract.template_value in
  [
    contract.template_param ^ " == " ^ value;
    "blockDim.x == " ^ value;
    "blockDim.y == 1";
    "blockDim.z == 1";
    "C / H == " ^ value;
    "B > 0";
    "T > 0";
    "C > 0";
    "H > 0";
    "gridDim.x == B * H";
    "gridDim.y == 1";
    "gridDim.z == 1";
  ]

let check_string row_id field actual expected =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some actual when String.equal actual expected -> Ok ()
  | Some actual -> Error (Field_mismatch { row_id; field; expected; actual })

let check_present_string row_id field actual =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some "" -> Error (Missing_field { row_id; field })
  | Some _ -> Ok ()

let check_int row_id field actual expected =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some actual when actual = expected -> Ok ()
  | Some actual ->
      Error
        (Field_mismatch
           {
             row_id;
             field;
             expected = string_of_int expected;
             actual = string_of_int actual;
           })

let check_bool row_id field actual expected =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some actual when Bool.equal actual expected -> Ok ()
  | Some actual ->
      Error
        (Field_mismatch
           {
             row_id;
             field;
             expected = string_of_bool expected;
             actual = string_of_bool actual;
           })

let string_of_string_list values = String.concat ", " values

let string_of_int_list values =
  values |> List.map string_of_int |> String.concat ", "

let string_of_string_int_pairs values =
  values
  |> List.map (fun (key, value) -> key ^ "=" ^ string_of_int value)
  |> String.concat ", "

let check_string_list row_id field actual expected =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some actual when actual = expected -> Ok ()
  | Some actual ->
      Error
        (Field_mismatch
           {
             row_id;
             field;
             expected = string_of_string_list expected;
             actual = string_of_string_list actual;
           })

let check_string_int_pairs row_id field actual expected =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some actual when List.sort compare actual = List.sort compare expected ->
      Ok ()
  | Some actual ->
      Error
        (Field_mismatch
           {
             row_id;
             field;
             expected = string_of_string_int_pairs expected;
             actual = string_of_string_int_pairs actual;
           })

let check_nonempty_string_list row_id field actual =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some [] -> Error (Missing_field { row_id; field })
  | Some _ -> Ok ()

let check_int_list row_id field actual expected =
  match actual with
  | None -> Error (Missing_field { row_id; field })
  | Some actual when actual = expected -> Ok ()
  | Some actual ->
      Error
        (Field_mismatch
           {
             row_id;
             field;
             expected = string_of_int_list expected;
             actual = string_of_int_list actual;
           })

let check_source_branch row_id actual expected =
  match actual with
  | None -> Error (Missing_field { row_id; field = "source_branch" })
  | Some actual when actual = expected -> Ok ()
  | Some actual ->
      Error
        (Field_mismatch
           {
             row_id;
             field = "source_branch";
             expected = source_branch_to_string expected;
             actual = source_branch_to_string actual;
           })

let check_family row_id actual expected =
  match actual with
  | [] -> Error (Missing_field { row_id; field = "family" })
  | [ actual ] when actual = expected -> Ok ()
  | [ actual ] ->
      Error
        (Field_mismatch
           {
             row_id;
             field = "family";
             expected = family_to_string expected;
             actual = family_to_string actual;
           })
  | values ->
      Error
        (Ambiguous_field
           {
             row_id;
             field = "family";
             values = List.map family_to_string values;
           })

let check_selected_family row_id actual expected =
  match actual with
  | [] -> Error (Missing_field { row_id; field = "family" })
  | [ actual ] when actual = expected -> Ok ()
  | [ actual ] ->
      Error
        (Field_mismatch
           {
             row_id;
             field = "family";
             expected = selected_family_to_string expected;
             actual = selected_family_to_string actual;
           })
  | values ->
      Error
        (Ambiguous_field
           {
             row_id;
             field = "family";
             values = List.map selected_family_to_string values;
           })

let fact_status_is_complete status = starts_with ~prefix:"populated" status

let check_required_fact_statuses row_id required_keys actual =
  match actual with
  | None -> Error (Missing_field { row_id; field = "required_fact_statuses" })
  | Some statuses ->
      let actual_keys = List.map fst statuses in
      let missing =
        List.filter
          (fun required -> not (List.mem required actual_keys))
          required_keys
      in
      let extras =
        List.filter
          (fun actual -> not (List.mem actual required_keys))
          actual_keys
      in
      if missing <> [] then
        Error (Missing_field { row_id; field = List.hd missing })
      else if extras <> [] then
        Error
          (Field_mismatch
             {
               row_id;
               field = "required_fact_keys";
               expected = string_of_string_list required_keys;
               actual = string_of_string_list actual_keys;
             })
      else Ok ()

let required_fact_statuses_complete = function
  | None -> false
  | Some statuses ->
      List.for_all (fun (_, status) -> fact_status_is_complete status) statuses

let check_candidate_readiness row_id facts =
  let ready_status = "ready_for_obligation_construction" in
  if
    required_fact_statuses_complete
      facts.candidate_row_fact_required_fact_statuses
  then
    check_string row_id "carrier_readiness_status"
      facts.candidate_row_fact_carrier_readiness_status ready_status
  else
    match facts.candidate_row_fact_carrier_readiness_status with
    | None ->
        Error (Missing_field { row_id; field = "carrier_readiness_status" })
    | Some status when starts_with ~prefix:"not_ready_" status ->
        check_nonempty_string_list row_id "missing_fact_if_any"
          facts.candidate_row_fact_missing_fact_if_any
    | Some status ->
        Error
          (Field_mismatch
             {
               row_id;
               field = "carrier_readiness_status";
               expected = "not_ready_*";
               actual = status;
             })

let validate_manifest_facts (generated : t) (facts : manifest_facts) =
  let contract = generated.contract in
  let row_id = contract.row_id in
  let checks =
    [
      (fun () ->
        if String.equal facts.row_id row_id then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "row_id";
                 expected = row_id;
                 actual = facts.row_id;
               }));
      (fun () -> check_family row_id facts.family_candidates contract.family);
      (fun () ->
        check_string row_id "manifest_kernel" facts.manifest_kernel
          contract.manifest_kernel);
      (fun () ->
        check_string row_id "parsed_kernel" facts.parsed_kernel
          contract.parsed_kernel);
      (fun () ->
        check_string row_id "template_arg" facts.template_arg
          contract.template_arg);
      (fun () ->
        check_int row_id "template_value" facts.template_value
          contract.template_value);
      (fun () ->
        check_string row_id "block_dim_source" facts.block_dim_source
          generated.block_dim_source);
      (fun () ->
        check_string row_id "grid_dim_source" facts.grid_dim_source
          generated.grid_dim_source);
      (fun () ->
        check_source_branch row_id facts.source_branch generated.source_branch);
      (fun () ->
        check_string row_id "dynamic_shared_memory" facts.dynamic_shared_memory
          generated.dynamic_shared_memory);
      (fun () ->
        check_string row_id "evidence_artifact_key" facts.evidence_artifact_key
          generated.evidence_artifact_key);
      (fun () ->
        check_int row_id "timeout_ms" facts.timeout_ms generated.timeout_ms);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_selected_manifest_facts (selected : selected_row)
    (facts : selected_manifest_facts) =
  let row_id = selected.selected_row_id in
  let checks =
    [
      (fun () ->
        if String.equal facts.selected_fact_row_id row_id then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "row_id";
                 expected = row_id;
                 actual = facts.selected_fact_row_id;
               }));
      (fun () ->
        check_selected_family row_id facts.selected_family_candidates
          selected.selected_family);
      (fun () ->
        check_string row_id "manifest_kernel"
          facts.selected_fact_manifest_kernel selected.selected_manifest_kernel);
      (fun () ->
        check_string row_id "source_file" facts.selected_fact_source_file
          selected.selected_source_file);
      (fun () ->
        check_string row_id "source_kernel_family"
          facts.selected_fact_source_kernel_family
          selected.selected_source_kernel_family);
      (fun () ->
        check_string row_id "parsed_kernel" facts.selected_fact_parsed_kernel
          selected.selected_parsed_kernel);
      (fun () ->
        check_string row_id "template_arg" facts.selected_fact_template_arg
          selected.selected_template_arg);
      (fun () ->
        check_string row_id "block_dim_source"
          facts.selected_fact_block_dim_source
          selected.selected_block_dim_source);
      (fun () ->
        check_string row_id "grid_dim_source"
          facts.selected_fact_grid_dim_source selected.selected_grid_dim_source);
      (fun () ->
        check_string_list row_id "source_branch"
          facts.selected_fact_source_branch_conditions
          selected.selected_source_branch_conditions);
      (fun () ->
        check_string row_id "dynamic_shared_memory"
          facts.selected_fact_dynamic_shared_memory
          selected.selected_dynamic_shared_memory);
      (fun () ->
        check_string row_id "feature_class" facts.selected_fact_feature_class
          selected.selected_feature_class);
      (fun () ->
        check_string_list row_id "required_semantics"
          facts.selected_fact_required_semantics
          selected.selected_required_semantics);
      (fun () ->
        check_string row_id "drf_status" facts.selected_fact_drf_status
          "not_attempted");
      (fun () ->
        check_string row_id "artifact_status"
          facts.selected_fact_artifact_status "none");
      (fun () ->
        check_string row_id "preprocessing_profile"
          facts.selected_fact_preprocessing_profile
          selected.selected_preprocessing_profile);
      (fun () ->
        check_string row_id "extraction_fixture"
          facts.selected_fact_extraction_fixture
          selected.selected_extraction_fixture);
      (fun () ->
        check_string row_id "subgroup_helper"
          facts.selected_fact_subgroup_helper selected.selected_subgroup_helper);
      (fun () ->
        check_int row_id "subgroup_size" facts.selected_fact_subgroup_size
          selected.selected_subgroup_size);
      (fun () ->
        check_int_list row_id "concrete_block_dim"
          facts.selected_fact_concrete_block_dim
          selected.selected_concrete_block_dim);
      (fun () ->
        check_string row_id "evidence_artifact_key"
          facts.selected_fact_evidence_artifact_key
          selected.selected_evidence_artifact_key);
      (fun () ->
        check_int row_id "timeout_ms" facts.selected_fact_timeout_ms
          selected.selected_timeout_ms);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_solve_tri_symbolic_k_guard (guard : solve_tri_symbolic_k_guard)
    (facts : solve_tri_symbolic_k_guard_facts) =
  let row_id = "solve_tri_f32_fast<N,K>" in
  let candidate_row_specs =
    List.map solve_tri_symbolic_row_spec guard.symbolic_guard_candidate_rows
  in
  let checks =
    [
      (fun () ->
        check_selected_family row_id facts.symbolic_fact_family_candidates
          guard.symbolic_guard_family);
      (fun () ->
        check_string row_id "source_file" facts.symbolic_fact_source_file
          guard.symbolic_guard_source_file);
      (fun () ->
        check_string row_id "source_kernel_family"
          facts.symbolic_fact_source_kernel_family
          guard.symbolic_guard_source_kernel_family);
      (fun () ->
        check_int row_id "n_template" facts.symbolic_fact_n_template
          guard.symbolic_guard_n_template);
      (fun () ->
        check_string row_id "k_parameter" facts.symbolic_fact_k_parameter
          guard.symbolic_guard_k_parameter);
      (fun () ->
        check_string row_id "k_source" facts.symbolic_fact_k_source
          guard.symbolic_guard_k_source);
      (fun () ->
        check_string_list row_id "candidate_rows"
          facts.symbolic_fact_candidate_rows candidate_row_specs);
      (fun () ->
        check_string_list row_id "lookup_anchor_rows"
          facts.symbolic_fact_lookup_anchor_row_ids
          guard.symbolic_guard_lookup_anchor_row_ids);
      (fun () ->
        check_string_list row_id "unpromoted_rows"
          facts.symbolic_fact_unpromoted_row_ids
          guard.symbolic_guard_unpromoted_row_ids);
      (fun () ->
        check_string_list row_id "excluded_rows"
          facts.symbolic_fact_excluded_row_ids
          guard.symbolic_guard_excluded_row_ids);
      (fun () ->
        check_string row_id "block_dim_relation"
          facts.symbolic_fact_block_dim_relation
          guard.symbolic_guard_block_dim_relation);
      (fun () ->
        check_string row_id "dynamic_shared_memory"
          facts.symbolic_fact_dynamic_shared_memory
          guard.symbolic_guard_dynamic_shared_memory);
      (fun () ->
        check_string row_id "subgroup_helper"
          facts.symbolic_fact_subgroup_helper
          guard.symbolic_guard_subgroup_helper);
      (fun () ->
        check_int row_id "subgroup_size" facts.symbolic_fact_subgroup_size
          guard.symbolic_guard_subgroup_size);
      (fun () ->
        check_string row_id "route_owner" facts.symbolic_fact_route_owner
          guard.symbolic_guard_route_owner);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_solve_tri_symbolic_dimension_carrier
    (carrier : solve_tri_symbolic_dimension_carrier)
    (facts : solve_tri_symbolic_dimension_carrier_facts) =
  let row_id = "solve_tri_f32_fast<N,K>" in
  let candidate_row_specs =
    List.map solve_tri_symbolic_row_spec carrier.carrier_candidate_rows
  in
  let template_dimensions =
    List.map symbolic_template_dimension_to_string
      carrier.carrier_template_dimensions
  in
  let k_candidate_values =
    carrier.carrier_block_dim.symbolic_dim_y
    |> symbolic_dimension_candidate_values
  in
  let checks =
    [
      (fun () ->
        check_selected_family row_id facts.carrier_fact_family_candidates
          carrier.carrier_family);
      (fun () ->
        check_string row_id "source_file" facts.carrier_fact_source_file
          carrier.carrier_source_file);
      (fun () ->
        check_string row_id "source_kernel_family"
          facts.carrier_fact_source_kernel_family
          carrier.carrier_source_kernel_family);
      (fun () ->
        check_string row_id "source_width_variable"
          facts.carrier_fact_source_width_variable
          carrier.carrier_source_width_variable);
      (fun () ->
        check_string row_id "source_width_relation"
          facts.carrier_fact_source_width_relation
          carrier.carrier_source_width_relation);
      (fun () ->
        check_string_list row_id "template_dimensions"
          facts.carrier_fact_template_dimensions template_dimensions);
      (fun () ->
        check_string row_id "symbolic_parameter"
          facts.carrier_fact_symbolic_parameter
          (symbolic_dimension_to_string carrier.carrier_block_dim.symbolic_dim_y));
      (fun () ->
        check_int_list row_id "symbolic_candidate_values"
          facts.carrier_fact_symbolic_candidate_values k_candidate_values);
      (fun () ->
        check_string row_id "block_dim" facts.carrier_fact_block_dim
          (symbolic_dim3_to_string carrier.carrier_block_dim));
      (fun () ->
        check_string row_id "grid_dim_source" facts.carrier_fact_grid_dim_source
          carrier.carrier_grid_dim_source);
      (fun () ->
        check_string row_id "dynamic_shared_memory"
          facts.carrier_fact_dynamic_shared_memory
          carrier.carrier_dynamic_shared_memory);
      (fun () ->
        check_string_list row_id "launch_branch_conditions"
          facts.carrier_fact_launch_branch_conditions
          carrier.carrier_launch_branch_conditions);
      (fun () ->
        check_string_list row_id "positive_shape_guards"
          facts.carrier_fact_positive_shape_guards
          carrier.carrier_positive_shape_guards);
      (fun () ->
        check_int row_id "subgroup_size" facts.carrier_fact_subgroup_size
          carrier.carrier_subgroup_size);
      (fun () ->
        check_string row_id "route_owner" facts.carrier_fact_route_owner
          carrier.carrier_route_owner);
      (fun () ->
        check_string_list row_id "candidate_rows"
          facts.carrier_fact_candidate_rows candidate_row_specs);
      (fun () ->
        check_string_list row_id "lookup_anchor_rows"
          facts.carrier_fact_lookup_anchor_row_ids
          carrier.carrier_lookup_anchor_row_ids);
      (fun () ->
        check_string_list row_id "unpromoted_rows"
          facts.carrier_fact_unpromoted_row_ids
          carrier.carrier_unpromoted_row_ids);
      (fun () ->
        check_string_list row_id "excluded_rows"
          facts.carrier_fact_excluded_row_ids carrier.carrier_excluded_row_ids);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_guarded_candidate_carrier (carrier : guarded_candidate_carrier)
    (facts : guarded_candidate_carrier_facts) =
  let row_id = carrier.candidate_carrier_id in
  let checks =
    [
      (fun () ->
        if String.equal facts.candidate_fact_carrier_id row_id then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = row_id;
                 actual = facts.candidate_fact_carrier_id;
               }));
      (fun () ->
        check_string row_id "priority_bucket"
          facts.candidate_fact_priority_bucket carrier.candidate_priority_bucket);
      (fun () ->
        check_string row_id "first_blocker" facts.candidate_fact_first_blocker
          carrier.candidate_first_blocker);
      (fun () ->
        check_string row_id "proof_ladder_stage"
          facts.candidate_fact_proof_ladder_stage
          carrier.candidate_proof_ladder_stage);
      (fun () ->
        check_string row_id "source_ledger" facts.candidate_fact_source_ledger
          carrier.candidate_source_ledger);
      (fun () ->
        check_int row_id "affected_family_count"
          facts.candidate_fact_affected_family_count
          carrier.candidate_affected_family_count);
      (fun () ->
        check_string_list row_id "required_fact_keys"
          facts.candidate_fact_required_fact_keys
          carrier.candidate_required_fact_keys);
      (fun () ->
        check_string row_id "route_owner" facts.candidate_fact_route_owner
          carrier.candidate_route_owner);
      (fun () ->
        check_string row_id "solver_policy" facts.candidate_fact_solver_policy
          carrier.candidate_solver_policy);
      (fun () ->
        check_string row_id "admission_status"
          facts.candidate_fact_admission_status
          carrier.candidate_admission_status);
      (fun () ->
        check_string row_id "next_support_step"
          facts.candidate_fact_next_support_step
          carrier.candidate_next_support_step);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_template_argument_resolution_carrier
    (carrier : template_argument_resolution_carrier)
    (facts : template_argument_resolution_facts) =
  let row_id = carrier.template_resolution_carrier_id in
  let checks =
    [
      (fun () ->
        if String.equal facts.template_resolution_fact_carrier_id row_id then
          Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = row_id;
                 actual = facts.template_resolution_fact_carrier_id;
               }));
      (fun () ->
        check_string row_id "source_ledger"
          facts.template_resolution_fact_source_ledger
          carrier.template_resolution_source_ledger);
      (fun () ->
        check_string row_id "input_blocker_ledger"
          facts.template_resolution_fact_input_blocker_ledger
          carrier.template_resolution_input_blocker_ledger);
      (fun () ->
        check_string row_id "proof_ladder_stage"
          facts.template_resolution_fact_proof_ladder_stage
          carrier.template_resolution_proof_ladder_stage);
      (fun () ->
        check_int row_id "families_attempted"
          facts.template_resolution_fact_families_attempted
          carrier.template_resolution_families_attempted);
      (fun () ->
        check_int row_id "rows_attempted"
          facts.template_resolution_fact_rows_attempted
          carrier.template_resolution_rows_attempted);
      (fun () ->
        check_int row_id "rows_known" facts.template_resolution_fact_rows_known
          carrier.template_resolution_rows_known);
      (fun () ->
        check_int row_id "rows_blocked"
          facts.template_resolution_fact_rows_blocked
          carrier.template_resolution_rows_blocked);
      (fun () ->
        check_int row_id "families_known"
          facts.template_resolution_fact_families_known
          carrier.template_resolution_families_known);
      (fun () ->
        check_int row_id "families_blocked"
          facts.template_resolution_fact_families_blocked
          carrier.template_resolution_families_blocked);
      (fun () ->
        check_string_int_pairs row_id "family_resolution_counts"
          facts.template_resolution_fact_family_resolution_counts
          carrier.template_resolution_family_resolution_counts);
      (fun () ->
        check_string_int_pairs row_id "row_resolution_counts"
          facts.template_resolution_fact_row_resolution_counts
          carrier.template_resolution_row_resolution_counts);
      (fun () ->
        check_string row_id "route_owner"
          facts.template_resolution_fact_route_owner
          carrier.template_resolution_route_owner);
      (fun () ->
        check_string row_id "solver_policy"
          facts.template_resolution_fact_solver_policy
          carrier.template_resolution_solver_policy);
      (fun () ->
        check_string row_id "admission_status"
          facts.template_resolution_fact_admission_status
          carrier.template_resolution_admission_status);
      (fun () ->
        check_string row_id "next_support_step"
          facts.template_resolution_fact_next_support_step
          carrier.template_resolution_next_support_step);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_launch_branch_frontier_carrier
    (carrier : launch_branch_frontier_carrier)
    (facts : launch_branch_frontier_facts) =
  let row_id = carrier.launch_branch_carrier_id in
  let checks =
    [
      (fun () ->
        if String.equal facts.launch_branch_fact_carrier_id row_id then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = row_id;
                 actual = facts.launch_branch_fact_carrier_id;
               }));
      (fun () ->
        check_string row_id "source_ledger"
          facts.launch_branch_fact_source_ledger
          carrier.launch_branch_source_ledger);
      (fun () ->
        check_string row_id "input_frontier_ledger"
          facts.launch_branch_fact_input_frontier_ledger
          carrier.launch_branch_input_frontier_ledger);
      (fun () ->
        check_string row_id "proof_ladder_stage"
          facts.launch_branch_fact_proof_ladder_stage
          carrier.launch_branch_proof_ladder_stage);
      (fun () ->
        check_int row_id "families_attempted"
          facts.launch_branch_fact_families_attempted
          carrier.launch_branch_families_attempted);
      (fun () ->
        check_int row_id "rows_attempted"
          facts.launch_branch_fact_rows_attempted
          carrier.launch_branch_rows_attempted);
      (fun () ->
        check_int row_id "rows_known" facts.launch_branch_fact_rows_known
          carrier.launch_branch_rows_known);
      (fun () ->
        check_int row_id "rows_blocked" facts.launch_branch_fact_rows_blocked
          carrier.launch_branch_rows_blocked);
      (fun () ->
        check_int row_id "families_known"
          facts.launch_branch_fact_families_known
          carrier.launch_branch_families_known);
      (fun () ->
        check_int row_id "families_blocked"
          facts.launch_branch_fact_families_blocked
          carrier.launch_branch_families_blocked);
      (fun () ->
        check_string_int_pairs row_id "row_resolution_counts"
          facts.launch_branch_fact_row_resolution_counts
          carrier.launch_branch_row_resolution_counts);
      (fun () ->
        check_string_int_pairs row_id "family_status_counts"
          facts.launch_branch_fact_family_status_counts
          carrier.launch_branch_family_status_counts);
      (fun () ->
        check_string_int_pairs row_id "next_blocker_counts"
          facts.launch_branch_fact_next_blocker_counts
          carrier.launch_branch_next_blocker_counts);
      (fun () ->
        check_string_int_pairs row_id "remaining_blocker_counts"
          facts.launch_branch_fact_remaining_blocker_counts
          carrier.launch_branch_remaining_blocker_counts);
      (fun () ->
        check_string row_id "route_owner" facts.launch_branch_fact_route_owner
          carrier.launch_branch_route_owner);
      (fun () ->
        check_string row_id "solver_policy"
          facts.launch_branch_fact_solver_policy
          carrier.launch_branch_solver_policy);
      (fun () ->
        check_string row_id "admission_status"
          facts.launch_branch_fact_admission_status
          carrier.launch_branch_admission_status);
      (fun () ->
        check_string row_id "next_support_step"
          facts.launch_branch_fact_next_support_step
          carrier.launch_branch_next_support_step);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_positive_shape_guard_carrier
    (carrier : positive_shape_guard_carrier)
    (facts : positive_shape_guard_facts) =
  let row_id = carrier.positive_shape_carrier_id in
  let family_specs =
    List.map positive_shape_family_spec
      carrier.positive_shape_candidate_families
  in
  let checks =
    [
      (fun () ->
        if String.equal facts.positive_shape_fact_carrier_id row_id then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = row_id;
                 actual = facts.positive_shape_fact_carrier_id;
               }));
      (fun () ->
        check_string row_id "source_ledger"
          facts.positive_shape_fact_source_ledger
          carrier.positive_shape_source_ledger);
      (fun () ->
        check_string row_id "input_frontier_ledger"
          facts.positive_shape_fact_input_frontier_ledger
          carrier.positive_shape_input_frontier_ledger);
      (fun () ->
        check_string row_id "proof_ladder_stage"
          facts.positive_shape_fact_proof_ladder_stage
          carrier.positive_shape_proof_ladder_stage);
      (fun () ->
        check_int row_id "families_attempted"
          facts.positive_shape_fact_families_attempted
          carrier.positive_shape_families_attempted);
      (fun () ->
        check_int row_id "rows_attempted"
          facts.positive_shape_fact_rows_attempted
          carrier.positive_shape_rows_attempted);
      (fun () ->
        check_int row_id "families_from_s477"
          facts.positive_shape_fact_families_from_s477
          carrier.positive_shape_families_from_s477);
      (fun () ->
        check_int row_id "rows_from_s477"
          facts.positive_shape_fact_rows_from_s477
          carrier.positive_shape_rows_from_s477);
      (fun () ->
        check_int row_id "preexisting_families"
          facts.positive_shape_fact_preexisting_families
          carrier.positive_shape_preexisting_families);
      (fun () ->
        check_int row_id "preexisting_rows"
          facts.positive_shape_fact_preexisting_rows
          carrier.positive_shape_preexisting_rows);
      (fun () ->
        check_string_list row_id "required_fact_keys"
          facts.positive_shape_fact_required_fact_keys
          carrier.positive_shape_required_fact_keys);
      (fun () ->
        check_string_list row_id "candidate_family_specs"
          facts.positive_shape_fact_candidate_family_specs family_specs);
      (fun () ->
        check_string_int_pairs row_id "next_blocker_counts"
          facts.positive_shape_fact_next_blocker_counts
          carrier.positive_shape_next_blocker_counts);
      (fun () ->
        check_string row_id "route_owner" facts.positive_shape_fact_route_owner
          carrier.positive_shape_route_owner);
      (fun () ->
        check_string row_id "solver_policy"
          facts.positive_shape_fact_solver_policy
          carrier.positive_shape_solver_policy);
      (fun () ->
        check_string row_id "admission_status"
          facts.positive_shape_fact_admission_status
          carrier.positive_shape_admission_status);
      (fun () ->
        check_string row_id "next_support_step"
          facts.positive_shape_fact_next_support_step
          carrier.positive_shape_next_support_step);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_positive_shape_verification_carrier
    (carrier : positive_shape_verification_carrier)
    (facts : positive_shape_verification_facts) =
  let row_id = carrier.positive_shape_verification_carrier_id in
  let family_specs =
    List.map positive_shape_family_spec
      carrier.positive_shape_verification_candidate_families
  in
  let checks =
    [
      (fun () ->
        if String.equal facts.positive_shape_verification_fact_carrier_id row_id
        then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = row_id;
                 actual = facts.positive_shape_verification_fact_carrier_id;
               }));
      (fun () ->
        check_string row_id "source_ledger"
          facts.positive_shape_verification_fact_source_ledger
          carrier.positive_shape_verification_source_ledger);
      (fun () ->
        check_string row_id "family_ledger"
          facts.positive_shape_verification_fact_family_ledger
          carrier.positive_shape_verification_family_ledger);
      (fun () ->
        check_string row_id "artifact_dir"
          facts.positive_shape_verification_fact_artifact_dir
          carrier.positive_shape_verification_artifact_dir);
      (fun () ->
        check_int row_id "families_attempted"
          facts.positive_shape_verification_fact_families_attempted
          carrier.positive_shape_verification_families_attempted);
      (fun () ->
        check_int row_id "rows_attempted"
          facts.positive_shape_verification_fact_rows_attempted
          carrier.positive_shape_verification_rows_attempted);
      (fun () ->
        check_int row_id "source_slice_verified_families"
          facts.positive_shape_verification_fact_source_slice_verified_families
          carrier.positive_shape_verification_source_slice_verified_families);
      (fun () ->
        check_int row_id "existing_guarded_families"
          facts.positive_shape_verification_fact_existing_guarded_families
          carrier.positive_shape_verification_existing_guarded_families);
      (fun () ->
        check_int row_id "blocked_families"
          facts.positive_shape_verification_fact_blocked_families
          carrier.positive_shape_verification_blocked_families);
      (fun () ->
        check_int row_id "source_slice_artifacts"
          facts.positive_shape_verification_fact_source_slice_artifacts
          carrier.positive_shape_verification_source_slice_artifacts);
      (fun () ->
        check_int row_id "exact_production_promotions"
          facts.positive_shape_verification_fact_exact_production_promotions
          carrier.positive_shape_verification_exact_production_promotions);
      (fun () ->
        check_bool row_id "manifest_verdict_fields_changed"
          facts.positive_shape_verification_fact_manifest_verdict_fields_changed
          carrier.positive_shape_verification_manifest_verdict_fields_changed);
      (fun () ->
        check_string_int_pairs row_id "family_status_counts"
          facts.positive_shape_verification_fact_family_status_counts
          carrier.positive_shape_verification_family_status_counts);
      (fun () ->
        check_string_int_pairs row_id "row_status_counts"
          facts.positive_shape_verification_fact_row_status_counts
          carrier.positive_shape_verification_row_status_counts);
      (fun () ->
        check_string_list row_id "candidate_family_specs"
          facts.positive_shape_verification_fact_candidate_family_specs
          family_specs);
      (fun () ->
        check_string row_id "evidence_policy"
          facts.positive_shape_verification_fact_evidence_policy
          carrier.positive_shape_verification_evidence_policy);
      (fun () ->
        check_string row_id "admission_status"
          facts.positive_shape_verification_fact_admission_status
          carrier.positive_shape_verification_admission_status);
      (fun () ->
        check_string row_id "next_support_step"
          facts.positive_shape_verification_fact_next_support_step
          carrier.positive_shape_verification_next_support_step);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_positive_shape_production_promotion_carrier
    (carrier : positive_shape_production_promotion_carrier)
    (facts : positive_shape_production_promotion_facts) =
  let row_id = carrier.positive_shape_production_carrier_id in
  let checks =
    [
      (fun () ->
        if String.equal facts.positive_shape_production_fact_carrier_id row_id
        then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = row_id;
                 actual = facts.positive_shape_production_fact_carrier_id;
               }));
      (fun () ->
        check_string row_id "source_ledger"
          facts.positive_shape_production_fact_source_ledger
          carrier.positive_shape_production_source_ledger);
      (fun () ->
        check_string row_id "input_verification_ledger"
          facts.positive_shape_production_fact_input_verification_ledger
          carrier.positive_shape_production_input_verification_ledger);
      (fun () ->
        check_string row_id "artifact_dir"
          facts.positive_shape_production_fact_artifact_dir
          carrier.positive_shape_production_artifact_dir);
      (fun () ->
        check_int row_id "families_attempted"
          facts.positive_shape_production_fact_families_attempted
          carrier.positive_shape_production_families_attempted);
      (fun () ->
        check_int row_id "rows_attempted"
          facts.positive_shape_production_fact_rows_attempted
          carrier.positive_shape_production_rows_attempted);
      (fun () ->
        check_int row_id "extraction_verified_families"
          facts.positive_shape_production_fact_extraction_verified_families
          carrier.positive_shape_production_extraction_verified_families);
      (fun () ->
        check_int row_id "profile_verified_families"
          facts.positive_shape_production_fact_profile_verified_families
          carrier.positive_shape_production_profile_verified_families);
      (fun () ->
        check_int row_id "source_slice_only_remaining"
          facts.positive_shape_production_fact_source_slice_only_remaining
          carrier.positive_shape_production_source_slice_only_remaining);
      (fun () ->
        check_int row_id "existing_guarded_families_preserved"
          facts
            .positive_shape_production_fact_existing_guarded_families_preserved
          carrier.positive_shape_production_existing_guarded_families_preserved);
      (fun () ->
        check_int row_id "exact_manifest_promotions"
          facts.positive_shape_production_fact_exact_manifest_promotions
          carrier.positive_shape_production_exact_manifest_promotions);
      (fun () ->
        check_bool row_id "manifest_verdict_fields_changed"
          facts.positive_shape_production_fact_manifest_verdict_fields_changed
          carrier.positive_shape_production_manifest_verdict_fields_changed);
      (fun () ->
        check_string_int_pairs row_id "status_counts"
          facts.positive_shape_production_fact_status_counts
          carrier.positive_shape_production_status_counts);
      (fun () ->
        check_string_list row_id "family_specs"
          facts.positive_shape_production_fact_family_specs
          carrier.positive_shape_production_family_specs);
      (fun () ->
        check_string row_id "evidence_policy"
          facts.positive_shape_production_fact_evidence_policy
          carrier.positive_shape_production_evidence_policy);
      (fun () ->
        check_string row_id "admission_status"
          facts.positive_shape_production_fact_admission_status
          carrier.positive_shape_production_admission_status);
      (fun () ->
        check_string row_id "next_support_step"
          facts.positive_shape_production_fact_next_support_step
          carrier.positive_shape_production_next_support_step);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let validate_guarded_candidate_row_facts (carrier : guarded_candidate_carrier)
    (facts : guarded_candidate_row_facts) =
  let row_id = facts.candidate_row_fact_row_id in
  let expected_family_key =
    Option.value facts.candidate_row_fact_source_file ~default:""
    ^ " :: "
    ^ Option.value facts.candidate_row_fact_kernel_or_template ~default:""
  in
  let checks =
    [
      (fun () ->
        if
          String.equal facts.candidate_row_fact_carrier_id
            carrier.candidate_carrier_id
        then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = carrier.candidate_carrier_id;
                 actual = facts.candidate_row_fact_carrier_id;
               }));
      (fun () ->
        check_present_string row_id "source_file"
          facts.candidate_row_fact_source_file);
      (fun () ->
        check_present_string row_id "kernel_or_template"
          facts.candidate_row_fact_kernel_or_template);
      (fun () ->
        check_string row_id "family_key" facts.candidate_row_fact_family_key
          expected_family_key);
      (fun () ->
        check_string row_id "first_blocker"
          facts.candidate_row_fact_first_blocker carrier.candidate_first_blocker);
      (fun () ->
        check_required_fact_statuses row_id carrier.candidate_required_fact_keys
          facts.candidate_row_fact_required_fact_statuses);
      (fun () -> check_candidate_readiness row_id facts);
      (fun () ->
        check_string row_id "solver_policy"
          facts.candidate_row_fact_solver_policy carrier.candidate_solver_policy);
      (fun () ->
        check_string row_id "admission_status"
          facts.candidate_row_fact_admission_status
          carrier.candidate_admission_status);
      (fun () ->
        check_int row_id "fresh_obligation_artifacts"
          facts.candidate_row_fact_fresh_obligation_artifacts 0);
      (fun () ->
        check_int row_id "solver_runs" facts.candidate_row_fact_solver_runs 0);
      (fun () ->
        check_int row_id "pre_solver_runs"
          facts.candidate_row_fact_pre_solver_runs 0);
      (fun () ->
        check_int row_id "new_guarded_family_admissions"
          facts.candidate_row_fact_new_guarded_family_admissions 0);
      (fun () ->
        check_bool row_id "manifest_verdict_fields_changed"
          facts.candidate_row_fact_manifest_verdict_fields_changed false);
      (fun () ->
        check_bool row_id "lookup_rows_added"
          facts.candidate_row_fact_lookup_rows_added false);
      (fun () ->
        check_bool row_id "shortcut_keying_used"
          facts.candidate_row_fact_shortcut_keying_used false);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks

let generate_row (family : family_seed) (row : row_seed) =
  let contract = contract_of_seed family.family row in
  {
    contract;
    source_branch = row.source_branch;
    row_shape_preconditions = row_shape_preconditions contract;
    source_file = family.source_file;
    preprocessing_profile = family.preprocessing_profile;
    extraction_fixture = family.extraction_fixture;
    block_dim_source = family.block_dim_source;
    grid_dim_source = family.grid_dim_source;
    dynamic_shared_memory = family.dynamic_shared_memory;
    feature_class = family.feature_class;
    required_semantics = family.required_semantics;
    evidence_artifact_key = row.evidence_artifact_key;
    timeout_ms = row.timeout_ms;
    shape_contract = standard_row_shape_contract contract;
  }

let generate_family family = List.map (generate_row family) family.rows
let all = List.concat_map generate_family family_seeds
let contracts = List.map (fun row -> row.contract) all
let catalog_launch_contract_rows = List.map launch_contract_row_of_generated all

let selected_launch_contract_rows =
  List.map launch_contract_row_of_selected selected_rows

let finite_type_launch_contract_rows =
  List.map launch_contract_row_of_finite_type finite_type_rows

let lookup_launch_contract_rows =
  catalog_launch_contract_rows @ selected_launch_contract_rows
  @ finite_type_launch_contract_rows
  @ im2col_symbolic_block_launch_contract_rows

let of_row_id row_id =
  match
    List.filter (fun row -> String.equal row.contract.row_id row_id) all
  with
  | [ row ] -> Ok row
  | [] -> Error (Unknown_generated_row row_id)
  | _ -> Error (Duplicate_generated_row row_id)

let selected_of_row_id row_id =
  match
    List.filter
      (fun row -> String.equal row.selected_row_id row_id)
      selected_rows
  with
  | [ row ] -> Ok row
  | [] -> Error (Unknown_generated_row row_id)
  | _ -> Error (Duplicate_generated_row row_id)

let finite_type_of_row_id row_id =
  match
    List.filter
      (fun row -> String.equal row.finite_type_row_id row_id)
      finite_type_rows
  with
  | [ row ] -> Ok row
  | [] -> Error (Unknown_generated_row row_id)
  | _ -> Error (Duplicate_generated_row row_id)

let sorted_strings values = List.sort String.compare values

let exact_policy_exact_row_ids =
  lookup_launch_contract_rows
  |> List.map (fun row -> row.launch_row_id)
  |> sorted_strings

let exact_policy_guarded_family_ids = [ "solve_tri_f32_fast<N,K>" ]

let exact_evidence_manifest_promotion_policy_carrier =
  {
    exact_policy_carrier_id = "s487_exact_evidence_manifest_promotion_policy";
    exact_policy_input_ledgers =
      [
        "agent_results/rewrite/component_summaries/S480/positive_shape_production_promotion_ledger.json";
        "agent_results/rewrite/component_summaries/S481/production_profile_ledger.json";
        "agent_results/rewrite/component_summaries/S482/exact_launch_contract_ledger.json";
        "agent_results/rewrite/component_summaries/S483/exact_launch_contract_ledger.json";
        "agent_results/rewrite/component_summaries/S485/exact_launch_contract_ledger.json";
        "agent_results/rewrite/component_summaries/S486A/exact_launch_contract_ledger.json";
        "agent_results/rewrite/component_summaries/S486B/exact_launch_contract_ledger.json";
        "agent_results/rewrite/component_summaries/S486C/exact_launch_contract_ledger.json";
      ];
    exact_policy_admissible_evidence_classes =
      [
        "exact_launch_contract_command";
        "structured_drf_json_zero_unknowns_errors";
        "row_owned_launch_profile_source_facts";
        "explicit_soundness_boundary";
      ];
    exact_policy_non_promoting_evidence_classes =
      [
        "source_slice_only_drf";
        "production_profile_without_executable_launch_contract";
        "row_or_family_similarity";
        "stale_or_missing_artifact";
        "numeric_equivalence_claim_without_proof";
      ];
    exact_policy_required_row_fact_keys =
      [
        "manifest_row";
        "parsed_kernel";
        "source_file";
        "launch_contract_row";
        "command_artifact";
        "stdout_json_artifact";
        "status_artifact";
        "faial_status";
        "unknown_count";
        "error_count";
        "soundness_boundary";
      ];
    exact_policy_row_local_manifest_status = "drf_exact_row";
    exact_policy_guarded_family_manifest_status =
      "guarded_symbolic_family_proof";
    exact_policy_source_slice_only_status = "source_slice_drf_not_promotable";
    exact_policy_profile_only_status = "production_profile_drf_not_promotable";
    exact_policy_exact_row_ids;
    exact_policy_exact_row_count = List.length exact_policy_exact_row_ids;
    exact_policy_guarded_family_ids;
    exact_policy_guarded_family_count =
      List.length exact_policy_guarded_family_ids;
    exact_policy_manifest_status_counts =
      [
        ("drf_exact_row", List.length exact_policy_exact_row_ids);
        ( "guarded_symbolic_family_proof",
          List.length exact_policy_guarded_family_ids );
        ("source_slice_drf_not_promotable", 0);
        ("production_profile_drf_not_promotable", 0);
      ];
    exact_policy_blocked_boundary_counts =
      [
        ("host_template_or_dependent_local", 21);
        ("indirect_launch_helper", 1);
        ("unsupported_boundary", 8);
      ];
    exact_policy_manifest_verdict_fields_changed = false;
    exact_policy_soundness_boundary =
      "row-local DRF only: exact launch-contract promotion records race \
       freedom for the checked row shape, not numeric kernel equivalence, full \
       host translation-unit parsing, or whole-family coverage";
    exact_policy_admission_status =
      "policy_defined_no_manifest_verdict_mutation";
    exact_policy_next_support_step =
      "apply the policy to manifest/coverage accounting in a separate step, \
       keeping drf_exact_row separate from guarded_symbolic_family_proof";
  }

let exact_evidence_manifest_promotion_policy_carrier_lines
    (carrier : exact_evidence_manifest_promotion_policy_carrier) =
  [
    "carrier_id: " ^ carrier.exact_policy_carrier_id;
    "input_ledgers: " ^ String.concat ", " carrier.exact_policy_input_ledgers;
    "admissible_evidence_classes: "
    ^ String.concat ", " carrier.exact_policy_admissible_evidence_classes;
    "non_promoting_evidence_classes: "
    ^ String.concat ", " carrier.exact_policy_non_promoting_evidence_classes;
    "required_row_fact_keys: "
    ^ String.concat ", " carrier.exact_policy_required_row_fact_keys;
    "row_local_manifest_status: "
    ^ carrier.exact_policy_row_local_manifest_status;
    "guarded_family_manifest_status: "
    ^ carrier.exact_policy_guarded_family_manifest_status;
    "source_slice_only_status: " ^ carrier.exact_policy_source_slice_only_status;
    "profile_only_status: " ^ carrier.exact_policy_profile_only_status;
    "exact_row_count: " ^ string_of_int carrier.exact_policy_exact_row_count;
    "exact_row_ids: " ^ String.concat ", " carrier.exact_policy_exact_row_ids;
    "guarded_family_count: "
    ^ string_of_int carrier.exact_policy_guarded_family_count;
    "guarded_family_ids: "
    ^ String.concat ", " carrier.exact_policy_guarded_family_ids;
    "manifest_status_counts: "
    ^ string_int_pairs_to_string carrier.exact_policy_manifest_status_counts;
    "blocked_boundary_counts: "
    ^ string_int_pairs_to_string carrier.exact_policy_blocked_boundary_counts;
    "manifest_verdict_fields_changed: "
    ^ string_of_bool carrier.exact_policy_manifest_verdict_fields_changed;
    "soundness_boundary: " ^ carrier.exact_policy_soundness_boundary;
    "admission_status: " ^ carrier.exact_policy_admission_status;
    "next_support_step: " ^ carrier.exact_policy_next_support_step;
  ]

let validate_exact_evidence_manifest_promotion_policy_carrier
    (carrier : exact_evidence_manifest_promotion_policy_carrier)
    (facts : exact_evidence_manifest_promotion_policy_facts) =
  let row_id = carrier.exact_policy_carrier_id in
  let checks =
    [
      (fun () ->
        if String.equal facts.exact_policy_fact_carrier_id row_id then Ok ()
        else
          Error
            (Field_mismatch
               {
                 row_id;
                 field = "carrier_id";
                 expected = row_id;
                 actual = facts.exact_policy_fact_carrier_id;
               }));
      (fun () ->
        check_string_list row_id "input_ledgers"
          facts.exact_policy_fact_input_ledgers
          carrier.exact_policy_input_ledgers);
      (fun () ->
        check_string_list row_id "admissible_evidence_classes"
          facts.exact_policy_fact_admissible_evidence_classes
          carrier.exact_policy_admissible_evidence_classes);
      (fun () ->
        check_string_list row_id "non_promoting_evidence_classes"
          facts.exact_policy_fact_non_promoting_evidence_classes
          carrier.exact_policy_non_promoting_evidence_classes);
      (fun () ->
        check_string_list row_id "required_row_fact_keys"
          facts.exact_policy_fact_required_row_fact_keys
          carrier.exact_policy_required_row_fact_keys);
      (fun () ->
        check_string row_id "row_local_manifest_status"
          facts.exact_policy_fact_row_local_manifest_status
          carrier.exact_policy_row_local_manifest_status);
      (fun () ->
        check_string row_id "guarded_family_manifest_status"
          facts.exact_policy_fact_guarded_family_manifest_status
          carrier.exact_policy_guarded_family_manifest_status);
      (fun () ->
        check_string row_id "source_slice_only_status"
          facts.exact_policy_fact_source_slice_only_status
          carrier.exact_policy_source_slice_only_status);
      (fun () ->
        check_string row_id "profile_only_status"
          facts.exact_policy_fact_profile_only_status
          carrier.exact_policy_profile_only_status);
      (fun () ->
        check_string_list row_id "exact_row_ids"
          facts.exact_policy_fact_exact_row_ids
          carrier.exact_policy_exact_row_ids);
      (fun () ->
        check_int row_id "exact_row_count"
          facts.exact_policy_fact_exact_row_count
          carrier.exact_policy_exact_row_count);
      (fun () ->
        check_string_list row_id "guarded_family_ids"
          facts.exact_policy_fact_guarded_family_ids
          carrier.exact_policy_guarded_family_ids);
      (fun () ->
        check_int row_id "guarded_family_count"
          facts.exact_policy_fact_guarded_family_count
          carrier.exact_policy_guarded_family_count);
      (fun () ->
        check_string_int_pairs row_id "manifest_status_counts"
          facts.exact_policy_fact_manifest_status_counts
          carrier.exact_policy_manifest_status_counts);
      (fun () ->
        check_string_int_pairs row_id "blocked_boundary_counts"
          facts.exact_policy_fact_blocked_boundary_counts
          carrier.exact_policy_blocked_boundary_counts);
      (fun () ->
        check_bool row_id "manifest_verdict_fields_changed"
          facts.exact_policy_fact_manifest_verdict_fields_changed
          carrier.exact_policy_manifest_verdict_fields_changed);
      (fun () ->
        check_string row_id "soundness_boundary"
          facts.exact_policy_fact_soundness_boundary
          carrier.exact_policy_soundness_boundary);
      (fun () ->
        check_string row_id "admission_status"
          facts.exact_policy_fact_admission_status
          carrier.exact_policy_admission_status);
      (fun () ->
        check_string row_id "next_support_step"
          facts.exact_policy_fact_next_support_step
          carrier.exact_policy_next_support_step);
    ]
  in
  let rec run = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> run rest | Error _ as error -> error)
  in
  run checks
