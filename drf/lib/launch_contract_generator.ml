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

let selected_family_to_string = function Solve_tri_fast -> "solve_tri_fast"

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
  [ (Solve_tri_fast, "solve_tri_f32_fast<") ]
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

let string_of_string_list values = String.concat ", " values

let string_of_int_list values =
  values |> List.map string_of_int |> String.concat ", "

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
  }

let generate_family family = List.map (generate_row family) family.rows
let all = List.concat_map generate_family family_seeds
let contracts = List.map (fun row -> row.contract) all

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
