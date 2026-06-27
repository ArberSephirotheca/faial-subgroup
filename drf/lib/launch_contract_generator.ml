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
  solve_tri_unpromoted_row_ids : string list;
  solve_tri_excluded_row_ids : string list;
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
