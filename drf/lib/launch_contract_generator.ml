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

let shared_memory_required_semantics =
  [
    "ordinary_memory_effects";
    "shared_memory_address_space";
    "workgroup_barrier_phase_splitting";
    "dynamic_shared_memory_accounting_when_nonzero";
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
