open Drf
open Protocols
open Exp
module Json = Yojson.Basic

let failf fmt = Printf.ksprintf (fun msg -> Alcotest.fail msg) fmt
let nvar name = Var (Variable.from_name name)

let has_conjunct expected actual =
  List.exists (( = ) expected) (b_and_split actual)

let check_conjunct name expected actual =
  Alcotest.(check bool) name true (has_conjunct expected actual)

let rec find_repo_root dir =
  let candidate =
    Filename.concat dir "agent_results/rewrite/cuda_launch_manifest.json"
  in
  if Sys.file_exists candidate then dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then
      failf "could not find repo root from %s" (Sys.getcwd ())
    else find_repo_root parent

let repo_root () = find_repo_root (Sys.getcwd ())

let repo_path root rel =
  let path =
    match String.split_on_char '#' rel with path :: _ -> path | [] -> rel
  in
  if Filename.is_relative path then Filename.concat root path else path

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let object_fields = function
  | `Assoc fields -> fields
  | json -> failf "expected JSON object, got %s" (Json.to_string json)

let field name json =
  match List.assoc_opt name (object_fields json) with
  | Some value -> value
  | None -> failf "missing JSON field %s in %s" name (Json.to_string json)

let field_opt name json = List.assoc_opt name (object_fields json)

let string_field name json =
  match field name json with
  | `String value -> value
  | value ->
      failf "field %s must be a string, got %s" name (Json.to_string value)

let int_field name json =
  match field name json with
  | `Int value -> value
  | value -> failf "field %s must be an int, got %s" name (Json.to_string value)

let nullable_int_field name json =
  match field name json with
  | `Null -> None
  | `Int value -> Some value
  | value ->
      failf "field %s must be null or int, got %s" name (Json.to_string value)

let list_field name json =
  match field name json with
  | `List values -> values
  | value -> failf "field %s must be a list, got %s" name (Json.to_string value)

let string_list_field name json =
  list_field name json
  |> List.map (function
    | `String value -> value
    | value ->
        failf "field %s must contain strings, got %s" name
          (Json.to_string value))

let null_field name json =
  match field name json with
  | `Null -> ()
  | value -> failf "field %s must be null, got %s" name (Json.to_string value)

let assert_existing_repo_file root label rel =
  let path = repo_path root rel in
  Alcotest.(check bool) (label ^ " exists: " ^ rel) true (Sys.file_exists path)

let manifest_path root =
  Filename.concat root "agent_results/rewrite/cuda_launch_manifest.json"

let manifest_summary_path root =
  Filename.concat root "agent_results/rewrite/cuda_launch_manifest_summary.json"

let load_manifest root = Json.from_file (manifest_path root)
let load_manifest_summary root = Json.from_file (manifest_summary_path root)
let manifest_rows manifest = list_field "rows" manifest
let sorted strings = List.sort String.compare strings

let string_contains haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec matches_at offset needle_offset =
    if needle_offset = needle_len then true
    else
      offset + needle_offset < haystack_len
      && Char.equal haystack.[offset + needle_offset] needle.[needle_offset]
      && matches_at offset (needle_offset + 1)
  in
  let rec search offset =
    if needle_len = 0 then true
    else if offset + needle_len > haystack_len then false
    else matches_at offset 0 || search (offset + 1)
  in
  search 0

let index_of_context_line label needle lines =
  let rec loop index = function
    | line :: _ when string_contains line needle -> index
    | _ :: rest -> loop (index + 1) rest
    | [] -> failf "missing %s containing %S" label needle
  in
  loop 0 lines

let duplicate_values values =
  let rec loop duplicates = function
    | first :: (second :: _ as rest) when String.equal first second ->
        loop (first :: duplicates) rest
    | _ :: rest -> loop duplicates rest
    | [] -> sorted duplicates
  in
  values |> sorted |> loop []

let row_id row = string_field "row_id" row

let find_row rows expected_id =
  match List.filter (fun row -> String.equal (row_id row) expected_id) rows with
  | [ row ] -> row
  | [] -> failf "missing manifest row %s" expected_id
  | _ -> failf "duplicate manifest row %s" expected_id

let count_by ?(include_null = false) ?(predicate = fun _ -> true) field_name
    rows =
  let counts = Hashtbl.create 16 in
  let increment key =
    let current =
      match Hashtbl.find_opt counts key with Some v -> v | None -> 0
    in
    Hashtbl.replace counts key (current + 1)
  in
  List.iter
    (fun row ->
      if predicate row then
        match field field_name row with
        | `String value -> increment value
        | `Null when include_null -> increment "none"
        | `Null -> ()
        | value ->
            failf "field %s must be string or null, got %s" field_name
              (Json.to_string value))
    rows;
  counts |> Hashtbl.to_seq |> List.of_seq |> List.sort compare

let int_assoc json =
  json |> object_fields
  |> List.map (function
    | key, `Int value -> (key, value)
    | key, value ->
        failf "count field %s must be an int, got %s" key (Json.to_string value))
  |> List.sort compare

let check_count_assoc label expected actual =
  Alcotest.(check (list (pair string int))) label expected actual

let check_count_field ?include_null ?predicate rows manifest summary name =
  let expected = count_by ?include_null ?predicate name rows in
  let manifest_counts = field "counts" manifest |> field ("by_" ^ name) in
  check_count_assoc ("manifest count " ^ name) expected
    (int_assoc manifest_counts);
  check_count_assoc ("summary count " ^ name) expected
    (int_assoc (field ("by_" ^ name) summary))

let expected_manifest_template_arg (contract : Launch_contract.t) =
  contract.template_arg

let expected_artifact_key row_id =
  match row_id with
  | "L072" -> "g504r_launch_contract"
  | "L073" -> "h501_launch_contract"
  | "L143" -> "h502_launch_contract"
  | "L144" -> "h506_launch_contract"
  | "L145" -> "h507_launch_contract"
  | "L146" -> "h508_launch_contract"
  | _ -> failf "no expected launch-contract artifact key for %s" row_id

let expected_timeout row_id =
  match row_id with
  | "L072" -> 1000
  | "L073" | "L143" | "L144" | "L145" | "L146" -> 10000
  | "L117" | "L118" -> 10000
  | _ -> failf "no expected launch-contract timeout for %s" row_id

let expected_wkv_rows =
  [
    ("L143", "h502_launch_contract");
    ("L144", "h506_launch_contract");
    ("L145", "h507_launch_contract");
    ("L146", "h508_launch_contract");
  ]

let expected_wkv_row_ids = List.map fst expected_wkv_rows

let launch_contract_artifact_owners =
  [
    ("g504r_launch_contract", "L072");
    ("h501_launch_contract", "L073");
    ("s404_launch_contract", "L117");
    ("s409_launch_contract", "L118");
    ("h502_launch_contract", "L143");
    ("h506_launch_contract", "L144");
    ("h507_launch_contract", "L145");
    ("h508_launch_contract", "L146");
  ]

let summary_launch_contract summary =
  match field_opt "launch_contract" summary with
  | Some (`Assoc _ as value) -> Some value
  | Some value ->
      failf "launch_contract summary field must be an object, got %s"
        (Json.to_string value)
  | None -> None

let summary_manifest_kernel summary =
  match field_opt "manifest_kernel" summary with
  | Some (`String value) -> value
  | Some value ->
      failf "manifest_kernel must be a string, got %s" (Json.to_string value)
  | None -> (
      match summary_launch_contract summary with
      | Some contract -> string_field "manifest_kernel" contract
      | None -> failf "summary is missing manifest_kernel")

let summary_parsed_kernel summary =
  match field_opt "parsed_kernel" summary with
  | Some (`String value) -> value
  | Some value ->
      failf "parsed_kernel must be a string, got %s" (Json.to_string value)
  | None -> (
      match summary_launch_contract summary with
      | Some contract -> string_field "parsed_kernel" contract
      | None -> failf "summary is missing parsed_kernel")

let summary_kernel_status summary =
  match field_opt "kernel_status" summary with
  | Some (`Assoc _ as kernel) -> kernel
  | Some value ->
      failf "kernel_status must be an object, got %s" (Json.to_string value)
  | None -> (
      match list_field "kernels" summary with
      | [ kernel ] -> kernel
      | kernels ->
          failf "expected one kernel summary, got %d" (List.length kernels))

let check_stdout_gate root contract stdout_rel =
  let stdout_json = Json.from_file (repo_path root stdout_rel) in
  let kernel =
    match list_field "kernels" stdout_json with
    | [ kernel ] -> kernel
    | kernels ->
        failf "expected one stdout kernel, got %d" (List.length kernels)
  in
  Alcotest.(check string)
    "stdout parsed kernel" contract.Launch_contract.parsed_kernel
    (string_field "kernel_name" kernel);
  Alcotest.(check string) "stdout status" "drf" (string_field "status" kernel);
  Alcotest.(check int)
    "stdout unknowns" 0
    (List.length (list_field "unknowns" kernel));
  Alcotest.(check int)
    "stdout errors" 0
    (List.length (list_field "errors" kernel))

let check_subgroup_stdout_gate root contract stdout_rel =
  let stdout_json = Json.from_file (repo_path root stdout_rel) in
  let kernel =
    match list_field "kernels" stdout_json with
    | [ kernel ] -> kernel
    | kernels ->
        failf "expected one subgroup stdout kernel, got %d"
          (List.length kernels)
  in
  Alcotest.(check string)
    "subgroup stdout parsed kernel" contract.Launch_contract.parsed_kernel
    (string_field "kernel_name" kernel);
  Alcotest.(check string)
    "subgroup stdout status" "drf"
    (string_field "status" kernel);
  Alcotest.(check string)
    "subgroup stdout mem_drf" "drf"
    (string_field "mem_drf" kernel);
  Alcotest.(check string)
    "subgroup stdout uniformity" "drf"
    (string_field "subgroup_uniformity" kernel);
  Alcotest.(check string)
    "subgroup stdout full verdict" "drf"
    (string_field "drf_full" kernel);
  let checks = field "memory_checks" kernel in
  Alcotest.(check int) "subgroup memory checks" 2 (int_field "total" checks);
  List.iter
    (fun name ->
      Alcotest.(check int)
        ("subgroup " ^ name ^ " checks")
        0 (int_field name checks))
    [ "racy"; "unknown"; "timeout"; "unsupported" ]

let check_summary_gate root contract artifact =
  let summary_rel = string_field "summary" artifact in
  let summary = Json.from_file (repo_path root summary_rel) in
  Alcotest.(check string)
    "summary row id" contract.Launch_contract.row_id
    (string_field "row_id" summary);
  Alcotest.(check string)
    "summary manifest kernel" contract.manifest_kernel
    (summary_manifest_kernel summary);
  Alcotest.(check string)
    "summary parsed kernel" contract.parsed_kernel
    (summary_parsed_kernel summary);
  Alcotest.(check int)
    "summary timeout"
    (expected_timeout contract.row_id)
    (int_field "timeout_ms" summary);
  Alcotest.(check string)
    "summary command path"
    (string_field "command" artifact)
    (string_field "command" summary);
  Alcotest.(check string)
    "summary stdout path"
    (string_field "stdout" artifact)
    (string_field "stdout" summary);
  Alcotest.(check string)
    "summary stderr path"
    (string_field "stderr" artifact)
    (string_field "stderr" summary);
  Alcotest.(check string)
    "summary status path"
    (string_field "status" artifact)
    (string_field "status_file" summary);
  let kernel = summary_kernel_status summary in
  Alcotest.(check string)
    "summary kernel status name" contract.parsed_kernel
    (string_field "kernel_name" kernel);
  Alcotest.(check string)
    "summary kernel status" "drf"
    (string_field "status" kernel);
  Alcotest.(check int)
    "summary unknown count" 0
    (int_field "unknown_count" kernel);
  Alcotest.(check int) "summary error count" 0 (int_field "error_count" kernel)

let check_subgroup_summary_gate root contract artifact =
  check_summary_gate root contract artifact;
  let summary_rel = string_field "summary" artifact in
  let summary = Json.from_file (repo_path root summary_rel) in
  let kernel = summary_kernel_status summary in
  Alcotest.(check string)
    "summary mem_drf" "drf"
    (string_field "mem_drf" kernel);
  Alcotest.(check string)
    "summary subgroup uniformity" "drf"
    (string_field "subgroup_uniformity" kernel);
  Alcotest.(check string)
    "summary full verdict" "drf"
    (string_field "drf_full" kernel);
  let checks = field "memory_checks" kernel in
  Alcotest.(check int) "summary memory checks" 2 (int_field "total" checks);
  List.iter
    (fun name ->
      Alcotest.(check int)
        ("summary " ^ name ^ " checks")
        0 (int_field name checks))
    [ "racy"; "unknown"; "timeout"; "unsupported" ]

let check_command_metadata manifest contract artifact =
  let attempted =
    manifest |> field "command_metadata" |> list_field "attempted_analyzer_runs"
  in
  let matches =
    List.filter
      (fun entry ->
        String.equal
          (string_field "row_id" entry)
          contract.Launch_contract.row_id)
      attempted
  in
  let entry =
    match matches with
    | [ entry ] -> entry
    | [] -> failf "missing command metadata for %s" contract.row_id
    | _ -> failf "duplicate command metadata for %s" contract.row_id
  in
  Alcotest.(check int)
    "command metadata exit status" 0
    (int_field "exit_status" entry);
  Alcotest.(check string)
    "command metadata artifact"
    (string_field "summary" artifact)
    (string_field "artifact" entry)

let manual_contract_row row_id =
  match
    List.filter
      (fun row -> String.equal row.Launch_contract_rows.row_id row_id)
      Launch_contract_rows.all
  with
  | [ row ] -> row
  | [] -> failf "missing manual launch-contract row %s" row_id
  | _ -> failf "duplicate manual launch-contract row %s" row_id

let check_generated_contract_matches_manual generated =
  let generated_contract = generated.Launch_contract_generator.contract in
  let manual = manual_contract_row generated_contract.row_id in
  Alcotest.(check bool)
    (generated_contract.row_id ^ " family")
    true
    (manual.family = generated_contract.family);
  Alcotest.(check string)
    (generated_contract.row_id ^ " manifest kernel")
    manual.manifest_kernel generated_contract.manifest_kernel;
  Alcotest.(check string)
    (generated_contract.row_id ^ " parsed kernel")
    manual.parsed_kernel generated_contract.parsed_kernel;
  Alcotest.(check string)
    (generated_contract.row_id ^ " template arg")
    manual.template_arg generated_contract.template_arg;
  Alcotest.(check string)
    (generated_contract.row_id ^ " template param")
    manual.template_param generated_contract.template_param;
  Alcotest.(check int)
    (generated_contract.row_id ^ " template value")
    manual.template_value generated_contract.template_value

let expected_generated_shape_preconditions (contract : Launch_contract_rows.t) =
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

let manifest_branch_condition row =
  string_field "block_dim_source" row
  ^ " == "
  ^ (row |> field "concrete_template_args" |> string_field "value")

let guarded_if_row_id_for_else = function
  | "L073" -> "L072"
  | "L144" -> "L143"
  | "L146" -> "L145"
  | row_id -> failf "no guarded-if manifest row for else row %s" row_id

let check_generated_source_branch_matches_manifest rows row generated =
  let generated_contract = generated.Launch_contract_generator.contract in
  let launch_site = field "launch_site" row in
  let context_lines = string_list_field "context_excerpt" launch_site in
  let line_text = string_field "line_text" launch_site in
  let launch_index =
    index_of_context_line
      (generated_contract.row_id ^ " launch line")
      line_text context_lines
  in
  let else_index =
    index_of_context_line
      (generated_contract.row_id ^ " else branch")
      "} else {" context_lines
  in
  Alcotest.(check bool)
    (generated_contract.row_id ^ " launch line has manifest kernel")
    true
    (string_contains line_text generated_contract.manifest_kernel);
  match generated.source_branch with
  | Launch_contract_generator.Guarded_if_branch { condition } ->
      let expected_condition = manifest_branch_condition row in
      Alcotest.(check string)
        (generated_contract.row_id ^ " guarded if condition")
        expected_condition condition;
      let guard_index =
        index_of_context_line
          (generated_contract.row_id ^ " guarded if")
          ("if (" ^ condition ^ ")")
          context_lines
      in
      Alcotest.(check bool)
        (generated_contract.row_id ^ " guarded if precedes launch")
        true
        (guard_index < launch_index);
      Alcotest.(check bool)
        (generated_contract.row_id ^ " launch precedes else")
        true
        (launch_index < else_index)
  | Launch_contract_generator.Else_branch_after { if_condition } ->
      let guard_row =
        find_row rows (guarded_if_row_id_for_else generated_contract.row_id)
      in
      Alcotest.(check string)
        (generated_contract.row_id ^ " else guard condition")
        (manifest_branch_condition guard_row)
        if_condition;
      Alcotest.(check bool)
        (generated_contract.row_id ^ " else precedes launch")
        true
        (else_index < launch_index);
      Alcotest.(check bool)
        (generated_contract.row_id
       ^ " source branch guard is not a row-shape precondition")
        false
        (List.mem if_condition generated.row_shape_preconditions)

let source_branch_from_manifest rows row generated =
  let generated_contract = generated.Launch_contract_generator.contract in
  match generated.Launch_contract_generator.source_branch with
  | Launch_contract_generator.Guarded_if_branch _ ->
      Launch_contract_generator.Guarded_if_branch
        { condition = manifest_branch_condition row }
  | Launch_contract_generator.Else_branch_after _ ->
      let guard_row =
        find_row rows (guarded_if_row_id_for_else generated_contract.row_id)
      in
      Launch_contract_generator.Else_branch_after
        { if_condition = manifest_branch_condition guard_row }

let generated_manifest_facts root rows row generated =
  let generated_contract = generated.Launch_contract_generator.contract in
  let manifest_kernel = string_field "kernel_or_template" row in
  let artifact =
    row |> field "evidence_artifact" |> field generated.evidence_artifact_key
  in
  let summary =
    Json.from_file (repo_path root (string_field "summary" artifact))
  in
  {
    Launch_contract_generator.row_id = row_id row;
    family_candidates =
      Launch_contract_generator.family_candidates_of_manifest_kernel
        manifest_kernel;
    manifest_kernel = Some manifest_kernel;
    parsed_kernel = Some (summary_parsed_kernel summary);
    template_arg =
      Some (row |> field "concrete_template_args" |> string_field "value");
    template_value = Some generated_contract.template_value;
    block_dim_source = Some (string_field "block_dim_source" row);
    grid_dim_source = Some (string_field "grid_dim_source" row);
    source_branch = Some (source_branch_from_manifest rows row generated);
    dynamic_shared_memory = Some (string_field "dynamic_shared_memory" row);
    evidence_artifact_key =
      (if
         Option.is_some
           (row |> field "evidence_artifact"
           |> field_opt generated.evidence_artifact_key)
       then Some generated.evidence_artifact_key
       else None);
    timeout_ms = Some (int_field "timeout_ms" artifact);
  }

let check_generated_validation_guard root rows row generated =
  let facts = generated_manifest_facts root rows row generated in
  match Launch_contract_generator.validate_manifest_facts generated facts with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error)

let check_generated_shape_preconditions_match_manifest row generated =
  let generated_contract = generated.Launch_contract_generator.contract in
  let exact_block_precondition =
    string_field "block_dim_source" row
    ^ " == "
    ^ string_of_int generated_contract.template_value
  in
  Alcotest.(check (list string))
    (generated_contract.row_id ^ " row-shape preconditions")
    (expected_generated_shape_preconditions generated_contract)
    generated.row_shape_preconditions;
  Alcotest.(check bool)
    (generated_contract.row_id ^ " shape uses manifest block source")
    true
    (List.mem exact_block_precondition generated.row_shape_preconditions)

let check_generated_row_matches_manifest root rows generated =
  let generated_contract = generated.Launch_contract_generator.contract in
  let row = find_row rows generated_contract.row_id in
  Alcotest.(check string)
    (generated_contract.row_id ^ " source file")
    generated.source_file
    (string_field "source_file" row);
  Alcotest.(check string)
    (generated_contract.row_id ^ " manifest kernel")
    generated_contract.manifest_kernel
    (string_field "kernel_or_template" row);
  Alcotest.(check string)
    (generated_contract.row_id ^ " template arg")
    generated_contract.template_arg
    (row |> field "concrete_template_args" |> string_field "value");
  Alcotest.(check string)
    (generated_contract.row_id ^ " feature class")
    generated.feature_class
    (string_field "feature_class" row);
  Alcotest.(check string)
    (generated_contract.row_id ^ " block dim source")
    generated.block_dim_source
    (string_field "block_dim_source" row);
  Alcotest.(check string)
    (generated_contract.row_id ^ " grid dim source")
    generated.grid_dim_source
    (string_field "grid_dim_source" row);
  Alcotest.(check string)
    (generated_contract.row_id ^ " dynamic shared memory")
    generated.dynamic_shared_memory
    (string_field "dynamic_shared_memory" row);
  Alcotest.(check (list string))
    (generated_contract.row_id ^ " required semantics")
    generated.required_semantics
    (string_list_field "required_semantics" row);
  Alcotest.(check string)
    (generated_contract.row_id ^ " preprocessing profile")
    generated.preprocessing_profile
    (string_field "preprocessing_profile" row);
  Alcotest.(check string)
    (generated_contract.row_id ^ " extraction fixture")
    generated.extraction_fixture
    (string_field "extraction_fixture" row);
  check_generated_source_branch_matches_manifest rows row generated;
  check_generated_shape_preconditions_match_manifest row generated;
  let artifact =
    row |> field "evidence_artifact" |> field generated.evidence_artifact_key
  in
  check_generated_validation_guard root rows row generated;
  Alcotest.(check int)
    (generated_contract.row_id ^ " artifact timeout")
    generated.timeout_ms
    (int_field "timeout_ms" artifact);
  assert_existing_repo_file root
    (generated_contract.row_id ^ " generated preprocessing profile")
    generated.preprocessing_profile;
  assert_existing_repo_file root
    (generated_contract.row_id ^ " generated extraction fixture")
    generated.extraction_fixture

let check_launch_contract_manifest_row root manifest rows row_id =
  let contract =
    Launch_contract.of_row_id row_id |> function
    | Ok contract -> contract
    | Error error -> Alcotest.fail (Launch_contract.error_to_string error)
  in
  let row = find_row rows row_id in
  Alcotest.(check string)
    "manifest kernel" contract.manifest_kernel
    (string_field "kernel_or_template" row);
  Alcotest.(check string)
    "feature class" "shared_memory_syncthreads"
    (string_field "feature_class" row);
  Alcotest.(check string)
    "block dim source" "C / H"
    (string_field "block_dim_source" row);
  Alcotest.(check string)
    "grid dim source" "B * H"
    (string_field "grid_dim_source" row);
  Alcotest.(check string)
    "dynamic shared memory" "0"
    (string_field "dynamic_shared_memory" row);
  Alcotest.(check string)
    "template arg"
    (expected_manifest_template_arg contract)
    (row |> field "concrete_template_args" |> string_field "value");
  Alcotest.(check string)
    "frontend status" "memory_event_obligations_generated"
    (string_field "frontend_status" row);
  Alcotest.(check string)
    "artifact status" "ordinary_launch_contract_drf_json_verdict"
    (string_field "artifact_status" row);
  Alcotest.(check string)
    "DRF status" "verified"
    (string_field "drf_status" row);
  Alcotest.(check (option int))
    "timeout"
    (Some (expected_timeout row_id))
    (nullable_int_field "timeout_ms" row);
  null_field "subgroup_size_if_any" row;
  null_field "not_attempted_reason" row;
  null_field "unsupported_reason" row;
  assert_existing_repo_file root "preprocessing profile"
    (string_field "preprocessing_profile" row);
  assert_existing_repo_file root "extraction fixture"
    (string_field "extraction_fixture" row);
  let artifact =
    row |> field "evidence_artifact" |> field (expected_artifact_key row_id)
  in
  Alcotest.(check int)
    "artifact timeout" (expected_timeout row_id)
    (int_field "timeout_ms" artifact);
  Alcotest.(check string)
    "nested artifact status" "ordinary_launch_contract_drf_json_verdict"
    (string_field "artifact_status" artifact);
  List.iter
    (fun key ->
      assert_existing_repo_file root ("artifact " ^ key)
        (string_field key artifact))
    [ "command"; "stdout"; "stderr"; "status"; "summary" ];
  Alcotest.(check string)
    "status artifact" "0"
    (String.trim (read_file (repo_path root (string_field "status" artifact))));
  check_stdout_gate root contract (string_field "stdout" artifact);
  check_summary_gate root contract artifact;
  check_command_metadata manifest contract artifact

let test_manifest_counts_are_row_derived () =
  let root = repo_root () in
  let manifest = load_manifest root in
  let summary = load_manifest_summary root in
  let rows = manifest_rows manifest in
  Alcotest.(check int)
    "manifest rows_total" (List.length rows)
    (manifest |> field "counts" |> int_field "rows_total");
  Alcotest.(check int)
    "summary rows_total" (List.length rows)
    (int_field "rows_total" summary);
  check_count_field rows manifest summary "drf_status";
  check_count_field rows manifest summary "feature_class";
  check_count_field rows manifest summary "frontend_status";
  check_count_field rows manifest summary "g501_classification";
  check_count_field rows manifest summary "artifact_status";
  check_count_field rows manifest summary "source_feature_hint";
  check_count_field rows manifest summary "not_attempted_reason";
  check_count_field ~include_null:true rows manifest summary
    "unsupported_reason";
  check_count_assoc "manifest concrete feature count"
    (count_by "feature_class" rows ~predicate:(fun row ->
         String.equal (string_field "g501_classification" row) "concrete_launch"))
    (manifest |> field "counts"
    |> field "concrete_rows_by_feature_class"
    |> int_assoc);
  check_count_assoc "summary concrete feature count"
    (count_by "feature_class" rows ~predicate:(fun row ->
         String.equal (string_field "g501_classification" row) "concrete_launch"))
    (summary |> field "concrete_rows_by_feature_class" |> int_assoc);
  check_count_assoc "manifest unresolved hint count"
    (count_by "source_feature_hint" rows ~predicate:(fun row ->
         String.equal
           (string_field "g501_classification" row)
           "unresolved_template_family"))
    (manifest |> field "counts"
    |> field "unresolved_rows_by_source_feature_hint"
    |> int_assoc);
  check_count_assoc "summary unresolved hint count"
    (count_by "source_feature_hint" rows ~predicate:(fun row ->
         String.equal
           (string_field "g501_classification" row)
           "unresolved_template_family"))
    (summary |> field "unresolved_rows_by_source_feature_hint" |> int_assoc)

let test_launch_contract_rows_match_manifest () =
  let root = repo_root () in
  let manifest = load_manifest root in
  let rows = manifest_rows manifest in
  Alcotest.(check (list string))
    "manifest duplicate row ids" []
    (duplicate_values (List.map row_id rows));
  let contract_ids =
    Launch_contract.catalog_rows
    |> List.map (fun row -> row.Launch_contract_rows.row_id)
    |> sorted
  in
  Alcotest.(check (list string))
    "catalog duplicate row ids" []
    (duplicate_values contract_ids);
  let manifest_contract_ids =
    rows
    |> List.filter (fun row ->
        String.equal
          (string_field "artifact_status" row)
          "ordinary_launch_contract_drf_json_verdict")
    |> List.map row_id |> sorted
  in
  Alcotest.(check (list string))
    "ordinary launch-contract manifest rows" contract_ids manifest_contract_ids;
  List.iter (check_launch_contract_manifest_row root manifest rows) contract_ids

let test_generated_rows_match_manual_catalog_and_manifest () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let generated_ids =
    Launch_contract_generator.all
    |> List.map (fun row -> row.Launch_contract_generator.contract.row_id)
    |> sorted
  in
  let manual_ids =
    Launch_contract_rows.all
    |> List.map (fun row -> row.Launch_contract_rows.row_id)
    |> sorted
  in
  Alcotest.(check (list string))
    "generated rows match manual catalog" manual_ids generated_ids;
  List.iter check_generated_contract_matches_manual
    Launch_contract_generator.all;
  List.iter
    (check_generated_row_matches_manifest root rows)
    Launch_contract_generator.all

let generated_row row_id =
  match Launch_contract_generator.of_row_id row_id with
  | Ok row -> row
  | Error error ->
      Alcotest.fail (Launch_contract_generator.error_to_string error)

let valid_manifest_facts_for_generated root rows generated =
  let row = find_row rows generated.Launch_contract_generator.contract.row_id in
  generated_manifest_facts root rows row generated

let selected_generated_row row_id =
  match Launch_contract_generator.selected_of_row_id row_id with
  | Ok row -> row
  | Error error ->
      Alcotest.fail (Launch_contract_generator.error_to_string error)

let solve_tri_seed_row_ids rows =
  rows
  |> List.map (fun row -> row.Launch_contract_generator.solve_tri_row_id)
  |> sorted

let selected_generated_manifest_facts row selected =
  let manifest_kernel = string_field "kernel_or_template" row in
  {
    Launch_contract_generator.selected_fact_row_id = row_id row;
    selected_family_candidates =
      Launch_contract_generator.selected_family_candidates_of_manifest_kernel
        manifest_kernel;
    selected_fact_manifest_kernel = Some manifest_kernel;
    selected_fact_source_file = Some (string_field "source_file" row);
    selected_fact_source_kernel_family =
      Some selected.Launch_contract_generator.selected_source_kernel_family;
    selected_fact_parsed_kernel =
      Some selected.Launch_contract_generator.selected_parsed_kernel;
    selected_fact_template_arg =
      Some (row |> field "concrete_template_args" |> string_field "value");
    selected_fact_block_dim_source = Some (string_field "block_dim_source" row);
    selected_fact_grid_dim_source = Some (string_field "grid_dim_source" row);
    selected_fact_source_branch_conditions =
      Some selected.Launch_contract_generator.selected_source_branch_conditions;
    selected_fact_dynamic_shared_memory =
      Some (string_field "dynamic_shared_memory" row);
    selected_fact_feature_class = Some (string_field "feature_class" row);
    selected_fact_required_semantics =
      Some (string_list_field "required_semantics" row);
    selected_fact_drf_status = Some (string_field "drf_status" row);
    selected_fact_artifact_status = Some (string_field "artifact_status" row);
    selected_fact_preprocessing_profile =
      Some selected.Launch_contract_generator.selected_preprocessing_profile;
    selected_fact_extraction_fixture =
      Some selected.Launch_contract_generator.selected_extraction_fixture;
    selected_fact_subgroup_helper =
      Some selected.Launch_contract_generator.selected_subgroup_helper;
    selected_fact_subgroup_size =
      Some selected.Launch_contract_generator.selected_subgroup_size;
    selected_fact_concrete_block_dim =
      Some selected.Launch_contract_generator.selected_concrete_block_dim;
    selected_fact_evidence_artifact_key =
      Some selected.Launch_contract_generator.selected_evidence_artifact_key;
    selected_fact_timeout_ms =
      Some selected.Launch_contract_generator.selected_timeout_ms;
  }

let selected_pre_promotion_manifest_facts row selected =
  let facts = selected_generated_manifest_facts row selected in
  {
    facts with
    selected_fact_required_semantics =
      Some selected.Launch_contract_generator.selected_required_semantics;
    selected_fact_drf_status = Some "not_attempted";
    selected_fact_artifact_status = Some "none";
    selected_fact_preprocessing_profile =
      Some selected.Launch_contract_generator.selected_preprocessing_profile;
    selected_fact_extraction_fixture =
      Some selected.Launch_contract_generator.selected_extraction_fixture;
    selected_fact_timeout_ms =
      Some selected.Launch_contract_generator.selected_timeout_ms;
  }

let symbolic_k_guard_facts guard =
  {
    Launch_contract_generator.symbolic_fact_family_candidates =
      [ Launch_contract_generator.Solve_tri_fast ];
    symbolic_fact_source_file =
      Some guard.Launch_contract_generator.symbolic_guard_source_file;
    symbolic_fact_source_kernel_family =
      Some guard.Launch_contract_generator.symbolic_guard_source_kernel_family;
    symbolic_fact_n_template =
      Some guard.Launch_contract_generator.symbolic_guard_n_template;
    symbolic_fact_k_parameter =
      Some guard.Launch_contract_generator.symbolic_guard_k_parameter;
    symbolic_fact_k_source =
      Some guard.Launch_contract_generator.symbolic_guard_k_source;
    symbolic_fact_candidate_rows =
      Some
        (List.map Launch_contract_generator.solve_tri_symbolic_row_spec
           guard.Launch_contract_generator.symbolic_guard_candidate_rows);
    symbolic_fact_lookup_anchor_row_ids =
      Some guard.Launch_contract_generator.symbolic_guard_lookup_anchor_row_ids;
    symbolic_fact_unpromoted_row_ids =
      Some guard.Launch_contract_generator.symbolic_guard_unpromoted_row_ids;
    symbolic_fact_excluded_row_ids =
      Some guard.Launch_contract_generator.symbolic_guard_excluded_row_ids;
    symbolic_fact_block_dim_relation =
      Some guard.Launch_contract_generator.symbolic_guard_block_dim_relation;
    symbolic_fact_dynamic_shared_memory =
      Some guard.Launch_contract_generator.symbolic_guard_dynamic_shared_memory;
    symbolic_fact_subgroup_helper =
      Some guard.Launch_contract_generator.symbolic_guard_subgroup_helper;
    symbolic_fact_subgroup_size =
      Some guard.Launch_contract_generator.symbolic_guard_subgroup_size;
    symbolic_fact_route_owner =
      Some guard.Launch_contract_generator.symbolic_guard_route_owner;
  }

let symbolic_dimension_carrier_facts carrier =
  {
    Launch_contract_generator.carrier_fact_family_candidates =
      [ Launch_contract_generator.Solve_tri_fast ];
    carrier_fact_source_file =
      Some carrier.Launch_contract_generator.carrier_source_file;
    carrier_fact_source_kernel_family =
      Some carrier.Launch_contract_generator.carrier_source_kernel_family;
    carrier_fact_source_width_variable =
      Some carrier.Launch_contract_generator.carrier_source_width_variable;
    carrier_fact_source_width_relation =
      Some carrier.Launch_contract_generator.carrier_source_width_relation;
    carrier_fact_template_dimensions =
      Some
        (List.map
           Launch_contract_generator.symbolic_template_dimension_to_string
           carrier.Launch_contract_generator.carrier_template_dimensions);
    carrier_fact_symbolic_parameter =
      Some
        (Launch_contract_generator.symbolic_dimension_to_string
           carrier.Launch_contract_generator.carrier_block_dim.symbolic_dim_y);
    carrier_fact_symbolic_candidate_values =
      Some
        (Launch_contract_generator.symbolic_dimension_candidate_values
           carrier.Launch_contract_generator.carrier_block_dim.symbolic_dim_y);
    carrier_fact_block_dim =
      Some
        (Launch_contract_generator.symbolic_dim3_to_string
           carrier.Launch_contract_generator.carrier_block_dim);
    carrier_fact_grid_dim_source =
      Some carrier.Launch_contract_generator.carrier_grid_dim_source;
    carrier_fact_dynamic_shared_memory =
      Some carrier.Launch_contract_generator.carrier_dynamic_shared_memory;
    carrier_fact_launch_branch_conditions =
      Some carrier.Launch_contract_generator.carrier_launch_branch_conditions;
    carrier_fact_positive_shape_guards =
      Some carrier.Launch_contract_generator.carrier_positive_shape_guards;
    carrier_fact_subgroup_size =
      Some carrier.Launch_contract_generator.carrier_subgroup_size;
    carrier_fact_route_owner =
      Some carrier.Launch_contract_generator.carrier_route_owner;
    carrier_fact_candidate_rows =
      Some
        (List.map Launch_contract_generator.solve_tri_symbolic_row_spec
           carrier.Launch_contract_generator.carrier_candidate_rows);
    carrier_fact_lookup_anchor_row_ids =
      Some carrier.Launch_contract_generator.carrier_lookup_anchor_row_ids;
    carrier_fact_unpromoted_row_ids =
      Some carrier.Launch_contract_generator.carrier_unpromoted_row_ids;
    carrier_fact_excluded_row_ids =
      Some carrier.Launch_contract_generator.carrier_excluded_row_ids;
  }

let guarded_candidate_carrier_facts carrier =
  {
    Launch_contract_generator.candidate_fact_carrier_id =
      carrier.Launch_contract_generator.candidate_carrier_id;
    candidate_fact_priority_bucket =
      Some carrier.Launch_contract_generator.candidate_priority_bucket;
    candidate_fact_first_blocker =
      Some carrier.Launch_contract_generator.candidate_first_blocker;
    candidate_fact_proof_ladder_stage =
      Some carrier.Launch_contract_generator.candidate_proof_ladder_stage;
    candidate_fact_source_ledger =
      Some carrier.Launch_contract_generator.candidate_source_ledger;
    candidate_fact_affected_family_count =
      Some carrier.Launch_contract_generator.candidate_affected_family_count;
    candidate_fact_required_fact_keys =
      Some carrier.Launch_contract_generator.candidate_required_fact_keys;
    candidate_fact_route_owner =
      Some carrier.Launch_contract_generator.candidate_route_owner;
    candidate_fact_solver_policy =
      Some carrier.Launch_contract_generator.candidate_solver_policy;
    candidate_fact_admission_status =
      Some carrier.Launch_contract_generator.candidate_admission_status;
    candidate_fact_next_support_step =
      Some carrier.Launch_contract_generator.candidate_next_support_step;
  }

let check_selected_validation_guard row selected =
  let facts = selected_pre_promotion_manifest_facts row selected in
  match
    Launch_contract_generator.validate_selected_manifest_facts selected facts
  with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error)

let test_l117_exact_production_promotion () =
  let root = repo_root () in
  let manifest = load_manifest root in
  let rows = manifest_rows manifest in
  let row = find_row rows "L117" in
  let selected = selected_generated_row "L117" in
  check_selected_validation_guard row selected;
  let contract =
    Launch_contract.of_row_id "L117" |> function
    | Ok contract -> contract
    | Error error -> Alcotest.fail (Launch_contract.error_to_string error)
  in
  Alcotest.(check string)
    "L117 manifest kernel" "solve_tri_f32_fast<64, 32>" contract.manifest_kernel;
  Alcotest.(check string)
    "L117 parsed kernel" "solve_tri_f32_fast_l117" contract.parsed_kernel;
  Alcotest.(check string) "L117 template arg" "64, 32" contract.template_arg;
  Alcotest.(check (list (pair string int)))
    "L117 template bindings"
    [ ("n_template", 64); ("k_template", 32) ]
    (Launch_contract.required_params contract);
  let block_dim = Launch_contract.block_dim contract in
  Alcotest.(check int) "L117 blockDim.x" 32 block_dim.x;
  Alcotest.(check int) "L117 blockDim.y" 32 block_dim.y;
  Alcotest.(check int) "L117 blockDim.z" 1 block_dim.z;
  let pre = Launch_contract.precondition contract in
  check_conjunct "L117 n template fact" (n_eq (nvar "n_template") (Num 64)) pre;
  check_conjunct "L117 k template fact" (n_eq (nvar "k_template") (Num 32)) pre;
  check_conjunct "L117 blockDim.x fact"
    (n_eq (Var Variable.bdim_x) (Num 32))
    pre;
  check_conjunct "L117 blockDim.y fact"
    (n_eq (Var Variable.bdim_y) (Num 32))
    pre;
  Alcotest.(check bool)
    "L117 is in production lookup rows" true
    (Launch_contract.lookup_rows
    |> List.exists (fun row -> String.equal row.Launch_contract.row_id "L117"));
  Alcotest.(check bool)
    "L117 is not an ordinary catalog row" false
    (Launch_contract.catalog_rows
    |> List.exists (fun row ->
        String.equal row.Launch_contract_rows.row_id "L117"));
  Alcotest.(check string)
    "canonical L117 DRF status" "verified"
    (string_field "drf_status" row);
  Alcotest.(check string)
    "canonical L117 artifact status" "subgroup_drf_json_verdict"
    (string_field "artifact_status" row);
  Alcotest.(check (option int))
    "canonical L117 timeout" (Some 10000)
    (nullable_int_field "timeout_ms" row);
  Alcotest.(check (option int))
    "canonical L117 subgroup size" (Some 32)
    (nullable_int_field "subgroup_size_if_any" row);
  null_field "not_attempted_reason" row;
  null_field "unsupported_reason" row;
  assert_existing_repo_file root "L117 preprocessing profile"
    (string_field "preprocessing_profile" row);
  assert_existing_repo_file root "L117 extraction fixture"
    (string_field "extraction_fixture" row);
  let artifact =
    row |> field "evidence_artifact" |> field "s404_launch_contract"
  in
  Alcotest.(check int)
    "L117 artifact timeout" 10000
    (int_field "timeout_ms" artifact);
  Alcotest.(check string)
    "L117 nested artifact status" "subgroup_drf_json_verdict"
    (string_field "artifact_status" artifact);
  List.iter
    (fun key ->
      assert_existing_repo_file root ("L117 artifact " ^ key)
        (string_field key artifact))
    [ "command"; "stdout"; "stderr"; "status"; "summary" ];
  Alcotest.(check string)
    "L117 status artifact" "0"
    (String.trim (read_file (repo_path root (string_field "status" artifact))));
  check_subgroup_stdout_gate root contract (string_field "stdout" artifact);
  check_subgroup_summary_gate root contract artifact;
  check_command_metadata manifest contract artifact

let test_l118_exact_production_promotion () =
  let root = repo_root () in
  let manifest = load_manifest root in
  let rows = manifest_rows manifest in
  let row = find_row rows "L118" in
  let selected = selected_generated_row "L118" in
  check_selected_validation_guard row selected;
  let contract =
    Launch_contract.of_row_id "L118" |> function
    | Ok contract -> contract
    | Error error -> Alcotest.fail (Launch_contract.error_to_string error)
  in
  Alcotest.(check string)
    "L118 manifest kernel" "solve_tri_f32_fast<64, 16>" contract.manifest_kernel;
  Alcotest.(check string)
    "L118 parsed kernel" "solve_tri_f32_fast_l118" contract.parsed_kernel;
  Alcotest.(check string) "L118 template arg" "64, 16" contract.template_arg;
  Alcotest.(check (list (pair string int)))
    "L118 template bindings"
    [ ("n_template", 64); ("k_template", 16) ]
    (Launch_contract.required_params contract);
  let block_dim = Launch_contract.block_dim contract in
  Alcotest.(check int) "L118 blockDim.x" 32 block_dim.x;
  Alcotest.(check int) "L118 blockDim.y" 16 block_dim.y;
  Alcotest.(check int) "L118 blockDim.z" 1 block_dim.z;
  Alcotest.(check bool)
    "L118 is in production lookup rows" true
    (Launch_contract.lookup_rows
    |> List.exists (fun row -> String.equal row.Launch_contract.row_id "L118"));
  Alcotest.(check bool)
    "L118 is not an ordinary catalog row" false
    (Launch_contract.catalog_rows
    |> List.exists (fun row ->
        String.equal row.Launch_contract_rows.row_id "L118"));
  Alcotest.(check string)
    "canonical L118 DRF status" "verified"
    (string_field "drf_status" row);
  Alcotest.(check string)
    "canonical L118 artifact status" "subgroup_drf_json_verdict"
    (string_field "artifact_status" row);
  Alcotest.(check (option int))
    "canonical L118 timeout" (Some 10000)
    (nullable_int_field "timeout_ms" row);
  Alcotest.(check (option int))
    "canonical L118 subgroup size" (Some 32)
    (nullable_int_field "subgroup_size_if_any" row);
  null_field "not_attempted_reason" row;
  null_field "unsupported_reason" row;
  assert_existing_repo_file root "L118 preprocessing profile"
    (string_field "preprocessing_profile" row);
  assert_existing_repo_file root "L118 extraction fixture"
    (string_field "extraction_fixture" row);
  let artifact =
    row |> field "evidence_artifact" |> field "s409_launch_contract"
  in
  Alcotest.(check int)
    "L118 artifact timeout" 10000
    (int_field "timeout_ms" artifact);
  Alcotest.(check string)
    "L118 nested artifact status" "subgroup_drf_json_verdict"
    (string_field "artifact_status" artifact);
  List.iter
    (fun key ->
      assert_existing_repo_file root ("L118 artifact " ^ key)
        (string_field key artifact))
    [ "command"; "stdout"; "stderr"; "status"; "summary" ];
  Alcotest.(check string)
    "L118 status artifact" "0"
    (String.trim (read_file (repo_path root (string_field "status" artifact))));
  check_subgroup_stdout_gate root contract (string_field "stdout" artifact);
  check_subgroup_summary_gate root contract artifact;
  check_command_metadata manifest contract artifact

let test_selected_l117_generated_facts_match_h516_evidence () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let row = find_row rows "L117" in
  let selected = selected_generated_row "L117" in
  Alcotest.(check (list string))
    "selected generated rows" [ "L117"; "L118" ]
    (Launch_contract_generator.selected_rows
    |> List.map (fun row -> row.Launch_contract_generator.selected_row_id)
    |> sorted);
  Alcotest.(check string)
    "selected source file"
    (string_field "source_file" row)
    selected.Launch_contract_generator.selected_source_file;
  Alcotest.(check string)
    "selected manifest kernel"
    (string_field "kernel_or_template" row)
    selected.Launch_contract_generator.selected_manifest_kernel;
  Alcotest.(check string)
    "selected template arg"
    (row |> field "concrete_template_args" |> string_field "value")
    selected.Launch_contract_generator.selected_template_arg;
  Alcotest.(check (list (pair string int)))
    "selected template bindings"
    [ ("n_template", 64); ("k_template", 32) ]
    selected.Launch_contract_generator.selected_template_bindings;
  Alcotest.(check string)
    "selected parsed source-slice kernel" "solve_tri_f32_fast_l117"
    selected.Launch_contract_generator.selected_parsed_kernel;
  Alcotest.(check (list string))
    "selected source branch conditions" [ "n == 64"; "case 32" ]
    selected.Launch_contract_generator.selected_source_branch_conditions;
  Alcotest.(check (list int))
    "selected concrete block dim" [ 32; 32; 1 ]
    selected.Launch_contract_generator.selected_concrete_block_dim;
  Alcotest.(check string)
    "selected subgroup helper" "warp_reduce_sum"
    selected.Launch_contract_generator.selected_subgroup_helper;
  Alcotest.(check int)
    "selected subgroup size" 32
    selected.Launch_contract_generator.selected_subgroup_size;
  Alcotest.(check int)
    "selected timeout" 1000
    selected.Launch_contract_generator.selected_timeout_ms;
  Alcotest.(check string)
    "canonical L117 DRF status" "verified"
    (string_field "drf_status" row);
  Alcotest.(check string)
    "canonical L117 artifact status" "subgroup_drf_json_verdict"
    (string_field "artifact_status" row);
  Alcotest.(check (option int))
    "canonical L117 timeout" (Some 10000)
    (nullable_int_field "timeout_ms" row);
  Alcotest.(check (option int))
    "canonical L117 subgroup size" (Some 32)
    (nullable_int_field "subgroup_size_if_any" row);
  assert_existing_repo_file root "canonical L117 preprocessing profile"
    (string_field "preprocessing_profile" row);
  assert_existing_repo_file root "canonical L117 extraction fixture"
    (string_field "extraction_fixture" row);
  assert_existing_repo_file root "H516 preprocessing profile"
    selected.Launch_contract_generator.selected_preprocessing_profile;
  assert_existing_repo_file root "H516 extraction fixture"
    selected.Launch_contract_generator.selected_extraction_fixture;
  let profile =
    read_file
      (repo_path root
         selected.Launch_contract_generator.selected_preprocessing_profile)
  in
  Alcotest.(check bool)
    "H516 profile records explicit subgroup config" true
    (string_contains profile "subgroup_size = 32");
  Alcotest.(check bool)
    "H516 profile records subgroup helper" true
    (string_contains profile "warp_reduce_sum");
  let source_excerpt =
    read_file
      (repo_path root
         "agent_results/rewrite/component_summaries/H516/solve_tri_l117_source_excerpt.txt")
  in
  Alcotest.(check bool)
    "H516 source excerpt records n guard" true
    (string_contains source_excerpt "if (n == 64)");
  Alcotest.(check bool)
    "H516 source excerpt records k case" true
    (string_contains source_excerpt "case 32:");
  check_selected_validation_guard row selected

let test_selected_l118_generated_facts_match_s406_schema () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let row = find_row rows "L118" in
  let selected = selected_generated_row "L118" in
  Alcotest.(check (list string))
    "selected generated rows" [ "L117"; "L118" ]
    (Launch_contract_generator.selected_rows
    |> List.map (fun row -> row.Launch_contract_generator.selected_row_id)
    |> sorted);
  Alcotest.(check string)
    "selected source file"
    (string_field "source_file" row)
    selected.Launch_contract_generator.selected_source_file;
  Alcotest.(check string)
    "selected manifest kernel"
    (string_field "kernel_or_template" row)
    selected.Launch_contract_generator.selected_manifest_kernel;
  Alcotest.(check string)
    "selected template arg"
    (row |> field "concrete_template_args" |> string_field "value")
    selected.Launch_contract_generator.selected_template_arg;
  Alcotest.(check (list (pair string int)))
    "selected template bindings"
    [ ("n_template", 64); ("k_template", 16) ]
    selected.Launch_contract_generator.selected_template_bindings;
  Alcotest.(check string)
    "selected parsed source-slice kernel" "solve_tri_f32_fast_l118"
    selected.Launch_contract_generator.selected_parsed_kernel;
  Alcotest.(check (list string))
    "selected source branch conditions" [ "n == 64"; "case 16" ]
    selected.Launch_contract_generator.selected_source_branch_conditions;
  Alcotest.(check (list int))
    "selected concrete block dim" [ 32; 16; 1 ]
    selected.Launch_contract_generator.selected_concrete_block_dim;
  Alcotest.(check string)
    "selected subgroup helper" "warp_reduce_sum"
    selected.Launch_contract_generator.selected_subgroup_helper;
  Alcotest.(check int)
    "selected subgroup size" 32
    selected.Launch_contract_generator.selected_subgroup_size;
  Alcotest.(check int)
    "selected timeout" 10000
    selected.Launch_contract_generator.selected_timeout_ms;
  Alcotest.(check string)
    "selected preprocessing profile"
    "agent_results/rewrite/component_summaries/S408/preprocessing_profile.md"
    selected.Launch_contract_generator.selected_preprocessing_profile;
  Alcotest.(check string)
    "selected extraction fixture"
    "agent_results/rewrite/component_summaries/S408/artifacts/L118_solve_tri_f32_fast_l118_source_slice.cu"
    selected.Launch_contract_generator.selected_extraction_fixture;
  Alcotest.(check string)
    "selected evidence key" "s408_source_intake"
    selected.Launch_contract_generator.selected_evidence_artifact_key;
  Alcotest.(check string)
    "canonical L118 artifact status" "subgroup_drf_json_verdict"
    (string_field "artifact_status" row);
  Alcotest.(check string)
    "canonical L118 DRF status" "verified"
    (string_field "drf_status" row);
  Alcotest.(check (option int))
    "canonical L118 timeout" (Some 10000)
    (nullable_int_field "timeout_ms" row);
  Alcotest.(check (option int))
    "canonical L118 subgroup size" (Some 32)
    (nullable_int_field "subgroup_size_if_any" row);
  assert_existing_repo_file root "canonical L118 preprocessing profile"
    (string_field "preprocessing_profile" row);
  assert_existing_repo_file root "canonical L118 extraction fixture"
    (string_field "extraction_fixture" row);
  check_selected_validation_guard row selected

let test_solve_tri_family_contract_manifest_boundaries () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let family = Launch_contract_generator.solve_tri_fast_family in
  Alcotest.(check string)
    "family source file" "llama.cpp/ggml/src/ggml-cuda/solve_tri.cu"
    family.Launch_contract_generator.solve_tri_source_file;
  Alcotest.(check string)
    "family kernel" "solve_tri_f32_fast"
    family.Launch_contract_generator.solve_tri_source_kernel_family;
  Alcotest.(check int)
    "family N template" 64 family.Launch_contract_generator.solve_tri_n_template;
  Alcotest.(check int)
    "family blockDim.x" 32
    family.Launch_contract_generator.solve_tri_block_dim_x;
  Alcotest.(check string)
    "family dynamic smem" "0"
    family.Launch_contract_generator.solve_tri_dynamic_shared_memory;
  Alcotest.(check int)
    "family subgroup size" 32
    family.Launch_contract_generator.solve_tri_subgroup_size;
  Alcotest.(check (list string))
    "family lookup rows" [ "L117"; "L118" ]
    (solve_tri_seed_row_ids
       family.Launch_contract_generator.solve_tri_lookup_rows);
  Alcotest.(check (list string))
    "symbolic K guard rows"
    [
      "L117:K=32:lookup_anchor";
      "L118:K=16:lookup_anchor";
      "L119:K=14:unpromoted_candidate";
      "L120:K=12:unpromoted_candidate";
      "L121:K=10:unpromoted_candidate";
      "L122:K=8:unpromoted_candidate";
      "L123:K=6:unpromoted_candidate";
      "L124:K=4:unpromoted_candidate";
      "L125:K=2:unpromoted_candidate";
      "L126:K=1:unpromoted_candidate";
    ]
    (List.map Launch_contract_generator.solve_tri_symbolic_row_spec
       family.Launch_contract_generator.solve_tri_symbolic_k_rows);
  Alcotest.(check (list string))
    "selected rows are family lookup rows" [ "L117"; "L118" ]
    (Launch_contract_generator.selected_rows
    |> List.map (fun row -> row.Launch_contract_generator.selected_row_id)
    |> sorted);
  List.iter
    (fun row_id ->
      let row = find_row rows row_id in
      Alcotest.(check string)
        (row_id ^ " remains verified row-local")
        "verified"
        (string_field "drf_status" row);
      Alcotest.(check string)
        (row_id ^ " remains subgroup artifact")
        "subgroup_drf_json_verdict"
        (string_field "artifact_status" row);
      Alcotest.(check (option int))
        (row_id ^ " explicit subgroup size")
        (Some 32)
        (nullable_int_field "subgroup_size_if_any" row))
    [ "L117"; "L118" ];
  List.iter
    (fun row_id ->
      let row = find_row rows row_id in
      Alcotest.(check string)
        (row_id ^ " remains unpromoted")
        "not_attempted"
        (string_field "drf_status" row);
      Alcotest.(check string)
        (row_id ^ " has no artifact")
        "none"
        (string_field "artifact_status" row);
      null_field "subgroup_size_if_any" row;
      match Launch_contract.of_row_id row_id with
      | Error (Launch_contract.Unknown_row actual) ->
          Alcotest.(check string) (row_id ^ " not in lookup") row_id actual
      | Ok _ -> failf "%s unexpectedly became a lookup row" row_id
      | Error error -> failf "%s" (Launch_contract.error_to_string error))
    (family.Launch_contract_generator.solve_tri_unpromoted_row_ids
   @ family.Launch_contract_generator.solve_tri_excluded_row_ids)

let test_solve_tri_symbolic_k_guard_consumption_and_blocker () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let guard = Launch_contract.solve_tri_symbolic_k_guard in
  let facts = symbolic_k_guard_facts guard in
  (match Launch_contract.validate_solve_tri_symbolic_k_guard guard facts with
  | Ok () -> ()
  | Error error ->
      failf "symbolic K guard rejected valid facts: %s"
        (Launch_contract_generator.validation_error_to_string error));
  Alcotest.(check string)
    "symbolic route owner" "Memory_event.Subgroup_obligation"
    guard.Launch_contract_generator.symbolic_guard_route_owner;
  Alcotest.(check string)
    "symbolic K parameter" "K"
    guard.Launch_contract_generator.symbolic_guard_k_parameter;
  Alcotest.(check string)
    "symbolic block relation" "blockDim = [32, K, 1]"
    guard.Launch_contract_generator.symbolic_guard_block_dim_relation;
  let carrier = Launch_contract.solve_tri_symbolic_dimension_carrier in
  let carrier_facts = symbolic_dimension_carrier_facts carrier in
  (match
     Launch_contract.validate_solve_tri_symbolic_dimension_carrier carrier
       carrier_facts
   with
  | Ok () -> ()
  | Error error ->
      failf "symbolic dimension carrier rejected valid facts: %s"
        (Launch_contract_generator.validation_error_to_string error));
  Alcotest.(check (list string))
    "carrier template dimensions"
    [ "n_template=64"; "k_template=K" ]
    (List.map Launch_contract_generator.symbolic_template_dimension_to_string
       carrier.Launch_contract_generator.carrier_template_dimensions);
  Alcotest.(check string)
    "carrier source width variable" "k"
    carrier.Launch_contract_generator.carrier_source_width_variable;
  Alcotest.(check string)
    "carrier source width relation" "k == K"
    carrier.Launch_contract_generator.carrier_source_width_relation;
  Alcotest.(check string)
    "carrier blockDim" "[32, K, 1]"
    (Launch_contract_generator.symbolic_dim3_to_string
       carrier.Launch_contract_generator.carrier_block_dim);
  Alcotest.(check (list int))
    "carrier symbolic K candidates"
    [ 32; 16; 14; 12; 10; 8; 6; 4; 2; 1 ]
    (Launch_contract_generator.symbolic_dimension_candidate_values
       carrier.Launch_contract_generator.carrier_block_dim.symbolic_dim_y);
  Alcotest.(check (list string))
    "carrier launch branches"
    [ "n == 64"; "case K in {32, 16, 14, 12, 10, 8, 6, 4, 2, 1}" ]
    carrier.Launch_contract_generator.carrier_launch_branch_conditions;
  Alcotest.(check bool)
    "carrier positive guard keeps K bounded" true
    (List.mem "K > 0"
       carrier.Launch_contract_generator.carrier_positive_shape_guards);
  Alcotest.(check bool)
    "carrier positive guard keeps blockDim symbolic" true
    (List.mem "blockDim.y == K"
       carrier.Launch_contract_generator.carrier_positive_shape_guards);
  List.iter
    (fun row_id ->
      let row = find_row rows row_id in
      Alcotest.(check string)
        (row_id ^ " symbolic neighbor remains unpromoted")
        "not_attempted"
        (string_field "drf_status" row);
      match Launch_contract.of_row_id row_id with
      | Error (Launch_contract.Unknown_row actual) ->
          Alcotest.(check string) (row_id ^ " absent from lookup") row_id actual
      | Ok _ -> failf "%s unexpectedly became a lookup row" row_id
      | Error error -> failf "%s" (Launch_contract.error_to_string error))
    guard.Launch_contract_generator.symbolic_guard_unpromoted_row_ids;
  List.iter
    (fun row_id ->
      let row = find_row rows row_id in
      Alcotest.(check string)
        (row_id ^ " excluded row remains unpromoted")
        "not_attempted"
        (string_field "drf_status" row))
    guard.Launch_contract_generator.symbolic_guard_excluded_row_ids;
  let dump =
    String.concat "\n"
      (Launch_contract.solve_tri_symbolic_k_obligation_blocker_lines ())
  in
  Alcotest.(check bool)
    "blocker dump contains K" true
    (string_contains dump "symbolic_parameter: K");
  Alcotest.(check bool)
    "blocker dump contains route owner" true
    (string_contains dump "Memory_event.Subgroup_obligation");
  Alcotest.(check bool)
    "blocker dump explains launch-contract provenance" true
    (string_contains dump "provenance rather than solver input")

let test_host_template_candidate_carrier_matches_s445_blocker () =
  let root = repo_root () in
  let carrier =
    Launch_contract.host_template_specialization_candidate_carrier
  in
  let facts = guarded_candidate_carrier_facts carrier in
  (match Launch_contract.validate_guarded_candidate_carrier carrier facts with
  | Ok () -> ()
  | Error error ->
      failf "candidate carrier rejected valid facts: %s"
        (Launch_contract_generator.validation_error_to_string error));
  let ledger =
    Json.from_file
      (repo_path root carrier.Launch_contract_generator.candidate_source_ledger)
  in
  let families = list_field "families" ledger in
  let matching =
    families
    |> List.filter (fun family ->
        String.equal
          (string_field "first_blocker" family)
          carrier.Launch_contract_generator.candidate_first_blocker)
  in
  Alcotest.(check int)
    "S445 host/template blocker count"
    carrier.Launch_contract_generator.candidate_affected_family_count
    (List.length matching);
  Alcotest.(check string)
    "carrier route owner" "Launch_contract_generator.guarded_candidate_carrier"
    carrier.Launch_contract_generator.candidate_route_owner;
  Alcotest.(check string)
    "carrier solver policy" "not_solver_input"
    carrier.Launch_contract_generator.candidate_solver_policy;
  Alcotest.(check string)
    "carrier admission status" "blocked_no_fresh_obligation"
    carrier.Launch_contract_generator.candidate_admission_status;
  match Launch_contract.of_row_id "L003" with
  | Error (Launch_contract.Unknown_row actual) ->
      Alcotest.(check string) "carrier does not admit L003" "L003" actual
  | Ok _ -> failf "S447 candidate carrier unexpectedly admitted L003"
  | Error error -> failf "%s" (Launch_contract.error_to_string error)

let check_symbolic_k_missing_validation_field label field guard facts =
  match Launch_contract.validate_solve_tri_symbolic_k_guard guard facts with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Missing_field { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let check_symbolic_k_field_mismatch label field guard facts =
  match Launch_contract.validate_solve_tri_symbolic_k_guard guard facts with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Field_mismatch { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let check_carrier_missing_validation_field label field carrier facts =
  match
    Launch_contract.validate_solve_tri_symbolic_dimension_carrier carrier facts
  with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Missing_field { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let check_carrier_field_mismatch label field carrier facts =
  match
    Launch_contract.validate_solve_tri_symbolic_dimension_carrier carrier facts
  with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Field_mismatch { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let check_candidate_missing_validation_field label field carrier facts =
  match Launch_contract.validate_guarded_candidate_carrier carrier facts with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Missing_field { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let check_candidate_field_mismatch label field carrier facts =
  match Launch_contract.validate_guarded_candidate_carrier carrier facts with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Field_mismatch { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let test_solve_tri_symbolic_k_guard_fails_closed_on_missing_facts () =
  let guard = Launch_contract.solve_tri_symbolic_k_guard in
  let facts = symbolic_k_guard_facts guard in
  check_symbolic_k_missing_validation_field "missing symbolic K parameter"
    "k_parameter" guard
    { facts with symbolic_fact_k_parameter = None };
  check_symbolic_k_missing_validation_field "missing symbolic K candidate rows"
    "candidate_rows" guard
    { facts with symbolic_fact_candidate_rows = None };
  check_symbolic_k_missing_validation_field "missing symbolic subgroup size"
    "subgroup_size" guard
    { facts with symbolic_fact_subgroup_size = None };
  check_symbolic_k_field_mismatch "mismatched symbolic N" "n_template" guard
    { facts with symbolic_fact_n_template = Some 128 };
  check_symbolic_k_field_mismatch "mismatched symbolic block relation"
    "block_dim_relation" guard
    {
      facts with
      symbolic_fact_block_dim_relation = Some "blockDim = [32, 16, 1]";
    };
  check_symbolic_k_field_mismatch "missing excluded sentinel row"
    "excluded_rows" guard
    { facts with symbolic_fact_excluded_row_ids = Some [ "L116"; "L127" ] };
  check_symbolic_k_field_mismatch "neighbor cannot become lookup anchor"
    "lookup_anchor_rows" guard
    {
      facts with
      symbolic_fact_lookup_anchor_row_ids = Some [ "L117"; "L118"; "L119" ];
    };
  check_symbolic_k_field_mismatch "route owner must remain subgroup obligation"
    "route_owner" guard
    { facts with symbolic_fact_route_owner = Some "task-local-report" }

let test_solve_tri_symbolic_dimension_carrier_fails_closed_on_missing_facts () =
  let carrier = Launch_contract.solve_tri_symbolic_dimension_carrier in
  let facts = symbolic_dimension_carrier_facts carrier in
  check_carrier_missing_validation_field "missing carrier K parameter"
    "symbolic_parameter" carrier
    { facts with carrier_fact_symbolic_parameter = None };
  check_carrier_missing_validation_field "missing carrier K candidates"
    "symbolic_candidate_values" carrier
    { facts with carrier_fact_symbolic_candidate_values = None };
  check_carrier_missing_validation_field "missing carrier launch branch"
    "launch_branch_conditions" carrier
    { facts with carrier_fact_launch_branch_conditions = None };
  check_carrier_missing_validation_field "missing carrier source width"
    "source_width_relation" carrier
    { facts with carrier_fact_source_width_relation = None };
  check_carrier_missing_validation_field "missing carrier positive guards"
    "positive_shape_guards" carrier
    { facts with carrier_fact_positive_shape_guards = None };
  check_carrier_missing_validation_field "missing carrier subgroup size"
    "subgroup_size" carrier
    { facts with carrier_fact_subgroup_size = None };
  check_carrier_field_mismatch "mismatched carrier template dimensions"
    "template_dimensions" carrier
    { facts with carrier_fact_template_dimensions = Some [ "n_template=64" ] };
  check_carrier_field_mismatch "mismatched carrier symbolic block dim"
    "block_dim" carrier
    { facts with carrier_fact_block_dim = Some "[32, 16, 1]" };
  check_carrier_field_mismatch "mismatched carrier source width"
    "source_width_variable" carrier
    { facts with carrier_fact_source_width_variable = Some "head_size" };
  check_carrier_field_mismatch "mismatched carrier route owner" "route_owner"
    carrier
    { facts with carrier_fact_route_owner = Some "task-local-report" };
  check_carrier_field_mismatch "neighbor cannot become carrier lookup anchor"
    "lookup_anchor_rows" carrier
    {
      facts with
      carrier_fact_lookup_anchor_row_ids = Some [ "L117"; "L118"; "L119" ];
    };
  check_carrier_field_mismatch "missing carrier excluded sentinel row"
    "excluded_rows" carrier
    { facts with carrier_fact_excluded_row_ids = Some [ "L116"; "L127" ] }

let test_host_template_candidate_carrier_fails_closed_on_missing_facts () =
  let carrier =
    Launch_contract.host_template_specialization_candidate_carrier
  in
  let facts = guarded_candidate_carrier_facts carrier in
  check_candidate_missing_validation_field "missing candidate first blocker"
    "first_blocker" carrier
    { facts with candidate_fact_first_blocker = None };
  check_candidate_missing_validation_field "missing candidate required facts"
    "required_fact_keys" carrier
    { facts with candidate_fact_required_fact_keys = None };
  check_candidate_missing_validation_field "missing candidate solver policy"
    "solver_policy" carrier
    { facts with candidate_fact_solver_policy = None };
  check_candidate_field_mismatch "mismatched candidate count"
    "affected_family_count" carrier
    { facts with candidate_fact_affected_family_count = Some 53 };
  check_candidate_field_mismatch "mismatched candidate route owner"
    "route_owner" carrier
    { facts with candidate_fact_route_owner = Some "Memory_event.translate" };
  check_candidate_field_mismatch "mismatched candidate solver policy"
    "solver_policy" carrier
    { facts with candidate_fact_solver_policy = Some "solver_input" };
  check_candidate_field_mismatch "mismatched candidate admission status"
    "admission_status" carrier
    { facts with candidate_fact_admission_status = Some "admitted" }

let check_selected_missing_validation_field label field selected facts =
  match
    Launch_contract_generator.validate_selected_manifest_facts selected facts
  with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Missing_field { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let check_selected_field_mismatch label field selected facts =
  match
    Launch_contract_generator.validate_selected_manifest_facts selected facts
  with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Field_mismatch { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let test_selected_l117_validation_fails_closed_on_missing_facts () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let row = find_row rows "L117" in
  let selected = selected_generated_row "L117" in
  let facts = selected_pre_promotion_manifest_facts row selected in
  check_selected_missing_validation_field "missing selected family classifier"
    "family" selected
    { facts with selected_family_candidates = [] };
  (match
     Launch_contract_generator.validate_selected_manifest_facts selected
       {
         facts with
         selected_family_candidates =
           [
             Launch_contract_generator.Solve_tri_fast;
             Launch_contract_generator.Solve_tri_fast;
           ];
       }
   with
  | Error (Launch_contract_generator.Ambiguous_field { field; _ }) ->
      Alcotest.(check string)
        "ambiguous selected family classifier" "family" field
  | Ok () ->
      failf "ambiguous selected family classifier: expected validation failure"
  | Error error ->
      failf
        "ambiguous selected family classifier: unexpected validation error: %s"
        (Launch_contract_generator.validation_error_to_string error));
  check_selected_missing_validation_field "missing selected parsed kernel"
    "parsed_kernel" selected
    { facts with selected_fact_parsed_kernel = None };
  check_selected_missing_validation_field "missing selected source branch"
    "source_branch" selected
    { facts with selected_fact_source_branch_conditions = None };
  check_selected_missing_validation_field "missing selected subgroup config"
    "subgroup_size" selected
    { facts with selected_fact_subgroup_size = None };
  check_selected_missing_validation_field "missing selected block dim"
    "concrete_block_dim" selected
    { facts with selected_fact_concrete_block_dim = None };
  check_selected_missing_validation_field "missing selected evidence key"
    "evidence_artifact_key" selected
    { facts with selected_fact_evidence_artifact_key = None };
  check_selected_field_mismatch "selected facts cannot move to neighbor row"
    "row_id" selected
    { facts with selected_fact_row_id = "L118" };
  check_selected_field_mismatch "mismatched selected manifest kernel"
    "manifest_kernel" selected
    {
      facts with
      selected_fact_manifest_kernel = Some "solve_tri_f32_fast<128, 32>";
    };
  check_selected_field_mismatch "mismatched selected parsed kernel"
    "parsed_kernel" selected
    { facts with selected_fact_parsed_kernel = Some "solve_tri_f32_fast" };
  check_selected_field_mismatch "mismatched selected template arg"
    "template_arg" selected
    { facts with selected_fact_template_arg = Some "64" };
  check_selected_field_mismatch "mismatched selected subgroup size"
    "subgroup_size" selected
    { facts with selected_fact_subgroup_size = Some 16 };
  check_selected_field_mismatch "mismatched selected block dim"
    "concrete_block_dim" selected
    { facts with selected_fact_concrete_block_dim = Some [ 64; 1; 1 ] };
  check_selected_field_mismatch "mismatched selected evidence key"
    "evidence_artifact_key" selected
    {
      facts with
      selected_fact_evidence_artifact_key = Some "h518_source_slice";
    };
  check_selected_field_mismatch "mismatched selected DRF status" "drf_status"
    selected
    { facts with selected_fact_drf_status = Some "verified" };
  check_selected_field_mismatch "mismatched selected artifact status"
    "artifact_status" selected
    { facts with selected_fact_artifact_status = Some "source_intake" };
  check_selected_field_mismatch "mismatched selected subgroup helper"
    "subgroup_helper" selected
    { facts with selected_fact_subgroup_helper = Some "__syncthreads" }

let test_selected_l118_validation_rejects_l117_facts () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let row = find_row rows "L118" in
  let selected = selected_generated_row "L118" in
  let facts = selected_pre_promotion_manifest_facts row selected in
  check_selected_field_mismatch "L118 rejects L117 manifest kernel"
    "manifest_kernel" selected
    {
      facts with
      selected_fact_manifest_kernel = Some "solve_tri_f32_fast<64, 32>";
    };
  check_selected_field_mismatch "L118 rejects L117 parsed kernel"
    "parsed_kernel" selected
    { facts with selected_fact_parsed_kernel = Some "solve_tri_f32_fast_l117" };
  check_selected_field_mismatch "L118 rejects L117 template arg" "template_arg"
    selected
    { facts with selected_fact_template_arg = Some "64, 32" };
  check_selected_field_mismatch "L118 rejects L117 block dim"
    "concrete_block_dim" selected
    { facts with selected_fact_concrete_block_dim = Some [ 32; 32; 1 ] };
  check_selected_field_mismatch "L118 rejects L117 evidence key"
    "evidence_artifact_key" selected
    {
      facts with
      selected_fact_evidence_artifact_key = Some "h516_source_intake";
    };
  check_selected_field_mismatch "L118 rejects premature verified status"
    "drf_status" selected
    { facts with selected_fact_drf_status = Some "verified" };
  check_selected_field_mismatch "L118 rejects subgroup verdict artifact status"
    "artifact_status" selected
    {
      facts with
      selected_fact_artifact_status = Some "subgroup_drf_json_verdict";
    }

let check_missing_validation_field label field generated facts =
  match Launch_contract_generator.validate_manifest_facts generated facts with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Missing_field { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let check_field_mismatch label field generated facts =
  match Launch_contract_generator.validate_manifest_facts generated facts with
  | Ok () -> failf "%s: expected validation failure" label
  | Error (Launch_contract_generator.Field_mismatch { field = actual; _ }) ->
      Alcotest.(check string) label field actual
  | Error error ->
      failf "%s: unexpected validation error: %s" label
        (Launch_contract_generator.validation_error_to_string error)

let test_generated_validation_fails_closed_on_missing_facts () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  let generated = generated_row "L072" in
  let facts = valid_manifest_facts_for_generated root rows generated in
  check_missing_validation_field "missing family classifier" "family" generated
    { facts with family_candidates = [] };
  (match
     Launch_contract_generator.validate_manifest_facts generated
       { facts with family_candidates = [ Launch_contract_rows.Gla; Wkv ] }
   with
  | Error (Launch_contract_generator.Ambiguous_field { field; _ }) ->
      Alcotest.(check string) "ambiguous family classifier" "family" field
  | Ok () -> failf "ambiguous family classifier: expected validation failure"
  | Error error ->
      failf "ambiguous family classifier: unexpected validation error: %s"
        (Launch_contract_generator.validation_error_to_string error));
  check_missing_validation_field "missing template value" "template_value"
    generated
    { facts with template_value = None };
  check_missing_validation_field "missing parsed kernel" "parsed_kernel"
    generated
    { facts with parsed_kernel = None };
  check_missing_validation_field "missing block-dim fact" "block_dim_source"
    generated
    { facts with block_dim_source = None };
  check_missing_validation_field "missing grid-dim fact" "grid_dim_source"
    generated
    { facts with grid_dim_source = None };
  check_missing_validation_field "missing source branch" "source_branch"
    generated
    { facts with source_branch = None };
  check_missing_validation_field "missing dynamic shared memory"
    "dynamic_shared_memory" generated
    { facts with dynamic_shared_memory = None };
  check_missing_validation_field "missing evidence artifact expectation"
    "evidence_artifact_key" generated
    { facts with evidence_artifact_key = None };
  check_field_mismatch "mismatched parsed kernel" "parsed_kernel" generated
    { facts with parsed_kernel = Some "other_kernel" }

let test_wkv_campaign_handoff_state () =
  let root = repo_root () in
  let manifest = load_manifest root in
  let rows = manifest_rows manifest in
  let wkv_rows =
    rows
    |> List.filter (fun row ->
        String.equal
          (string_field "source_file" row)
          "llama.cpp/ggml/src/ggml-cuda/wkv.cu")
  in
  Alcotest.(check (list string))
    "WKV source rows are exactly the closed campaign rows" expected_wkv_row_ids
    (wkv_rows |> List.map row_id |> sorted);
  List.iter
    (fun (row_id, artifact_key) ->
      let row = find_row rows row_id in
      let contract =
        Launch_contract.of_row_id row_id |> function
        | Ok contract -> contract
        | Error error -> Alcotest.fail (Launch_contract.error_to_string error)
      in
      Alcotest.(check string)
        (row_id ^ " manifest kernel")
        contract.manifest_kernel
        (string_field "kernel_or_template" row);
      Alcotest.(check string)
        (row_id ^ " template arg") contract.template_arg
        (row |> field "concrete_template_args" |> string_field "value");
      Alcotest.(check string)
        (row_id ^ " frontend status")
        "memory_event_obligations_generated"
        (string_field "frontend_status" row);
      Alcotest.(check string)
        (row_id ^ " artifact status")
        "ordinary_launch_contract_drf_json_verdict"
        (string_field "artifact_status" row);
      Alcotest.(check string)
        (row_id ^ " DRF status") "verified"
        (string_field "drf_status" row);
      Alcotest.(check (option int))
        (row_id ^ " timeout") (Some 10000)
        (nullable_int_field "timeout_ms" row);
      null_field "subgroup_size_if_any" row;
      Alcotest.(check bool)
        (row_id ^ " owns " ^ artifact_key)
        true
        (Option.is_some
           (row |> field "evidence_artifact" |> field_opt artifact_key)))
    expected_wkv_rows;
  let attempted =
    manifest |> field "command_metadata" |> list_field "attempted_analyzer_runs"
  in
  Alcotest.(check (list string))
    "WKV command metadata rows" expected_wkv_row_ids
    (attempted
    |> List.map (fun entry -> string_field "row_id" entry)
    |> List.filter (fun row_id -> List.mem row_id expected_wkv_row_ids)
    |> sorted)

let test_launch_contract_artifact_keys_are_row_local () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  List.iter
    (fun (artifact_key, expected_owner) ->
      let owners =
        rows
        |> List.filter (fun row ->
            Option.is_some
              (row |> field "evidence_artifact" |> field_opt artifact_key))
        |> List.map row_id |> sorted
      in
      Alcotest.(check (list string))
        (artifact_key ^ " owner") [ expected_owner ] owners)
    launch_contract_artifact_owners

let test_neighbor_rows_remain_unpromoted () =
  let root = repo_root () in
  let rows = load_manifest root |> manifest_rows in
  List.iter
    (fun row_id ->
      let row = find_row rows row_id in
      Alcotest.(check string)
        (row_id ^ " DRF status") "not_attempted"
        (string_field "drf_status" row);
      Alcotest.(check string)
        (row_id ^ " artifact status")
        "none"
        (string_field "artifact_status" row);
      null_field "timeout_ms" row;
      null_field "preprocessing_profile" row;
      null_field "extraction_fixture" row;
      Alcotest.(check bool)
        (row_id ^ " has no H502 artifact")
        false
        (Option.is_some
           (row |> field "evidence_artifact" |> field_opt "h502_launch_contract"));
      Alcotest.(check bool)
        (row_id ^ " has no H507 artifact")
        false
        (Option.is_some
           (row |> field "evidence_artifact" |> field_opt "h507_launch_contract"));
      Alcotest.(check bool)
        (row_id ^ " has no H508 artifact")
        false
        (Option.is_some
           (row |> field "evidence_artifact" |> field_opt "h508_launch_contract")))
    [
      "L074";
      "L116";
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

let test_readme_lists_current_launch_contract_rows () =
  let root = repo_root () in
  let readme = read_file (repo_path root "faial/drf/README.md") in
  List.iter
    (fun row_id ->
      Alcotest.(check bool)
        ("README lists " ^ row_id) true
        (string_contains readme ("`" ^ row_id ^ "`")))
    [ "L072"; "L073"; "L117"; "L118"; "L143"; "L144"; "L145"; "L146" ];
  Alcotest.(check bool)
    "README documents WKV campaign guard" true
    (string_contains readme "exact WKV row set");
  Alcotest.(check bool)
    "README documents L117 subgroup promotion" true
    (string_contains readme "S404 promotes only canonical `L117`");
  Alcotest.(check bool)
    "README documents explicit L117 subgroup size" true
    (string_contains readme "`--subgroup-size 32`");
  Alcotest.(check bool)
    "README documents no ordinary L117 fallback" true
    (string_contains readme "same-named ordinary kernel");
  Alcotest.(check bool)
    "README documents L118 subgroup promotion" true
    (string_contains readme "S409 promotes only canonical `L118`");
  Alcotest.(check bool)
    "README documents L118 non-claim" true
    (string_contains readme "This does not promote");
  Alcotest.(check bool)
    "README documents L118 neighbor boundary" true
    (string_contains readme "solve-tri neighbors `L116` or `L119`-`L128`");
  Alcotest.(check bool)
    "README documents S430 symbolic K guard" true
    (string_contains readme "S430 adds a typed symbolic `K` guard");
  Alcotest.(check bool)
    "README documents S430 blocker" true
    (string_contains readme "Memory_event.Subgroup_obligation` blocker");
  Alcotest.(check bool)
    "README documents S433 carrier" true
    (string_contains readme "S433 adds a typed symbolic dimension carrier");
  Alcotest.(check bool)
    "README documents S437 symbolic checked dimensions" true
    (string_contains readme "S437 extends `Memory_event.Subgroup_obligation`");
  Alcotest.(check bool)
    "README documents S447 candidate carrier" true
    (string_contains readme
       "S447 adds a non-admission generic candidate carrier")

let tests =
  [
    ( "manifest counts are row-derived",
      `Quick,
      test_manifest_counts_are_row_derived );
    ( "launch-contract rows match manifest",
      `Quick,
      test_launch_contract_rows_match_manifest );
    ( "generated rows match manual catalog and manifest",
      `Quick,
      test_generated_rows_match_manual_catalog_and_manifest );
    ( "generated validation fails closed on missing facts",
      `Quick,
      test_generated_validation_fails_closed_on_missing_facts );
    ( "L117 exact production promotion",
      `Quick,
      test_l117_exact_production_promotion );
    ( "L118 exact production promotion",
      `Quick,
      test_l118_exact_production_promotion );
    ( "selected L117 generated facts match H516 evidence",
      `Quick,
      test_selected_l117_generated_facts_match_h516_evidence );
    ( "selected L118 generated facts match S406 schema",
      `Quick,
      test_selected_l118_generated_facts_match_s406_schema );
    ( "solve-tri family contract manifest boundaries",
      `Quick,
      test_solve_tri_family_contract_manifest_boundaries );
    ( "solve-tri symbolic K guard consumption and blocker",
      `Quick,
      test_solve_tri_symbolic_k_guard_consumption_and_blocker );
    ( "host/template candidate carrier matches S445 blocker",
      `Quick,
      test_host_template_candidate_carrier_matches_s445_blocker );
    ( "solve-tri symbolic K guard fails closed on missing facts",
      `Quick,
      test_solve_tri_symbolic_k_guard_fails_closed_on_missing_facts );
    ( "solve-tri symbolic dimension carrier fails closed on missing facts",
      `Quick,
      test_solve_tri_symbolic_dimension_carrier_fails_closed_on_missing_facts );
    ( "host/template candidate carrier fails closed on missing facts",
      `Quick,
      test_host_template_candidate_carrier_fails_closed_on_missing_facts );
    ( "selected L117 validation fails closed on missing facts",
      `Quick,
      test_selected_l117_validation_fails_closed_on_missing_facts );
    ( "selected L118 validation rejects L117 facts",
      `Quick,
      test_selected_l118_validation_rejects_l117_facts );
    ("WKV campaign handoff state", `Quick, test_wkv_campaign_handoff_state);
    ( "launch-contract artifact keys are row-local",
      `Quick,
      test_launch_contract_artifact_keys_are_row_local );
    ( "neighbor rows remain unpromoted",
      `Quick,
      test_neighbor_rows_remain_unpromoted );
    ( "README lists current launch-contract rows",
      `Quick,
      test_readme_lists_current_launch_contract_rows );
  ]

let () =
  Alcotest.run "Launch_contract_manifest"
    [ ("launch_contract_manifest", tests) ]
