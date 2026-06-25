open Drf
module Json = Yojson.Basic

let failf fmt = Printf.ksprintf (fun msg -> Alcotest.fail msg) fmt

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
    Launch_contract_rows.all
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
    [ "L074" ]

let test_readme_lists_current_launch_contract_rows () =
  let root = repo_root () in
  let readme = read_file (repo_path root "faial/drf/README.md") in
  List.iter
    (fun row_id ->
      Alcotest.(check bool)
        ("README lists " ^ row_id) true
        (string_contains readme ("`" ^ row_id ^ "`")))
    [ "L072"; "L073"; "L143"; "L144"; "L145"; "L146" ];
  Alcotest.(check bool)
    "README documents WKV campaign guard" true
    (string_contains readme "exact WKV row set")

let tests =
  [
    ( "manifest counts are row-derived",
      `Quick,
      test_manifest_counts_are_row_derived );
    ( "launch-contract rows match manifest",
      `Quick,
      test_launch_contract_rows_match_manifest );
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
