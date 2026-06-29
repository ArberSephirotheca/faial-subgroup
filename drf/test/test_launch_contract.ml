open Protocols
open Drf
open Exp
module LC = Launch_contract

let expect_ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (LC.error_to_string error)

let expect_error = function
  | Ok _ -> Alcotest.fail "expected error"
  | Error error -> error

let expect_some label = function
  | Some value -> value
  | None -> Alcotest.fail (label ^ " unexpectedly missing")

let nvar name = Var (Variable.from_name name)

let has_conjunct expected actual =
  List.exists (( = ) expected) (b_and_split actual)

let check_conjunct name expected actual =
  Alcotest.(check bool) name true (has_conjunct expected actual)

let check_dim3 label (expected : Dim3.t) (actual : Dim3.t) =
  Alcotest.(check int) (label ^ ".x") expected.x actual.x;
  Alcotest.(check int) (label ^ ".y") expected.y actual.y;
  Alcotest.(check int) (label ^ ".z") expected.z actual.z

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

let test_gla_l072_contract_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L072") in
  Alcotest.(check string) "row id" "L072" contract.row_id;
  Alcotest.(check string)
    "manifest kernel" "gated_linear_attn_f32<64>" contract.manifest_kernel;
  Alcotest.(check string)
    "parsed kernel" "gated_linear_attn_f32" contract.parsed_kernel;
  Alcotest.(check string) "template arg" "64" contract.template_arg;
  Alcotest.(check string) "template param" "HEAD_SIZE" contract.template_param;
  Alcotest.(check int) "template value" 64 contract.template_value;
  Alcotest.(check int)
    "catalog row count" 6
    (List.length Launch_contract_rows.all);
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 64 block_dim.x;
  Alcotest.(check int) "blockDim.y" 1 block_dim.y;
  Alcotest.(check int) "blockDim.z" 1 block_dim.z;
  Alcotest.(check (list (pair string int)))
    "required params"
    [ ("HEAD_SIZE", 64) ]
    (LC.required_params contract);
  let pre = LC.precondition contract in
  check_conjunct "HEAD_SIZE fact" (n_eq (nvar "HEAD_SIZE") (Num 64)) pre;
  check_conjunct "C/H fact" (n_eq (n_div (nvar "C") (nvar "H")) (Num 64)) pre;
  check_conjunct "B positive" (n_gt (nvar "B") (Num 0)) pre;
  check_conjunct "T positive" (n_gt (nvar "T") (Num 0)) pre;
  check_conjunct "C positive" (n_gt (nvar "C") (Num 0)) pre;
  check_conjunct "H positive" (n_gt (nvar "H") (Num 0)) pre;
  check_conjunct "symbolic gridDim.x"
    (n_eq (Var Variable.gdim_x) (n_mult (nvar "B") (nvar "H")))
    pre

let test_gla_l073_contract_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L073") in
  Alcotest.(check string) "row id" "L073" contract.row_id;
  Alcotest.(check string)
    "manifest kernel" "gated_linear_attn_f32<128>" contract.manifest_kernel;
  Alcotest.(check string) "template arg" "128" contract.template_arg;
  Alcotest.(check string) "template param" "HEAD_SIZE" contract.template_param;
  Alcotest.(check int) "template value" 128 contract.template_value;
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 128 block_dim.x;
  check_conjunct "C/H fact"
    (n_eq (n_div (nvar "C") (nvar "H")) (Num 128))
    (LC.precondition contract)

let test_wkv_l143_contract_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L143") in
  Alcotest.(check string) "row id" "L143" contract.row_id;
  Alcotest.(check string)
    "manifest kernel" "rwkv_wkv_f32<CUDA_WKV_BLOCK_SIZE>"
    contract.manifest_kernel;
  Alcotest.(check string) "parsed kernel" "rwkv_wkv_f32" contract.parsed_kernel;
  Alcotest.(check string)
    "template arg" "CUDA_WKV_BLOCK_SIZE" contract.template_arg;
  Alcotest.(check string) "template param" "block_size" contract.template_param;
  Alcotest.(check int) "template value" 64 contract.template_value;
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 64 block_dim.x;
  Alcotest.(check int) "blockDim.y" 1 block_dim.y;
  Alcotest.(check int) "blockDim.z" 1 block_dim.z;
  Alcotest.(check (list (pair string int)))
    "required params"
    [ ("block_size", 64) ]
    (LC.required_params contract);
  let pre = LC.precondition contract in
  check_conjunct "block_size fact" (n_eq (nvar "block_size") (Num 64)) pre;
  check_conjunct "C/H fact" (n_eq (n_div (nvar "C") (nvar "H")) (Num 64)) pre;
  check_conjunct "B positive" (n_gt (nvar "B") (Num 0)) pre;
  check_conjunct "T positive" (n_gt (nvar "T") (Num 0)) pre;
  check_conjunct "C positive" (n_gt (nvar "C") (Num 0)) pre;
  check_conjunct "H positive" (n_gt (nvar "H") (Num 0)) pre;
  check_conjunct "symbolic gridDim.x"
    (n_eq (Var Variable.gdim_x) (n_mult (nvar "B") (nvar "H")))
    pre

let test_wkv_l144_contract_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L144") in
  Alcotest.(check string) "row id" "L144" contract.row_id;
  Alcotest.(check string)
    "manifest kernel" "rwkv_wkv_f32<CUDA_WKV_BLOCK_SIZE * 2>"
    contract.manifest_kernel;
  Alcotest.(check string) "parsed kernel" "rwkv_wkv_f32" contract.parsed_kernel;
  Alcotest.(check string)
    "template arg" "CUDA_WKV_BLOCK_SIZE * 2" contract.template_arg;
  Alcotest.(check string) "template param" "block_size" contract.template_param;
  Alcotest.(check int) "template value" 128 contract.template_value;
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 128 block_dim.x;
  Alcotest.(check int) "blockDim.y" 1 block_dim.y;
  Alcotest.(check int) "blockDim.z" 1 block_dim.z;
  Alcotest.(check (list (pair string int)))
    "required params"
    [ ("block_size", 128) ]
    (LC.required_params contract);
  let pre = LC.precondition contract in
  check_conjunct "block_size fact" (n_eq (nvar "block_size") (Num 128)) pre;
  check_conjunct "C/H fact" (n_eq (n_div (nvar "C") (nvar "H")) (Num 128)) pre;
  check_conjunct "B positive" (n_gt (nvar "B") (Num 0)) pre;
  check_conjunct "T positive" (n_gt (nvar "T") (Num 0)) pre;
  check_conjunct "C positive" (n_gt (nvar "C") (Num 0)) pre;
  check_conjunct "H positive" (n_gt (nvar "H") (Num 0)) pre;
  check_conjunct "symbolic gridDim.x"
    (n_eq (Var Variable.gdim_x) (n_mult (nvar "B") (nvar "H")))
    pre

let test_wkv7_l145_contract_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L145") in
  Alcotest.(check string) "row id" "L145" contract.row_id;
  Alcotest.(check string)
    "manifest kernel" "rwkv_wkv7_f32<CUDA_WKV_BLOCK_SIZE>"
    contract.manifest_kernel;
  Alcotest.(check string) "parsed kernel" "rwkv_wkv7_f32" contract.parsed_kernel;
  Alcotest.(check string)
    "template arg" "CUDA_WKV_BLOCK_SIZE" contract.template_arg;
  Alcotest.(check string) "template param" "block_size" contract.template_param;
  Alcotest.(check int) "template value" 64 contract.template_value;
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 64 block_dim.x;
  Alcotest.(check int) "blockDim.y" 1 block_dim.y;
  Alcotest.(check int) "blockDim.z" 1 block_dim.z;
  Alcotest.(check (list (pair string int)))
    "required params"
    [ ("block_size", 64) ]
    (LC.required_params contract);
  let pre = LC.precondition contract in
  check_conjunct "block_size fact" (n_eq (nvar "block_size") (Num 64)) pre;
  check_conjunct "C/H fact" (n_eq (n_div (nvar "C") (nvar "H")) (Num 64)) pre;
  check_conjunct "B positive" (n_gt (nvar "B") (Num 0)) pre;
  check_conjunct "T positive" (n_gt (nvar "T") (Num 0)) pre;
  check_conjunct "C positive" (n_gt (nvar "C") (Num 0)) pre;
  check_conjunct "H positive" (n_gt (nvar "H") (Num 0)) pre;
  check_conjunct "symbolic gridDim.x"
    (n_eq (Var Variable.gdim_x) (n_mult (nvar "B") (nvar "H")))
    pre

let test_wkv7_l146_contract_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L146") in
  Alcotest.(check string) "row id" "L146" contract.row_id;
  Alcotest.(check string)
    "manifest kernel" "rwkv_wkv7_f32<CUDA_WKV_BLOCK_SIZE * 2>"
    contract.manifest_kernel;
  Alcotest.(check string) "parsed kernel" "rwkv_wkv7_f32" contract.parsed_kernel;
  Alcotest.(check string)
    "template arg" "CUDA_WKV_BLOCK_SIZE * 2" contract.template_arg;
  Alcotest.(check string) "template param" "block_size" contract.template_param;
  Alcotest.(check int) "template value" 128 contract.template_value;
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 128 block_dim.x;
  Alcotest.(check int) "blockDim.y" 1 block_dim.y;
  Alcotest.(check int) "blockDim.z" 1 block_dim.z;
  Alcotest.(check (list (pair string int)))
    "required params"
    [ ("block_size", 128) ]
    (LC.required_params contract);
  let pre = LC.precondition contract in
  check_conjunct "block_size fact" (n_eq (nvar "block_size") (Num 128)) pre;
  check_conjunct "C/H fact" (n_eq (n_div (nvar "C") (nvar "H")) (Num 128)) pre;
  check_conjunct "B positive" (n_gt (nvar "B") (Num 0)) pre;
  check_conjunct "T positive" (n_gt (nvar "T") (Num 0)) pre;
  check_conjunct "C positive" (n_gt (nvar "C") (Num 0)) pre;
  check_conjunct "H positive" (n_gt (nvar "H") (Num 0)) pre;
  check_conjunct "symbolic gridDim.x"
    (n_eq (Var Variable.gdim_x) (n_mult (nvar "B") (nvar "H")))
    pre

let test_unknown_row_fails_closed () : unit =
  match expect_error (LC.of_row_id "L999") with
  | LC.Unknown_row "L999" -> ()
  | error -> Alcotest.fail (LC.error_to_string error)

let test_catalog_and_contract_view_match () : unit =
  let catalog_ids =
    List.map (fun row -> row.Launch_contract_rows.row_id) LC.catalog_rows
  in
  let contract_ids = List.map (fun row -> row.LC.row_id) LC.all in
  Alcotest.(check (list string)) "row ids" catalog_ids contract_ids

let row_ids rows = List.map (fun row -> row.Launch_contract_rows.row_id) rows

let solve_tri_seed_row_ids rows =
  List.map (fun row -> row.Launch_contract_generator.solve_tri_row_id) rows

let solve_tri_symbolic_row_ids rows =
  List.map
    (fun row -> row.Launch_contract_generator.solve_tri_symbolic_row_id)
    rows

let carrier_template_dimensions carrier =
  List.map Launch_contract_generator.symbolic_template_dimension_to_string
    carrier.Launch_contract_generator.carrier_template_dimensions

let is_gla_row (row : Launch_contract_rows.t) =
  match row.family with Gla -> true | Wkv | Wkv7 -> false

let test_catalog_rows_are_generator_backed () : unit =
  Alcotest.(check (list string))
    "generated GLA rows" [ "L072"; "L073" ]
    (row_ids LC.generated_gla_rows);
  Alcotest.(check (list string))
    "generated WKV/WKV7 rows"
    [ "L143"; "L144"; "L145"; "L146" ]
    (row_ids LC.generated_wkv_rows);
  Alcotest.(check (list string))
    "production GLA rows use generated source"
    (row_ids LC.generated_gla_rows)
    (LC.catalog_rows |> List.filter is_gla_row |> row_ids);
  Alcotest.(check (list string))
    "production WKV/WKV7 rows use generated source"
    (row_ids LC.generated_wkv_rows)
    (LC.catalog_rows |> List.filter (fun row -> not (is_gla_row row)) |> row_ids);
  Alcotest.(check (list string))
    "production catalog is generated seed set"
    (row_ids Launch_contract_generator.contracts)
    (row_ids LC.catalog_rows)

let test_conflicting_param_fails_closed () : unit =
  let contract = expect_ok (LC.of_row_id "L072") in
  match expect_error (LC.merge_params contract [ ("HEAD_SIZE", 128) ]) with
  | LC.Conflicting_param { key = "HEAD_SIZE"; expected = 64; actual = 128 } ->
      ()
  | error -> Alcotest.fail (LC.error_to_string error)

let empty_kernel name : Kernel.t =
  {
    name;
    global_variables = Params.empty;
    local_variables = Params.empty;
    arrays = Variable.Map.empty;
    pre = Bool true;
    code = Code.Skip;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

let test_apply_contract_adds_global_precondition () : unit =
  let contract = expect_ok (LC.of_row_id "L072") in
  let kernel =
    expect_ok
      (LC.apply_to_kernel contract (empty_kernel "gated_linear_attn_f32"))
  in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        ("global " ^ name) true
        (Params.mem (Variable.from_name name) kernel.global_variables))
    [ "B"; "T"; "C"; "H"; "HEAD_SIZE" ];
  check_conjunct "contract precondition"
    (n_eq (n_div (nvar "C") (nvar "H")) (Num 64))
    kernel.pre

let test_kernel_mismatch_fails_closed () : unit =
  let contract = expect_ok (LC.of_row_id "L072") in
  match expect_error (LC.apply_to_kernel contract (empty_kernel "other")) with
  | LC.Kernel_mismatch { expected = "gated_linear_attn_f32"; actual = "other" }
    ->
      ()
  | error -> Alcotest.fail (LC.error_to_string error)

let test_solve_tri_l117_lookup_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L117") in
  Alcotest.(check string) "row id" "L117" contract.row_id;
  (match contract.family with
  | LC.Solve_tri_fast -> ()
  | LC.Gla | LC.Wkv | LC.Wkv7 ->
      Alcotest.fail "L117 must remain a solve-tri pending lookup row");
  Alcotest.(check string)
    "manifest kernel" "solve_tri_f32_fast<64, 32>" contract.manifest_kernel;
  Alcotest.(check string)
    "parsed kernel" "solve_tri_f32_fast_l117" contract.parsed_kernel;
  Alcotest.(check string) "template arg" "64, 32" contract.template_arg;
  Alcotest.(check (list (pair string int)))
    "template bindings"
    [ ("n_template", 64); ("k_template", 32) ]
    contract.template_bindings;
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 32 block_dim.x;
  Alcotest.(check int) "blockDim.y" 32 block_dim.y;
  Alcotest.(check int) "blockDim.z" 1 block_dim.z;
  Alcotest.(check bool)
    "L117 is lookup-only" true
    (LC.lookup_rows
    |> List.exists (fun row -> String.equal row.LC.row_id "L117"));
  Alcotest.(check bool)
    "L117 is not in ordinary catalog" false
    (LC.catalog_rows
    |> List.exists (fun row ->
        String.equal row.Launch_contract_rows.row_id "L117"));
  let pre = LC.precondition contract in
  check_conjunct "n_template fact" (n_eq (nvar "n_template") (Num 64)) pre;
  check_conjunct "k_template fact" (n_eq (nvar "k_template") (Num 32)) pre;
  check_conjunct "blockDim.x fact" (n_eq (Var Variable.bdim_x) (Num 32)) pre;
  check_conjunct "blockDim.y fact" (n_eq (Var Variable.bdim_y) (Num 32)) pre

let test_solve_tri_l118_lookup_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L118") in
  Alcotest.(check string) "row id" "L118" contract.row_id;
  (match contract.family with
  | LC.Solve_tri_fast -> ()
  | LC.Gla | LC.Wkv | LC.Wkv7 ->
      Alcotest.fail "L118 must be a solve-tri pending lookup row");
  Alcotest.(check string)
    "manifest kernel" "solve_tri_f32_fast<64, 16>" contract.manifest_kernel;
  Alcotest.(check string)
    "parsed kernel" "solve_tri_f32_fast_l118" contract.parsed_kernel;
  Alcotest.(check string) "template arg" "64, 16" contract.template_arg;
  Alcotest.(check (list (pair string int)))
    "template bindings"
    [ ("n_template", 64); ("k_template", 16) ]
    contract.template_bindings;
  let block_dim = LC.block_dim contract in
  Alcotest.(check int) "blockDim.x" 32 block_dim.x;
  Alcotest.(check int) "blockDim.y" 16 block_dim.y;
  Alcotest.(check int) "blockDim.z" 1 block_dim.z;
  Alcotest.(check bool)
    "L118 is lookup-only" true
    (LC.lookup_rows
    |> List.exists (fun row -> String.equal row.LC.row_id "L118"));
  Alcotest.(check bool)
    "L118 is not in ordinary catalog" false
    (LC.catalog_rows
    |> List.exists (fun row ->
        String.equal row.Launch_contract_rows.row_id "L118"));
  let pre = LC.precondition contract in
  check_conjunct "n_template fact" (n_eq (nvar "n_template") (Num 64)) pre;
  check_conjunct "k_template fact" (n_eq (nvar "k_template") (Num 16)) pre;
  check_conjunct "blockDim.x fact" (n_eq (Var Variable.bdim_x) (Num 32)) pre;
  check_conjunct "blockDim.y fact" (n_eq (Var Variable.bdim_y) (Num 16)) pre

let test_solve_tri_l117_route_options_fail_closed () : unit =
  let contract = expect_ok (LC.of_row_id "L117") in
  (match LC.check_only_kernel contract None with
  | Error
      (LC.Missing_kernel_selection
         { row_id = "L117"; expected = "solve_tri_f32_fast_l117" }) ->
      ()
  | Ok () -> Alcotest.fail "missing kernel selection unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_only_kernel contract (Some "solve_tri_f32_fast") with
  | Error
      (LC.Kernel_mismatch
         { expected = "solve_tri_f32_fast_l117"; actual = "solve_tri_f32_fast" })
    ->
      ()
  | Ok () -> Alcotest.fail "source-family kernel selection unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  expect_ok (LC.check_only_kernel contract (Some "solve_tri_f32_fast_l117"));
  let expected_block = Dim3.make ~x:32 ~y:32 () in
  (match LC.check_block_dim contract None with
  | Ok (Some actual) ->
      check_dim3 "defaulted L117 blockDim" expected_block actual
  | Ok None -> Alcotest.fail "L117 blockDim was not defaulted"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract (Some expected_block) with
  | Ok (Some actual) ->
      check_dim3 "explicit L117 blockDim" expected_block actual
  | Ok None -> Alcotest.fail "explicit L117 blockDim was dropped"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  let wrong_block = Dim3.make ~x:64 () in
  (match LC.check_block_dim contract (Some wrong_block) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 "expected L117 blockDim" expected_block expected;
      check_dim3 "actual wrong blockDim" wrong_block actual
  | Ok _ -> Alcotest.fail "wrong L117 blockDim unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  let concrete_grid = Dim3.make ~x:1 () in
  (match LC.check_grid_dim contract (Some concrete_grid) with
  | Error (LC.Grid_dim_unsupported actual) ->
      check_dim3 "unsupported concrete gridDim" concrete_grid actual
  | Ok () -> Alcotest.fail "concrete L117 gridDim unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  match LC.check_all_dims contract true with
  | Error (LC.All_dims_unsupported "L117") -> ()
  | Ok () -> Alcotest.fail "--all-dims unexpectedly worked for L117"
  | Error error -> Alcotest.fail (LC.error_to_string error)

let test_solve_tri_l118_route_options_fail_closed () : unit =
  let contract = expect_ok (LC.of_row_id "L118") in
  (match LC.check_only_kernel contract None with
  | Error
      (LC.Missing_kernel_selection
         { row_id = "L118"; expected = "solve_tri_f32_fast_l118" }) ->
      ()
  | Ok () -> Alcotest.fail "missing kernel selection unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_only_kernel contract (Some "solve_tri_f32_fast") with
  | Error
      (LC.Kernel_mismatch
         { expected = "solve_tri_f32_fast_l118"; actual = "solve_tri_f32_fast" })
    ->
      ()
  | Ok () -> Alcotest.fail "source-family kernel selection unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  expect_ok (LC.check_only_kernel contract (Some "solve_tri_f32_fast_l118"));
  let expected_block = Dim3.make ~x:32 ~y:16 () in
  (match LC.check_block_dim contract None with
  | Ok (Some actual) ->
      check_dim3 "defaulted L118 blockDim" expected_block actual
  | Ok None -> Alcotest.fail "L118 blockDim was not defaulted"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract (Some expected_block) with
  | Ok (Some actual) ->
      check_dim3 "explicit L118 blockDim" expected_block actual
  | Ok None -> Alcotest.fail "explicit L118 blockDim was dropped"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  let wrong_block = Dim3.make ~x:32 ~y:32 () in
  (match LC.check_block_dim contract (Some wrong_block) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 "expected L118 blockDim" expected_block expected;
      check_dim3 "actual wrong blockDim" wrong_block actual
  | Ok _ -> Alcotest.fail "wrong L118 blockDim unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  let concrete_grid = Dim3.make ~x:1 () in
  (match LC.check_grid_dim contract (Some concrete_grid) with
  | Error (LC.Grid_dim_unsupported actual) ->
      check_dim3 "unsupported concrete gridDim" concrete_grid actual
  | Ok () -> Alcotest.fail "concrete L118 gridDim unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  match LC.check_all_dims contract true with
  | Error (LC.All_dims_unsupported "L118") -> ()
  | Ok () -> Alcotest.fail "--all-dims unexpectedly worked for L118"
  | Error error -> Alcotest.fail (LC.error_to_string error)

let test_solve_tri_l117_subgroup_route_requires_explicit_size () : unit =
  let l117 = expect_ok (LC.of_row_id "L117") in
  Alcotest.(check bool)
    "L117 requires subgroup route" true
    (LC.requires_subgroup_route l117);
  Alcotest.(check bool)
    "L117 routes with explicit subgroup size 32" true
    (LC.allows_subgroup_route l117 ~subgroup_size:(Some 32));
  Alcotest.(check bool)
    "L117 rejects missing subgroup size" false
    (LC.allows_subgroup_route l117 ~subgroup_size:None);
  Alcotest.(check bool)
    "L117 rejects mismatched subgroup size" false
    (LC.allows_subgroup_route l117 ~subgroup_size:(Some 16));
  let l118 = expect_ok (LC.of_row_id "L118") in
  Alcotest.(check bool)
    "L118 requires subgroup route" true
    (LC.requires_subgroup_route l118);
  Alcotest.(check bool)
    "L118 routes with explicit subgroup size 32" true
    (LC.allows_subgroup_route l118 ~subgroup_size:(Some 32));
  Alcotest.(check bool)
    "L118 rejects missing subgroup size" false
    (LC.allows_subgroup_route l118 ~subgroup_size:None);
  Alcotest.(check bool)
    "L118 rejects mismatched subgroup size" false
    (LC.allows_subgroup_route l118 ~subgroup_size:(Some 16));
  let l072 = expect_ok (LC.of_row_id "L072") in
  Alcotest.(check bool)
    "ordinary rows do not require subgroup route" false
    (LC.requires_subgroup_route l072);
  Alcotest.(check bool)
    "ordinary rows do not use subgroup launch-contract route" false
    (LC.allows_subgroup_route l072 ~subgroup_size:(Some 32))

let test_solve_tri_family_contract_guards () : unit =
  let family = Launch_contract_generator.solve_tri_fast_family in
  (match family.Launch_contract_generator.solve_tri_family with
  | Launch_contract_generator.Solve_tri_fast -> ());
  Alcotest.(check string)
    "family source file" "llama.cpp/ggml/src/ggml-cuda/solve_tri.cu"
    family.Launch_contract_generator.solve_tri_source_file;
  Alcotest.(check string)
    "family source kernel" "solve_tri_f32_fast"
    family.Launch_contract_generator.solve_tri_source_kernel_family;
  Alcotest.(check int)
    "family N template" 64 family.Launch_contract_generator.solve_tri_n_template;
  Alcotest.(check int)
    "family blockDim.x" 32
    family.Launch_contract_generator.solve_tri_block_dim_x;
  Alcotest.(check string)
    "family blockDim source" "threads"
    family.Launch_contract_generator.solve_tri_block_dim_source;
  Alcotest.(check string)
    "family grid source" "grid"
    family.Launch_contract_generator.solve_tri_grid_dim_source;
  Alcotest.(check string)
    "family dynamic smem" "0"
    family.Launch_contract_generator.solve_tri_dynamic_shared_memory;
  Alcotest.(check string)
    "family subgroup helper" "warp_reduce_sum"
    family.Launch_contract_generator.solve_tri_subgroup_helper;
  Alcotest.(check int)
    "family subgroup size" 32
    family.Launch_contract_generator.solve_tri_subgroup_size;
  Alcotest.(check (list string))
    "family lookup rows" [ "L117"; "L118" ]
    (solve_tri_seed_row_ids
       family.Launch_contract_generator.solve_tri_lookup_rows);
  Alcotest.(check (list string))
    "symbolic K candidate rows"
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
    ]
    (solve_tri_symbolic_row_ids
       family.Launch_contract_generator.solve_tri_symbolic_k_rows);
  Alcotest.(check (list string))
    "family unpromoted rows"
    [ "L119"; "L120"; "L121"; "L122"; "L123"; "L124"; "L125"; "L126" ]
    family.Launch_contract_generator.solve_tri_unpromoted_row_ids;
  Alcotest.(check (list string))
    "family excluded rows" [ "L116"; "L127"; "L128" ]
    family.Launch_contract_generator.solve_tri_excluded_row_ids;
  Alcotest.(check (list string))
    "generated selected rows follow family lookup rows" [ "L117"; "L118" ]
    (Launch_contract_generator.selected_rows
    |> List.map (fun row -> row.Launch_contract_generator.selected_row_id));
  let guard = LC.solve_tri_symbolic_k_guard in
  Alcotest.(check string)
    "symbolic K parameter" "K"
    guard.Launch_contract_generator.symbolic_guard_k_parameter;
  Alcotest.(check string)
    "symbolic block relation" "blockDim = [32, K, 1]"
    guard.Launch_contract_generator.symbolic_guard_block_dim_relation;
  Alcotest.(check (list string))
    "symbolic lookup anchors" [ "L117"; "L118" ]
    guard.Launch_contract_generator.symbolic_guard_lookup_anchor_row_ids;
  Alcotest.(check (list string))
    "symbolic unpromoted rows"
    [ "L119"; "L120"; "L121"; "L122"; "L123"; "L124"; "L125"; "L126" ]
    guard.Launch_contract_generator.symbolic_guard_unpromoted_row_ids;
  Alcotest.(check (list string))
    "symbolic excluded rows" [ "L116"; "L127"; "L128" ]
    guard.Launch_contract_generator.symbolic_guard_excluded_row_ids;
  let blocker_dump =
    String.concat "\n" (LC.solve_tri_symbolic_k_obligation_blocker_lines ())
  in
  Alcotest.(check bool)
    "blocker names symbolic K" true
    (string_contains blocker_dump "symbolic_parameter: K");
  Alcotest.(check bool)
    "blocker names subgroup obligation owner" true
    (string_contains blocker_dump "Memory_event.Subgroup_obligation");
  Alcotest.(check bool)
    "blocker records non-solver provenance" true
    (string_contains blocker_dump "provenance rather than solver input")

let test_solve_tri_symbolic_dimension_carrier () : unit =
  let l117 = expect_ok (LC.of_row_id "L117") in
  let l118 = expect_ok (LC.of_row_id "L118") in
  let l072 = expect_ok (LC.of_row_id "L072") in
  let carrier =
    expect_some "L117 symbolic dimension carrier"
      (LC.symbolic_dimension_carrier l117)
  in
  ignore
    (expect_some "L118 symbolic dimension carrier"
       (LC.symbolic_dimension_carrier l118));
  Alcotest.(check (option string))
    "ordinary row has no symbolic dimension carrier" None
    (Option.map
       (fun carrier -> carrier.Launch_contract_generator.carrier_source_file)
       (LC.symbolic_dimension_carrier l072));
  Alcotest.(check string)
    "carrier source file" "llama.cpp/ggml/src/ggml-cuda/solve_tri.cu"
    carrier.Launch_contract_generator.carrier_source_file;
  Alcotest.(check string)
    "carrier source kernel" "solve_tri_f32_fast"
    carrier.Launch_contract_generator.carrier_source_kernel_family;
  Alcotest.(check string)
    "carrier source width variable" "k"
    carrier.Launch_contract_generator.carrier_source_width_variable;
  Alcotest.(check string)
    "carrier source width relation" "k == K"
    carrier.Launch_contract_generator.carrier_source_width_relation;
  Alcotest.(check (list string))
    "carrier template dimensions"
    [ "n_template=64"; "k_template=K" ]
    (carrier_template_dimensions carrier);
  Alcotest.(check string)
    "carrier blockDim" "[32, K, 1]"
    (Launch_contract_generator.symbolic_dim3_to_string
       carrier.Launch_contract_generator.carrier_block_dim);
  Alcotest.(check (list int))
    "carrier symbolic K values"
    [ 32; 16; 14; 12; 10; 8; 6; 4; 2; 1 ]
    (Launch_contract_generator.symbolic_dimension_candidate_values
       carrier.Launch_contract_generator.carrier_block_dim.symbolic_dim_y);
  Alcotest.(check (list string))
    "carrier launch branch conditions"
    [ "n == 64"; "case K in {32, 16, 14, 12, 10, 8, 6, 4, 2, 1}" ]
    carrier.Launch_contract_generator.carrier_launch_branch_conditions;
  Alcotest.(check bool)
    "carrier records positive K guard" true
    (List.mem "K > 0"
       carrier.Launch_contract_generator.carrier_positive_shape_guards);
  Alcotest.(check bool)
    "carrier preserves symbolic blockDim.y" true
    (List.mem "blockDim.y == K"
       carrier.Launch_contract_generator.carrier_positive_shape_guards);
  Alcotest.(check int)
    "carrier explicit subgroup size" 32
    carrier.Launch_contract_generator.carrier_subgroup_size;
  Alcotest.(check string)
    "carrier route owner" "Memory_event.Subgroup_obligation"
    carrier.Launch_contract_generator.carrier_route_owner;
  Alcotest.(check (list string))
    "carrier lookup anchors" [ "L117"; "L118" ]
    carrier.Launch_contract_generator.carrier_lookup_anchor_row_ids;
  Alcotest.(check (list string))
    "carrier unpromoted rows"
    [ "L119"; "L120"; "L121"; "L122"; "L123"; "L124"; "L125"; "L126" ]
    carrier.Launch_contract_generator.carrier_unpromoted_row_ids;
  Alcotest.(check (list string))
    "carrier excluded rows" [ "L116"; "L127"; "L128" ]
    carrier.Launch_contract_generator.carrier_excluded_row_ids

let test_host_template_candidate_carrier_is_non_admission () : unit =
  let carrier = LC.host_template_specialization_candidate_carrier in
  Alcotest.(check string)
    "carrier id" "s447_host_template_specialization"
    carrier.Launch_contract_generator.candidate_carrier_id;
  Alcotest.(check string)
    "carrier blocker" "template_args_unresolved_or_conflicting"
    carrier.Launch_contract_generator.candidate_first_blocker;
  Alcotest.(check string)
    "carrier stage" "blocked_at_host_or_template_specialization"
    carrier.Launch_contract_generator.candidate_proof_ladder_stage;
  Alcotest.(check int)
    "affected families" 54
    carrier.Launch_contract_generator.candidate_affected_family_count;
  Alcotest.(check bool)
    "carrier requires template args" true
    (List.mem "concrete_template_args"
       carrier.Launch_contract_generator.candidate_required_fact_keys);
  Alcotest.(check bool)
    "carrier requires macro profile" true
    (List.mem "macro_profile"
       carrier.Launch_contract_generator.candidate_required_fact_keys);
  Alcotest.(check string)
    "carrier is not solver input" "not_solver_input"
    carrier.Launch_contract_generator.candidate_solver_policy;
  Alcotest.(check string)
    "carrier does not admit proof" "blocked_no_fresh_obligation"
    carrier.Launch_contract_generator.candidate_admission_status;
  (match LC.of_row_id "L003" with
  | Error (LC.Unknown_row "L003") -> ()
  | Ok _ -> Alcotest.fail "S447 carrier unexpectedly admitted L003 lookup row"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  let rendered = String.concat "\n" (LC.guarded_candidate_carrier_lines ()) in
  Alcotest.(check bool)
    "rendered carrier records task-local route" true
    (string_contains rendered
       "Launch_contract_generator.guarded_candidate_carrier");
  Alcotest.(check bool)
    "rendered carrier records non-solver status" true
    (string_contains rendered "solver_policy: not_solver_input")

let test_unselected_solve_tri_neighbors_are_not_lookup_rows () : unit =
  List.iter
    (fun row_id ->
      match expect_error (LC.of_row_id row_id) with
      | LC.Unknown_row actual -> Alcotest.(check string) row_id row_id actual
      | error -> Alcotest.fail (LC.error_to_string error))
    [
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

let tests =
  [
    ("GLa L072 contract shape", `Quick, test_gla_l072_contract_shape);
    ("GLa L073 contract shape", `Quick, test_gla_l073_contract_shape);
    ("WKV L143 contract shape", `Quick, test_wkv_l143_contract_shape);
    ("WKV L144 contract shape", `Quick, test_wkv_l144_contract_shape);
    ("WKV7 L145 contract shape", `Quick, test_wkv7_l145_contract_shape);
    ("WKV7 L146 contract shape", `Quick, test_wkv7_l146_contract_shape);
    ("unknown row fails closed", `Quick, test_unknown_row_fails_closed);
    ( "catalog and contract view match",
      `Quick,
      test_catalog_and_contract_view_match );
    ( "catalog rows are generator-backed",
      `Quick,
      test_catalog_rows_are_generator_backed );
    ( "conflicting param fails closed",
      `Quick,
      test_conflicting_param_fails_closed );
    ( "apply contract adds global precondition",
      `Quick,
      test_apply_contract_adds_global_precondition );
    ("kernel mismatch fails closed", `Quick, test_kernel_mismatch_fails_closed);
    ("solve-tri L117 lookup shape", `Quick, test_solve_tri_l117_lookup_shape);
    ("solve-tri L118 lookup shape", `Quick, test_solve_tri_l118_lookup_shape);
    ( "solve-tri L117 route options fail closed",
      `Quick,
      test_solve_tri_l117_route_options_fail_closed );
    ( "solve-tri L118 route options fail closed",
      `Quick,
      test_solve_tri_l118_route_options_fail_closed );
    ( "solve-tri L117 subgroup route requires explicit size",
      `Quick,
      test_solve_tri_l117_subgroup_route_requires_explicit_size );
    ( "solve-tri family contract guards",
      `Quick,
      test_solve_tri_family_contract_guards );
    ( "solve-tri symbolic dimension carrier",
      `Quick,
      test_solve_tri_symbolic_dimension_carrier );
    ( "host/template candidate carrier is non-admission",
      `Quick,
      test_host_template_candidate_carrier_is_non_admission );
    ( "solve-tri neighbors are not lookup rows",
      `Quick,
      test_unselected_solve_tri_neighbors_are_not_lookup_rows );
  ]

let () = Alcotest.run "Launch_contract" [ ("launch_contract", tests) ]
