open Protocols
open Drf
open Exp
module LC = Launch_contract
module LCG = Launch_contract_generator

let expect_ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (LC.error_to_string error)

let expect_generator_ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (LCG.error_to_string error)

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

let catalog_contains_row row_id =
  Launch_contract.catalog_rows
  |> List.exists (fun row -> String.equal row.Launch_contract.row_id row_id)

let lookup_contains_row row_id =
  Launch_contract.lookup_rows
  |> List.exists (fun row -> String.equal row.Launch_contract.row_id row_id)

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
  Alcotest.(check (option string))
    "template param" (Some "HEAD_SIZE") contract.template_param;
  Alcotest.(check (option int))
    "template value" (Some 64) contract.template_value;
  Alcotest.(check int)
    "catalog row count" 6
    (List.length Launch_contract.catalog_rows);
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
  Alcotest.(check (option string))
    "template param" (Some "HEAD_SIZE") contract.template_param;
  Alcotest.(check (option int))
    "template value" (Some 128) contract.template_value;
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
  Alcotest.(check (option string))
    "template param" (Some "block_size") contract.template_param;
  Alcotest.(check (option int))
    "template value" (Some 64) contract.template_value;
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
  Alcotest.(check (option string))
    "template param" (Some "block_size") contract.template_param;
  Alcotest.(check (option int))
    "template value" (Some 128) contract.template_value;
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
  Alcotest.(check (option string))
    "template param" (Some "block_size") contract.template_param;
  Alcotest.(check (option int))
    "template value" (Some 64) contract.template_value;
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
  Alcotest.(check (option string))
    "template param" (Some "block_size") contract.template_param;
  Alcotest.(check (option int))
    "template value" (Some 128) contract.template_value;
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
  let catalog_ids = List.map (fun row -> row.LC.row_id) LC.catalog_rows in
  let contract_ids = List.map (fun row -> row.LC.row_id) LC.all in
  Alcotest.(check (list string)) "row ids" catalog_ids contract_ids

let row_ids rows = List.map (fun row -> row.LC.row_id) rows

let generated_launch_row_ids rows =
  List.map (fun row -> row.Launch_contract_generator.launch_row_id) rows

let solve_tri_seed_row_ids rows =
  List.map (fun row -> row.Launch_contract_generator.solve_tri_row_id) rows

let solve_tri_symbolic_row_ids rows =
  List.map
    (fun row -> row.Launch_contract_generator.solve_tri_symbolic_row_id)
    rows

let carrier_template_dimensions carrier =
  List.map Launch_contract_generator.symbolic_template_dimension_to_string
    carrier.Launch_contract_generator.carrier_template_dimensions

let is_gla_contract (row : LC.t) =
  match row.family with LC.Gla -> true | _ -> false

let is_wkv_contract (row : LC.t) =
  match row.family with LC.Wkv | LC.Wkv7 -> true | _ -> false

let test_catalog_rows_are_generator_backed () : unit =
  Alcotest.(check (list string))
    "generated catalog rows"
    [ "L072"; "L073"; "L143"; "L144"; "L145"; "L146" ]
    (generated_launch_row_ids
       Launch_contract_generator.catalog_launch_contract_rows);
  Alcotest.(check (list string))
    "production catalog is generator launch-contract set"
    (generated_launch_row_ids
       Launch_contract_generator.catalog_launch_contract_rows)
    (row_ids LC.catalog_rows);
  Alcotest.(check (list string))
    "production GLA rows use generated source" [ "L072"; "L073" ]
    (LC.catalog_rows |> List.filter is_gla_contract |> row_ids);
  Alcotest.(check (list string))
    "production WKV/WKV7 rows use generated source"
    [ "L143"; "L144"; "L145"; "L146" ]
    (LC.catalog_rows |> List.filter is_wkv_contract |> row_ids)

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
  | _ -> Alcotest.fail "L117 must remain a solve-tri pending lookup row");
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
    |> List.exists (fun row -> String.equal row.LC.row_id "L117"));
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
  | _ -> Alcotest.fail "L118 must be a solve-tri pending lookup row");
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
    |> List.exists (fun row -> String.equal row.LC.row_id "L118"));
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

let test_l012_finite_type_launch_contract_shape () : unit =
  let contract = expect_ok (LC.of_row_id "L012") in
  Alcotest.(check string) "row id" "L012" contract.row_id;
  (match contract.family with
  | LC.Finite_type_template -> ()
  | _ -> Alcotest.fail "L012 must use finite type-template contract family");
  Alcotest.(check string)
    "manifest kernel" "op_clamp_kernel<T>" contract.manifest_kernel;
  Alcotest.(check string)
    "parsed kernel" "op_clamp_kernel_l012_float" contract.parsed_kernel;
  Alcotest.(check string) "template arg" "T={half,float}" contract.template_arg;
  Alcotest.(check (option string))
    "integer template param" None contract.template_param;
  Alcotest.(check (option int))
    "integer template value" None contract.template_value;
  Alcotest.(check (list (pair string (list string))))
    "finite type domains"
    [ ("T", [ "half"; "float" ]) ]
    (LC.finite_type_domains contract);
  Alcotest.(check (list string))
    "template domains" [ "T={half,float}" ]
    (List.map LC.template_domain_to_string (LC.template_domains contract));
  Alcotest.(check (list (pair string int)))
    "no integer params required" []
    (LC.required_params contract);
  Alcotest.(check bool) "L012 is lookup row" true (lookup_contains_row "L012");
  Alcotest.(check bool)
    "L012 is not exact catalog row" false
    (catalog_contains_row "L012");
  let block_dim = LC.block_dim contract in
  check_dim3 "L012 blockDim" (Dim3.make ~x:256 ()) block_dim;
  let pre = LC.precondition contract in
  check_conjunct "blockDim.x fact" (n_eq (Var Variable.bdim_x) (Num 256)) pre;
  check_conjunct "blockDim.y fact" (n_eq (Var Variable.bdim_y) (Num 1)) pre;
  check_conjunct "blockDim.z fact" (n_eq (Var Variable.bdim_z) (Num 1)) pre;
  check_conjunct "k positive" (n_gt (nvar "k") (Num 0)) pre;
  expect_ok (LC.check_only_kernel contract (Some "op_clamp_kernel_l012_float"));
  (match LC.check_only_kernel contract (Some "op_clamp_kernel") with
  | Error
      (LC.Kernel_mismatch
         { expected = "op_clamp_kernel_l012_float"; actual = "op_clamp_kernel" })
    ->
      ()
  | Ok () -> Alcotest.fail "L012 unexpectedly accepted source template name"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract None with
  | Ok (Some actual) -> check_dim3 "defaulted L012 blockDim" block_dim actual
  | Ok None -> Alcotest.fail "L012 blockDim was not defaulted"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract (Some (Dim3.make ~x:128 ())) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 "expected L012 blockDim" block_dim expected;
      check_dim3 "actual wrong L012 blockDim" (Dim3.make ~x:128 ()) actual
  | Ok _ -> Alcotest.fail "wrong L012 blockDim unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_grid_dim contract (Some (Dim3.make ~x:1 ())) with
  | Error (LC.Grid_dim_unsupported _) -> ()
  | Ok () -> Alcotest.fail "concrete L012 gridDim unexpectedly worked"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  Alcotest.(check bool)
    "L012 does not require subgroup route" false
    (LC.requires_subgroup_route contract)

let check_fill_launch_contract row_id parsed_kernel template_arg type_domain
    artifact_label =
  let contract = expect_ok (LC.of_row_id row_id) in
  Alcotest.(check string) (row_id ^ " row id") row_id contract.row_id;
  (match contract.family with
  | LC.Finite_type_template -> ()
  | _ ->
      Alcotest.fail (row_id ^ " must use finite type-template contract family"));
  Alcotest.(check string)
    (row_id ^ " manifest kernel")
    "fill_kernel<T>" contract.manifest_kernel;
  Alcotest.(check string)
    (row_id ^ " parsed kernel")
    parsed_kernel contract.parsed_kernel;
  Alcotest.(check string)
    (row_id ^ " template arg") template_arg contract.template_arg;
  Alcotest.(check (option string))
    (row_id ^ " integer template param")
    None contract.template_param;
  Alcotest.(check (option int))
    (row_id ^ " integer template value")
    None contract.template_value;
  Alcotest.(check (list (pair string (list string))))
    (row_id ^ " finite type domains")
    [ ("T", [ type_domain ]) ]
    (LC.finite_type_domains contract);
  Alcotest.(check (list string))
    (row_id ^ " template domains")
    [ "T={" ^ type_domain ^ "}" ]
    (List.map LC.template_domain_to_string (LC.template_domains contract));
  Alcotest.(check (list (pair string int)))
    (row_id ^ " no integer params required")
    []
    (LC.required_params contract);
  Alcotest.(check bool)
    (row_id ^ " is lookup row")
    true
    (lookup_contains_row row_id);
  Alcotest.(check bool)
    (row_id ^ " is not exact catalog row")
    false
    (catalog_contains_row row_id);
  let block_dim = LC.block_dim contract in
  check_dim3 (row_id ^ " blockDim") (Dim3.make ~x:256 ()) block_dim;
  let pre = LC.precondition contract in
  check_conjunct
    (row_id ^ " blockDim.x fact")
    (n_eq (Var Variable.bdim_x) (Num 256))
    pre;
  check_conjunct
    (row_id ^ " blockDim.y fact")
    (n_eq (Var Variable.bdim_y) (Num 1))
    pre;
  check_conjunct
    (row_id ^ " blockDim.z fact")
    (n_eq (Var Variable.bdim_z) (Num 1))
    pre;
  check_conjunct (row_id ^ " k positive") (n_gt (nvar "k") (Num 0)) pre;
  expect_ok (LC.check_only_kernel contract (Some parsed_kernel));
  (match LC.check_only_kernel contract (Some "fill_kernel") with
  | Error (LC.Kernel_mismatch { expected; actual = "fill_kernel" }) ->
      Alcotest.(check string)
        (row_id ^ " expected parsed kernel")
        parsed_kernel expected
  | Ok () -> Alcotest.fail (row_id ^ " unexpectedly accepted source family name")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract None with
  | Ok (Some actual) ->
      check_dim3 (row_id ^ " default blockDim") block_dim actual
  | Ok None -> Alcotest.fail (row_id ^ " blockDim was not defaulted")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract (Some (Dim3.make ~x:128 ())) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 (row_id ^ " expected blockDim") block_dim expected;
      check_dim3
        (row_id ^ " actual wrong blockDim")
        (Dim3.make ~x:128 ()) actual
  | Ok _ -> Alcotest.fail (row_id ^ " wrong blockDim unexpectedly worked")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_grid_dim contract (Some (Dim3.make ~x:1 ())) with
  | Error (LC.Grid_dim_unsupported _) -> ()
  | Ok () -> Alcotest.fail (row_id ^ " concrete gridDim unexpectedly worked")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  Alcotest.(check bool)
    (row_id ^ " artifact label")
    true
    (string_contains artifact_label row_id);
  Alcotest.(check bool)
    (row_id ^ " does not require subgroup route")
    false
    (LC.requires_subgroup_route contract)

let test_l067_fill_float_launch_contract_shape () : unit =
  check_fill_launch_contract "L067" "fill_kernel_l067_float" "T=float" "float"
    "S482 L067 exact launch-contract row"

let test_l068_fill_half_launch_contract_shape () : unit =
  check_fill_launch_contract "L068" "fill_kernel_l068_half_profile" "T=half"
    "half" "S482 L068 exact launch-contract row"

let check_s483_profile_launch_contract row_id parsed_kernel manifest_kernel
    source_kernel template_arg type_domain block_dim_x positive_param
    ?exact_grid_x () =
  let contract = expect_ok (LC.of_row_id row_id) in
  Alcotest.(check string) (row_id ^ " row id") row_id contract.row_id;
  (match contract.family with
  | LC.Finite_type_template -> ()
  | _ -> Alcotest.fail (row_id ^ " must use finite type/profile contract family"));
  Alcotest.(check string)
    (row_id ^ " manifest kernel")
    manifest_kernel contract.manifest_kernel;
  Alcotest.(check string)
    (row_id ^ " parsed kernel")
    parsed_kernel contract.parsed_kernel;
  Alcotest.(check string)
    (row_id ^ " template arg") template_arg contract.template_arg;
  Alcotest.(check (list (pair string (list string))))
    (row_id ^ " finite type domains")
    [ ("T", type_domain) ]
    (LC.finite_type_domains contract);
  Alcotest.(check bool)
    (row_id ^ " is lookup row")
    true
    (lookup_contains_row row_id);
  Alcotest.(check bool)
    (row_id ^ " is not exact catalog row")
    false
    (catalog_contains_row row_id);
  let block_dim = LC.block_dim contract in
  check_dim3 (row_id ^ " blockDim") (Dim3.make ~x:block_dim_x ()) block_dim;
  let pre = LC.precondition contract in
  check_conjunct
    (row_id ^ " blockDim.x fact")
    (n_eq (Var Variable.bdim_x) (Num block_dim_x))
    pre;
  check_conjunct
    (row_id ^ " blockDim.y fact")
    (n_eq (Var Variable.bdim_y) (Num 1))
    pre;
  check_conjunct
    (row_id ^ " blockDim.z fact")
    (n_eq (Var Variable.bdim_z) (Num 1))
    pre;
  check_conjunct
    (row_id ^ " positive shape param")
    (n_gt (nvar positive_param) (Num 0))
    pre;
  (match exact_grid_x with
  | Some grid_x ->
      check_conjunct
        (row_id ^ " exact gridDim.x fact")
        (n_eq (Var Variable.gdim_x) (Num grid_x))
        pre
  | None ->
      check_conjunct
        (row_id ^ " symbolic positive gridDim.x fact")
        (n_gt (Var Variable.gdim_x) (Num 0))
        pre);
  expect_ok (LC.check_only_kernel contract (Some parsed_kernel));
  (match LC.check_only_kernel contract (Some source_kernel) with
  | Error (LC.Kernel_mismatch { expected; actual }) ->
      Alcotest.(check string)
        (row_id ^ " expected parsed kernel")
        parsed_kernel expected;
      Alcotest.(check string)
        (row_id ^ " rejected source family")
        source_kernel actual
  | Ok () -> Alcotest.fail (row_id ^ " unexpectedly accepted source kernel")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract None with
  | Ok (Some actual) ->
      check_dim3 (row_id ^ " default blockDim") block_dim actual
  | Ok None -> Alcotest.fail (row_id ^ " blockDim was not defaulted")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract (Some (Dim3.make ~x:128 ())) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 (row_id ^ " expected blockDim") block_dim expected;
      check_dim3
        (row_id ^ " actual wrong blockDim")
        (Dim3.make ~x:128 ()) actual
  | Ok _ -> Alcotest.fail (row_id ^ " wrong blockDim unexpectedly worked")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  Alcotest.(check bool)
    (row_id ^ " does not require subgroup route")
    false
    (LC.requires_subgroup_route contract)

let test_s483_profile_launch_contract_shapes () : unit =
  check_s483_profile_launch_contract "L076" "divide_by_count_l076_float"
    "divide_by_count<float>" "divide_by_count" "T=float" [ "float" ] 1 "count"
    ~exact_grid_x:1 ();
  check_s483_profile_launch_contract "L135" "swiglu_oai_kernel_l135_profile"
    "swiglu_oai_kernel<T>" "swiglu_oai_kernel" "T=float" [ "float" ] 256 "k" ();
  check_s483_profile_launch_contract "L136" "xielu_kernel_l136_profile"
    "xielu_kernel<T>" "xielu_kernel" "T={half,float}" [ "half"; "float" ] 256
    "k" ();
  check_s483_profile_launch_contract "L137" "silu_back_kernel_l137_profile"
    "silu_back_kernel<T>" "silu_back_kernel" "T={half,float}"
    [ "half"; "float" ] 256 "k" ();
  check_s483_profile_launch_contract "L138" "leaky_relu_kernel_l138_profile"
    "leaky_relu_kernel<T>" "leaky_relu_kernel" "T={half,float}"
    [ "half"; "float" ] 256 "k" ()

let expected_dequantize_profile_rows =
  [
    ("L026", "dequantize_block_q2_K", 64);
    ("L027", "dequantize_block_q3_K", 64);
    ("L028", "dequantize_block_q4_0", 32);
    ("L029", "dequantize_block_q4_1", 32);
    ("L030", "dequantize_block_q4_K", 32);
    ("L031", "dequantize_block_q5_K", 64);
    ("L032", "dequantize_block_q6_K", 64);
    ("L033", "dequantize_block_iq2_xxs", 32);
    ("L034", "dequantize_block_iq2_xs", 32);
    ("L035", "dequantize_block_iq2_s", 32);
    ("L036", "dequantize_block_iq3_xxs", 32);
    ("L037", "dequantize_block_iq3_s", 32);
    ("L038", "dequantize_block_iq1_s", 32);
    ("L039", "dequantize_block_iq4_nl", 32);
    ("L040", "dequantize_block_iq1_m", 32);
    ("L041", "dequantize_block_iq4_xs", 32);
    ("L042", "dequantize_block_mxfp4", 32);
    ("L043", "dequantize_block_nvfp4", 32);
  ]

let expected_dequantize_need_check_rows =
  [
    ("L024", "dequantize_block_q8_0_f16", "need_check=false", "false");
    ("L025", "dequantize_block_q8_0_f16", "need_check=true", "true");
  ]

let check_dequantize_launch_contract (row_id, kernel, block_dim_x) =
  let contract = expect_ok (LC.of_row_id row_id) in
  Alcotest.(check string) (row_id ^ " row id") row_id contract.row_id;
  Alcotest.(check string)
    (row_id ^ " manifest kernel")
    kernel contract.manifest_kernel;
  Alcotest.(check string)
    (row_id ^ " parsed kernel")
    kernel contract.parsed_kernel;
  Alcotest.(check string)
    (row_id ^ " template arg") "none" contract.template_arg;
  Alcotest.(check (list (pair string (list string))))
    (row_id ^ " finite type domains")
    []
    (LC.finite_type_domains contract);
  Alcotest.(check bool)
    (row_id ^ " is lookup row")
    true
    (lookup_contains_row row_id);
  Alcotest.(check bool)
    (row_id ^ " is not exact catalog row")
    false
    (catalog_contains_row row_id);
  let block_dim = LC.block_dim contract in
  check_dim3 (row_id ^ " blockDim") (Dim3.make ~x:block_dim_x ()) block_dim;
  let pre = LC.precondition contract in
  check_conjunct
    (row_id ^ " blockDim.x fact")
    (n_eq (Var Variable.bdim_x) (Num block_dim_x))
    pre;
  check_conjunct
    (row_id ^ " nblocks positive")
    (n_gt (nvar "nblocks") (Num 0))
    pre;
  check_conjunct
    (row_id ^ " symbolic positive gridDim.x fact")
    (n_gt (Var Variable.gdim_x) (Num 0))
    pre;
  expect_ok (LC.check_only_kernel contract (Some kernel));
  (match LC.check_block_dim contract None with
  | Ok (Some actual) ->
      check_dim3 (row_id ^ " default blockDim") block_dim actual
  | Ok None -> Alcotest.fail (row_id ^ " blockDim was not defaulted")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  match LC.check_block_dim contract (Some (Dim3.make ~x:128 ())) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 (row_id ^ " expected blockDim") block_dim expected;
      check_dim3
        (row_id ^ " actual wrong blockDim")
        (Dim3.make ~x:128 ()) actual
  | Ok _ -> Alcotest.fail (row_id ^ " wrong blockDim unexpectedly worked")
  | Error error -> Alcotest.fail (LC.error_to_string error)

let check_dequantize_need_check_launch_contract
    (row_id, kernel, template_arg, template_value) =
  let contract = expect_ok (LC.of_row_id row_id) in
  Alcotest.(check string) (row_id ^ " row id") row_id contract.row_id;
  Alcotest.(check string)
    (row_id ^ " manifest kernel")
    "dequantize_block_q8_0_f16<need_check>" contract.manifest_kernel;
  Alcotest.(check string)
    (row_id ^ " parsed kernel")
    kernel contract.parsed_kernel;
  Alcotest.(check string)
    (row_id ^ " template arg") template_arg contract.template_arg;
  Alcotest.(check (list (pair string (list string))))
    (row_id ^ " need_check domain")
    [ ("need_check", [ template_value ]) ]
    (LC.finite_type_domains contract);
  Alcotest.(check (list string))
    (row_id ^ " template domain text")
    [ "need_check={" ^ template_value ^ "}" ]
    (List.map LC.template_domain_to_string (LC.template_domains contract));
  Alcotest.(check bool)
    (row_id ^ " is lookup row")
    true
    (lookup_contains_row row_id);
  Alcotest.(check bool)
    (row_id ^ " is not exact catalog row")
    false
    (catalog_contains_row row_id);
  let block_dim = LC.block_dim contract in
  check_dim3 (row_id ^ " blockDim") (Dim3.make ~x:32 ()) block_dim;
  let pre = LC.precondition contract in
  check_conjunct
    (row_id ^ " blockDim.x fact")
    (n_eq (Var Variable.bdim_x) (Num 32))
    pre;
  check_conjunct
    (row_id ^ " nblocks positive")
    (n_gt (nvar "nblocks") (Num 0))
    pre;
  check_conjunct
    (row_id ^ " symbolic positive gridDim.x fact")
    (n_gt (Var Variable.gdim_x) (Num 0))
    pre;
  expect_ok (LC.check_only_kernel contract (Some kernel));
  (match LC.check_block_dim contract None with
  | Ok (Some actual) ->
      check_dim3 (row_id ^ " default blockDim") block_dim actual
  | Ok None -> Alcotest.fail (row_id ^ " blockDim was not defaulted")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  match LC.check_block_dim contract (Some (Dim3.make ~x:64 ())) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 (row_id ^ " expected blockDim") block_dim expected;
      check_dim3 (row_id ^ " actual wrong blockDim") (Dim3.make ~x:64 ()) actual
  | Ok _ -> Alcotest.fail (row_id ^ " wrong blockDim unexpectedly worked")
  | Error error -> Alcotest.fail (LC.error_to_string error)

let test_s486a_dequantize_need_check_launch_contract_shapes () : unit =
  List.iter check_dequantize_need_check_launch_contract
    expected_dequantize_need_check_rows

let expected_conv_cpy_profile_rows =
  [
    ( "L018",
      "conv2d_dw_kernel<float, whcn_layout>",
      "conv2d_dw_kernel",
      "T=float, layout=whcn_layout",
      [ ("T", [ "float" ]); ("layout", [ "whcn_layout" ]) ],
      256,
      "total" );
    ( "L019",
      "conv2d_dw_kernel<float, cwhn_layout>",
      "conv2d_dw_kernel",
      "T=float, layout=cwhn_layout",
      [ ("T", [ "float" ]); ("layout", [ "cwhn_layout" ]) ],
      256,
      "total" );
    ( "L020",
      "conv2d_transpose_kernel<half>",
      "conv2d_transpose_kernel",
      "T=half",
      [ ("T", [ "half" ]) ],
      256,
      "total" );
    ( "L021",
      "conv2d_transpose_kernel<float>",
      "conv2d_transpose_kernel",
      "T=float",
      [ ("T", [ "float" ]) ],
      256,
      "total" );
    ( "L046",
      "cpy_f32_q<cpy_blck_f32_q8_0, QK8_0>",
      "cpy_f32_q",
      "cpy_blck_f32_q8_0, QK8_0",
      [ ("copy_helper", [ "cpy_blck_f32_q8_0" ]); ("QK", [ "QK8_0" ]) ],
      1,
      "num_blocks" );
    ( "L048",
      "cpy_f32_q<cpy_blck_f32_q4_0, QK4_0>",
      "cpy_f32_q",
      "cpy_blck_f32_q4_0, QK4_0",
      [ ("copy_helper", [ "cpy_blck_f32_q4_0" ]); ("QK", [ "QK4_0" ]) ],
      1,
      "num_blocks" );
    ( "L050",
      "cpy_f32_q<cpy_blck_f32_q4_1, QK4_1>",
      "cpy_f32_q",
      "cpy_blck_f32_q4_1, QK4_1",
      [ ("copy_helper", [ "cpy_blck_f32_q4_1" ]); ("QK", [ "QK4_1" ]) ],
      1,
      "num_blocks" );
    ( "L052",
      "cpy_f32_q<cpy_blck_f32_q5_0, QK5_0>",
      "cpy_f32_q",
      "cpy_blck_f32_q5_0, QK5_0",
      [ ("copy_helper", [ "cpy_blck_f32_q5_0" ]); ("QK", [ "QK5_0" ]) ],
      1,
      "num_blocks" );
    ( "L054",
      "cpy_f32_q<cpy_blck_f32_q5_1, QK5_1>",
      "cpy_f32_q",
      "cpy_blck_f32_q5_1, QK5_1",
      [ ("copy_helper", [ "cpy_blck_f32_q5_1" ]); ("QK", [ "QK5_1" ]) ],
      1,
      "num_blocks" );
    ( "L056",
      "cpy_f32_q<cpy_blck_f32_iq4_nl, QK4_NL>",
      "cpy_f32_q",
      "cpy_blck_f32_iq4_nl, QK4_NL",
      [ ("copy_helper", [ "cpy_blck_f32_iq4_nl" ]); ("QK", [ "QK4_NL" ]) ],
      1,
      "num_blocks" );
    ( "L047",
      "cpy_q_f32<cpy_blck_q8_0_f32, QK8_0>",
      "cpy_q_f32",
      "cpy_blck_q8_0_f32, QK8_0",
      [ ("copy_helper", [ "cpy_blck_q8_0_f32" ]); ("QK", [ "QK8_0" ]) ],
      1,
      "num_blocks" );
    ( "L049",
      "cpy_q_f32<cpy_blck_q_f32<dequantize_q4_0, QK4_0>, QK4_0>",
      "cpy_q_f32",
      "cpy_blck_q_f32<dequantize_q4_0, QK4_0>, QK4_0",
      [
        ("copy_helper", [ "cpy_blck_q_f32<dequantize_q4_0, QK4_0>" ]);
        ("QK", [ "QK4_0" ]);
      ],
      1,
      "num_blocks" );
    ( "L051",
      "cpy_q_f32<cpy_blck_q_f32<dequantize_q4_1, QK4_1>, QK4_1>",
      "cpy_q_f32",
      "cpy_blck_q_f32<dequantize_q4_1, QK4_1>, QK4_1",
      [
        ("copy_helper", [ "cpy_blck_q_f32<dequantize_q4_1, QK4_1>" ]);
        ("QK", [ "QK4_1" ]);
      ],
      1,
      "num_blocks" );
    ( "L053",
      "cpy_q_f32<cpy_blck_q_f32<dequantize_q5_0, QK5_0>, QK5_0>",
      "cpy_q_f32",
      "cpy_blck_q_f32<dequantize_q5_0, QK5_0>, QK5_0",
      [
        ("copy_helper", [ "cpy_blck_q_f32<dequantize_q5_0, QK5_0>" ]);
        ("QK", [ "QK5_0" ]);
      ],
      1,
      "num_blocks" );
    ( "L055",
      "cpy_q_f32<cpy_blck_q_f32<dequantize_q5_1, QK5_1>, QK5_1>",
      "cpy_q_f32",
      "cpy_blck_q_f32<dequantize_q5_1, QK5_1>, QK5_1",
      [
        ("copy_helper", [ "cpy_blck_q_f32<dequantize_q5_1, QK5_1>" ]);
        ("QK", [ "QK5_1" ]);
      ],
      1,
      "num_blocks" );
  ]

let check_conv_cpy_launch_contract
    ( row_id,
      manifest_kernel,
      parsed_kernel,
      template_arg,
      expected_domains,
      block_dim_x,
      positive_param ) =
  let contract = expect_ok (LC.of_row_id row_id) in
  Alcotest.(check string) (row_id ^ " row id") row_id contract.row_id;
  Alcotest.(check string)
    (row_id ^ " manifest kernel")
    manifest_kernel contract.manifest_kernel;
  Alcotest.(check string)
    (row_id ^ " parsed kernel")
    parsed_kernel contract.parsed_kernel;
  Alcotest.(check string)
    (row_id ^ " template arg") template_arg contract.template_arg;
  Alcotest.(check (list (pair string (list string))))
    (row_id ^ " finite template domains")
    expected_domains
    (LC.finite_type_domains contract);
  Alcotest.(check bool)
    (row_id ^ " is lookup row")
    true
    (lookup_contains_row row_id);
  Alcotest.(check bool)
    (row_id ^ " is not exact catalog row")
    false
    (catalog_contains_row row_id);
  let block_dim = LC.block_dim contract in
  check_dim3 (row_id ^ " blockDim") (Dim3.make ~x:block_dim_x ()) block_dim;
  let pre = LC.precondition contract in
  check_conjunct
    (row_id ^ " blockDim.x fact")
    (n_eq (Var Variable.bdim_x) (Num block_dim_x))
    pre;
  check_conjunct
    (row_id ^ " positive param")
    (n_gt (nvar positive_param) (Num 0))
    pre;
  check_conjunct
    (row_id ^ " symbolic positive gridDim.x fact")
    (n_gt (Var Variable.gdim_x) (Num 0))
    pre;
  expect_ok (LC.check_only_kernel contract (Some parsed_kernel));
  (match LC.check_block_dim contract None with
  | Ok (Some actual) ->
      check_dim3 (row_id ^ " default blockDim") block_dim actual
  | Ok None -> Alcotest.fail (row_id ^ " blockDim was not defaulted")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  let wrong_block_dim =
    if Int.equal block_dim_x 1 then Dim3.make ~x:2 () else Dim3.make ~x:1 ()
  in
  match LC.check_block_dim contract (Some wrong_block_dim) with
  | Error (LC.Conflicting_block_dim { expected; actual }) ->
      check_dim3 (row_id ^ " expected blockDim") block_dim expected;
      check_dim3 (row_id ^ " actual wrong blockDim") wrong_block_dim actual
  | Ok _ -> Alcotest.fail (row_id ^ " wrong blockDim unexpectedly worked")
  | Error error -> Alcotest.fail (LC.error_to_string error)

let test_s486b_conv_cpy_launch_contract_shapes () : unit =
  List.iter check_conv_cpy_launch_contract expected_conv_cpy_profile_rows

let check_im2col_symbolic_block_launch_contract row_id manifest_kernel
    parsed_kernel block_dim_source =
  let contract = expect_ok (LC.of_row_id row_id) in
  Alcotest.(check string) (row_id ^ " row id") row_id contract.row_id;
  Alcotest.(check string)
    (row_id ^ " manifest kernel")
    manifest_kernel contract.manifest_kernel;
  Alcotest.(check string)
    (row_id ^ " parsed kernel")
    parsed_kernel contract.parsed_kernel;
  Alcotest.(check string)
    (row_id ^ " template arg")
    ("T={half,float}; blockDim.x=" ^ block_dim_source ^ "; gridDim=block_nums")
    contract.template_arg;
  Alcotest.(check (list (pair string (list string))))
    (row_id ^ " finite type domains")
    [ ("T", [ "half"; "float" ]) ]
    (LC.finite_type_domains contract);
  Alcotest.(check bool)
    (row_id ^ " is lookup row")
    true
    (lookup_contains_row row_id);
  Alcotest.(check bool)
    (row_id ^ " is not exact catalog row")
    false
    (catalog_contains_row row_id);
  Alcotest.(check bool)
    (row_id ^ " has symbolic blockDim")
    true
    (Option.is_none (LC.block_dim_option contract));
  let pre = LC.precondition contract in
  check_conjunct
    (row_id ^ " im2col block cap")
    (n_eq (nvar "CUDA_IM2COL_BLOCK_SIZE") (Num 256))
    pre;
  check_conjunct
    (row_id ^ " positive local extent")
    (n_gt (nvar "local_extent") (Num 0))
    pre;
  check_conjunct
    (row_id ^ " positive symbolic blockDim.x")
    (n_gt (Var Variable.bdim_x) (Num 0))
    pre;
  check_conjunct
    (row_id ^ " blockDim.x bounded by local extent")
    (n_le (Var Variable.bdim_x) (nvar "local_extent"))
    pre;
  check_conjunct
    (row_id ^ " blockDim.x bounded by macro")
    (n_le (Var Variable.bdim_x) (nvar "CUDA_IM2COL_BLOCK_SIZE"))
    pre;
  check_conjunct
    (row_id ^ " blockDim.y fact")
    (n_eq (Var Variable.bdim_y) (Num 1))
    pre;
  check_conjunct
    (row_id ^ " blockDim.z fact")
    (n_eq (Var Variable.bdim_z) (Num 1))
    pre;
  check_conjunct
    (row_id ^ " positive gridDim.x")
    (n_gt (Var Variable.gdim_x) (Num 0))
    pre;
  check_conjunct
    (row_id ^ " positive gridDim.y")
    (n_gt (Var Variable.gdim_y) (Num 0))
    pre;
  check_conjunct
    (row_id ^ " positive gridDim.z")
    (n_gt (Var Variable.gdim_z) (Num 0))
    pre;
  expect_ok (LC.check_only_kernel contract (Some parsed_kernel));
  expect_ok (LC.check_all_dims contract true);
  (match LC.check_all_dims contract false with
  | Error (LC.All_dims_required actual_row_id) ->
      Alcotest.(check string)
        (row_id ^ " all-dims required row")
        row_id actual_row_id
  | Ok () -> Alcotest.fail (row_id ^ " unexpectedly allowed default dims")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  (match LC.check_block_dim contract None with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail (row_id ^ " unexpectedly defaulted blockDim")
  | Error error -> Alcotest.fail (LC.error_to_string error));
  match LC.check_block_dim contract (Some (Dim3.make ~x:256 ())) with
  | Error (LC.Concrete_block_dim_unsupported { row_id = actual_row_id; actual })
    ->
      Alcotest.(check string)
        (row_id ^ " concrete blockDim rejected row")
        row_id actual_row_id;
      check_dim3
        (row_id ^ " rejected concrete blockDim")
        (Dim3.make ~x:256 ()) actual
  | Ok _ -> Alcotest.fail (row_id ^ " unexpectedly accepted concrete blockDim")
  | Error error -> Alcotest.fail (LC.error_to_string error)

let test_s486c_im2col_symbolic_block_launch_contract_shapes () : unit =
  check_im2col_symbolic_block_launch_contract "L074" "im2col_kernel<T>"
    "im2col_kernel" "MIN(IC_KH_KW, CUDA_IM2COL_BLOCK_SIZE)";
  check_im2col_symbolic_block_launch_contract "L075" "im2col_3d_kernel<T>"
    "im2col_3d_kernel" "MIN(IC_KD_KH_KW, CUDA_IM2COL_BLOCK_SIZE)"

let test_s485_dequantize_launch_contract_shapes () : unit =
  List.iter check_dequantize_launch_contract expected_dequantize_profile_rows

let profile_context_row_id (context : LCG.profile_launch_context) =
  let row = context.LCG.context_row in
  row.LCG.finite_type_row_id

let test_profile_launch_contexts_gate_exact_rows () : unit =
  let expected_context_rows =
    [ "L012"; "L067"; "L068"; "L076"; "L135"; "L136"; "L137"; "L138" ]
    @ List.map
        (fun (row_id, _, _, _) -> row_id)
        expected_dequantize_need_check_rows
    @ List.map
        (fun (row_id, _, _, _, _, _, _) -> row_id)
        expected_conv_cpy_profile_rows
    @ List.map (fun (row_id, _, _) -> row_id) expected_dequantize_profile_rows
  in
  Alcotest.(check (list string))
    "profile-backed exact context row ids" expected_context_rows
    (List.map profile_context_row_id LCG.profile_launch_contexts);
  List.iter
    (fun context ->
      Alcotest.(check (list string))
        (context.LCG.context_id ^ " exact blockers")
        []
        (LCG.profile_launch_context_blockers context))
    LCG.profile_launch_contexts;
  let l076 =
    expect_generator_ok (LCG.profile_launch_context_of_row_id "L076")
  in
  let rendered =
    String.concat "\n" (LCG.profile_launch_context_role_lines l076)
  in
  Alcotest.(check bool)
    "L076 renders exact grid role" true
    (string_contains rendered "grid_dim: fixed_by_launch([1,1,1])");
  Alcotest.(check bool)
    "L076 renders user-controlled count" true
    (string_contains rendered "positive:count: user_symbolic");
  Alcotest.(check bool)
    "L076 renders soundness boundary" true
    (string_contains rendered "soundness_boundary:");
  let bounded =
    {
      l076 with
      LCG.context_template_role =
        LCG.Profile_bounded
          { bound = "observed T=float"; evidence = "unit test" };
    }
  in
  Alcotest.(check bool)
    "bounded context reports blocker" true
    (LCG.profile_launch_context_blockers bounded
    |> List.exists (fun blocker ->
        string_contains blocker "template=profile_bounded"));
  let rejected =
    try
      let _ = LCG.finite_type_row_of_profile_launch_context bounded in
      false
    with Invalid_argument message ->
      string_contains message "not exact-launch-contract ready"
  in
  Alcotest.(check bool) "bounded context rejects exact row" true rejected

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

let test_template_argument_resolution_carrier_is_non_admission () : unit =
  let carrier = LC.template_argument_resolution_carrier in
  Alcotest.(check string)
    "carrier id" "s475_template_argument_resolution"
    carrier.Launch_contract_generator.template_resolution_carrier_id;
  Alcotest.(check int)
    "families attempted" 54
    carrier.Launch_contract_generator.template_resolution_families_attempted;
  Alcotest.(check int)
    "rows attempted" 79
    carrier.Launch_contract_generator.template_resolution_rows_attempted;
  Alcotest.(check int)
    "rows known" 48
    carrier.Launch_contract_generator.template_resolution_rows_known;
  Alcotest.(check int)
    "rows blocked" 31
    carrier.Launch_contract_generator.template_resolution_rows_blocked;
  Alcotest.(check int)
    "families known" 33
    carrier.Launch_contract_generator.template_resolution_families_known;
  Alcotest.(check int)
    "families blocked" 21
    carrier.Launch_contract_generator.template_resolution_families_blocked;
  Alcotest.(check (list (pair string int)))
    "family resolution counts"
    [
      ("all_rows_template_args_known", 6);
      ("not_applicable_no_template_args", 27);
      ("some_rows_still_dependent_or_unresolved", 21);
    ]
    carrier
      .Launch_contract_generator.template_resolution_family_resolution_counts;
  Alcotest.(check string)
    "solver policy" "not_solver_input"
    carrier.Launch_contract_generator.template_resolution_solver_policy;
  Alcotest.(check string)
    "admission status" "blocked_no_fresh_obligation"
    carrier.Launch_contract_generator.template_resolution_admission_status;
  Alcotest.(check bool)
    "S475 does not add L012 to exact catalog rows" false
    (catalog_contains_row "L012");
  let facts =
    {
      Launch_contract_generator.template_resolution_fact_carrier_id =
        "s475_template_argument_resolution";
      template_resolution_fact_source_ledger =
        Some
          "agent_results/rewrite/component_summaries/S475/template_arg_resolution_ledger.json";
      template_resolution_fact_input_blocker_ledger =
        Some
          "agent_results/rewrite/component_summaries/S470/launch_template_carrier_blockers.json";
      template_resolution_fact_proof_ladder_stage =
        Some "blocked_after_template_argument_resolution";
      template_resolution_fact_families_attempted = Some 54;
      template_resolution_fact_rows_attempted = Some 79;
      template_resolution_fact_rows_known = Some 48;
      template_resolution_fact_rows_blocked = Some 31;
      template_resolution_fact_families_known = Some 33;
      template_resolution_fact_families_blocked = Some 21;
      template_resolution_fact_family_resolution_counts =
        Some
          [
            ("all_rows_template_args_known", 6);
            ("not_applicable_no_template_args", 27);
            ("some_rows_still_dependent_or_unresolved", 21);
          ];
      template_resolution_fact_row_resolution_counts =
        Some
          [
            ("concrete_from_launch_expression", 18);
            ("concrete_from_local_const", 2);
            ("dependent_or_unresolved_template_args", 31);
            ("not_applicable_no_template_args", 28);
          ];
      template_resolution_fact_route_owner =
        Some "Launch_contract_generator.template_argument_resolution_carrier";
      template_resolution_fact_solver_policy = Some "not_solver_input";
      template_resolution_fact_admission_status =
        Some "blocked_no_fresh_obligation";
      template_resolution_fact_next_support_step =
        Some
          "consume S475-known families in the proof-input frontier; extract \
           host template domains for S475-blocked families";
    }
  in
  (match LC.validate_template_argument_resolution_carrier carrier facts with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let bad_facts =
    { facts with template_resolution_fact_families_known = Some 34 }
  in
  (match LC.validate_template_argument_resolution_carrier carrier bad_facts with
  | Error
      (Launch_contract_generator.Field_mismatch
         { field = "families_known"; expected = "33"; actual = "34"; _ }) ->
      ()
  | Ok () -> Alcotest.fail "mismatched S475 family count unexpectedly validated"
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let rendered =
    String.concat "\n" (LC.template_argument_resolution_carrier_lines ())
  in
  Alcotest.(check bool)
    "rendered carrier records S475 id" true
    (string_contains rendered "carrier_id: s475_template_argument_resolution");
  Alcotest.(check bool)
    "rendered carrier records non-solver status" true
    (string_contains rendered "solver_policy: not_solver_input")

let test_launch_branch_frontier_carrier_is_non_admission () : unit =
  let carrier = LC.launch_branch_frontier_carrier in
  Alcotest.(check string)
    "carrier id" "s477_launch_branch_frontier"
    carrier.Launch_contract_generator.launch_branch_carrier_id;
  Alcotest.(check int)
    "families attempted" 33
    carrier.Launch_contract_generator.launch_branch_families_attempted;
  Alcotest.(check int)
    "rows attempted" 46
    carrier.Launch_contract_generator.launch_branch_rows_attempted;
  Alcotest.(check int)
    "rows known" 45 carrier.Launch_contract_generator.launch_branch_rows_known;
  Alcotest.(check int)
    "rows blocked" 1
    carrier.Launch_contract_generator.launch_branch_rows_blocked;
  Alcotest.(check int)
    "families known" 32
    carrier.Launch_contract_generator.launch_branch_families_known;
  Alcotest.(check int)
    "families blocked" 1
    carrier.Launch_contract_generator.launch_branch_families_blocked;
  Alcotest.(check (list (pair string int)))
    "row resolution counts"
    [
      ("indirect_kernel_parameter", 1);
      ("selected_source_launch_branch_profile", 45);
    ]
    carrier.Launch_contract_generator.launch_branch_row_resolution_counts;
  Alcotest.(check (list (pair string int)))
    "next blocker counts"
    [ ("positive_shape_guard_status", 32) ]
    carrier.Launch_contract_generator.launch_branch_next_blocker_counts;
  Alcotest.(check string)
    "solver policy" "not_solver_input"
    carrier.Launch_contract_generator.launch_branch_solver_policy;
  Alcotest.(check string)
    "admission status" "blocked_no_fresh_obligation"
    carrier.Launch_contract_generator.launch_branch_admission_status;
  (match LC.of_row_id "L013" with
  | Error (LC.Unknown_row "L013") -> ()
  | Ok _ -> Alcotest.fail "S477 carrier unexpectedly admitted L013 lookup row"
  | Error error -> Alcotest.fail (LC.error_to_string error));
  let facts =
    {
      Launch_contract_generator.launch_branch_fact_carrier_id =
        "s477_launch_branch_frontier";
      launch_branch_fact_source_ledger =
        Some
          "agent_results/rewrite/component_summaries/S477/launch_branch_frontier_ledger.json";
      launch_branch_fact_input_frontier_ledger =
        Some
          "agent_results/rewrite/component_summaries/S476/s475_consumed_frontier_ledger.json";
      launch_branch_fact_proof_ladder_stage =
        Some "blocked_after_launch_branch_frontier";
      launch_branch_fact_families_attempted = Some 33;
      launch_branch_fact_rows_attempted = Some 46;
      launch_branch_fact_rows_known = Some 45;
      launch_branch_fact_rows_blocked = Some 1;
      launch_branch_fact_families_known = Some 32;
      launch_branch_fact_families_blocked = Some 1;
      launch_branch_fact_row_resolution_counts =
        Some
          [
            ("indirect_kernel_parameter", 1);
            ("selected_source_launch_branch_profile", 45);
          ];
      launch_branch_fact_family_status_counts =
        Some [ ("blocked", 1); ("known", 32) ];
      launch_branch_fact_next_blocker_counts =
        Some [ ("positive_shape_guard_status", 32) ];
      launch_branch_fact_remaining_blocker_counts =
        Some
          [ ("launch_branch_status", 1); ("positive_shape_guard_status", 33) ];
      launch_branch_fact_route_owner =
        Some "Launch_contract_generator.launch_branch_frontier_carrier";
      launch_branch_fact_solver_policy = Some "not_solver_input";
      launch_branch_fact_admission_status = Some "blocked_no_fresh_obligation";
      launch_branch_fact_next_support_step =
        Some
          "consume S477 positive-shape families in the proof-input frontier; \
           extract indirect kernel specialization for the one S477-blocked \
           helper";
    }
  in
  (match LC.validate_launch_branch_frontier_carrier carrier facts with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let bad_facts = { facts with launch_branch_fact_rows_known = Some 46 } in
  (match LC.validate_launch_branch_frontier_carrier carrier bad_facts with
  | Error
      (Launch_contract_generator.Field_mismatch
         { field = "rows_known"; expected = "45"; actual = "46"; _ }) ->
      ()
  | Ok () -> Alcotest.fail "mismatched S477 row count unexpectedly validated"
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let rendered =
    String.concat "\n" (LC.launch_branch_frontier_carrier_lines ())
  in
  Alcotest.(check bool)
    "rendered carrier records S477 id" true
    (string_contains rendered "carrier_id: s477_launch_branch_frontier");
  Alcotest.(check bool)
    "rendered carrier records indirect blocker" true
    (string_contains rendered "indirect kernel specialization");
  Alcotest.(check bool)
    "rendered carrier records non-solver status" true
    (string_contains rendered "solver_policy: not_solver_input")

let test_positive_shape_guard_carrier_is_non_admission () : unit =
  let carrier = LC.positive_shape_guard_carrier in
  Alcotest.(check string)
    "carrier id" "s478_positive_shape_guard_frontier"
    carrier.Launch_contract_generator.positive_shape_carrier_id;
  Alcotest.(check int)
    "families attempted" 33
    carrier.Launch_contract_generator.positive_shape_families_attempted;
  Alcotest.(check int)
    "rows attempted" 57
    carrier.Launch_contract_generator.positive_shape_rows_attempted;
  Alcotest.(check int)
    "families from S477" 32
    carrier.Launch_contract_generator.positive_shape_families_from_s477;
  Alcotest.(check int)
    "rows from S477" 45
    carrier.Launch_contract_generator.positive_shape_rows_from_s477;
  Alcotest.(check int)
    "preexisting families" 1
    carrier.Launch_contract_generator.positive_shape_preexisting_families;
  Alcotest.(check int)
    "preexisting rows" 12
    carrier.Launch_contract_generator.positive_shape_preexisting_rows;
  Alcotest.(check int)
    "candidate family count" 33
    (List.length
       carrier.Launch_contract_generator.positive_shape_candidate_families);
  Alcotest.(check (list (pair string int)))
    "next blocker counts"
    [ ("positive_block_grid_shape_guard", 33) ]
    carrier.Launch_contract_generator.positive_shape_next_blocker_counts;
  Alcotest.(check string)
    "solver policy" "not_solver_input"
    carrier.Launch_contract_generator.positive_shape_solver_policy;
  Alcotest.(check string)
    "admission status" "blocked_no_fresh_obligation"
    carrier.Launch_contract_generator.positive_shape_admission_status;
  let family_specs =
    List.map Launch_contract_generator.positive_shape_family_spec
      carrier.Launch_contract_generator.positive_shape_candidate_families
  in
  Alcotest.(check bool)
    "candidate families include L012 clamp family" true
    (List.exists
       (fun spec ->
         string_contains spec "F011:rows=L012"
         && string_contains spec "op_clamp_kernel")
       family_specs);
  Alcotest.(check bool)
    "candidate families include preexisting solve-tri boundary" true
    (List.exists
       (fun spec ->
         string_contains spec "F082:rows=L117,L118,L119,L120"
         && string_contains spec "preexisting_guarded_family_boundary")
       family_specs);
  Alcotest.(check bool)
    "S478 does not add L012 to exact catalog rows" false
    (catalog_contains_row "L012");
  let facts =
    {
      Launch_contract_generator.positive_shape_fact_carrier_id =
        "s478_positive_shape_guard_frontier";
      positive_shape_fact_source_ledger =
        Some
          "agent_results/rewrite/component_summaries/S477/launch_branch_frontier_ledger.json";
      positive_shape_fact_input_frontier_ledger =
        Some
          "agent_results/rewrite/component_summaries/S477/launch_branch_frontier_families.tsv";
      positive_shape_fact_proof_ladder_stage =
        Some "blocked_at_positive_shape_guard_construction";
      positive_shape_fact_families_attempted = Some 33;
      positive_shape_fact_rows_attempted = Some 57;
      positive_shape_fact_families_from_s477 = Some 32;
      positive_shape_fact_rows_from_s477 = Some 45;
      positive_shape_fact_preexisting_families = Some 1;
      positive_shape_fact_preexisting_rows = Some 12;
      positive_shape_fact_required_fact_keys =
        Some carrier.Launch_contract_generator.positive_shape_required_fact_keys;
      positive_shape_fact_candidate_family_specs = Some family_specs;
      positive_shape_fact_next_blocker_counts =
        Some [ ("positive_block_grid_shape_guard", 33) ];
      positive_shape_fact_route_owner =
        Some "Launch_contract_generator.positive_shape_guard_carrier";
      positive_shape_fact_solver_policy = Some "not_solver_input";
      positive_shape_fact_admission_status = Some "blocked_no_fresh_obligation";
      positive_shape_fact_next_support_step =
        Some
          "derive executable positive block/grid/dynamic-shared-memory guards \
           and zero-work exclusions per family before memory-event obligation \
           construction";
    }
  in
  (match LC.validate_positive_shape_guard_carrier carrier facts with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let bad_facts = { facts with positive_shape_fact_rows_attempted = Some 56 } in
  (match LC.validate_positive_shape_guard_carrier carrier bad_facts with
  | Error
      (Launch_contract_generator.Field_mismatch
         { field = "rows_attempted"; expected = "57"; actual = "56"; _ }) ->
      ()
  | Ok () -> Alcotest.fail "mismatched S478 row count unexpectedly validated"
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let rendered =
    String.concat "\n" (LC.positive_shape_guard_carrier_lines ())
  in
  Alcotest.(check bool)
    "rendered carrier records S478 id" true
    (string_contains rendered "carrier_id: s478_positive_shape_guard_frontier");
  Alcotest.(check bool)
    "rendered carrier records row count" true
    (string_contains rendered "candidate_row_count: 57");
  Alcotest.(check bool)
    "rendered carrier records non-solver status" true
    (string_contains rendered "solver_policy: not_solver_input")

let test_positive_shape_verification_carrier_records_s479 () : unit =
  let carrier = LC.positive_shape_verification_carrier in
  Alcotest.(check string)
    "carrier id" "s479_positive_shape_verification"
    carrier.Launch_contract_generator.positive_shape_verification_carrier_id;
  Alcotest.(check int)
    "families attempted" 33
    carrier
      .Launch_contract_generator.positive_shape_verification_families_attempted;
  Alcotest.(check int)
    "rows attempted" 57
    carrier.Launch_contract_generator.positive_shape_verification_rows_attempted;
  Alcotest.(check int)
    "source-slice verified families" 32
    carrier
      .Launch_contract_generator
       .positive_shape_verification_source_slice_verified_families;
  Alcotest.(check int)
    "existing guarded families" 1
    carrier
      .Launch_contract_generator
       .positive_shape_verification_existing_guarded_families;
  Alcotest.(check int)
    "blocked families" 0
    carrier
      .Launch_contract_generator.positive_shape_verification_blocked_families;
  Alcotest.(check int)
    "source-slice artifacts" 32
    carrier
      .Launch_contract_generator
       .positive_shape_verification_source_slice_artifacts;
  Alcotest.(check int)
    "exact production promotions" 0
    carrier
      .Launch_contract_generator
       .positive_shape_verification_exact_production_promotions;
  Alcotest.(check bool)
    "manifest verdict fields unchanged" false
    carrier
      .Launch_contract_generator
       .positive_shape_verification_manifest_verdict_fields_changed;
  Alcotest.(check (list (pair string int)))
    "family status counts"
    [
      ("source_slice_verified", 32);
      ("verified_existing_guarded_symbolic_family", 1);
    ]
    carrier
      .Launch_contract_generator
       .positive_shape_verification_family_status_counts;
  Alcotest.(check (list (pair string int)))
    "row status counts"
    [
      ("source_slice_verified", 45);
      ("verified_existing_guarded_symbolic_family", 12);
    ]
    carrier
      .Launch_contract_generator.positive_shape_verification_row_status_counts;
  Alcotest.(check string)
    "evidence policy" "source_slice_or_existing_guarded_family_evidence"
    carrier
      .Launch_contract_generator.positive_shape_verification_evidence_policy;
  Alcotest.(check string)
    "admission status" "not_exact_production_promotion"
    carrier
      .Launch_contract_generator.positive_shape_verification_admission_status;
  let family_specs =
    List.map Launch_contract_generator.positive_shape_family_spec
      carrier
        .Launch_contract_generator
         .positive_shape_verification_candidate_families
  in
  Alcotest.(check int) "candidate family count" 33 (List.length family_specs);
  Alcotest.(check bool)
    "source-slice families include clamp" true
    (List.exists
       (fun spec ->
         string_contains spec "F011:rows=L012"
         && string_contains spec "op_clamp_kernel")
       family_specs);
  Alcotest.(check bool)
    "existing guarded family includes solve-tri" true
    (List.exists
       (fun spec ->
         string_contains spec "F082:rows=L117,L118,L119,L120"
         && string_contains spec "solve_tri_f32_fast")
       family_specs);
  Alcotest.(check bool)
    "S479 does not add L012 to exact catalog rows" false
    (catalog_contains_row "L012");
  let facts =
    {
      Launch_contract_generator.positive_shape_verification_fact_carrier_id =
        "s479_positive_shape_verification";
      positive_shape_verification_fact_source_ledger =
        Some
          "agent_results/rewrite/component_summaries/S479/positive_shape_verification_ledger.json";
      positive_shape_verification_fact_family_ledger =
        Some
          "agent_results/rewrite/component_summaries/S479/positive_shape_verification_families.tsv";
      positive_shape_verification_fact_artifact_dir =
        Some "agent_results/rewrite/component_summaries/S479/artifacts";
      positive_shape_verification_fact_families_attempted = Some 33;
      positive_shape_verification_fact_rows_attempted = Some 57;
      positive_shape_verification_fact_source_slice_verified_families = Some 32;
      positive_shape_verification_fact_existing_guarded_families = Some 1;
      positive_shape_verification_fact_blocked_families = Some 0;
      positive_shape_verification_fact_source_slice_artifacts = Some 32;
      positive_shape_verification_fact_exact_production_promotions = Some 0;
      positive_shape_verification_fact_manifest_verdict_fields_changed =
        Some false;
      positive_shape_verification_fact_family_status_counts =
        Some
          [
            ("source_slice_verified", 32);
            ("verified_existing_guarded_symbolic_family", 1);
          ];
      positive_shape_verification_fact_row_status_counts =
        Some
          [
            ("source_slice_verified", 45);
            ("verified_existing_guarded_symbolic_family", 12);
          ];
      positive_shape_verification_fact_candidate_family_specs =
        Some family_specs;
      positive_shape_verification_fact_evidence_policy =
        Some "source_slice_or_existing_guarded_family_evidence";
      positive_shape_verification_fact_admission_status =
        Some "not_exact_production_promotion";
      positive_shape_verification_fact_next_support_step =
        Some
          "replace generated source-slice evidence with row-owned extraction \
           profiles or exact launch-contract rows before manifest promotion";
    }
  in
  (match LC.validate_positive_shape_verification_carrier carrier facts with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let bad_facts =
    { facts with positive_shape_verification_fact_blocked_families = Some 1 }
  in
  (match LC.validate_positive_shape_verification_carrier carrier bad_facts with
  | Error
      (Launch_contract_generator.Field_mismatch
         { field = "blocked_families"; expected = "0"; actual = "1"; _ }) ->
      ()
  | Ok () ->
      Alcotest.fail "mismatched S479 blocker count unexpectedly validated"
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let rendered =
    String.concat "\n" (LC.positive_shape_verification_carrier_lines ())
  in
  Alcotest.(check bool)
    "rendered carrier records S479 id" true
    (string_contains rendered "carrier_id: s479_positive_shape_verification");
  Alcotest.(check bool)
    "rendered carrier records 33 attempted" true
    (string_contains rendered "families_attempted: 33");
  Alcotest.(check bool)
    "rendered carrier records no exact promotions" true
    (string_contains rendered "exact_production_promotions: 0")

let test_positive_shape_production_promotion_carrier_records_s481 () : unit =
  let carrier = LC.positive_shape_production_promotion_carrier in
  Alcotest.(check string)
    "carrier id" "s481_positive_shape_production_profile"
    carrier.Launch_contract_generator.positive_shape_production_carrier_id;
  Alcotest.(check int)
    "families attempted" 31
    carrier
      .Launch_contract_generator.positive_shape_production_families_attempted;
  Alcotest.(check int)
    "rows attempted" 44
    carrier.Launch_contract_generator.positive_shape_production_rows_attempted;
  Alcotest.(check int)
    "extraction verified families" 1
    carrier
      .Launch_contract_generator
       .positive_shape_production_extraction_verified_families;
  Alcotest.(check int)
    "profile verified families" 31
    carrier
      .Launch_contract_generator
       .positive_shape_production_profile_verified_families;
  Alcotest.(check int)
    "source-slice-only families remaining" 0
    carrier
      .Launch_contract_generator
       .positive_shape_production_source_slice_only_remaining;
  Alcotest.(check int)
    "existing guarded families preserved" 1
    carrier
      .Launch_contract_generator
       .positive_shape_production_existing_guarded_families_preserved;
  Alcotest.(check int)
    "exact manifest promotions" 0
    carrier
      .Launch_contract_generator
       .positive_shape_production_exact_manifest_promotions;
  Alcotest.(check bool)
    "manifest verdict fields unchanged" false
    carrier
      .Launch_contract_generator
       .positive_shape_production_manifest_verdict_fields_changed;
  Alcotest.(check (list (pair string int)))
    "status counts"
    [
      ("production_backed_extraction_verified", 1);
      ("production_backed_profile_verified", 31);
      ("blocked", 0);
    ]
    carrier.Launch_contract_generator.positive_shape_production_status_counts;
  Alcotest.(check int)
    "family spec count" 32
    (List.length
       carrier.Launch_contract_generator.positive_shape_production_family_specs);
  Alcotest.(check string)
    "evidence policy"
    "production_backed_extraction_or_profile_not_full_host_header_parse"
    carrier.Launch_contract_generator.positive_shape_production_evidence_policy;
  Alcotest.(check string)
    "admission status"
    "production_backed_profiles_verified_no_manifest_promotion"
    carrier.Launch_contract_generator.positive_shape_production_admission_status;
  Alcotest.(check bool)
    "family spec names clamp row" true
    (string_contains Launch_contract_generator.l012_clamp_production_family_spec
       "F011:rows=L012");
  Alcotest.(check bool)
    "family spec records finite T domain" true
    (string_contains Launch_contract_generator.l012_clamp_production_family_spec
       "T={half,float}");
  Alcotest.(check bool)
    "family spec records block dim" true
    (string_contains Launch_contract_generator.l012_clamp_production_family_spec
       "blockDim=[256,1,1]");
  Alcotest.(check bool)
    "profile specs include first remaining family" true
    (List.exists
       (fun spec ->
         string_contains spec "F016:rows=L018,L019"
         && string_contains spec "conv2d_dw_kernel_profile.json")
       carrier.Launch_contract_generator.positive_shape_production_family_specs);
  Alcotest.(check bool)
    "profile specs include last remaining family" true
    (List.exists
       (fun spec ->
         string_contains spec "F089:rows=L138"
         && string_contains spec "leaky_relu_kernel_profile.json")
       carrier.Launch_contract_generator.positive_shape_production_family_specs);
  let l012 = expect_ok (LC.of_row_id "L012") in
  Alcotest.(check string)
    "S480 finite type lookup row" "op_clamp_kernel_l012_float"
    l012.parsed_kernel;
  let facts =
    {
      Launch_contract_generator.positive_shape_production_fact_carrier_id =
        "s481_positive_shape_production_profile";
      positive_shape_production_fact_source_ledger =
        Some
          "agent_results/rewrite/component_summaries/S481/production_profile_ledger.json";
      positive_shape_production_fact_input_verification_ledger =
        Some
          "agent_results/rewrite/component_summaries/S479/positive_shape_verification_ledger.json";
      positive_shape_production_fact_artifact_dir =
        Some "agent_results/rewrite/component_summaries/S481";
      positive_shape_production_fact_families_attempted = Some 31;
      positive_shape_production_fact_rows_attempted = Some 44;
      positive_shape_production_fact_extraction_verified_families = Some 1;
      positive_shape_production_fact_profile_verified_families = Some 31;
      positive_shape_production_fact_source_slice_only_remaining = Some 0;
      positive_shape_production_fact_existing_guarded_families_preserved =
        Some 1;
      positive_shape_production_fact_exact_manifest_promotions = Some 0;
      positive_shape_production_fact_manifest_verdict_fields_changed =
        Some false;
      positive_shape_production_fact_status_counts =
        Some
          [
            ("production_backed_extraction_verified", 1);
            ("production_backed_profile_verified", 31);
            ("blocked", 0);
          ];
      positive_shape_production_fact_family_specs =
        Some
          (Launch_contract_generator.l012_clamp_production_family_spec
         :: Launch_contract_generator.s481_profile_family_specs);
      positive_shape_production_fact_evidence_policy =
        Some
          "production_backed_extraction_or_profile_not_full_host_header_parse";
      positive_shape_production_fact_admission_status =
        Some "production_backed_profiles_verified_no_manifest_promotion";
      positive_shape_production_fact_next_support_step =
        Some
          "consume S481 production-backed profiles as exact launch-contract \
           rows or manifest-promotion inputs before changing manifest verdict \
           fields";
    }
  in
  (match
     LC.validate_positive_shape_production_promotion_carrier carrier facts
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let bad_facts =
    {
      facts with
      positive_shape_production_fact_exact_manifest_promotions = Some 1;
    }
  in
  (match
     LC.validate_positive_shape_production_promotion_carrier carrier bad_facts
   with
  | Error
      (Launch_contract_generator.Field_mismatch
         {
           field = "exact_manifest_promotions";
           expected = "0";
           actual = "1";
           _;
         }) ->
      ()
  | Ok () -> Alcotest.fail "mismatched S480 manifest-promotion count validated"
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let rendered =
    String.concat "\n" (LC.positive_shape_production_promotion_carrier_lines ())
  in
  Alcotest.(check bool)
    "rendered carrier records S481 id" true
    (string_contains rendered
       "carrier_id: s481_positive_shape_production_profile");
  Alcotest.(check bool)
    "rendered carrier records production-backed status" true
    (string_contains rendered "production_backed_profile_verified")

let test_exact_evidence_manifest_promotion_policy_carrier_records_s487 () : unit
    =
  let carrier = LC.exact_evidence_manifest_promotion_policy_carrier in
  Alcotest.(check string)
    "carrier id" "s487_exact_evidence_manifest_promotion_policy"
    carrier.Launch_contract_generator.exact_policy_carrier_id;
  Alcotest.(check int)
    "exact row count" 53
    carrier.Launch_contract_generator.exact_policy_exact_row_count;
  List.iter
    (fun row_id ->
      Alcotest.(check bool)
        ("exact policy contains " ^ row_id)
        true
        (List.mem row_id
           carrier.Launch_contract_generator.exact_policy_exact_row_ids))
    [
      "L012";
      "L018";
      "L024";
      "L026";
      "L046";
      "L067";
      "L074";
      "L075";
      "L117";
      "L118";
      "L143";
      "L146";
    ];
  Alcotest.(check string)
    "row-local manifest status" "drf_exact_row"
    carrier.Launch_contract_generator.exact_policy_row_local_manifest_status;
  Alcotest.(check string)
    "guarded family status" "guarded_symbolic_family_proof"
    carrier
      .Launch_contract_generator.exact_policy_guarded_family_manifest_status;
  Alcotest.(check (list string))
    "guarded family ids"
    [ "solve_tri_f32_fast<N,K>" ]
    carrier.Launch_contract_generator.exact_policy_guarded_family_ids;
  Alcotest.(check (list (pair string int)))
    "manifest status counts"
    [
      ("drf_exact_row", 53);
      ("guarded_symbolic_family_proof", 1);
      ("source_slice_drf_not_promotable", 0);
      ("production_profile_drf_not_promotable", 0);
    ]
    carrier.Launch_contract_generator.exact_policy_manifest_status_counts;
  Alcotest.(check (list (pair string int)))
    "blocked boundary counts"
    [
      ("host_template_or_dependent_local", 21);
      ("indirect_launch_helper", 1);
      ("unsupported_boundary", 8);
    ]
    carrier.Launch_contract_generator.exact_policy_blocked_boundary_counts;
  Alcotest.(check bool)
    "source slices alone do not promote" true
    (List.mem "source_slice_only_drf"
       carrier
         .Launch_contract_generator.exact_policy_non_promoting_evidence_classes);
  Alcotest.(check bool)
    "profiles without executable launch contracts do not promote" true
    (List.mem "production_profile_without_executable_launch_contract"
       carrier
         .Launch_contract_generator.exact_policy_non_promoting_evidence_classes);
  Alcotest.(check bool)
    "manifest verdict fields unchanged" false
    carrier
      .Launch_contract_generator.exact_policy_manifest_verdict_fields_changed;
  let facts =
    {
      Launch_contract_generator.exact_policy_fact_carrier_id =
        "s487_exact_evidence_manifest_promotion_policy";
      exact_policy_fact_input_ledgers =
        Some carrier.Launch_contract_generator.exact_policy_input_ledgers;
      exact_policy_fact_admissible_evidence_classes =
        Some
          carrier
            .Launch_contract_generator.exact_policy_admissible_evidence_classes;
      exact_policy_fact_non_promoting_evidence_classes =
        Some
          carrier
            .Launch_contract_generator
             .exact_policy_non_promoting_evidence_classes;
      exact_policy_fact_required_row_fact_keys =
        Some
          carrier.Launch_contract_generator.exact_policy_required_row_fact_keys;
      exact_policy_fact_row_local_manifest_status =
        Some
          carrier
            .Launch_contract_generator.exact_policy_row_local_manifest_status;
      exact_policy_fact_guarded_family_manifest_status =
        Some
          carrier
            .Launch_contract_generator
             .exact_policy_guarded_family_manifest_status;
      exact_policy_fact_source_slice_only_status =
        Some
          carrier
            .Launch_contract_generator.exact_policy_source_slice_only_status;
      exact_policy_fact_profile_only_status =
        Some carrier.Launch_contract_generator.exact_policy_profile_only_status;
      exact_policy_fact_exact_row_ids =
        Some carrier.Launch_contract_generator.exact_policy_exact_row_ids;
      exact_policy_fact_exact_row_count =
        Some carrier.Launch_contract_generator.exact_policy_exact_row_count;
      exact_policy_fact_guarded_family_ids =
        Some carrier.Launch_contract_generator.exact_policy_guarded_family_ids;
      exact_policy_fact_guarded_family_count =
        Some carrier.Launch_contract_generator.exact_policy_guarded_family_count;
      exact_policy_fact_manifest_status_counts =
        Some
          carrier.Launch_contract_generator.exact_policy_manifest_status_counts;
      exact_policy_fact_blocked_boundary_counts =
        Some
          carrier.Launch_contract_generator.exact_policy_blocked_boundary_counts;
      exact_policy_fact_manifest_verdict_fields_changed =
        Some
          carrier
            .Launch_contract_generator
             .exact_policy_manifest_verdict_fields_changed;
      exact_policy_fact_soundness_boundary =
        Some carrier.Launch_contract_generator.exact_policy_soundness_boundary;
      exact_policy_fact_admission_status =
        Some carrier.Launch_contract_generator.exact_policy_admission_status;
      exact_policy_fact_next_support_step =
        Some carrier.Launch_contract_generator.exact_policy_next_support_step;
    }
  in
  (match
     LC.validate_exact_evidence_manifest_promotion_policy_carrier carrier facts
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let bad_facts = { facts with exact_policy_fact_exact_row_count = Some 54 } in
  (match
     LC.validate_exact_evidence_manifest_promotion_policy_carrier carrier
       bad_facts
   with
  | Error
      (Launch_contract_generator.Field_mismatch
         { field = "exact_row_count"; expected = "53"; actual = "54"; _ }) ->
      ()
  | Ok () -> Alcotest.fail "wrong exact row count unexpectedly validated"
  | Error error ->
      Alcotest.fail (Launch_contract_generator.validation_error_to_string error));
  let rendered =
    String.concat "\n"
      (LC.exact_evidence_manifest_promotion_policy_carrier_lines ())
  in
  Alcotest.(check bool)
    "rendered S487 policy records drf_exact_row" true
    (string_contains rendered "row_local_manifest_status: drf_exact_row");
  Alcotest.(check bool)
    "rendered S487 policy records non-promotion class" true
    (string_contains rendered
       "production_profile_without_executable_launch_contract")

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

let all_family_frontier_family family_id =
  match
    List.find_opt
      (fun family -> String.equal family.LCG.all_family_id family_id)
      LCG.all_family_frontier_families
  with
  | Some family -> family
  | None -> Alcotest.fail ("missing all-family frontier family " ^ family_id)

let test_all_family_frontier_inventory_shape () : unit =
  Alcotest.(check int)
    "full family count" 95 LCG.all_family_frontier_family_count;
  Alcotest.(check int) "full row count" 146 LCG.all_family_frontier_row_count;
  let first = all_family_frontier_family "F001" in
  Alcotest.(check (list string))
    "F001 rows" [ "L001" ] first.LCG.all_family_rows;
  Alcotest.(check string)
    "F001 baseline status" "blocked"
    (LCG.all_family_frontier_status_to_string
       first.LCG.all_family_baseline_status);
  let atomic = all_family_frontier_family "F040" in
  Alcotest.(check string)
    "F040 baseline status" "unsupported"
    (LCG.all_family_frontier_status_to_string
       atomic.LCG.all_family_baseline_status);
  Alcotest.(check (option string))
    "F040 unsupported category" (Some "atomic")
    atomic.LCG.all_family_unsupported_category;
  let wkv7 = all_family_frontier_family "F095" in
  Alcotest.(check (list string))
    "F095 rows" [ "L145"; "L146" ] wkv7.LCG.all_family_rows

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
    ( "L012 finite type launch-contract shape",
      `Quick,
      test_l012_finite_type_launch_contract_shape );
    ( "L067 fill float launch-contract shape",
      `Quick,
      test_l067_fill_float_launch_contract_shape );
    ( "L068 fill half launch-contract shape",
      `Quick,
      test_l068_fill_half_launch_contract_shape );
    ( "S483 profile launch-contract shapes",
      `Quick,
      test_s483_profile_launch_contract_shapes );
    ( "S485 dequantize launch-contract shapes",
      `Quick,
      test_s485_dequantize_launch_contract_shapes );
    ( "S486-A dequantize need_check launch-contract shapes",
      `Quick,
      test_s486a_dequantize_need_check_launch_contract_shapes );
    ( "S486-B conv/cpy launch-contract shapes",
      `Quick,
      test_s486b_conv_cpy_launch_contract_shapes );
    ( "S486-C im2col symbolic block launch-contract shapes",
      `Quick,
      test_s486c_im2col_symbolic_block_launch_contract_shapes );
    ( "profile launch contexts gate exact rows",
      `Quick,
      test_profile_launch_contexts_gate_exact_rows );
    ( "solve-tri family contract guards",
      `Quick,
      test_solve_tri_family_contract_guards );
    ( "solve-tri symbolic dimension carrier",
      `Quick,
      test_solve_tri_symbolic_dimension_carrier );
    ( "host/template candidate carrier is non-admission",
      `Quick,
      test_host_template_candidate_carrier_is_non_admission );
    ( "template-argument resolution carrier is non-admission",
      `Quick,
      test_template_argument_resolution_carrier_is_non_admission );
    ( "launch-branch frontier carrier is non-admission",
      `Quick,
      test_launch_branch_frontier_carrier_is_non_admission );
    ( "positive-shape guard carrier is non-admission",
      `Quick,
      test_positive_shape_guard_carrier_is_non_admission );
    ( "positive-shape verification carrier records S479",
      `Quick,
      test_positive_shape_verification_carrier_records_s479 );
    ( "positive-shape production promotion carrier records S481",
      `Quick,
      test_positive_shape_production_promotion_carrier_records_s481 );
    ( "exact-evidence manifest promotion policy carrier records S487",
      `Quick,
      test_exact_evidence_manifest_promotion_policy_carrier_records_s487 );
    ( "all-family frontier imports full inventory",
      `Quick,
      test_all_family_frontier_inventory_shape );
    ( "solve-tri neighbors are not lookup rows",
      `Quick,
      test_unselected_solve_tri_neighbors_are_not_lookup_rows );
  ]

let () = Alcotest.run "Launch_contract" [ ("launch_contract", tests) ]
