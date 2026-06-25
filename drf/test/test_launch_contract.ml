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

let nvar name = Var (Variable.from_name name)

let has_conjunct expected actual =
  List.exists (( = ) expected) (b_and_split actual)

let check_conjunct name expected actual =
  Alcotest.(check bool) name true (has_conjunct expected actual)

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
    List.map
      (fun row -> row.Launch_contract_rows.row_id)
      Launch_contract_rows.all
  in
  let contract_ids = List.map (fun row -> row.LC.row_id) LC.all in
  Alcotest.(check (list string)) "row ids" catalog_ids contract_ids

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
    ( "conflicting param fails closed",
      `Quick,
      test_conflicting_param_fails_closed );
    ( "apply contract adds global precondition",
      `Quick,
      test_apply_contract_adds_global_precondition );
    ("kernel mismatch fails closed", `Quick, test_kernel_mismatch_fails_closed);
  ]

let () = Alcotest.run "Launch_contract" [ ("launch_contract", tests) ]
