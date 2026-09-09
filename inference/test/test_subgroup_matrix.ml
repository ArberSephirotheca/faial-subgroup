open Inference
open Protocols
module SM = Subgroup_matrix

let var (name : string) : Variable.t = Variable.from_name name

let expect_ok (type a) (result : (a, string) result) : a =
  match result with Ok value -> value | Error msg -> Alcotest.fail msg

let expect_config_ok (type a) (result : (a, SM.Target_config.error) result) : a
    =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (SM.Target_config.error_to_string error)

let read_base () : Access.t = Access.read (var "tile") [ Exp.Var (var "base") ]

let write_base () : Access.t =
  Access.write (var "dst") [ Exp.Var (var "base") ] None

let rectangular_footprint (base : Access.t) : SM.Matrix.footprint =
  SM.Matrix.rectangular ~base ~rows:(Exp.Num 16) ~cols:(Exp.Num 8)
    ~leading_dimension:(Exp.Num 32) ~layout:SM.Matrix.Row_major ~row:(var "row")
    ~col:(var "col")
  |> expect_ok

let test_site_identity_is_stable () : unit =
  let site = SM.Site.make ~label:"load-q" 7 in
  Alcotest.(check int) "site id" 7 (SM.Site.id site);
  Alcotest.(check (option string))
    "site label" (Some "load-q") (SM.Site.label_opt site);
  Alcotest.(check string)
    "site rendering" "site#7[load-q]" (SM.Site.to_string site)

let test_explicit_cuda_subgroup_config () : unit =
  let size = SM.Target_config.subgroup_size 32 |> expect_config_ok in
  let config = SM.Target_config.cuda_x_contiguous size in
  let left = SM.Target_config.cuda_thread_idx_with_suffix "_1" in
  let right = SM.Target_config.cuda_thread_idx_with_suffix "_2" in
  let same_subgroup =
    SM.Target_config.same_subgroup config ~left ~right |> expect_config_ok
  in
  let rendered = Exp.b_to_string same_subgroup in
  Alcotest.(check bool)
    "same-subgroup formula uses y coordinate" true
    (Stage0.Common.contains ~substring:"threadIdx.y_1 == threadIdx.y_2" rendered);
  Alcotest.(check bool)
    "same-subgroup formula uses z coordinate" true
    (Stage0.Common.contains ~substring:"threadIdx.z_1 == threadIdx.z_2" rendered);
  Alcotest.(check bool)
    "same-subgroup formula mentions left x coordinate" true
    (Stage0.Common.contains ~substring:"threadIdx.x_1" rendered);
  Alcotest.(check bool)
    "same-subgroup formula mentions right x coordinate" true
    (Stage0.Common.contains ~substring:"threadIdx.x_2" rendered);
  Alcotest.(check bool)
    "same-subgroup formula uses explicit x-contiguous subgroup size" true
    (Stage0.Common.contains ~substring:"/ 32" rendered)

let test_missing_subgroup_config_fails_explicitly () : unit =
  let left = SM.Target_config.cuda_thread_idx in
  let right = SM.Target_config.cuda_thread_idx_with_suffix "_other" in
  match
    SM.Target_config.same_subgroup SM.Target_config.missing_cuda ~left ~right
  with
  | Ok _ -> Alcotest.fail "missing subgroup configuration unexpectedly worked"
  | Error error ->
      Alcotest.(check bool)
        "error explains missing lane mapping" true
        (Stage0.Common.contains
           ~substring:"missing explicit subgroup lane mapping"
           (SM.Target_config.error_to_string error))

let test_rectangular_matrix_footprint_preserves_shape () : unit =
  let footprint = rectangular_footprint (read_base ()) in
  let indexed_access = SM.Matrix.indexed_access footprint in
  let bounds = SM.Matrix.bounds_condition footprint in
  let rendered_access = Access.to_string indexed_access in
  Alcotest.(check bool)
    "indexed access keeps base pointer" true
    (Stage0.Common.contains ~substring:"tile[" rendered_access);
  Alcotest.(check bool)
    "indexed access keeps row witness" true
    (Stage0.Common.contains ~substring:"row * 32" rendered_access);
  Alcotest.(check bool)
    "indexed access keeps column witness" true
    (Stage0.Common.contains ~substring:"col" rendered_access);
  Alcotest.(check bool)
    "indexed access keeps base offset" true
    (Stage0.Common.contains ~substring:"base" rendered_access);
  Alcotest.(check bool)
    "bounds mention row lower bound" true
    (Stage0.Common.contains ~substring:"row >= 0" (Exp.b_to_string bounds));
  Alcotest.(check bool)
    "bounds mention column upper bound" true
    (Stage0.Common.contains ~substring:"col < 8" (Exp.b_to_string bounds));
  let rendered_footprint = SM.Matrix.footprint_to_string footprint in
  Alcotest.(check bool)
    "footprint rendering keeps shape" true
    (Stage0.Common.contains ~substring:"footprint<16x8, ldm=32, row_major"
       rendered_footprint)

let test_matrix_collective_carries_required_memory_effect () : unit =
  let load_site = SM.Site.make ~label:"load_matrix_sync" 11 in
  let store_site = SM.Site.make ~label:"store_matrix_sync" 12 in
  let load =
    SM.Matrix.load_matrix_sync load_site (rectangular_footprint (read_base ()))
    |> expect_ok
  in
  let store =
    SM.Matrix.store_matrix_sync store_site
      (rectangular_footprint (write_base ()))
    |> expect_ok
  in
  Alcotest.(check (option string))
    "load memory effect is read" (Some "tile")
    (load.memory
    |> Option.map SM.Matrix.memory_effect_access
    |> Option.map Access.array |> Option.map Variable.name);
  Alcotest.(check (option string))
    "store memory effect is write" (Some "dst")
    (store.memory
    |> Option.map SM.Matrix.memory_effect_access
    |> Option.map Access.array |> Option.map Variable.name);
  Alcotest.(check (option string))
    "fill_fragment has no source memory effect" None
    ((SM.Matrix.fill_fragment (SM.Site.make 13)).memory
    |> Option.map SM.Matrix.memory_effect_to_string)

let test_subgroup_statement_boundary_classification () : unit =
  let site = SM.Site.make 21 in
  let subgroup_stmt = SM.Stmt.subgroup_barrier site in
  let workgroup_stmt = SM.Stmt.workgroup_barrier (SM.Site.make 22) in
  let matrix_stmt =
    let collective =
      SM.Matrix.load_matrix_sync (SM.Site.make 23)
        (rectangular_footprint (read_base ()))
      |> expect_ok
    in
    SM.Stmt.Matrix_collective collective
  in
  Alcotest.(check bool)
    "subgroup barrier is a subgroup boundary" true
    (SM.Stmt.is_subgroup_boundary subgroup_stmt);
  Alcotest.(check bool)
    "workgroup barrier is not a subgroup boundary" false
    (SM.Stmt.is_subgroup_boundary workgroup_stmt);
  Alcotest.(check bool)
    "subgroup barrier orders memory" true
    (SM.Stmt.orders_memory subgroup_stmt);
  Alcotest.(check bool)
    "workgroup barrier orders memory" true
    (SM.Stmt.orders_memory workgroup_stmt);
  Alcotest.(check bool)
    "matrix collective does not order memory" false
    (SM.Stmt.orders_memory matrix_stmt);
  Alcotest.(check bool)
    "matrix collective exposes matrix memory effect" true
    (Option.is_some (SM.Stmt.matrix_memory_effect matrix_stmt))

let test_kernel_records_target_config_without_semantic_dispatch () : unit =
  let size = SM.Target_config.subgroup_size_exn 32 in
  let config = SM.Target_config.cuda_x_contiguous size in
  let kernel =
    SM.Kernel.make ~target_config:config ~name:"wmma_probe"
      [ SM.Stmt.subgroup_barrier (SM.Site.make 30) ]
  in
  Alcotest.(check bool)
    "kernel records subgroup boundary" true
    (SM.Kernel.has_subgroup_boundary kernel);
  Alcotest.(check string)
    "kernel keeps explicit target config"
    "cuda-like(threadIdx.x-contiguous(size=32))"
    (SM.Target_config.to_string kernel.target_config)

let tests : unit Alcotest.test_case list =
  [
    ("site identity is stable", `Quick, test_site_identity_is_stable);
    ("explicit CUDA subgroup config", `Quick, test_explicit_cuda_subgroup_config);
    ( "missing subgroup config fails explicitly",
      `Quick,
      test_missing_subgroup_config_fails_explicitly );
    ( "rectangular matrix footprint preserves shape",
      `Quick,
      test_rectangular_matrix_footprint_preserves_shape );
    ( "matrix collective carries required memory effect",
      `Quick,
      test_matrix_collective_carries_required_memory_effect );
    ( "subgroup statement boundary classification",
      `Quick,
      test_subgroup_statement_boundary_classification );
    ( "kernel records target config without semantic dispatch",
      `Quick,
      test_kernel_records_target_config_without_semantic_dispatch );
  ]

let () = Alcotest.run "Subgroup_matrix" [ ("subgroup_matrix", tests) ]
