open Stage0
open Protocols
module SD = Drf.Subgroup_delinearize
module SM = Inference.Subgroup_matrix
module SS = Inference.Subgroup_source

let var name = Variable.from_name name
let nvar name = Exp.Var (var name)

let subgroup_config () =
  SM.Target_config.subgroup_size_exn 32 |> SM.Target_config.cuda_x_contiguous

let memory_effect ?(id = 0) ?(kind = SS.Ordinary_write) index :
    SS.ordinary_memory_effect =
  {
    kind;
    site = { id; source_order = id; label = "ordinary"; location = None };
    access = Access.write (var "buf") [ index ] None;
    source_conditions = [ Exp.n_ge (nvar "row") (Exp.Num 0) ];
    runtime_condition = None;
    phase = { workgroup = 0; subgroup = [] };
    target_config = subgroup_config ();
  }

let flat_index = Exp.n_plus (Exp.n_mult (nvar "row") (nvar "N")) (nvar "col")

let expect_ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (SD.error_to_string error)

let expect_single = function
  | [ memory_effect ] -> memory_effect
  | effects ->
      Alcotest.failf "expected one memory effect, got %d" (List.length effects)

let test_rewrites_access_and_attaches_axis_bound () =
  let effects =
    SD.rewrite ~enabled:true ~rewrite_access:true ~check_vacuity:false
      ~algo:Drf.Delinearize.Algo.Cramer ~weak_in_range:false
      ~globals:(Variable.Set.singleton (var "N"))
      [ memory_effect flat_index ]
    |> expect_ok
  in
  let rewritten = expect_single effects in
  Alcotest.(check (list string))
    "multidimensional index" [ "row"; "col" ]
    (List.map Exp.n_to_string rewritten.access.index);
  let conditions =
    rewritten.source_conditions |> List.map Exp.b_to_string
    |> String.concat "\n"
  in
  Alcotest.(check bool)
    "lower bound" true
    (Common.contains ~substring:"0 <= col" conditions);
  Alcotest.(check bool)
    "upper bound" true
    (Common.contains ~substring:"col < N" conditions)

let test_no_rewrite_keeps_flat_index_but_attaches_bound () =
  let effects =
    SD.rewrite ~enabled:true ~rewrite_access:false ~check_vacuity:false
      ~algo:Drf.Delinearize.Algo.Cramer ~weak_in_range:false
      ~globals:(Variable.Set.singleton (var "N"))
      [ memory_effect flat_index ]
    |> expect_ok
  in
  let rewritten = expect_single effects in
  Alcotest.(check int) "flat index count" 1 (List.length rewritten.access.index);
  Alcotest.(check bool)
    "bound added" true
    (List.exists
       (fun condition ->
         Common.contains ~substring:"col < N" (Exp.b_to_string condition))
       rewritten.source_conditions)

let test_weak_algorithm_rewrites_access () =
  let effects =
    SD.rewrite ~enabled:true ~rewrite_access:true ~check_vacuity:false
      ~algo:Drf.Delinearize.Algo.Weak ~weak_in_range:false
      ~globals:(Variable.Set.singleton (var "N"))
      [ memory_effect flat_index ]
    |> expect_ok
  in
  let rewritten = expect_single effects in
  Alcotest.(check bool)
    "weak decomposition adds an axis" true
    (List.length rewritten.access.index > 1);
  Alcotest.(check bool)
    "weak decomposition adds its bound" true
    (List.length rewritten.source_conditions > 1)

let () =
  Alcotest.run "Subgroup_delinearize"
    [
      ( "ordinary effects",
        [
          Alcotest.test_case "rewrite and bounds" `Quick
            test_rewrites_access_and_attaches_axis_bound;
          Alcotest.test_case "bounds without rewrite" `Quick
            test_no_rewrite_keeps_flat_index_but_attaches_bound;
          Alcotest.test_case "weak rewrite" `Quick
            test_weak_algorithm_rewrites_access;
        ] );
    ]
