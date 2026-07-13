open Drf

let test_ident (name : string) (input : string) (expected : bool) =
  ( name,
    `Quick,
    fun () ->
      let actual = Assume_scope.looks_like_ident input in
      Alcotest.(check bool) name expected actual )

let test_split (name : string) (input : string)
    (expected : string option * string) =
  ( name,
    `Quick,
    fun () ->
      let actual = Assume_scope.split input in
      Alcotest.(check (pair (option string) string)) name expected actual )

let ident_tests =
  [
    test_ident "plain identifier" "ckMedian" true;
    test_ident "with underscore and digits" "rope_457_2" true;
    test_ident "launch pseudo-kernel name" "rope_multi@rope_457_2" true;
    test_ident "empty string" "" false;
    test_ident "dotted member" "blockDim.x" false;
    test_ident "with space" "a b" false;
  ]

let split_tests =
  [
    test_split "no colon is a bare expression" "blockDim.x == 32"
      (None, "blockDim.x == 32");
    test_split "scoped by plain identifier" "ckMedian:blockDim.x == 16"
      (Some "ckMedian", "blockDim.x == 16");
    test_split "scoped by launch pseudo-kernel name"
      "rope_multi@rope_457_2:n_dims>0"
      (Some "rope_multi@rope_457_2", "n_dims>0");
    test_split "surrounding spaces trimmed from scope" "  ckMedian : x > 0"
      (Some "ckMedian", " x > 0");
    test_split "non-identifier prefix stays in the expression" "a b:c"
      (None, "a b:c");
    test_split "empty prefix stays in the expression" ":x>0" (None, ":x>0");
  ]

let () =
  Alcotest.run "Assume_scope"
    [ ("looks_like_ident", ident_tests); ("split", split_tests) ]
