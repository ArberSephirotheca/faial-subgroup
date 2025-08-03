open Stage0
open Py

(* Helper functions to reduce repetition *)
let test_range (name : string) ?start ?step (stop : int) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let actual = range ?start ?step stop in
      Alcotest.(check (list int)) name expected actual )

let basic_range_tests =
  [
    test_range "empty range (0)" 0 [];
    test_range "empty range (start=0, stop=0)" ~start:0 0 [];
    test_range "single element" 1 [ 0 ];
    test_range "simple range" ~start:1 3 [ 1; 2 ];
  ]

let empty_range_tests =
  [
    test_range "empty range (start > stop)" ~start:5 0 [];
    test_range "empty range (negative step, start < stop)" ~start:1 ~step:(-1) 3 [];
    test_range "empty range (positive step, start > stop)" ~start:3 ~step:1 1 [];
  ]

let negative_step_tests =
  [
    test_range "negative step (5 down to 0)" ~start:5 ~step:(-1) 0 [ 5; 4; 3; 2; 1 ];
    test_range "negative step with step=-2" ~start:5 ~step:(-2) 0 [ 5; 3; 1 ];
  ]

let all_tests =
  [
    ("basic range", basic_range_tests);
    ("empty ranges", empty_range_tests);
    ("negative step", negative_step_tests);
  ]

let () = Alcotest.run "Py" all_tests
