open Stage0

(* Helper function to reduce repetition *)
let test_slice (name : string) (start : int) (finish : int option) (text : string) (expected : string) =
  ( name,
    `Quick,
    fun () ->
      let slice = Slice.make ~start ~finish in
      let actual = Slice.substring text slice in
      Alcotest.(check string) name expected actual )

(* Test case groups *)
let lower_bound_tests =
  [
    test_slice "start beyond end" 10 None "asdf" "";
    test_slice "start at end" 4 None "asdf" "";
    test_slice "start at last char" 3 None "asdf" "f";
    test_slice "start at second-to-last" 2 None "asdf" "df";
    test_slice "start at second char" 1 None "asdf" "sdf";
    test_slice "start at beginning" 0 None "asdf" "asdf";
    test_slice "negative start -1" (-1) None "asdf" "f";
    test_slice "negative start -2" (-2) None "asdf" "df";
    test_slice "negative start -3" (-3) None "asdf" "sdf";
    test_slice "negative start -4" (-4) None "asdf" "asdf";
    test_slice "negative start beyond length" (-10) None "asdf" "asdf";
  ]

let upper_bound_tests =
  [
    test_slice "finish before start" 0 (Some (-10)) "asdf" "";
    test_slice "finish at negative length" 0 (Some (-4)) "asdf" "";
    test_slice "finish at -3" 0 (Some (-3)) "asdf" "a";
    test_slice "finish at -2" 0 (Some (-2)) "asdf" "as";
    test_slice "finish at -1" 0 (Some (-1)) "asdf" "asd";
    test_slice "finish at 0" 0 (Some 0) "asdf" "";
    test_slice "finish at 1" 0 (Some 1) "asdf" "a";
    test_slice "finish at 2" 0 (Some 2) "asdf" "as";
    test_slice "finish at 3" 0 (Some 3) "asdf" "asd";
    test_slice "finish at length" 0 (Some 4) "asdf" "asdf";
    test_slice "finish beyond length" 0 (Some 5) "asdf" "asdf";
    test_slice "finish far beyond length" 0 (Some 10) "asdf" "asdf";
  ]

let assorted_tests =
  [
    test_slice "middle slice" 1 (Some (-1)) "asdf" "sd";
    test_slice "start to zero" 1 (Some 0) "asdf" "";
    test_slice "from middle to end" 1 None "asdf" "sdf";
    test_slice "last char with negative" (-1) None "asdf" "f";
    test_slice "last two chars" (-2) None "asdf" "df";
    test_slice "negative start full string" (-4) None "asdf" "asdf";
    test_slice "middle to negative beyond" 1 (Some (-4)) "asdf" "";
  ]

let all_tests =
  [
    ("lower bound", lower_bound_tests);
    ("upper bound", upper_bound_tests);
    ("assorted", assorted_tests);
  ]

let () = Alcotest.run "Slice" all_tests
