open Stage0
open Common

(* Helper functions to reduce repetition *)
let test_append_rev1 (name : string) (l1 : int list) (l2 : int list) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let actual = append_rev1 l1 l2 in
      Alcotest.(check (list int)) name expected actual )

let test_append_tr (name : string) (l1 : int list) (l2 : int list) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let actual = append_tr l1 l2 in
      Alcotest.(check (list int)) name expected actual )

let test_contains (name : string) (substring : string) (text : string) (expected : bool) =
  ( name,
    `Quick,
    fun () ->
      let actual = contains ~substring text in
      Alcotest.(check bool) name expected actual )

let test_range (name : string) ?from (to_ : int) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let actual = range ?from to_ in
      Alcotest.(check (list int)) name expected actual )

let test_substring (name : string) (start : int) (finish : int option) (text : string) (expected : string) =
  ( name,
    `Quick,
    fun () ->
      let actual = Slice.make ~start ~finish |> Slice.substring text in
      Alcotest.(check string) name expected actual )

(* Test case groups *)
let append_rev1_tests =
  [
    test_append_rev1 "reverse first list only" [ 1; 2; 3; 4 ] [] [ 4; 3; 2; 1 ];
    test_append_rev1 "reverse first, append second" [ 1; 2; 3 ] [ 4 ] [ 3; 2; 1; 4 ];
    test_append_rev1 "balanced lists" [ 1; 2 ] [ 3; 4 ] [ 2; 1; 3; 4 ];
    test_append_rev1 "single element first" [ 1 ] [ 2; 3; 4 ] [ 1; 2; 3; 4 ];
    test_append_rev1 "empty first list" [] [ 1; 2; 3; 4 ] [ 1; 2; 3; 4 ];
  ]

let append_tr_tests =
  [
    test_append_tr "append empty to non-empty" [ 1; 2; 3; 4 ] [] [ 1; 2; 3; 4 ];
    test_append_tr "append single element" [ 1; 2; 3 ] [ 4 ] [ 1; 2; 3; 4 ];
    test_append_tr "append equal length lists" [ 1; 2 ] [ 3; 4 ] [ 1; 2; 3; 4 ];
    test_append_tr "append longer second list" [ 1 ] [ 2; 3; 4 ] [ 1; 2; 3; 4 ];
    test_append_tr "append to empty list" [] [ 1; 2; 3; 4 ] [ 1; 2; 3; 4 ];
  ]

let contains_tests =
  [
    test_contains "exact match" "abc" "abc" true;
    test_contains "empty substring" "" "abc" true;
    test_contains "single char at start" "a" "abc" true;
    test_contains "single char in middle" "b" "abc" true;
    test_contains "single char at end" "c" "abc" true;
    test_contains "prefix" "ab" "abc" true;
    test_contains "suffix" "bc" "abc" true;
    test_contains "substring in longer text" "abc" "aaabc" true;
    test_contains "partial match at end" "abc" "aaab" false;
    test_contains "scattered chars" "abc" "aabab" false;
    test_contains "substring after partial match" "abc" "aababc" true;
  ]

let range_tests =
  [
    test_range "single element range" 0 [ 0 ];
    test_range "explicit from=0" ~from:0 0 [ 0 ];
    test_range "two element range" 1 [ 0; 1 ];
    test_range "custom start" ~from:1 3 [ 1; 2; 3 ];
  ]

let hashtbl_elements_tests =
  [
    ( "empty hashtable",
      `Quick,
      fun () ->
        let ht = Hashtbl.create 0 in
        let actual = Common.hashtbl_elements ht in
        Alcotest.(check (list (pair int bool))) "empty hashtable" [] actual );
    ( "single element",
      `Quick,
      fun () ->
        let ht = Hashtbl.create 0 in
        Hashtbl.add ht 0 true;
        let actual = Common.hashtbl_elements ht in
        Alcotest.(check (list (pair int bool))) "single element" [ (0, true) ] actual );
    ( "multiple elements",
      `Quick,
      fun () ->
        let ht = Hashtbl.create 0 in
        Hashtbl.add ht 0 true;
        Hashtbl.add ht 1 false;
        let elems () =
          Common.hashtbl_elements ht
          |> List.sort (fun (x, _) (y, _) -> compare x y)
        in
        Alcotest.(check (list (pair int bool))) "two elements" [ (0, true); (1, false) ] (elems ());
        Hashtbl.add ht 2 true;
        Alcotest.(check (list (pair int bool))) "three elements" [ (0, true); (1, false); (2, true) ] (elems ()) );
  ]

let hashtbl_update_tests =
  [
    ( "hashtbl_update operations",
      `Quick,
      fun () ->
        let ht = Hashtbl.create 0 in
        let elems () =
          Common.hashtbl_elements ht
          |> List.sort (fun (x, _) (y, _) -> compare x y)
        in
        hashtbl_update ht [ (0, true); (1, false); (2, true) ];
        Alcotest.(check (list (pair int bool))) "initial update" [ (0, true); (1, false); (2, true) ] (elems ());
        hashtbl_update ht [];
        Alcotest.(check (list (pair int bool))) "empty update" [ (0, true); (1, false); (2, true) ] (elems ());
        hashtbl_update ht [ (5, true); (4, false) ];
        Alcotest.(check (list (pair int bool))) "additional update" 
          [ (0, true); (1, false); (2, true); (4, false); (5, true) ] (elems ()) );
  ]

let hashtbl_from_list_tests =
  [
    ( "hashtbl_from_list operations",
      `Quick,
      fun () ->
        let elems kv =
          Common.hashtbl_from_list kv
          |> Common.hashtbl_elements
          |> List.sort (fun (x, _) (y, _) -> compare x y)
        in
        let check_equal kv = 
          Alcotest.(check (list (pair int bool))) "hashtbl_from_list" kv (elems kv) in
        check_equal [];
        check_equal [ (0, true); (2, true) ];
        check_equal [ (0, true); (1, false); (2, true) ] );
  ]

let substring_tests =
  [
    test_substring "slice from 1 to -1" 1 (Some (-1)) "asdf" "sd";
    test_substring "slice from 1 to 0" 1 (Some 0) "asdf" "";
    test_substring "slice from 1 to end" 1 None "asdf" "sdf";
    test_substring "slice from -1 to end" (-1) None "asdf" "f";
    test_substring "slice from -2 to end" (-2) None "asdf" "df";
    test_substring "slice from -4 to end" (-4) None "asdf" "asdf";
    test_substring "slice from 1 to -4" 1 (Some (-4)) "asdf" "";
    test_substring "slice from 0 to 0" 0 (Some 0) "asdf" "";
    test_substring "slice from 0 to 1" 0 (Some 1) "asdf" "a";
  ]

let highest_power_tests =
  [
    ( "highest power base 2",
      `Quick,
      fun () ->
        let actual = highest_power ~base:2 10 in
        Alcotest.(check int) "highest power of 2 <= 10" 8 actual );
  ]

let all_tests =
  [
    ("append_rev1", append_rev1_tests);
    ("append_tr", append_tr_tests);
    ("contains", contains_tests);
    ("range", range_tests);
    ("hashtbl_elements", hashtbl_elements_tests);
    ("hashtbl_update", hashtbl_update_tests);
    ("hashtbl_from_list", hashtbl_from_list_tests);
    ("substring", substring_tests);
    ("highest_power", highest_power_tests);
  ]

let () = Alcotest.run "Common" all_tests
