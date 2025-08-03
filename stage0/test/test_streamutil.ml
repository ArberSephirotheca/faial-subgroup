open Stage0
open Streamutil

(* Helper functions to reduce repetition *)
let test_stream_conversion (name : string) (input : int list) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let actual = from_list input |> to_list in
      Alcotest.(check (list int)) name expected actual )

let test_empty_stream (name : string) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let actual = empty |> to_list in
      Alcotest.(check (list int)) name expected actual )

let test_sequence (name : string) (l1 : int list) (l2 : int list) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let s1 = from_list l1 in
      let s2 = from_list l2 in
      let actual = sequence s1 s2 |> to_list in
      Alcotest.(check (list int)) name expected actual )

let test_take (name : string) (input : int list) (n : int) (expected : int list) =
  ( name,
    `Quick,
    fun () ->
      let actual = from_list input |> take n |> from_list |> to_list in
      Alcotest.(check (list int)) name expected actual )

(* Test case groups *)
let stream_conversion_tests =
  [
    test_stream_conversion "non-empty list" [ 1; 2; 3 ] [ 1; 2; 3 ];
    test_stream_conversion "empty list" [] [];
  ]

let empty_stream_tests =
  [
    test_empty_stream "empty stream" [];
  ]

let sequence_tests =
  [
    test_sequence "concatenate two streams" [ 1; 2; 3 ] [ 4; 5; 6 ] [ 1; 2; 3; 4; 5; 6 ];
  ]

let take_tests =
  [
    test_take "take first 3 elements" [ 1; 2; 3; 4 ] 3 [ 1; 2; 3 ];
  ]

let all_tests =
  [
    ("stream conversion", stream_conversion_tests);
    ("empty stream", empty_stream_tests);
    ("sequence", sequence_tests);
    ("take", take_tests);
  ]

let () = Alcotest.run "Streamutil" all_tests
