open Stage0
open OUnit2
open Py

let s_list (f : 'a -> string) (l : 'a list) : string =
  let x = List.map f l |> String.concat ", " in
  "[" ^ x ^ "]"

let s_list_int : int list -> string = s_list string_of_int

let assert_list (l1 : int list) (l2 : int list) : unit =
  assert_equal l1 l2 ~printer:s_list_int

let tests =
  "tests"
  >::: [
         ( "range" >:: fun _ ->
           assert_list [] (range 0);
           assert_list [] (range ~start:0 0);
           assert_list [ 0 ] (range 1);
           assert_list [ 1; 2 ] (range ~start:1 3);
           assert_list [] (range ~start:5 0);
           assert_list [ 5; 4; 3; 2; 1 ] (range ~start:5 ~step:(-1) 0);
           assert_list [] (range ~start:1 ~step:(-1) 3);
           assert_list [] (range ~start:3 ~step:1 1);
           assert_list [ 5; 3; 1 ] (range ~start:5 ~step:(-2) 0) );
       ]

let _ = run_test_tt_main tests
