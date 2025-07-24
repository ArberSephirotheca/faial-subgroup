open Stage0
open OUnit2
open Streamutil

let list_str (f : 'a -> string) (l : 'a list) : string =
  let x = List.map f l in
  match x with [] -> "[]" | _ -> "[ " ^ String.concat "," x ^ " ]"

let stream_eq (f : 'a -> string) (l : 'a list) (s : 'a stream) : unit =
  let g = to_list s in
  let msg = "expected: " ^ list_str f l ^ " given: " ^ list_str f g in
  assert_equal l g ~msg

let stream_int_eq = stream_eq string_of_int

let tests =
  "streams"
  >::: [
         ( "stream_to_list" >:: fun _ ->
           let l = [ 1; 2; 3 ] in
           stream_int_eq l (from_list l);
           stream_int_eq [] (from_list []) );
         ("stream_make" >:: fun _ -> stream_int_eq [] empty);
         ( "stream_seq" >:: fun _ ->
           let s1 = from_list [ 1; 2; 3 ] in
           let s2 = from_list [ 4; 5; 6 ] in
           let e = [ 1; 2; 3; 4; 5; 6 ] in
           let g = sequence s1 s2 |> to_list in
           assert_equal e g );
         ( "stream_take" >:: fun _ ->
           stream_int_eq [ 1; 2; 3 ]
             (from_list [ 1; 2; 3; 4 ] |> take 3 |> from_list) );
       ]

let _ = run_test_tt_main tests
