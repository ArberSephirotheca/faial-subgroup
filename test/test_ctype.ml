open Protocols
open OUnit2
open C_type

let s_int : int -> string = string_of_int
let s_string (x : string) : string = "'" ^ x ^ "'"

let s_option (f : 'a -> string) (o : 'a option) : string =
  match o with Some x -> "Some " ^ f x | None -> "None"

let s_list (f : 'a -> string) (l : 'a list) : string =
  let x = List.map f l |> String.concat ", " in
  "[" ^ x ^ "]"

let s_list_string : string list -> string = s_list s_string

let assert_o (f : 'a -> string) (o1 : 'a option) (o2 : 'a option) : unit =
  assert_equal ~printer:(s_option f) o1 o2

let tests =
  "ctype"
  >::: [
         ( "split_array_type" >:: fun _ ->
           [
             ("int *", Some ("int", "*"));
             ("int", None);
             ("unsigned int [8][8] *", Some ("unsigned int", "[8][8] *"));
             ("int [128]", Some ("int", "[128]"));
             ("float[1024]", Some ("float", "[1024]"));
           ]
           |> List.iter (fun (given, expected) ->
                  assert_o
                    (fun (x, y) -> "('" ^ x ^ "', '" ^ y ^ "')")
                    expected (split_array_type given)) );
         ( "parse_array_type_opt" >:: fun _ ->
           assert (parse_dim "[8][8]" = [ 8; 8 ]);
           assert (parse_dim "[128]" = [ 128 ]);
           () );
         ( "parse_array_type_opt_1" >:: fun _ ->
           let ex1 = "int *" in
           let ex2 = "int" in
           let ex3 = "unsigned int [8][8] *" in
           let ex4 = "int [128]" in
           let ex5 = "float[1024]" in
           let assert_eq expected given =
             assert_o s_list_string expected (parse_array_type_opt given)
           in
           assert_eq (Some [ "int" ]) ex1;
           assert_eq None ex2;
           assert_eq (Some [ "unsigned"; "int" ]) ex3;
           assert_eq (Some [ "int" ]) ex4;
           assert_eq (Some [ "float" ]) ex5;
           () );
         ( "parse_array_split_opt_2" >:: fun _ ->
           let ex1 = "int *" in
           let ex2 = "int" in
           let ex3 = "unsigned int [8][8] *" in
           let ex4 = "int [128]" in
           let ex5 = "float[1024]" in
           assert (parse_array_dim_opt ex1 = None);
           assert (parse_array_dim_opt ex2 = None);
           assert (parse_array_dim_opt ex3 = Some [ 8; 8 ]);
           assert (parse_array_dim_opt ex4 = Some [ 128 ]);
           assert_o (s_list s_int) (parse_array_dim_opt ex5) (Some [ 1024 ]);
           () );
       ]

let _ = run_test_tt_main tests
