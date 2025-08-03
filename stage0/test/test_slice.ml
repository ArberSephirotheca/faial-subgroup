open Stage0
open OUnit2

let tests =
  "tests"
  >::: [
         ( "substring" >:: fun _ ->
           [
             (* Test the lower bound: *)
             (10, None, "");
             (4, None, "");
             (3, None, "f");
             (2, None, "df");
             (1, None, "sdf");
             (0, None, "asdf");
             (-1, None, "f");
             (-2, None, "df");
             (-3, None, "sdf");
             (-4, None, "asdf");
             (-10, None, "asdf");
             (* Test the upper bound: *)
             (0, Some (-10), "");
             (0, Some (-4), "");
             (0, Some (-3), "a");
             (0, Some (-2), "as");
             (0, Some (-1), "asd");
             (0, Some 0, "");
             (0, Some 1, "a");
             (0, Some 2, "as");
             (0, Some 3, "asd");
             (0, Some 4, "asdf");
             (0, Some 5, "asdf");
             (0, Some 10, "asdf");
             (* Assorted tests *)
             (1, Some (-1), "sd");
             (1, Some 0, "");
             (1, None, "sdf");
             (-1, None, "f");
             (-2, None, "df");
             (-4, None, "asdf");
             (1, Some (-4), "");
           ]
           |> List.iter (fun (start, finish, expected) ->
                  let given = "asdf" in
                  let s = Slice.make ~start ~finish in
                  assert_equal
                    ~msg:(Printf.sprintf "\"%s\"%s" given (Slice.repr s))
                    ~printer:(fun x -> "'" ^ x ^ "'")
                    expected (Slice.substring given s)) );
       ]

let _ = run_test_tt_main tests
