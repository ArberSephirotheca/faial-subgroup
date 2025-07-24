open Stage0
open OUnit2
open Common

let tests =
  "tests"
  >::: [
         ( "app_rev" >:: fun _ ->
           assert_equal [ 4; 3; 2; 1 ] (append_rev1 [ 1; 2; 3; 4 ] []);
           assert_equal [ 3; 2; 1; 4 ] (append_rev1 [ 1; 2; 3 ] [ 4 ]);
           assert_equal [ 2; 1; 3; 4 ] (append_rev1 [ 1; 2 ] [ 3; 4 ]);
           assert_equal [ 1; 2; 3; 4 ] (append_rev1 [ 1 ] [ 2; 3; 4 ]);
           assert_equal [ 1; 2; 3; 4 ] (append_rev1 [] [ 1; 2; 3; 4 ]) );
         ( "app_tr" >:: fun _ ->
           assert_equal [ 1; 2; 3; 4 ] (append_tr [ 1; 2; 3; 4 ] []);
           assert_equal [ 1; 2; 3; 4 ] (append_tr [ 1; 2; 3 ] [ 4 ]);
           assert_equal [ 1; 2; 3; 4 ] (append_tr [ 1; 2 ] [ 3; 4 ]);
           assert_equal [ 1; 2; 3; 4 ] (append_tr [ 1 ] [ 2; 3; 4 ]);
           assert_equal [ 1; 2; 3; 4 ] (append_tr [] [ 1; 2; 3; 4 ]) );
         ( "contains" >:: fun _ ->
           assert (contains ~substring:"abc" "abc");
           assert (contains ~substring:"" "abc");
           assert (contains ~substring:"a" "abc");
           assert (contains ~substring:"b" "abc");
           assert (contains ~substring:"c" "abc");
           assert (contains ~substring:"ab" "abc");
           assert (contains ~substring:"bc" "abc");
           assert (contains ~substring:"abc" "aaabc");
           assert (not (contains ~substring:"abc" "aaab"));
           assert (not (contains ~substring:"abc" "aabab"));
           assert (contains ~substring:"abc" "aababc") );
         ( "range" >:: fun _ ->
           assert_equal [ 0 ] (range 0);
           assert_equal [ 0 ] (range ~from:0 0);
           assert_equal [ 0; 1 ] (range 1);
           assert_equal [ 1; 2; 3 ] (range ~from:1 3) );
         ( "hashtbl_elements" >:: fun _ ->
           let ht = Hashtbl.create 0 in
           assert_equal (Common.hashtbl_elements ht) [];
           Hashtbl.add ht 0 true;
           assert_equal (Common.hashtbl_elements ht) [ (0, true) ];
           Hashtbl.add ht 1 false;
           let elems () =
             Common.hashtbl_elements ht
             |> List.sort (fun (x, _) (y, _) -> compare x y)
           in
           assert_equal (elems ()) [ (0, true); (1, false) ];
           Hashtbl.add ht 2 true;
           assert_equal (elems ()) [ (0, true); (1, false); (2, true) ] );
         ( "hashtbl_update" >:: fun _ ->
           let ht = Hashtbl.create 0 in
           let elems () =
             Common.hashtbl_elements ht
             |> List.sort (fun (x, _) (y, _) -> compare x y)
           in
           hashtbl_update ht [ (0, true); (1, false); (2, true) ];
           assert_equal (elems ()) [ (0, true); (1, false); (2, true) ];
           hashtbl_update ht [];
           assert_equal (elems ()) [ (0, true); (1, false); (2, true) ];
           hashtbl_update ht [ (5, true); (4, false) ];
           assert_equal (elems ())
             [ (0, true); (1, false); (2, true); (4, false); (5, true) ] );
         ( "hashtbl_from_list" >:: fun _ ->
           let elems kv =
             Common.hashtbl_from_list kv
             |> Common.hashtbl_elements
             |> List.sort (fun (x, _) (y, _) -> compare x y)
           in
           let check_equal kv = assert_equal (elems kv) kv in
           check_equal [];
           check_equal [ (0, true); (2, true) ];
           check_equal [ (0, true); (1, false); (2, true) ] );
         ( "substring" >:: fun _ ->
           [
             (1, Some (-1), "sd");
             (1, Some 0, "");
             (1, None, "sdf");
             (-1, None, "f");
             (-2, None, "df");
             (-4, None, "asdf");
             (1, Some (-4), "");
             (0, Some 0, "");
             (0, Some 1, "a");
           ]
           |> List.iter (fun (start, finish, expected) ->
                  let f =
                    match finish with None -> "?" | Some n -> string_of_int n
                  in
                  let given = "asdf" in
                  assert_equal
                    ~msg:(Printf.sprintf "\"%s\"[%d:%s]" given start f)
                    ~printer:(fun x -> "'" ^ x ^ "'")
                    expected
                    (Slice.make ~start ~finish |> Slice.substring given)) );
         ( "highest_power" >:: fun _ ->
           assert_equal (highest_power ~base:2 10) 8 ~printer:string_of_int );
       ]

let _ = run_test_tt_main tests
