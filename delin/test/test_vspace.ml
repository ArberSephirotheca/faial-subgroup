open OUnit2

let vN = Poly.parameter "N"
let vM = Poly.parameter "M"
let ( +: ) = Poly.( + )
let ( *: ) = Poly.( * )

let n2_plus_n = (vN *: vN) +: vN
let n_plus_1 = vN +: Poly.of_int 1

let poly_eq (a : Poly.t) (b : Poly.t) : bool = Poly.compare a b = 0

let assert_div ~num ~den ~expected =
  match Vspace.divide_exact num den with
  | Some q -> assert_bool "quotient mismatch" (poly_eq q expected)
  | None -> assert_failure "expected Some, got None"

let assert_no_div ~num ~den =
  match Vspace.divide_exact num den with
  | None -> ()
  | Some _ -> assert_failure "expected None, got Some"

let divide_exact_tests =
  [
    ("N^2+N / (N+1) = N" >:: fun _ ->
      assert_div ~num:n2_plus_n ~den:n_plus_1 ~expected:vN);
    ("N^2+N / N = N+1" >:: fun _ ->
      assert_div ~num:n2_plus_n ~den:vN ~expected:n_plus_1);
    ("N+1 / N not exact" >:: fun _ -> assert_no_div ~num:n_plus_1 ~den:vN);
    ("N*M / N = M" >:: fun _ ->
      assert_div ~num:(vN *: vM) ~den:vN ~expected:vM);
    ("N / (N+1) not exact" >:: fun _ -> assert_no_div ~num:vN ~den:n_plus_1);
  ]

let degree_tests =
  [
    ("deg(N^2+N) = 2" >:: fun _ ->
      assert_equal ~printer:string_of_int 2 (Vspace.degree n2_plus_n));
    ("deg(N+1) = 1" >:: fun _ ->
      assert_equal ~printer:string_of_int 1 (Vspace.degree n_plus_1));
    ("deg(1) = 0" >:: fun _ ->
      assert_equal ~printer:string_of_int 0 (Vspace.degree (Poly.of_int 7)));
  ]

let assert_rank expected polys =
  assert_equal ~printer:string_of_int expected
    (List.length (Vspace.column_space polys))

let column_space_tests =
  [
    ("place-value chain spans rank 3" >:: fun _ ->
      assert_rank 3 [ n2_plus_n; n_plus_1; Poly.of_int 1 ]);
    ("dependent {N, 2N, N+1} spans rank 2" >:: fun _ ->
      assert_rank 2 [ vN; Poly.scale 2 vN; n_plus_1 ]);
    ("{N, M, N*M} spans rank 3" >:: fun _ ->
      assert_rank 3 [ vN; vM; vN *: vM ]);
  ]

let () =
  run_test_tt_main
    ("vspace"
    >::: [
           "divide_exact" >::: divide_exact_tests;
           "degree" >::: degree_tests;
           "column_space" >::: column_space_tests;
         ])
