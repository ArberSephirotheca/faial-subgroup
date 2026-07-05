open Protocols
open Exp
open OUnit2

module Build = struct
  let var x = Var (Variable.from_name x)
  let ( + ) a b = Binary (Plus Signedness.Signed, a, b)
  let ( * ) a b = Binary (Mult Signedness.Signed, a, b)
  let i = var "i"
  let j = var "j"
  let l = var "l"
  let k = var "k"
  let x = var "x"
  let y = var "y"
  let vM = var "M"
  let vN = var "N"
  let vK = var "K"
end

let globals =
  Variable.Set.of_list ([ "M"; "N"; "K" ] |> List.map Variable.from_name)

let poly (e : nexp) : Poly.t = Poly.from_nexp ~globals e

let poly_list_eq (a : Poly.t list) (b : Poly.t list) : bool =
  List.length a = List.length b
  && List.for_all2 (fun x y -> Poly.compare x y = 0) a b

let vec = Int_linear.Vector.of_list

let mat (rows : int list list) : Int_linear.Matrix.t =
  Int_linear.Matrix.of_rows (List.map vec rows)

let vec_opt_printer = function
  | None -> "None"
  | Some v -> Int_linear.Vector.to_string v

let assert_vec_opt expected actual =
  assert_equal ~cmp:(Option.equal Int_linear.Vector.equal)
    ~printer:vec_opt_printer expected actual

let int_linear_tests =
  [
    ("det empty" >:: fun _ -> assert_equal 1 (Int_linear.Matrix.det (mat [])));
    ( "det 1x1" >:: fun _ ->
      assert_equal 5 (Int_linear.Matrix.det (mat [ [ 5 ] ])) );
    ( "det 2x2" >:: fun _ ->
      assert_equal 1 (Int_linear.Matrix.det (mat [ [ 1; 2 ]; [ 3; 7 ] ])) );
    ( "det anti-diagonal" >:: fun _ ->
      assert_equal (-1)
        (Int_linear.Matrix.det (mat [ [ 0; 0; 1 ]; [ 0; 1; 0 ]; [ 1; 0; 0 ] ]))
    );
    ( "cramer_solve integral" >:: fun _ ->
      assert_vec_opt
        (Some (vec [ 2; 1 ]))
        (Int_linear.Matrix.cramer_solve
           (mat [ [ 1; 1 ]; [ 1; -1 ] ])
           (vec [ 3; 1 ])) );
    ( "cramer_solve singular" >:: fun _ ->
      assert_vec_opt None
        (Int_linear.Matrix.cramer_solve
           (mat [ [ 1; 1 ]; [ 2; 2 ] ])
           (vec [ 1; 2 ])) );
    ( "cramer_solve non-integral" >:: fun _ ->
      assert_vec_opt None
        (Int_linear.Matrix.cramer_solve
           (mat [ [ 2; 0 ]; [ 0; 1 ] ])
           (vec [ 1; 3 ])) );
    ( "int_solve overdetermined consistent" >:: fun _ ->
      assert_vec_opt
        (Some (vec [ 2; 3 ]))
        (Int_linear.int_solve
           (mat [ [ 1; 0 ]; [ 0; 1 ]; [ 1; 1 ] ])
           (vec [ 2; 3; 5 ])) );
    ( "int_solve inconsistent" >:: fun _ ->
      assert_vec_opt None
        (Int_linear.int_solve
           (mat [ [ 1; 0 ]; [ 0; 1 ]; [ 1; 1 ] ])
           (vec [ 2; 3; 9 ])) );
  ]

let vector_tests =
  [
    ( "get" >:: fun _ ->
      assert_equal 7 (Int_linear.Vector.get (vec [ 3; 7; 9 ]) 1) );
    ( "nth_opt in range" >:: fun _ ->
      assert_equal (Some 9) (Int_linear.Vector.nth_opt (vec [ 3; 7; 9 ]) 2) );
    ( "nth_opt out of range" >:: fun _ ->
      assert_equal None (Int_linear.Vector.nth_opt (vec [ 3; 7; 9 ]) 3) );
    ( "select" >:: fun _ ->
      assert_bool "select picks positions"
        (Int_linear.Vector.equal (vec [ 3; 9 ])
           (Int_linear.Vector.select (vec [ 3; 7; 9 ]) [ 0; 2 ])) );
    ( "to_string" >:: fun _ ->
      assert_equal "[3; 7; 9]" (Int_linear.Vector.to_string (vec [ 3; 7; 9 ])) );
  ]

let matrix_tests =
  [
    ( "rows and cols" >:: fun _ ->
      let m = mat [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] in
      assert_equal (2, 3) (Int_linear.Matrix.rows m, Int_linear.Matrix.cols m) );
    ( "of_rows rejects ragged" >:: fun _ ->
      assert_raises
        (Invalid_argument "Matrix.of_rows: rows have differing lengths")
        (fun () -> mat [ [ 1; 2 ]; [ 3 ] ]) );
  ]

let assert_delin ~radix ~expr ~expected =
  let radix = List.map poly radix in
  match Cramer.delin ~radix (poly expr) with
  | None -> assert_failure "expected Some, got None"
  | Some idx ->
    assert_bool "indices mismatch"
      (poly_list_eq idx.numeral (List.map poly expected))

let assert_none ~radix ~expr =
  let radix = List.map poly radix in
  match Cramer.delin ~radix (poly expr) with
  | None -> ()
  | Some _ -> assert_failure "expected None, got Some"

let cramer_tests =
  let open Build in
  [
    ( "A[M] single atom" >:: fun _ ->
      assert_delin ~radix:[ vM ] ~expr:((x * vM) + y) ~expected:[ x; y ] );
    ( "A[N][N][N] repeated-atom cube" >:: fun _ ->
      assert_delin
        ~radix:[ vN; vN ]
        ~expr:((i * (vN * vN)) + (j * vN) + l)
        ~expected:[ i; j; l ] );
    ( "A[M][N+1] affine, distinct" >:: fun _ ->
      assert_delin
        ~radix:[ vM; vN + Num 1 ]
        ~expr:((i * (vM * (vN + Num 1))) + (j * (vN + Num 1)) + k)
        ~expected:[ i; j; k ] );
    ( "[N; N+1] repeated-affine cube" >:: fun _ ->
      assert_delin
        ~radix:[ vN; vN + Num 1 ]
        ~expr:((i * (vN * (vN + Num 1))) + (j * (vN + Num 1)) + l)
        ~expected:[ i; j; l ] );
    ( "[N*M; K+1] product and affine" >:: fun _ ->
      assert_delin
        ~radix:[ vN * vM; vK + Num 1 ]
        ~expr:((i * (vN * vM * (vK + Num 1))) + (j * (vK + Num 1)) + l)
        ~expected:[ i; j; l ] );
    ( "[N^2+N] quadratic base" >:: fun _ ->
      assert_delin
        ~radix:[ (vN * vN) + vN ]
        ~expr:((x * ((vN * vN) + vN)) + y)
        ~expected:[ x; y ] );
    ( "[N*M+1] bilinear base" >:: fun _ ->
      assert_delin
        ~radix:[ (vN * vM) + Num 1 ]
        ~expr:((x * ((vN * vM) + Num 1)) + y)
        ~expected:[ x; y ] );
    ( "[N+M] degree-tie base" >:: fun _ ->
      assert_delin
        ~radix:[ vN + vM ]
        ~expr:((x * (vN + vM)) + y)
        ~expected:[ x; y ] );
    ( "[2N+1] non-monic base" >:: fun _ ->
      assert_delin
        ~radix:[ (Num 2 * vN) + Num 1 ]
        ~expr:((x * ((Num 2 * vN) + Num 1)) + y)
        ~expected:[ x; y ] );
    ( "[2N] non-monic rejects x*N+y" >:: fun _ ->
      assert_none ~radix:[ Num 2 * vN ] ~expr:((x * vN) + y) );
  ]

let () =
  run_test_tt_main
    ("cramer"
    >::: [
           "int_linear" >::: int_linear_tests;
           "vector" >::: vector_tests;
           "matrix" >::: matrix_tests;
           "decompose" >::: cramer_tests;
         ])
