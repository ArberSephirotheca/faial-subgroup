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

let int_linear_tests =
  [
    ("det empty" >:: fun _ -> assert_equal 1 (Int_linear.det []));
    ("det 1x1" >:: fun _ -> assert_equal 5 (Int_linear.det [ [ 5 ] ]));
    ( "det 2x2" >:: fun _ ->
      assert_equal 1 (Int_linear.det [ [ 1; 2 ]; [ 3; 7 ] ]) );
    ( "det anti-diagonal" >:: fun _ ->
      assert_equal (-1)
        (Int_linear.det [ [ 0; 0; 1 ]; [ 0; 1; 0 ]; [ 1; 0; 0 ] ]) );
    ( "solve_square integral" >:: fun _ ->
      assert_equal (Some [ 2; 1 ])
        (Int_linear.solve_square [ [ 1; 1 ]; [ 1; -1 ] ] [ 3; 1 ]) );
    ( "solve_square singular" >:: fun _ ->
      assert_equal None
        (Int_linear.solve_square [ [ 1; 1 ]; [ 2; 2 ] ] [ 1; 2 ]) );
    ( "solve_square non-integral" >:: fun _ ->
      assert_equal None
        (Int_linear.solve_square [ [ 2; 0 ]; [ 0; 1 ] ] [ 1; 3 ]) );
    ( "int_solve overdetermined consistent" >:: fun _ ->
      assert_equal (Some [ 2; 3 ])
        (Int_linear.int_solve [ [ 1; 0 ]; [ 0; 1 ]; [ 1; 1 ] ] [ 2; 3; 5 ]) );
    ( "int_solve inconsistent" >:: fun _ ->
      assert_equal None
        (Int_linear.int_solve [ [ 1; 0 ]; [ 0; 1 ]; [ 1; 1 ] ] [ 2; 3; 9 ]) );
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
    >::: [ "int_linear" >::: int_linear_tests; "decompose" >::: cramer_tests ])
