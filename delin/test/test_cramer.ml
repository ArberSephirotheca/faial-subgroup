open Protocols
open Exp

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

let vector_t : Int_linear.Vector.t Alcotest.testable =
  Alcotest.testable
    (fun ppf v -> Format.pp_print_string ppf (Int_linear.Vector.to_string v))
    Int_linear.Vector.equal

let check_vec_opt name expected actual =
  Alcotest.(check (option vector_t)) name expected actual

let int_linear_tests =
  [
    ( "det empty",
      `Quick,
      fun () -> Alcotest.(check int) "det empty" 1 (Int_linear.Matrix.det (mat [])) );
    ( "det 1x1",
      `Quick,
      fun () -> Alcotest.(check int) "det 1x1" 5 (Int_linear.Matrix.det (mat [ [ 5 ] ])) );
    ( "det 2x2",
      `Quick,
      fun () ->
        Alcotest.(check int) "det 2x2" 1
          (Int_linear.Matrix.det (mat [ [ 1; 2 ]; [ 3; 7 ] ])) );
    ( "det anti-diagonal",
      `Quick,
      fun () ->
        Alcotest.(check int) "det anti-diagonal" (-1)
          (Int_linear.Matrix.det (mat [ [ 0; 0; 1 ]; [ 0; 1; 0 ]; [ 1; 0; 0 ] ]))
    );
    ( "cramer_solve integral",
      `Quick,
      fun () ->
        check_vec_opt "cramer_solve integral"
          (Some (vec [ 2; 1 ]))
          (Int_linear.Matrix.cramer_solve
             (mat [ [ 1; 1 ]; [ 1; -1 ] ])
             (vec [ 3; 1 ])) );
    ( "cramer_solve singular",
      `Quick,
      fun () ->
        check_vec_opt "cramer_solve singular" None
          (Int_linear.Matrix.cramer_solve
             (mat [ [ 1; 1 ]; [ 2; 2 ] ])
             (vec [ 1; 2 ])) );
    ( "cramer_solve non-integral",
      `Quick,
      fun () ->
        check_vec_opt "cramer_solve non-integral" None
          (Int_linear.Matrix.cramer_solve
             (mat [ [ 2; 0 ]; [ 0; 1 ] ])
             (vec [ 1; 3 ])) );
    ( "int_solve overdetermined consistent",
      `Quick,
      fun () ->
        check_vec_opt "int_solve overdetermined consistent"
          (Some (vec [ 2; 3 ]))
          (Int_linear.int_solve
             (mat [ [ 1; 0 ]; [ 0; 1 ]; [ 1; 1 ] ])
             (vec [ 2; 3; 5 ])) );
    ( "int_solve inconsistent",
      `Quick,
      fun () ->
        check_vec_opt "int_solve inconsistent" None
          (Int_linear.int_solve
             (mat [ [ 1; 0 ]; [ 0; 1 ]; [ 1; 1 ] ])
             (vec [ 2; 3; 9 ])) );
  ]

let vector_tests =
  [
    ( "get",
      `Quick,
      fun () ->
        Alcotest.(check int) "get" 7 (Int_linear.Vector.get (vec [ 3; 7; 9 ]) 1) );
    ( "nth_opt in range",
      `Quick,
      fun () ->
        Alcotest.(check (option int))
          "nth_opt in range" (Some 9)
          (Int_linear.Vector.nth_opt (vec [ 3; 7; 9 ]) 2) );
    ( "nth_opt out of range",
      `Quick,
      fun () ->
        Alcotest.(check (option int))
          "nth_opt out of range" None
          (Int_linear.Vector.nth_opt (vec [ 3; 7; 9 ]) 3) );
    ( "select",
      `Quick,
      fun () ->
        Alcotest.(check vector_t) "select" (vec [ 3; 9 ])
          (Int_linear.Vector.select (vec [ 3; 7; 9 ]) [ 0; 2 ]) );
    ( "to_string",
      `Quick,
      fun () ->
        Alcotest.(check string) "to_string" "[3; 7; 9]"
          (Int_linear.Vector.to_string (vec [ 3; 7; 9 ])) );
  ]

let matrix_tests =
  [
    ( "rows and cols",
      `Quick,
      fun () ->
        let m = mat [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] in
        Alcotest.(check (pair int int))
          "rows and cols" (2, 3)
          (Int_linear.Matrix.rows m, Int_linear.Matrix.cols m) );
    ( "of_rows rejects ragged",
      `Quick,
      fun () ->
        Alcotest.check_raises "of_rows rejects ragged"
          (Invalid_argument "Matrix.of_rows: rows have differing lengths")
          (fun () -> ignore (mat [ [ 1; 2 ]; [ 3 ] ])) );
  ]

let check_delin ~radix ~expr ~expected () =
  let radix = List.map poly radix in
  match Cramer.delin ~radix (poly expr) with
  | None -> Alcotest.fail "expected Some, got None"
  | Some idx ->
    Alcotest.(check bool) "indices match" true
      (poly_list_eq idx.numeral (List.map poly expected))

let check_none ~radix ~expr () =
  let radix = List.map poly radix in
  match Cramer.delin ~radix (poly expr) with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None, got Some"

let cramer_tests =
  let open Build in
  [
    ( "A[M] single atom",
      `Quick,
      check_delin ~radix:[ vM ] ~expr:((x * vM) + y) ~expected:[ x; y ] );
    ( "A[N][N][N] repeated-atom cube",
      `Quick,
      check_delin
        ~radix:[ vN; vN ]
        ~expr:((i * (vN * vN)) + (j * vN) + l)
        ~expected:[ i; j; l ] );
    ( "A[M][N+1] affine, distinct",
      `Quick,
      check_delin
        ~radix:[ vM; vN + Num 1 ]
        ~expr:((i * (vM * (vN + Num 1))) + (j * (vN + Num 1)) + k)
        ~expected:[ i; j; k ] );
    ( "[N; N+1] repeated-affine cube",
      `Quick,
      check_delin
        ~radix:[ vN; vN + Num 1 ]
        ~expr:((i * (vN * (vN + Num 1))) + (j * (vN + Num 1)) + l)
        ~expected:[ i; j; l ] );
    ( "[N*M; K+1] product and affine",
      `Quick,
      check_delin
        ~radix:[ vN * vM; vK + Num 1 ]
        ~expr:((i * (vN * vM * (vK + Num 1))) + (j * (vK + Num 1)) + l)
        ~expected:[ i; j; l ] );
    ( "[N^2+N] quadratic base",
      `Quick,
      check_delin
        ~radix:[ (vN * vN) + vN ]
        ~expr:((x * ((vN * vN) + vN)) + y)
        ~expected:[ x; y ] );
    ( "[N*M+1] bilinear base",
      `Quick,
      check_delin
        ~radix:[ (vN * vM) + Num 1 ]
        ~expr:((x * ((vN * vM) + Num 1)) + y)
        ~expected:[ x; y ] );
    ( "[N+M] degree-tie base",
      `Quick,
      check_delin
        ~radix:[ vN + vM ]
        ~expr:((x * (vN + vM)) + y)
        ~expected:[ x; y ] );
    ( "[2N+1] non-monic base",
      `Quick,
      check_delin
        ~radix:[ (Num 2 * vN) + Num 1 ]
        ~expr:((x * ((Num 2 * vN) + Num 1)) + y)
        ~expected:[ x; y ] );
    ( "[2N] non-monic rejects x*N+y",
      `Quick,
      check_none ~radix:[ Num 2 * vN ] ~expr:((x * vN) + y) );
  ]

let () =
  Alcotest.run "cramer"
    [
      ("int_linear", int_linear_tests);
      ("vector", vector_tests);
      ("matrix", matrix_tests);
      ("decompose", cramer_tests);
    ]
