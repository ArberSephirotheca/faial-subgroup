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

let tri_solve_tests =
  [
    ( "lower-triangular unit diagonal",
      `Quick,
      fun () ->
        check_vec_opt "lower-triangular unit diagonal"
          (Some (vec [ 1; 2; 0 ]))
          (Int_linear.tri_solve
             (mat [ [ 1; 0; 0 ]; [ 2; 1; 0 ]; [ 3; 4; 1 ] ])
             (vec [ 1; 4; 11 ])) );
    ( "non-unit diagonal",
      `Quick,
      fun () ->
        check_vec_opt "non-unit diagonal"
          (Some (vec [ 2; 3 ]))
          (Int_linear.tri_solve (mat [ [ 2; 0 ]; [ 0; 1 ] ]) (vec [ 4; 3 ])) );
    ( "inexact division",
      `Quick,
      fun () ->
        check_vec_opt "inexact division" None
          (Int_linear.tri_solve (mat [ [ 2; 0 ]; [ 0; 1 ] ]) (vec [ 3; 3 ])) );
    ( "zero diagonal",
      `Quick,
      fun () ->
        check_vec_opt "zero diagonal" None
          (Int_linear.tri_solve (mat [ [ 0; 0 ]; [ 3; 1 ] ]) (vec [ 0; 5 ])) );
  ]

let shapes =
  let open Build in
  [
    ("A[M] single atom", [ vM ], (x * vM) + y, Some [ x; y ]);
    ( "A[N][N][N] repeated-atom cube",
      [ vN; vN ],
      (i * (vN * vN)) + (j * vN) + l,
      Some [ i; j; l ] );
    ( "A[M][N+1] affine, distinct",
      [ vM; vN + Num 1 ],
      (i * (vM * (vN + Num 1))) + (j * (vN + Num 1)) + k,
      Some [ i; j; k ] );
    ( "[N; N+1] repeated-affine cube",
      [ vN; vN + Num 1 ],
      (i * (vN * (vN + Num 1))) + (j * (vN + Num 1)) + l,
      Some [ i; j; l ] );
    ( "[N*M; K+1] product and affine",
      [ vN * vM; vK + Num 1 ],
      (i * (vN * vM * (vK + Num 1))) + (j * (vK + Num 1)) + l,
      Some [ i; j; l ] );
    ( "[N^2+N] quadratic base",
      [ (vN * vN) + vN ],
      (x * ((vN * vN) + vN)) + y,
      Some [ x; y ] );
    ( "[N*M+1] bilinear base",
      [ (vN * vM) + Num 1 ],
      (x * ((vN * vM) + Num 1)) + y,
      Some [ x; y ] );
    ("[N+M] degree-tie base", [ vN + vM ], (x * (vN + vM)) + y, Some [ x; y ]);
    ( "[2N+1] non-monic base",
      [ (Num 2 * vN) + Num 1 ],
      (x * ((Num 2 * vN) + Num 1)) + y,
      Some [ x; y ] );
    ("[2N] non-monic rejects x*N+y", [ Num 2 * vN ], (x * vN) + y, None);
    ( "[N;M;K] three dimensions",
      [ vN; vM; vK ],
      (i * (vN * vM * vK)) + (j * (vM * vK)) + (l * vK) + k,
      Some [ i; j; l; k ] );
  ]

let check_delin ~radix ~expr ~expected () =
  let radix = List.map poly radix in
  match Bs_delin.delin ~radix (poly expr) with
  | None -> Alcotest.fail "expected Some, got None"
  | Some idx ->
    Alcotest.(check bool) "indices match" true
      (poly_list_eq idx.numeral (List.map poly expected))

let check_none ~radix ~expr () =
  let radix = List.map poly radix in
  match Bs_delin.delin ~radix (poly expr) with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None, got Some"

let check_same_as_cramer ~radix ~expr () =
  let radix = List.map poly radix in
  let p = poly expr in
  match (Bs_delin.delin ~radix p, Cramer.delin ~radix p) with
  | None, None -> ()
  | Some b, Some c ->
    Alcotest.(check bool) "numerals agree with Cramer" true
      (poly_list_eq b.numeral c.numeral)
  | Some _, None -> Alcotest.fail "Bs_delin says Some, Cramer says None"
  | None, Some _ -> Alcotest.fail "Bs_delin says None, Cramer says Some"

let decompose_tests =
  shapes
  |> List.map (fun (name, radix, expr, expected) ->
         ( name,
           `Quick,
           match expected with
           | Some expected -> check_delin ~radix ~expr ~expected
           | None -> check_none ~radix ~expr ))

let cross_check_tests =
  shapes
  |> List.map (fun (name, radix, expr, _) ->
         (name, `Quick, check_same_as_cramer ~radix ~expr))

let () =
  Alcotest.run "bs_delin"
    [
      ("tri_solve", tri_solve_tests);
      ("decompose", decompose_tests);
      ("cross_check", cross_check_tests);
    ]
