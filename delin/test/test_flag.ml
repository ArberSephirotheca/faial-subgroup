open Protocols
open OUnit2

let vN = Poly.parameter "N"
let vM = Poly.parameter "M"
let vK = Poly.parameter "K"
let ( +: ) = Poly.( + )
let ( *: ) = Poly.( * )
let one = Poly.of_int 1

let subs = [ Poly.induction "x"; Poly.induction "y"; Poly.induction "z";
             Poly.induction "w" ]

let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs

(* canonical access for a radix: sum_k subscript_k * place_value_k *)
let access (radix : Poly.t list) : Poly.t =
  Poly.dot (take (List.length radix + 1) subs) (Subscript.place_values radix)

let check_shape ~name ~radix ~arity =
  name >:: fun _ ->
    let acc = access radix in
    match Flag.infer ~globals:Variable.Set.empty [ acc ] |> Seq.uncons with
    | None -> assert_failure "Flag.infer produced no radix"
    | Some (r, _) ->
      assert_equal ~printer:string_of_int ~msg:"arity" arity (List.length r);
      (match Cramer.delin ~radix:r acc with
       | Some _ -> ()
       | None -> assert_failure "Cramer rejected the inferred radix")

let shape_tests =
  [
    check_shape ~name:"[N; M]" ~radix:[ vN; vM ] ~arity:2;
    check_shape ~name:"[N; N+1] repeated affine" ~radix:[ vN; vN +: one ] ~arity:2;
    check_shape ~name:"[N*M; K+1] product+affine"
      ~radix:[ vN *: vM; vK +: one ] ~arity:2;
    check_shape ~name:"[N+M] degree-tie" ~radix:[ vN +: vM ] ~arity:1;
    check_shape ~name:"[N*M+1] bilinear" ~radix:[ (vN *: vM) +: one ] ~arity:1;
    check_shape ~name:"[N^2+N] quadratic" ~radix:[ (vN *: vN) +: vN ] ~arity:1;
    check_shape ~name:"[2N+1] non-monic" ~radix:[ Poly.scale 2 vN +: one ] ~arity:1;
  ]

(* Rank-deficient: a single access that only exercises one inner stride.
   Flag degrades to the coarser shape, which still decodes. *)
let degraded_test =
  "rank-deficient degrades to a valid coarser radix" >:: fun _ ->
  let acc = (Poly.induction "x" *: (vN *: vM)) +: Poly.induction "y" in
  match Flag.infer ~globals:Variable.Set.empty [ acc ] |> Seq.uncons with
  | None -> assert_failure "no radix"
  | Some (r, _) ->
    assert_equal ~printer:string_of_int 1 (List.length r);
    (match Cramer.delin ~radix:r acc with
     | Some _ -> ()
     | None -> assert_failure "Cramer rejected the degraded radix")

let () =
  run_test_tt_main
    ("flag" >::: [ "shapes" >::: shape_tests; "degrade" >::: [ degraded_test ] ])
