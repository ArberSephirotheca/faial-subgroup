open Protocols
open Exp
open Imp
open Scoped

(* Helper functions to reduce repetition *)
let var (name : string) : Variable.t = Variable.from_name name

let encode_assigns_testable : Encode_assigns.t Alcotest.testable =
  let pp fmt ea = Format.fprintf fmt "%s" (Encode_assigns.to_string ea) in
  let equal = ( = ) in
  Alcotest.testable pp equal

let test_encode_assigns_conversion (name : string) (scoped : Scoped.Code.t)
    (expected : Encode_assigns.t) =
  ( name,
    `Quick,
    fun () ->
      let actual = Encode_assigns.from_scoped Variable.Set.empty scoped in
      Alcotest.check encode_assigns_testable name expected actual )

let encode_assigns_tests =
  [
    (* Basic assignment encoding *)
    test_encode_assigns_conversion "encode simple assignment"
      (let id = var "id" in
       let sq = var "s_Q" in
       Code.Decl
         ( Decl.set id (n_plus (Num 32) (Var id)),
           Access (Access.write sq [ Var id ] None) ))
      (let id = var "id" in
       let sq = var "s_Q" in
       Access (Access.write sq [ n_plus (Num 32) (Var id) ] None));
    (* Variable declaration without assignment *)
    test_encode_assigns_conversion "encode variable declaration"
      (Code.Decl (Decl.unset (var "x"), Skip))
      (Encode_assigns.decl (var "x") Skip);
  ]

let all_tests = [ ("encode assigns", encode_assigns_tests) ]
let () = Alcotest.run "Imp" all_tests
