open Protocols
open Exp
open Imp
open Scoped

(* Helper functions to reduce repetition *)
let var (name : string) : Variable.t = Variable.from_name name

(* Alcotest testable types *)
let scoped_testable : Scoped.Code.t Alcotest.testable =
  let pp fmt s = Format.fprintf fmt "%s" (Scoped.Code.to_string s) in
  let equal = ( = ) in
  Alcotest.testable pp equal

(* Test helper functions *)
let test_scoped_conversion (name : string) (stmt : Stmt.t)
    (expected : Scoped.Code.t) =
  ( name,
    `Quick,
    fun () ->
      let _, actual = Scoped.Code.from_stmt (Params.empty, stmt) in
      Alcotest.check scoped_testable name expected actual )

(* Test data using helper functions - following test_exp_parser.ml style *)
let scoped_conversion_tests =
  [
    (* Simple increment and write *)
    test_scoped_conversion "increment and write"
      (let id = var "id" in
       let sq = var "s_Q" in
       let wr =
         Imp.Stmt.(Write { path = Protocols.Field_path.parse (sq); index = [ Var id ]; payload = None; guard = None })
       in
       let inc (x : Variable.t) =
         Imp.Stmt.decl_set x (n_plus (Num 32) (Var x))
       in
       Stmt.from_list [ inc id; wr ])
      (let id = var "id" in
       let sq = var "s_Q" in
       Code.Decl
         ( Decl.set id (n_plus (Num 32) (Var id)),
           Access (Mem_access.write sq [ Var id ] None) ));
    (* Simple variable declaration *)
    test_scoped_conversion "simple variable declaration"
      (Stmt.decl_unset (var "x"))
      (Code.Decl (Decl.unset (var "x"), Skip));
  ]

let all_tests = [ ("scoped conversions", scoped_conversion_tests) ]
let () = Alcotest.run "scoped" all_tests
