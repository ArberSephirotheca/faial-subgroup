open OUnit2
open Inference
open D_lang

let tests =
  "dlang"
  >::: [
         ( "last + skip_last" >:: fun _ ->
           let open Stmt in
           let s = Stmt.from_list [ BreakStmt; GotoStmt; ContinueStmt ] in
           assert_equal (last s) ContinueStmt;
           assert_equal (skip_last s) (Stmt.from_list [ BreakStmt; GotoStmt ])
         );
       ]

let _ = run_test_tt_main tests
