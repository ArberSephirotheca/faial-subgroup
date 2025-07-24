open OUnit2
open Protocols
open Inference
open Queries
open C_lang
module VarSet = Variable.Set

let parm_var_decl ?(ty = J_type.int) (name : string) : Expr.t =
  Ident { name = Variable.from_name name; ty; kind = ParmVar }

let tests =
  "tests"
  >::: [
         ( "variables" >:: fun _ ->
           let open C_lang.Expr in
           let assert_vars expected given =
             let expected =
               expected |> List.map Variable.from_name |> VarSet.of_list
             in
             let given = Variables.from_expr given |> Variables.to_set in
             let to_s (x : VarSet.t) =
               VarSet.elements x |> List.map Variable.name |> String.concat ", "
             in
             assert_equal ~printer:(fun x -> "[" ^ to_s x ^ "]") expected given
           in
           BinaryOperator
             {
               opcode = "+";
               lhs = parm_var_decl "x";
               rhs = IntegerLiteral 0;
               ty = J_type.int;
             }
           |> assert_vars [ "x" ];
           BinaryOperator
             {
               opcode = "+";
               lhs = IntegerLiteral 0;
               rhs = IntegerLiteral 0;
               ty = J_type.int;
             }
           |> assert_vars [];
           BinaryOperator
             {
               opcode = "+";
               lhs = parm_var_decl "x";
               rhs = parm_var_decl "y";
               ty = J_type.int;
             }
           |> assert_vars [ "x"; "y" ] );
         ( "NestedLoops.make" >:: fun _ ->
           let open NestedLoops in
           let assert_make expected given =
             assert_equal ~printer:to_string expected (make given)
           in
           let g_for ?(body = []) idx =
             Stmt.ForStmt
               {
                 init = None;
                 cond = Some (IntegerLiteral idx);
                 inc = Skip;
                 body = Stmt.from_list body;
               }
           in
           let e_for ?(body = []) ?(data = []) idx =
             For
               {
                 init = None;
                 cond = Some (IntegerLiteral idx);
                 inc = Skip;
                 data = Stmt.from_list data;
                 body;
               }
           in
           assert_make [ e_for 0 ] (g_for 0);
           assert_make
             [
               e_for 0
                 ~body:[ e_for 1 ]
                 ~data:[ ReturnStmt None; g_for 1 ~body:[] ];
             ]
             (g_for ~body:[ ReturnStmt None; g_for 1 ~body:[] ] 0) );
         ( "NestedLoops.filter_using_loop_vars" >:: fun _ ->
           let open NestedLoops in
           let given =
             [
               For
                 {
                   init = None;
                   cond = None;
                   inc = Skip;
                   data = ReturnStmt None;
                   body = [];
                 };
             ]
           in
           assert_equal [] (filter_using_loop_vars given) );
       ]

let _ = run_test_tt_main tests
