open OUnit2
open Protocols
open Protocols.Gen_z3
open Protocols.Exp

let tests =
  "test_gen_z3"
  >::: [
         ( "optimize_expr_simple" >:: fun _ ->
           (* Test maximizing a simple constant *)
           let result =
             IntGen.optimize_expr Optimizer.Strategy.Maximize (Num 42)
           in
           assert_equal (Ok 42) result );
         ( "optimize_expr_minimize" >:: fun _ ->
           (* Test minimizing a constant *)
           let result =
             IntGen.optimize_expr Optimizer.Strategy.Minimize (Num 10)
           in
           assert_equal (Ok 10) result );
         ( "optimize_expr_with_variable" >:: fun _ ->
           (* Test maximizing a variable x with constraint x <= 5 *)
           let x = Variable.from_name "x" in
           let pre = NRel (N_rel.Le, Var x, Num 5) in
           let result =
             IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize (Var x)
           in
           assert_equal (Ok 5) result );
         ( "optimize_expr_arithmetic" >:: fun _ ->
           (* Test maximizing x + 3 where x <= 2 *)
           let x = Variable.from_name "x" in
           let pre = NRel (N_rel.Le, Var x, Num 2) in
           let expr = Binary (N_binary.Plus, Var x, Num 3) in
           let result =
             IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize expr
           in
           assert_equal (Ok 5) result );
         ( "optimize_expr_minimize_with_constraint" >:: fun _ ->
           (* Test minimizing x where x >= 10 *)
           let x = Variable.from_name "x" in
           let pre = NRel (N_rel.Ge, Var x, Num 10) in
           let result =
             IntGen.optimize_expr ~pre Optimizer.Strategy.Minimize (Var x)
           in
           assert_equal (Ok 10) result );
         ( "optimize_expr_unsat" >:: fun _ ->
           (* Test with contradictory constraints: x > 5 and x < 5 *)
           let x = Variable.from_name "x" in
           let pre =
             BRel
               ( B_rel.BAnd,
                 NRel (N_rel.Gt, Var x, Num 5),
                 NRel (N_rel.Lt, Var x, Num 5) )
           in
           let result =
             IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize (Var x)
           in
           assert_equal (Error "unsat") result );
         ( "optimize_expr_multiplication" >:: fun _ ->
           (* Test maximizing x * 2 where x <= 3 *)
           let x = Variable.from_name "x" in
           let pre = NRel (N_rel.Le, Var x, Num 3) in
           let expr = Binary (N_binary.Mult, Var x, Num 2) in
           let result =
             IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize expr
           in
           assert_equal (Ok 6) result );
         ( "optimize_expr_with_timeout" >:: fun _ ->
           (* Test with timeout parameter *)
           let result =
             IntGen.optimize_expr ~timeout:1000 Optimizer.Strategy.Maximize
               (Num 7)
           in
           assert_equal (Ok 7) result );
       ]

let _ = run_test_tt_main tests
