open Protocols
open Protocols.Gen_z3
open Protocols.Exp

let test_optimize_expr_simple () : unit =
  (* Test maximizing a simple constant *)
  let result = IntGen.optimize_expr Optimizer.Strategy.Maximize (Num 42) in
  Alcotest.check
    Alcotest.(result (option int) string)
    "maximize constant 42" (Ok (Some 42)) result

let test_optimize_expr_minimize () : unit =
  (* Test minimizing a constant *)
  let result = IntGen.optimize_expr Optimizer.Strategy.Minimize (Num 10) in
  Alcotest.check
    Alcotest.(result (option int) string)
    "minimize constant 10" (Ok (Some 10)) result

let test_optimize_expr_with_variable () : unit =
  (* Test maximizing a variable x with constraint x <= 5 *)
  let x = Variable.from_name "x" in
  let pre = NRel (N_rel.Le, Var x, Num 5) in
  let result = IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize (Var x) in
  Alcotest.check
    Alcotest.(result (option int) string)
    "maximize x where x <= 5" (Ok (Some 5)) result

let test_optimize_expr_arithmetic () : unit =
  (* Test maximizing x + 3 where x <= 2 *)
  let x = Variable.from_name "x" in
  let pre = NRel (N_rel.Le, Var x, Num 2) in
  let expr = Binary (N_binary.Plus, Var x, Num 3) in
  let result = IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize expr in
  Alcotest.check
    Alcotest.(result (option int) string)
    "maximize x + 3 where x <= 2" (Ok (Some 5)) result

let test_optimize_expr_minimize_with_constraint () : unit =
  (* Test minimizing x where x >= 10 *)
  let x = Variable.from_name "x" in
  let pre = NRel (N_rel.Ge, Var x, Num 10) in
  let result = IntGen.optimize_expr ~pre Optimizer.Strategy.Minimize (Var x) in
  Alcotest.check
    Alcotest.(result (option int) string)
    "minimize x where x >= 10" (Ok (Some 10)) result

let test_optimize_expr_unsat () : unit =
  (* Test with contradictory constraints: x > 5 and x < 5 *)
  let x = Variable.from_name "x" in
  let pre =
    BRel
      (B_rel.BAnd, NRel (N_rel.Gt, Var x, Num 5), NRel (N_rel.Lt, Var x, Num 5))
  in
  let result = IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize (Var x) in
  Alcotest.check
    Alcotest.(result (option int) string)
    "contradictory constraints should be unsat" (Ok None) result

let test_optimize_expr_multiplication () : unit =
  (* Test maximizing x * 2 where x <= 3 *)
  let x = Variable.from_name "x" in
  let pre = NRel (N_rel.Le, Var x, Num 3) in
  let expr = Binary (N_binary.Mult, Var x, Num 2) in
  let result = IntGen.optimize_expr ~pre Optimizer.Strategy.Maximize expr in
  Alcotest.check
    Alcotest.(result (option int) string)
    "maximize x * 2 where x <= 3" (Ok (Some 6)) result

let test_optimize_expr_with_timeout () : unit =
  (* Test with timeout parameter *)
  let result =
    IntGen.optimize_expr ~timeout:1000 Optimizer.Strategy.Maximize (Num 7)
  in
  Alcotest.check
    Alcotest.(result (option int) string)
    "maximize with timeout" (Ok (Some 7)) result

let test_tactic_fail () : unit =
  (* Test Fail tactic in debugging mode - should always fail with detailed info *)
  let tautology = Bool true in
  [ true; false ]
  |> List.iter (fun debug ->
      let result = IntGen.solve_with_tactic ~debug Tactic.Fail tautology in
      match result with
      | Error _ -> () (* Expected - tactic should fail *)
      | Ok e ->
          Printf.sprintf "debug=%b, unexpected: %s" debug (Solver.to_string e)
          |> Alcotest.fail)

let test_medianfilter_overflow_shape () : unit =
  (* Pin the [Bv64Gen] verdict on the shape that motivated this feature.
     The setup mirrors what [Params.to_bexp] emits for two declared
     [unsigned int] kernel parameters: each is range-bound to
     [0, 2^32 - 1]. The encoder runs at 64-bit BV, so a product of
     two values up to [2^32 - 1] can cross [2^63] without overflowing
     the 64-bit width. The claim under test is:
       x in [2, 2^32-1] /\ y in [2, 2^32-1] /\ x >= x*y
     Under signed [Ge]: SAT — Z3 finds [x = y = 2^32-1], whose 64-bit
     product has the high bit set and is interpreted as negative;
     "positive >= negative" then holds.
     Under unsigned [UGe]: UNSAT — both sides stay positive in
     unsigned BV, and a value cannot be greater-than-or-equal to its
     own square when the square does not wrap. *)
  let x = Variable.from_name "x" in
  let y = Variable.from_name "y" in
  let two = Num 2 in
  let uint_max = Num 4294967295 in
  let xy_signed = Binary (N_binary.Mult, Var x, Var y) in
  let xy_unsigned = Binary (N_binary.UMult, Var x, Var y) in
  (* 32-bit-unsigned-int range bound on x and y, expressed via UGe
     (no ULe constructor yet). *)
  let in_uint x =
    BRel
      ( B_rel.BAnd,
        NRel (N_rel.UGe, Var x, two),
        NRel (N_rel.UGe, uint_max, Var x) )
  in
  let pre_bounds = BRel (B_rel.BAnd, in_uint x, in_uint y) in
  let claim_signed =
    BRel (BAnd, pre_bounds, NRel (N_rel.Ge, Var x, xy_signed))
  in
  let claim_unsigned =
    BRel (BAnd, pre_bounds, NRel (N_rel.UGe, Var x, xy_unsigned))
  in
  (match Bv64Gen.solve claim_signed with
  | Ok (Solver.Sat _) -> ()
  | Ok Solver.Unsat ->
      Alcotest.fail "signed Ge admits overflow model and should be SAT"
  | Error msg -> Alcotest.failf "signed claim solver error: %s" msg);
  match Bv64Gen.solve claim_unsigned with
  | Ok Solver.Unsat -> ()
  | Ok (Solver.Sat _) ->
      Alcotest.fail "unsigned UGe should reject the overflow model and be UNSAT"
  | Error msg -> Alcotest.failf "unsigned claim solver error: %s" msg

let test_ult_vs_lt_on_zero () : unit =
  (* ULt should be encoded as bvult: no unsigned BV value is < 0, so UNSAT.
     Lt should be encoded as bvslt: half the signed BV range is < 0, so SAT.
     This pins the encoder distinction at the only place ULt currently
     diverges from Lt in [Gen_z3]. *)
  let x = Variable.from_name "x" in
  let ult_zero = NRel (N_rel.ULt, Var x, Num 0) in
  let slt_zero = NRel (N_rel.Lt, Var x, Num 0) in
  (match Bv64Gen.solve ult_zero with
  | Ok Solver.Unsat -> ()
  | Ok (Solver.Sat _) -> Alcotest.fail "ULt(x, 0) should be UNSAT, got SAT"
  | Error msg -> Alcotest.failf "ULt(x, 0) solver error: %s" msg);
  match Bv64Gen.solve slt_zero with
  | Ok (Solver.Sat _) -> ()
  | Ok Solver.Unsat -> Alcotest.fail "Lt(x, 0) should be SAT, got UNSAT"
  | Error msg -> Alcotest.failf "Lt(x, 0) solver error: %s" msg

let test_tactic_skip () : unit =
  (* Test Skip tactic in production mode - should solve tautology *)
  let tautology = Bool true in
  [ true; false ]
  |> List.iter (fun debug ->
      let result = IntGen.solve_with_tactic ~debug Tactic.Skip tautology in
      match result with
      | Ok (Solver.Sat _) ->
          ()
          (* Expected - skip should leave goal unchanged, tautology should be sat *)
      | Ok e ->
          Printf.sprintf "debug=%b, unexpected: %s" debug (Solver.to_string e)
          |> Alcotest.fail
      | Error msg ->
          Printf.sprintf "debug=%b, solver error: %s" debug msg |> Alcotest.fail)

let tests : unit Alcotest.test_case list =
  [
    ("optimize_expr_simple", `Quick, test_optimize_expr_simple);
    ("optimize_expr_minimize", `Quick, test_optimize_expr_minimize);
    ("optimize_expr_with_variable", `Quick, test_optimize_expr_with_variable);
    ("optimize_expr_arithmetic", `Quick, test_optimize_expr_arithmetic);
    ( "optimize_expr_minimize_with_constraint",
      `Quick,
      test_optimize_expr_minimize_with_constraint );
    ("optimize_expr_unsat", `Quick, test_optimize_expr_unsat);
    ("optimize_expr_multiplication", `Quick, test_optimize_expr_multiplication);
    ("optimize_expr_with_timeout", `Quick, test_optimize_expr_with_timeout);
    ("tactic_fail", `Quick, test_tactic_fail);
    ("tactic_skip", `Quick, test_tactic_skip);
    ("ult_vs_lt_on_zero", `Quick, test_ult_vs_lt_on_zero);
    ("medianfilter_overflow_shape", `Quick, test_medianfilter_overflow_shape);
  ]

let () = Alcotest.run "Gen_z3" [ ("test_gen_z3", tests) ]
