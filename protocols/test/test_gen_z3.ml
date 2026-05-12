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

let test_ule_ugt_on_zero () : unit =
  (* Symmetric to [test_ult_vs_lt_on_zero], for [ULe] and [UGt].
     With [x] a free 64-bit BV variable:
       UGt(x, -1)  encoded as bvugt(x, all-ones)  -> UNSAT
                   (no unsigned value > the max unsigned).
       Gt(x, -1)   encoded as bvsgt(x, all-ones)  -> SAT
                   (all-ones is signed -1, anything > -1 exists).
       ULe(x, -1)  encoded as bvule(x, all-ones)  -> SAT (always).
       Le(x, -1)   encoded as bvsle(x, all-ones)  -> SAT
                   (the negative half of signed range). *)
  let x = Variable.from_name "x" in
  let minus_one = Num (-1) in
  let ugt = NRel (N_rel.UGt, Var x, minus_one) in
  let sgt = NRel (N_rel.Gt, Var x, minus_one) in
  (match Bv64Gen.solve ugt with
  | Ok Solver.Unsat -> ()
  | Ok (Solver.Sat _) -> Alcotest.fail "UGt(x, -1) should be UNSAT"
  | Error msg -> Alcotest.failf "UGt solver error: %s" msg);
  match Bv64Gen.solve sgt with
  | Ok (Solver.Sat _) -> ()
  | Ok Solver.Unsat -> Alcotest.fail "Gt(x, -1) should be SAT"
  | Error msg -> Alcotest.failf "Gt solver error: %s" msg

let test_urshift_vs_rshift_on_high_bit () : unit =
  (* Pin the encoder distinction for right shift.
     With [x] free at 64-bit BV, the claim
       x = -1 /\ (x >> 1) > 0
     is SAT under unsigned [URightShift] (logical shift fills zero;
     all-ones >>>u 1 = 2^63 - 1, which is positive in any reading)
     and UNSAT under signed [RightShift] (arithmetic shift fills the
     sign bit; all-ones >> 1 stays all-ones, which is signed -1, not
     > 0). *)
  let x = Variable.from_name "x" in
  let one = Num 1 in
  let zero = Num 0 in
  let minus_one = Num (-1) in
  let eq_minus_one = NRel (Eq, Var x, minus_one) in
  let shifted_signed = Binary (N_binary.RightShift, Var x, one) in
  let shifted_unsigned = Binary (N_binary.URightShift, Var x, one) in
  let claim_signed =
    BRel (BAnd, eq_minus_one, NRel (Gt, shifted_signed, zero))
  in
  let claim_unsigned =
    BRel (BAnd, eq_minus_one, NRel (Gt, shifted_unsigned, zero))
  in
  (match Bv64Gen.solve claim_signed with
  | Ok Solver.Unsat -> ()
  | Ok (Solver.Sat _) ->
      Alcotest.fail "RightShift on all-ones stays negative; should be UNSAT"
  | Error msg -> Alcotest.failf "signed shift solver error: %s" msg);
  match Bv64Gen.solve claim_unsigned with
  | Ok (Solver.Sat _) -> ()
  | Ok Solver.Unsat ->
      Alcotest.fail "URightShift on all-ones yields 2^63-1; should be SAT"
  | Error msg -> Alcotest.failf "unsigned shift solver error: %s" msg

let test_udiv_vs_sdiv_on_minus_one () : unit =
  (* Pin the encoder distinction for division.
     With [x = -1] (all-ones at 64-bit BV), the claim
       x = -1 /\ (x / 2) > 0
     is UNSAT under signed [Div Signed] (bvsdiv truncates toward
     zero: -1 / 2 = 0, not > 0) and SAT under unsigned
     [Div Unsigned] (bvudiv: (2^64 - 1) / 2 = 2^63 - 1, which is the
     max signed positive value). *)
  let x = Variable.from_name "x" in
  let one = Num 1 in
  let zero = Num 0 in
  let minus_one = Num (-1) in
  let two = Num 2 in
  let _ = one in
  let eq_minus_one = NRel (Eq, Var x, minus_one) in
  let div_signed = Binary (N_binary.Div Signedness.Signed, Var x, two) in
  let div_unsigned = Binary (N_binary.Div Signedness.Unsigned, Var x, two) in
  let claim_signed =
    BRel (BAnd, eq_minus_one, NRel (Gt, div_signed, zero))
  in
  let claim_unsigned =
    BRel (BAnd, eq_minus_one, NRel (Gt, div_unsigned, zero))
  in
  (match Bv64Gen.solve claim_signed with
  | Ok Solver.Unsat -> ()
  | Ok (Solver.Sat _) ->
      Alcotest.fail "signed bvsdiv(-1, 2) = 0; should be UNSAT"
  | Error msg -> Alcotest.failf "signed div solver error: %s" msg);
  match Bv64Gen.solve claim_unsigned with
  | Ok (Solver.Sat _) -> ()
  | Ok Solver.Unsat ->
      Alcotest.fail "unsigned bvudiv(all-ones, 2) = 2^63-1; should be SAT"
  | Error msg -> Alcotest.failf "unsigned div solver error: %s" msg

let test_umod_vs_smod_on_minus_one () : unit =
  (* Pin the encoder distinction for modulo, plus the signed-`%`
     fix from bvsmod to bvsrem (C99 semantics).
     With [x = -1] (all-ones at 64-bit BV), the claim
       x = -1 /\ (x % 3) == 0
     is UNSAT under signed [Mod Signed] (bvsrem(-1, 3) = -1) and SAT
     under unsigned [Mod Unsigned] (bvurem(2^64 - 1, 3) = 0, since
     2^64 - 1 is divisible by 3). *)
  let x = Variable.from_name "x" in
  let zero = Num 0 in
  let minus_one = Num (-1) in
  let three = Num 3 in
  let eq_minus_one = NRel (Eq, Var x, minus_one) in
  let mod_signed = Binary (N_binary.Mod Signedness.Signed, Var x, three) in
  let mod_unsigned =
    Binary (N_binary.Mod Signedness.Unsigned, Var x, three)
  in
  let claim_signed =
    BRel (BAnd, eq_minus_one, NRel (Eq, mod_signed, zero))
  in
  let claim_unsigned =
    BRel (BAnd, eq_minus_one, NRel (Eq, mod_unsigned, zero))
  in
  (match Bv64Gen.solve claim_signed with
  | Ok Solver.Unsat -> ()
  | Ok (Solver.Sat _) ->
      Alcotest.fail "signed bvsrem(-1, 3) = -1; should be UNSAT for == 0"
  | Error msg -> Alcotest.failf "signed mod solver error: %s" msg);
  match Bv64Gen.solve claim_unsigned with
  | Ok (Solver.Sat _) -> ()
  | Ok Solver.Unsat ->
      Alcotest.fail "unsigned bvurem(2^64-1, 3) = 0; should be SAT"
  | Error msg -> Alcotest.failf "unsigned mod solver error: %s" msg

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
    ("ule_ugt_on_zero", `Quick, test_ule_ugt_on_zero);
    ("urshift_vs_rshift_on_high_bit", `Quick, test_urshift_vs_rshift_on_high_bit);
    ("udiv_vs_sdiv_on_minus_one", `Quick, test_udiv_vs_sdiv_on_minus_one);
    ("umod_vs_smod_on_minus_one", `Quick, test_umod_vs_smod_on_minus_one);
  ]

let () = Alcotest.run "Gen_z3" [ ("test_gen_z3", tests) ]
