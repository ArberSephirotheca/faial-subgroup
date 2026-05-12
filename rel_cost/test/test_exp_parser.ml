open Protocols.Exp
open Rel_cost_parsing.Parsers
open Protocols

(* Helper functions to reduce repetition *)
let parse_nexp_ok (input : string) : nexp =
  match NExpParser.of_string input with
  | Ok expr -> expr
  | Error msg -> Alcotest.failf "Parse error for '%s': %s" input msg

let parse_bexp_ok (input : string) : bexp =
  match BExpParser.of_string input with
  | Ok expr -> expr
  | Error msg -> Alcotest.failf "Parse error for '%s': %s" input msg

let test_nexp_parse (name : string) (input : string) (expected : nexp) =
  ( name,
    `Quick,
    fun () ->
      let actual = parse_nexp_ok input in
      Alcotest.(check bool) name true (actual = expected) )

let test_bexp_parse (name : string) (input : string) (expected : bexp) =
  ( name,
    `Quick,
    fun () ->
      let actual = parse_bexp_ok input in
      Alcotest.(check bool) name true (actual = expected) )

let test_nexp_error (name : string) (input : string) =
  ( name,
    `Quick,
    fun () ->
      match NExpParser.of_string input with
      | Ok expr ->
          Alcotest.failf "Expected parse error for '%s', got: %s" input
            (n_to_string expr)
      | Error _ -> () )

let test_bexp_error (name : string) (input : string) =
  ( name,
    `Quick,
    fun () ->
      match BExpParser.of_string input with
      | Ok expr ->
          Alcotest.failf "Expected parse error for '%s', got: %s" input
            (b_to_string expr)
      | Error _ -> () )

let test_nexp_error_location (name : string) (input : string)
    (expected_line : int) (expected_col : int) =
  ( name,
    `Quick,
    fun () ->
      match NExpParser.of_string input with
      | Ok expr ->
          Alcotest.failf "Expected parse error for '%s', got: %s" input
            (n_to_string expr)
      | Error msg ->
          let line_str = "line " ^ string_of_int expected_line in
          let col_str = "column " ^ string_of_int expected_col in
          if
            Stage0.Common.contains ~substring:line_str msg
            && Stage0.Common.contains ~substring:col_str msg
          then ()
          else
            Alcotest.failf
              "Expected error message containing line %d, column %d, got: %s"
              expected_line expected_col msg )

let test_bexp_error_location (name : string) (input : string)
    (expected_line : int) (expected_col : int) =
  ( name,
    `Quick,
    fun () ->
      match BExpParser.of_string input with
      | Ok expr ->
          Alcotest.failf "Expected parse error for '%s', got: %s" input
            (b_to_string expr)
      | Error msg ->
          let line_str = "line " ^ string_of_int expected_line in
          let col_str = "column " ^ string_of_int expected_col in
          if
            Stage0.Common.contains ~substring:line_str msg
            && Stage0.Common.contains ~substring:col_str msg
          then ()
          else
            Alcotest.failf
              "Expected error message containing line %d, column %d, got: %s"
              expected_line expected_col msg )

(* Helper to create variables *)
let var (name : string) : nexp = Var (Variable.from_name name)

(* Test cases *)
let nexp_literal_tests =
  [
    test_nexp_parse "integer literal" "42" (Num 42);
    test_nexp_parse "zero" "0" (Num 0);
    test_nexp_parse "negative number" "-42" (Unary (N_unary.Negate, Num 42));
  ]

let nexp_variable_tests =
  [
    test_nexp_parse "simple variable" "x" (var "x");
    test_nexp_parse "variable with underscore" "_var" (var "_var");
    test_nexp_parse "variable with dots" "tid.x" (var "tid.x");
    test_nexp_parse "variable with numbers" "var123" (var "var123");
  ]

let nexp_arithmetic_tests =
  [
    test_nexp_parse "addition" "x + y"
      (Binary (N_binary.Plus, var "x", var "y"));
    test_nexp_parse "subtraction" "x - y"
      (Binary (N_binary.Minus Signedness.Signed, var "x", var "y"));
    test_nexp_parse "multiplication" "x * y"
      (Binary (N_binary.Mult, var "x", var "y"));
    test_nexp_parse "division" "x / y"
      (Binary (N_binary.Div Signedness.Signed, var "x", var "y"));
    test_nexp_parse "modulo" "x % y"
      (Binary (N_binary.Mod Signedness.Signed, var "x", var "y"));
  ]

let nexp_bitwise_tests =
  [
    test_nexp_parse "bitwise and" "x & y"
      (Binary (N_binary.BitAnd, var "x", var "y"));
    test_nexp_parse "bitwise or" "x | y"
      (Binary (N_binary.BitOr, var "x", var "y"));
    test_nexp_parse "bitwise xor" "x ^ y"
      (Binary (N_binary.BitXOr, var "x", var "y"));
    test_nexp_parse "left shift" "x << y"
      (Binary (N_binary.LeftShift, var "x", var "y"));
    test_nexp_parse "right shift" "x >> y"
      (Binary (N_binary.RightShift, var "x", var "y"));
    test_nexp_parse "bitwise not" "~x" (Unary (N_unary.BitNot, var "x"));
    test_nexp_parse "bitwise and" "mask & 255"
      (Binary (N_binary.BitAnd, var "mask", Num 255));
  ]

let nexp_precedence_tests =
  [
    test_nexp_parse "multiplication before addition" "2 + 3 * 4"
      (Binary (N_binary.Plus, Num 2, Binary (N_binary.Mult, Num 3, Num 4)));
    test_nexp_parse "parentheses override precedence" "(2 + 3) * 4"
      (Binary (N_binary.Mult, Binary (N_binary.Plus, Num 2, Num 3), Num 4));
    test_nexp_parse "bitwise operations precedence" "a & b | c"
      (Binary
         (N_binary.BitOr, Binary (N_binary.BitAnd, var "a", var "b"), var "c"));
    test_bexp_parse "bitwise and equal" "(mask & 255) == mask"
      (NRel (N_rel.Eq, Binary (N_binary.BitAnd, var "mask", Num 255), var "mask"));
  ]

let nexp_ternary_tests =
  [
    test_nexp_parse "simple ternary" "x > 0 ? y : z"
      (NIf (NRel (N_rel.Gt, var "x", Num 0), var "y", var "z"));
    test_nexp_parse "ternary with bool cast" "bool(a) ? y : z"
      (NIf (CastBool (var "a"), var "y", var "z"));
  ]

let nexp_cast_tests =
  [
    test_nexp_parse "int cast" "int(x > 0)"
      (CastInt (NRel (N_rel.Gt, var "x", Num 0)));
    test_nexp_parse "int cast with bool literal" "int(true)"
      (CastInt (Bool true));
  ]

let bexp_literal_tests =
  [
    test_bexp_parse "true literal" "true" (Bool true);
    test_bexp_parse "false literal" "false" (Bool false);
  ]

let bexp_comparison_tests =
  [
    test_bexp_parse "equality" "x == 42" (NRel (N_rel.Eq, var "x", Num 42));
    test_bexp_parse "inequality" "x != 42" (NRel (N_rel.Neq, var "x", Num 42));
    test_bexp_parse "less than" "x < y" (NRel (N_rel.Lt, var "x", var "y"));
    test_bexp_parse "less equal" "x <= y" (NRel (N_rel.Le, var "x", var "y"));
    test_bexp_parse "greater than" "x > y" (NRel (N_rel.Gt, var "x", var "y"));
    test_bexp_parse "greater equal" "x >= y" (NRel (N_rel.Ge, var "x", var "y"));
  ]

let bexp_logical_tests =
  [
    test_bexp_parse "logical and" "x > 0 && y < 10"
      (BRel
         ( B_rel.BAnd,
           NRel (N_rel.Gt, var "x", Num 0),
           NRel (N_rel.Lt, var "y", Num 10) ));
    test_bexp_parse "logical or" "x < 0 || y > 10"
      (BRel
         ( B_rel.BOr,
           NRel (N_rel.Lt, var "x", Num 0),
           NRel (N_rel.Gt, var "y", Num 10) ));
    test_bexp_parse "logical not" "!true" (BNot (Bool true));
  ]

let bexp_cast_tests =
  [
    test_bexp_parse "bool cast" "bool(x)" (CastBool (var "x"));
    test_bexp_parse "bool cast with number" "bool(42)" (CastBool (Num 42));
  ]

let comment_tests =
  [
    test_nexp_parse "line comment" "// comment\n42" (Num 42);
    test_nexp_parse "block comment" "/* block comment */ 42" (Num 42);
    test_bexp_parse "comment in expression" "true /* comment */ && false"
      (BRel (B_rel.BAnd, Bool true, Bool false));
  ]

let error_tests =
  [
    test_nexp_error "unterminated block comment" "/* comment";
    test_nexp_error "invalid character" "42 @ 43";
    test_bexp_error "missing operand" "x &&";
  ]

let location_error_tests =
  [
    (* Test parser errors (these include location info) *)
    test_nexp_error_location "parser error at end" "42 +" 1 4;
    test_bexp_error_location "parser error (incomplete expression)" "x ==" 1 4;
    (* Test multiline parser errors *)
    test_nexp_error_location "parser error with newline" "42 +\n   +" 2 3;
    test_bexp_error_location "multiline boolean error" "x > 0 &&\n   " 2 3;
    test_nexp_error_location "error on line 3" "x +\ny +\n(" 3 1;
    (* Test line tracking through multi-line comments *)
    test_nexp_error_location "error after multi-line comment"
      "42 /* comment\nspanning\nmultiple lines */ +" 3 19;
    test_nexp_error_location "error on line after comment"
      "42 /* comment\nspanning\nmultiple lines */\n+" 4 1;
    (* Test parser errors at different positions *)
    test_nexp_error_location "error after parenthesis" "(" 1 1;
    test_nexp_error_location "error in cast" "int(" 1 4;
    test_bexp_error_location "error in bool cast" "bool(" 1 5;
    (* Test basic lexer errors (these don't include location currently) *)
    test_nexp_error "lexer error (bad character)" "42 @ 43";
    test_nexp_error "lexer error at start" "@";
    test_nexp_error "lexer error with whitespace" "   @";
    (* TODO: Add location info to lexer errors in future *)
  ]

let all_tests =
  [
    ("nexp literals", nexp_literal_tests);
    ("nexp variables", nexp_variable_tests);
    ("nexp arithmetic", nexp_arithmetic_tests);
    ("nexp bitwise", nexp_bitwise_tests);
    ("nexp precedence", nexp_precedence_tests);
    ("nexp ternary", nexp_ternary_tests);
    ("nexp casts", nexp_cast_tests);
    ("bexp literals", bexp_literal_tests);
    ("bexp comparisons", bexp_comparison_tests);
    ("bexp logical", bexp_logical_tests);
    ("bexp casts", bexp_cast_tests);
    ("comments", comment_tests);
    ("error handling", error_tests);
    ("error locations", location_error_tests);
  ]

let () = Alcotest.run "Expression Parser" all_tests
