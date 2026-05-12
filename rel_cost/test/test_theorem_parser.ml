open Protocols.Exp
open Rel_cost_parsing.Parsers
open Protocols
open Rel_cost_parsing
open Rel_cost_parsing.Theorem_file
open Rel_cost.Symbolic_metric_analysis.Theorem

(* Helper functions to reduce repetition *)
let parse_theorem_ok (input : string) : Theorem_file.t =
  match TheoremFileParser.of_string input with
  | Ok file -> file
  | Error msg -> Alcotest.failf "Parse error for theorem: %s" msg

let test_theorem_parse (name : string) (input : string)
    (expected : Theorem_file.t) =
  ( name,
    `Quick,
    fun () ->
      let actual = parse_theorem_ok input in
      Alcotest.(check bool) name true (actual = expected) )

let test_theorem_error (name : string) (input : string) =
  ( name,
    `Quick,
    fun () ->
      match TheoremFileParser.of_string input with
      | Ok p ->
          Alcotest.failf
            "[%s] Expected parse error for input:\n%s\ngot valid parse: %s" name
            input (Theorem_file.to_string p)
      | Error _ -> () )

(* Helper to create variables *)
let var (name : string) : nexp = Var (Variable.from_name name)

(* Test cases based on benchmark_constraints.ml examples *)
let benchmark_tests =
  [
    (* thm1: cost(2 * threadIdx.x) = 2 *)
    test_theorem_parse "thm1 - simple constant theorem"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(2 * tidx) == 2;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = [];
        globals = [];
        active_threads = Some (Bool true);
        assumptions = Some (Bool true);
        goals =
          [
            Goal.Prop
              (NRel
                 ( N_rel.Eq,
                   NCall ("ua", Binary (N_binary.Mult, Num 2, var "tidx")),
                   Num 2 ));
          ];
      };
    (* thm2: cost(x * threadIdx.x) = x where 1 ≤ x ≤ 10 *)
    test_theorem_parse "thm2 - variable with constraints"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : x >= 1 && x <= 10;
      prove ua(x * tidx) == x;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = [];
        globals = [];
        active_threads = Some (Bool true);
        assumptions =
          Some
            (BRel
               ( B_rel.BAnd,
                 NRel (N_rel.Ge, var "x", Num 1),
                 NRel (N_rel.Le, var "x", Num 10) ));
        goals =
          [
            Goal.Prop
              (NRel
                 ( N_rel.Eq,
                   NCall ("ua", Binary (N_binary.Mult, var "x", var "tidx")),
                   var "x" ));
          ];
      };
    (* thm3: cost(x * threadIdx.x) = x where 1 ≤ x ≤ 32 *)
    test_theorem_parse "thm3 - variable with larger bounds"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : x >= 1 && x <= 32;
      prove ua(x * tidx) == x;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = [];
        globals = [];
        active_threads = Some (Bool true);
        assumptions =
          Some
            (BRel
               ( B_rel.BAnd,
                 NRel (N_rel.Ge, var "x", Num 1),
                 NRel (N_rel.Le, var "x", Num 32) ));
        goals =
          [
            Goal.Prop
              (NRel
                 ( N_rel.Eq,
                   NCall ("ua", Binary (N_binary.Mult, var "x", var "tidx")),
                   var "x" ));
          ];
      };
  ]

let field_order_tests =
  [
    (* Test that fields can appear in any order *)
    test_theorem_parse "fields in different order"
      {|
      assumptions : true;
      locals : [x, y];
      threads_per_warp : 16;
      active_threads : x > 0;
      block_dim : {y: 2, x: 16, z: 1};
      prove ua(x + y) == 2;
    |}
      {
        assumptions = Some (Bool true);
        locals = [ Variable.from_name "x"; Variable.from_name "y" ];
        globals = [];
        threads_per_warp = Some 16;
        active_threads = Some (NRel (N_rel.Gt, var "x", Num 0));
        block_dim = Some (Dim3.make ~x:16 ~y:2 ~z:1 ());
        goals =
          [
            Goal.Prop
              (NRel
                 ( N_rel.Eq,
                   NCall ("ua", Binary (N_binary.Plus, var "x", var "y")),
                   Num 2 ));
          ];
      };
    (* Test dim3 fields in different order *)
    test_theorem_parse "dim3 fields in different order"
      {|
      threads_per_warp : 8;
      block_dim : {z: 4, y: 2, x: 8};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(tidx) == 1;
    |}
      {
        threads_per_warp = Some 8;
        block_dim = Some (Dim3.make ~x:8 ~y:2 ~z:4 ());
        locals = [];
        globals = [];
        active_threads = Some (Bool true);
        assumptions = Some (Bool true);
        goals = [ Goal.Prop (NRel (N_rel.Eq, NCall ("ua", var "tidx"), Num 1)) ];
      };
  ]

let relational_operator_tests =
  [
    test_theorem_parse "not equal relation"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(x) != 0;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = [];
        globals = [];
        active_threads = Some (Bool true);
        assumptions = Some (Bool true);
        goals = [ Goal.Prop (NRel (N_rel.Neq, NCall ("ua", var "x"), Num 0)) ];
      };
    test_theorem_parse "less than relation"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(x) < 10;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = [];
        globals = [];
        active_threads = Some (Bool true);
        assumptions = Some (Bool true);
        goals = [ Goal.Prop (NRel (N_rel.Lt, NCall ("ua", var "x"), Num 10)) ];
      };
    test_theorem_parse "greater equal relation"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(x + 1) >= x;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = [];
        globals = [];
        active_threads = Some (Bool true);
        assumptions = Some (Bool true);
        goals =
          [
            Goal.Prop
              (NRel
                 ( N_rel.Ge,
                   NCall ("ua", Binary (N_binary.Plus, var "x", Num 1)),
                   var "x" ));
          ];
      };
  ]

let complex_expression_tests =
  [
    test_theorem_parse "complex arithmetic expression"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [x, y, stride];
      active_threads : stride > 0 && x < 100;
      assumptions : y >= 0;
      prove ua((x + y) * stride + tidx % 32) == x * stride;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals =
          [
            Variable.from_name "x";
            Variable.from_name "y";
            Variable.from_name "stride";
          ];
        globals = [];
        active_threads =
          Some
            (BRel
               ( B_rel.BAnd,
                 NRel (N_rel.Gt, var "stride", Num 0),
                 NRel (N_rel.Lt, var "x", Num 100) ));
        assumptions = Some (NRel (N_rel.Ge, var "y", Num 0));
        goals =
          [
            Goal.Prop
              (NRel
                 ( N_rel.Eq,
                   NCall
                     ( "ua",
                       Binary
                         ( N_binary.Plus,
                           Binary
                             ( N_binary.Mult,
                               Binary (N_binary.Plus, var "x", var "y"),
                               var "stride" ),
                           Binary (N_binary.Mod Signedness.Signed, var "tidx", Num 32) ) ),
                   Binary (N_binary.Mult, var "x", var "stride") ));
          ];
      };
    test_theorem_parse "bitwise operations"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [mask];
      active_threads : true;
      assumptions : (mask & 255) == mask;
      prove ua(tidx & mask) <= mask;
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = [ Variable.from_name "mask" ];
        globals = [];
        active_threads = Some (Bool true);
        assumptions =
          Some
            (NRel
               ( N_rel.Eq,
                 Binary (N_binary.BitAnd, var "mask", Num 255),
                 var "mask" ));
        goals =
          [
            Goal.Prop
              (NRel
                 ( N_rel.Le,
                   NCall ("ua", Binary (N_binary.BitAnd, var "tidx", var "mask")),
                   var "mask" ));
          ];
      };
  ]

let error_tests =
  [
    test_theorem_error "missing semicolon after field"
      {|
      threads_per_warp : 32
      block_dim : {x: 32, y: 1, z: 1};
      ua(x) == 1
    |};
    test_theorem_error "invalid relational operator"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      ua(x) === 1
    |};
    test_theorem_error "missing ua function call"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      x == 1
    |};
  ]

(* Round-trip serialization tests *)
let test_round_trip (name : string) (input : string) =
  ( name,
    `Quick,
    fun () ->
      (* Parse the input string *)
      let original = parse_theorem_ok input in
      (* Serialize it back to string *)
      let serialized = Theorem_file.to_string original in
      (* Parse the serialized string *)
      let reparsed = parse_theorem_ok serialized in
      (* Use Alcotest.failf for better error reporting on mismatch *)
      if not (original = reparsed) then
        Alcotest.failf
          "Round-trip serialization failed.\n\
           Original: %s\n\
           Serialized: %s\n\
           Reparsed: %s"
          (Theorem_file.to_string original)
          serialized
          (Theorem_file.to_string reparsed)
      else () )

let round_trip_tests =
  [
    test_round_trip "simple constant theorem round-trip"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      globals : [];
      active_threads : true;
      assumptions : true;
      prove ua(2 * tidx) == 2;
    |};
    test_round_trip "variable with constraints round-trip"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      globals : [];
      active_threads : true;
      assumptions : x >= 1 && x <= 10;
      prove ua(x * tidx) == x;
    |};
    test_round_trip "ua on both sides comparison round-trip"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [offset];
      globals : [];
      active_threads : offset > 0;
      assumptions : true;
      prove ua(tidx) <= ua(tidx + offset);
    |};
    test_round_trip "minimize goal round-trip"
      {|
      threads_per_warp : 8;
      block_dim : {x: 8, y: 2, z: 1};
      locals : [];
      globals : [];
      active_threads : true;
      assumptions : true;
      min ua(tidx * 2 + 1);
    |};
    test_round_trip "maximize goal round-trip"
      {|
      threads_per_warp : 16;
      block_dim : {x: 16, y: 1, z: 1};
      locals : [x];
      globals : [];
      active_threads : x > 0;
      assumptions : true;
      max ua(x * tidx);
    |};
    test_round_trip "multiple goals round-trip"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [x, y];
      globals : [];
      active_threads : x > 0 && y >= 0;
      assumptions : x <= 100;
      prove ua(x * tidx) >= 1;
      max ua(y + tidx);
      min x + y;
    |};
    test_round_trip "complex arithmetic expression round-trip"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [x, y, stride];
      globals : [];
      active_threads : stride > 0 && x < 100;
      assumptions : y >= 0;
      prove ua((x + y) * stride + tidx % 32) == x * stride;
    |};
    test_round_trip "bitwise operations round-trip"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [mask];
      globals : [];
      active_threads : true;
      assumptions : (mask & 255) == mask;
      prove ua(tidx & mask) <= mask;
    |};
    test_round_trip "different field order round-trip"
      {|
      assumptions : true;
      locals : [x, y];
      globals : [];
      threads_per_warp : 16;
      active_threads : x > 0;
      block_dim : {y: 2, x: 16, z: 1};
      prove ua(x + y) == 2;
    |};
    test_round_trip "different dim3 field order round-trip"
      {|
      threads_per_warp : 8;
      block_dim : {z: 4, y: 2, x: 8};
      locals : [];
      globals : [];
      active_threads : true;
      assumptions : true;
      prove ua(tidx) == 1;
    |};
  ]

(* Comment support tests *)
let test_comment_parsing (name : string) (input : string) =
  ( name,
    `Quick,
    fun () ->
      (* Just test that parsing succeeds with comments *)
      let _parsed = parse_theorem_ok input in
      (* If we get here without exception, comments work *)
      () )

let comment_tests =
  [
    test_comment_parsing "line comments"
      {|
      threads_per_warp : 32; // line comment
      block_dim : {x: 32, y: 1, z: 1}; // another comment
      locals : []; // empty locals
      active_threads : true; // always true
      assumptions : true; // global true
      prove ua(tidx) == 1; // goal comment
    |};
    test_comment_parsing "block comments"
      {|
      /* This is a block comment */
      threads_per_warp : 32;
      /* Another block comment
         spanning multiple lines */
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(tidx) == 1;
    |};
    test_comment_parsing "mixed comments"
      {|
      threads_per_warp : 32; // line comment
      /* block comment */ block_dim : {x: 32, y: 1, z: 1};
      locals : []; /* inline block */
      active_threads : true; // another line comment
      assumptions : true;
      prove ua(tidx) == 1; // final comment
    |};
    test_comment_parsing "comments in expressions"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [x, y];
      active_threads : x > 0 /* greater than zero */ && y >= 0;
      assumptions : true;
      prove ua(x + y) == 2; // sum should be 2
    |};
    test_comment_parsing "comments everywhere"
      {|
      // File header comment
      /* Configuration section */
      threads_per_warp : 32; // threads per warp
      block_dim : {x: 32, y: 1, z: 1}; /* block dimensions */

      // Variables section
      locals : [mask, offset]; // local variables

      /* Context definitions */
      active_threads : mask > 0 && /* positive mask */ offset >= 0;
      assumptions : true; // always true

      // Goals section
      prove ua(tidx + offset) <= mask; // prove statement
      max ua(mask & tidx); // maximize statement
      min offset + 1; // minimize statement
    |};
    test_comment_parsing "multiline block comments"
      {|
      threads_per_warp : 32;
      /* This is a multiline block comment
         that spans several lines
         and contains various text
         but no nested comments */
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(tidx) == 1;
    |};
  ]

(* Test for comment error handling *)
let test_comment_error (name : string) (input : string) =
  ( name,
    `Quick,
    fun () ->
      (* Test that parsing fails with appropriate error *)
      match TheoremFileParser.of_string input with
      | Ok _ -> Alcotest.failf "Expected parse error for %s" name
      | Error msg ->
          (* Verify error message mentions unterminated comment *)
          let open Stage0.Common in
          if
            contains ~substring:"Unterminated" msg
            || contains ~substring:"comment" msg
          then ()
          else
            Alcotest.failf
              "Error message should mention unterminated comment: %s" msg )

let comment_error_tests =
  [
    test_comment_error "unterminated block comment"
      {|
      threads_per_warp : 32;
      /* This comment is never closed
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      active_threads : true;
      assumptions : true;
      prove ua(tidx) == 1;
    |};
  ]

let all_tests =
  [
    ("benchmark examples", benchmark_tests);
    ("field order flexibility", field_order_tests);
    ("relational operators", relational_operator_tests);
    ("complex expressions", complex_expression_tests);
    ("error handling", error_tests);
    ("round-trip serialization", round_trip_tests);
    ("comment support", comment_tests);
    ("comment error handling", comment_error_tests);
  ]

(* Run the tests *)
let () = Alcotest.run "Theorem Parser" all_tests
