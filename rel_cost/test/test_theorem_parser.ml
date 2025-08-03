open Protocols.Exp
open Rel_cost_parsing.Parsers
open Protocols
open Rel_cost_parsing
open Rel_cost_parsing.Theorem_file

(* Helper functions to reduce repetition *)
let parse_theorem_ok (input : string) : t =
  match TheoremFileParser.of_string input with
  | Ok file -> file
  | Error msg -> Alcotest.failf "Parse error for theorem: %s" msg

let test_theorem_parse (name : string) (input : string) (expected : t) =
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
      local_context : true;
      global_context : true;
      ua(2 * tidx) == 2
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = Some [];
        local_context = Some (Bool true);
        global_context = Some (Bool true);
        index = Binary (N_binary.Mult, Num 2, var "tidx");
        rel = N_rel.Eq;
        cost = Num 2;
      };
    (* thm2: cost(x * threadIdx.x) = x where 1 ≤ x ≤ 10 *)
    test_theorem_parse "thm2 - variable with constraints"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      local_context : true;
      global_context : x >= 1 && x <= 10;
      ua(x * tidx) == x
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = Some [];
        local_context = Some (Bool true);
        global_context =
          Some
            (BRel
               ( B_rel.BAnd,
                 NRel (N_rel.Ge, var "x", Num 1),
                 NRel (N_rel.Le, var "x", Num 10) ));
        index = Binary (N_binary.Mult, var "x", var "tidx");
        rel = N_rel.Eq;
        cost = var "x";
      };
    (* thm3: cost(x * threadIdx.x) = x where 1 ≤ x ≤ 32 *)
    test_theorem_parse "thm3 - variable with larger bounds"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      local_context : true;
      global_context : x >= 1 && x <= 32;
      ua(x * tidx) == x
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = Some [];
        local_context = Some (Bool true);
        global_context =
          Some
            (BRel
               ( B_rel.BAnd,
                 NRel (N_rel.Ge, var "x", Num 1),
                 NRel (N_rel.Le, var "x", Num 32) ));
        index = Binary (N_binary.Mult, var "x", var "tidx");
        rel = N_rel.Eq;
        cost = var "x";
      };
  ]

let field_order_tests =
  [
    (* Test that fields can appear in any order *)
    test_theorem_parse "fields in different order"
      {|
      global_context : true;
      locals : [x, y];
      threads_per_warp : 16;
      local_context : x > 0;
      block_dim : {y: 2, x: 16, z: 1};
      ua(x + y) == 2
    |}
      {
        global_context = Some (Bool true);
        locals = Some [ Variable.from_name "x"; Variable.from_name "y" ];
        threads_per_warp = Some 16;
        local_context = Some (NRel (N_rel.Gt, var "x", Num 0));
        block_dim = Some (Dim3.make ~x:16 ~y:2 ~z:1 ());
        index = Binary (N_binary.Plus, var "x", var "y");
        rel = N_rel.Eq;
        cost = Num 2;
      };
    (* Test dim3 fields in different order *)
    test_theorem_parse "dim3 fields in different order"
      {|
      threads_per_warp : 8;
      block_dim : {z: 4, y: 2, x: 8};
      locals : [];
      local_context : true;
      global_context : true;
      ua(tidx) == 1
    |}
      {
        threads_per_warp = Some 8;
        block_dim = Some (Dim3.make ~x:8 ~y:2 ~z:4 ());
        locals = Some [];
        local_context = Some (Bool true);
        global_context = Some (Bool true);
        index = var "tidx";
        rel = N_rel.Eq;
        cost = Num 1;
      };
  ]

let relational_operator_tests =
  [
    test_theorem_parse "not equal relation"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      local_context : true;
      global_context : true;
      ua(x) != 0
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = Some [];
        local_context = Some (Bool true);
        global_context = Some (Bool true);
        index = var "x";
        rel = N_rel.Neq;
        cost = Num 0;
      };
    test_theorem_parse "less than relation"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      local_context : true;
      global_context : true;
      ua(x) < 10
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = Some [];
        local_context = Some (Bool true);
        global_context = Some (Bool true);
        index = var "x";
        rel = N_rel.Lt;
        cost = Num 10;
      };
    test_theorem_parse "greater equal relation"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [];
      local_context : true;
      global_context : true;
      ua(x + 1) >= x
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = Some [];
        local_context = Some (Bool true);
        global_context = Some (Bool true);
        index = Binary (N_binary.Plus, var "x", Num 1);
        rel = N_rel.Ge;
        cost = var "x";
      };
  ]

let complex_expression_tests =
  [
    test_theorem_parse "complex arithmetic expression"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [x, y, stride];
      local_context : stride > 0 && x < 100;
      global_context : y >= 0;
      ua((x + y) * stride + tidx % 32) == x * stride
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals =
          Some
            [
              Variable.from_name "x";
              Variable.from_name "y";
              Variable.from_name "stride";
            ];
        local_context =
          Some
            (BRel
               ( B_rel.BAnd,
                 NRel (N_rel.Gt, var "stride", Num 0),
                 NRel (N_rel.Lt, var "x", Num 100) ));
        global_context = Some (NRel (N_rel.Ge, var "y", Num 0));
        index =
          Binary
            ( N_binary.Plus,
              Binary
                ( N_binary.Mult,
                  Binary (N_binary.Plus, var "x", var "y"),
                  var "stride" ),
              Binary (N_binary.Mod, var "tidx", Num 32) );
        rel = N_rel.Eq;
        cost = Binary (N_binary.Mult, var "x", var "stride");
      };
    test_theorem_parse "bitwise operations"
      {|
      threads_per_warp : 32;
      block_dim : {x: 32, y: 1, z: 1};
      locals : [mask];
      local_context : true;
      global_context : (mask & 255) == mask;
      ua(tidx & mask) <= mask
    |}
      {
        threads_per_warp = Some 32;
        block_dim = Some (Dim3.make ~x:32 ~y:1 ~z:1 ());
        locals = Some [ Variable.from_name "mask" ];
        local_context = Some (Bool true);
        global_context =
          Some
            (NRel
               ( N_rel.Eq,
                 Binary (N_binary.BitAnd, var "mask", Num 255),
                 var "mask" ));
        index = Binary (N_binary.BitAnd, var "tidx", var "mask");
        rel = N_rel.Le;
        cost = var "mask";
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

let all_tests =
  [
    ("benchmark examples", benchmark_tests);
    ("field order flexibility", field_order_tests);
    ("relational operators", relational_operator_tests);
    ("complex expressions", complex_expression_tests);
    ("error handling", error_tests);
  ]

(* Run the tests *)
let () = Alcotest.run "Theorem Parser" all_tests
