open Protocols
open OUnit2

let gv_equal ~expected ~given : unit =
  let msg =
    "Expected: "
    ^ Gv_parser.to_string expected
    ^ "\nGiven: " ^ Gv_parser.to_string given
  in
  assert_equal expected given ~msg

let parses_ser (expected : Gv_parser.t) : unit =
  let ser = Gv_parser.serialize expected in
  match ser |> Gv_parser.from_string with
  | Some given -> gv_equal ~expected ~given
  | None -> assert_failure ("Could not parse: " ^ ser)

let tests =
  "test_predicates"
  >::: [
         ( "parse_param" >:: fun _ ->
           let open Gv_parser in
           assert_equal (Some true) (parse_pass "//pass");
           assert_equal (Some true) (parse_pass "//pass\n");
           assert_equal (Some true) (parse_pass "// pass   \n");
           assert_equal (Some false) (parse_pass "//xfail:BOOGIE_ERROR   \n");
           assert_equal (Some false) (parse_pass "// xfail:BOOGIE_ERROR   ");
           assert_equal (Some false) (parse_pass "// xfail:BOOGIE_ERROR   \n");
           assert_equal None (parse_pass "xfail:BOOGIE_ERROR   \n");
           assert_equal None (parse_pass "/ xfail:BOOGIE_ERROR   ");
           assert_equal None (parse_pass "// ail:BOOGIE_ERROR   \n");
           assert_equal None (parse_pass "  pass   \n");
           () );
         ( "parse" >:: fun _ ->
           let open Gv_parser in
           let pass = "//pass\n" in
           let params = "//--blockDim=12\n" in
           gv_equal
             ~expected:
               {
                 default with
                 pass = true;
                 block_dim = Dim3.{ x = 12; y = 1; z = 1 };
               }
             ~given:(from_pair ~pass ~params |> Option.get);
           let pass = "//pass\n" in
           let params = "// --blockDim=32 --gridDim=[2]\n" in
           gv_equal
             ~expected:
               {
                 default with
                 pass = true;
                 block_dim = Dim3.{ x = 32; y = 1; z = 1 };
                 grid_dim = Dim3.{ x = 2; y = 1; z = 1 };
               }
             ~given:(from_pair ~pass ~params |> Option.get);
           () );
         ( "parse_ser" >:: fun _ ->
           let open Gv_parser in
           parses_ser default;
           parses_ser
             {
               default with
               pass = true;
               block_dim = Dim3.{ x = 12; y = 1; z = 1 };
             };
           parses_ser
             {
               default with
               pass = false;
               block_dim = Dim3.{ x = 32; y = 1; z = 1 };
               grid_dim = Dim3.{ x = 2; y = 1; z = 1 };
             };
           () );
       ]

let _ = run_test_tt_main tests
