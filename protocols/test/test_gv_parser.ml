open Protocols

let gv_parser : Gv_parser.t Alcotest.testable =
  let pp fmt gv = Format.fprintf fmt "%s" (Gv_parser.to_string gv) in
  let equal = ( = ) in
  Alcotest.testable pp equal

let parses_ser (expected : Gv_parser.t) : unit =
  let ser = Gv_parser.serialize expected in
  match ser |> Gv_parser.from_string with
  | Some given ->
      Alcotest.check gv_parser "serialize/parse roundtrip" expected given
  | None -> Alcotest.fail ("Could not parse: " ^ ser)

let test_parse_param () : unit =
  let open Gv_parser in
  let test_cases =
    [
      ("//pass", Some true, "parse //pass");
      ("//pass\n", Some true, "parse //pass\\n");
      ("// pass   \n", Some true, "parse // pass with spaces");
      ("//xfail:BOOGIE_ERROR   \n", Some false, "parse xfail");
      ("// xfail:BOOGIE_ERROR   ", Some false, "parse xfail no newline");
      ("// xfail:BOOGIE_ERROR   \n", Some false, "parse xfail with newline");
      ("xfail:BOOGIE_ERROR   \n", None, "invalid: no //");
      ("/ xfail:BOOGIE_ERROR   ", None, "invalid: single /");
      ("// ail:BOOGIE_ERROR   \n", None, "invalid: typo in xfail");
      ("  pass   \n", None, "invalid: no //");
    ]
  in
  List.iter
    (fun (input, expected, desc) ->
      Alcotest.check Alcotest.(option bool) desc expected (parse_pass input))
    test_cases

let test_parse () : unit =
  let open Gv_parser in
  let pass = "//pass\n" in
  let params = "//--blockDim=12\n" in
  let expected =
    { default with pass = true; block_dim = Dim3.{ x = 12; y = 1; z = 1 } }
  in
  let given = from_pair ~pass ~params |> Option.get in
  Alcotest.check gv_parser "parse blockDim=12" expected given;

  let pass = "//pass\n" in
  let params = "// --blockDim=32 --gridDim=[2]\n" in
  let expected =
    {
      default with
      pass = true;
      block_dim = Dim3.{ x = 32; y = 1; z = 1 };
      grid_dim = Dim3.{ x = 2; y = 1; z = 1 };
    }
  in
  let given = from_pair ~pass ~params |> Option.get in
  Alcotest.check gv_parser "parse blockDim=32 gridDim=[2]" expected given

let test_parse_ser () : unit =
  let open Gv_parser in
  parses_ser default;
  parses_ser
    { default with pass = true; block_dim = Dim3.{ x = 12; y = 1; z = 1 } };
  parses_ser
    {
      default with
      pass = false;
      block_dim = Dim3.{ x = 32; y = 1; z = 1 };
      grid_dim = Dim3.{ x = 2; y = 1; z = 1 };
    }

let tests : unit Alcotest.test_case list =
  [
    ("parse_param", `Quick, test_parse_param);
    ("parse", `Quick, test_parse);
    ("parse_ser", `Quick, test_parse_ser);
  ]

let () = Alcotest.run "Gv_parser" [ ("test_predicates", tests) ]
