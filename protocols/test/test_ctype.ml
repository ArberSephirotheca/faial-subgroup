open Protocols
open C_type

let test_split_array_type () : unit =
  let test_cases =
    [
      ("int *", Some ("int", "*"));
      ("int", None);
      ("unsigned int [8][8] *", Some ("unsigned int", "[8][8] *"));
      ("int [128]", Some ("int", "[128]"));
      ("float[1024]", Some ("float", "[1024]"));
    ]
  in
  List.iter
    (fun (given, expected) ->
      Alcotest.check
        Alcotest.(option (pair string string))
        ("split_array_type " ^ given)
        expected (split_array_type given))
    test_cases

let test_parse_dim () : unit =
  Alcotest.check
    Alcotest.(list int)
    "parse_dim [8][8]" [ 8; 8 ] (parse_dim "[8][8]");
  Alcotest.check
    Alcotest.(list int)
    "parse_dim [128]" [ 128 ] (parse_dim "[128]")

let test_parse_array_type_opt () : unit =
  let test_cases =
    [
      ("int *", Some [ "int" ]);
      ("int", None);
      ("unsigned int [8][8] *", Some [ "unsigned"; "int" ]);
      ("int [128]", Some [ "int" ]);
      ("float[1024]", Some [ "float" ]);
    ]
  in
  List.iter
    (fun (given, expected) ->
      Alcotest.check
        Alcotest.(option (list string))
        ("parse_array_type_opt " ^ given)
        expected
        (parse_array_type_opt given))
    test_cases

let test_parse_array_dim_opt () : unit =
  let test_cases =
    [
      ("int *", None);
      ("int", None);
      ("unsigned int [8][8] *", Some [ 8; 8 ]);
      ("int [128]", Some [ 128 ]);
      ("float[1024]", Some [ 1024 ]);
    ]
  in
  List.iter
    (fun (given, expected) ->
      Alcotest.check
        Alcotest.(option (list int))
        ("parse_array_dim_opt " ^ given)
        expected
        (parse_array_dim_opt given))
    test_cases

let tests : unit Alcotest.test_case list =
  [
    ("split_array_type", `Quick, test_split_array_type);
    ("parse_array_type_opt", `Quick, test_parse_dim);
    ("parse_array_type_opt_1", `Quick, test_parse_array_type_opt);
    ("parse_array_split_opt_2", `Quick, test_parse_array_dim_opt);
  ]

let () = Alcotest.run "C_type" [ ("ctype", tests) ]
