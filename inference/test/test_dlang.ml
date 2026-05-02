open Stage0
open Inference
open D_lang

let stmt : Stmt.t Alcotest.testable =
  let pp fmt stmt = Format.fprintf fmt "%s" (Stmt.to_string stmt) in
  let equal = ( = ) in
  Alcotest.testable pp equal

let test_last_and_skip_last () : unit =
  let open Stmt in
  let s = Stmt.from_list [ BreakStmt; GotoStmt; ContinueStmt ] in
  Alcotest.check stmt "last returns ContinueStmt" ContinueStmt (last s);
  Alcotest.check stmt "skip_last returns correct list"
    (Stmt.from_list [ BreakStmt; GotoStmt ])
    (skip_last s)

(* Pin the IntegerLiteral parser to specific values across the three
   widths it handles (OCaml int, signed Int64, uint64 reinterpreted as
   signed). A regression to the [Int.max_int] fallback would be caught
   here even when an end-to-end fixture's DRF verdict is unaffected. *)
let parse_int_literal (s : string) : int =
  let j : Yojson.Basic.t =
    `Assoc
      [
        ("kind", `String "IntegerLiteral");
        ("value", `String s);
        ("type", `Assoc [ ("qualType", `String "unsigned long long") ]);
      ]
  in
  match C_lang.parse_expr j with
  | Ok (IntegerLiteral i) -> i
  | Ok _ -> Alcotest.failf "parse_expr: expected IntegerLiteral for %s" s
  | Error e -> Alcotest.failf "parse_expr failed: %s" (Rjson.error_to_string e)

let test_integer_literal_parses_ocaml_int_range () : unit =
  Alcotest.(check int) "small positive" 42 (parse_int_literal "42");
  Alcotest.(check int) "small negative" (-42) (parse_int_literal "-42");
  Alcotest.(check int) "OCaml max_int" Int.max_int
    (parse_int_literal (string_of_int Int.max_int))

let test_integer_literal_parses_uint64_sentinel () : unit =
  (* [0xFFFFFFFFFFFFFFFFULL] (decimal [18446744073709551615]) exceeds
     signed Int64; it parses through the ["0u" ^ s] path and
     reinterprets to signed [-1], which fits OCaml [int]. *)
  Alcotest.(check int) "uint64 max" (-1)
    (parse_int_literal "18446744073709551615");
  (* Real-world sentinels observed in HeCBench logic-rewrite-cuda
     ([0xFF42E54B94E2DA0DULL] and [0xC4D7F9E2C7CDA4D3ULL]). The
     two's-complement signed values fit OCaml's 63-bit [int]. *)
  Alcotest.(check int) "logic-rewrite 0xFF42... sentinel"
    (-49064778989728563)
    (parse_int_literal "18397679294719823053");
  Alcotest.(check int) "logic-rewrite 0xC4D7... sentinel"
    (-4265267296055464877)
    (parse_int_literal "14181476777654086739")

let tests : unit Alcotest.test_case list =
  [
    ("last + skip_last", `Quick, test_last_and_skip_last);
    ( "IntegerLiteral: OCaml int range",
      `Quick,
      test_integer_literal_parses_ocaml_int_range );
    ( "IntegerLiteral: uint64 sentinel",
      `Quick,
      test_integer_literal_parses_uint64_sentinel );
  ]

let () = Alcotest.run "D_lang" [ ("dlang", tests) ]
