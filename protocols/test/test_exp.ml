open Protocols
open Protocols.Exp

(* Helper to create variables *)
let var (name : string) : nexp = Var (Variable.from_name name)

(* Test that demonstrates the serialization issue for the round-trip problem *)
let test_precedence_serialization () =
  let mask = var "mask" in

  (* Create the problematic expression: (mask & 255) == mask *)
  let original_expr =
    NRel (N_rel.Eq, Binary (N_binary.BitAnd, mask, Num 255), mask)
  in

  let serialized = b_to_string original_expr in

  Printf.printf "Original expression serialized as: '%s'\n" serialized;

  (* The issue is that this serializes to "mask & 255 == mask" which is ambiguous.
     It should ideally serialize to "(mask & 255) == mask" to preserve precedence. *)

  (* After the fix, it should produce unambiguous output with parentheses *)
  let expected_fixed = "(mask & 255) == mask" in

  (* This test should pass after fixing b_to_string *)
  Alcotest.(check string)
    "fixed serialization with parentheses" expected_fixed serialized

(* Test simple expressions that shouldn't need parentheses *)
let test_simple_serialization () =
  let x = var "x" in
  let y = var "y" in

  (* Simple equality: x == y *)
  let simple_eq = NRel (N_rel.Eq, x, y) in
  let serialized = b_to_string simple_eq in

  (* Should be straightforward serialization *)
  Alcotest.(check string) "simple equality" "x == y" serialized

let precedence_tests =
  [
    ("precedence serialization", `Quick, test_precedence_serialization);
    ("simple serialization", `Quick, test_simple_serialization);
  ]

let all_tests = [ ("precedence", precedence_tests) ]

(* Run the tests *)
let () = Alcotest.run "Expression" all_tests
