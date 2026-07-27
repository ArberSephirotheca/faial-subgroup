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

let kept (ty : Scalar.t) (n : int) : nexp = Convert { ty; arg = Num n }

let test_convert (name : string) (ty : Scalar.t) (n : int) (expected : nexp) =
  ( name,
    `Quick,
    fun () -> Alcotest.(check bool) name true (convert ty (Num n) = expected) )

let convert_tests =
  [
    test_convert "char lower bound is elided" Scalar.char (-128) (Num (-128));
    test_convert "char upper bound is elided" Scalar.char 127 (Num 127);
    test_convert "char below range is kept" Scalar.char (-129)
      (kept Scalar.char (-129));
    test_convert "char above range is kept" Scalar.char 128
      (kept Scalar.char 128);
    test_convert "unsigned char upper bound is elided" Scalar.unsigned_char 255
      (Num 255);
    test_convert "unsigned char rejects a negative" Scalar.unsigned_char (-1)
      (kept Scalar.unsigned_char (-1));
  ]

let test_subst_elides () =
  let x = Variable.from_name "x" in
  let e = Convert { ty = Scalar.char; arg = Var x } in
  Alcotest.(check bool) "an in-range literal loses the conversion" true
    (Subst.ReplacePair.n_subst (x, Num 5) e = Num 5)

let test_subst_keeps () =
  let x = Variable.from_name "x" in
  let e = Convert { ty = Scalar.char; arg = Var x } in
  Alcotest.(check bool) "an out-of-range literal keeps the conversion" true
    (Subst.ReplacePair.n_subst (x, Num 300) e = kept Scalar.char 300)

let subst_tests =
  [
    ("substituting an in-range literal", `Quick, test_subst_elides);
    ("substituting an out-of-range literal", `Quick, test_subst_keeps);
  ]

let all_tests =
  [
    ("precedence", precedence_tests);
    ("convert elision", convert_tests);
    ("convert under substitution", subst_tests);
  ]

(* Run the tests *)
let () = Alcotest.run "Expression" all_tests
