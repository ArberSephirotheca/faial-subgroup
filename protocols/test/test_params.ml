open Protocols

let kind_of (v : Variable.t) : string =
  Variable.Kind.to_string (Variable.kind v)

let sole (vs : Variable.Set.t) : Variable.t =
  match Variable.Set.elements vs with
  | [ v ] -> v
  | _ -> Alcotest.fail "expected exactly one variable"

let test_reset_stamps_key_and_bound () =
  let n = Variable.from_name "n" in
  let params = Variable.Set.singleton n in
  let m = Params.add n C_type.int Params.empty in
  Alcotest.(check string)
    "key starts as decl" "decl"
    (kind_of (sole (Params.to_set m)));
  Alcotest.(check string)
    "bound starts as decl" "decl"
    (kind_of (sole (Exp.b_free_names (Params.to_bexp m) Variable.Set.empty)));
  let m = Params.reset_kind ~kernel_parameters:params m in
  Alcotest.(check string)
    "key stamped kernel-parameter" "kernel-parameter"
    (kind_of (sole (Params.to_set m)));
  Alcotest.(check string)
    "bound occurrence stamped kernel-parameter" "kernel-parameter"
    (kind_of (sole (Exp.b_free_names (Params.to_bexp m) Variable.Set.empty)))

let tests : unit Alcotest.test_case list =
  [
    ( "reset_kind stamps key and bound",
      `Quick,
      test_reset_stamps_key_and_bound );
  ]

let () = Alcotest.run "Params" [ ("params", tests) ]
