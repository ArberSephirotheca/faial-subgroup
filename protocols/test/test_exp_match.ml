open Protocols
open Exp

let v (s : string) : Variable.t = Variable.from_name s
let var (s : string) : nexp = Var (v s)
let num (n : int) : nexp = Num n
let ( +@ ) a b = Binary (N_binary.Plus Signedness.Unsigned, a, b)
let ( -@ ) a b = Binary (N_binary.Minus Signedness.Signed, a, b)
let band a b = Binary (N_binary.BitAnd, a, b)
let bor a b = Binary (N_binary.BitOr, a, b)
let shr a b = Binary (N_binary.RightShift Signedness.Signed, a, b)
let ushr a b = Binary (N_binary.RightShift Signedness.Unsigned, a, b)
let shl a b = Binary (N_binary.LeftShift, a, b)
let udiv a b = Binary (N_binary.Div Signedness.Unsigned, a, b)
let gt a b = NRel (N_rel.Gt Signedness.Signed, a, b)
let umulhi a b = NCall ("__umulhi", [ a; b ])
let mul24 a b = NCall ("__mul24", [ a; b ])

let results (pat : nexp) (subj : nexp) : Exp_match.subst list =
  Exp_match.matches pat subj |> List.of_seq

let nexp_t : nexp Alcotest.testable =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt (Exp.n_to_string e))
    Exp_match.n_equal

let check_matched (name : string) (pat : nexp) (subj : nexp) : unit =
  Alcotest.(check bool) name true (results pat subj <> [])

let check_no_match (name : string) (pat : nexp) (subj : nexp) : unit =
  Alcotest.(check bool) name true (results pat subj = [])

let check_binding (name : string) (pat : nexp) (subj : nexp) (hole : string)
    (expected : nexp) : unit =
  match results pat subj with
  | [] -> Alcotest.failf "%s: expected a match, got none" name
  | subst :: _ -> (
      match Variable.Map.find_opt (v hole) subst with
      | Some actual -> Alcotest.check nexp_t name expected actual
      | None -> Alcotest.failf "%s: hole %s not bound" name hole)

let bexp_t : bexp Alcotest.testable =
  Alcotest.testable
    (fun fmt b -> Format.pp_print_string fmt (Exp.b_to_string b))
    Exp_match.b_equal

let raises (f : unit -> 'a) : bool =
  try
    ignore (f ());
    false
  with _ -> true

let first_subst (pat : nexp) (subj : nexp) : Exp_match.subst =
  match results pat subj with
  | s :: _ -> s
  | [] -> Alcotest.fail "expected a match"

let check_apply (name : string) (rule : Exp_match.rule) (subj : nexp)
    (expected_rhs : nexp) (expected_emits : bexp list) : unit =
  match Exp_match.apply_rule rule subj with
  | None -> Alcotest.failf "%s: expected the rule to fire" name
  | Some (rhs, emits) ->
      Alcotest.check nexp_t (name ^ " rhs") expected_rhs rhs;
      Alcotest.check (Alcotest.list bexp_t) (name ^ " emits") expected_emits
        emits

let check_no_apply (name : string) (rule : Exp_match.rule) (subj : nexp) : unit
    =
  Alcotest.(check bool) name true (Exp_match.apply_rule rule subj = None)

let is_num (name : string) (s : Exp_match.subst) : bool =
  match Variable.Map.find_opt (v name) s with
  | Some (Num _) -> true
  | _ -> false

let literal_first : Exp_match.rule =
  {
    lhs = var "?a" +@ var "?b";
    fire = (fun s -> if is_num "?a" s then Some s else None);
    rhs = var "?a";
    emits = [ gt (var "?b") (num 0) ];
  }

let never : Exp_match.rule =
  {
    lhs = var "?a" +@ var "?b";
    fire = (fun _ -> None);
    rhs = num 0;
    emits = [];
  }

let third_field : Exp_match.rule =
  {
    lhs = var "?FD.x" +@ var "?FD.y";
    fire = (fun s -> Some s);
    rhs = var "?FD.z";
    emits = [];
  }

let const_rule (n : int) : Exp_match.rule =
  {
    lhs = var "?a" +@ var "?b";
    fire = (fun s -> Some s);
    rhs = num n;
    emits = [];
  }

let instantiation_tests =
  [
    ( "instantiating the matched pattern reproduces the subject",
      `Quick,
      fun () ->
        let pat = ushr (umulhi (var "?N") (var "?x") +@ var "?N") (var "?y") in
        let subj = ushr (umulhi (var "n") (num 7) +@ var "n") (num 1) in
        Alcotest.check nexp_t "round-trip" subj
          (Exp_match.instantiate (first_subst pat subj) pat) );
    ( "field hole instantiates to base.field",
      `Quick,
      fun () ->
        let s = first_subst (var "?FD.x") (var "fd.x") in
        Alcotest.check nexp_t "?FD.z" (var "fd.z")
          (Exp_match.instantiate s (var "?FD.z")) );
    ( "instantiate fills plain holes in a template",
      `Quick,
      fun () ->
        let s =
          Variable.Map.add (v "?N") (var "n")
            (Variable.Map.add (v "?z") (num 8) Exp_match.empty)
        in
        Alcotest.check nexp_t "n /u 8"
          (udiv (var "n") (num 8))
          (Exp_match.instantiate s (udiv (var "?N") (var "?z"))) );
    ( "instantiate raises on an unbound hole",
      `Quick,
      fun () ->
        Alcotest.(check bool)
          "raises" true
          (raises (fun () -> Exp_match.instantiate Exp_match.empty (var "?zz")))
    );
  ]

let apply_tests =
  [
    ( "fire veto selects the literal operand under commutativity",
      `Quick,
      fun () ->
        check_apply "literal-first" literal_first
          (var "x" +@ num 5)
          (num 5)
          [ gt (var "x") (num 0) ] );
    ( "apply_rule returns None when fire always vetoes",
      `Quick,
      fun () -> check_no_apply "never" never (var "x" +@ var "y") );
    ( "apply instantiates a field hole in the rhs",
      `Quick,
      fun () ->
        check_apply "third-field" third_field
          (var "fd.x" +@ var "fd.y")
          (var "fd.z") [] );
    ( "apply_rules is first-rule-wins",
      `Quick,
      fun () ->
        let subj = var "x" +@ var "y" in
        (match Exp_match.apply_rules [ const_rule 1; const_rule 2 ] subj with
        | Some (rhs, _) -> Alcotest.check nexp_t "first wins" (num 1) rhs
        | None -> Alcotest.fail "expected a match");
        match Exp_match.apply_rules [ const_rule 2; const_rule 1 ] subj with
        | Some (rhs, _) ->
            Alcotest.check nexp_t "order flips winner" (num 2) rhs
        | None -> Alcotest.fail "expected a match" );
    ( "apply_rules on an empty list returns None",
      `Quick,
      fun () ->
        Alcotest.(check bool)
          "empty" true
          (Exp_match.apply_rules [] (var "x") = None) );
  ]

let plain_hole_tests =
  [
    ( "hole binds any subterm",
      `Quick,
      fun () -> check_binding "bind ?a" (var "?a") (var "x") "?a" (var "x") );
    ( "hole binds a compound subterm",
      `Quick,
      fun () ->
        check_binding "bind ?a" (var "?a")
          (var "x" +@ num 1)
          "?a"
          (var "x" +@ num 1) );
    ( "literal variable matches itself",
      `Quick,
      fun () -> check_matched "n ~ n" (var "n") (var "n") );
    ( "literal variable rejects a different variable",
      `Quick,
      fun () -> check_no_match "n ~ m" (var "n") (var "m") );
    ( "literal numeral matches",
      `Quick,
      fun () -> check_matched "1 ~ 1" (num 1) (num 1) );
    ( "literal numeral rejects a different numeral",
      `Quick,
      fun () -> check_no_match "1 ~ 2" (num 1) (num 2) );
  ]

let nonlinear_tests =
  [
    ( "repeated hole accepts equal subterms",
      `Quick,
      fun () ->
        check_binding "?a + ?a"
          (var "?a" +@ var "?a")
          (var "x" +@ var "x")
          "?a" (var "x") );
    ( "repeated hole rejects unequal subterms",
      `Quick,
      fun () ->
        check_no_match "?a + ?a" (var "?a" +@ var "?a") (var "x" +@ var "y") );
  ]

let commutativity_tests =
  [
    ( "plus matches swapped operands",
      `Quick,
      fun () ->
        check_binding "?a + b"
          (var "?a" +@ var "b")
          (var "b" +@ var "x")
          "?a" (var "x") );
    ( "bitand matches swapped operands",
      `Quick,
      fun () ->
        check_binding "?x & 1"
          (band (var "?x") (num 1))
          (band (num 1) (var "a"))
          "?x" (var "a") );
    ( "minus does not match swapped operands",
      `Quick,
      fun () ->
        check_no_match "?a - b" (var "?a" -@ var "b") (var "b" -@ var "x") );
  ]

let shift_tests =
  [
    ( "shift-right binds base and amount",
      `Quick,
      fun () ->
        let pat = shr (var "?a") (var "?k") in
        check_binding "?a" pat (shr (var "x") (num 3)) "?a" (var "x");
        check_binding "?k" pat (shr (var "x") (num 3)) "?k" (num 3) );
    ( "shift-right amount may be non-literal (builder vetoes, not matcher)",
      `Quick,
      fun () ->
        check_binding "?k <- m"
          (shr (var "?a") (var "?k"))
          (shr (var "x") (var "m"))
          "?k" (var "m") );
    ( "shift-right rejects shift-left",
      `Quick,
      fun () ->
        check_no_match "shr ~ shl"
          (shr (var "?a") (var "?k"))
          (shl (var "x") (num 3)) );
    ( "shift-right is signedness-exact",
      `Quick,
      fun () ->
        check_no_match "signed ~ unsigned"
          (shr (var "?a") (var "?k"))
          (ushr (var "x") (num 3)) );
    ( "shift-left binds base and amount",
      `Quick,
      fun () ->
        check_binding "?k"
          (shl (var "?a") (var "?k"))
          (shl (var "x") (num 2))
          "?k" (num 2) );
  ]

let bitwise_idiom_tests =
  [
    ( "x & 1 matches",
      `Quick,
      fun () ->
        check_binding "?x"
          (band (var "?x") (num 1))
          (band (var "a") (num 1))
          "?x" (var "a") );
    ( "x & 1 rejects x & 2",
      `Quick,
      fun () ->
        check_no_match "& 2" (band (var "?x") (num 1)) (band (var "a") (num 2))
    );
    ( "x & 1 rejects x | 1",
      `Quick,
      fun () ->
        check_no_match "| 1" (band (var "?x") (num 1)) (bor (var "a") (num 1))
    );
    ( "n & (n-1) matches with the same n",
      `Quick,
      fun () ->
        let pat = band (var "?n") (var "?n" -@ num 1) in
        check_binding "?n" pat
          (band (var "k") (var "k" -@ num 1))
          "?n" (var "k") );
    ( "n & (n-1) matches swapped (commutative &)",
      `Quick,
      fun () ->
        let pat = band (var "?n") (var "?n" -@ num 1) in
        check_matched "swapped" pat (band (var "k" -@ num 1) (var "k")) );
    ( "n & (n-1) rejects a & (b-1)",
      `Quick,
      fun () ->
        let pat = band (var "?n") (var "?n" -@ num 1) in
        check_no_match "a<>b" pat (band (var "a") (var "b" -@ num 1)) );
    ( "n & (n-1) rejects n & (n+1)",
      `Quick,
      fun () ->
        let pat = band (var "?n") (var "?n" -@ num 1) in
        check_no_match "plus" pat (band (var "k") (var "k" +@ num 1)) );
  ]

let field_tests =
  [
    ( "field hole binds the base",
      `Quick,
      fun () -> check_binding "?X.x" (var "?X.x") (var "fd.x") "?X" (var "fd")
    );
    ( "field hole rejects a different field",
      `Quick,
      fun () -> check_no_match "?X.x ~ fd.y" (var "?X.x") (var "fd.y") );
    ( "field hole rejects a non-variable subject",
      `Quick,
      fun () -> check_no_match "?X.x ~ 3" (var "?X.x") (num 3) );
    ( "same base ties two field holes",
      `Quick,
      fun () ->
        let pat = var "?X.x" +@ var "?X.y" in
        check_binding "?X" pat (var "fd.x" +@ var "fd.y") "?X" (var "fd") );
    ( "different bases reject the sibling tie",
      `Quick,
      fun () ->
        let pat = var "?X.x" +@ var "?X.y" in
        check_no_match "fd vs gd" pat (var "fd.x" +@ var "gd.y") );
    ( "literal dotted variable is matched exactly, not as a field hole",
      `Quick,
      fun () ->
        check_matched "blockDim.x" (var "blockDim.x") (var "blockDim.x");
        check_no_match "blockDim.x ~ gridDim.x" (var "blockDim.x")
          (var "gridDim.x") );
  ]

let fastdiv_tests =
  let magic = num 2863311531 in
  [
    ( "fastdiv matches and binds N, x, y",
      `Quick,
      fun () ->
        let pat = ushr (umulhi (var "?N") (var "?x") +@ var "?N") (var "?y") in
        let subj = ushr (umulhi (var "n") magic +@ var "n") (num 1) in
        check_binding "?N" pat subj "?N" (var "n");
        check_binding "?x" pat subj "?x" magic;
        check_binding "?y" pat subj "?y" (num 1) );
    ( "fastdiv matches with the addend written first (commutative +)",
      `Quick,
      fun () ->
        let pat = ushr (umulhi (var "?N") (var "?x") +@ var "?N") (var "?y") in
        let subj = ushr (var "n" +@ umulhi (var "n") magic) (num 1) in
        check_binding "?N" pat subj "?N" (var "n") );
    ( "fastdiv rejects mismatched N occurrences",
      `Quick,
      fun () ->
        let pat = ushr (umulhi (var "?N") (var "?x") +@ var "?N") (var "?y") in
        let subj = ushr (umulhi (var "a") magic +@ var "b") (num 1) in
        check_no_match "a<>b" pat subj );
    ( "fastdiv rejects a different intrinsic",
      `Quick,
      fun () ->
        let pat = ushr (umulhi (var "?N") (var "?x") +@ var "?N") (var "?y") in
        let subj = ushr (mul24 (var "n") magic +@ var "n") (num 1) in
        check_no_match "mul24" pat subj );
    ( "fastdiv rejects a left shift outer op",
      `Quick,
      fun () ->
        let pat = ushr (umulhi (var "?N") (var "?x") +@ var "?N") (var "?y") in
        let subj = shl (umulhi (var "n") magic +@ var "n") (num 1) in
        check_no_match "shl" pat subj );
    ( "fastdiv with struct-field multiplier and shift ties the struct",
      `Quick,
      fun () ->
        let pat =
          ushr (umulhi (var "?N") (var "?FD.x") +@ var "?N") (var "?FD.y")
        in
        let subj =
          ushr (umulhi (var "n") (var "fd.x") +@ var "n") (var "fd.y")
        in
        check_binding "?N" pat subj "?N" (var "n");
        check_binding "?FD" pat subj "?FD" (var "fd") );
    ( "fastdiv rejects fields from different structs",
      `Quick,
      fun () ->
        let pat =
          ushr (umulhi (var "?N") (var "?FD.x") +@ var "?N") (var "?FD.y")
        in
        let subj =
          ushr (umulhi (var "n") (var "fd.x") +@ var "n") (var "gd.y")
        in
        check_no_match "fd vs gd" pat subj );
    ( "fastdiv without the correction term matches",
      `Quick,
      fun () ->
        let pat = ushr (umulhi (var "?N") (var "?x")) (var "?y") in
        let subj = ushr (umulhi (var "n") magic) (num 1) in
        check_binding "?N" pat subj "?N" (var "n") );
  ]

let () =
  Alcotest.run "exp_match"
    [
      ("plain holes", plain_hole_tests);
      ("non-linear", nonlinear_tests);
      ("commutativity", commutativity_tests);
      ("shift rewrites", shift_tests);
      ("bitwise idioms", bitwise_idiom_tests);
      ("field holes", field_tests);
      ("fastdiv", fastdiv_tests);
      ("instantiation", instantiation_tests);
      ("apply", apply_tests);
    ]
