open Protocols
open Exp
open Imp

let v (s : string) : Variable.t = Variable.from_name s
let var (s : string) : nexp = Var (v s)
let num (n : int) : nexp = Num n
let ( +@ ) a b = Binary (N_binary.Plus Signedness.Unsigned, a, b)
let ( *@ ) a b = Binary (N_binary.Mult Signedness.Unsigned, a, b)
let ( -@ ) a b = Binary (N_binary.Minus Signedness.Unsigned, a, b)
let ushr a b = Binary (N_binary.RightShift Signedness.Unsigned, a, b)
let udiv a b = Binary (N_binary.Div Signedness.Unsigned, a, b)
let gt a b = NRel (N_rel.Gt Signedness.Signed, a, b)
let uge a b = NRel (N_rel.Ge Signedness.Unsigned, a, b)
let umulhi a b = NCall ("__umulhi", [ a; b ])

let nexp_t : nexp Alcotest.testable =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt (Exp.n_to_string e))
    Exp_match.n_equal

let bexp_t : bexp Alcotest.testable =
  Alcotest.testable
    (fun fmt b -> Format.pp_print_string fmt (Exp.b_to_string b))
    Exp_match.b_equal

let access (idx : nexp list) : Encode_assigns.t =
  Encode_assigns.Access { array = v "A"; index = idx; mode = Access.Mode.Read }

let rec index_of : Encode_assigns.t -> nexp list = function
  | Encode_assigns.Access a -> a.Access.index
  | Encode_assigns.Seq (_, s) -> index_of s
  | _ -> []

let drop_shift : Exp_match.rule =
  {
    lhs = ushr (var "?a") (var "?k");
    fire = (fun s -> Some s);
    rhs = var "?a";
    emits = [ gt (var "?k") (num 0) ];
  }

let fastdiv_expr : nexp =
  ushr (umulhi (var "n") (var "fdv.x") +@ var "n") (var "fdv.y")

let driver_tests =
  [
    ( "no rules leaves the tree physically unchanged",
      `Quick,
      fun () ->
        let prog = access [ ushr (var "x") (num 3) ] in
        Alcotest.(check bool)
          "identity" true
          (Idiom_rewrite.rewrite [] prog == prog) );
    ( "a program with no matching idiom is physically unchanged",
      `Quick,
      fun () ->
        let prog = access [ var "x" +@ num 1 ] in
        Alcotest.(check bool)
          "identity" true
          (Idiom_rewrite.rewrite [ drop_shift ] prog == prog) );
    ( "a matching idiom rewrites the index and prepends one global assert",
      `Quick,
      fun () ->
        let prog = access [ ushr (var "x") (num 3) ] in
        match Idiom_rewrite.rewrite [ drop_shift ] prog with
        | Encode_assigns.Seq (Encode_assigns.Assert a, rest) ->
            Alcotest.(check bool)
              "global visibility" true
              (a.Assert.visibility = Assert.Visibility.Global);
            Alcotest.check bexp_t "assert is ?k > 0 instantiated"
              (gt (num 3) (num 0)) a.Assert.cond;
            Alcotest.check (Alcotest.list nexp_t) "index rewritten to the base"
              [ var "x" ] (index_of rest)
        | other ->
            Alcotest.failf "unexpected shape: %s"
              (Encode_assigns.to_string other) );
    ( "duplicate emits within one expression are deduped to a single assert",
      `Quick,
      fun () ->
        let prog = access [ ushr (var "x") (num 3); ushr (var "y") (num 3) ] in
        match Idiom_rewrite.rewrite [ drop_shift ] prog with
        | Encode_assigns.Seq (Encode_assigns.Assert _, rest) ->
            Alcotest.check (Alcotest.list nexp_t)
              "both indices rewritten, one assert" [ var "x"; var "y" ]
              (index_of rest)
        | other ->
            Alcotest.failf "expected a single prepended assert, got: %s"
              (Encode_assigns.to_string other) );
  ]

let fastdiv_tests =
  [
    ( "fastdiv rewrites to unsigned division by the packed divisor",
      `Quick,
      fun () ->
        match Exp_match.apply_rules Idiom_rewrite.all fastdiv_expr with
        | Some (rhs, emits) ->
            Alcotest.check nexp_t "rhs" (udiv (var "n") (var "fdv.z")) rhs;
            Alcotest.check (Alcotest.list bexp_t) "emits"
              [ uge (var "fdv.z") (num 1) ]
              emits
        | None -> Alcotest.fail "expected fastdiv to fire" );
    ( "fastdiv fires inside a strided index and prepends the divisor bound",
      `Quick,
      fun () ->
        let prog = access [ fastdiv_expr *@ var "stride" ] in
        match Idiom_rewrite.rewrite Idiom_rewrite.all prog with
        | Encode_assigns.Seq (Encode_assigns.Assert a, _) as res ->
            Alcotest.check bexp_t "assert" (uge (var "fdv.z") (num 1))
              a.Assert.cond;
            Alcotest.check (Alcotest.list nexp_t) "index"
              [ udiv (var "n") (var "fdv.z") *@ var "stride" ]
              (index_of res)
        | other ->
            Alcotest.failf "unexpected: %s" (Encode_assigns.to_string other) );
    ( "fastmodulo is handled for free via the inner fastdiv",
      `Quick,
      fun () ->
        let modexpr = var "n" -@ (fastdiv_expr *@ var "fdv.z") in
        let res = Idiom_rewrite.rewrite Idiom_rewrite.all (access [ modexpr ]) in
        Alcotest.check (Alcotest.list nexp_t) "n - (n/d)*d"
          [ var "n" -@ (udiv (var "n") (var "fdv.z") *@ var "fdv.z") ]
          (index_of res) );
  ]

let loader_tests =
  [
    ( "the embedded fastdiv text parses to a single rule",
      `Quick,
      fun () ->
        match Idiom_rewrite.parse Idiom_rewrite.fastdiv_text with
        | Ok [ _ ] -> ()
        | Ok rs -> Alcotest.failf "expected one rule, got %d" (List.length rs)
        | Error e -> Alcotest.failf "parse failed: %s" e );
    ( "a rule referencing an unbound hole is rejected",
      `Quick,
      fun () ->
        match Idiom_rewrite.parse "$a => $b ;" with
        | Error _ -> ()
        | Ok _ -> Alcotest.fail "expected an unbound-hole error" );
    ( "a syntactically invalid rule is rejected",
      `Quick,
      fun () ->
        match Idiom_rewrite.parse "this is not a rule" with
        | Error _ -> ()
        | Ok _ -> Alcotest.fail "expected a parse error" );
    ( "a parsed rule applies like a built-in",
      `Quick,
      fun () ->
        match Idiom_rewrite.parse "$a >>u $k => $a ;" with
        | Ok [ r ] -> (
            match Exp_match.apply_rule r (ushr (var "x") (num 3)) with
            | Some (rhs, []) -> Alcotest.check nexp_t "rhs" (var "x") rhs
            | _ -> Alcotest.fail "expected the parsed rule to fire")
        | _ -> Alcotest.fail "parse failed" );
  ]

let () =
  Alcotest.run "idiom_rewrite"
    [
      ("driver", driver_tests);
      ("fastdiv", fastdiv_tests);
      ("loader", loader_tests);
    ]
