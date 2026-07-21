open Protocols
module M = Assumption.Match
module P = Protocols_parsing.Assumption_parser

let bx (s : string) : Exp.bexp =
  Protocols_parsing.Parsers.BExpParser.of_string s |> Result.get_ok

let assume : Assumption.t Alcotest.testable =
  Alcotest.testable
    (fun fmt a -> Format.fprintf fmt "%s" (Assumption.to_string a))
    ( = )

(* Build the expected value; the bexp is parsed with the same parser so
   structural equality compares like with like. *)
let mk ?kernel ?binder ?line (bexp : string) : Assumption.t =
  let m = function None -> M.Any | Some x -> M.Exact x in
  let target =
    match binder with
    | None -> Assumption.Target.Pre
    | Some label -> Assumption.Target.Binder { label; line = m line }
  in
  { Assumption.kernel = m kernel; target; bexp = bx bexp }

let ok (input : string) (expected : Assumption.t) : unit Alcotest.test_case =
  ( input,
    `Quick,
    fun () ->
      match P.of_string input with
      | Ok a -> Alcotest.check assume input expected a
      | Error e -> Alcotest.failf "expected Ok for %S, got Error %S" input e )

let err (input : string) : unit Alcotest.test_case =
  ( input,
    `Quick,
    fun () ->
      match P.of_string input with
      | Ok a ->
          Alcotest.failf "expected Error for %S, got Ok %S" input
            (Assumption.to_string a)
      | Error _ -> () )

let accepts : unit Alcotest.test_case list =
  [
    ok "x < 10" (mk "x < 10");
    ok "kernel=foo: x < 10" (mk ~kernel:"foo" "x < 10");
    ok "binder=x: x < 10" (mk ~binder:"x" "x < 10");
    ok "kernel=foo,binder=x: x < 10" (mk ~kernel:"foo" ~binder:"x" "x < 10");
    ok "binder=x,line=100: x < 10" (mk ~binder:"x" ~line:100 "x < 10");
    ok "kernel=foo,binder=x,line=100: x < 10"
      (mk ~kernel:"foo" ~binder:"x" ~line:100 "x < 10");
    (* keys are order-independent *)
    ok "line=16,binder=i00,kernel=k_get_rows: i00 < ne00"
      (mk ~kernel:"k_get_rows" ~binder:"i00" ~line:16 "i00 < ne00");
    (* whitespace around keys, values, and the colon is trimmed *)
    ok "  kernel = foo , binder = x , line = 100 :  x < 10  "
      (mk ~kernel:"foo" ~binder:"x" ~line:100 "x < 10");
    ok "binder=nex_prev: nex_prev >= 0" (mk ~binder:"nex_prev" "nex_prev >= 0");
    (* registered predicates parse as boolean calls in the bexp:
       thread-uniformity, its negation, and the bvumul overflow guard *)
    ok "binder=max_expert: __uniform_int(max_expert)"
      (mk ~binder:"max_expert" "__uniform_int(max_expert)");
    ok "binder=x: __distinct_int(x)" (mk ~binder:"x" "__distinct_int(x)");
    ok "kernel=k: bvumul_noovfl(a, b)" (mk ~kernel:"k" "bvumul_noovfl(a, b)");
  ]

let rejects : unit Alcotest.test_case list =
  [
    err "";
    err "kernel=foo:";
    err ": x < 10";
    (* line= without binder= is unrepresentable, so a syntax error *)
    err "line=100: x < 10";
    (* bare kernel name, no key= *)
    err "foo: x < 10";
    err "kernl=foo: x < 10";
    err "binder=x,binder=y: x < 10";
    err "binder=: x < 10";
    err "binder=x,line=abc: x < 10";
    err "binder=x,line=-3: x < 10";
    (* the bexp itself fails to parse *)
    err "binder=x: )";
  ]

let () =
  Alcotest.run "Assumption_parser"
    [ ("accepts", accepts); ("rejects", rejects) ]
