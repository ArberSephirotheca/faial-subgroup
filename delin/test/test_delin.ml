open Protocols
open Exp
open OUnit2

module Build = struct
  let var x = Var (Variable.from_name x)
  let ( + ) a b = Binary (Plus Signedness.Signed, a, b)
  let ( * ) a b = Binary (Mult Signedness.Signed, a, b)

  let x = var "x"
  let y = var "y"
  let z = var "z"
  let vM = var "M"
  let vN = var "N"
end

let globals = Variable.Set.of_list (["M"; "N"] |> List.map Variable.from_name)

let normalize (e : nexp) : nexp =
  e |> Poly.from_nexp ~globals |> Poly.to_nexp

let string_of_list (f : 'a -> string) (l : 'a list) : string =
  l |> List.map f |> String.concat "; " |> Printf.sprintf "[%s]"

let string_of_option (f : 'a -> string) : 'a option -> string = function
  | None -> "None"
  | Some x -> "Some " ^ f x

let string_of_index (i : Index.t) : string =
  let exprs es =
    string_of_list (fun e -> Exp.n_to_string (Poly.to_nexp e)) es
  in
  Printf.sprintf "{indices=%s; dims=%s}" (exprs i.indices) (exprs i.dims)

let expr_list_eq (a : Poly.t list) (b : Poly.t list) : bool =
  List.length a = List.length b
  && List.for_all2 (fun x y -> Poly.compare x y = 0) a b

let index_eq (i1 : Index.t) (i2 : Index.t) : bool =
  expr_list_eq i1.indices i2.indices
  && expr_list_eq i1.dims i2.dims
  && i1.conditions = i2.conditions

(* Stage 1: per-access size_params extracts the parameter-only portion of
   every term that mixes an induction variable with a parameter. *)
let size_param_examples : (string * nexp * nexp list) list =
  let open Build in
  [
    "constant", Num 1, [];
    "linear", x, [];
    "affine", x + Num 1, [];
    "constdim", Num 10 * x + y, [];
    "numdim", vN * x + y, [vN];
    "3dim", vM * vN * x + vN * y + z, [vM * vN; vN];
    "3dim+dist", vN * (vM * x + y) + z, [vM * vN; vN];
    "duplicate", vN * (x + y) + z, [vN];
  ]
  |> List.map (fun (l, b, a) -> (l, b, List.map normalize a))

(* Stage 2: dims divides successive size_params pairs to recover per-axis
   dimensions. *)
let dim_examples : (string * nexp * nexp list) list =
  let open Build in
  [
    "constant", Num 1, [];
    "linear", x, [];
    "affine", x + Num 1, [];
    "constdim", Num 10 * x + y, [];
    "numdim", vN * x + y, [vN];
    "numdim_scaled", Num 10 * vN * x + y, [Num 10 * vN];
    "numdim_nested", Num 10 * (vM * x + y) + z, [Num 10 * vM];
    "3dim", vM * vN * x + vN * y + z, [vM; vN];
    "3dim+dist", vN * (vM * x + y) + z, [vM; vN];
    "duplicate", vN * (x + y) + z, [vN];
  ]
  |> List.map (fun (l, b, a) -> (l, b, List.map normalize a))

let stage1_tests =
  "size_params (stage1)" >:: fun _ ->
  size_param_examples
  |> List.iter (fun (msg, exp, params) ->
      let got =
        exp
        |> Poly.from_nexp ~globals
        |> Shape.size_params
        |> List.map Mono.to_nexp
      in
      assert_equal
        ~msg
        ~printer:(string_of_list Exp.n_to_string)
        params got)

let stage2_tests =
  "dims (stage2)" >:: fun _ ->
  dim_examples
  |> List.iter (fun (msg, exp, expected) ->
      let got =
        exp
        |> Poly.from_nexp ~globals
        |> Shape.size_params
        |> Shape.dims
        |> Option.map (List.map Mono.to_nexp)
      in
      assert_equal
        ~msg
        ~printer:(string_of_option (string_of_list Exp.n_to_string))
        (Some expected) got)

let reconstruct_tests =
  "Index.reconstruct" >:: fun _ ->
  let open Build in
  let check ~msg before =
    let expr = Poly.from_nexp ~globals before in
    let size_params = Shape.size_params expr in
    match
      Greedy.candidates ~globals ~size_params expr |> Seq.uncons
    with
    | Some (idx, _) ->
      let rebuilt = Index.reconstruct idx in
      assert_equal
        ~msg
        ~printer:Exp.n_to_string
        (normalize before)
        (Poly.to_nexp rebuilt)
    | None ->
      failwith "test fixture: Greedy returned no candidate"
  in
  check ~msg:"linear" x;
  check ~msg:"affine" (x + Num 1);
  check ~msg:"numdim" (vN * x + y);
  check ~msg:"3dim" (vM * vN * x + vN * y + z);
  check ~msg:"3dim+dist" (vN * (vM * x + y) + z)

(* ICS15 should reconstruct shape A[?][N][M+1] from the expansion
   x*N*(M+1) + y*(M+1) + z = N*M*x + N*x + M*y + y + z, where
   Greedy's pairwise [try_div] fails (size_params {N*M, N, M} don't
   form a divisibility chain). *)
let grosser_offset_test =
  "ICS15.candidates handles A[?][N][M+1]" >:: fun _ ->
  let open Build in
  let expr =
    Poly.from_nexp ~globals
      (vN * vM * x + vN * x + vM * y + y + z)
  in
  let size_params = Shape.size_params expr in
  match
    Ics15.candidates ~globals ~size_params expr
    |> Seq.uncons
  with
  | None -> assert_failure "ICS15 produced no candidate"
  | Some (idx, _) ->
    let rebuilt = Index.reconstruct idx in
    assert_equal
      ~msg:"reconstructed polynomial"
      ~printer:Exp.n_to_string
      (Poly.to_nexp
        (Poly.from_nexp ~globals
          (vN * vM * x + vN * x + vM * y + y + z)))
      (Poly.to_nexp rebuilt);
    assert_equal
      ~msg:"index count"
      ~printer:string_of_int
      3 (List.length idx.indices);
    assert_equal
      ~msg:"dim count"
      ~printer:string_of_int
      2 (List.length idx.dims)

(* Greedy alone fails on the same input. *)
let greedy_fails_on_offset_test =
  "Greedy.candidates fails on A[?][N][M+1]" >:: fun _ ->
  let open Build in
  let expr =
    Poly.from_nexp ~globals
      (vN * vM * x + vN * x + vM * y + y + z)
  in
  let size_params = Shape.size_params expr in
  match
    Greedy.candidates ~globals ~size_params expr |> Seq.uncons
  with
  | None -> ()  (* expected *)
  | Some _ ->
    assert_failure "Greedy produced a candidate; expected Seq.empty"

(* The optimized ICS15 driver must yield the same first candidate as
   the reference on every input: same Some/None, and when both produce
   a candidate the same indices and dims. [expect] pins the outcome
   only where it is certain (delinearizable shapes; degenerate shapes
   with no candidate); elsewhere it just demands the two drivers agree.
   When both produce a candidate we also confirm the optimized one
   actually reconstructs the input, so a shared regression to "both
   None" or "both wrong" cannot pass unnoticed. *)
let ics15_differential_tests =
  "Ics15 vs Ics15_opt agree" >:: fun _ ->
  let open Build in
  let head s = s |> Seq.uncons |> Option.map fst in
  let check ?(expect = `Any) ~msg before =
    let expr = Poly.from_nexp ~globals before in
    let size_params = Shape.size_params expr in
    let ref_idx = head (Ics15.candidates ~globals ~size_params expr) in
    let opt_idx = head (Ics15_opt.candidates ~globals ~size_params expr) in
    (match ref_idx, opt_idx with
     | None, None ->
       assert_bool (msg ^ ": expected a candidate, got none") (expect <> `Some)
     | Some r, Some o ->
       assert_bool (msg ^ ": expected no candidate") (expect <> `None);
       assert_bool
         (Printf.sprintf "%s: drivers disagree\n  ref=%s\n  opt=%s" msg
            (string_of_index r) (string_of_index o))
         (index_eq r o);
       assert_equal ~msg:(msg ^ ": opt reconstructs input")
         ~printer:Exp.n_to_string
         (normalize before)
         (Poly.to_nexp (Index.reconstruct o))
     | _ ->
       assert_failure
         (Printf.sprintf "%s: only one driver produced a candidate (ref=%s opt=%s)"
            msg
            (string_of_option string_of_index ref_idx)
            (string_of_option string_of_index opt_idx)))
  in
  check ~msg:"constant" ~expect:`Some (Num 1);
  check ~msg:"linear" ~expect:`Some x;
  check ~msg:"affine" ~expect:`Some (x + Num 1);
  check ~msg:"constdim" ~expect:`Some (Num 10 * x + y);
  check ~msg:"numdim" ~expect:`Some (vN * x + y);
  check ~msg:"numdim_scaled" ~expect:`Some (Num 10 * vN * x + y);
  check ~msg:"3dim" (vM * vN * x + vN * y + z);
  check ~msg:"3dim+dist" (vN * (vM * x + y) + z);
  check ~msg:"grosser_offset" ~expect:`Some
    (vN * vM * x + vN * x + vM * y + y + z);
  (* f0 = bucket(all candidates) vanishes: no N*M monomial, so no
     permutation reconstructs. Exercises the (B) early exit. *)
  check ~msg:"f0_zero" ~expect:`None (vN * x + vM * y + z);
  (* Both N and M have an undefined quotient, so neither can leave the
     other off position 0. Exercises the (C) two-bad short circuit. *)
  check ~msg:"two_bad" ~expect:`None (vN * vM * x + vN * y + vM * z)

let tests =
  "delin" >::: [
    stage1_tests;
    stage2_tests;
    reconstruct_tests;
    grosser_offset_test;
    greedy_fails_on_offset_test;
    ics15_differential_tests;
  ]

let _ = run_test_tt_main tests
