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
  e |> Delin.Expr.from_nexp ~globals |> Delin.Expr.to_nexp

let string_of_list (f : 'a -> string) (l : 'a list) : string =
  l |> List.map f |> String.concat "; " |> Printf.sprintf "[%s]"

let string_of_option (f : 'a -> string) : 'a option -> string = function
  | None -> "None"
  | Some x -> "Some " ^ f x

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
        |> Delin.Expr.from_nexp ~globals
        |> Delin.size_params
        |> List.map Delin.Term.to_nexp
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
        |> Delin.Expr.from_nexp ~globals
        |> Delin.size_params
        |> Delin.dims
        |> Option.map (List.map Delin.Term.to_nexp)
      in
      assert_equal
        ~msg
        ~printer:(string_of_option (string_of_list Exp.n_to_string))
        (Some expected) got)

let reconstruct_tests =
  "Index.reconstruct" >:: fun _ ->
  let open Build in
  let check ~msg before =
    let expr = Delin.Expr.from_nexp ~globals before in
    let size_params = Delin.size_params expr in
    match
      Delin.Greedy.candidates ~globals ~size_params expr |> Seq.uncons
    with
    | Some (idx, _) ->
      let rebuilt = Delin.Index.reconstruct idx in
      assert_equal
        ~msg
        ~printer:Exp.n_to_string
        (normalize before)
        (Delin.Expr.to_nexp rebuilt)
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
    Delin.Expr.from_nexp ~globals
      (vN * vM * x + vN * x + vM * y + y + z)
  in
  let size_params = Delin.size_params expr in
  match
    Delin.ICS15.candidates ~globals ~size_params expr
    |> Seq.uncons
  with
  | None -> assert_failure "ICS15 produced no candidate"
  | Some (idx, _) ->
    let rebuilt = Delin.Index.reconstruct idx in
    assert_equal
      ~msg:"reconstructed polynomial"
      ~printer:Exp.n_to_string
      (Delin.Expr.to_nexp
        (Delin.Expr.from_nexp ~globals
          (vN * vM * x + vN * x + vM * y + y + z)))
      (Delin.Expr.to_nexp rebuilt);
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
    Delin.Expr.from_nexp ~globals
      (vN * vM * x + vN * x + vM * y + y + z)
  in
  let size_params = Delin.size_params expr in
  match
    Delin.Greedy.candidates ~globals ~size_params expr |> Seq.uncons
  with
  | None -> ()  (* expected *)
  | Some _ ->
    assert_failure "Greedy produced a candidate; expected Seq.empty"

let tests =
  "delin" >::: [
    stage1_tests;
    stage2_tests;
    reconstruct_tests;
    grosser_offset_test;
    greedy_fails_on_offset_test;
  ]

let _ = run_test_tt_main tests
