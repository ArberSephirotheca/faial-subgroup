open Protocols
open Exp
open Drf
open OUnit2

module Build = struct
  let var x = Var (Variable.from_name x)
  let ( + ) a b = Binary (Plus Signedness.Signed, a, b)
  let ( * ) a b = Binary (Mult Signedness.Signed, a, b)

  let x = var "x"
  let y = var "y"
  let z = var "z"
  let m = var "m"
  let n = var "n"
  let vM = var "M"
  let vN = var "N"
  let aA = Variable.from_name "A"
end

let globals = Variable.Set.of_list (["M"; "N"] |> List.map Variable.from_name)

let normalize (e : nexp) : nexp =
  e |> Delinearize.Expr.from_nexp ~globals |> Delinearize.Expr.to_nexp

let bound (i : nexp) (d : nexp) : bexp =
  b_and (n_le (Num 0) i) (n_lt i d)

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

(* Stage 3: end-to-end on a single access expression. Uses [AllBounds]
   so [t.conditions] contains the full per-axis bound list. *)
let delin ~globals (e : nexp) : Delinearize.t option =
  let expr = Delinearize.Expr.from_nexp ~globals e in
  let size_params = Delinearize.size_params expr in
  Delinearize.All.from_exp
    ~globals
    ~scope:Delinearize.AllBounds.initial_scope
    ~size_params
    expr

let positive_examples : (string * nexp * Delinearize.t) list =
  let open Build in
  [
    "constant", Num 1, [Num 1], [], [];
    "linear", x, [x], [], [];
    "affine", x + Num 1, [x + Num 1], [], [];
    "constdim", Num 10 * x + y, [Num 10 * x + y], [], [];
    "numdim", vN * x + y, [x; y], [vN], [bound y vN];
    "numdim_scaled", Num 10 * vN * x + y, [x; y], [Num 10 * vN],
      [bound y (Num 10 * vN)];
    "numdim_nested", Num 10 * (vM * x + y) + z, [x; Num 10 * y + z],
      [Num 10 * vM], [bound (Num 10 * y + z) (Num 10 * vM)];
    "3dim", vM * vN * x + vN * y + z, [x; y; z], [vM; vN],
      [bound y vM; bound z vN];
    "3dim+dist", vN * (vM * x + y) + z, [x; y; z], [vM; vN],
      [bound y vM; bound z vN];
    "duplicate", vN * (x + y) + z, [x + y; z], [vN], [bound z vN];
  ]
  |> List.map (fun (name, before, idxs, ds, conds) ->
      ( name,
        before,
        Delinearize.{
          indices = List.map normalize idxs;
          dims = List.map normalize ds;
          conditions = conds;
        } ))

(* Stage 4: kernel-level rewrite. Drives [rewrite_kernel] end-to-end. *)
let make_kernel
    ((name : string), (globals : string list), (before : Aligned.Code.t),
     (after : Aligned.Code.t)) :
    string * Aligned.Kernel.t * Aligned.Kernel.t =
  let kernel : Aligned.Kernel.t = {
    name = "";
    global_variables =
      globals
      |> List.map (fun n -> (Variable.from_name n, C_type.int))
      |> Params.from_list;
    local_variables = Params.empty;
    arrays = Variable.Map.empty;
    pre = Exp.b_true;
    code = before;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  } in
  (name, kernel, { kernel with code = after })

let kernels : (string * Aligned.Kernel.t * Aligned.Kernel.t) list =
  let open Aligned.Code in
  let open Build in
  let acc array index =
    Unsynced.Access { array; index; mode = Access.Mode.Write None }
  in
  let loop (var : string) (body : Aligned.Code.t) : Aligned.Code.t = Loop {
    range = {
      var = Variable.from_name var;
      ty = C_type.int;
      dir = Range.Increase;
      lower_bound = Num 0;
      upper_bound = Num 0;
      step = Range.Step.Plus (Num 1);
    };
    body;
  } in
  [
    "trivial", [], Sync Skip, Sync Skip;
    "3dim+param", ["m"; "n"; "M"; "N"],
      Sync (acc aA [m * n * x + n * y + z]),
      Sync (acc aA [x; y; z]);
    "3dim+loop", ["M"; "N"],
      loop "m" (loop "n" (Sync (acc aA [m * n * x + n * y + z]))),
      loop "m" (loop "n" (Sync (acc aA [x; y; z])));
  ]
  |> List.map make_kernel

let stage1_tests =
  "size_params (stage1)" >:: fun _ ->
  size_param_examples
  |> List.iter (fun (msg, exp, params) ->
      let got =
        exp
        |> Delinearize.Expr.from_nexp ~globals
        |> Delinearize.size_params
        |> List.map Delinearize.Term.to_nexp
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
        |> Delinearize.Expr.from_nexp ~globals
        |> Delinearize.size_params
        |> Delinearize.dims
        |> Option.map (List.map Delinearize.Term.to_nexp)
      in
      assert_equal
        ~msg
        ~printer:(string_of_option (string_of_list Exp.n_to_string))
        (Some expected) got)

let stage3_tests =
  "from_exp (stage3)" >:: fun _ ->
  positive_examples
  |> List.iter (fun (msg, exp, expected) ->
      let got = delin ~globals exp in
      assert_equal
        ~msg
        ~printer:(string_of_option Delinearize.to_string)
        (Some expected) got)

let kernel_tests =
  "rewrite_kernel" >:: fun _ ->
  kernels
  |> List.iter (fun (msg, before, after) ->
      let got = Delinearize.Default.rewrite_kernel before in
      assert_equal
        ~msg
        ~printer:Aligned.Kernel.to_string
        after got)

(* Maslov elision: white-box tests for [Maslov.add_bound] / [get_bounds]
   on individual [(i, d)] pairs, then end-to-end tests comparing
   [AllBounds.from_exp] and [Maslov_elide.from_exp] on the same input. *)

(* Build a [Maslov] scope from a fixture list of [(varname, lb, ub)]
   triples. Loops with non-zero lower bound are omitted (the Maslov
   strategy ignores them in [add_range], so the fixture matches). *)
let maslov_scope_of (items : (string * nexp * nexp) list)
    : Delinearize.Maslov.scope =
  items
  |> List.fold_left (fun scope (n, lb, ub) ->
      let r : Range.t = {
        var = Variable.from_name n;
        ty = C_type.int;
        dir = Range.Increase;
        lower_bound = lb;
        upper_bound = ub;
        step = Range.Step.Plus (Num 1);
      } in
      Delinearize.Maslov.add_range ~globals r scope)
    Delinearize.Maslov.initial_scope

(* Did [Maslov.add_bound] elide the (i, d) pair? Equivalent to
   [provable]: feeding one bound, the accumulator stays empty iff the
   bound was proved statically. *)
let maslov_provable scope (i : Delinearize.Expr.t) (d : Delinearize.Expr.t)
    : bool =
  let acc = Delinearize.Maslov.create scope in
  let acc' = Delinearize.Maslov.add_bound acc i d in
  Delinearize.Maslov.get_bounds acc' = []

let maslov_tests =
  "Maslov.add_bound" >:: fun _ ->
  let open Build in
  let expr e = Delinearize.Expr.from_nexp ~globals e in
  let check ?(msg = "") ~scope expected i d =
    let got = maslov_provable scope i d in
    assert_equal ~msg ~printer:string_of_bool expected got
  in
  (* y in [0, N-1], dim N: provable, bound elided. *)
  check
    ~msg:"loop range matches dim"
    ~scope:(maslov_scope_of [("y", Num 0, vN + Num (-1))])
    true (expr y) (expr vN);
  (* No range information: bound kept. *)
  check
    ~msg:"empty scope -> bound kept"
    ~scope:Delinearize.Maslov.initial_scope
    false (expr y) (expr vN);
  (* y in [0, N] (one too large): not provable. *)
  check
    ~msg:"loose upper bound -> bound kept"
    ~scope:(maslov_scope_of [("y", Num 0, vN)])
    false (expr y) (expr vN);
  (* y in [1, N-1] (non-zero lower bound is not recognised). *)
  check
    ~msg:"non-zero lower bound -> bound kept"
    ~scope:(maslov_scope_of [("y", Num 1, vN + Num (-1))])
    false (expr y) (expr vN);
  (* Constant index: not the single-atom pattern. *)
  check
    ~msg:"constant index -> bound kept"
    ~scope:Delinearize.Maslov.initial_scope
    false (expr (Num 3)) (expr vN);
  (* Sum of two atoms: not the single-atom pattern. *)
  check
    ~msg:"two-atom index -> bound kept"
    ~scope:(maslov_scope_of [("y", Num 0, vN + Num (-1));
                             ("z", Num 0, vN + Num (-1))])
    false (expr (y + z)) (expr vN);
  (* Numeric range: y in [0, 9], dim 10. *)
  check
    ~msg:"numeric range matches numeric dim"
    ~scope:(maslov_scope_of [("y", Num 0, Num 9)])
    true (expr y) (expr (Num 10))

(* End-to-end via [from_exp]: [Maslov_elide] drops bounds [Maslov.add_bound]
   can prove; [All] keeps every bound. *)
let cond_count = function
  | None -> -1
  | Some (t : Delinearize.t) -> List.length t.conditions

let delin_all (e : nexp) : Delinearize.t option =
  let expr = Delinearize.Expr.from_nexp ~globals e in
  let size_params = Delinearize.size_params expr in
  Delinearize.All.from_exp
    ~globals
    ~scope:Delinearize.AllBounds.initial_scope
    ~size_params
    expr

let delin_maslov ~scope (e : nexp) : Delinearize.t option =
  let expr = Delinearize.Expr.from_nexp ~globals e in
  let size_params = Delinearize.size_params expr in
  Delinearize.Maslov_elide.from_exp
    ~globals
    ~scope
    ~size_params
    expr

let elision_tests =
  "from_exp elision" >:: fun _ ->
  let open Build in
  let case ~msg ~scope expr expected_all expected_maslov =
    let n_all = delin_all expr |> cond_count in
    let n_mas = delin_maslov ~scope expr |> cond_count in
    assert_equal ~msg:(msg ^ " (AllBounds)") ~printer:string_of_int
      expected_all n_all;
    assert_equal ~msg:(msg ^ " (Maslov)") ~printer:string_of_int
      expected_maslov n_mas
  in
  (* Single inner-axis bound, loop range matches: elidable under Maslov. *)
  case
    ~msg:"numdim with loop range"
    ~scope:(maslov_scope_of [("y", Num 0, vN + Num (-1))])
    (vN * x + y)
    1 0;
  (* No range info: bound stays under both. *)
  case
    ~msg:"numdim no scope"
    ~scope:Delinearize.Maslov.initial_scope
    (vN * x + y)
    1 1;
  (* Two axes, only [y] bounded: AllBounds emits 2, Maslov elides [y]. *)
  case
    ~msg:"3dim, only y bounded"
    ~scope:(maslov_scope_of [("y", Num 0, vM + Num (-1))])
    (vM * vN * x + vN * y + z)
    2 1

let reconstruct_tests =
  "Index.reconstruct" >:: fun _ ->
  let open Build in
  let check ~msg before =
    let expr = Delinearize.Expr.from_nexp ~globals before in
    let size_params = Delinearize.size_params expr in
    match
      Delinearize.Greedy.candidates ~globals ~size_params expr |> Seq.uncons
    with
    | Some (Delinearize.Tactic.Use idx, _) ->
      let rebuilt = Delinearize.Index.reconstruct idx in
      assert_equal
        ~msg
        ~printer:Exp.n_to_string
        (normalize before)
        (Delinearize.Expr.to_nexp rebuilt)
    | Some (Delinearize.Tactic.Try _, _) ->
      failwith "test fixture: Greedy should not produce Try"
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
    Delinearize.Expr.from_nexp ~globals
      (vN * vM * x + vN * x + vM * y + y + z)
  in
  let size_params = Delinearize.size_params expr in
  match
    Delinearize.ICS15.candidates ~globals ~size_params expr
    |> Seq.uncons
  with
  | None -> assert_failure "ICS15 produced no candidate"
  | Some (Delinearize.Tactic.Try _, _) ->
    assert_failure "ICS15 produced a Try, expected Use"
  | Some (Delinearize.Tactic.Use idx, _) ->
    let rebuilt = Delinearize.Index.reconstruct idx in
    assert_equal
      ~msg:"reconstructed polynomial"
      ~printer:Exp.n_to_string
      (Delinearize.Expr.to_nexp
        (Delinearize.Expr.from_nexp ~globals
          (vN * vM * x + vN * x + vM * y + y + z)))
      (Delinearize.Expr.to_nexp rebuilt);
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
    Delinearize.Expr.from_nexp ~globals
      (vN * vM * x + vN * x + vM * y + y + z)
  in
  let size_params = Delinearize.size_params expr in
  match
    Delinearize.Greedy.candidates ~globals ~size_params expr |> Seq.uncons
  with
  | None -> ()  (* expected *)
  | Some _ ->
    assert_failure "Greedy produced a candidate; expected Seq.empty"

let tests =
  "delinearization" >::: [
    stage1_tests;
    stage2_tests;
    stage3_tests;
    kernel_tests;
    maslov_tests;
    elision_tests;
    reconstruct_tests;
    grosser_offset_test;
    greedy_fails_on_offset_test;
  ]

let _ = run_test_tt_main tests
