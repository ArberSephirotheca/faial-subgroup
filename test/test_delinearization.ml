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

(* Stage 3: end-to-end on a single access expression. Composes Expr.from_nexp,
   size_params, dims, from_exp; matches the current [from_exp] behaviour
   (inner-index bounds only). *)
let delin ~globals (e : nexp) : Delinearize.t option =
  let expr = Delinearize.Expr.from_nexp ~globals e in
  match Delinearize.size_params expr |> Delinearize.dims with
  | None -> None
  | Some d -> Delinearize.Silent.from_exp d expr

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
      let got = Delinearize.Silent.rewrite_kernel before in
      assert_equal
        ~msg
        ~printer:Aligned.Kernel.to_string
        after got)

(* Static_bound predicate: white-box tests that exercise the recognised
   patterns directly. *)
let make_range (name : string) (lb : nexp) (ub : nexp) : Range.t =
  {
    var = Variable.from_name name;
    ty = C_type.int;
    dir = Range.Increase;
    lower_bound = lb;
    upper_bound = ub;
    step = Range.Step.Plus (Num 1);
  }

let ranges_of (items : (string * nexp * nexp) list) : Range.t Variable.Map.t =
  items
  |> List.map (fun (n, lb, ub) -> (Variable.from_name n, make_range n lb ub))
  |> List.to_seq
  |> Variable.Map.of_seq

let static_bound_tests =
  "Static_bound.holds" >:: fun _ ->
  let open Build in
  let expr e = Delinearize.Expr.from_nexp ~globals e in
  let term e =
    match
      Delinearize.Expr.to_list (Delinearize.Expr.from_nexp ~globals e)
    with
    | [t] -> t
    | _ -> failwith "test fixture: expected single-term dim"
  in
  let check ?(msg = "") ~ranges expected i d =
    let got = Delinearize.Static_bound.holds ~globals ~ranges i d in
    assert_equal ~msg ~printer:string_of_bool expected got
  in
  (* y in [0, N-1], dim N: holds *)
  check
    ~msg:"loop range matches dim"
    ~ranges:(ranges_of [("y", Num 0, vN + Num (-1))])
    true (expr y) (term vN);
  (* No range information: cannot prove *)
  check
    ~msg:"empty ranges -> false"
    ~ranges:Variable.Map.empty
    false (expr y) (term vN);
  (* y in [0, N] (one too large): cannot prove *)
  check
    ~msg:"loose upper bound -> false"
    ~ranges:(ranges_of [("y", Num 0, vN)])
    false (expr y) (term vN);
  (* y in [1, N-1] (lower bound > 0 not recognised): conservative false *)
  check
    ~msg:"non-zero lower bound -> false"
    ~ranges:(ranges_of [("y", Num 1, vN + Num (-1))])
    false (expr y) (term vN);
  (* Constant index: not the single-atom pattern, falls through *)
  check
    ~msg:"constant index -> false"
    ~ranges:Variable.Map.empty
    false (expr (Num 3)) (term vN);
  (* Sum of two atoms: not the single-atom pattern *)
  check
    ~msg:"two-atom index -> false"
    ~ranges:(ranges_of [("y", Num 0, vN + Num (-1)); ("z", Num 0, vN + Num (-1))])
    false (expr (y + z)) (term vN);
  (* Numeric range: y in [0, 9], dim 10 *)
  check
    ~msg:"numeric range matches numeric dim"
    ~ranges:(ranges_of [("y", Num 0, Num 9)])
    true (expr y) (term (Num 10))

(* End-to-end via [from_exp]: with the elision flag, [t.conditions] drops
   bounds that [Static_bound] can prove; without the flag, all bounds emit. *)
let delin_with
    ~elide_provable_bounds
    ~ranges
    (e : nexp) : Delinearize.t option =
  let expr = Delinearize.Expr.from_nexp ~globals e in
  match Delinearize.size_params expr |> Delinearize.dims with
  | None -> None
  | Some d ->
    Delinearize.Silent.from_exp
      ~elide_provable_bounds ~globals ~ranges d expr

let elision_tests =
  "from_exp elision" >:: fun _ ->
  let open Build in
  let cond_count r =
    match r with
    | None -> -1
    | Some (t : Delinearize.t) -> List.length t.conditions
  in
  let case ~msg ~ranges expr expected_off expected_on =
    let off =
      delin_with ~elide_provable_bounds:false ~ranges expr |> cond_count
    in
    let on =
      delin_with ~elide_provable_bounds:true ~ranges expr |> cond_count
    in
    assert_equal ~msg:(msg ^ " (flag off)") ~printer:string_of_int
      expected_off off;
    assert_equal ~msg:(msg ^ " (flag on)") ~printer:string_of_int
      expected_on on
  in
  (* Single inner-axis bound, loop range matches: elidable. *)
  case
    ~msg:"numdim with loop range"
    ~ranges:(ranges_of [("y", Num 0, vN + Num (-1))])
    (vN * x + y)
    1 0;
  (* No range info available: bound stays. *)
  case
    ~msg:"numdim no range"
    ~ranges:Variable.Map.empty
    (vN * x + y)
    1 1;
  (* Two axes, one elidable, the other not (z has no range). *)
  case
    ~msg:"3dim, only y bounded"
    ~ranges:(ranges_of [("y", Num 0, vM + Num (-1))])
    (vM * vN * x + vN * y + z)
    2 1

let tests =
  "delinearization" >::: [
    stage1_tests;
    stage2_tests;
    stage3_tests;
    kernel_tests;
    static_bound_tests;
    elision_tests;
  ]

let _ = run_test_tt_main tests
