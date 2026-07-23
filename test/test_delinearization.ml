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
  e |> Poly.from_nexp ~globals |> Poly.to_nexp

let bound (i : nexp) (d : nexp) : bexp =
  b_and (n_le (Num 0) i) (n_lt i d)

let string_of_option (f : 'a -> string) : 'a option -> string = function
  | None -> "None"
  | Some x -> "Some " ^ f x

(* Stage 3: end-to-end on a single access expression. Uses [AllBounds]
   so [t.conditions] contains the full per-axis bound list. *)
let delin ~globals (e : nexp) : Delinearize.t option =
  let expr = Poly.from_nexp ~globals e in
  let size_params = Shape.size_params expr in
  match Greedy.candidates ~globals ~size_params expr |> Seq.uncons with
  | None -> None
  | Some (idx, _) ->
    Delinearize.All.from_exp
      ~scope:Delinearize.AllBounds.initial_scope
      ~loop_scope:[]
      ~check:Delinearize.trivially_true_oracle
      ~radix:(idx : Subscript.t).radix
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
    Unsynced.Access (Access.write array index None)
  in
  let loop (var : string) (body : Aligned.Code.t) : Aligned.Code.t = Loop {
    cond_range = Cond_range.of_range {
      var = Variable.from_name var;
      ty = C_type.int;
      dir = Range.Increase;
      lower_bound = Num 0;
      upper_bound = Num 0;
      step = Range.Step.Plus (Num 1);
    };
    body;
  } in
  let assert_seq (conds : bexp list) (body : Unsynced.t) : Unsynced.t =
    List.fold_right
      (fun c b -> Unsynced.Seq (Assert c, b)) conds body
  in
  let access_3dim =
    assert_seq [bound y m; bound z n] (acc aA [x; y; z])
  in
  [
    "trivial", [], Sync Skip, Sync Skip;
    "3dim+param", ["m"; "n"; "M"; "N"],
      Sync (acc aA [m * n * x + n * y + z]),
      Sync access_3dim;
    "3dim+loop", ["M"; "N"],
      loop "m" (loop "n" (Sync (acc aA [m * n * x + n * y + z]))),
      loop "m" (loop "n" (Sync access_3dim));
  ]
  |> List.map make_kernel

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
      let got =
        Delinearize.All.rewrite_kernel ~rewrite_access:true ~assume:false
          ~check:Delinearize.trivially_true_oracle before
      in
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
let maslov_provable scope (i : Poly.t) (d : Poly.t)
    : bool =
  let acc = Delinearize.Maslov.create scope in
  let acc' = Delinearize.Maslov.add_bound acc i d in
  Delinearize.Maslov.get_bounds acc' = []

let maslov_tests =
  "Maslov.add_bound" >:: fun _ ->
  let open Build in
  let expr e = Poly.from_nexp ~globals e in
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
  let expr = Poly.from_nexp ~globals e in
  let size_params = Shape.size_params expr in
  match Greedy.candidates ~globals ~size_params expr |> Seq.uncons with
  | None -> None
  | Some (idx, _) ->
    Delinearize.All.from_exp
      ~scope:Delinearize.AllBounds.initial_scope
      ~loop_scope:[]
      ~check:Delinearize.trivially_true_oracle
      ~radix:(idx : Subscript.t).radix
      expr

let delin_maslov ~scope (e : nexp) : Delinearize.t option =
  let expr = Poly.from_nexp ~globals e in
  let size_params = Shape.size_params expr in
  match Greedy.candidates ~globals ~size_params expr |> Seq.uncons with
  | None -> None
  | Some (idx, _) ->
    Delinearize.Maslov_elide.from_exp
      ~scope
      ~loop_scope:[]
      ~check:Delinearize.trivially_true_oracle
      ~radix:(idx : Subscript.t).radix
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

let tests =
  "delinearization" >::: [
    stage3_tests;
    kernel_tests;
    maslov_tests;
    elision_tests;
  ]

let _ = run_test_tt_main tests
