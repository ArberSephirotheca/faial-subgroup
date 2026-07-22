open Protocols
open Drf_genie

(* Regression guards on the comparators that drive Abduction.build_pool's
   deduplication and the sort used by build_pool_union.

   Two concerns:

   1. [Variable.compare] currently keys on [name] only — [label] and
      [location] are ignored. [List.sort_uniq Variable.compare] therefore
      collapses any two distinct variables sharing a name. Pinning this
      makes the limitation explicit: a fix that strengthens the
      comparator would break this test, which is the right reminder.

   2. [Exp.b_compare] must be total (trichotomous on every pair). If a
      future variant addition leaves a comparator branch returning a
      non-total ordering, [List.sort_uniq] degenerates and pool
      ordering becomes nondeterministic. Spot-check a few shapes. *)

let test_variable_compare_name_only () =
  let v_plain = Variable.from_name "x" in
  let v_labelled =
    Variable.make ~name:"x" ~label:"kernel.A" ()
  in
  Alcotest.(check int) "same-name same-label compares 0"
    0 (Variable.compare v_plain (Variable.from_name "x"));
  Alcotest.(check int) "same-name different-label still compares 0"
    0 (Variable.compare v_plain v_labelled);
  Alcotest.(check bool) "different names compare non-zero"
    true (Variable.compare v_plain (Variable.from_name "y") <> 0)

let test_variable_compare_total () =
  let vs =
    [ "a"; "b"; "abc"; ""; "z"; "aa" ] |> List.map Variable.from_name
  in
  let pairs =
    List.concat_map (fun a -> List.map (fun b -> (a, b)) vs) vs
  in
  List.iter (fun (a, b) ->
    let c = Variable.compare a b in
    let c' = Variable.compare b a in
    let consistent = (c = 0 && c' = 0) || (c < 0 && c' > 0) || (c > 0 && c' < 0)
    in
    Alcotest.(check bool)
      (Printf.sprintf "compare antisymmetric on %s vs %s"
         (Variable.name a) (Variable.name b))
      true consistent)
    pairs

let test_b_compare_total () =
  let open Exp in
  let v = Var (Variable.from_name "x") in
  let samples : bexp list = [
    Bool true;
    Bool false;
    NRel (Eq, v, Num 0);
    NRel (Lt Signedness.Signed, v, Num 1);
    NRel (Gt Signedness.Signed, v, Num 1);
    BRel (BAnd, Bool true, Bool false);
    BNot (Bool true);
  ] in
  List.iter (fun a ->
    List.iter (fun b ->
      let c = Exp.b_compare a b in
      let c' = Exp.b_compare b a in
      let consistent =
        (c = 0 && c' = 0) || (c < 0 && c' > 0) || (c > 0 && c' < 0)
      in
      Alcotest.(check bool)
        (Printf.sprintf "b_compare antisymmetric on %s vs %s"
           (Exp.b_to_string a) (Exp.b_to_string b))
        true consistent)
      samples)
    samples

let test_build_pool_referentially_transparent () =
  let kernel : Kernel.t = {
    name = "k_test";
    global_variables =
      Params.add (Variable.from_name "n") C_type.int Params.empty;
    local_variables = Params.empty;
    arrays = Variable.Map.empty;
    pre = Exp.Bool true;
    code = Code.Skip;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  } in
  let pool1 = Abduction.build_pool kernel in
  let pool2 = Abduction.build_pool kernel in
  let union1 = Abduction.build_pool_union [ kernel ] in
  let union2 = Abduction.build_pool_union [ kernel ] in
  let bexp_list_eq xs ys =
    List.length xs = List.length ys
    && List.for_all2 (fun x y -> Exp.b_compare x y = 0) xs ys
  in
  Alcotest.(check bool) "build_pool deterministic" true (bexp_list_eq pool1 pool2);
  Alcotest.(check bool) "build_pool_union deterministic"
    true (bexp_list_eq union1 union2);
  Alcotest.(check bool) "build_pool_union nonempty"
    true (List.length union1 > 0)

(* G–I exercise [build_pool]'s [?scope] parameter: empty scope drops
   every candidate, full-scope is identical to no scope, and a single-
   variable scope keeps only candidates whose free names lie inside
   that scope. *)

let bexp_list_eq xs ys =
  List.length xs = List.length ys
  && List.for_all2 (fun x y -> Exp.b_compare x y = 0) xs ys

let make_kernel_with_params (param_names : string list) : Kernel.t = {
  name = "k_scope";
  global_variables =
    List.fold_left
      (fun p n -> Params.add (Variable.from_name n) C_type.int p)
      Params.empty param_names;
  local_variables = Params.empty;
  arrays = Variable.Map.empty;
  pre = Exp.Bool true;
  code = Code.Skip;
  visibility = Visibility.Global;
  grid_dim = None;
  block_dim = None;
}

let all_vars_of (k : Kernel.t) : Variable.Set.t =
  (* The pool draws from [int_params] and the six dim built-ins; the
     "full scope" is therefore their union, regardless of whatever
     else the kernel happens to mention. *)
  let open Variable in
  Set.union
    (Set.of_list
       [ bdim_x; bdim_y; bdim_z; gdim_x; gdim_y; gdim_z ])
    (Params.to_set k.global_variables
     |> Set.union (Params.to_set k.local_variables))

let test_build_pool_scope_empty () =
  let k = make_kernel_with_params [ "n"; "m" ] in
  let pool = Abduction.build_pool ~scope:Variable.Set.empty k in
  Alcotest.(check int) "empty scope yields empty pool" 0 (List.length pool)

let test_build_pool_scope_full_matches_default () =
  let k = make_kernel_with_params [ "n"; "m" ] in
  let pool_default = Abduction.build_pool k in
  let pool_full = Abduction.build_pool ~scope:(all_vars_of k) k in
  Alcotest.(check bool) "scope = all vars is identical to no scope"
    true (bexp_list_eq pool_default pool_full)

let test_build_pool_scope_single_param () =
  let k = make_kernel_with_params [ "n"; "m" ] in
  let n = Variable.from_name "n" in
  let m = Variable.from_name "m" in
  let pool = Abduction.build_pool ~scope:(Variable.Set.singleton n) k in
  (* No candidate may reference [m]: it's outside the scope and so
     was excluded from both [params] and [all_dims] before pool
     construction. *)
  Alcotest.(check bool) "no candidate mentions m"
    true
    (List.for_all (fun b -> not (Exp.b_mem m b)) pool);
  (* At least one candidate must mention [n], otherwise the scope had
     no effect. *)
  Alcotest.(check bool) "at least one candidate mentions n"
    true
    (List.exists (fun b -> Exp.b_mem n b) pool)

let test_build_pool_union_dedup_stable () =
  (* Two kernels with the same int parameter should produce a single
     union pool whose size doesn't depend on the order of [kernels]. *)
  let make_kernel name : Kernel.t = {
    name;
    global_variables =
      Params.add (Variable.from_name "n") C_type.int Params.empty;
    local_variables = Params.empty;
    arrays = Variable.Map.empty;
    pre = Exp.Bool true;
    code = Code.Skip;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  } in
  let k1 = make_kernel "k1" in
  let k2 = make_kernel "k2" in
  let union_ab = Abduction.build_pool_union [ k1; k2 ] in
  let union_ba = Abduction.build_pool_union [ k2; k1 ] in
  Alcotest.(check int) "union size stable under kernel ordering"
    (List.length union_ab) (List.length union_ba);
  let bexp_list_eq xs ys =
    List.length xs = List.length ys
    && List.for_all2 (fun x y -> Exp.b_compare x y = 0) xs ys
  in
  Alcotest.(check bool) "union contents stable under kernel ordering"
    true (bexp_list_eq union_ab union_ba)

let variable_tests = [
  ("compare ignores label / location",   `Quick, test_variable_compare_name_only);
  ("compare is antisymmetric (total)",   `Quick, test_variable_compare_total);
]

let bexp_tests = [
  ("b_compare antisymmetric on a sample", `Quick, test_b_compare_total);
]

let abduction_tests = [
  ("build_pool referentially transparent", `Quick, test_build_pool_referentially_transparent);
  ("build_pool_union stable under order",  `Quick, test_build_pool_union_dedup_stable);
  ("G. scope = empty -> empty pool",       `Quick, test_build_pool_scope_empty);
  ("H. scope = all vars matches default",  `Quick, test_build_pool_scope_full_matches_default);
  ("I. scope = {n} excludes m",            `Quick, test_build_pool_scope_single_param);
]

let () =
  Alcotest.run "drf_genie/abduction" [
    ("Variable.compare",  variable_tests);
    ("Exp.b_compare",     bexp_tests);
    ("Abduction.pool",    abduction_tests);
  ]
