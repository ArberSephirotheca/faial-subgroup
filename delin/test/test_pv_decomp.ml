open Protocols
open Exp
open OUnit2

module Eval = Pv_decomp.Eval

module Build = struct
  let v x = Var (Variable.from_name x)
  let ( + ) = n_plus
  let ( * ) = n_mult
  let ( / ) = n_udiv
  let ( mod ) = n_umod
  let num n = Num n
end

let gset l = Variable.Set.of_list (List.map Variable.from_name l)
let poly ~globals e = Poly.from_nexp ~globals e

let sort_pairs =
  List.sort (fun (n1, p1) (n2, p2) ->
    match Poly.compare p1 p2 with 0 -> Poly.compare n1 n2 | c -> c)

let show ps =
  ps
  |> List.map (fun (n, p) ->
    Printf.sprintf "%s @ %s"
      (Exp.n_to_string (Poly.to_nexp n))
      (Exp.n_to_string (Poly.to_nexp p)))
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let pairs_eq a b =
  List.compare_lengths a b = 0
  && List.for_all2
       (fun (n1, p1) (n2, p2) -> Poly.compare n1 n2 = 0 && Poly.compare p1 p2 = 0)
       a b

let eval_pairs (t : Eval.t) =
  List.combine t.Eval.numeral t.Eval.place_value |> sort_pairs

let expected ~globals axes =
  List.map (fun (n, p) -> (poly ~globals n, poly ~globals p)) axes |> sort_pairs

let assert_axes ~msg actual exp =
  assert_bool
    (Printf.sprintf "%s\n  expected %s\n  actual   %s" msg (show exp) (show actual))
    (pairs_eq actual exp)

let check_make ~globals ~msg (e : nexp) ~(axes : (nexp * nexp) list) () =
  assert_axes ~msg (eval_pairs (Eval.make ~globals e)) (expected ~globals axes)

open Build

let make_tests =
  [ ( "simple 2D   i*s1 + j*s2" >:: fun _ ->
      check_make ~globals:(gset [ "s1"; "s2" ]) ~msg:"simple"
        (v "i" * v "s1" + v "j" * v "s2")
        ~axes:[ (v "i", v "s1"); (v "j", v "s2"); (num 0, num 1) ]
        () );
    ( "grouping    i*s1 + i*s2" >:: fun _ ->
      check_make ~globals:(gset [ "s1"; "s2" ]) ~msg:"grouping"
        (v "i" * v "s1" + v "i" * v "s2")
        ~axes:[ (v "i", v "s1" + v "s2"); (num 0, num 1) ]
        () );
    ( "getrows" >:: fun _ ->
      check_make
        ~globals:(gset [ "blockIdx.x"; "s1"; "s2"; "s3"; "ne12" ])
        ~msg:"getrows"
        (v "blockIdx.x" * v "s1"
        + (v "z" / v "ne12") * v "s2"
        + (v "z" mod v "ne12") * v "s3"
        + v "i00")
        ~axes:
          [ (v "z" / v "ne12", v "s2");
            (v "z" mod v "ne12", v "s3");
            (v "i00" + v "blockIdx.x" * v "s1", num 1) ]
        () );
    ( "mmvf" >:: fun _ ->
      check_make
        ~globals:(gset [ "SS"; "SC"; "SD"; "blockIdx.x"; "blockIdx.y" ])
        ~msg:"mmvf"
        (v "sample" * v "SS"
        + v "blockIdx.y" * v "SC"
        + v "threadIdx.x" * v "SD"
        + v "blockIdx.x")
        ~axes:
          [ (v "sample", v "SS");
            (v "threadIdx.x", v "SD");
            (v "blockIdx.y" * v "SC" + v "blockIdx.x", num 1) ]
        () );
    ( "solve_tri" >:: fun _ ->
      let batch =
        (v "blockIdx.x" mod v "ne03") * v "nb2" + (v "blockIdx.x" / v "ne03") * v "nb3"
      in
      let row = v "rr" * num 32 + v "threadIdx.x" in
      check_make
        ~globals:(gset [ "blockIdx.x"; "ne03"; "nb2"; "nb3"; "k" ])
        ~msg:"solve_tri"
        (batch + row * v "k" + v "threadIdx.y")
        ~axes:
          [ (v "rr", num 32 * v "k");
            (v "threadIdx.x", v "k");
            (v "threadIdx.y" + batch, num 1) ]
        () )
  ]

let normalize_tests =
  [ ( "aliasing   i*s + j*s -> (i+j, s)" >:: fun _ ->
      let globals = gset [ "s" ] in
      let t = Eval.make ~globals (v "i" * v "s" + v "j" * v "s") in
      assert_axes ~msg:"aliasing" (eval_pairs t)
        (expected ~globals [ (v "i" + v "j", v "s"); (num 0, num 1) ]) )
  ]

let poly_set ~globals subs = List.map (poly ~globals) subs |> List.sort Poly.compare

let subs_eq ~globals actual exp =
  List.compare_lengths actual exp = 0
  && List.for_all2
       (fun a e ->
         List.equal
           (fun x y -> Poly.compare x y = 0)
           (poly_set ~globals a) (poly_set ~globals e))
       actual exp

let norm ~globals e = Poly.to_nexp (Poly.from_nexp ~globals e)

let rec b_conjuncts = function
  | BRel (B_rel.BAnd, a, b) -> b_conjuncts a @ b_conjuncts b
  | Bool true -> []
  | x -> [ x ]

let atom_str ~globals b =
  match b with
  | NRel (op, l, r) ->
    let s e = Exp.n_to_string (Poly.to_nexp (poly ~globals e)) in
    Printf.sprintf "%s %s %s" (s l) (N_rel.to_string op) (s r)
  | _ -> "?"

let bound_atoms ~globals b =
  b_conjuncts b |> List.map (atom_str ~globals) |> List.sort compare

let rec b_disjuncts = function
  | BRel (B_rel.BOr, a, b) -> b_disjuncts a @ b_disjuncts b
  | Bool false -> []
  | x -> [ x ]

let bound_orders ~globals b =
  b_disjuncts b |> List.map (bound_atoms ~globals) |> List.sort compare

let show_orders os = os |> List.map (String.concat " ; ") |> String.concat "  |  "

let is_disjunction = function
  | BRel (B_rel.BOr, _, _) | Bool false -> true
  | _ -> false

(* The bound is [AND (order-disjunction, pv >= 1 guards)]. Peel the two apart:
   the single [OR] conjunct is the ordering disjunction, the rest are the
   per-place-value [pv >= 1] guards. *)
let order_disjunction b =
  match List.filter is_disjunction (b_conjuncts b) with [ d ] -> d | _ -> b

let guard_atoms ~globals b =
  b_conjuncts b
  |> List.filter (fun x -> not (is_disjunction x))
  |> List.map (atom_str ~globals)
  |> List.sort compare

let check_bound ~globals ~msg actual expected =
  let a = bound_orders ~globals (order_disjunction actual)
  and e = bound_orders ~globals (order_disjunction expected) in
  let ag = guard_atoms ~globals actual and eg = guard_atoms ~globals expected in
  assert_bool
    (Printf.sprintf "%s\n  expected %s  [pv guards: %s]\n  actual   %s  [pv guards: %s]"
       msg (show_orders e) (String.concat " ; " eg) (show_orders a)
       (String.concat " ; " ag))
    (a = e && ag = eg)

let pipeline_tests =
  [ ( "single access: subscripts + nesting bound" >:: fun _ ->
      let g = gset [ "s1"; "s2" ] in
      let subs, bound = Pv_decomp.make ~globals:g [ v "i" * v "s1" + v "j" * v "s2" ] in
      assert_bool "subscripts"
        (subs_eq ~globals:g subs [ [ v "i"; v "j"; num 0 ] ]);
      let s1 = norm ~globals:g (v "s1") and s2 = norm ~globals:g (v "s2") in
      let expected =
        b_and_ex
          [ b_or_ex [ n_eq (n_umod s2 s1) (num 0); n_eq (n_umod s1 s2) (num 0) ];
            n_ge s1 (num 1);
            n_ge s2 (num 1) ]
      in
      check_bound ~globals:g ~msg:"bound" bound expected );
    ( "single access: --in-range adds the digit span" >:: fun _ ->
      let g = gset [ "s1"; "s2" ] in
      let _subs, bound =
        Pv_decomp.make ~globals:g ~in_range:true
          [ v "i" * v "s1" + v "j" * v "s2" ]
      in
      let s1 = norm ~globals:g (v "s1") and s2 = norm ~globals:g (v "s2") in
      let expected =
        b_and_ex
          [ b_or_ex
              [ b_and_ex
                  [ n_eq (n_umod s2 s1) (num 0);
                    n_lt (num 0) (v "s1");
                    n_le (num 0) (v "i" * v "s1");
                    n_lt (v "i" * v "s1") (v "s2") ];
                b_and_ex
                  [ n_eq (n_umod s1 s2) (num 0);
                    n_lt (num 0) (v "s2");
                    n_le (num 0) (v "j" * v "s2");
                    n_lt (v "j" * v "s2") (v "s1") ] ];
            n_ge s1 (num 1);
            n_ge s2 (num 1) ]
      in
      check_bound ~globals:g ~msg:"in-range bound" bound expected );
    ( "two accesses share a frame" >:: fun _ ->
      let g = gset [ "s1"; "s2" ] in
      let subs, _bound =
        Pv_decomp.make ~globals:g
          [ v "i" * v "s1" + v "j" * v "s2"; v "i" * v "s1" + v "k" * v "s2" ]
      in
      assert_bool "shared frame subscripts"
        (subs_eq ~globals:g subs
           [ [ v "i"; v "j"; num 0 ]; [ v "i"; v "k"; num 0 ] ]) );
    ( "getrows: divisibility + offset nesting" >:: fun _ ->
      let g = gset [ "blockIdx.x"; "s1"; "s2"; "s3"; "ne12" ] in
      let flat =
        v "blockIdx.x" * v "s1"
        + (v "z" / v "ne12") * v "s2"
        + (v "z" mod v "ne12") * v "s3"
        + v "i00"
      in
      let _subs, bound = Pv_decomp.make ~globals:g [ flat ] in
      let s2 = norm ~globals:g (v "s2") and s3 = norm ~globals:g (v "s3") in
      let expected =
        b_and_ex
          [ b_or_ex [ n_eq (n_umod s3 s2) (num 0); n_eq (n_umod s2 s3) (num 0) ];
            n_ge s2 (num 1);
            n_ge s3 (num 1) ]
      in
      check_bound ~globals:g ~msg:"getrows bound" bound expected )
  ]

let tests =
  "pv_decomp (opaque delin)"
  >::: [ "Eval.make" >::: make_tests;
         "Eval.normalize" >::: normalize_tests;
         "make (pipeline)" >::: pipeline_tests ]

let () = run_test_tt_main tests
