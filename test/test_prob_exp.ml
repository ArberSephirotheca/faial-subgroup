open OUnit2
open Protocols
open Prob_exp
open Exp

let assert_i32 (x : Int32.t) (y : int) : unit =
  assert_equal x (Int32.of_int y) ~printer:Int32.to_string

let assert_between (x : float) (y : float) (z : float) : unit =
  let msg =
    Float.to_string x ^ " < " ^ Float.to_string y ^ " <= " ^ Float.to_string z
  in
  assert_equal ~cmp:(fun a b -> a < b) ~msg x y;
  assert_equal ~cmp:(fun a b -> a <= b) ~msg y z

let assert_between_i32 (x : int) (y : Int32.t) (z : int) : unit =
  let x = Int32.of_int x in
  let z = Int32.of_int z in
  let msg =
    Int32.to_string x ^ " < " ^ Int32.to_string y ^ " <= " ^ Int32.to_string z
  in
  assert_equal ~cmp:(fun a b -> a < b) ~msg x y;
  assert_equal ~cmp:(fun a b -> a <= b) ~msg y z

let tests =
  "Prob_exp"
  >::: [
         ( "prob" >:: fun _ ->
           let x = Variable.from_name "x" in
           let fns = [ x ] in
           let x_gt_0 = n_lt (Var x) (Num 0) in
           let p = prob ~sample_size:5000 fns x_gt_0 in
           (* there are fewer negatives than positives *)
           assert_between 0.48 p 0.52;
           let x_is_even = n_eq (n_mod (Var x) (Num 2)) (Num 0) in
           let p = prob ~sample_size:5000 fns x_is_even in
           assert_between 0.49 p 0.51;
           () );
         ( "infer_dom_1" >:: fun _ ->
           let x = Variable.from_name "x" in
           let x_gt_0 = b_and (n_ge (Var x) (Num 0)) (n_lt (Var x) (Num 10)) in
           match infer_dom x x_gt_0 with
           | Some (lo, hi) ->
               assert_equal (Int32.of_int 0) lo ~printer:Int32.to_string;
               assert_equal (Int32.of_int 9) hi ~printer:Int32.to_string;
               ()
           | None -> assert false );
         ( "infer_dom_2" >:: fun _ ->
           let x = Variable.from_name "x" in
           match infer_dom x (Bool true) with
           | Some (lo, hi) ->
               assert_equal (fst int32_range) lo ~printer:Int32.to_string;
               assert_equal (snd int32_range) hi ~printer:Int32.to_string;
               ()
           | None -> assert false );
         ( "infer_dom_3" >:: fun _ ->
           let x = Variable.from_name "x" in
           (* When the pre-condition is false, then no bound is found *)
           assert (infer_dom x (Bool false) |> Option.is_none) );
         ( "make_doms" >:: fun _ ->
           let x = Variable.from_name "x" in
           let _y = Variable.from_name "y" in
           let fns = [ x ] in
           (* asserts to true *)
           let goal = Bool true in
           let doms = make_doms fns goal in
           let lo, hi = Variable.Map.find x doms in
           assert_i32 lo Int32.(min_int |> to_int);
           assert_i32 hi Int32.(max_int |> to_int);
           (* asserts to false *)
           let goal = Bool false in
           let doms = make_doms fns goal in
           let lo, hi = Variable.Map.find x doms in
           assert_i32 lo 0;
           assert_i32 hi 1;
           (* lo=0 hi=9 *)
           let goal = b_or (n_eq (Var x) (Num 0)) (n_eq (Var x) (Num 9)) in
           let doms = make_doms fns goal in
           let lo, hi = Variable.Map.find x doms in
           assert_i32 lo 0;
           assert_i32 hi 9;
           () );
         ( "calc_sample_size" >:: fun _ ->
           let range (x : string) (n : int) =
             (Variable.from_name x, (Int32.zero, Int32.of_int n))
           in
           let doms = Variable.Map.of_list [ range "x" 3; range "y" 2 ] in
           let sample_size = calc_sample_size doms in
           assert_equal (2 * 3) sample_size ~printer:string_of_int;
           (* use a lower sample size *)
           let sample_size = calc_sample_size ~sample_size:(2 * 2) doms in
           assert_equal (2 * 2) sample_size ~printer:string_of_int );
         ( "make_sample_var" >:: fun _ ->
           let x = Variable.from_name "x" in
           let y = Variable.from_name "y" in
           let fns = [ x; y ] in
           let goal =
             b_and
               (b_and (n_gt (Var x) (Num 0)) (n_lt (Var x) (Num 10)))
               (b_and (n_gt (Var y) (Num 10)) (n_lt (Var y) (Num 15)))
           in
           let doms = make_doms fns goal in
           let env = make_sample_var doms in
           (* sample multiple values *)
           let i1 = env x in
           assert_between_i32 0 i1 10;
           let i2 = env x in
           assert_between_i32 0 i2 10;
           let i3 = env y in
           assert_between_i32 10 i3 15;
           let i4 = env y in
           assert_between_i32 10 i4 15;
           () );
         ( "cond_prob" >:: fun _ ->
           let x = Variable.from_name "x" in
           (* x ranges from 0 until 9 *)
           let pre = b_and (n_ge (Var x) (Num 0)) (n_lt (Var x) (Num 10)) in
           (* is even *)
           let goal = n_eq (n_mod (Var x) (Num 2)) (Num 0) in
           let p = cond_prob ~pre goal in
           (* there are fewer negatives than positives *)
           assert_between 0.2 p 0.4;
           () );
       ]

let _ = run_test_tt_main tests
