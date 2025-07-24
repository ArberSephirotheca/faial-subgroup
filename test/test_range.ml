open OUnit2
open Protocols

let inc_range (lb : int) (ub : int) (inc : int -> int) : int list =
  let rec iter n = if n <= ub then n :: iter (inc n) else [] in
  iter lb

let dec_range (lb : int) (ub : int) (dec : int -> int) : int list =
  let rec iter n = if n >= lb then n :: iter (dec n) else [] in
  iter ub

let plus_range (lb : int) (ub : int) (s : int) : int list =
  inc_range lb ub (fun x -> x + s)

let minus_range (lb : int) (ub : int) (s : int) : int list =
  dec_range lb ub (fun x -> x - s)

let mult_range (lb : int) (ub : int) (s : int) : int list =
  inc_range lb ub (fun x -> x * s)

let div_range (lb : int) (ub : int) (s : int) : int list =
  dec_range lb ub (fun x -> x / s)

let rec list_last : 'a list -> 'a = function
  | [] -> failwith "last: empty list"
  | [ x ] -> x
  | _ :: l -> list_last l

let list_first : 'a list -> 'a = List.hd

module ForStep = struct
  type kind = Plus | Minus | Div | Mult
  type t = kind * int

  let to_range : t -> Range.Step.t * Range.direction = function
    | Plus, n -> (Range.Step.Plus (Num n), Range.Increase)
    | Minus, n -> (Range.Step.Plus (Num n), Range.Decrease)
    | Mult, n -> (Range.Step.Mult (Num n), Range.Increase)
    | Div, n -> (Range.Step.Mult (Num n), Range.Decrease)
end

let range ~lb ~ub ~step : Range.t =
  let step, dir = ForStep.to_range step in
  let open Range in
  {
    var = Variable.from_name "x";
    lower_bound = Num lb;
    upper_bound = Num ub;
    step;
    dir;
    ty = C_type.int;
  }

let assert_int n1 n2 ~msg : unit =
  assert_equal n1 n2 ~printer:string_of_int ~msg

let eval (e : Exp.nexp) : int =
  match Exp.n_eval_res e with Ok e -> e | Error e -> failwith e

let assert_eval (expected : int) (e : Exp.nexp) =
  assert_equal expected (eval e) ~printer:string_of_int

let assert_first r expected =
  let given = eval (Range.first r) in
  assert_int expected given
    ~msg:
      ("expected: " ^ string_of_int expected ^ " but got: first("
     ^ Range.to_string r ^ ") = " ^ string_of_int given)

let assert_last r expected =
  let given = eval (Range.last r |> Option.get) in
  assert_int expected given
    ~msg:
      ("expected: " ^ string_of_int expected ^ " but got: last("
     ^ Range.to_string r ^ ") = " ^ string_of_int given)

let int_list l = "[" ^ String.concat ", " (List.map string_of_int l) ^ "]"

let assert_list (l1 : int list) (l2 : int list) : unit =
  assert_equal l1 l2 ~printer:int_list

let tests =
  "tests"
  >::: [
         ( "plus_range" >:: fun _ ->
           assert_list [ 0; 1; 2; 3 ] (plus_range 0 3 1);
           assert_list [ 3; 6; 9; 12 ] (plus_range 3 12 3);
           assert_list [ 0; 2 ] (plus_range 0 3 2) );
         ( "minus_range" >:: fun _ ->
           assert_list [ 3; 2; 1; 0 ] (minus_range 0 3 1);
           assert_list [ 13; 10; 7; 4 ] (minus_range 2 13 3);
           assert_list [ 3; 1 ] (minus_range 0 3 2) );
         ( "mult_range" >:: fun _ ->
           assert_list [ 1; 2; 4; 8 ] (mult_range 1 10 2);
           assert_list [ 3; 15; 75 ] (mult_range 3 76 5);
           assert_list [ 12 ] (mult_range 12 12 2);
           assert_list [ 10; 20 ] (mult_range 10 20 2);
           assert_list [ 1; 2; 4; 8; 16 ] (mult_range 1 16 2) );
         ( "div_range" >:: fun _ ->
           assert_list [ 10; 5; 2; 1 ] (div_range 1 10 2);
           assert_list [ 12 ] (div_range 12 12 2);
           assert_list [ 15; 5 ] (div_range 4 15 3) );
         ( "first" >:: fun _ ->
           let open ForStep in
           List.iter
             (fun (r, n) -> assert_first r n)
             [
               (range ~lb:0 ~ub:3 ~step:(Plus, 1), 0);
               (range ~lb:0 ~ub:3 ~step:(Minus, 1), 3);
               (range ~lb:0 ~ub:3 ~step:(Mult, 1), 0);
               (range ~lb:0 ~ub:3 ~step:(Div, 1), 3);
             ] );
         ( "last" >:: fun _ ->
           let open ForStep in
           List.iter
             (fun (r, n) -> assert_last r n)
             [
               (range ~lb:0 ~ub:3 ~step:(Plus, 1), 3);
               (range ~lb:3 ~ub:13 ~step:(Plus, 3), 12);
               (range ~lb:4 ~ub:15 ~step:(Plus, 3), 13);
               (range ~lb:0 ~ub:3 ~step:(Minus, 1), 0);
               (range ~lb:2 ~ub:13 ~step:(Minus, 3), 4);
               (range ~lb:4 ~ub:15 ~step:(Minus, 3), 6);
               (range ~lb:4 ~ub:15 ~step:(Div, 3), 5);
               (range ~lb:3 ~ub:76 ~step:(Div, 5), 3);
               (range ~lb:1 ~ub:10 ~step:(Mult, 2), 8);
               (range ~lb:3 ~ub:10 ~step:(Mult, 4), 3);
               (range ~lb:4 ~ub:15 ~step:(Mult, 3), 12);
               (range ~lb:3 ~ub:76 ~step:(Mult, 5), 75);
             ] );
         ( "gen_highest_power" >:: fun _ ->
           assert_eval 8 (Range.gen_highest_power ~base:2 (Num 10));
           assert_eval 8 (Range.gen_highest_power ~base:2 (Num 9));
           assert_eval 8 (Range.gen_highest_power ~base:2 (Num 8));
           assert_eval 1 (Range.gen_highest_power ~base:2 (Num 1));
           () );
         ( "highest_power" >:: fun _ ->
           assert_eval 8 (Range.highest_power ~base:2 (Num 10));
           assert_eval 8
             (Range.highest_power ~base:2 (Binary (Plus, Num 5, Num 5)));
           () );
         ( "all" >:: fun _ ->
           let open ForStep in
           List.concat_map
             (fun lb ->
               List.concat_map
                 (fun ub -> List.map (fun s -> (lb, ub, s)) [ 1; 2; 3; 4; 5 ])
                 [ 10; 11; 12; 13; 74; 75; 76; 77; 100 ])
             [ 0; 1; 2; 3; 4; 5; 6; 7 ]
           |> List.concat_map (fun (lb, ub, s) ->
                  (if s > 1 then
                     [ (*range ~lb ~ub ~step:(Div, s), div_range lb ub s*) ]
                   else [])
                  @ (if s > 1 && lb > 0 then
                       [ (range ~lb ~ub ~step:(Mult, s), mult_range lb ub s) ]
                     else [])
                  @ [
                      (range ~lb ~ub ~step:(Plus, s), plus_range lb ub s);
                      (range ~lb ~ub ~step:(Minus, s), minus_range lb ub s);
                    ])
           |> List.iter (fun (r, l) ->
                  assert_first r (list_first l);
                  assert_last r (list_last l)) );
       ]

let _ = run_test_tt_main tests
