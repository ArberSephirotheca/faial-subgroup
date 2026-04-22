open Protocols
open Drf
open OUnit2
open Exp

let x = Variable.from_name "x"
let y = Variable.from_name "y"
let b = Bool true

let r =
  Range.
    {
      var = Variable.from_name "z";
      lower_bound = Num 1;
      upper_bound = Num 2;
      step = Plus (Num 1);
      dir = Increase;
      ty = C_type.int;
    }

let write a = Access.{ array = a; index = []; mode = Mode.Write None }
let x_acc = Unsynced.Access (write x)
let y_acc = Unsynced.Access (write y)

let assert_filter i (expected : Unsynced.t option) =
  let given = Unsynced.filter_by_location x i in
  let msg =
    match (expected, given) with
    | Some _, None -> "given none, expecting:"
    | None, Some _ -> "expecting none, given:"
    | Some l1, Some l2 ->
        "expecting:\n" ^ Unsynced.to_string l1 ^ "\ngiven:\n"
        ^ Unsynced.to_string l2
    | None, None -> ""
  in
  assert_equal given expected ~msg

let expect_none l = assert_filter (Cond (b, l)) None
let expect_some l1 l2 = assert_filter (Cond (b, l1)) (Some (Cond (b, l2)))

let tests =
  "locsplit"
  >::: [
         ( "lvl1" >:: fun _ ->
           (* Empty code, should return none *)
           expect_none Skip;
           (* Single access, should return same *)
           expect_some x_acc x_acc;
           (* y-accesses are filtered out *)
           expect_none y_acc;
           (* y-accesses are filtered out *)
           expect_some (Seq (x_acc, y_acc)) (Seq (x_acc, Skip));
           expect_some
             (Seq (x_acc, Seq (Assert b, y_acc)))
             (Seq (x_acc, Seq (Assert b, Skip)));
           expect_some
             (Seq (x_acc, Seq (Assert b, Seq (Assert b, Assert b))))
             (Seq (x_acc, Seq (Assert b, Seq (Assert b, Assert b))));
           () );
         ( "lvl2" >:: fun _ ->
           expect_some
             (Seq (Cond (b, x_acc), y_acc))
             (Seq (Cond (b, x_acc), Skip));
           expect_some
             (Seq (Cond (b, x_acc), Seq (Assert b, y_acc)))
             (Seq (Cond (b, x_acc), Seq (Assert b, Skip)));
           expect_some
             (Seq (Loop (r, Cond (b, x_acc)), Seq (Assert b, y_acc)))
             (Seq (Loop (r, Cond (b, x_acc)), Seq (Assert b, Skip)));
           expect_some
             (Seq
                ( Loop (r, Cond (b, x_acc)),
                  Seq (Assert b, Seq (Assert b, Seq (Assert b, y_acc))) ))
             (Seq
                ( Loop (r, Cond (b, x_acc)),
                  Seq (Assert b, Seq (Assert b, Seq (Assert b, Skip))) )) );
       ]

let _ = run_test_tt_main tests
