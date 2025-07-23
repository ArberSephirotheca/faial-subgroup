open Rel_cost
open OUnit2
open Protocols

let cfg =
  let block_dim = Dim3.make ~x:32 () in
  let grid_dim = Dim3.one in
  Config.make ~block_dim ~grid_dim ()

let assert_bc ?(cfg=cfg) ?(locals=Variable.Set.empty) ~expected ~given () : unit =
  let given =
    Index_analysis.BC.from_nexp cfg locals given
  in
  let msg =
    "Expected: " ^ Index_analysis.BC.to_string expected ^ "\n" ^
    "Given: " ^ Index_analysis.BC.to_string given
  in
  assert_equal expected given ~msg

let assert_ua ?(cfg=cfg) ?(locals=Variable.Set.empty) ~expected ~given:(given:Exp.nexp) () : unit =
  let given = Index_analysis.UA.from_nexp cfg locals given in
  let msg =
    "Expected: " ^ Index_analysis.UA.to_string expected ^ "\n" ^
    "Given: " ^ Index_analysis.UA.to_string given
  in
  assert_equal expected given ~msg

let bc_any ~expected ~given : unit =
  assert_bc ~expected:(expected, Any) ~given ()

let bc_uniform ~expected ~given : unit =
  assert_bc ~expected:(expected, Uniform) ~given ()

let ua_any ~expected ~given : unit =
  assert_ua ~expected:(expected, AnyAccurate) ~given ()

let ua_uniform ~expected ~given : unit =
  assert_ua ~expected:(expected, Uniform) ~given ()

let ua_const ~expected ~given : unit =
  assert_ua ~expected:(expected, Constant) ~given ()

let ua_inc ~expected ~given : unit =
  assert_ua ~expected:(expected, Inc) ~given ()

let tests = "test_predicates" >::: [
  "bc" >:: (fun _ ->
    let open Exp in
    let tidx = Var Variable.tid_x in
    bc_any ~expected:tidx ~given:tidx;
    bc_uniform ~expected:(Num 10) ~given:(Num 10);
    bc_uniform ~expected:(Binary (Plus, Num 10, Num 20)) ~given:(Binary (Plus, Num 10, Num 20));
    bc_any
      ~expected:tidx
      ~given:(Binary (Plus, tidx, Num 20));
    bc_any
      ~expected:tidx
      ~given:(Binary (Minus, tidx, Num 20));
    bc_any ~expected:tidx ~given:(Binary (Plus, tidx, Num 20));
    bc_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, tidx, Num 20));
    bc_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, Binary (Plus, tidx, Num 5), Num 20));
    ()
  );
  "ua" >:: (fun _ ->
    let open Exp in
    let tidx = Var Variable.tid_x in
    let tidy = Var Variable.tid_y in
    let x = Var (Variable.from_name "x") in
    let y = Var (Variable.from_name "y") in
    ua_any ~expected:tidx ~given:tidx;
    ua_uniform ~expected:tidy ~given:tidy;
    ua_const ~expected:(Num 10) ~given:(Num 10);
    ua_const
      ~given:(Binary (Plus, Num 10, Num 20))
      ~expected:(Binary (Plus, Num 10, Num 20))
    ;
    ua_any
      ~given:(Binary (Plus, tidx, Num 20))
      ~expected:(Binary (Plus, tidx, Num 20))
    ;
    ua_inc
      ~given:(Binary (Plus, tidx, tidy))
      ~expected:tidx
    ;
    ua_inc
      ~given:(Binary (Plus, tidx, x))
      ~expected:tidx
    ;
    ua_uniform
      ~given:(Binary (Plus, x, y))
      ~expected:(Binary (Plus, x, y))
    ;
    ua_uniform
      ~given:(n_mult (n_plus (Num 1) x) y)
      ~expected:(n_mult (n_plus (Num 1) x) y)
    ;
    (*
    ua_any
      ~expected:tidx
      ~given:(Binary (Minus, tidx, Num 20));
    ua_any ~expected:tidx ~given:(Binary (Plus, tidx, Num 20));
    ua_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, tidx, Num 20));
    ua_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, Binary (Plus, tidx, Num 5), Num 20));
    *)
  )
]

let _ = run_test_tt_main tests

