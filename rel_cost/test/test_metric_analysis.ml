open Rel_cost
open Protocols

let cfg : Config.t =
  let block_dim = Dim3.make ~x:32 () in
  let grid_dim = Dim3.one in
  Config.make ~block_dim ~grid_dim ()

(* Test-specific Alcotest testable types *)
let bc_testable : (Exp.nexp * Metric_analysis.BC.t) Alcotest.testable =
  let pp : (Exp.nexp * Metric_analysis.BC.t) Fmt.t =
   fun fmt bc_result ->
    Format.fprintf fmt "%s" (Metric_analysis.BC.to_string bc_result)
  in
  let equal :
      Exp.nexp * Metric_analysis.BC.t -> Exp.nexp * Metric_analysis.BC.t -> bool
      =
    ( = )
  in
  Alcotest.testable pp equal

let ua_testable : (Exp.nexp * Metric_analysis.UA.t) Alcotest.testable =
  let pp : (Exp.nexp * Metric_analysis.UA.t) Fmt.t =
   fun fmt ua_result ->
    Format.fprintf fmt "%s" (Metric_analysis.UA.to_string ua_result)
  in
  let equal :
      Exp.nexp * Metric_analysis.UA.t -> Exp.nexp * Metric_analysis.UA.t -> bool
      =
    ( = )
  in
  Alcotest.testable pp equal

let assert_bc ?(cfg : Config.t = cfg)
    ?(locals : Variable.Set.t = Variable.Set.empty)
    ~(expected : Exp.nexp * Metric_analysis.BC.t) ~(given : Exp.nexp) () : unit
    =
  let given = Metric_analysis.BC.from_nexp cfg locals given in
  Alcotest.check bc_testable "BC analysis" expected given

let assert_ua ?(cfg : Config.t = cfg)
    ?(locals : Variable.Set.t = Variable.Set.empty)
    ~(expected : Exp.nexp * Metric_analysis.UA.t) ~(given : Exp.nexp) () : unit
    =
  let given = Metric_analysis.UA.from_nexp cfg locals given in
  Alcotest.check ua_testable "UA analysis" expected given

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

let test_bc () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  bc_any ~expected:tidx ~given:tidx;
  bc_uniform ~expected:(Num 10) ~given:(Num 10);
  bc_uniform
    ~expected:(Binary (Plus, Num 10, Num 20))
    ~given:(Binary (Plus, Num 10, Num 20));
  bc_any ~expected:tidx ~given:(Binary (Plus, tidx, Num 20));
  bc_any ~expected:tidx ~given:(Binary (Minus Signedness.Signed, tidx, Num 20));
  bc_any ~expected:tidx ~given:(Binary (Plus, tidx, Num 20));
  bc_any
    ~expected:(Binary (Mult, tidx, Num 20))
    ~given:(Binary (Mult, tidx, Num 20));
  bc_any
    ~expected:(Binary (Mult, tidx, Num 20))
    ~given:(Binary (Mult, Binary (Plus, tidx, Num 5), Num 20))

let test_ua () : unit =
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
    ~expected:(Binary (Plus, Num 10, Num 20));
  ua_any
    ~given:(Binary (Plus, tidx, Num 20))
    ~expected:(Binary (Plus, tidx, Num 20));
  ua_inc ~given:(Binary (Plus, tidx, tidy)) ~expected:tidx;
  ua_inc ~given:(Binary (Plus, tidx, x)) ~expected:tidx;
  ua_uniform ~given:(Binary (Plus, x, y)) ~expected:(Binary (Plus, x, y));
  ua_uniform
    ~given:(n_mult (n_plus (Num 1) x) y)
    ~expected:(n_mult (n_plus (Num 1) x) y)

let tests : unit Alcotest.test_case list =
  [ ("bc", `Quick, test_bc); ("ua", `Quick, test_ua) ]

let () = Alcotest.run "Index Analysis" [ ("test_predicates", tests) ]
