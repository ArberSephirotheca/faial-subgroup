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
    ~expected:(Binary (Plus Signedness.Signed, Num 10, Num 20))
    ~given:(Binary (Plus Signedness.Signed, Num 10, Num 20));
  bc_any ~expected:tidx ~given:(Binary (Plus Signedness.Signed, tidx, Num 20));
  bc_any ~expected:tidx ~given:(Binary (Minus Signedness.Signed, tidx, Num 20));
  bc_any ~expected:tidx ~given:(Binary (Plus Signedness.Signed, tidx, Num 20));
  bc_any
    ~expected:(Binary (Mult Signedness.Signed, tidx, Num 20))
    ~given:(Binary (Mult Signedness.Signed, tidx, Num 20));
  bc_any
    ~expected:(Binary (Mult Signedness.Signed, tidx, Num 20))
    ~given:(Binary (Mult Signedness.Signed, Binary (Plus Signedness.Signed, tidx, Num 5), Num 20))

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
    ~given:(Binary (Plus Signedness.Signed, Num 10, Num 20))
    ~expected:(Binary (Plus Signedness.Signed, Num 10, Num 20));
  ua_any
    ~given:(Binary (Plus Signedness.Signed, tidx, Num 20))
    ~expected:(Binary (Plus Signedness.Signed, tidx, Num 20));
  ua_inc ~given:(Binary (Plus Signedness.Signed, tidx, tidy)) ~expected:tidx;
  ua_inc ~given:(Binary (Plus Signedness.Signed, tidx, x)) ~expected:tidx;
  ua_uniform ~given:(Binary (Plus Signedness.Signed, x, y)) ~expected:(Binary (Plus Signedness.Signed, x, y));
  ua_uniform
    ~given:(n_mult (n_plus (Num 1) x) y)
    ~expected:(n_mult (n_plus (Num 1) x) y)

(* --- Bc.Axis (delin-based BC preprocessing) tests ----------------- *)

(* Run [bc_preprocess]-style logic from the outside: invoke [Make.run]
   with [delin_bc:true] and read the value back. [exact:true] in the
   returned [IndexCost.t] signals the [Exact] branch fired; [exact:false]
   signals a simulation fallback (or [max_cost] under simulation
   failure). *)
let run_bc_delin ?(pre = Exp.Bool true) ~cfg
    ?(locals = Variable.Set.empty) (index : Exp.nexp)
    : Metric_analysis.IndexCost.t =
  Metric_analysis.Silent.run ~delin_bc:true ~pre Metric.BankConflicts cfg
    ~verbose:false
    ~strategy:Analysis_strategy.OverApproximation
    ~locals ~index ~divergence:(Bool true)

let run_bc_legacy ~cfg ?(locals = Variable.Set.empty) (index : Exp.nexp)
    : Metric_analysis.IndexCost.t =
  Metric_analysis.Silent.run Metric.BankConflicts cfg ~verbose:false
    ~strategy:Analysis_strategy.OverApproximation
    ~locals ~index ~divergence:(Bool true)

(* Direct unit tests against [Bc.Rules.decide], exercising each of
   the five decision branches with hand-built [Bc.Delinearize.t]
   values. *)
let test_bc_rules_decide () : unit =
  let cfg32 = cfg in
  let mk reduced axes : Bc.Delinearize.t = { reduced; axes } in
  (* Rule 1: all axes Uniform → Exact 0 *)
  let r =
    Bc.Rules.decide ~config:cfg32 ~tid_count:32
      (mk (Exp.Num 0) [ Bc.Delinearize.Uniform ])
  in
  (match r with
   | Bc.Rules.Exact c -> Alcotest.(check int) "all-uniform" 0 (Cost.value c)
   | Bc.Rules.NeedsSimulation _ -> Alcotest.fail "expected Exact for all-uniform");
  (* Rule 2: single warp-varying BankBlind axis → Exact (n-1) *)
  let r =
    Bc.Rules.decide ~config:cfg32 ~tid_count:32
      (mk (Exp.Num 0) [ Bc.Delinearize.BankBlind; Bc.Delinearize.Uniform ])
  in
  (match r with
   | Bc.Rules.Exact c -> Alcotest.(check int) "single bank-blind" 31 (Cost.value c)
   | Bc.Rules.NeedsSimulation _ -> Alcotest.fail "expected Exact for bank-blind");
  (* Rule 3a: single warp-varying Diverse axis g=1 → Exact 0 *)
  let r =
    Bc.Rules.decide ~config:cfg32 ~tid_count:32
      (mk (Exp.Num 0) [ Bc.Delinearize.Diverse 1 ])
  in
  (match r with
   | Bc.Rules.Exact c ->
       Alcotest.(check int) "diverse g=1 conflict-free" 0 (Cost.value c)
   | Bc.Rules.NeedsSimulation _ ->
       Alcotest.fail "expected Exact for diverse g=1");
  (* Rule 3b: single warp-varying Diverse axis g=2 → Exact 1 (2-way) *)
  let r =
    Bc.Rules.decide ~config:cfg32 ~tid_count:32
      (mk (Exp.Num 0) [ Bc.Delinearize.Diverse 2 ])
  in
  (match r with
   | Bc.Rules.Exact c -> Alcotest.(check int) "diverse g=2" 1 (Cost.value c)
   | Bc.Rules.NeedsSimulation _ ->
       Alcotest.fail "expected Exact for diverse g=2");
  (* Rule 4: multiple warp-varying axes → NeedsSimulation *)
  let r =
    Bc.Rules.decide ~config:cfg32 ~tid_count:32
      (mk (Exp.Num 42)
         [ Bc.Delinearize.Diverse 1; Bc.Delinearize.Diverse 1 ])
  in
  (match r with
   | Bc.Rules.NeedsSimulation e ->
       Alcotest.(check int) "multi-warp-varying" 42
         (match e with Num n -> n | _ -> Alcotest.fail "expected Num 42")
   | Bc.Rules.Exact _ -> Alcotest.fail "expected NeedsSimulation");
  (* Rule 5: any Unknown → NeedsSimulation *)
  let r =
    Bc.Rules.decide ~config:cfg32 ~tid_count:32
      (mk (Exp.Num 7) [ Bc.Delinearize.Unknown ])
  in
  match r with
  | Bc.Rules.NeedsSimulation _ -> ()
  | Bc.Rules.Exact _ -> Alcotest.fail "expected NeedsSimulation for Unknown"

(* End-to-end via [Make.run] with the toggle: confirms that the delin
   path produces the same cost values as the legacy path for the
   handful of cases the decision rules can resolve to Exact. *)
let test_bc_delin_e2e () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let bdim_x = Var Variable.bdim_x in
  (* tile[tx][ty]-style: tx * blockDim.x stripped down to tx after the
     BC.from_nexp pre-pass kills warp-uniform terms. With blockDim.x
     = 32 the outer stride is bank-blind → Exact 31. Legacy path
     simulates and gets the same answer. *)
  let idx = Binary (Mult Signedness.Signed, tidx, bdim_x) in
  let delin = run_bc_delin ~cfg idx in
  let legacy = run_bc_legacy ~cfg idx in
  let delin_cost =
    Metric_analysis.IndexCost.to_cost delin |> Result.get_ok |> Cost.value
  in
  let legacy_cost =
    Metric_analysis.IndexCost.to_cost legacy |> Result.get_ok |> Cost.value
  in
  Alcotest.(check int) "delin vs legacy parity: tx * blockDim.x"
    legacy_cost delin_cost

(* --- Precision-gain fixtures -------------------------------------- *)

(* Each fixture compares the legacy and delin paths on a single
   indexing expression. The legacy path runs [Vectorized.bank_conflicts]
   directly; with symbolic [blockDim.*] or [gridDim.*] terms still
   present, the per-thread evaluator errors out and the path falls
   back to [Vectorized.max_cost], which is conservative
   (always [threads_per_warp - 1] and [exact = false]). The delin
   path substitutes the dim variables from [Config] before classifying
   axes, so concretely-bounded launches resolve to [Exact] with the
   true cost. The "precision win" is either a lower exact value than
   the legacy approximation, or the same value tagged exact rather
   than approximate. *)
let check_fixture ?(pre = Exp.Bool true) ~msg ~cfg ~index
    ~expected_delin_value ~expected_delin_exact ~expected_legacy_value
    ~expected_legacy_exact () : unit =
  let delin = run_bc_delin ~pre ~cfg index in
  let legacy = run_bc_legacy ~cfg index in
  let delin_cost = Metric_analysis.IndexCost.to_cost delin |> Result.get_ok in
  let legacy_cost = Metric_analysis.IndexCost.to_cost legacy |> Result.get_ok in
  Alcotest.(check int)
    (msg ^ " delin value") expected_delin_value (Cost.value delin_cost);
  Alcotest.(check bool)
    (msg ^ " delin exact") expected_delin_exact delin_cost.exact;
  Alcotest.(check int)
    (msg ^ " legacy value") expected_legacy_value (Cost.value legacy_cost);
  Alcotest.(check bool)
    (msg ^ " legacy exact") expected_legacy_exact legacy_cost.exact

(* Helper for building cfgs with different launch dims. *)
let cfg_of ?(bx = 32) ?(by = 1) ?(bz = 1) ?(gx = 1) ?(gy = 1) ?(gz = 1) () =
  let block_dim = Dim3.make ~x:bx ~y:by ~z:bz () in
  let grid_dim = Dim3.make ~x:gx ~y:gy ~z:gz () in
  Config.make ~block_dim ~grid_dim ()

(* Fixture 1: same value, but delin upgrades [exact = false] to
   [exact = true]. [tx * blockDim.x] with [blockDim.x = 32] has outer
   stride 32 (≡ 0 mod 32: bank-blind), so all warp threads land on the
   same bank → single bank-blind axis, cost = n - 1 = 31. Legacy
   simulation errors on the symbolic [blockDim.x] inside [n_eval_res]
   and falls back to [max_cost = 31, exact = false]. *)
let test_precision_exact_flag () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let bdim_x = Var Variable.bdim_x in
  check_fixture
    ~msg:"tx * blockDim.x (bx=32) bank-blind axis"
    ~cfg:(cfg_of ~bx:32 ())
    ~index:(Binary (Mult Signedness.Signed, tidx, bdim_x))
    ~expected_delin_value:31 ~expected_delin_exact:true
    ~expected_legacy_value:31 ~expected_legacy_exact:false
    ()

(* Fixture 2: different values. [tx * gridDim.x] with [gridDim.x = 1]
   is a stride-1 access (σ̂ = 1, g = gcd(1, 32) = 1). The delin path
   recognises the single warp-varying diverse axis with coprime
   stride → cost 0 (conflict-free, every thread hits a different
   bank). Legacy simulation errors on [gridDim.x] and gives
   [max_cost = 31, exact = false] — a 31-unit over-approximation
   for an access that's actually optimal. *)
let test_precision_stride_one () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let gdim_x = Var Variable.gdim_x in
  check_fixture
    ~msg:"tx * gridDim.x (gx=1) coprime stride"
    ~cfg:(cfg_of ~gx:1 ())
    ~index:(Binary (Mult Signedness.Signed, tidx, gdim_x))
    ~expected_delin_value:0 ~expected_delin_exact:true
    ~expected_legacy_value:31 ~expected_legacy_exact:false
    ()

(* Fixture 3: different values, intermediate-g case.
   [tx * gridDim.x] with [gridDim.x = 4] gives σ̂ = 4, g = 4, so the
   warp threads land on 32/4 = 8 distinct banks with 4 threads per
   bank → cost = 32 * 4 / 32 - 1 = 3 (4-way conflict). Legacy
   simulation errors → 31, exact = false. *)
let test_precision_four_way () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let gdim_x = Var Variable.gdim_x in
  check_fixture
    ~msg:"tx * gridDim.x (gx=4) 4-way conflict"
    ~cfg:(cfg_of ~gx:4 ())
    ~index:(Binary (Mult Signedness.Signed, tidx, gdim_x))
    ~expected_delin_value:3 ~expected_delin_exact:true
    ~expected_legacy_value:31 ~expected_legacy_exact:false
    ()

(* Fixture 4: different values, 8-way conflict.
   [tx * gridDim.x] with [gridDim.x = 8]: σ̂ = 8, g = 8, cost =
   32 * 8 / 32 - 1 = 7. Legacy simulation errors → 31, exact = false. *)
let test_precision_eight_way () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let gdim_x = Var Variable.gdim_x in
  check_fixture
    ~msg:"tx * gridDim.x (gx=8) 8-way conflict"
    ~cfg:(cfg_of ~gx:8 ())
    ~index:(Binary (Mult Signedness.Signed, tidx, gdim_x))
    ~expected_delin_value:7 ~expected_delin_exact:true
    ~expected_legacy_value:31 ~expected_legacy_exact:false
    ()

(* Soundness regression: when [blockDim.x < threads_per_warp], the
   simulation's [tid_x = id mod block_dim.x] wraps within the warp,
   so a bare [Var tid_x] subscript is not warp-injective. The
   exact-cost formulas assume injectivity; without the
   [tid_is_warp_injective] guard in [Bc.Axis.injectivity_class] this
   case would return [Exact 15] when the true cost is 7. With the
   guard the axis classifies as [NotInjective] and the path falls
   through to simulation, matching the legacy fallback. *)
let test_precision_blockdim_below_warp_falls_back () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let bdim_x = Var Variable.bdim_x in
  let cfg16 = cfg_of ~bx:16 ~by:2 () in
  let delin = run_bc_delin ~cfg:cfg16
    (Binary (Mult Signedness.Signed, tidx, bdim_x))
  in
  let delin_cost = Metric_analysis.IndexCost.to_cost delin |> Result.get_ok in
  Alcotest.(check bool)
    "blockDim.x=16: tx*blockDim.x must not claim Exact"
    false delin_cost.exact

(* --- +1 padding fixtures (documents the v2 frontier) -------------- *)

(* The classic bank-conflict-avoidance pattern is [__shared__ T tile
   [H][W+1]] (pad column dim by one so transposed accesses land on
   different banks). Three sub-cases of how v1 handles this. *)

(* +1 case A: literal padded transpose, [tile[32][33]] accessed as
   [tile[tx][ty]] → flat [tx*33 + ty]. BC.from_nexp strips the
   warp-uniform [ty] (with [bx=32], [ty] is warp-uniform), leaving
   [tx*33]. Delin can't grab a [size_params] here because the [33]
   coefficient is a literal (not a parameter atom), so Greedy yields
   no candidate and we fall through to simulation. Simulation
   evaluates [tx*33 mod 32 = tx*1 mod 32] across the warp, finds 32
   distinct banks, returns Exact 0. Legacy takes the identical path.
   Both Exact 0; the +1 trick works correctly in both pipelines when
   the dim is a literal. *)
let test_padding_literal_transpose () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let tidy = Var Variable.tid_y in
  let idx =
    Binary (Plus Signedness.Signed,
            Binary (Mult Signedness.Signed, tidx, Num 33),
            tidy)
  in
  check_fixture
    ~msg:"+1 literal: tile[32][33] / tile[tx][ty]"
    ~cfg:(cfg_of ~bx:32 ())
    ~index:idx
    ~expected_delin_value:0 ~expected_delin_exact:true
    ~expected_legacy_value:0 ~expected_legacy_exact:true
    ()

(* +1 case B: parametric padded transpose,
   [__shared__ T tile[H][blockDim.x + 1]] accessed as [tile[tx][ty]]
   → flat [tx * (blockDim.x + 1) + ty]. BC strips [ty], leaving
   [tx * (blockDim.x + 1)]. Delin decomposes this into indices
   [tx; tx] with dim [blockDim.x] — both axes contain [tx], so the
   classifier returns [BankBlind; Diverse 1] and the multi-warp-
   varying rule defers to simulation. Simulation can't evaluate
   [blockDim.x] (it's not in [Vectorized]'s env) so it falls back
   to [max_cost = 31, exact = false]. Legacy follows the identical
   path. Both 31 approx. This is the canonical v2 modular-oracle
   target: proving [gcd(blockDim.x + 1, 32) = 1] from [kernel.pre]
   would let delin return [Exact 0] without simulation. *)
let test_padding_parametric_transpose () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let tidy = Var Variable.tid_y in
  let bdim_x = Var Variable.bdim_x in
  let idx =
    Binary (Plus Signedness.Signed,
            Binary (Mult Signedness.Signed, tidx,
                    Binary (Plus Signedness.Signed, bdim_x, Num 1)),
            tidy)
  in
  check_fixture
    ~msg:"+1 parametric: tile[H][bx+1] / tile[tx][ty]"
    ~cfg:(cfg_of ~bx:32 ())
    ~index:idx
    ~expected_delin_value:31 ~expected_delin_exact:false
    ~expected_legacy_value:31 ~expected_legacy_exact:false
    ()

(* +1 case C: parametric padded non-transpose,
   [__shared__ T tile[H][blockDim.x + 1]] accessed as [tile[ty][tx]]
   → flat [ty * (blockDim.x + 1) + tx]. BC strips the entire
   [ty * (...)] term (it's warp-uniform under [bx=32]), leaving just
   [tx]. Simulation handles it trivially: tx is in [Vectorized]'s
   env, distinct values across the warp, all banks distinct → Exact
   0. Legacy and delin both end up here. Bears noting that the BC
   strip is what makes this case easy — it kills the parametric
   stride before delin sees it. *)
let test_padding_parametric_non_transpose () : unit =
  let open Exp in
  let tidx = Var Variable.tid_x in
  let tidy = Var Variable.tid_y in
  let bdim_x = Var Variable.bdim_x in
  let idx =
    Binary (Plus Signedness.Signed,
            Binary (Mult Signedness.Signed, tidy,
                    Binary (Plus Signedness.Signed, bdim_x, Num 1)),
            tidx)
  in
  check_fixture
    ~msg:"+1 parametric non-transpose: tile[H][bx+1] / tile[ty][tx]"
    ~cfg:(cfg_of ~bx:32 ())
    ~index:idx
    ~expected_delin_value:0 ~expected_delin_exact:true
    ~expected_legacy_value:0 ~expected_legacy_exact:true
    ()

let tests : unit Alcotest.test_case list =
  [
    ("bc", `Quick, test_bc);
    ("ua", `Quick, test_ua);
    ("bc_rules_decide", `Quick, test_bc_rules_decide);
    ("bc_delin_e2e", `Quick, test_bc_delin_e2e);
    ("precision: exact-flag upgrade on bank-blind axis", `Quick,
     test_precision_exact_flag);
    ("precision: coprime stride → cost 0 vs 31", `Quick,
     test_precision_stride_one);
    ("precision: gridDim.x=4 → cost 3 vs 31", `Quick,
     test_precision_four_way);
    ("precision: gridDim.x=8 → cost 7 vs 31", `Quick,
     test_precision_eight_way);
    ("soundness: blockDim.x<warp falls back to simulation", `Quick,
     test_precision_blockdim_below_warp_falls_back);
    ("+1 padding: literal tile[32][33] transpose", `Quick,
     test_padding_literal_transpose);
    ("+1 padding: parametric tile[H][bx+1] transpose (v2 frontier)",
     `Quick, test_padding_parametric_transpose);
    ("+1 padding: parametric tile[H][bx+1] non-transpose", `Quick,
     test_padding_parametric_non_transpose);
    ("v2 oracle: stride = K * 32 → bank-blind → Exact 31", `Quick,
     fun () ->
       (* Models the canonical [extern __shared__] case with the
          host computing [stride = K * blockDim.x]. Under
          --assume-launch, [Synthesise_launches] lifts the binding
          into the wrapper as [decl stride = K * blockDim.x], and
          [Ra_compiler]'s upstream [subst_block_dim] replaces
          [blockDim.x] with the cfg value [32] before the analysis
          runs. So by the time [bc_preprocess] sees [kernel.pre],
          the binding has reduced to [stride == K * 32]. Test mirrors
          this post-subst shape. The v2 oracle proves
          [stride mod 32 = 0] (BV unfolds [K*32], formula UNSAT),
          so the outer axis classifies as BankBlind. Single
          warp-varying bank-blind axis → Exact 31. Legacy errors on
          the symbolic stride and falls back to max_cost. *)
       let open Exp in
       let tx = Var Variable.tid_x in
       let stride = Var (Variable.from_name "stride") in
       let k = Var (Variable.from_name "K") in
       let index = Binary (Mult Signedness.Signed, tx, stride) in
       let pre =
         n_eq stride (Binary (Mult Signedness.Signed, k, Num 32))
       in
       check_fixture
         ~msg:"stride = K * 32 → bank-blind"
         ~pre ~cfg:(cfg_of ~bx:32 ()) ~index
         ~expected_delin_value:31 ~expected_delin_exact:true
         ~expected_legacy_value:31 ~expected_legacy_exact:false
         ());
    ("v2 oracle: stride = 2*K+1 → coprime → Exact 0", `Quick,
     fun () ->
       (* Coprime-with-32 host expression (anything odd works).
          The oracle's coprime query for bank_count = 32 reduces
          to a single mod-2 check, which UNSAT-discharges given
          [stride == 2*K + 1]. Single warp-varying diverse-with-
          g=1 axis → Exact 0. *)
       let open Exp in
       let tx = Var Variable.tid_x in
       let stride = Var (Variable.from_name "stride") in
       let k = Var (Variable.from_name "K") in
       let index = Binary (Mult Signedness.Signed, tx, stride) in
       let pre =
         n_eq stride
           (Binary (Plus Signedness.Signed,
                    Binary (Mult Signedness.Signed, Num 2, k), Num 1))
       in
       check_fixture
         ~msg:"stride = 2K + 1 → coprime"
         ~pre ~cfg:(cfg_of ~bx:32 ()) ~index
         ~expected_delin_value:0 ~expected_delin_exact:true
         ~expected_legacy_value:31 ~expected_legacy_exact:false
         ());
    ("v2 oracle: no useful pre → NeedsSimulation (parity)", `Quick,
     fun () ->
       (* When [kernel.pre] is trivial (no equation for the
          stride), neither bank_blind nor coprime can fire, so
          the axis stays Unknown and we fall back to simulation.
          Same outcome as legacy: 31 approx. *)
       let open Exp in
       let tx = Var Variable.tid_x in
       let stride = Var (Variable.from_name "stride") in
       let index = Binary (Mult Signedness.Signed, tx, stride) in
       check_fixture
         ~msg:"no pre → Unknown"
         ~pre:(Bool true) ~cfg:(cfg_of ~bx:32 ()) ~index
         ~expected_delin_value:31 ~expected_delin_exact:false
         ~expected_legacy_value:31 ~expected_legacy_exact:false
         ());
  ]

let () = Alcotest.run "Index Analysis" [ ("test_predicates", tests) ]
