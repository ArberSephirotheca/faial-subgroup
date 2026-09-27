open Protocols
open Exp

(* SMT-backed counting queries on path-condition predicates: how many
   threads in a single warp/block satisfy [b]?

   Two flavors of SMT call are exposed:

   1. SAT-based primitives ([below], [above]). Returns [Sat k] with [k]
      a witnessing count value, [Unsat] if no such valuation exists, or
      [Unknown] on solver error / timeout. Cheap — the solver only has
      to find one witness.

   2. Optimizer-based primitives ([refine_min], [refine_max]). Used in
      [--precise] mode to refine a SAT witness into an actual extremum.
      Considerably more expensive — the optimizer must prove its result
      is optimal — so callers should usually try the SAT primitive first
      and only refine when a witness has been found. *)

type sat_result = Sat of int | Unsat | Unknown

let of_sat_witness : Rel_cost.Symbolic_metric_analysis.sat_witness -> sat_result
    = function
  | Sat k -> Sat k
  | Unsat_w -> Unsat
  | Unknown_w -> Unknown

let below
    ?(generator = Rel_cost.Symbolic_metric_analysis.Constraints.default)
    ?(timeout = 0) (cfg : Rel_cost.Config.t) (locals : Variable.Set.t)
    (b : bexp) (bound : int) : sat_result =
  Rel_cost.Symbolic_metric_analysis.sat_count
    ~generator ~timeout cfg locals b
    (fun count -> n_lt count (Num bound))
  |> of_sat_witness

let above
    ?(generator = Rel_cost.Symbolic_metric_analysis.Constraints.default)
    ?(timeout = 0) (cfg : Rel_cost.Config.t) (locals : Variable.Set.t)
    (b : bexp) (bound : int) : sat_result =
  Rel_cost.Symbolic_metric_analysis.sat_count
    ~generator ~timeout cfg locals b
    (fun count -> n_gt count (Num bound))
  |> of_sat_witness

(* [is_uniform b n] is true iff every valuation of [b] yields a cohort
   of exactly [n] threads — i.e. neither below nor above is SAT.
   Conservative on Unknown: returns false (treats indeterminate result
   as "not finished"), matching the prior behavior of [equals]. *)
let is_uniform ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (b : bexp) (n : int) : bool =
  match below ~timeout cfg locals b n with
  | Sat _ | Unknown -> false
  | Unsat -> (
      match above ~timeout cfg locals b n with
      | Sat _ | Unknown -> false
      | Unsat -> true)

(* [at_most b n] is true iff every valuation of [b] yields a cohort of
   at most [n] threads. Conservative on Unknown — used by Phase.can_admit
   for rule A's budget premise. *)
let at_most ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (b : bexp) (n : int) : bool =
  match above ~timeout cfg locals b n with
  | Unsat -> true
  | Sat _ | Unknown -> false

(* Single-tid SAT for block-wide barriers. Returns [Some (x, y, z)] if
   there is a valid block-thread that fails [arrive_cohort] under the
   kernel pre — i.e. a missing participant. Returns [None] on UNSAT
   (every thread arrives) or on solver error / timeout (conservative).

   Uses no Vectorizer, no distinctness — just one tid triple as free
   variables, the kernel pre, the cohort negation, and the in-block
   bounds. Cheap even for 1024-thread blocks. *)
let find_missing_thread ?(timeout = 0) (cfg : Rel_cost.Config.t)
    ~(pre : bexp) (arrive_cohort : bexp) : (int * int * int) option =
  let bdim = cfg.block_dim in
  let in_block =
    Exp.b_and_ex
      [
        n_le (Num 0) (Var Variable.tid_x);
        n_lt (Var Variable.tid_x) (Num bdim.x);
        n_le (Num 0) (Var Variable.tid_y);
        n_lt (Var Variable.tid_y) (Num bdim.y);
        n_le (Num 0) (Var Variable.tid_z);
        n_lt (Var Variable.tid_z) (Num bdim.z);
      ]
  in
  let goal =
    Formula.make (Exp.b_not arrive_cohort)
    |> Formula.assume pre
    |> Formula.assume in_block
  in
  let module S = Gen_z3.Bv64Gen in
  let witnesses : Exp.nexp list =
    [ Var Variable.tid_x; Var Variable.tid_y; Var Variable.tid_z ]
  in
  match S.solve_with_int_witnesses ~timeout goal witnesses with
  | Ok (Some [ Some x; Some y; Some z ]) -> Some (x, y, z)
  | _ -> None

(* Sub-warp Oversize detection: returns a list of [bound + 1] distinct
   tids that all satisfy [arrive_cohort], if such a configuration exists.
   A non-empty result is a witness that the cohort can hold strictly
   more than [bound] participants — the bug for a [bar.sync] expecting
   exactly [bound]. Returns [None] on UNSAT or solver error.

   Uses [Symbolic_metric_analysis.sat_n_distinct_in_cohort] with
   [n = bound + 1]; the SMT problem has [3 * (bound + 1)] tid variables
   and [bound + 1 choose 2] pairwise-distinct disjunctions — small for
   typical sub-warp counts (e.g. 33 tids for [bar.sync 0, 32]). *)
let exceeds_cardinality ?(timeout = 0) (cfg : Rel_cost.Config.t)
    ~(pre : bexp) (arrive_cohort : bexp) (bound : int) :
    (int * int * int) list option =
  Rel_cost.Symbolic_metric_analysis.sat_n_distinct_in_cohort ~timeout cfg
    ~pre ~n:(bound + 1) arrive_cohort

(* Optimizer-based refinement. Returns the actual minimum / maximum
   cohort size, or [None] on timeout / error. Used in [--precise] mode
   to upgrade a SAT witness into a tight extremum. *)
let refine_min
    ?(generator = Rel_cost.Symbolic_metric_analysis.Constraints.default)
    ?(timeout = 0) (cfg : Rel_cost.Config.t) (locals : Variable.Set.t)
    (b : bexp) : int option =
  Rel_cost.Symbolic_metric_analysis.count_active_threads
    ~strategy:Gen_z3.Optimizer.Strategy.Minimize ~generator ~timeout cfg locals
    b (Num 0)

let refine_max
    ?(generator = Rel_cost.Symbolic_metric_analysis.Constraints.default)
    ?(timeout = 0) (cfg : Rel_cost.Config.t) (locals : Variable.Set.t)
    (b : bexp) : int option =
  Rel_cost.Symbolic_metric_analysis.count_active_threads
    ~strategy:Gen_z3.Optimizer.Strategy.Maximize ~generator ~timeout cfg locals
    b (Num 0)
