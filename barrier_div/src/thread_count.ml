open Protocols
open Exp

(* SMT-backed counting queries on path-condition predicates: how many
   threads in a single warp/block satisfy [b]? Wraps Rel_cost's
   [count_active_threads] so the rest of the analysis stays SMT-free
   at the call sites. *)

let max_count
    ?(generator = Rel_cost.Symbolic_metric_analysis.Constraints.default)
    ?(timeout = 0) (cfg : Rel_cost.Config.t) (locals : Variable.Set.t)
    (b : bexp) : int option =
  Rel_cost.Symbolic_metric_analysis.count_active_threads
    ~strategy:Gen_z3.Optimizer.Strategy.Maximize ~generator ~timeout cfg locals
    b (Num 0)

let min_count
    ?(generator = Rel_cost.Symbolic_metric_analysis.Constraints.default)
    ?(timeout = 0) (cfg : Rel_cost.Config.t) (locals : Variable.Set.t)
    (b : bexp) : int option =
  Rel_cost.Symbolic_metric_analysis.count_active_threads
    ~strategy:Gen_z3.Optimizer.Strategy.Minimize ~generator ~timeout cfg locals
    b (Num 0)

(* [equals cfg locals b n] is true iff every valuation of free variables
   in [b] yields a cohort of exactly [n] threads. Used by Phase.is_finished
   for rule F's exact-match fire premise. *)
let equals ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (b : bexp) (n : int) : bool =
  match (max_count ~timeout cfg locals b, min_count ~timeout cfg locals b) with
  | Some mx, Some mn -> mx = n && mn = n
  | _ -> false

(* [at_most cfg locals b n] is true iff every valuation of [b] yields
   a cohort of at most [n] threads. Used by Phase.can_admit for rule A's
   budget premise. SMT failure is treated conservatively as "budget
   exceeded" — the merge is rejected, not silently allowed. *)
let at_most ?(timeout = 0) (cfg : Rel_cost.Config.t)
    (locals : Variable.Set.t) (b : bexp) (n : int) : bool =
  match max_count ~timeout cfg locals b with
  | Some mx -> mx <= n
  | None -> false
