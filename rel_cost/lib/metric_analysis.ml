open Stage0
open Protocols

(* Per-access analysis context. The record itself lives in
   [Analysis_ctx] so that [Bc.Analysis] and [Ua_analysis] can refer
   to it without cycling back through this module; re-exposed here
   for any caller that already references [Metric_analysis.t]. *)
type t = Analysis_ctx.t = {
  strategy : Analysis_strategy.t;
  locals : Variable.Set.t;
  index : Exp.nexp;
  divergence : Exp.bexp;
  config : Config.t;
  pre : Exp.bexp;
}

(* Public re-exports. The actual definitions live in [Index_cost],
   [Bc.Analysis], and [Ua_analysis] now; aliased here so external
   callers that already reference [Metric_analysis.{IndexCost,BC,UA}]
   keep working. *)
module IndexCost = Index_cost
module BC = Bc.Normalize.BC
module UA = Ua_analysis.UA

module Make (L : Logger.Logger) = struct
  (* Locally bind the per-logger analyses. Naming them [Bca] / [Uaa]
     avoids shadowing the outer [Bc] subdir-group module ([Bc.Axis] /
     [Bc.Modular] stay reachable from elsewhere in this file). *)
  module Bca = Bc.Analysis.Make (L)
  module Uaa = Ua_analysis.Make (L)

  let run_count (_ctx : Analysis_ctx.t) : Index_cost.t =
    Index_cost.from_cost (Cost.from_int ~value:1 ~exact:true ())

  let run ?(delin_bc = false) ?(pre = Exp.Bool true) (m : Metric.t)
      (config : Config.t) ~verbose ~strategy ~locals ~index ~divergence
      : Index_cost.t =
    let run =
      match m with
      | Metric.BankConflicts -> Bca.run_bc ~delin_bc
      | UncoalescedAccesses -> Uaa.run_ua
      | UncoalescedAccessesSat -> Uaa.run_ua_sat ~verbose
      | CountAccesses -> run_count
      | ActiveThreads -> Uaa.run_count_active_threads ~verbose
    in
    run { config; divergence; strategy; locals; index; pre }
end

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
