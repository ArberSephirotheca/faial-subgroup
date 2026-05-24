(* Per-access analysis context. Originally lived as
   [Metric_analysis.t]; lifted out so [Bc_analysis] and [Ua_analysis]
   can refer to it without a dependency cycle through
   [Metric_analysis]. [Metric_analysis.run] constructs one of these
   and dispatches to the relevant analysis. *)
open Protocols

type t = {
  strategy : Analysis_strategy.t;
  locals : Variable.Set.t;
  index : Exp.nexp;
  divergence : Exp.bexp;
  config : Config.t;
  (* Kernel-level precondition consumed by [Bc_modular] under the
     v2 delin-BC oracle path. Default [Bool true] for callers that
     don't carry a kernel precondition. *)
  pre : Exp.bexp;
}
