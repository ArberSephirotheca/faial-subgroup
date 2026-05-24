(* Index-level cost result returned by the per-access analyses.
   Originally lived in [Metric_analysis.IndexCost]; lifted out so the
   [Bc_analysis] and [Ua_analysis] modules can refer to it without a
   dependency cycle through [Metric_analysis]. [Metric_analysis] still
   re-exports it as [IndexCost] for backward compat with existing
   external callers. *)
type t = { code : Ra.Stmt.t; exact : bool }

let from_cost (c : Cost.t) : t =
  { code = Ra.Stmt.Tick (Cost.value c); exact = c.exact }

let to_cost (e : t) : (Cost.t, string) Result.t =
  match e.code with
  | Ra.Stmt.Tick n -> Ok (Cost.from_int ~value:n ~exact:e.exact ())
  | _ -> Error ("to_cost: " ^ Ra.Stmt.to_string e.code)

let to_string (e : t) : string = Ra.Stmt.to_string e.code
