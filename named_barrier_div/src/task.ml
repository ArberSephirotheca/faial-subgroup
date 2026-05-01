(* T = (Σ, π, δ, s)

   The four components of a task:
   - [sigma]:    typing context.
   - [pi]:       participation predicate. Conjunction of branching
                 guards along the path whose free variables include
                 some [Local]. Selects the cohort of threads routed
                 to this site.
   - [delta]:    dynamic path condition. Conjunction of guards on the
                 path whose free variables are confined to [Unif] /
                 [Iter] (loop ranges, iter-dependent guards, uniform
                 conditions). Captures iter-time reachability.
   - [residual]: the remaining program. *)

open Protocols
open Exp

type t = {
  sigma    : Sigma.t;
  pi       : bexp;
  delta    : bexp;
  residual : Code.t;
}

let make ?(pi = Bool true) ?(delta = Bool true) ~(sigma : Sigma.t)
    (residual : Code.t) : t =
  { sigma; pi; delta; residual }

(* π +_Σ b   /   δ +_Σ b

   Routes a guard [b] under context [sigma] into either [pi] (if [b]
   mentions any [Local]) or [delta] (otherwise). Returns the updated
   pair. *)
let route_guard (sigma : Sigma.t) ~(pi : bexp) ~(delta : bexp) (b : bexp)
    : bexp * bexp =
  let fv = b_free_names b Variable.Set.empty in
  if Sigma.mentions_local sigma fv then (b_and pi b, delta)
  else (pi, b_and delta b)
