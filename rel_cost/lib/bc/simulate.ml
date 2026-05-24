open Protocols

(* Pre-existing rel_cost simulation: per-thread evaluation of the
   bank-conflict pattern via [Vectorized]. The per-thread evaluator
   errors out when the expression contains symbolic content it can't
   reduce; in that case the caller falls back to [fallback_cost] (a
   conservative [Vectorized.max_cost] upper bound).

   Pure module (no functor): the caller logs the error if desired. *)

(* Concrete per-thread evaluation. [Ok cost] when every thread can be
   evaluated; [Error msg] when symbolic content blocks evaluation. *)
let run (vec : Vectorized.t) (index : Exp.nexp) : (Cost.t, string) Result.t =
  Vectorized.bank_conflicts index vec

(* Conservative upper bound on bank-conflict cost (typically
   [threads_per_warp - 1] for a full warp). Used as the fallback when
   [run] errors. The returned [Cost.t] has [exact = false]. *)
let fallback_cost (vec : Vectorized.t) : Cost.t =
  Vectorized.max_cost Metric.BankConflicts vec
