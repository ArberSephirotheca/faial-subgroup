open Protocols

(* Z3 BV-based modular oracle used by the v2 and v3 BC preprocessing.

   Persistent solver slot loaded with [kernel.pre], queried during
   axis classification ([gcd_value], single-stride) and after the
   per-axis rules return [NeedsSimulation] ([warp_injective],
   whole-index). Mirrors the lifecycle of [drf/lib/bound_check.ml]:
   [make] preloads, [release] resets, the per-query functions push
   the negated goal, check, pop. *)
type t

val make : timeout:int -> Exp.bexp -> t
val release : t -> unit
val with_slot : timeout:int -> Exp.bexp -> (t -> 'a) -> 'a

(* Divisor-ladder query: returns the proven [gcd(stride, bank_count)]
   under [kernel.pre], or [None] if no divisor query
   UNSAT-discharges. Each successful return value maps to a named
   Rocq theorem: [coprime] = 1, [bc_stride_{2,4,8,16}] for
   intermediate divisors, [bank_count] (= bank-blind) for the
   all-into-one-bank case. Subsumes the v2 endpoints. *)
val gcd_value :
  t -> stride:Exp.nexp -> bank_count:int -> int option

(* Whole-index modular injectivity query: true iff [kernel.pre]
   entails [Distinct (e[t1] mod bank_count, ..., e[tn] mod
   bank_count)] over the conservatively-filtered enabled warp
   tids. Soundness: [f_pairwise_distinct_supset]. Our Z3
   [Distinct] establishes [NoDup (List.map Bank.id L)] (third
   premise); since [Bank.id] is a function, distinct images force
   distinct preimages, giving [NoDup L], which transfers to
   [NoDup (filter_enabled vb vn)] (second premise) because
   [filter_enabled] is a positional sublist of L. The first
   premise [List.incl (filter_enabled vb vn) L] holds because our
   filter only excludes lanes whose divergence reduces to concrete
   [Bool false], so the actually-enabled lanes are a subset.
   Conclusion [f vb vn <= 1] (conflict cost 0). Returns true
   trivially when the enabled superset is empty or singleton. *)
val warp_injective :
  t ->
  config:Config.t ->
  divergence:Exp.bexp ->
  index:Exp.nexp ->
  bool
