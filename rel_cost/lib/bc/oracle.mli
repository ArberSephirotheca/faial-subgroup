open Protocols

(* Z3 BV-based modular oracle used by the v2 BC preprocessing.

   Persistent solver slot loaded with [kernel.pre], queried per-stride
   during axis classification. Mirrors the lifecycle of
   [drf/lib/bound_check.ml]: [make] preloads, [release] resets, the
   per-query functions push the negated goal, check, pop. *)
type t

val make : timeout:int -> Exp.bexp -> t
val release : t -> unit
val with_slot : timeout:int -> Exp.bexp -> (t -> 'a) -> 'a

(* True iff [kernel.pre] entails [stride mod bank_count = 0]. UNSAT
   on [pre /\ ~(stride mod bank_count = 0)]. *)
val bank_blind : t -> stride:Exp.nexp -> bank_count:int -> bool

(* True iff [kernel.pre] entails [gcd(stride, bank_count) = 1].
   Encoded as: [pre /\ \exists p in prime_factors(bank_count).
   stride mod p = 0] is UNSAT. For [bank_count = 32] this is the
   single-prime check [stride mod 2 = 0]. *)
val coprime : t -> stride:Exp.nexp -> bank_count:int -> bool
