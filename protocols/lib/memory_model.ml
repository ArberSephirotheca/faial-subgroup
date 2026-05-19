(* Memory-consistency-model assumptions threaded through the analyses.

   Distinct from [Architecture.t]: architecture describes the hardware
   shape (what counts as a thread, what globals exist); the memory
   model describes the inter-thread orderings the analysis treats as
   given.

   - [warp_synchronous]: pre-Volta semantics. Threads within the same
     warp execute in lockstep, so an implicit barrier holds between
     every statement for any two threads with equal warp id. Under
     this assumption a same-warp pair cannot race; cross-warp pairs
     keep normal race-detection semantics. Opt-in soundness downgrade:
     the verdict only holds on pre-Volta hardware (or with
     [-arch sm_60 -Xptxas -dlcm=cg] and no independent thread
     scheduling features). *)

type t = { warp_synchronous : bool }

let default : t = { warp_synchronous = false }

let to_string (m : t) : string =
  if m.warp_synchronous then "warp-synchronous" else "standard"
