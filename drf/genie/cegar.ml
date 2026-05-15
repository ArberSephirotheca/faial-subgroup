(* Per-CEGAR-round acceptance: ordered short-circuit over three
   tiers of checks.

   Each candidate Φ from MaxSAT is evaluated by three checks of
   increasing cost:

   - Tier 1: single-thread reachability preserved. Cheapest — one
     SAT query per kernel asking whether [k.pre ∧ Φ ∧ runtime] still
     admits a thread state.
   - Tier 2: co-reach pair-subset gate. Per-round rebuilds the
     under-Φ pair set and checks baseline ⊆ under-Φ keyed by
     [(kernel_name, array_name, id)].
   - DRF: full race-query pipeline. Most expensive.

   The tiers form a soundness chain: Tier 1 rejection implies Tier 2
   rejection (an unreachable access cannot co-exist with anything),
   and Tier 2 rejection means the race query would either still
   accept Φ or accept it spuriously by trivialising a fragment. So
   running cheaper tiers first only short-circuits Φs the later tiers
   would also reject — the accepted set is unchanged. *)

let check_three_tier
    ~(tier1 : unit -> bool)
    ~(tier2 : unit -> bool)
    ~(drf : unit -> bool)
    : bool =
  tier1 () && tier2 () && drf ()
