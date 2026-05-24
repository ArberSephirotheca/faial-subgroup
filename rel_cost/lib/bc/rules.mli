open Protocols

(* The 5-rule cost decision over a delinearized + classified access.
   Pure: no logging, no functor, no state. The novel cost model from
   rel-cost-delin.md is expressed here as a small, total decision
   procedure consuming [Delinearize.t]. *)

type bc_outcome =
  | Exact of Cost.t
  | NeedsSimulation of Exp.nexp

val decide :
  config:Config.t ->
  tid_count:int ->
  Delinearize.t ->
  bc_outcome
