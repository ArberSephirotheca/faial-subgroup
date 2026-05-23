open Protocols

(* Per-axis classification used by the delin-based BC preprocessing. *)
type axis_class =
  | BankBlind
      (* sigma_hat = 0: axis contributes 0 to bank diversity regardless
         of subscript value. *)
  | Uniform
      (* Subscript is warp-uniform: same value on every thread, shifts
         base bank but doesn't add diversity. *)
  | Diverse of int
      (* Concrete normalized stride sigma_hat in [1, bank_count), with
         a warp-varying subscript proven injective on the warp-active
         set. *)
  | NotInjective of int
      (* Warp-varying subscript with concrete sigma_hat, but injectivity
         couldn't be proven. *)
  | Unknown
      (* Stride couldn't be reduced to an integer, or some other
         classification step bailed. *)

type bc_outcome =
  | Exact of Cost.t
  | NeedsSimulation of Exp.nexp

val classify_index :
  config:Config.t ->
  locals:Variable.Set.t ->
  Delin.Index.t ->
  axis_class list

val decide :
  config:Config.t ->
  tid_count:int ->
  reduced:Exp.nexp ->
  axis_class list ->
  bc_outcome
