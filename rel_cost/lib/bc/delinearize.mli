open Protocols

(* BC's encapsulation of the delinearization step: takes an
   [Exp.nexp] (typically the output of [Normalize.strip]), invokes
   delin's polynomial driver, and labels each recovered axis with the
   metadata BC's cost decider needs. Consumes the optional modular
   oracle to discharge symbolic strides.

   This is the BC-side analogue of [drf/lib/delinearize.ml]: the
   scientific "what delin contributes to BC" piece, isolated from
   both the baseline pre-pass ([Normalize]) and the cost model
   ([Rules]). *)

(* Per-axis classification produced by [from_exp]. *)
type axis_class =
  | BankBlind
  | Uniform
  | Diverse of int
  | NotInjective of int
  | Unknown

(* Result of delinearizing a single BC access. [axes = []] means the
   delin algorithm produced no candidate; downstream rules treat
   this as "fall through to simulation". *)
type t = {
  reduced : Exp.nexp;
  axes : axis_class list;
}

val from_exp :
  ?oracle:Oracle.t ->
  config:Config.t ->
  locals:Variable.Set.t ->
  Exp.nexp ->
  t
