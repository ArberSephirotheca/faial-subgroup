open Protocols

type t
val compare : t -> t -> int
val to_string : t -> string
val to_nexp : t -> Exp.nexp
val from_nexp : globals:Variable.Set.t -> Exp.nexp -> t

val induction : string -> t
val parameter : string -> t

val is_induction : t -> bool
val is_parameter : t -> bool

(* When the indeterminate is a bare induction variable [Var v], return [Some v].
   Returns [None] for parameters or compound induction expressions. *)
val as_induction_var : t -> Variable.t option

module Map : Map.S with type key = t
