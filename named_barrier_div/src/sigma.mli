(** Σ : Var → Modifier.t — the typing context. Variables not present in
    the context default to [Unif] (architectural / kernel-parameter
    variables that haven't been explicitly declared). *)

open Protocols

type t

val empty : t

val add : Variable.t -> Modifier.t -> t -> t

val find : Variable.t -> t -> Modifier.t

val locals : t -> Variable.Set.t
(** [locals s] is the set of variables [x] with [find x s = Local]. *)

val mentions_local : t -> Variable.Set.t -> bool
(** [mentions_local s vars] iff some [x ∈ vars] has [find x s = Local].
    Used by the routing helper [+_Σ b] to decide whether a guard's
    free variables include a local. *)
