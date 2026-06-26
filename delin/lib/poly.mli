open Protocols

type t
val to_string : t -> string
val parameter : string -> t
val induction : string -> t
val of_int : int -> t
val of_indet : Indet.t -> t
val zero : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t

val div_mod : t -> Mono.t -> t * t

val fold : (Mono.t -> 'a -> 'a) -> 'a -> t -> 'a
val to_list : t -> Mono.t list
val of_list : Mono.t list -> t
val compare : t -> t -> int
val from_nexp : globals:Variable.Set.t -> Exp.nexp -> t
val to_nexp : t -> Exp.nexp

val group_by_parameters : candidates:Indet.t list -> t -> t Monic.Map.t
val try_scalar_quotient : t -> t -> int option
