open Protocols

type t
val to_string : t -> string
val parameter : string -> t
val induction : string -> t
val of_int : int -> t
val of_indet : Indet.t -> t
val of_monic : Monic.t -> t
val zero : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t

val div_mod : t -> Mono.t -> t * t

val fold : (Mono.t -> 'a -> 'a) -> 'a -> t -> 'a
val to_mono : t -> Mono.t option
val to_mono_list : t -> Mono.t list
val to_monic_list : t -> Monic.t list
val of_list : Mono.t list -> t
val compare : t -> t -> int
val from_nexp : globals:Variable.Set.t -> Exp.nexp -> t
val to_nexp : t -> Exp.nexp

val try_scalar_quotient : t -> t -> int option

val coeff_of : Monic.t -> t -> int
val scale : int -> t -> t
val indets : t -> Indet.t list
val dot : t list -> t list -> t
val linear_combination : t list -> int list -> t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
