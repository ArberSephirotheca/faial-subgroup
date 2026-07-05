open Protocols

type t
val compare : t -> t -> int
val to_string : t -> string
val parameter : string -> t
val induction : string -> t
val ( * ) : t -> t -> t

val try_div : t -> t -> t option

val coeff : t -> int
val monic : t -> Monic.t
val factors : t -> (Indet.t * int) list
val of_factors : ?coeff:int -> (Indet.t * int) list -> t
val of_monic : ?coeff:int -> Monic.t -> t

val fold : (Indet.t -> int -> 'a -> 'a) -> 'a -> t -> 'a
val filter : (Indet.t -> int -> bool) -> t -> t

val has_induction : t -> bool
val has_parameter : t -> bool
val is_const : t -> bool
val nfactors: t -> int
val to_nexp : t -> Exp.nexp
