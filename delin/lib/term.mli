open Protocols

type t = int * Term_inner.t
val compare : t -> t -> int
val to_string : t -> string
val parameter : string -> t
val induction : string -> t
val ( * ) : t -> t -> t

val try_div : t -> t -> t option

val coeff : t -> int
val factors : t -> (Atom.t * int) list
val of_factors : ?coeff:int -> (Atom.t * int) list -> t

val fold : (Atom.t -> int -> 'a -> 'a) -> 'a -> t -> 'a
val filter : (Atom.t -> int -> bool) -> t -> t

val has_induction : t -> bool
val has_parameter : t -> bool
val is_const : t -> bool
val nfactors: t -> int
val to_nexp : t -> Exp.nexp
