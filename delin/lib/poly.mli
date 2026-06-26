open Protocols

type t
val to_string : t -> string
val parameter : string -> t
val induction : string -> t
val of_int : int -> t
val of_atom : Atom.t -> t
val zero : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t

val div_mod : t -> Term.t -> t * t

val fold : (Term.t -> 'a -> 'a) -> 'a -> t -> 'a
val to_list : t -> Term.t list
val of_list : Term.t list -> t
val compare : t -> t -> int
val from_nexp : globals:Variable.Set.t -> Exp.nexp -> t
val to_nexp : t -> Exp.nexp
