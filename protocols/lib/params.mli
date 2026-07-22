type t

val empty : t
val filter : (Variable.t -> bool) -> t -> t
val union_left : t -> t -> t
val union_right : t -> t -> t

(* Remove the variables that are members of the set *)
val remove_all : Variable.Set.t -> t -> t

(* Keep the variables that are members of the set *)
val retain_all : Variable.Set.t -> t -> t

val reset_kind : kernel_parameters:Variable.Set.t -> t -> t

val to_string : t -> string
val from_set : C_type.t -> Variable.Set.t -> t
val to_set : t -> Variable.Set.t
val add : ?bound:Exp.bexp option -> Variable.t -> C_type.t -> t -> t
val from_list : (Variable.t * C_type.t) list -> t
val to_list : t -> (Variable.t * C_type.t) list
val mem : Variable.t -> t -> bool
val find_opt : Variable.t -> t -> (Exp.bexp * C_type.t) option
val to_bexp : t -> Exp.bexp
