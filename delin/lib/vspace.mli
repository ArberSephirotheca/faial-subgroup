(* Exact integer linear algebra over parameter-pure polynomials, plus exact
   polynomial division. Used by the flag-based shape inferencer. *)

(* Max total degree of a polynomial (0 for the constant). *)
val degree : Poly.t -> int

(* A basis of the span of the given polynomials, as a fraction-free echelon set
   (primitive rows), ordered by ascending degree. *)
val column_space : Poly.t list -> Poly.t list

(* Exact polynomial division [num / den]; [None] if the remainder is nonzero. *)
val divide_exact : Poly.t -> Poly.t -> Poly.t option
