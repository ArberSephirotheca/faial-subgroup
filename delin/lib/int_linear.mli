module Vector : sig
  type t

  val of_list : int list -> t
  val length : t -> int
  val get : t -> int -> int
  val nth_opt : t -> int -> int option
  val select : t -> int list -> t
  val equal : t -> t -> bool
  val to_string : t -> string
end

module Matrix : sig
  type t

  val of_rows : Vector.t list -> t
  val rows : t -> int
  val cols : t -> int
  val det : t -> int
  val select_rows : t -> int list -> t
  val cramer_solve : t -> Vector.t -> Vector.t option
  val solves : t -> x:Vector.t -> b:Vector.t -> bool
  val to_string : t -> string
end

val int_solve : Matrix.t -> Vector.t -> Vector.t option
