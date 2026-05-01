(* The three-way classification of variables in the NBD typing
   judgment (see documentation/named-barriers-typing.md).

   - [Unif]:  same value across iterations and across the two
              executions T1, T2.
   - [Iter]:  varies across iterations of an enclosing loop, same
              across T1 and T2 at a fixed iteration.
   - [Local]: varies across iterations and across T1, T2. *)

type t = Unif | Iter | Local

let to_string : t -> string = function
  | Unif -> "unif"
  | Iter -> "iter"
  | Local -> "local"

let equal (a : t) (b : t) : bool = a = b
