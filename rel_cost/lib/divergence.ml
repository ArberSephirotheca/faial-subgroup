type t =
  | Uniform
  | Divergent

let add (lhs:t) (rhs:t) : t =
  match lhs, rhs with
  | Uniform, Uniform -> Uniform
  | _, _ -> Divergent
