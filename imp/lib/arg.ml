open Protocols

type t = Scalar of Exp.nexp | Array of Array_use.t | Unsupported

let to_string : t -> string = function
  | Unsupported -> "_"
  | Scalar e -> Exp.n_to_string e
  | Array l -> Array_use.to_string l

let array : t -> Variable.t option = function
  | Unsupported | Scalar _ -> None
  | Array a -> Some a.array

let map (f : Exp.nexp -> Exp.nexp) : t -> t = function
  | Unsupported -> Unsupported
  | Scalar e -> Scalar (f e)
  | Array a -> Array (Array_use.map f a)

let loc_subst (alias : Alias.t) : t -> t = function
  | (Unsupported | Scalar _) as i -> i
  | Array a -> Array (Array_use.loc_subst alias a)
