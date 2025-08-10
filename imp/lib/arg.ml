open Protocols

type t = Scalar of Exp.nexp | Array of Array_use.t | Unsupported

let to_string : t -> string = function
  | Unsupported -> "_"
  | Scalar e -> Exp.n_to_string e
  | Array l -> Array_use.to_string l
