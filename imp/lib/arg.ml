open Protocols

(* [Unsupported] retains the source [C_type.t] of the argument
   expression so the IR keeps the declared type even when the
   C-to-Imp lifting has no specialised handling for it. *)
type t =
  | Scalar of Exp.nexp
  | Array of Array_use.t
  | Unsupported of C_type.t

let to_string : t -> string = function
  | Unsupported ty -> "_:" ^ C_type.to_string ty
  | Scalar e -> Exp.n_to_string e
  | Array l -> Array_use.to_string l

let array : t -> Variable.t option = function
  | Unsupported _ | Scalar _ -> None
  | Array a -> Some a.array

let map (f : Exp.nexp -> Exp.nexp) : t -> t = function
  | Unsupported ty -> Unsupported ty
  | Scalar e -> Scalar (f e)
  | Array a -> Array (Array_use.map f a)

let loc_subst (alias : Alias.t) : t -> t = function
  | (Unsupported _ | Scalar _) as i -> i
  | Array a -> Array (Array_use.loc_subst alias a)
