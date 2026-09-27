open Protocols

(* [Unsupported] retains the source [Ty.t] of the argument
   expression so the IR keeps the declared type even when the
   C-to-Imp lifting has no specialised handling for it. *)
type t =
  | Scalar of Exp.nexp
  | Array of Array_use.t
  | Unsupported of Ty.t

let to_string : t -> string = function
  | Unsupported ty -> "_:" ^ Ty.to_string ty
  | Scalar e -> Exp.n_to_string e
  | Array l -> Array_use.to_string l

let array : t -> Variable.t option = function
  | Unsupported _ | Scalar _ -> None
  | Array a -> Some a.array
