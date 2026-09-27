module Type = Kernel.Parameter.Type
open Protocols

(* The parameter is the contract, so the front end's classification of it
   decides how an argument binds. The argument expression is inspected only
   to recover which array a pointer parameter refers to. *)
let classify ~(arrays : Variable.Set.t) (ty : Type.t) (e : Exp.nexp) : Arg.t =
  match ty with
  | Type.Array _ -> (
      match Array_use.from_nexp ~arrays e with
      | Some u -> Arg.Array u
      | None -> Arg.Unsupported (Type.to_c_type ty))
  | Type.Scalar _ | Type.Enum _ -> Arg.Scalar e
  | Type.Unsupported ty ->
      if Option.is_some (Ty.vector_lanes ty) then
        (* A vector argument binds lane by lane, which the inliner does
           from the parameter's type, so the variable is carried through. *)
        match e with Exp.Var _ -> Arg.Scalar e | _ -> Arg.Unsupported ty
      else Arg.Unsupported ty
