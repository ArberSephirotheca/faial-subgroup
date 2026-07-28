open Protocols
module S = Subst.Make (Subst.SubstPair)

type t = {
  result : (Variable.t * Ty.t) option;
  kernel : string;
  ty : string;
  args : Exp.nexp list;
}

let kernel_id ~kernel ~ty : string = kernel ^ ":" ^ ty
let unique_id (c : t) : string = kernel_id ~kernel:c.kernel ~ty:c.ty
let map (f : Exp.nexp -> Exp.nexp) (a : t) : t = { a with args = List.map f a.args }

(* Returns the arrays in arguments *)
let arrays (c : t) : Variable.t list = List.filter_map Array_use.base c.args

let loc_subst (alias : Alias.t) (c : t) : t =
  let e = Exp.n_plus (Exp.Var alias.source) alias.offset in
  map (S.n_subst (alias.target, e)) c

let to_string (c : t) : string =
  let args = c.args |> List.map Exp.n_to_string |> String.concat ", " in
  let pre =
    match c.result with
    | Some (v, ty) -> Variable.name v ^ " : " ^ Ty.to_string ty ^ " = "
    | None -> ""
  in
  pre ^ c.kernel ^ "(" ^ args ^ ")"
