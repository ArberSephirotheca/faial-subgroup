open Protocols
module S = Subst.Make (Subst.SubstPair)

type t = {
  result : (Variable.t * Ty.t) option;
  id : Function_id.t;
  args : Exp.nexp list;
}

let unique_id (c : t) : Function_id.t = c.id
let map (f : Exp.nexp -> Exp.nexp) (a : t) : t = { a with args = List.map f a.args }

(* Returns the arrays in arguments *)
let arrays (c : t) : Variable.t list = List.filter_map Array_use.base c.args

(* An argument that names the pointer is rewritten to name the memory it
   stands for. The steps do not travel: the argument counts the caller's
   units and the inline site rescales it against the callee's parameter. *)
let resolve ~(target : Variable.t) (pointer : Pointer.t) (c : t) : t =
  match Pointer.to_nexp pointer with
  | Some e -> map (S.n_subst (target, e)) c
  | None -> c

let to_string (c : t) : string =
  let args = c.args |> List.map Exp.n_to_string |> String.concat ", " in
  let pre =
    match c.result with
    | Some (v, ty) -> Variable.name v ^ " : " ^ Ty.to_string ty ^ " = "
    | None -> ""
  in
  pre ^ Function_id.label c.id ^ "(" ^ args ^ ")"
