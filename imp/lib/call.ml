open Protocols

type t = {
  result : (Variable.t * C_type.t) option;
  kernel : string;
  ty : string;
  args : Arg.t list;
}

let kernel_id ~kernel ~ty : string = kernel ^ ":" ^ ty
let unique_id (c : t) : string = kernel_id ~kernel:c.kernel ~ty:c.ty

(* Returns the arrays in arguments *)
let arrays (c : t) : Variable.t list = List.filter_map Arg.array c.args

let map (f : Exp.nexp -> Exp.nexp) (a : t) : t =
  { a with args = List.map (Arg.map f) a.args }

let loc_subst (alias : Alias.t) (a : t) : t =
  { a with args = List.map (Arg.loc_subst alias) a.args }

let to_string (c : t) : string =
  let args = c.args |> List.map Arg.to_string |> String.concat ", " in
  let pre =
    match c.result with
    | Some (v, ty) -> Variable.name v ^ " : " ^ C_type.to_string ty ^ " = "
    | None -> ""
  in
  pre ^ c.kernel ^ "(" ^ args ^ ")"
