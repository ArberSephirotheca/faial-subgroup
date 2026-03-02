open Protocols

type t = { array : Variable.t; offset : Exp.nexp }

let make ?(offset = Exp.Num 0) (array : Variable.t) : t = { array; offset }
let map (f : Exp.nexp -> Exp.nexp) (a : t) : t = { a with offset = f a.offset }

let loc_subst (alias : Alias.t) (a : t) : t =
  if Variable.equal a.array alias.target then
    (* Update the name of the resolved array,
    but keep the original location *)
    (* use the inlined variable but with the location of the alias,
    so that the error message appears in the right place. *)
    let array = { alias.source with location = a.array.location } in
    { array; offset = Exp.n_plus alias.offset a.offset }
  else a

let to_string (l : t) : string =
  "&" ^ Variable.name l.array ^ "[" ^ Exp.n_to_string l.offset ^ "]"
