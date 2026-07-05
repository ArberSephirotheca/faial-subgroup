open Protocols

type t =
  | Induction of Exp.nexp
  | Parameter of Exp.nexp

let induction s = Induction (Exp.Var (Variable.from_name s))
let parameter s = Parameter (Exp.Var (Variable.from_name s))

let to_nexp : t -> Exp.nexp = function
  | Induction n | Parameter n -> n

let is_induction = function Induction _ -> true | Parameter _ -> false
let is_parameter = function Parameter _ -> true | Induction _ -> false

let as_induction_var = function
  | Induction (Exp.Var v) -> Some v
  | _ -> None

let compare x y = match Exp.n_compare (to_nexp x) (to_nexp y) with
  | 0 -> compare (is_parameter x) (is_parameter y)
  | n -> n

let to_string = function
  | Parameter n -> Exp.n_to_string n ^ " (global)"
  | Induction n -> Exp.n_to_string n

let from_nexp ~globals (value : Exp.nexp) : t =
  let thread_global =
    let free = Exp.n_free_names value Variable.Set.empty in
    Variable.Set.diff free globals |> Variable.Set.is_empty
  in
  if thread_global then Parameter value else Induction value


module OT = struct
  type nonrec t = t
  let compare = compare
end

module Map = Map.Make (OT)
module Set = Set.Make (OT)
