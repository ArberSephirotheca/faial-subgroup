open Protocols

type t = { array : Variable.t; offset : Exp.nexp }

let make ?(offset = Exp.Num 0) (array : Variable.t) : t = { array; offset }

(* Recover the array an expression addresses, with whatever is added to it as
   the offset. The caller's array map settles which operand of a [+] is the
   base; when neither names known memory, keep the left, which is where a
   pointer conventionally sits. *)
let rec from_nexp ?(arrays = Variable.Set.empty) (e : Exp.nexp) : t option =
  let known (x : Variable.t) : bool = Variable.Set.mem x arrays in
  match e with
  | Var x -> Some (make x)
  | Binary (Plus _, Var x, offset) when known x -> Some { array = x; offset }
  | Binary (Plus _, offset, Var x) when known x -> Some { array = x; offset }
  | Binary (Plus _, l, r) ->
      from_nexp ~arrays l
      |> Option.map (fun a -> { a with offset = Exp.n_plus a.offset r })
  | _ -> None

let base (e : Exp.nexp) : Variable.t option =
  from_nexp e |> Option.map (fun a -> a.array)

let to_string (l : t) : string =
  "&" ^ Variable.name l.array ^ "[" ^ Exp.n_to_string l.offset ^ "]"
