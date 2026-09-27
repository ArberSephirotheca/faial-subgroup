open Protocols
open Stage0

(* The pointer algebra as the front end can spell it, before an expression
   carrying an unknown has been given a name. Mirrors [Pointer.t] one for one,
   over [Infer_exp.t] rather than over settled expressions, so that a pointer
   whose offset the front end cannot read still reaches Imp as a pointer with
   a free variable in it rather than being declined outright. *)
type t =
  | Base of { source : Variable.t }
  | Row of { base : t; index : Infer_exp.t }
  | Shift of { base : t; offset : Infer_exp.t; step : Pointer.Step.t option }
  | Linear of { base : t; scale : int list; shift : Infer_exp.t }
  | Select of { cond : Infer_exp.t; if_true : t; if_false : t }

let from_array (source : Variable.t) : t = Base { source }
let row ~(index : Infer_exp.t) (base : t) : t = Row { base; index }

let shift ~(offset : Infer_exp.t) ~(step : Pointer.Step.t option) (base : t) : t
    =
  Shift { base; offset; step }

let select ~(cond : Infer_exp.t) ~(if_true : t) ~(if_false : t) : t =
  Select { cond; if_true; if_false }

let linear ~scale ~shift base = Linear { base; scale; shift }

let rec map (f : Infer_exp.t -> Infer_exp.t) : t -> t = function
  | Base b -> Base b
  | Row { base; index } -> Row { base = map f base; index = f index }
  | Shift { base; offset; step } ->
      Shift { base = map f base; offset = f offset; step }
  | Linear { base; scale; shift } ->
      Linear { base = map f base; scale; shift = f shift }
  | Select { cond; if_true; if_false } ->
      Select
        { cond = f cond; if_true = map f if_true; if_false = map f if_false }

open State.Syntax

let rec to_pointer : t -> Pointer.t Infer_exp.state = function
  | Base { source } -> return (Pointer.from_array source)
  | Row { base; index } ->
      let* base = to_pointer base in
      let* index = Infer_exp.to_nexp index in
      return (Pointer.row ~index base)
  | Shift { base; offset; step } ->
      let* base = to_pointer base in
      let* offset = Infer_exp.to_nexp offset in
      let offset =
        match step with
        | Some step -> Pointer.Offset.bytes ~amount:offset ~step
        | None -> Pointer.Offset.elements offset
      in
      return (Pointer.shift ~offset base)
  | Select { cond; if_true; if_false } ->
      let* cond = Infer_exp.to_bexp cond in
      let* if_true = to_pointer if_true in
      let* if_false = to_pointer if_false in
      return (Pointer.select ~cond ~if_true ~if_false)
  | Linear { base; scale; shift } ->
      let* base = to_pointer base in
      let* shift = Infer_exp.to_nexp shift in
      return (Pointer.linear ~scale ~shift base)

let rec to_string : t -> string = function
  | Base { source } -> Variable.name source
  | Row { base; index } ->
      to_string base ^ "[" ^ Infer_exp.to_string index ^ "]"
  | Shift { base; offset; step } ->
      to_string base ^ " + " ^ Infer_exp.to_string offset
      ^ (step |> Option.map Pointer.Step.comment |> Option.value ~default:"")
  | Select { cond; if_true; if_false } ->
      "(" ^ Infer_exp.to_string cond ^ " ? " ^ to_string if_true ^ " : "
      ^ to_string if_false ^ ")"
  | Linear { base; scale; shift } ->
      to_string base ^ "[linear "
      ^ String.concat "," (List.map string_of_int scale)
      ^ "; " ^ Infer_exp.to_string shift ^ "]"
