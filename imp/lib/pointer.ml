open Protocols
open Exp

module Step = struct
  (* Pointer steps in bytes: [view] is how far the pointer moves per unit,
     [elem] how far the memory it lands on does. One [Ty.pointee_size] result
     settles each, and they are minted together, so a step exists for both
     sides or for neither. *)
  type t = { view : int; elem : int }

  let make ~(view : int) ~(elem : int) : t = { view; elem }
  let view (x : t) : int = x.view
  let elem (x : t) : int = x.elem
  let is_scaled (x : t) : bool = x.view <> x.elem

  (* Spelled for a dump, and only where it says something: a view that
     matches the element it lands on moves by one cell, which is what an
     unqualified pointer already reads as. *)
  let comment (x : t) : string =
    if is_scaled x then
      " /* step " ^ string_of_int x.view ^ " over " ^ string_of_int x.elem
      ^ " */"
    else ""
end

module Index = struct
  type t = Exact of { value : nexp } | Span of { first : nexp; last : nexp }

  let exact (value : nexp) : t = Exact { value }

  let first : t -> nexp = function
    | Exact { value } -> value
    | Span { first; _ } -> first

  let last : t -> nexp = function
    | Exact { value } -> value
    | Span { last; _ } -> last

  let to_string : t -> string = function
    | Exact { value } -> n_to_string value
    | Span { first; last } -> n_to_string first ^ ".." ^ n_to_string last
end

module Offset = struct
  (* [Bytes] counts bytes and carries the two steps that give them meaning;
     [Elements] counts whatever unit the source supplied. The unit and the
     steps are one decision, so the two cannot come apart. *)
  type t =
    | Elements of { amount : nexp }
    | Bytes of { amount : nexp; step : Step.t }

  let elements (amount : nexp) : t = Elements { amount }
  let bytes ~(amount : nexp) ~(step : Step.t) : t = Bytes { amount; step }
  let zero : t = elements (Num 0)

  let amount : t -> nexp = function
    | Elements { amount } | Bytes { amount; _ } -> amount

  let is_zero : t -> bool = function
    | Elements { amount } -> amount = Num 0
    | Bytes { amount; step } -> amount = Num 0 && not (Step.is_scaled step)

  (* An access is a byte address and a byte extent, truncated by the step of
     the array it lands on:

       b = off + index * view,  first = b / elem,  last = (b + view - 1) / elem

     The offset divides out whenever [elem] divides it, and the index divides
     out by the step relation; the two folds are independent, and each is an
     identity rather than a shortcut. *)
  let rescale ~(off : nexp) ~(step : Step.t) (index : nexp) : Index.t =
    let view = Step.view step and elem = Step.elem step in
    let quotient (e : nexp) : nexp =
      match exact_div e elem with Some m -> m | None -> n_div e (Num elem)
    in
    let truncate (extra : int) : nexp =
      let b = n_plus (n_mult index (Num view)) (Num extra) in
      match exact_div off elem with
      | Some off -> n_plus off (quotient b)
      | None -> quotient (n_plus off b)
    in
    (* The whole access sits inside one element exactly when [view] divides
       both [elem] and [off]. A view no wider than the element does not settle
       it on its own: a [short] view at byte offset 3 of an [int] array
       straddles two of them. *)
    if elem mod view = 0 && Option.is_some (exact_div off view) then
      Index.Exact { value = truncate 0 }
    else Index.Span { first = truncate 0; last = truncate (view - 1) }

  (* Rewriting is monotone in the index, so a span is carried through by its
     two endpoints. *)
  let head (o : t) (index : Index.t) : Index.t =
    match o with
    | Elements { amount } when amount = Num 0 -> index
    | Elements { amount } -> (
        match index with
        | Index.Exact { value } -> Index.Exact { value = n_plus amount value }
        | Index.Span { first; last } ->
            Index.Span
              { first = n_plus amount first; last = n_plus amount last })
    | Bytes { amount; step } -> (
        match index with
        | Index.Exact { value } -> rescale ~off:amount ~step value
        | Index.Span { first; last } ->
            let lo = rescale ~off:amount ~step first in
            let hi = rescale ~off:amount ~step last in
            Index.Span { first = Index.first lo; last = Index.last hi })

  let apply (o : t) : Index.t list -> Index.t list = function
    | index :: rest -> head o index :: rest
    | [] -> failwith "Pointer.Offset.apply: an access has no index."

  (* The payload records the literal stored, and two writes of the same
     literal are taken not to conflict. Once the index changes units the two
     land on one cell, and storing 1 as a byte does not store the bits that
     storing 1 as an [int] does, so the payload stops holding. Only a change
     of units drops it. *)
  let keeps_payload : t -> bool = function
    | Elements _ -> true
    | Bytes { step; _ } -> not (Step.is_scaled step)

  let is_scaled : t -> bool = function
    | Elements _ -> false
    | Bytes { step; _ } -> Step.is_scaled step

  let to_string : t -> string = function
    | Elements { amount } | Bytes { amount; _ } -> n_to_string amount
end


module Address = struct
  type t = { array : Variable.t; index : Index.t list }
end

type t = Base of { array : Variable.t } | Shift of { base : t; offset : Offset.t }

let from_array (array : Variable.t) : t = Base { array }

let shift ~(offset : Offset.t) (base : t) : t =
  if Offset.is_zero offset then base else Shift { base; offset }

let rec array : t -> Variable.t = function
  | Base { array } -> array
  | Shift { base; _ } -> array base

let arrays (p : t) : Variable.Set.t = Variable.Set.singleton (array p)

let rec keeps_payload : t -> bool = function
  | Base _ -> true
  | Shift { base; offset } -> Offset.keeps_payload offset && keeps_payload base

let to_array : t -> Variable.t option = function
  | Base { array } -> Some array
  | Shift _ -> None

let addresses ~(index : nexp list) (p : t) : Address.t list =
  let rec walk (p : t) (index : Index.t list) : Address.t list =
    match p with
    | Base { array } -> [ { Address.array; index } ]
    | Shift { base; offset } -> walk base (Offset.apply offset index)
  in
  walk p (List.map Index.exact index)

(* The root name plus the shift amounts, dropping the steps. A call argument
   is written in the caller's units and rescaled where the callee's parameter
   type settles them, so the steps do not travel with it. *)
let to_nexp (p : t) : nexp =
  let rec amount : t -> nexp = function
    | Base _ -> Num 0
    | Shift { base; offset } -> n_plus (Offset.amount offset) (amount base)
  in
  n_plus (Var (array p)) (amount p)

let rec to_string : t -> string = function
  | Base { array } -> Variable.name array
  | Shift { base; offset } -> to_string base ^ " + " ^ Offset.to_string offset

let rec step_comment : t -> string = function
  | Base _ -> ""
  | Shift { base; offset } -> (
      match offset with
      | Offset.Bytes { step; _ } when Step.is_scaled step -> Step.comment step
      | Offset.Bytes _ | Offset.Elements _ -> step_comment base)
