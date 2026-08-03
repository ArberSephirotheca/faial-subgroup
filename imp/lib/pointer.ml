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

  let map (f : nexp -> nexp) : t -> t = function
    | Elements { amount } -> Elements { amount = f amount }
    | Bytes { amount; step } -> Bytes { amount = f amount; step }

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
  type t = {
    array : Variable.t;
    index : Index.t list;
    guard : Exp.bexp option;
  }
end

let split_index ~(dims : int option list) (index : nexp list) : nexp list option
    =
  let arity = List.length index in
  let rank = List.length dims in
  if arity = 0 || rank <= arity then None
  else
    let suffix = List.filteri (fun i _ -> i >= arity - 1) dims in
    match suffix with
    (* The head of the suffix is the axis the flat index is already known to
       sit inside, so its extent is never read; every extent below it is. *)
    | _ :: below when below <> [] && List.for_all Option.is_some below ->
        let below = List.filter_map Fun.id below in
        let prefix, flat =
          let rec split_last : nexp list -> nexp list * nexp = function
            | [ x ] -> ([], x)
            | x :: l ->
                let prefix, last = split_last l in
                (x :: prefix, last)
            | [] -> ([], Num 0)
          in
          split_last index
        in
        let scale (below : int list) (e : nexp) : nexp =
          match List.fold_left ( * ) 1 below with
          | 1 -> e
          | d -> n_div e (Num d)
        in
        let rec axes : int list -> nexp list = function
          | [] -> []
          | n :: rest -> n_umod (scale rest flat) (Num n) :: axes rest
        in
        Some ((prefix @ [ scale below flat ]) @ axes below)
    | _ -> None

(* A [Row] fixes the leading index of the memory it names, which is how a
   pointer read out of a table of pointers says which row it is. It is not a
   [Shift]: a shift moves within one row and carries the units of that move,
   where a row selects among rows and has no units of its own. *)
type t =
  | Base of { array : Variable.t }
  | Row of { base : t; index : nexp }
  | Shift of { base : t; offset : Offset.t }
  | Select of { cond : Exp.bexp; if_true : t; if_false : t }

let from_array (array : Variable.t) : t = Base { array }
let row ~(index : nexp) (base : t) : t = Row { base; index }

let shift ~(offset : Offset.t) (base : t) : t =
  if Offset.is_zero offset then base else Shift { base; offset }

let select ~(cond : Exp.bexp) ~(if_true : t) ~(if_false : t) : t =
  Select { cond; if_true; if_false }

let rec arrays : t -> Variable.Set.t = function
  | Base { array } -> Variable.Set.singleton array
  | Row { base; _ } | Shift { base; _ } -> arrays base
  | Select { if_true; if_false; _ } ->
      Variable.Set.union (arrays if_true) (arrays if_false)

let rec keeps_payload : t -> bool = function
  | Base _ -> true
  | Row { base; _ } -> keeps_payload base
  | Shift { base; offset } -> Offset.keeps_payload offset && keeps_payload base
  | Select { if_true; if_false; _ } ->
      keeps_payload if_true && keeps_payload if_false

let to_array : t -> Variable.t option = function
  | Base { array } -> Some array
  | Row _ | Shift _ | Select _ -> None

let rec map ~(n : nexp -> nexp) ~(b : Exp.bexp -> Exp.bexp) : t -> t = function
  | Base _ as p -> p
  | Row { base; index } -> Row { base = map ~n ~b base; index = n index }
  | Shift { base; offset } ->
      Shift { base = map ~n ~b base; offset = Offset.map n offset }
  | Select { cond; if_true; if_false } ->
      Select
        {
          cond = b cond;
          if_true = map ~n ~b if_true;
          if_false = map ~n ~b if_false;
        }

(* The variables the pointer's own expressions read. These are what a
   binder below it must not capture, and they exclude the names of the
   arrays it reaches, which are memory rather than values. *)
let rec free_names (p : t) (acc : Variable.Set.t) : Variable.Set.t =
  match p with
  | Base _ -> acc
  | Row { base; index } -> free_names base (Exp.n_free_names index acc)
  | Shift { base; offset } ->
      free_names base (Exp.n_free_names (Offset.amount offset) acc)
  | Select { cond; if_true; if_false } ->
      Exp.b_free_names cond acc |> free_names if_true |> free_names if_false

(* Compose one pointer onto another by grafting: wherever [p] bottoms out
   at [target], continue into [source]. This is what a pointer taken from
   another pointer means, and it is how the binding of the inner one is
   discharged against the outer. *)
let rec subst_bases (f : Variable.t -> t option) (p : t) : t =
  match p with
  | Base { array } -> f array |> Option.value ~default:p
  | Row { base; index } -> Row { base = subst_bases f base; index }
  | Shift { base; offset } -> Shift { base = subst_bases f base; offset }
  | Select { cond; if_true; if_false } ->
      Select
        {
          cond;
          if_true = subst_bases f if_true;
          if_false = subst_bases f if_false;
        }

let subst_base ~(target : Variable.t) ~(source : t) : t -> t =
  subst_bases (fun x -> if Variable.equal x target then Some source else None)

let addresses ~(index : nexp list) (p : t) : Address.t list =
  let guarded (cond : Exp.bexp) : Exp.bexp option -> Exp.bexp option = function
    | Some guard -> Some (b_and guard cond)
    | None -> Some cond
  in
  let rec walk (p : t) (index : Index.t list) (guard : Exp.bexp option) :
      Address.t list =
    match p with
    | Base { array } -> [ { Address.array; index; guard } ]
    | Row { base; index = i } -> walk base (Index.exact i :: index) guard
    | Shift { base; offset } -> walk base (Offset.apply offset index) guard
    | Select { cond; if_true; if_false } ->
        walk if_true index (guarded cond guard)
        @ walk if_false index (guarded (b_not cond) guard)
  in
  walk p (List.map Index.exact index) None

(* The root name plus the shift amounts, dropping the steps. A call argument
   is written in the caller's units and rescaled where the callee's parameter
   type settles them, so the steps do not travel with it. A row or a choice
   has no such spelling, which is exactly when an argument has to carry the
   pointer itself. *)
let to_nexp (p : t) : nexp option =
  let rec walk : t -> nexp option = function
    | Base { array } -> Some (Var array)
    | Shift { base; offset } ->
        walk base |> Option.map (n_plus (Offset.amount offset))
    | Row _ | Select _ -> None
  in
  walk p

let rec to_string : t -> string = function
  | Base { array } -> Variable.name array
  | Row { base; index } -> to_string base ^ "[" ^ n_to_string index ^ "]"
  | Shift { base; offset } -> to_string base ^ " + " ^ Offset.to_string offset
  | Select { cond; if_true; if_false } ->
      "(" ^ b_to_string cond ^ " ? " ^ to_string if_true ^ " : "
      ^ to_string if_false ^ ")"

let rec step_comment : t -> string = function
  | Base _ -> ""
  | Row { base; _ } -> step_comment base
  | Select { if_true; _ } -> step_comment if_true
  | Shift { base; offset } -> (
      match offset with
      | Offset.Bytes { step; _ } when Step.is_scaled step -> Step.comment step
      | Offset.Bytes _ | Offset.Elements _ -> step_comment base)
