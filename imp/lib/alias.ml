open Protocols
open Exp

(* [view] and [elem] are pointer steps in bytes, and they decide the unit of
   [offset]: bytes when both are known, and whatever the source supplied when
   either is absent. One [Ty.pointee_size] result settles both at the mint
   site, so the two cannot come apart. *)
type t = {
  source : Variable.t;
  target : Variable.t;
  offset : nexp;
  view : int option;
  elem : int option;
}

type span = Single of nexp | Span of { first : nexp; last : nexp }

let is_scaled (a : t) : bool =
  match (a.view, a.elem) with Some v, Some e -> v <> e | _ -> false

let is_trivial (a : t) : bool =
  Variable.equal a.source a.target && a.offset = Num 0 && not (is_scaled a)

(* An access is a byte address and a byte extent, truncated by the step of the
   array it lands on:

     b = off + index * view,  first = b / elem,  last = (b + view - 1) / elem

   The offset divides out whenever [elem] divides it, and the index divides out
   by the step relation; the two folds are independent, and each is an identity
   rather than a shortcut. *)
let elements ~(off : nexp) ~(view : int) ~(elem : int) (index : nexp) : span =
  let quotient (e : nexp) : nexp =
    match exact_div e elem with Some m -> m | None -> n_div e (Num elem)
  in
  let truncate (extra : int) : nexp =
    let b = n_plus (n_mult index (Num view)) (Num extra) in
    match exact_div off elem with
    | Some off -> n_plus off (quotient b)
    | None -> quotient (n_plus off b)
  in
  (* The whole access sits inside one element exactly when [view] divides both
     [elem] and [off]. A view no wider than the element does not settle it on
     its own: a [short] view at byte offset 3 of an [int] array straddles two
     of them. *)
  if elem mod view = 0 && Option.is_some (exact_div off view) then
    Single (truncate 0)
  else Span { first = truncate 0; last = truncate (view - 1) }

let to_string (l : t) : string =
  let step : int option -> string = function
    | Some n -> string_of_int n
    | None -> "?"
  in
  let steps =
    if is_scaled l then " /* step " ^ step l.view ^ " over " ^ step l.elem ^ " */"
    else ""
  in
  Variable.name l.target ^ " = " ^ Variable.name l.source ^ " + "
  ^ n_to_string l.offset ^ ";" ^ steps
