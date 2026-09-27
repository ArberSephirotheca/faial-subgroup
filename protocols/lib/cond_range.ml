open Exp

(* A range restricted by an extra predicate: the set-builder
   [{ x | x in lo..hi /\ cond }]. It pairs a [Range.t] iteration space
   with a boolean [cond] so the two travel together through the pipeline. *)
type t = { range : Range.t; cond : bexp }

let make (range : Range.t) (cond : bexp) : t = { range; cond }
let of_range (range : Range.t) : t = { range; cond = Bool true }
let var (r : t) : Variable.t = Range.var r.range

(* The predicate that defines the restricted iteration set. *)
let to_bexp (r : t) : bexp = b_and (Range.to_bexp r.range) r.cond

(* The bound variable [var r] is local to the range, so it is not free. *)
let free_names (r : t) (fns : Variable.Set.t) : Variable.Set.t =
  Range.free_names r.range fns
  |> b_free_names r.cond
  |> Variable.Set.remove (var r)

let map (f : nexp -> nexp) (r : t) : t =
  { range = Range.map f r.range; cond = b_map f r.cond }

let to_string (r : t) : string =
  match r.cond with
  | Bool true -> Range.to_string r.range
  | _ -> Range.to_string r.range ^ " if " ^ b_to_string r.cond

module Make (S : Subst.SUBST) = struct
  module M = Subst.Make (S)

  let subst (s : S.t) (r : t) : t =
    { range = M.r_subst s r.range; cond = M.b_subst s r.cond }
end
