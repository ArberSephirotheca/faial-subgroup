(* Reparametrizes a strided additive loop over a fresh unit-stride index
   so its iteration bound is linear ([k*i <= ub - lb]) rather than a
   divisibility ([mod]) or quotient ([div]) constraint. Those take z3 out
   of the linear integer arithmetic it decides stably, so this keeps the
   race query both exact and reproducible. *)

open Protocols
open Exp

(* A normalized loop is the dual of a value range: instead of the
   bound variable being the iteration value, it is a fresh unit-stride
   index, and the value is rebuilt from the index by a recovery map.

   The recovery has one form per step kind, dual to the step itself.
   Only the additive form is realizable here: the geometric recovery
   [lb * b^i] needs a variable-exponent power, which the expression
   language has no constructor for. The variant leaves room for that
   arm once a power term exists. *)
type recovery = Additive of nexp (* stride k; value = first +/- k * index *)

type index = {
  source : Variable.t; (* original value variable, eliminated by substitution *)
  index : Variable.t; (* fresh unit-stride index *)
  ty : C_type.t;
  dir : Range.direction;
  lower_bound : nexp;
  upper_bound : nexp;
  recovery : recovery;
  cond : bexp; (* the restriction, reparametrized over [index] *)
}

type t = Plain of Cond_range.t | Index of index

let var : t -> Variable.t = function
  | Plain cr -> Cond_range.var cr
  | Index r -> r.index

(* The value denoted by index [i]: [lb + k*i] when increasing,
   [ub - k*i] when decreasing. This is the right-hand side that
   replaces the original value variable in the loop body. *)
let to_value (r : index) : nexp =
  let i = Var r.index in
  match r.recovery with
  | Additive k -> (
      let accum = n_mult k i in
      match r.dir with
      | Range.Increase -> n_plus r.lower_bound accum
      | Range.Decrease -> n_minus r.upper_bound accum)

let substitution (r : index) : Variable.t * nexp = (r.source, to_value r)

(* [0 <= i] and the recovered value stays within [[lb, ub]]. With a
   literal stride the value bound is linear, so no div/mod is emitted. *)
let index_to_bexp (r : index) : bexp =
  let i = Var r.index in
  let value = to_value r in
  let value_bound =
    match r.dir with
    | Range.Increase -> n_le value r.upper_bound
    | Range.Decrease -> n_le r.lower_bound value
  in
  b_and_ex [ n_le (Num 0) i; value_bound; Range.decl_to_bexp r.index r.ty ]

let to_bexp : t -> bexp = function
  | Plain cr -> Cond_range.to_bexp cr
  | Index r -> b_and (index_to_bexp r) r.cond

(* The range's own free names (bounds, stride, and restriction), excluding
   the bound index. The source variable is gone from the body after
   substitution, so it is not free here. *)
let free_names (r : t) (fns : Variable.Set.t) : Variable.Set.t =
  match r with
  | Plain cr -> Cond_range.free_names cr fns
  | Index r ->
      let fns = n_free_names r.lower_bound fns in
      let fns = n_free_names r.upper_bound fns in
      let fns =
        match r.recovery with Additive k -> n_free_names k fns
      in
      let cond_fns =
        b_free_names r.cond Variable.Set.empty |> Variable.Set.remove r.index
      in
      Variable.Set.union fns cond_fns

let map (f : nexp -> nexp) (r : t) : t =
  match r with
  | Plain cr -> Plain (Cond_range.map f cr)
  | Index r ->
      Index
        {
          r with
          lower_bound = f r.lower_bound;
          upper_bound = f r.upper_bound;
          recovery = (match r.recovery with Additive k -> Additive (f k));
          cond = b_map f r.cond;
        }

(* Reparametrize a strided additive loop over a fresh unit-stride
   index. Returns [Plain] unchanged for anything outside the
   realizable domain (unit, symbolic, or geometric steps). The
   restriction is carried over, reparametrized onto the fresh index. *)
let normalize (cr : Cond_range.t) : t =
  let r = cr.range in
  match Range.plus_step_literal (Range.step r) with
  | None -> Plain cr
  | Some k ->
      let source = Range.var r in
      let index = Variable.from_name (Variable.name source ^ "$q") in
      let ix =
        {
          source;
          index;
          ty = Range.ty r;
          dir = Range.dir r;
          lower_bound = Range.lower_bound r;
          upper_bound = Range.upper_bound r;
          recovery = Additive (Num k);
          cond = cr.cond;
        }
      in
      Index { ix with cond = Subst.ReplacePair.b_subst (substitution ix) ix.cond }

let to_string : t -> string = function
  | Plain cr -> Cond_range.to_string cr
  | Index r ->
      let base =
        Variable.name r.index ^ " for " ^ Variable.name r.source ^ " = "
        ^ n_to_string (to_value r)
      in
      (match r.cond with
      | Bool true -> base
      | _ -> base ^ " if " ^ b_to_string r.cond)
