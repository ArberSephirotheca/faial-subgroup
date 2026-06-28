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
}

type t = Plain of Range.t | Index of index

let var : t -> Variable.t = function
  | Plain r -> Range.var r
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
let index_to_cond (r : index) : bexp =
  let i = Var r.index in
  let value = to_value r in
  let value_bound =
    match r.dir with
    | Range.Increase -> n_le value r.upper_bound
    | Range.Decrease -> n_le r.lower_bound value
  in
  b_and_ex [ n_le (Num 0) i; value_bound; Range.decl_to_bexp r.index r.ty ]

let to_cond : t -> bexp = function
  | Plain r -> Range.to_cond r
  | Index r -> index_to_cond r

(* The range's own free names (bounds and stride), excluding the bound
   index, mirroring [Range.free_names]. The source variable is gone
   from the body after substitution, so it is not free here. *)
let free_names (r : t) (fns : Variable.Set.t) : Variable.Set.t =
  match r with
  | Plain r -> Range.free_names r fns
  | Index r -> (
      let fns = n_free_names r.lower_bound fns in
      let fns = n_free_names r.upper_bound fns in
      match r.recovery with Additive k -> n_free_names k fns)

let map (f : nexp -> nexp) (r : t) : t =
  match r with
  | Plain r -> Plain (Range.map f r)
  | Index r ->
      Index
        {
          r with
          lower_bound = f r.lower_bound;
          upper_bound = f r.upper_bound;
          recovery = (match r.recovery with Additive k -> Additive (f k));
        }

(* Reparametrize a strided additive loop over a fresh unit-stride
   index. Returns [Plain] unchanged for anything outside the
   realizable domain (unit, symbolic, or geometric steps). *)
let normalize (r : Range.t) : t =
  match Range.plus_step_literal (Range.step r) with
  | None -> Plain r
  | Some k ->
      let source = Range.var r in
      let index = Variable.from_name (Variable.name source ^ "$q") in
      Index
        {
          source;
          index;
          ty = Range.ty r;
          dir = Range.dir r;
          lower_bound = Range.lower_bound r;
          upper_bound = Range.upper_bound r;
          recovery = Additive (Num k);
        }

let to_string : t -> string = function
  | Plain r -> Range.to_string r
  | Index r ->
      Variable.name r.index ^ " for " ^ Variable.name r.source ^ " = "
      ^ n_to_string (to_value r)
