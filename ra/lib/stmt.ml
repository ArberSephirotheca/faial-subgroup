open Stage0
open Protocols

type t =
  | Skip
  | Clamp of { value : Exp.nexp; upper_bound : int }
  | Tick of int
  | Loop of { range : Range.t; body : t }
  | Seq of t * t
  | If of Exp.bexp * t * t
  | Choice of t * t

let rec is_zero : t -> bool = function
  | Clamp { value = Num 0; upper_bound = _ } | Skip | Tick 0 -> true
  | Clamp _ | Tick _ -> false
  | Loop { body = r; _ } -> is_zero r
  | Seq (p, q) | Choice (p, q) -> is_zero p && is_zero q
  | If (_, p, q) -> is_zero p && is_zero q

(* We try to generate unique names for cost to not confuse backends like Absynth *)
let cost_id : int ref = ref 0

let tick (e : Exp.nexp) : t =
  match e with
  | Num n -> Tick n
  | _ ->
      let range =
        (* Make sure we generate a fresh loop variable *)
        let fns = Exp.n_free_names e Variable.Set.empty in
        let c_id = !cost_id in
        cost_id := c_id + 1;
        let c_id = "__cost" ^ string_of_int c_id in
        let x = Variable.fresh fns (Variable.from_name c_id) in
        Range.make ~lower_bound:(Num 1) x e
      in
      Loop { range; body = Tick 1 }

let to_environ (s : t) : Environ.t =
  let rec fvs (s : t) (env : Environ.Fvs.t) : Environ.Fvs.t =
    match s with
    | Skip | Tick _ -> env
    | Clamp { value; _ } -> Environ.Fvs.add_n value env
    | Loop { range = r; body = p } -> env |> fvs p |> Environ.Fvs.add_r r
    | Seq (p, q) | Choice (p, q) -> env |> fvs p |> fvs q
    | If (b, p, q) -> env |> fvs p |> fvs q |> Environ.Fvs.add_b b
  in
  Environ.Fvs.empty |> fvs s |> Environ.from_fvs

module MakeSubst (S : Subst.SUBST) = struct
  module M = Subst.Make (S)

  let rec subst (s : S.t) : t -> t = function
    | Skip -> Skip
    | Clamp { value; upper_bound } ->
        Clamp { value = M.n_subst s value; upper_bound }
    | Tick k -> Tick k
    | Seq (p, q) -> Seq (subst s p, subst s q)
    | Choice (p, q) -> Choice (subst s p, subst s q)
    | If (b, p, q) -> If (M.b_subst s b, subst s p, subst s q)
    | Loop { range = r; body = p } ->
        let r = M.r_subst s r in
        M.add s r.var (function
          | Some s -> Loop { range = r; body = subst s p }
          | None -> Loop { range = r; body = p })
end

module PSubstAssoc = MakeSubst (Subst.SubstAssoc)
module PSubstPair = MakeSubst (Subst.SubstPair)

let subst : Variable.t * Exp.nexp -> t -> t = PSubstPair.subst

let rec indent : t -> Indent.t list =
  let open Indent in
  function
  | Tick k -> [ Line ("tick(" ^ string_of_int k ^ ");") ]
  | Clamp { value; upper_bound } ->
      [
        Line
          ("clamp_tick(" ^ Exp.n_to_string value ^ ", "
         ^ string_of_int upper_bound ^ ")");
      ]
  | Skip -> [ Line "skip;" ]
  | Seq (p, q) -> indent p @ indent q
  | Choice (p, q) ->
      [
        Line "if <> {";
        Block (indent p);
        Line "} else {";
        Block (indent q);
        Line "}";
      ]
  | If (b, p, q) ->
      [
        Line ("if (" ^ Exp.b_to_string b ^ ") {");
        Block (indent p);
        Line "} else {";
        Block (indent q);
        Line "}";
      ]
  | Loop { range = r; body = p } ->
      [
        Line ("foreach (" ^ Range.to_string r ^ ") {");
        Block (indent p);
        Line "}";
      ]

module Opt = struct
  let skip : t = Skip
  let tick n : t = if n = 0 then Skip else Tick n

  let if_ (b : Exp.bexp) (p : t) (q : t) : t =
    match (b, p, q) with
    | Bool b, _, _ -> if b then p else q
    | _, Skip, Skip -> Skip
    | _, Skip, _ -> If (Exp.b_not b, p, Skip)
    | _, _, _ -> If (b, p, q)

  let seq (p : t) (q : t) : t =
    match (p, q) with Skip, p | p, Skip -> p | _, _ -> Seq (p, q)

  let loop (r : Range.t) (p : t) : t =
    if p = Skip || Range.eval_is_empty r then Skip
    else Loop { range = r; body = p }

  let choice (p : t) (q : t) : t = Choice (p, q)
end

(* Performs a tick, up to a certain upper bound.
   Negative values are ignored. *)
let clamp_to_tick ~value ~upper_bound : t =
  Opt.if_ (Exp.n_lt value (Num upper_bound)) (tick value) (Tick upper_bound)

let rec simplify : t -> t = function
  | Skip -> Skip
  | Tick n -> Opt.tick n
  | If (b, p, q) -> Opt.if_ (Constfold.b_opt b) (simplify p) (simplify q)
  | Choice (p, q) -> Opt.choice (simplify p) (simplify q)
  | Loop { range = r; body = p } -> Opt.loop (Constfold.r_opt r) (simplify p)
  | Seq (p, q) -> Opt.seq (simplify p) (simplify q)
  | Clamp { value; upper_bound } ->
      if upper_bound <= 0 then Skip
      else Clamp { value = Constfold.n_opt value; upper_bound }

let to_string (x : t) : string = indent x |> Indent.to_string
let print (x : t) : unit = indent x |> Indent.print
