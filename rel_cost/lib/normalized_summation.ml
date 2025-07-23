(*
open Protocols

type t =
  | Const of Reals.t
  | Sum of (Variable.t * Reals.t * t)
  | Plus of t * t

let zero : t = Const (Reals.from_int 0)

let normalize_range (r:Set_range.t) : (Reals.t * Reals.t) option=
  if Reals.is_one r.lower_bound then
    None
  else
    Some (
      Reals.plus (Reals.dec (Var r.var)) r.lower_bound,
      Reals.inc (Reals.minus r.upper_bound r.lower_bound)
    )

let subst ((x:Variable.t), (v:Reals.t)) : t -> t =
  let rec subst : t -> t =
    function
    | Const e -> Const (Reals.i_subst (x, v) e)
    | Plus (lhs, rhs) -> Plus (subst lhs, subst rhs)
    | Sum (y, ub, e) ->
      let ub = Reals.i_subst (x, v) ub in
      if Variable.equal x y then
        Sum (y, ub, e)
      else
        Sum (y, ub, subst e)
  in subst

let rec from_summation : Summation.t -> t =
  function
  | Const n -> Const (Reals.from_int n)
  | If (_, _, _) -> failwith "if"
  | Plus (lhs, rhs) -> Plus (from_summation lhs, from_summation rhs)
  | Sum (r, s) ->
    let s = from_summation s in
    normalize_range r
    |> Option.map (fun (new_var, new_ub) ->
      Sum (r.var, new_ub, subst (r.var, new_var) s)
    )
    |> Option.value ~default:(Sum (r.var, r.upper_bound, s))

let plus (lhs:t) (rhs:t) : t =
  match lhs, rhs with
  | Const lhs, Const rhs -> Const (Reals.plus lhs rhs)
  | _, _ -> Plus (lhs, rhs)

let rec optimize : t -> t =
  function
  | Const e -> Const (Poly.optimize_reals e)
  | Plus (lhs, rhs) -> Plus (optimize lhs, optimize rhs)
  | Sum (x, ub, s) -> Sum (x, Reals.optimize ub, optimize s)

type factor = { power: int; divisor: int }

let factor_to_n (e:Reals.integer) (i: factor) : Reals.integer =
  Reals.div (Reals.pow e (Num i.power)) (Num i.divisor)

let sum power e : Reals.integer =
  let rec formula : factor list -> Reals.integer =
    function
    | [f] -> factor_to_n e f
    | f :: l -> Reals.plus (factor_to_n e f) (formula l)
    | [] -> Num 0
  in
  (* https://en.wikipedia.org/wiki/Faulhaber%27s_formula *)
  match power with
  | 0 -> e
  | 1 ->
    formula [
      {power=1; divisor=2};
      {power=2; divisor=2};
    ]
  | 2 ->
    formula [
      {power=1; divisor=6};
      {power=2; divisor=2};
      {power=3; divisor=3};
    ]
  | 3 ->
    formula [
      {power=2; divisor=4};
      {power=3; divisor=2};
      {power=4; divisor=4};
    ]
  | 4 ->
    formula [
      {power=1; divisor=(-30)};
      {power=3; divisor=3};
      {power=4; divisor=2};
      {power=5; divisor=5};
    ]
  | 5 ->
    formula [
      {power=2; divisor=(-12)};
      {power=4; divisor=12};
      {power=5; divisor=2};
      {power=6; divisor=6};
    ]
  | 6 ->
    formula [
      {power=1; divisor=42};
      {power=3; divisor=(-6)};
      {power=5; divisor=2};
      {power=6; divisor=2};
      {power=7; divisor=7};
    ]
  | _ -> failwith ("S_" ^ string_of_int power ^ " not implemented")

let rec flatten : t -> t =
  function
  | Const _ as e -> e
  | Plus (lhs, rhs) -> plus (flatten lhs) (flatten rhs)
  | Sum (x, ub, s) ->
    let s = flatten s in
    (match s with
    | Const e ->
      Poly.from_reals x e
      |> Option.map (fun p ->
          print_endline (Poly.to_string p);
          let e =
            p
            |> Poly.to_seq
            |> Seq.map (fun (coefficient, degree) ->
                Reals.mult coefficient (sum degree ub)
              )
            |> Seq.fold_left Reals.plus Reals.zero
          in
          Const e
        )
      |> Option.value ~default:(Sum (x, ub, s))
    | _ -> Sum (x, ub, s)
    )

let rec to_string : t -> string =
  function
  | Const x -> Reals.to_string x
  | Sum (x, ub, s) ->
    "Σ_{" ^ Variable.name x ^ " | 1 ≤ " ^ Variable.name x ^ " ≤ " ^ Reals.to_string ub ^ "} " ^
    to_string s
  | Plus (lhs, rhs) -> "(" ^ to_string lhs ^ ") + (" ^ to_string rhs ^ ")"

let run_ra ~show_code (r: Ra.t) : string =
  let s = Summation.from_ra r in
  (if show_code then (Summation.to_string s |> print_endline) else ());
  s
  |> from_summation
  |> flatten
  |> optimize
  |> to_string
*)
