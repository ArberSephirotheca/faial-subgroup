open Stage0
open Protocols
open Exp

type t = {
  ctx : Z3.context;
  solver : Z3.Solver.solver;
}

let preprocess (b : bexp) : bexp =
  b |> Predicates.b_inline |> Predicates.strip_cross_thread

let make ~(timeout : int) (pre : bexp) : t =
  let args =
    if timeout > 0 then [ ("timeout", string_of_int timeout) ] else []
  in
  let ctx = Z3.mk_context args in
  let solver = Z3.Solver.mk_solver ctx None in
  Z3.Solver.add solver [ Gen_z3.Bv64Gen.b_to_expr ctx (preprocess pre) ];
  { ctx; solver }

let release (t : t) : unit = Z3.Solver.reset t.solver

let with_slot ~(timeout : int) (pre : bexp) (f : t -> 'a) : 'a =
  let slot = make ~timeout pre in
  Fun.protect ~finally:(fun () -> release slot) (fun () -> f slot)

(* Z3 BV unsigned modulo of [stride] by [n], compared to zero. *)
let mod_eq_zero (stride : nexp) (n : int) : bexp =
  n_eq (n_umod stride (Num n)) (Num 0)

(* Push (preprocessed delta), check, pop. UNSAT means [pre]
   entails [~delta]. *)
let unsat_under_pre (t : t) (delta : bexp) : bool =
  let delta = preprocess delta in
  Z3.Solver.push t.solver;
  Z3.Solver.add t.solver [ Gen_z3.Bv64Gen.b_to_expr t.ctx delta ];
  let result =
    Phase_timer.measure "bc/modular" (fun () ->
      Z3.Solver.check t.solver [])
  in
  Z3.Solver.pop t.solver 1;
  match result with
  | Z3.Solver.UNSATISFIABLE -> true
  | Z3.Solver.SATISFIABLE | Z3.Solver.UNKNOWN -> false

(* Divisors of [n] in ascending order. *)
let divisors (n : int) : int list =
  let rec go d acc =
    if d > n then List.rev acc
    else if n mod d = 0 then go (d + 1) (d :: acc)
    else go (d + 1) acc
  in
  go 1 []

(* Build the formula [gcd(stride, n) = g]: [g | stride] and for every
   divisor [d] of [n] strictly above [g] with [g | d], [d ∤ stride].
   The maximal divisor candidates pin [gcd] down exactly. *)
let gcd_eq_formula ~(divisors_of_n : int list) ~(stride : nexp) (g : int)
    : bexp =
  let g_divides = mod_eq_zero stride g in
  let larger_multiples =
    divisors_of_n |> List.filter (fun d -> d > g && d mod g = 0)
  in
  let none_of_larger_divide =
    larger_multiples
    |> List.map (fun d -> b_not (mod_eq_zero stride d))
    |> List.fold_left b_and (Bool true)
  in
  b_and g_divides none_of_larger_divide

(* Soundness: each successful query maps to one of
   [bc_stride_{2,4,8,16}], [bc_mul_tid_add_eq_gcd] for the offset
   variant, plus the [bank_count] and coprime endpoints from the
   pre-existing closed forms. Returns the proven gcd of [stride] and
   [bank_count] under [pre], or [None] if no divisor query
   UNSAT-discharges. *)
let gcd_value (t : t) ~(stride : nexp) ~(bank_count : int) : int option =
  let ds = divisors bank_count in
  let rec find = function
    | [] -> None
    | g :: rest ->
      let formula = gcd_eq_formula ~divisors_of_n:ds ~stride g in
      if unsat_under_pre t (b_not formula) then Some g else find rest
  in
  find ds

(* Bounded enumeration of warp tids matching [Vectorized.put_tids]:
   for each lane [id ∈ [0, threads_per_warp)],
   [tid_x = id mod bd.x], [tid_y = (id / bd.x) mod bd.y],
   [tid_z = (id / (bd.x * bd.y)) mod bd.z]. *)
let warp_tid_positions (cfg : Config.t) : (int * int * int) list =
  let bd = cfg.block_dim in
  List.init cfg.threads_per_warp (fun id ->
    let tx = id mod bd.x in
    let ty = id / bd.x mod bd.y in
    let tz = id / (bd.x * bd.y) mod bd.z in
    (tx, ty, tz))

let subst_tids ~(tx : int) ~(ty : int) ~(tz : int) (e : nexp) : nexp =
  e
  |> Subst.ReplacePair.n_subst (Variable.tid_x, Num tx)
  |> Subst.ReplacePair.n_subst (Variable.tid_y, Num ty)
  |> Subst.ReplacePair.n_subst (Variable.tid_z, Num tz)
  |> Constfold.n_opt

(* Whole-index modular injectivity query. Soundness:
   [f_pairwise_distinct_enabled]; pairwise-distinct bank IDs on the
   enabled-tid sublist gives [f <= 1], i.e. conflict cost 0. The
   premise of the lemma is stated on [filter_enabled vb vn]; here
   [divergence] gates whether the lane contributes, mirroring
   [BMap.t] selection on the Rocq side. *)
let warp_injective (t : t) ~(config : Config.t) ~(divergence : bexp)
    ~(index : nexp) : bool =
  let n = config.bank_count in
  let lane_enabled (tx, ty, tz) : bool =
    match
      divergence
      |> Subst.ReplacePair.b_subst (Variable.tid_x, Num tx)
      |> Subst.ReplacePair.b_subst (Variable.tid_y, Num ty)
      |> Subst.ReplacePair.b_subst (Variable.tid_z, Num tz)
      |> Constfold.b_opt
    with
    | Bool false -> false
    | _ -> true
  in
  let bank_id_of (tx, ty, tz) : nexp =
    n_umod (subst_tids ~tx ~ty ~tz index) (Num n)
  in
  let enabled_positions =
    warp_tid_positions config |> List.filter lane_enabled
  in
  match enabled_positions with
  | [] | [ _ ] -> true
  | _ ->
    let bank_ids = List.map bank_id_of enabled_positions in
    unsat_under_pre t (b_not (Distinct bank_ids))
