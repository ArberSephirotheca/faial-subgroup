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

let bank_blind (t : t) ~(stride : nexp) ~(bank_count : int) : bool =
  unsat_under_pre t (b_not (mod_eq_zero stride bank_count))

(* Prime factorisation of [n], each prime listed once. *)
let prime_factors (n : int) : int list =
  let rec go n p acc =
    if p * p > n then if n > 1 then n :: acc else acc
    else if n mod p = 0 then
      let rec drain n = if n mod p = 0 then drain (n / p) else n in
      go (drain n) (p + 1) (p :: acc)
    else go n (p + 1) acc
  in
  go n 2 [] |> List.rev

let coprime (t : t) ~(stride : nexp) ~(bank_count : int) : bool =
  match prime_factors bank_count with
  | [] -> true
  | factors ->
    let any_factor_divides =
      factors
      |> List.map (fun p -> mod_eq_zero stride p)
      |> List.fold_left b_or (Bool false)
    in
    unsat_under_pre t any_factor_divides
