open Stage0
open Exp

module Step = struct
  type t = Plus of nexp | Mult of nexp

  let plus (e : nexp) : t = Plus e
  let mult (e : nexp) : t = Mult e
  let is_plus : t -> bool = function Plus _ -> true | _ -> false
  let is_mult : t -> bool = function Mult _ -> true | _ -> false
  let zero : t -> nexp = function Mult _ -> Num 1 | Plus _ -> Num 0

  (** Returns the increase function *)
  let to_inc : t -> nexp -> nexp -> nexp = function
    | Mult _ -> n_mult
    | Plus _ -> n_plus

  (** Returns the decrease function *)
  let to_dec : t -> nexp -> nexp -> nexp = function
    | Mult _ -> n_div
    | Plus _ -> n_minus

  let to_string : t -> string = function
    | Plus x -> "+= " ^ n_to_string x
    | Mult x -> "*= " ^ n_to_string x

  let inc : t -> nexp -> nexp = function
    | Plus n -> n_plus n
    | Mult n -> n_mult n

  let dec : t -> nexp -> nexp = function
    | Mult n -> fun m -> n_div m n
    | Plus n -> fun m -> n_minus m n

  let stride : t -> nexp = function Plus e -> e | Mult e -> e

  let is_valid : t -> bexp = function
    | Plus e -> n_gt e (Num 0)
    | Mult e -> n_gt e (Num 1)

  let map (f : nexp -> nexp) : t -> t = function
    | Plus n -> Plus (f n)
    | Mult n -> Mult (f n)

  let eval_res : t -> (int -> int, string) Result.t =
    let ( let* ) = Result.bind in
    function
    | Plus n ->
        let* x = n_eval_res n in
        Ok (fun y -> x + y)
    | Mult n ->
        let* x = n_eval_res n in
        Ok (fun y -> x * y)
end

type direction = Increase | Decrease

type t = {
  var : Variable.t;
  ty : C_type.t;
  dir : direction;
  lower_bound : nexp;
  upper_bound : nexp;
  step : Step.t;
}

let var (r : t) : Variable.t = r.var
let ty (r : t) : C_type.t = r.ty
let lower_bound (r : t) : nexp = r.lower_bound
let upper_bound (r : t) : nexp = r.upper_bound
let dir (r : t) : direction = r.dir
let step (r : t) : Step.t = r.step

let to_string (r : t) : string =
  let x = Variable.name r.var in
  let lb = n_to_string r.lower_bound in
  let ub = n_to_string r.upper_bound in
  let s =
    match r.step with
    | Plus (Num 1) -> ""
    | _ -> "; " ^ Variable.name r.var ^ " " ^ Step.to_string r.step
  in
  let d = match r.dir with Increase -> "" | Decrease -> ";↓" in
  x ^ " ∈ " ^ C_type.to_string r.ty ^ " | " ^ lb ^ " ≤ " ^ x ^ " ≤ " ^ ub ^ s
  ^ d

(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let map (f : nexp -> nexp) (r : t) : t =
  {
    r with
    lower_bound = f r.lower_bound;
    upper_bound = f r.upper_bound;
    step = Step.map f r.step;
  }

let make ?(lower_bound = Num 0) ?(step : Step.t = Plus (Num 1))
    ?(dir = Increase) ?(ty = C_type.int) (var : Variable.t) (upper_bound : nexp)
    : t =
  { var; lower_bound; upper_bound; step; dir; ty }

let eq_nums x l : bexp = List.map (fun i -> n_eq x (Num i)) l |> b_or_ex

let pow ~base (n : nexp) : bexp =
  let ub = 0xFFFFFFFF in
  (* Generate a list of powers *)
  let rec pows (n : int) : int list =
    let x = Common.pow ~base n in
    if x > ub then [] else if x == ub then [ x ] else x :: pows (n + 1)
  in
  pows 0 |> eq_nums n

let from_decl (var : Variable.t) (ty : C_type.t) : t =
  let lower_bound, upper_bound =
    ty |> C_type.to_int_dom
    |> Option.value ~default:Int_dom.signed_int
    |> Int_dom.to_range
  in
  {
    var;
    ty;
    dir = Increase;
    step = Plus (Num 1);
    lower_bound = Num lower_bound;
    upper_bound = Num upper_bound;
  }

(* Convert a variable declaration into a range *)
let decl_to_bexp (var : Variable.t) (ty : C_type.t) : bexp =
  ty |> C_type.to_int_dom
  |> Option.value ~default:Int_dom.signed_int
  |> Int_dom.to_bexp var

(* For a [Plus] range whose stride evaluates to a literal [k > 1],
   returns [k]. Detects strided additive loops that can be
   reparametrized over a unit-stride index. *)
let plus_step_literal (s : Step.t) : int option =
  match s with
  | Step.Plus e ->
      (match n_eval_res e with
       | Ok k when k > 1 -> Some k
       | _ -> None)
  | _ -> None

let to_bexp (r : t) : bexp =
  let x = Var r.var in
  let lb = r.lower_bound in
  let ub = r.upper_bound in
  (match r.step with
    | Plus (Num 1) -> []
    | Plus n ->
        [
          (* (x + lb) % step  == 0 *)
          n_eq (n_mod (n_minus x lb) n) (Num 0);
          (* Ensure that the step is positive *)
          (* n > 0 *)
          n_gt n (Num 0);
        ]
    | Mult (Num base) -> [ pow ~base x; (* base > 1 *) n_gt (Num base) (Num 1) ]
    | Mult e ->
        prerr_endline ("range_to_cond: unsupported range: " ^ Exp.n_to_string e);
        [ (* Ensure that the step is positive *) n_gt e (Num 1) ])
  @ [ (* lb <= x < ub *) n_le lb x; n_le x ub; decl_to_bexp r.var r.ty ]
  |> b_and_ex

(*
  Returns the bounds of the range variable
  as a boolean expression: lb <= x <= ub
*)
let bounds (r : t) : bexp =
  b_and (n_ge (Var r.var) r.lower_bound) (n_le (Var r.var) r.upper_bound)

let prev (r : t) : nexp =
  match r.dir with
  | Increase -> Step.dec r.step (Var r.var)
  | Decrease -> Step.inc r.step (Var r.var)

let has_next (r : t) : bexp = n_le r.lower_bound r.upper_bound
let is_empty (r : t) : bexp = b_not (has_next r)
let is_first (r : t) : bexp = n_eq (Var r.var) r.lower_bound

let first (r : t) : nexp =
  match r.dir with Increase -> r.lower_bound | Decrease -> r.upper_bound

(*
  ub - ((ub-lb) % step)

  0 ... 3 += 1 -> [0, 1, 2, 3]
  = 3 - ((3 - 0) % 3)
  = 3 - (3 % 3)
  = 3

  3 ... 13  += 3 -> [3, 6, 9, 12]
  = 13 - ((13 - 3) % 3)
  = 13 - (10 % 3)
  = 13 - 1
  = 12


  4 ... 15 += 3 -> [4, 7, 10, 13]
  = 15 - ((15 - 4) % 3)
  = 15 - (11 % 3)
  = 15 - 2
  = 13

 *)

let last_plus ~lower_bound ~upper_bound (step : nexp) : nexp =
  n_minus upper_bound (n_mod (n_minus upper_bound lower_bound) step)

(*

  lb + ((ub-lb) % step)

  0 ... 3 -= 1 -> [3, 2, 1, 0]
  = 0 + ((3 - 0) % 3)
  = 0 + (3 % 3)
  = 0

  3 ... 13  -= 3 -> [13, 10, 7, 4]
  = 3 + ((13 - 3) % 3)
  = 3 + (10 % 3)
  = 3 + 1
  = 4

  4 ... 15 += 3 -> [15, 12, 9, 6]
  = 4 + ((15 - 4) % 3)
  = 4 + (11 % 3)
  = 4 + 2
  = 6
 *)

let last_minus ~lower_bound ~upper_bound (step : nexp) : nexp =
  n_plus lower_bound (n_mod (n_minus upper_bound lower_bound) step)

let highest_power =
  let open Common in
  [|
    0;
    0;
    pow ~base:2 29;
    pow ~base:3 18;
    pow ~base:4 14;
    pow ~base:5 12;
    pow ~base:6 11;
    pow ~base:7 10;
    pow ~base:8 9;
    pow ~base:9 9;
    pow ~base:10 9;
    pow ~base:11 8;
    pow ~base:12 8;
    pow ~base:13 8;
    pow ~base:14 7;
    pow ~base:15 7;
    pow ~base:16 7;
    pow ~base:17 7;
    pow ~base:18 7;
    pow ~base:19 7;
    pow ~base:20 6;
    pow ~base:21 6;
    pow ~base:22 6;
    pow ~base:23 6;
    pow ~base:24 6;
    pow ~base:25 6;
  |]

let gen_highest_power ~base (e : nexp) : nexp =
  let rec gen (pow : int) : nexp =
    let p = Num pow in
    if pow <= 1 then Num 1 else NIf (n_le p e, p, gen (pow / base))
  in
  gen highest_power.(base)

let highest_power ~base : nexp -> nexp = function
  | Num n -> Num (Common.highest_power ~base n)
  | e -> gen_highest_power ~base e

(*

  lb * (ub / lb)

  lb * step^log_step(ub/lb)

  4 ... 15 *= 3 = [4, 12]
  = 4 * 3 ^ (log3 (15/4))
  = 4 * 3 ^ (log3 3)
  = 4 * 3
  = 12

  3 ... 76 *= 5 -> [3, 15, 75]
  = 3 * 5 ^ (log5 (76 / 3))
  = 3 * 5 ^ (log5 25)
  = 3 * 5 ^ 2
  = 3 * 25
  = 75

  4 ... 20 *= 2 -> [4, 8, 16]
  = 4 * 2 ^ (log2 (20/4))
  = 4 * 2 ^ (log2 5)
  = 4 * 2 ^ 2
  = 4 * 4
  = 16

  3 .. 10  *= 4 -> [3]
  = 3 * 4 ^ log4 (10/3)
  = 3 * 4 ^ 0
  = 3
 *)

let last_mult ~lower_bound ~upper_bound (step : int) : nexp =
  if step >= 2 then
    Binary
      ( Mult Signedness.Signed,
        lower_bound,
        highest_power ~base:step
          (Binary (Div Signedness.Signed, upper_bound, lower_bound)) )
  else failwith ("last_mult: invalid base: " ^ string_of_int step)

(*
  ub / (ub / lb)

  4 ... 15 /= 3 = [15, 5]
  = 15 / (15 / 4)
  = 15 / 3
  = 5

  3 ... 76 /= 5 -> [76, 15, 3]
  = 76 / (76 / 3)
  = 3

 *)
let last_div ~lower_bound ~upper_bound (step : int) : nexp =
  if step >= 2 then
    Exp.n_div upper_bound
      (highest_power ~base:step (Exp.n_div upper_bound lower_bound))
  else failwith ("last_mult: invalid base: " ^ string_of_int step)

let last (r : t) : nexp option =
  match (r.dir, r.step) with
  | Increase, Plus s ->
      Some (last_plus ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | Decrease, Plus s ->
      Some (last_minus ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | Increase, Mult (Num s) when s >= 2 ->
      Some (last_mult ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | Decrease, Mult (Num s) when s >= 2 ->
      Some (last_div ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | _ -> None

(* Returns the last element of a bound *)

let lossy_last (r : t) : nexp =
  match last r with
  | Some e -> e
  | None ->
      prerr_endline ("WARNING: lossy_last: unsupported base: " ^ to_string r);
      last_plus ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound (Num 1)

let stride (r : t) : nexp = Step.stride r.step

(* The first element in a while loop *)
let while_init (r : t) : nexp =
  match r.dir with Increase -> r.lower_bound | Decrease -> r.upper_bound

(* The condition in a while loop *)
let while_cond (r : t) : bexp =
  match r.dir with
  | Increase -> n_le (Var r.var) r.upper_bound
  | Decrease -> n_ge (Var r.var) r.lower_bound

(* An increment of a while loop *)
let while_inc (r : t) : nexp =
  let x = Var r.var in
  let o, e1, e2 =
    match (r.dir, r.step) with
    | Increase, Step.Plus e -> (N_binary.Plus Signedness.Signed, x, e)
    | Decrease, Step.Plus e -> (Minus Signedness.Signed, x, e)
    | Increase, Step.Mult e -> (Mult Signedness.Signed, x, e)
    | Decrease, Step.Mult e -> (Div Signedness.Signed, x, e)
  in
  Binary (o, e1, e2)

let next (r : t) : t =
  match r.dir with
  | Increase -> { r with lower_bound = Step.inc r.step r.lower_bound }
  | Decrease -> { r with upper_bound = Step.dec r.step r.upper_bound }

(* Translates the range to zero, meaning if the range is
   [x, y] becomes [0, y - x] for an additive range
   [x, y] becomes [1, y / x] for a multiplicative range
*)
let to_zero (r : t) : t =
  {
    r with
    lower_bound = Step.zero r.step;
    upper_bound = Step.to_dec r.step r.upper_bound r.lower_bound;
  }

let is_valid (r : t) : bexp =
  b_and (Step.is_valid r.step)
    (match r.step with
    | Mult _ -> n_neq r.lower_bound (Num 0)
    | Plus _ -> Bool true)

let eval_res (r : t) : (int list, string) Result.t =
  let ( let* ) = Result.bind in
  let* lb = n_eval_res r.lower_bound in
  let* ub = n_eval_res r.upper_bound in
  let* b = b_eval_res (is_valid r) in
  let* inc = Step.eval_res r.step in
  if b then
    let rec iter lb = if lb < ub then lb :: iter (inc lb) else [] in
    Ok (iter lb)
  else Error ("Invalid range: " ^ to_string r)

let eval_opt (r : t) : int list option = eval_res r |> Result.to_option
let eval_is_empty (r : t) : bool = Exp.b_eval_res (is_empty r) = Ok true

let exists (f : Variable.t -> bool) (r : t) : bool =
  n_exists f r.lower_bound || n_exists f r.upper_bound

let free_names (r : t) (fns : Variable.Set.t) : Variable.Set.t =
  n_free_names r.lower_bound fns
  |> n_free_names r.upper_bound
  |> n_free_names (Step.stride r.step)

let intersects (s : Variable.Set.t) (r : t) : bool =
  n_intersects s r.lower_bound || n_intersects s r.upper_bound
