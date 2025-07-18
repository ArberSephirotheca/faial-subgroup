open Stage0
module IntMap = Common.IntMap
module Variable = Protocols.Variable
open Protocols

(*
  The idea behind this algorithm is to take each local variable and
  rename it so that each thread can refer to a distinct variable name.
  For instance, say expression is `2 * x + k` and `x` is thread local
  and `k` is thread global, then we want make `x` a different variable per
  thread.

  2 * x + k -> [ 2 * x_1 + k, 2 * x_2 + k, ...]
  *)
open Exp
type t = {
  suffix: string;
  locals: Variable.Set.t;
}

let proj_var (x:Variable.t) (ctx:t) : Variable.t =
  if Variable.Set.mem x ctx.locals then
    Variable.update_name (fun n -> n ^ "$" ^ ctx.suffix) x
  else
    x

let rec proj_n (n: nexp) (ctx:t) : nexp =
  match n with
  | Num _ -> n
  | CastInt e -> CastInt (proj_b e ctx)
  | Var x -> Var (proj_var x ctx)
  | Unary (o, e) -> Unary (o, proj_n e ctx)
  | Other _ -> failwith "unsupported"
  | Binary (o, n1, n2) -> Binary (o, proj_n n1 ctx, proj_n n2 ctx)
  | NIf (b, n1, n2) -> NIf (proj_b b ctx, proj_n n1 ctx, proj_n n2 ctx)
  | NCall (x, n) -> NCall (x, proj_n n ctx)

and proj_b (b: bexp) (ctx:t) : bexp =
  match b with
  | CastBool e -> CastBool (proj_n e ctx)
  | Pred (x, n) -> Pred (x, proj_n n ctx)
  | Bool _ -> b
  | BNot b -> BNot (proj_b b ctx)
  | BRel (o, b1, b2) -> BRel (o, proj_b b1 ctx, proj_b b2 ctx)
  | NRel (o, n1, n2) -> NRel (o, proj_n n1 ctx, proj_n n2 ctx)

(*
  General algorithm to replicate an element as a list of elements
  *)
let proj (count:int) (locals:Variable.Set.t) (f: t -> 'a) : 'a list =
  let rec loop (idx:int) (accum:'a list) : 'a list =
    if idx <= 0 then
      accum
    else
      let ctx = {
        suffix = string_of_int idx;
        locals = locals;
      } in
      loop (idx - 1) (f ctx :: accum)
  in
  loop count []


let replicate (cfg:Config.t) (locals:Variable.Set.t) (cond:bexp) (index:nexp) : (bexp * nexp) list =
  proj cfg.threads_per_warp locals
    (fun ctx -> proj_b cond ctx, proj_n index ctx)

(*
  Let us introduce a dissimilarity function `dissimilar e l` that
  given an expression `e` and a list of expressions `l` returns a condition
  that tests whether element `e` is dissimilar from every element of `l`,
  that is, ∀e' ∈ l : e <> e', defined as follows.
*)
let dissimilar (e:nexp) (l:nexp list) : bexp =
  List.fold_left
    (fun (accum:bexp) (e':nexp) -> b_and accum (n_neq e e'))
    (Bool true)
    l

(*
  Let us introduce a dissimilarity function `dissimilar e l` that
  given an expression `e` and a list of expressions `l` returns a condition
  that tests whether element `e` is dissimilar from every element of `l`,
  that is, ∀e' ∈ l : e <> e', defined as follows.
*)
let cond_dissimilar ((cnd,e):bexp * nexp) (l:(bexp * nexp) list) : bexp =
  List.fold_left
    (fun (accum:bexp) ((cnd', e'):bexp * nexp) ->
      let matches =
        b_and (n_eq e e')
          (b_and cnd cnd')
      in
      b_and accum (b_not matches)
    )
    (Bool true)
    l


let encode_ua (cfg:Config.t) (locals:Variable.Set.t) (cond:bexp) (index:nexp) : nexp =
  (* replicate index per each thread *)
  (proj cfg.threads_per_warp locals
    (fun ctx ->
      (proj_b cond ctx, proj_n index ctx)
    )
  )
  (* for each replciated index *)
  |> List.fold_left
    (fun (accum,visited: nexp * (bexp * nexp) list) (p:bexp * nexp) ->
      n_plus (n_if (cond_dissimilar p visited) (Num 1) (Num 0)) accum, p :: visited
    )
    (* total cost = 0, visited = [] *)
    (Num 0, [])
  |>
  (* take only the accumulated value *)
  fst

let ua (cfg:Config.t) (locals:Variable.Set.t) (cond:bexp) (index:nexp) : int option =
  let open Gen_z3 in
  let open Gen_z3.IntGen in
  let formula = encode_ua cfg locals cond index in
  optimize_expr Optimizer.Strategy.Maximize formula
  |> Result.to_option
