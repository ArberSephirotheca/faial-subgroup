open Protocols
open Exp

module Proj = struct
  (*
    The idea behind this algorithm is to take each local variable and
    rename it so that each thread can refer to a distinct variable name.
    For instance, say expression is `2 * x + k` and `x` is thread local
    and `k` is thread global, then we want make `x` a different variable per
    thread.

    2 * x + k -> [ 2 * x_1 + k, 2 * x_2 + k, ...]
    *)
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
    | Distinct exprs -> Distinct (List.map (fun expr -> proj_n expr ctx) exprs)

  (*
    General algorithm to replicate an element as a list of elements
    *)
  let run (count:int) (locals:Variable.Set.t) (f: t -> 'a) : 'a list =
    let rec loop (idx:int) (accum:'a list) : 'a list =
      if idx < 0 then
        accum
      else
        let ctx = {
          suffix = string_of_int idx;
          locals = locals;
        } in
        loop (idx - 1) (f ctx :: accum)
    in
    loop (count - 1) []

  (* Project a condition and an index *)
  let run_pair
    (cfg:Config.t)
    (locals:Variable.Set.t)
    (cond:bexp)
    (index:nexp)
  :
    (bexp * nexp) list
  =
    run cfg.threads_per_warp locals
      (fun ctx -> proj_b cond ctx, proj_n index ctx)
end

(*
  Let us introduce a dissimilarity function `dissimilar e l` that
  given an expression `e` and a list of expressions `l` returns a condition
  that tests whether element `e` is dissimilar from every element of `l`,
  that is, ∀e' ∈ l : e <> e', defined as follows.
*)
let cond_dissimilar ((cnd,e):bexp * nexp) (l:(bexp * nexp) list) : bexp =
  List.fold_left
    (* every index in `l` must differ from `e` *)
    (fun (accum:bexp) ((cnd', e'):bexp * nexp) ->
      let matches =
        b_and
          (n_eq e e')
          cnd'
      in
      b_and accum (b_not matches)
    )
    cnd (* the condition `cnd` of `e` must be enabled *)
    l

let clamp ~lower ~value ~upper : bexp =
  b_and (n_ge value lower) (n_lt value upper)

let gen_tid (x:string) (suffix:string) : nexp =
  Var (Variable.from_name ("threadIdx." ^ x ^ "$" ^ suffix))

let gen_tid_x = gen_tid "x"
let gen_tid_y = gen_tid "y"
let gen_tid_z = gen_tid "z"

let gen_tids (suffix:string) : nexp * nexp * nexp =
  (gen_tid_x suffix, gen_tid_y suffix, gen_tid_z suffix)

let tid_bounds_constraint (cfg:Config.t) : bexp =
  (* only generate variables for warp divergent tid *)
  let bounds_for_thread (suffix:string) : bexp =
    [
      Variable.tid_x, cfg.block_dim.x;
      Variable.tid_y, cfg.block_dim.y;
      Variable.tid_z, cfg.block_dim.z
    ]
    |> List.filter (fun (x, _) -> not (Config.is_warp_uniform x cfg))
    |> List.map (fun (x, d) ->
        let x = Variable.update_name (fun n -> n ^ "$" ^ suffix) x in
        clamp ~lower:(Num 0) ~value:(Var x) ~upper:(Num d)
      )
    |> b_and_ex
  in
  List.init cfg.threads_per_warp (fun i -> bounds_for_thread (string_of_int i))
  |> b_and_ex

(* Compute linear thread ID from (x,y,z) coordinates; omit warp-uniform fragments *)
let thread_id (suffix:string) (cfg:Config.t) : nexp =
  let tid_x, tid_y, tid_z = gen_tids suffix in
  (* only generate variables for warp divergent tid *)
  let tid_x =
    if Config.is_warp_uniform Variable.tid_x cfg then
      Num 0
    else
      tid_x
  in
  let tid_y =
    if Config.is_warp_uniform Variable.tid_y cfg then
      Num 0
    else
      n_mult tid_y (Num cfg.block_dim.x)
  in
  let tid_z =
    if Config.is_warp_uniform Variable.tid_z cfg then
      Num 0
    else
      n_mult tid_z (Num (cfg.block_dim.x * cfg.block_dim.y))
  in
  n_plus tid_x (n_plus tid_y tid_z)

(* Compute warp ID for each thread *)
let warp_id (suffix:string) (cfg:Config.t) : nexp =
  n_div (thread_id suffix cfg) (Num cfg.threads_per_warp)

(* All threads must belong to the same warp *)
let same_warp_constraint (cfg:Config.t) : bexp =
  List.init (cfg.threads_per_warp - 1) (fun i ->
    let wid_i = warp_id (string_of_int i) cfg in
    let wid_next = warp_id (string_of_int (i + 1)) cfg in
    n_eq wid_i wid_next
  )
  |> b_and_ex


let unique_tid_constraint (cfg:Config.t) : bexp =
  (* Generate thread IDs for each thread in the warp *)
  let thread_ids = 
    List.init cfg.threads_per_warp (fun i -> 
      thread_id (string_of_int i) cfg
    )
  in
  (* Use the built-in distinct primitive to ensure all thread IDs are unique *)
  Distinct thread_ids

let warp_constraints (cfg:Config.t) : bexp =
  b_and_ex [
    unique_tid_constraint cfg;
    tid_bounds_constraint cfg;
    same_warp_constraint cfg;
  ]

let encode_ua
  (cfg:Config.t)
  (locals:Variable.Set.t)
  (cond:bexp)
  (index:nexp)
:
  nexp
=
  let index =
    (* replicate index per each thread *)
    (Proj.run cfg.threads_per_warp locals
      (fun ctx ->
        (Proj.proj_b cond ctx, Proj.proj_n index ctx)
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
  in
  index

let thread_locals_list (cfg:Config.t) : Variable.t list =
  Variable.tid_list
  |> List.filter (fun x -> not (Config.is_warp_uniform x cfg))

let thread_locals_set (cfg:Config.t) : Variable.Set.t =
  cfg
  |> thread_locals_list
  |> Variable.Set.of_list


let ua
  ?(strategy=Gen_z3.Optimizer.Strategy.Maximize)
  (cfg:Config.t)
  (locals:Variable.Set.t)
  (cond:bexp)
  (index:nexp)
:
  int option
=
  let open Gen_z3.IntGen in
  let locals = Variable.Set.union locals (thread_locals_set cfg) in
  let formula = encode_ua cfg locals cond index in
  optimize_expr strategy ~pre:(warp_constraints cfg) formula
  |> Result.to_option
