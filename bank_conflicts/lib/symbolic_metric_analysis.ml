open Protocols
open Exp

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

(*
  General algorithm to replicate an element as a list of elements
  *)
let proj (count:int) (locals:Variable.Set.t) (f: t -> 'a) : 'a list =
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

let generate_thread_tids (cfg:Config.t) : (nexp * nexp * nexp) list =
  proj cfg.threads_per_warp Variable.tid_set (fun ctx ->
    let tidx = proj_var Variable.tid_x ctx in
    let tidy = proj_var Variable.tid_y ctx in
    let tidz = proj_var Variable.tid_z ctx in
    (Var tidx, Var tidy, Var tidz)
  )

let clamp ~lower ~value ~upper : bexp =
  b_and (n_ge value lower) (n_lt value upper)

let unique_tid_constraint (cfg:Config.t) : bexp =
  let thread_tids = generate_thread_tids cfg in

  (* For each pair of threads, ensure their tid tuples are different *)
  let rec loop tids acc =
    match tids with
    | [] -> acc
    | (tx1, ty1, tz1) :: rest ->
        let constraints_for_this_thread =
          List.map (fun (tx2, ty2, tz2) ->
            (* NOT((tx1, ty1, tz1) == (tx2, ty2, tz2)) *)
            b_not (b_and_ex [n_eq tx1 tx2; n_eq ty1 ty2; n_eq tz1 tz2])
          ) rest
        in
        let combined = List.fold_left b_and b_true constraints_for_this_thread in
        loop rest (b_and acc combined)
  in

  loop thread_tids b_true

let gen_tid (x:string) (suffix:string) : nexp =
  Var (Variable.from_name ("threadIdx." ^ x ^ "$" ^ suffix))

let gen_tid_x = gen_tid "x"
let gen_tid_y = gen_tid "y"
let gen_tid_z = gen_tid "z"

let gen_tids (suffix:string) : nexp * nexp * nexp =
  (gen_tid_x suffix, gen_tid_y suffix, gen_tid_z suffix)

let tid_bounds_constraint (cfg:Config.t) : bexp =
  let bounds_for_thread (suffix:string) : bexp =
    let tid_x, tid_y, tid_z = gen_tids suffix in
    b_and_ex [
      clamp ~lower:(Num 0) ~value:tid_x ~upper:(Num cfg.block_dim.x);
      clamp ~lower:(Num 0) ~value:tid_y ~upper:(Num cfg.block_dim.y);
      clamp ~lower:(Num 0) ~value:tid_z ~upper:(Num cfg.block_dim.z);
    ]
  in
  let thread_constraints = 
    List.init cfg.threads_per_warp (fun i -> bounds_for_thread (string_of_int i))
  in
  b_and_ex thread_constraints

let same_warp_constraint (cfg:Config.t) : bexp =
  (* Compute linear thread ID from (x,y,z) coordinates *)
  let linear_tid_for_thread suffix =
    let tid_x, tid_y, tid_z = gen_tids suffix in
    n_plus tid_x
      (n_plus 
        (n_mult tid_y (Num cfg.block_dim.x))
        (n_mult tid_z (Num (cfg.block_dim.x * cfg.block_dim.y))))
  in
  
  (* Compute warp ID for each thread *)
  let warp_id_for_thread suffix =
    let linear_tid = linear_tid_for_thread suffix in
    n_div linear_tid (Num cfg.threads_per_warp)
  in
  
  (* All threads must belong to the same warp *)
  let warp_equalities = 
    List.init (cfg.threads_per_warp - 1) (fun i ->
      let wid_i = warp_id_for_thread (string_of_int i) in
      let wid_next = warp_id_for_thread (string_of_int (i + 1)) in
      n_eq wid_i wid_next
    )
  in
  
  b_and_ex warp_equalities

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
  in
  index



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
  let formula = encode_ua cfg locals cond index in
  optimize_expr strategy ~pre:(warp_constraints cfg) formula
  |> Result.to_option
