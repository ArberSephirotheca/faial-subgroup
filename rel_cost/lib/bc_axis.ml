open Protocols

type axis_class =
  | BankBlind
  | Uniform
  | Diverse of int
  | NotInjective of int
  | Unknown

type bc_outcome =
  | Exact of Cost.t
  | NeedsSimulation of Exp.nexp

(* Product of [dims.(j)], [dims.(j+1)], ..., [dims.(last)] as a polynomial.
   Empty product (j past the last dim) returns 1. *)
let element_stride (dims : Delin.Expr.t list) (j : int) : Delin.Expr.t =
  dims
  |> List.filteri (fun i _ -> i >= j)
  |> List.fold_left Delin.Expr.( * ) (Delin.Expr.of_int 1)

(* Substitute the six [blockDim.*] / [gridDim.*] variables with concrete
   integers from [cfg], constfold, and read off a [Num n] if everything
   reduced. Returns [None] when the expression remains symbolic. *)
let try_concrete (cfg : Config.t) (e : Delin.Expr.t) : int option =
  let bdim = cfg.block_dim in
  let gdim = cfg.grid_dim in
  let subst1 (x : Variable.t) (n : int) (e : Exp.nexp) : Exp.nexp =
    Subst.ReplacePair.n_subst (x, Num n) e
  in
  let reduced =
    Delin.Expr.to_nexp e
    |> subst1 Variable.bdim_x bdim.x
    |> subst1 Variable.bdim_y bdim.y
    |> subst1 Variable.bdim_z bdim.z
    |> subst1 Variable.gdim_x gdim.x
    |> subst1 Variable.gdim_y gdim.y
    |> subst1 Variable.gdim_z gdim.z
    |> Constfold.n_opt
  in
  match reduced with Num n -> Some n | _ -> None

(* Mirror [Metric_analysis.BC.from_nexp]'s warp-variation rule: a name is
   warp-varying iff it sits in [locals ∪ tid_set] and is not pinned by
   [Config.is_warp_uniform] (which catches warp-local tids like [tid_y]
   when [blockDim.x >= threads_per_warp]). *)
let subscript_warp_class (cfg : Config.t) (locals : Variable.Set.t)
    (e : Delin.Expr.t) : [ `Uniform | `Varying ] =
  let free = Exp.n_free_names (Delin.Expr.to_nexp e) Variable.Set.empty in
  let scope = Variable.Set.union locals Variable.tid_set in
  let varying =
    Variable.Set.exists
      (fun x ->
        Variable.Set.mem x scope && not (Config.is_warp_uniform x cfg))
      free
  in
  if varying then `Varying else `Uniform

let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

(* Three injectivity patterns from rel-cost-delin.md:
   - bare warp-divergent atom [Var tid]
   - [tid + uniform_polynomial]: exactly one warp-varying term, that
     term is a bare warp-divergent tid
   - [c * tid] where [gcd(|c|, bank_count / g_stride) = 1]: the
     coprime-stride generator is injective modulo the bank cycle, so
     the [bank_count / g_stride] distinct banks each take exactly one
     [s_j] value
   "Warp-injective" is stricter than "warp-divergent": [Vectorized.
   put_tids] computes [tid_x = id mod block_dim.x] across the 32
   thread slots, so when [block_dim.x < threads_per_warp] the
   [tid_x] values wrap and aren't a one-to-one map. Each of the
   three patterns above only counts as injective once the tid in
   question actually spans the warp uniquely. *)
let tid_is_warp_injective (cfg : Config.t) (v : Variable.t) : bool =
  let n = cfg.threads_per_warp in
  if Variable.equal v Variable.tid_x then cfg.block_dim.x >= n
  else if Variable.equal v Variable.tid_y then
    cfg.block_dim.x = 1 && cfg.block_dim.y >= n
  else if Variable.equal v Variable.tid_z then
    cfg.block_dim.x * cfg.block_dim.y = 1 && cfg.block_dim.z >= n
  else false

let injectivity_class (cfg : Config.t) (locals : Variable.Set.t)
    ~(g_stride : int) (sub : Delin.Expr.t) : [ `Injective | `Unknown ] =
  let bank_count = cfg.bank_count in
  let modulus = if g_stride = 0 then 1 else bank_count / g_stride in
  let scope = Variable.Set.union locals Variable.tid_set in
  let is_warp_varying_var (x : Variable.t) : bool =
    Variable.Set.mem x scope && not (Config.is_warp_uniform x cfg)
  in
  let term_warp_class (t : Delin.Term.t) : [ `Uniform | `Varying ] =
    let free =
      Exp.n_free_names (Delin.Term.to_nexp t) Variable.Set.empty
    in
    if Variable.Set.exists is_warp_varying_var free then `Varying
    else `Uniform
  in
  let varying =
    Delin.Expr.to_list sub
    |> List.filter (fun t -> term_warp_class t = `Varying)
  in
  match varying with
  | [ single ] -> (
      let c = Delin.Term.coeff single in
      match Delin.Term.factors single with
      | [ (atom, 1) ] -> (
          match Delin.Atom.as_induction_var atom with
          | Some v
            when is_warp_varying_var v && tid_is_warp_injective cfg v ->
              if gcd (abs c) modulus = 1 then `Injective else `Unknown
          | _ -> `Unknown)
      | _ -> `Unknown)
  | _ -> `Unknown

(* Classify each axis j of the index given delin's [s_j] subscripts and
   [d_1..d_{n}] dims. Stride [σ̂_j = Π dims[j..] mod bank_count] under
   the v1 assumption that elt_size = bytes_per_word. *)
let classify_index ~(config : Config.t) ~(locals : Variable.Set.t)
    (idx : Delin.Index.t) : axis_class list =
  let bank_count = config.bank_count in
  let normalize n = ((n mod bank_count) + bank_count) mod bank_count in
  let nsubs = List.length idx.indices in
  List.init nsubs (fun j ->
      let sub = List.nth idx.indices j in
      let stride_expr = element_stride idx.dims j in
      match try_concrete config stride_expr with
      | None -> Unknown
      | Some n_raw ->
          let sigma_hat = normalize n_raw in
          let warp = subscript_warp_class config locals sub in
          if sigma_hat = 0 then
            match warp with `Uniform -> Uniform | `Varying -> BankBlind
          else
            match warp with
            | `Uniform -> Uniform
            | `Varying ->
                let g = gcd sigma_hat bank_count in
                let inj = injectivity_class config locals ~g_stride:g sub in
                if inj = `Injective then Diverse sigma_hat
                else NotInjective sigma_hat)

(* Decision rules from rel-cost-delin.md. The classification above
   never emits [NotInjective] or [Unknown] for warp-uniform axes; those
   labels can only land on warp-varying axes, so the case split below
   treats them as fall-throughs to simulation. *)
let decide ~(config : Config.t) ~(tid_count : int) ~(reduced : Exp.nexp)
    (classes : axis_class list) : bc_outcome =
  let needs () = NeedsSimulation reduced in
  let exact value =
    Exact (Cost.from_int ~value ~exact:true ())
  in
  let bank_count = config.bank_count in
  let unknown_present =
    List.exists (function Unknown -> true | _ -> false) classes
  in
  let warp_varying =
    List.filter
      (function
        | BankBlind | Diverse _ | NotInjective _ -> true
        | Uniform | Unknown -> false)
      classes
  in
  if unknown_present then needs ()
  else
    match warp_varying with
    | [] -> exact 0
    | [ BankBlind ] -> exact (max (tid_count - 1) 0)
    | [ Diverse sigma_hat ] ->
        let g = gcd sigma_hat bank_count in
        exact (max ((tid_count * g / bank_count) - 1) 0)
    | [ NotInjective _ ] -> needs ()
    | _ -> needs ()
