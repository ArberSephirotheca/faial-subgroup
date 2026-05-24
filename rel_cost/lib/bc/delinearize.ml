open Protocols

(* BC's per-axis label, produced by [from_exp]. The [classify_with_sigma]
   helper inside [from_exp] is the only producer. *)
type axis_class =
  | BankBlind
  | Uniform
  | Diverse of int
  | NotInjective of int
  | Unknown

(* Self-contained delinearized view of a single BC access. [reduced]
   is the input expression (the analysis layer may still hand it to
   simulation as a fallback); [axes] is the per-axis classification.
   When delin's algorithm produces no candidate, [axes = []] and the
   downstream decider treats this as "fall through to simulation". *)
type t = {
  reduced : Exp.nexp;
  axes : axis_class list;
}

(* Product of [dims.(j)], [dims.(j+1)], ..., [dims.(last)] as a polynomial.
   Empty product (j past the last dim) returns 1. *)
let element_stride (dims : Expr.t list) (j : int) : Expr.t =
  dims
  |> List.filteri (fun i _ -> i >= j)
  |> List.fold_left Expr.( * ) (Expr.of_int 1)

(* Substitute the six [blockDim.*] / [gridDim.*] variables with concrete
   integers from [cfg], constfold, and read off a [Num n] if everything
   reduced. Returns [None] when the expression remains symbolic.

   Note: in the production pipeline, [Ra_compiler]'s upstream
   [Code.subst_block_dim] / [subst_grid_dim] already inline these
   variables to literals before [Metric_analysis.run] is invoked, so
   this helper is mostly a no-op against real kernels and only does
   work for callers that bypass the upstream substitution (e.g. unit
   tests). The oracle below covers the symbolic-stride cases that
   survive substitution. *)
let try_concrete (cfg : Config.t) (e : Expr.t) : int option =
  let bdim = cfg.block_dim in
  let gdim = cfg.grid_dim in
  let subst1 (x : Variable.t) (n : int) (e : Exp.nexp) : Exp.nexp =
    Subst.ReplacePair.n_subst (x, Num n) e
  in
  let reduced =
    Expr.to_nexp e
    |> subst1 Variable.bdim_x bdim.x
    |> subst1 Variable.bdim_y bdim.y
    |> subst1 Variable.bdim_z bdim.z
    |> subst1 Variable.gdim_x gdim.x
    |> subst1 Variable.gdim_y gdim.y
    |> subst1 Variable.gdim_z gdim.z
    |> Constfold.n_opt
  in
  match reduced with Num n -> Some n | _ -> None

(* Mirror [Normalize.BC.from_nexp]'s warp-variation rule: a name is
   warp-varying iff it sits in [locals ∪ tid_set] and is not pinned by
   [Config.is_warp_uniform] (which catches warp-local tids like [tid_y]
   when [blockDim.x >= threads_per_warp]). *)
let subscript_warp_class (cfg : Config.t) (locals : Variable.Set.t)
    (e : Expr.t) : [ `Uniform | `Varying ] =
  let free = Exp.n_free_names (Expr.to_nexp e) Variable.Set.empty in
  let scope = Variable.Set.union locals Variable.tid_set in
  let varying =
    Variable.Set.exists
      (fun x ->
        Variable.Set.mem x scope && not (Config.is_warp_uniform x cfg))
      free
  in
  if varying then `Varying else `Uniform

let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

(* "Warp-injective" is stricter than "warp-divergent": [Vectorized.
   put_tids] computes [tid_x = id mod block_dim.x] across the 32 thread
   slots, so when [block_dim.x < threads_per_warp] the [tid_x] values
   wrap within the warp and aren't a one-to-one map. A bare-tid
   subscript only counts as warp-injective once the tid in question
   actually spans the warp uniquely. *)
let tid_is_warp_injective (cfg : Config.t) (v : Variable.t) : bool =
  let n = cfg.threads_per_warp in
  if Variable.equal v Variable.tid_x then cfg.block_dim.x >= n
  else if Variable.equal v Variable.tid_y then
    cfg.block_dim.x = 1 && cfg.block_dim.y >= n
  else if Variable.equal v Variable.tid_z then
    cfg.block_dim.x * cfg.block_dim.y = 1 && cfg.block_dim.z >= n
  else false

(* Three injectivity patterns from rel-cost-delin.md:
   - bare warp-divergent atom [Var tid]
   - [tid + uniform_polynomial]: exactly one warp-varying term, that
     term is a bare warp-divergent tid
   - [c * tid] where [gcd(|c|, bank_count / g_stride) = 1]: the
     coprime-stride generator is injective modulo the bank cycle, so
     the [bank_count / g_stride] distinct banks each take exactly one
     [s_j] value. *)
let injectivity_class (cfg : Config.t) (locals : Variable.Set.t)
    ~(g_stride : int) (sub : Expr.t) : [ `Injective | `Unknown ] =
  let bank_count = cfg.bank_count in
  let modulus = if g_stride = 0 then 1 else bank_count / g_stride in
  let scope = Variable.Set.union locals Variable.tid_set in
  let is_warp_varying_var (x : Variable.t) : bool =
    Variable.Set.mem x scope && not (Config.is_warp_uniform x cfg)
  in
  let term_warp_class (t : Term.t) : [ `Uniform | `Varying ] =
    let free =
      Exp.n_free_names (Term.to_nexp t) Variable.Set.empty
    in
    if Variable.Set.exists is_warp_varying_var free then `Varying
    else `Uniform
  in
  let varying =
    Expr.to_list sub
    |> List.filter (fun t -> term_warp_class t = `Varying)
  in
  match varying with
  | [ single ] -> (
      let c = Term.coeff single in
      match Term.factors single with
      | [ (atom, 1) ] -> (
          match Atom.as_induction_var atom with
          | Some v
            when is_warp_varying_var v && tid_is_warp_injective cfg v ->
              if gcd (abs c) modulus = 1 then `Injective else `Unknown
          | _ -> `Unknown)
      | _ -> `Unknown)
  | _ -> `Unknown

(* Decorate each axis j of [idx] given delin's [s_j] subscripts and
   [d_1..d_{n}] dims. Stride [σ̂_j = Π dims[j..] mod bank_count] under
   the v1 assumption that elt_size = bytes_per_word. The optional
   [oracle] is the v2/v3 modular oracle: when [try_concrete] can't
   reduce a stride to a concrete integer, [Oracle.gcd_value] queries
   [kernel.pre] for the full divisor ladder and returns the proven
   [gcd(stride, bank_count)]. A returned [bank_count] maps to
   BankBlind; any smaller divisor [g] becomes [Diverse g] (when the
   subscript is also injective) or [NotInjective g]. *)
let classify_index ?(oracle : Oracle.t option) ~(config : Config.t)
    ~(locals : Variable.Set.t) (idx : Index.t) : axis_class list =
  let bank_count = config.bank_count in
  let normalize n = ((n mod bank_count) + bank_count) mod bank_count in
  let nsubs = List.length idx.indices in
  let classify_with_sigma sigma_hat sub =
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
          else NotInjective sigma_hat
  in
  let classify_symbolic ~stride sub =
    match oracle with
    | None -> Unknown
    | Some o ->
        match Oracle.gcd_value o ~stride ~bank_count with
        | None -> Unknown
        | Some g when g = bank_count -> classify_with_sigma 0 sub
        | Some g -> classify_with_sigma g sub
  in
  List.init nsubs (fun j ->
      let sub = List.nth idx.indices j in
      let stride_expr = element_stride idx.dims j in
      match try_concrete config stride_expr with
      | Some n_raw -> classify_with_sigma (normalize n_raw) sub
      | None ->
          let stride = Expr.to_nexp stride_expr in
          classify_symbolic ~stride sub)

(* Encapsulates BC's use of delin. Builds [Delin.Expr.from_nexp] with
   the right globals (the locals-with-tids union excluded), runs
   [Delin.Greedy.candidates], and decorates the resulting axes.
   Returns a self-contained record the analysis layer can interpret.
   When delin produces no candidate, [axes = []]. *)
let from_exp ?(oracle : Oracle.t option) ~(config : Config.t)
    ~(locals : Variable.Set.t) (reduced : Exp.nexp) : t =
  (* Mirror [Normalize.BC.from_nexp]'s locals-with-tids union: a thread
     coord is induction-side in delin's classification (not a parameter),
     so it must be excluded from the [globals] passed to
     [Expr.from_nexp]. *)
  let local_scope = Variable.Set.union locals Variable.tid_set in
  let globals =
    Variable.Set.diff
      (Exp.n_free_names reduced Variable.Set.empty)
      local_scope
  in
  let expr = Expr.from_nexp ~globals reduced in
  let size_params = Polynomial.size_params expr in
  match Greedy.candidates ~globals ~size_params expr |> Seq.uncons with
  | None -> { reduced; axes = [] }
  | Some (idx, _) ->
      let axes = classify_index ?oracle ~config ~locals idx in
      { reduced; axes }
