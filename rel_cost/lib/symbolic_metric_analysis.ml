open Stage0
open Protocols
open Exp

let proj ~(suffix : string) : Variable.t -> Variable.t =
  Variable.update_name (fun n -> n ^ "$" ^ suffix)

module Proj = struct
  (*
    The idea behind this algorithm is to take each local variable and
    rename it so that each thread can refer to a distinct variable name.
    For instance, say expression is `2 * x + k` and `x` is thread local
    and `k` is thread global, then we want make `x` a different variable per
    thread.

    2 * x + k -> [ 2 * x_1 + k, 2 * x_2 + k, ...]
    *)
  type t = { suffix : string; locals : Variable.Set.t }

  let proj_var (x : Variable.t) (ctx : t) : Variable.t =
    if Variable.Set.mem x ctx.locals then proj ~suffix:ctx.suffix x else x

  let rec proj_n (n : nexp) (ctx : t) : nexp =
    match n with
    | Num _ -> n
    | CastInt e -> CastInt (proj_b e ctx)
    | Var x -> Var (proj_var x ctx)
    | Unary (o, e) -> Unary (o, proj_n e ctx)
    | Other _ -> failwith "unsupported"
    | Binary (o, n1, n2) -> Binary (o, proj_n n1 ctx, proj_n n2 ctx)
    | NIf (b, n1, n2) -> NIf (proj_b b ctx, proj_n n1 ctx, proj_n n2 ctx)
    | NCall (x, n) -> NCall (x, proj_n n ctx)

  and proj_b (b : bexp) (ctx : t) : bexp =
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
  let split (count : int) (locals : Variable.Set.t) (f : t -> 'a) : 'a list =
    let rec loop (idx : int) (accum : 'a list) : 'a list =
      if idx < 0 then accum
      else
        let ctx = { suffix = string_of_int idx; locals } in
        loop (idx - 1) (f ctx :: accum)
    in
    loop (count - 1) []

  let b_split (count : int) (locals : Variable.Set.t) (e : bexp) : bexp list =
    e |> proj_b |> split count locals

  let n_split (count : int) (locals : Variable.Set.t) (e : nexp) : nexp list =
    e |> proj_n |> split count locals
end

(*
  Let us introduce a dissimilarity function `dissimilar e l` that
  given an expression `e` and a list of expressions `l` returns a condition
  that tests whether element `e` is dissimilar from every element of `l`,
  that is, ∀e' ∈ l : e <> e', defined as follows.
*)
let cond_dissimilar ((cnd, e) : bexp * nexp) (l : (bexp * nexp) list) : bexp =
  List.fold_left
    (* every index in `l` must differ from `e` *)
    (fun (accum : bexp) ((cnd', e') : bexp * nexp) ->
      let matches = b_and (n_eq e e') cnd' in
      b_and accum (b_not matches))
    cnd (* the condition `cnd` of `e` must be enabled *)
    l

let clamp ~lower ~value ~upper : bexp =
  b_and (n_ge value lower) (n_lt value upper)

let warp_id_var : Variable.t = Variable.from_name "$warp_id"
let warp_id : Exp.nexp = Var warp_id_var

(* This module holds the constraint-generation code. *)
module Constraints = struct
  type t = V1 | V2 | V3 | V4

  let to_string : t -> string = function
    | V1 -> "v1"
    | V2 -> "v2"
    | V3 -> "v3"
    | V4 -> "v4"

  let of_string : string -> t option = function
    | "V1" | "v1" -> Some V1
    | "V2" | "v2" -> Some V2
    | "V3" | "v3" -> Some V3
    | "V4" | "v4" -> Some V4
    | _ -> None

  let default : t = V4
  let values : t list = [ V1; V2; V3; V4 ]

  let tids (cfg : Config.t) : Variable.Set.t =
    Variable.tid_list
    |> List.filter (fun x -> not (Config.is_warp_uniform x cfg))
    |> Variable.Set.of_list

  (* We could revisit this idea with native support for tuples:
     https://stackoverflow.com/questions/39692790/ *)
  (* Constraint generation 1.0: add constraints to threadIdx *)
  module V1_3Gen = struct
    let unique_tid_constraint_1 (thread_ids : nexp list) : bexp =
      (* Generate pairwise inequality constraints: t1 != t2 && t1 != t3 && ... *)
      thread_ids
      |> List.fold_left
           (fun (visited, acc) tid ->
             (* tid is dissimilar from all threads in visited *)
             let dissim =
               visited |> List.map (fun t -> n_neq tid t) |> Exp.b_and_ex
             in
             (* add another visited, accumulate dissimilarity *)
             (tid :: visited, b_and dissim acc))
           ([], b_true)
      |> snd (* ignore the list of visited tids *)

    let unique_tid_constraint_2 (thread_ids : nexp list) : bexp =
      (* Use the built-in distinct primitive *)
      Distinct thread_ids

    let unique_tid_constraint_3 (thread_ids : nexp list) : bexp =
      (* Create chain of less-than constraints: tid0 < tid1 < tid2 < ... < tid(n-1) *)
      let rec make_chain = function
        | [] | [ _ ] -> b_true
        | tid1 :: tid2 :: rest ->
            b_and (n_lt tid1 tid2) (make_chain (tid2 :: rest))
      in
      make_chain thread_ids

    (* All threads must belong to the same warp *)
    let same_warp_constraint (cfg : Config.t) (warp_ids : nexp Array.t) : bexp =
      List.init (cfg.threads_per_warp - 1) (fun i ->
          let wid_i = warp_ids.(i) in
          let wid_next = warp_ids.(i + 1) in
          n_eq wid_i wid_next)
      |> b_and_ex

    let make (version : t) (cfg : Config.t) : bexp =
      (* First constraint: tid_1 != tid_2 != ... != tid_n *)
      let thread_ids : nexp list =
        (* 1. only generate variables for warp divergent tid *)
        (* 2. use cfg.block_dim instead of variable blockDim *)
        let tid_x =
          (* threadIdx.x *)
          if Config.tid_x_is_warp_uniform cfg then Num 0 else Var Variable.tid_x
        in
        let tid_y =
          (* blockDim.x * threadIdx.y *)
          if Config.tid_y_is_warp_uniform cfg then Num 0
          else n_mult (Var Variable.tid_y) (Num cfg.block_dim.x)
        in
        let tid_z =
          (* blockDim.x * blockDim.y * threadIdx.z *)
          if Config.tid_z_is_warp_uniform cfg then Num 0
          else
            n_mult (Var Variable.tid_z)
              (Num (cfg.block_dim.x * cfg.block_dim.y))
        in
        (* n_plus elides (Num 0) from the expression *)
        n_plus tid_x (n_plus tid_y tid_z) (* tid_x + tid_y + tid_z *)
        |> Proj.n_split cfg.threads_per_warp (Config.warp_divergent_tid_set cfg)
      in
      (* 3 different versions generate equivalent unique_tid constraints *)
      let unique_tid =
        match version with
        | V1 -> unique_tid_constraint_1
        | V2 -> unique_tid_constraint_2
        | V3 -> unique_tid_constraint_3
        | V4 ->
            failwith ("Internal error: unexpected version " ^ to_string version)
      in
      let c1 = unique_tid thread_ids in
      (* Second constraint: tid / threads_per_warp *)
      let warp_ids =
        thread_ids (* <- crucially, we reuse thread_ids *)
        |> List.map (fun tid -> n_div tid (Num cfg.threads_per_warp))
        |> Array.of_list
      in
      let c2 = same_warp_constraint cfg warp_ids in
      b_and c1 c2
  end

  (* Constraint generation 2.0: define threadIdx directly *)
  module V4Gen = struct
    let make (cfg : Config.t) : bexp =
      if Config.total_threads_per_block cfg <= cfg.threads_per_warp then
        (* Single warp: directly calculate tid values from uid *)
        let thread_constraints =
          List.init cfg.threads_per_warp (fun uid ->
              let suffix = string_of_int uid in

              (* Calculate tid_x, tid_y, tid_z directly from uid *)
              let constraints = [] in

              (* tid_x = uid % block_dim.x *)
              let constraints =
                if Config.tid_x_is_warp_divergent cfg then
                  let tid_x = proj ~suffix Variable.tid_x in
                  let tid_x_val = uid mod cfg.block_dim.x in
                  n_eq (Var tid_x) (Num tid_x_val) :: constraints
                else constraints
              in

              (* tid_y = (uid / block_dim.x) % block_dim.y *)
              let constraints =
                if Config.tid_y_is_warp_divergent cfg then
                  let tid_y = proj ~suffix Variable.tid_y in
                  let tid_y_val = uid / cfg.block_dim.x mod cfg.block_dim.y in
                  n_eq (Var tid_y) (Num tid_y_val) :: constraints
                else constraints
              in

              (* tid_z = uid / (block_dim.x * block_dim.y) *)
              let constraints =
                if Config.tid_z_is_warp_divergent cfg then
                  let tid_z = proj ~suffix Variable.tid_z in
                  let tid_z_val = uid / (cfg.block_dim.x * cfg.block_dim.y) in
                  n_eq (Var tid_z) (Num tid_z_val) :: constraints
                else constraints
              in

              b_and_ex constraints)
        in

        b_and_ex thread_constraints
      else
        (* Hoisted: determine which coordinates need constraints *)
        let tid_generators =
          [
            ( Variable.tid_x,
              fun thread_id ->
                if Config.tid_x_is_last_warp_divergent cfg then thread_id
                else n_mod thread_id (Num cfg.block_dim.x) );
            ( Variable.tid_y,
              fun thread_id ->
                if Config.tid_y_is_last_warp_divergent cfg then
                  n_div thread_id (Num cfg.block_dim.x)
                else
                  n_mod
                    (n_div thread_id (Num cfg.block_dim.x))
                    (Num cfg.block_dim.y) );
            ( Variable.tid_z,
              fun thread_id ->
                n_div thread_id (Num (cfg.block_dim.x * cfg.block_dim.y)) );
          ]
          |> List.filter (fun (var, _) -> Config.is_warp_divergent var cfg)
        in

        (* Generate constraints for each thread *)
        let thread_constraints =
          List.init cfg.threads_per_warp (fun uid ->
              let suffix = string_of_int uid in
              (* Thread id within a block *)
              let tid_expr =
                n_plus
                  (n_mult (Var warp_id_var) (Num cfg.threads_per_warp))
                  (Num uid)
              in

              tid_generators
              |> List.map (fun (tid_var, make_expr) ->
                  (* tid for a certain suffix *)
                  let tid_var = proj ~suffix tid_var in
                  n_eq (Var tid_var) (make_expr tid_expr))
              |> b_and_ex)
        in

        (* Add warp_id bounds constraint: 0 <= warp_id < total_warps *)
        let warp_bounds =
          clamp ~lower:(Num 0) ~value:(Var warp_id_var)
            ~upper:(Num (Config.total_warps cfg))
        in

        b_and_ex (warp_bounds :: thread_constraints)
  end

  let distinct (cfg : Config.t) : t -> bexp = function
    | (V1 | V2 | V3) as s -> V1_3Gen.make s cfg
    | V4 -> V4Gen.make cfg
end

open State.Syntax

let var (i : int) (x : Variable.t) : Variable.t =
  proj ~suffix:(string_of_int i) x

module Vectorizer = struct
  type 'a t = Vector of 'a list | Scalar of 'a

  let of_bexp (count : int) (locals : Variable.Set.t) (e : Exp.bexp) :
      Exp.bexp t =
    if b_intersects locals e then Vector (Proj.b_split count locals e)
    else Scalar e

  let of_nexp (count : int) (locals : Variable.Set.t) (e : Exp.nexp) :
      Exp.nexp t =
    if n_intersects locals e then Vector (Proj.n_split count locals e)
    else Scalar e

  let b_split (count : int) (locals : Variable.Set.t) (e : bexp) : bexp t =
    Vector (Proj.b_split count locals e)

  let n_split (count : int) (locals : Variable.Set.t) (e : nexp) : nexp t =
    Vector (Proj.n_split count locals e)

  let to_list (count : int) : 'a t -> 'a list = function
    | Scalar s -> List.init count (fun _ -> s)
    | Vector l -> l

  (* Convert a vector to a boolean expression *)
  let to_bexp : bexp t -> bexp = function
    | Scalar s -> s
    | Vector l -> Exp.b_and_ex l

  (* Vectorize a boolean expression: e -> e$1 && e$2 && .. *)
  let vectorize (count : int) (locals : Variable.Set.t) (e : bexp) : bexp =
    of_bexp count locals e |> to_bexp

  let rec b_and (e1 : bexp t) (e2 : bexp t) : bexp t =
    match (e1, e2) with
    | Vector e1, Vector e2 ->
        if List.length e1 <> List.length e2 then
          raise (failwith "unexpected different lengths");
        Vector (Common.zip e1 e2 |> List.map (fun (e1, e2) -> Exp.b_and e1 e2))
    | Scalar e1, Scalar e2 -> Scalar (Exp.b_and e1 e2)
    | Scalar e1, Vector e2 ->
        let n = List.length e2 in
        b_and (Vector (List.init n (fun _ -> e1))) (Vector e2)
    | Vector e1, Scalar e2 ->
        let n = List.length e1 in
        b_and (Vector e1) (Vector (List.init n (fun _ -> e2)))

  let to_string (f : 'a -> string) : 'a t -> string = function
    | Scalar x -> Printf.sprintf "Scalar[%s]" (f x)
    | Vector xs ->
        Printf.sprintf "Vector[%s]" (xs |> List.map f |> String.concat ", ")
end

type t = {
  locals : Variable.Set.t;
  globals : Variable.Set.t;
  assumptions : bexp;
  active_threads : bexp Vectorizer.t;
  config : Config.t;
  generator : Constraints.t;
}

type 'a state = (t, 'a) State.t

let of_bexp (b : bexp) (st : t) : bexp Vectorizer.t =
  Vectorizer.of_bexp st.config.threads_per_warp st.locals b

let add_assumption (b : bexp Vectorizer.t) (st : t) : t =
  let b = Vectorizer.to_bexp b in
  { st with assumptions = Exp.b_and st.assumptions b }

let extract_assumptions (b : Exp.bexp) : bexp state =
  State.update_return (fun (st : t) ->
      let with_locals, with_globals =
        b |> Exp.b_and_split |> List.partition (b_intersects st.locals)
      in
      let with_locals = Exp.b_and_ex with_locals in
      let with_globals = Exp.b_and_ex with_globals in
      (add_assumption (Scalar with_globals) st, with_locals))

let add_active_threads_vec (e : bexp Vectorizer.t) (st : t) : t =
  { st with active_threads = Vectorizer.b_and e st.active_threads }

let add_active_threads (active_threads : bexp) (st : t) : t =
  let st, active_threads = extract_assumptions active_threads st in
  add_active_threads_vec (of_bexp active_threads st) st

(* e -> [e$0; e$1; ...] *)
let n_split (e : nexp) (st : t) : nexp list =
  Proj.n_split st.config.threads_per_warp st.locals e

let to_string (st : t) : string =
  Printf.sprintf
    "SymbolicMetric {\n\
    \  config: %s\n\
    \  generator: %s\n\
    \  locals: [%s]\n\
    \  globals: [%s]\n\
    \  active_threads: %s\n\
    \  assumptions: %s\n\
     }"
    (Config.to_string st.config)
    (Constraints.to_string st.generator)
    (Variable.set_to_string st.locals)
    (Variable.set_to_string st.globals)
    (Vectorizer.to_string Exp.b_to_string st.active_threads)
    (Exp.b_to_string st.assumptions)

let add_architecture_constraints (st : t) : t =
  (* tidx < bdim.x && bidx < gdim.x && ... *)
  (*let b_split = Vectorizer.b_split st.config.threads_per_warp st.locals in*)
  let bdim = st.config.block_dim in
  let gdim = st.config.grid_dim in
  (* Generate runtime constraints on demand *)
  [
    (Variable.tid_x, bdim.x);
    (Variable.tid_y, bdim.y);
    (Variable.tid_z, bdim.z);
    (Variable.bid_x, gdim.x);
    (Variable.bid_y, gdim.y);
    (Variable.bid_z, gdim.z);
  ]
  |> List.fold_left
       (fun st (x, dim) : t ->
         if Variable.Set.mem x st.locals || Variable.Set.mem x st.globals then
           let unif_warps, rem_threads =
             Config.divide_total_threads_per_warp st.config
           in
           let e = b_and (n_le (Num 0) (Var x)) (n_lt (Var x) (Num dim)) in
           if rem_threads > 0 && unif_warps = 0 then
             let e = of_bexp e st in
             add_active_threads_vec e st
           else if rem_threads = 0 && unif_warps > 0 then
             let e = of_bexp e st in
             add_assumption e st
           else
             let st =
               if rem_threads > 0 then
                 let e =
                   of_bexp (b_impl (n_eq warp_id (Num unif_warps)) e) st
                 in
                 add_active_threads_vec e st (* only assumptions *)
               else st
             in
             if unif_warps > 0 then
               let e = of_bexp (b_impl (n_lt warp_id (Num unif_warps)) e) st in
               add_assumption e st
             else st
         else st)
       st

(*
  TODO: This function doesn't yet use st.locals or st.globals. The distinct
  constraint generation is self-contained within Constraints.distinct.
  Consider refactoring to use user-provided locals/globals.
*)
let distinct_constraints (st : t) : bexp Vectorizer.t =
  (* distinct(tidx$0, tidx$1, tidx$2, ...) *)
  Scalar (Constraints.distinct st.config st.generator)

let make (generator : Constraints.t) (config : Config.t)
    (locals : Variable.Set.t) (globals : Variable.Set.t) : t =
  let locals =
    Variable.Set.union locals (Config.warp_divergent_tid_set config)
  in
  let total_threads = Config.total_threads_per_block config in
  let config =
    { config with threads_per_warp = min config.threads_per_warp total_threads }
  in
  let st : t =
    {
      assumptions = b_true;
      active_threads = Scalar b_true;
      locals;
      globals;
      generator;
      config;
    }
  in
  st |> add_architecture_constraints |> add_assumption (distinct_constraints st)

let print_optimize (pre : bexp) (formula : nexp) : unit =
  let pre =
    pre |> Exp.b_and_split |> List.map Exp.b_to_string
    |> String.concat "\n    && "
  in
  let formula =
    formula
    |> Exp.n_bin_split N_binary.Plus
    |> List.map Exp.n_to_string |> String.concat "\n    + "
  in
  prerr_endline
    (Printf.sprintf "optimize {\n  pre: %s\n  cost: %s\n}" pre formula)

(** Optimizes a formula *)
let optimize ?(verbose = false) ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) ?(default_cost = 0)
    ?(timeout = 0) (formula : nexp) (st : t) : (int, string) Result.t =
  let module S = (val solver) in
  let pre = st.assumptions in
  (* pre: the generated runtime constraints (eg, tid is unique) *)
  let solve formula : (int, string) Result.t =
    S.optimize_expr ~timeout strategy ~pre formula
    |> Result.map (fun o -> Option.value ~default:default_cost o)
  in
  if verbose then print_optimize pre formula;
  try solve formula
  with Protocols.Gen_z3.Preprocessing_error _ ->
    solve (Predicates.n_inline formula)

let print_prove (pre : bexp) (formula : bexp) : unit =
  let pre =
    pre |> Exp.b_and_split |> List.map Exp.b_to_string
    |> String.concat "\n    && "
  in
  let formula =
    formula |> Exp.b_and_split |> List.map Exp.b_to_string
    |> String.concat "\n    && "
  in
  prerr_endline (Printf.sprintf "prove {\n  pre: %s\n  goal: %s\n}" pre formula)

let prove ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) ?(debug = true)
    ?(verbose = false) ?(tactic : Gen_z3.Tactic.t option = None) (goal : bexp)
    (st : t) : (Gen_z3.Solver.t, string) Result.t =
  let module S = (val solver) in
  let pre = st.assumptions in
  let goal = b_and pre (b_not goal) in
  if verbose then print_prove pre goal;
  match tactic with
  | Some tactic_strategy -> S.solve_with_tactic ~debug tactic_strategy goal
  | None -> S.solve goal

(* Calculates the cost of a metric analysis *)
let cost_of (metric : nexp -> t -> nexp) (index : nexp) : nexp state =
  State.update_return (fun st ->
      (* Add non-negative index constraint *)
      let n_ge_index_0 = of_bexp (n_ge index (Num 0)) st in
      let st = add_assumption n_ge_index_0 st in
      (st, metric index st))

let to_int : bexp -> nexp = function
  | Bool b -> Num (if b then 1 else 0)
  | e -> CastInt e

let encode_count_active_threads (_index : nexp) (st : t) : nexp =
  st.active_threads
  (* count how many threads are active *)
  |> Vectorizer.to_list st.config.threads_per_warp
  (* Count 1 if thread is active *)
  |> List.map to_int
  (* Add all 1s *)
  |> Exp.sum

let optimize_metric (metric : nexp -> t -> nexp) ?(verbose = false)
    ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) ?(timeout = 0)
    (config : Config.t) (locals : Variable.Set.t) (active_threads : bexp)
    (index : nexp) : int option =
  (* Compute free names from active_threads and index *)
  let fns =
    Exp.b_free_names active_threads Variable.Set.empty |> Exp.n_free_names index
  in
  (* Compute globals as: free_names - locals *)
  let globals = Variable.Set.diff fns locals in

  State.run_result
    (let* n = cost_of metric index in
     let* st = State.get in
     return (optimize ~verbose ~strategy ~solver ~default_cost:0 ~timeout n st))
    (make generator config locals globals |> add_active_threads active_threads)
  |> Result.to_option

let count_active_threads = optimize_metric encode_count_active_threads

(* SAT-based cohort counting. Asks Z3: is there a valuation where the
   active-thread count satisfies [predicate count_expr]? Returns:
     - [Sat k] if such a valuation exists, where [k] is the count value
       in the witnessing model;
     - [Unsat] if no such valuation exists;
     - [Unknown] on solver error / timeout.
   Much cheaper than [optimize_metric] when only a witness is needed —
   the optimizer must additionally prove its result is the extremum. *)
type sat_witness = Sat of int | Unsat_w | Unknown_w

let sat_count
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) ?(timeout = 0)
    (config : Config.t) (locals : Variable.Set.t) (active_threads : bexp)
    (predicate : nexp -> bexp) : sat_witness =
  let module S = (val solver) in
  let fns = Exp.b_free_names active_threads Variable.Set.empty in
  let globals = Variable.Set.diff fns locals in
  let st =
    make generator config locals globals |> add_active_threads active_threads
  in
  let count_expr = encode_count_active_threads (Num 0) st in
  (* Syntactic bound on the count: a sum of [threads_per_warp] booleans
     lies in [0, threads_per_warp]. Without this hint, Z3 has to derive
     the bound from the BV-encoded sum, which can take seconds-to-
     minutes on large warp sizes — turning UNSAT proofs of
     [count > threads_per_warp] into a bottleneck. *)
  let count_bounds =
    Exp.b_and
      (n_ge count_expr (Num 0))
      (n_le count_expr (Num st.config.threads_per_warp))
  in
  let goal =
    Exp.b_and (Exp.b_and st.assumptions count_bounds) (predicate count_expr)
  in
  match S.solve_with_int_witness ~timeout goal count_expr with
  | Ok (Some k) -> Sat k
  | Ok None -> Unsat_w
  | Error _ -> Unknown_w

(* SAT("there are [n] pairwise-distinct tids in the block, all
   satisfying [cohort], under [pre]"). Returns the [n] witnessing
   tid triples on SAT, [None] on UNSAT or solver error.

   Used for sub-warp Oversize: pass [n = expected + 1]; a SAT result
   exhibits a configuration where strictly more than [expected]
   threads arrive at the barrier — the bug witness is the [n] tids
   themselves.

   The encoding is small: [n] is typically a sub-warp count plus one
   (e.g. 33 for [bar.sync 0, 32]), not the block size. Distinctness
   is on tid triples — [(x,y,z)] differ in at least one component —
   rather than the heavy [Distinct] over the full block. *)
let sat_n_distinct_in_cohort
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) ?(timeout = 0)
    (config : Config.t) ~(pre : bexp) ~(n : int) (cohort : bexp) :
    (int * int * int) list option =
  if n <= 0 then Some []
  else
    let module S = (val solver) in
    let bdim = config.block_dim in
    let tid_locals = Variable.tid_set in
    let proj_ctx i : Proj.t =
      { suffix = string_of_int i; locals = tid_locals }
    in
    let tid_at i base = proj ~suffix:(string_of_int i) base in
    let triple_at i =
      ( Var (tid_at i Variable.tid_x),
        Var (tid_at i Variable.tid_y),
        Var (tid_at i Variable.tid_z) )
    in
    let mk_instance i =
      let cohort_i = Proj.proj_b cohort (proj_ctx i) in
      let pre_i = Proj.proj_b pre (proj_ctx i) in
      let xi, yi, zi = triple_at i in
      let in_block =
        Exp.b_and_ex
          [
            n_le (Num 0) xi;
            n_lt xi (Num bdim.x);
            n_le (Num 0) yi;
            n_lt yi (Num bdim.y);
            n_le (Num 0) zi;
            n_lt zi (Num bdim.z);
          ]
      in
      Exp.b_and_ex [ pre_i; in_block; cohort_i ]
    in
    let pairwise_distinct =
      let acc = ref [] in
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          let xi, yi, zi = triple_at i in
          let xj, yj, zj = triple_at j in
          acc :=
            Exp.b_or_ex
              [
                Exp.b_not (n_eq xi xj);
                Exp.b_not (n_eq yi yj);
                Exp.b_not (n_eq zi zj);
              ]
            :: !acc
        done
      done;
      Exp.b_and_ex !acc
    in
    let goal =
      Exp.b_and_ex (pairwise_distinct :: List.init n mk_instance)
    in
    let witness_exprs =
      List.init n (fun i ->
          let xi, yi, zi = triple_at i in
          [ xi; yi; zi ])
      |> List.concat
    in
    match S.solve_with_int_witnesses ~timeout goal witness_exprs with
    | Ok (Some vs) when List.length vs = 3 * n ->
        let triples =
          List.init n (fun i ->
              match
                ( List.nth vs (3 * i),
                  List.nth vs ((3 * i) + 1),
                  List.nth vs ((3 * i) + 2) )
              with
              | Some x, Some y, Some z -> Some (x, y, z)
              | _ -> None)
        in
        if List.for_all Option.is_some triples then
          Some (List.map Option.get triples)
        else None
    | _ -> None

let encode_ua (index : nexp) (st : t) : nexp =
  let index = n_div index (Num (Config.memory_segments_bits st.config)) in
  let index = n_split index st in
  let active_threads =
    st.active_threads |> Vectorizer.to_list st.config.threads_per_warp
  in
  Common.zip active_threads index
  (* for each replicated index *)
  |> List.fold_left
       (fun ((accum, visited) : nexp * (bexp * nexp) list) (p : bexp * nexp) ->
         (* cond_dissimilar p visited + accum *)
         (n_plus (to_int (cond_dissimilar p visited)) accum, p :: visited))
       (* total cost = 0, visited = [] *)
       (Num 0, [])
  (* take only the accumulated value *)
  |> fst

let ua = optimize_metric encode_ua

(* Inline ua() function calls in expressions *)
let rec n_inline_cost : nexp -> nexp state = function
  | NCall ("ua", index) -> cost_of encode_ua index
  | NCall ("count_active", index) -> cost_of encode_count_active_threads index
  | (Var _ | Num _) as e -> return e
  | Other e ->
      let* e' = n_inline_cost e in
      return (Other e')
  | Binary (op, e1, e2) ->
      let* e1' = n_inline_cost e1 in
      let* e2' = n_inline_cost e2 in
      return (Binary (op, e1', e2'))
  | Unary (op, e) ->
      let* e' = n_inline_cost e in
      return (Unary (op, e'))
  | NCall (name, e) ->
      let* e' = n_inline_cost e in
      return (NCall (name, e'))
  | NIf (b, e1, e2) ->
      let* b' = b_inline_cost b in
      let* e1' = n_inline_cost e1 in
      let* e2' = n_inline_cost e2 in
      return (NIf (b', e1', e2'))
  | CastInt b ->
      let* b' = b_inline_cost b in
      return (CastInt b')

and b_inline_cost : bexp -> bexp state = function
  | CastBool e ->
      let* e' = n_inline_cost e in
      return (CastBool e')
  | Pred (x, n) ->
      let* n' = n_inline_cost n in
      return (Pred (x, n'))
  | Bool _ as b -> return b
  | BNot b ->
      let* b' = b_inline_cost b in
      return (BNot b')
  | BRel (o, b1, b2) ->
      let* b1' = b_inline_cost b1 in
      let* b2' = b_inline_cost b2 in
      return (BRel (o, b1', b2'))
  | NRel (o, n1, n2) ->
      let* n1' = n_inline_cost n1 in
      let* n2' = n_inline_cost n2 in
      return (NRel (o, n1', n2'))
  | Distinct exprs ->
      let* exprs' = State.list_map n_inline_cost exprs in
      return (Distinct exprs')

module ProofResult = struct
  type t =
    | Proved (* Theorem successfully proven *)
    | Counterexample of Z3.Model.model (* Found counterexample with model *)

  let to_string : t -> string = function
    | Proved -> "Proved"
    | Counterexample model -> "Counterexample: " ^ Z3.Model.to_string model

  let of_solver : Gen_z3.Solver.t -> t = function
    | Sat e -> Counterexample e
    | Unsat -> Proved

  let is_success : t -> bool = function
    | Proved -> true
    | Counterexample _ -> false
end

module TheoremResult = struct
  type t = OptimizationResult of int | ProofResult of ProofResult.t

  let to_string : t -> string = function
    | OptimizationResult e -> Printf.sprintf "Optimization: %d" e
    | ProofResult e -> "Proof: " ^ ProofResult.to_string e

  let proved : t = ProofResult Proved
  let counterexample (m : Z3.Model.model) : t = ProofResult (Counterexample m)
  let optimization (i : int) : t = OptimizationResult i

  let of_solver (e : Gen_z3.Solver.t) : t =
    ProofResult (ProofResult.of_solver e)

  let is_success : t -> bool = function
    | OptimizationResult _ ->
        true (* Optimization always succeeds if it returns a value *)
    | ProofResult r -> ProofResult.is_success r
end

module Theorem = struct
  module Goal = struct
    type t =
      | Optimize of { strategy : Gen_z3.Optimizer.Strategy.t; expr : Exp.nexp }
      | Prop of Exp.bexp

    let subst (kvs: Subst.Vars.t) : t -> t = function
      | Optimize o -> Optimize { o with expr = Subst.ReplaceVars.n_subst kvs o.expr }
      | Prop e -> Prop (Subst.ReplaceVars.b_subst kvs e)

    let to_string : t -> string = function
      | Optimize { strategy = s; expr = e } ->
          Gen_z3.Optimizer.Strategy.to_string s ^ " " ^ Exp.n_to_string e
      | Prop e -> "prop " ^ Exp.b_to_string e
  end

  type context = t

  type t = {
    cfg : Config.t;
    locals : Variable.Set.t;
    globals : Variable.Set.t;
    active_threads : bexp;
    assumptions : bexp;
    goals : Goal.t list;
  }

  let config_subst (e: t) : Subst.Vars.t =
    [
      Variable.from_name "$config.threads_per_warp", Num e.cfg.threads_per_warp;
      Variable.from_name "$config.bank_count", Num e.cfg.bank_count;
    ]
    |> Subst.Vars.make

  let inline_config (e : t) : t =
    let kvs = config_subst e in
    let b_subst = Subst.ReplaceVars.b_subst kvs in
    {
      e with
      active_threads = b_subst e.active_threads;
      assumptions = b_subst e.assumptions;
      goals = List.map (Goal.subst kvs) e.goals;
    }

  (* Execute all goals in a theorem *)
  let execute ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER))
      ?(debug = true) ?(verbose = false) ?(generator = Constraints.default)
      ?(tactic : Gen_z3.Tactic.t option = None) (thm : t) :
      (TheoremResult.t, string) Result.t list =
    let thm = inline_config thm in
    let st : context =
      make generator thm.cfg thm.locals thm.globals
      (* set active threads *)
      |> add_active_threads thm.active_threads
      |> fun st -> add_assumption (of_bexp thm.assumptions st) st
    in
    thm.goals
    |> List.map (fun g ->
        State.run_result
          (match g with
          | Goal.Optimize { strategy; expr } ->
              let* expr = n_inline_cost expr in
              let* st = State.get in
              let r = optimize ~verbose ~strategy ~solver expr st in
              return (r |> Result.map TheoremResult.optimization)
          | Prop g ->
              let* g = b_inline_cost g in
              let* st = State.get in
              let r = prove ~solver ~debug ~verbose ~tactic g st in
              return (r |> Result.map TheoremResult.of_solver))
          st)

  let to_string (thm : t) : string =
    Printf.sprintf
      "config: %s\n\
       locals: [%s]\n\
       globals: [%s]\n\
       active_threads: %s;\n\
       assumptions: %s;\n\
       ⊢ %s"
      (Config.to_string thm.cfg)
      (Variable.set_to_string thm.locals)
      (Variable.set_to_string thm.globals)
      (b_to_string thm.active_threads)
      (b_to_string thm.assumptions)
      (List.map Goal.to_string thm.goals |> String.concat "\n")

  let b_to_string ?(indent = "    ") (e : Exp.bexp) : string =
    e |> Exp.b_and_split |> List.map Exp.b_to_string
    |> String.concat ("\n" ^ indent ^ "&& ")
end
