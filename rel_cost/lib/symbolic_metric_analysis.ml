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
  let run (count : int) (locals : Variable.Set.t) (f : t -> 'a) : 'a list =
    let rec loop (idx : int) (accum : 'a list) : 'a list =
      if idx < 0 then accum
      else
        let ctx = { suffix = string_of_int idx; locals } in
        loop (idx - 1) (f ctx :: accum)
    in
    loop (count - 1) []

  (* Project a condition and an index *)
  let run_pair (cfg : Config.t) (locals : Variable.Set.t) (cond : bexp)
      (index : nexp) : (bexp * nexp) list =
    run cfg.threads_per_warp locals (fun ctx ->
        (proj_b cond ctx, proj_n index ctx))
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
        |> Proj.proj_n
        |> Proj.run cfg.threads_per_warp (Config.warp_divergent_tid_set cfg)
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
      let warp_id_var = Variable.from_name "$warp_id" in

      (* Hoisted: determine which coordinates need constraints *)
      let tid_generators =
        [
          ( Variable.tid_x,
            fun thread_id -> n_mod thread_id (Num cfg.block_dim.x) );
          ( Variable.tid_y,
            fun thread_id ->
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

  let to_architecture (cfg : Config.t) (strategy : t) : Architecture.Defaults.t
      =
    let globals =
      Variable.Set.empty
      |> Variable.Set.union Variable.bid_set
      |> Variable.Set.union Variable.bdim_set
      |> Variable.Set.union Variable.gdim_set
      |> Params.from_set C_type.unsigned_int
    in
    let locals =
      Config.warp_divergent_tid_set cfg |> Params.from_set C_type.unsigned_int
    in
    let distinct =
      match strategy with
      | V1 | V2 | V3 -> V1_3Gen.make strategy cfg
      | V4 -> V4Gen.make cfg
    in
    { globals; locals; distinct }

  let to_bexp (cfg : Config.t) (strategy : t) : bexp =
    strategy
    |> to_architecture cfg
    |> Architecture.Defaults.to_dyn_bexp ~bdim:cfg.block_dim ~gdim:cfg.grid_dim
end

let print_optimize (pre:bexp) (formula:nexp) : unit =
  let pre =
    pre
    |> Exp.b_and_split
    |> List.map Exp.b_to_string
    |> String.concat "\n    && "
  in
  let formula =
    formula
    |> Exp.n_bin_split N_binary.Plus
    |> List.map Exp.n_to_string
    |> String.concat "\n    + "
  in
  prerr_endline (Printf.sprintf
    "optimize {\n  pre: %s\n  cost: %s\n}" pre formula)

(* Optimizes an encoding *)
let run_encoding ?(verbose=false) ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (cfg : Config.t)
    (formula : nexp) : int option =
  let module S = (val solver) in
  let pre = Constraints.to_bexp cfg generator in
  let solve formula =
    S.optimize_expr strategy ~pre formula |> Result.to_option
  in
  if verbose then print_optimize pre formula;
  try solve formula
  with Protocols.Gen_z3.Preprocessing_error _ ->
    solve (Predicates.n_inline formula)

let encode_count_active_threads (cfg : Config.t) (locals : Variable.Set.t)
    (cond : bexp) : nexp =
  let locals =
    Variable.Set.union
      (Variable.Set.diff locals Variable.tid_set)
      (Config.warp_divergent_tid_set cfg)
  in
  (* replicate index per each thread *)
  Proj.run cfg.threads_per_warp locals (fun ctx -> Proj.proj_b cond ctx)
  (* for each replicated index *)
  |> List.fold_left
       (fun (accum : nexp) (cond : bexp) ->
         n_plus (n_if cond (Num 1) (Num 0)) accum)
       (* total cost = 0, visited = [] *)
       (Num 0)

let count_active_threads ?(verbose=false) ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (cfg : Config.t)
    (locals : Variable.Set.t) (cond : bexp) : int option =
  cond
  |> encode_count_active_threads cfg locals
  |> run_encoding ~verbose ~strategy ~generator ~solver cfg

let encode_ua (cfg : Config.t) (locals : Variable.Set.t) (cond : bexp)
    (index : nexp) : nexp =
  let locals =
    Variable.Set.union
      (Variable.Set.diff locals Variable.tid_set)
      (Config.warp_divergent_tid_set cfg)
  in
  let index = n_div index (Num (Config.memory_segments_bits cfg)) in
  (* replicate index per each thread *)
  Proj.run cfg.threads_per_warp locals (fun ctx ->
      (Proj.proj_b cond ctx, Proj.proj_n index ctx))
  (* for each replicated index *)
  |> List.fold_left
       (fun ((accum, visited) : nexp * (bexp * nexp) list) (p : bexp * nexp) ->
         ( n_plus (n_if (cond_dissimilar p visited) (Num 1) (Num 0)) accum,
           p :: visited ))
       (* total cost = 0, visited = [] *)
       (Num 0, [])
  |>
  (* take only the accumulated value *)
  fst

let ua ?(verbose=true) ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (cfg : Config.t)
    (locals : Variable.Set.t) (cond : bexp) (index : nexp) : int option =
  encode_ua cfg locals cond index
  |> run_encoding ~verbose ~strategy ~generator ~solver cfg

module ProofResult = struct
  type t =
    | Proved (* Theorem successfully proven *)
    | Counterexample of Z3.Model.model (* Found counterexample with model *)
    | Unknown of string (* Solver couldn't determine *)

  let to_string : t -> string = function
    | Proved -> "Proved"
    | Counterexample model -> "Counterexample: " ^ Z3.Model.to_string model
    | Unknown msg -> "Unknown: " ^ msg

  let is_success : t -> bool = function
    | Proved -> true
    | Counterexample _ | Unknown _ -> false
end

module Theorem = struct
  type t = {
    cfg : Config.t;
    locals : Variable.Set.t;
    local_context : bexp;
    global_context : bexp;
    index : nexp;
    rel : N_rel.t;
    expected_cost : nexp;
  }

  let to_string (thm : t) : string =
    Printf.sprintf
      "config: %s\n\
       locals: [%s]\n\
       local_context: %s;\n\
       global_context: %s;\n\
       ⊢ cost(%s) %s %s"
      (Config.to_string thm.cfg)
      (Variable.set_to_string thm.locals)
      (b_to_string thm.local_context)
      (b_to_string thm.global_context)
      (n_to_string thm.index) (N_rel.to_string thm.rel)
      (n_to_string thm.expected_cost)

  let prove ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER))
      ?(debug = true) ?(generator = Constraints.default)
      ?(tactic : Gen_z3.Tactic.t option = None) (thm : t) : ProofResult.t =
    let module S = (val solver) in
    let given = encode_ua thm.cfg thm.locals thm.local_context thm.index in
    let goal = NRel (thm.rel, given, thm.expected_cost) in
    (* Prove that NOT(actual_cost comparison expected_cost) is UNSAT *)
    let constraint_system =
      b_and_ex
        [
          thm.global_context; Constraints.to_bexp thm.cfg generator; b_not goal;
        ]
    in
    let result =
      match tactic with
      | Some tactic_strategy ->
          (* Use tactic-based solving with debug mode enabled *)
          S.solve_with_tactic ~debug tactic_strategy constraint_system
      | None ->
          (* Use default solver *)
          S.solve constraint_system
    in
    match result with
    | Unsat -> ProofResult.Proved
    | Sat model -> ProofResult.Counterexample model
    | Unknown msg -> ProofResult.Unknown msg

  let optimize_cost ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
      ?(generator = Constraints.default) (thm : t) : int option =
    ua ~strategy ~generator thm.cfg thm.locals
      (b_and thm.local_context thm.global_context)
      thm.index
end
