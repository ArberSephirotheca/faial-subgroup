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

  let b_split (cfg : Config.t) (locals : Variable.Set.t) (e : bexp) : bexp list
      =
    e |> proj_b |> split cfg.threads_per_warp locals

  let n_split (cfg : Config.t) (locals : Variable.Set.t) (e : nexp) : nexp list
      =
    e |> proj_n |> split cfg.threads_per_warp locals

  let extract_global (locals : Variable.Set.t) (b : Exp.bexp) :
      Exp.bexp * Exp.bexp =
    b |> Exp.b_and_split |> List.partition (b_intersects locals)
    |> fun (l, r) -> (Exp.b_and_ex l, Exp.b_and_ex r)
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
        |> Proj.n_split cfg (Config.warp_divergent_tid_set cfg)
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
    strategy |> to_architecture cfg
    |> Architecture.Defaults.to_dyn_bexp ~bdim:cfg.block_dim ~gdim:cfg.grid_dim
end

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
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (cfg : Config.t)
    ((pre, formula) : bexp * nexp) : int option =
  let module S = (val solver) in
  (* pre: the generated runtime constraints (eg, tid is unique) *)
  let pre = b_and (Constraints.to_bexp cfg generator) pre in
  let solve formula =
    S.optimize_expr strategy ~pre formula |> Result.to_option
  in
  if verbose then print_optimize pre formula;
  try solve formula
  with Protocols.Gen_z3.Preprocessing_error _ ->
    solve (Predicates.n_inline formula)

(* Calculates the cost of a metric analysis *)
let cost (cfg : Config.t) (locals : Variable.Set.t)
    (metric : bexp -> nexp -> nexp) (active_threads : bexp) (index : nexp) :
    bexp * nexp =
  let active_threads, cond = Proj.extract_global locals active_threads in
  (* Add non-negative index constraint *)
  let valid_index : bexp =
    n_ge index (Num 0) |> Proj.b_split cfg locals |> Exp.b_and_ex
  in
  (valid_index, n_if (b_and cond b_true) (metric active_threads index) (Num 0))

(** Optimizes an encoding *)
let optimize_cost ?(verbose = false)
    ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (cfg : Config.t)
    (locals : Variable.Set.t) (metric : bexp -> nexp -> nexp)
    (active_threads : bexp) (index : nexp) : int option =
  index
  |> cost cfg locals metric active_threads
  |> optimize ~verbose ~strategy ~generator ~solver cfg

let encode_count_active_threads (cfg : Config.t) (locals : Variable.Set.t)
    (cond : bexp) (_index : nexp) : nexp =
  (* replicate index per each thread *)
  cond |> Proj.b_split cfg locals
  (* Count 1 if thread is active *)
  |> List.map (fun e -> n_if e (Num 1) (Num 0))
  (* Add all 1s *)
  |> Exp.sum

let count_active_threads ?(verbose = false)
    ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (cfg : Config.t)
    (locals : Variable.Set.t) (cond : bexp) (index : nexp) : int option =
  optimize_cost ~verbose ~strategy ~generator ~solver cfg locals
    (encode_count_active_threads cfg locals)
    cond index

let encode_ua (cfg : Config.t) (locals : Variable.Set.t) (active_threads : bexp)
    (index : nexp) : nexp =
  let index = n_div index (Num (Config.memory_segments_bits cfg)) in
  (* replicate index per each thread *)
  Common.zip
    (Proj.b_split cfg locals active_threads)
    (Proj.n_split cfg locals index)
  (* for each replicated index *)
  |> List.fold_left
       (fun ((accum, visited) : nexp * (bexp * nexp) list) (p : bexp * nexp) ->
         ( n_plus (n_if (cond_dissimilar p visited) (Num 1) (Num 0)) accum,
           p :: visited ))
       (* total cost = 0, visited = [] *)
       (Num 0, [])
  (* take only the accumulated value *)
  |> fst

let ua ?(verbose = true) ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.default)
    ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (cfg : Config.t)
    (locals : Variable.Set.t) (active_threads : bexp) (index : nexp) :
    int option =
  optimize_cost ~verbose ~strategy ~generator ~solver cfg locals
    (encode_ua cfg locals) active_threads index

module ProofResult = struct
  type t =
    | Proved (* Theorem successfully proven *)
    | Counterexample of Z3.Model.model (* Found counterexample with model *)

  let to_string : t -> string = function
    | Proved -> "Proved"
    | Counterexample model -> "Counterexample: " ^ Z3.Model.to_string model

  let is_success : t -> bool = function
    | Proved -> true
    | Counterexample _ -> false
end

module TheoremResult = struct
  type t = OptimizationResult of int | ProofResult of ProofResult.t

  let to_string : t -> string = function
    | OptimizationResult value -> Printf.sprintf "Optimization: %d" value
    | ProofResult result -> "Proof: " ^ ProofResult.to_string result

  let is_success : t -> bool = function
    | OptimizationResult _ ->
        true (* Optimization always succeeds if it returns a value *)
    | ProofResult result -> ProofResult.is_success result
end

module Theorem = struct
  module Goal = struct
    type t =
      | Optimize of { strategy : Gen_z3.Optimizer.Strategy.t; expr : Exp.nexp }
      | Prop of Exp.bexp

    let to_string : t -> string = function
      | Optimize { strategy = s; expr = e } ->
          Gen_z3.Optimizer.Strategy.to_string s ^ " " ^ Exp.n_to_string e
      | Prop e -> "prop " ^ Exp.b_to_string e
  end

  type t = {
    cfg : Config.t;
    locals : Variable.Set.t;
    active_threads : bexp;
    assumptions : bexp;
    goals : Goal.t list;
  }

  let cost (thm : t) (metric : bexp -> nexp -> nexp) :
      nexp -> (bexp, nexp) State.t =
   fun index ->
    let constraint_part, expression_part =
      cost thm.cfg thm.locals metric thm.active_threads index
    in
    let open State.Syntax in
    let* current_constraints = State.get in
    let* () = State.put (b_and current_constraints constraint_part) in
    State.return expression_part

  (* Inline ua() function calls in expressions *)
  let rec n_inline_cost (thm : t) : nexp -> (bexp, nexp) State.t =
    let open State.Syntax in
    function
      | NCall ("ua", index) -> cost thm (encode_ua thm.cfg thm.locals) index
      | (Var _ | Num _) as e -> return e
      | Other e ->
          let* e' = n_inline_cost thm e in
          return (Other e')
      | Binary (op, e1, e2) ->
          let* e1' = n_inline_cost thm e1 in
          let* e2' = n_inline_cost thm e2 in
          return (Binary (op, e1', e2'))
      | Unary (op, e) ->
          let* e' = n_inline_cost thm e in
          return (Unary (op, e'))
      | NCall (name, e) ->
          let* e' = n_inline_cost thm e in
          return (NCall (name, e'))
      | NIf (b, e1, e2) ->
          let* b' = b_inline_cost thm b in
          let* e1' = n_inline_cost thm e1 in
          let* e2' = n_inline_cost thm e2 in
          return (NIf (b', e1', e2'))
      | CastInt b ->
          let* b' = b_inline_cost thm b in
          return (CastInt b')

  and b_inline_cost (thm : t) : bexp -> (bexp, bexp) State.t =
    let open State.Syntax in
    function
      | CastBool e ->
          let* e' = n_inline_cost thm e in
          return (CastBool e')
      | Pred (x, n) ->
          let* n' = n_inline_cost thm n in
          return (Pred (x, n'))
      | Bool _ as b -> return b
      | BNot b ->
          let* b' = b_inline_cost thm b in
          return (BNot b')
      | BRel (o, b1, b2) ->
          let* b1' = b_inline_cost thm b1 in
          let* b2' = b_inline_cost thm b2 in
          return (BRel (o, b1', b2'))
      | NRel (o, n1, n2) ->
          let* n1' = n_inline_cost thm n1 in
          let* n2' = n_inline_cost thm n2 in
          return (NRel (o, n1', n2'))
      | Distinct exprs ->
          let* exprs' = State.list_map (n_inline_cost thm) exprs in
          return (Distinct exprs')

  let to_string (thm : t) : string =
    Printf.sprintf
      "config: %s\nlocals: [%s]\nactive_threads: %s;\nassumptions: %s;\n⊢ %s"
      (Config.to_string thm.cfg)
      (Variable.set_to_string thm.locals)
      (b_to_string thm.active_threads)
      (b_to_string thm.assumptions)
      (List.map Goal.to_string thm.goals |> String.concat "\n")

  let b_to_string ?(indent = "    ") (e : Exp.bexp) : string =
    e |> Exp.b_and_split |> List.map Exp.b_to_string
    |> String.concat ("\n" ^ indent ^ "&& ")

  let flatten_assumptions (thm : t) : bexp =
    let locals, globals = Proj.extract_global thm.locals thm.assumptions in
    let locals = Proj.b_split thm.cfg thm.locals locals |> Exp.b_and_ex in
    b_and globals locals

  let prove ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER))
      ?(debug = true) ?(verbose = false) ?(generator = Constraints.default)
      ?(tactic : Gen_z3.Tactic.t option = None) (thm : t) (goal : bexp) :
      (ProofResult.t, string) Result.t =
    let module S = (val solver) in
    let (assumptions, goal) = b_inline_cost thm goal |> State.run b_true in
    if verbose then prerr_endline ("POST GOAL: " ^ b_to_string goal);
    (* Prove that NOT(goal) is UNSAT *)
    let constraint_system =
      b_and_ex
        [
          Constraints.to_bexp thm.cfg generator;
          assumptions;
          flatten_assumptions thm;
          b_not goal;
        ]
    in
    if verbose then (
      prerr_endline "\n=== PROOF CONSTRAINT SYSTEM DEBUG ===";
      prerr_endline ("assumptions: " ^ (flatten_assumptions thm |> b_to_string));
      prerr_endline ("GOAL: " ^ b_to_string constraint_system);
      prerr_endline "=====================================\n");
    let result =
      match tactic with
      | Some tactic_strategy ->
          S.solve_with_tactic ~debug tactic_strategy constraint_system
      | None -> S.solve constraint_system
    in
    match result with
    | Unsat -> Ok ProofResult.Proved
    | Sat model -> Ok (ProofResult.Counterexample model)
    | Unknown msg -> Error ("Proof inconclusive: " ^ msg)

  let optimize_cost ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
      ?(verbose = false) ?(generator = Constraints.default)
      ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) (thm : t)
      (expr : nexp) : (int, string) Result.t =
    let p = n_inline_cost thm expr |> State.run b_true in
    match optimize ~verbose ~strategy ~generator ~solver thm.cfg p with
    | Some value -> Ok value
    | None -> Error "Optimization failed"

  (* Execute a single goal *)
  let execute_goal ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER))
      ?(debug = true) ?(verbose = false) ?(generator = Constraints.default)
      ?(tactic : Gen_z3.Tactic.t option = None) (thm : t) (goal : Goal.t) :
      (TheoremResult.t, string) Result.t =
    match goal with
    | Goal.Optimize { strategy; expr } -> (
        match optimize_cost ~strategy ~verbose ~generator ~solver thm expr with
        | Ok value -> Ok (TheoremResult.OptimizationResult value)
        | Error msg -> Error msg)
    | Goal.Prop proposition -> (
        match
          prove ~solver ~debug ~verbose ~generator ~tactic thm proposition
        with
        | Ok proof_result -> Ok (TheoremResult.ProofResult proof_result)
        | Error msg -> Error msg)

  (* Execute all goals in a theorem *)
  let execute ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER))
      ?(debug = true) ?(verbose = false) ?(generator = Constraints.default)
      ?(tactic : Gen_z3.Tactic.t option = None) (thm : t) :
      (TheoremResult.t, string) Result.t list =
    List.map
      (execute_goal ~solver ~debug ~verbose ~generator ~tactic thm)
      thm.goals
end
