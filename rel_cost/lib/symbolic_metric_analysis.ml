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
  type t = { suffix : string; locals : Variable.Set.t }

  let proj_var (x : Variable.t) (ctx : t) : Variable.t =
    if Variable.Set.mem x ctx.locals then
      Variable.update_name (fun n -> n ^ "$" ^ ctx.suffix) x
    else x

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
  type t = V1 | V2

  let to_string : t -> string = function V1 -> "V1" | V2 -> "V2"

  let tids (cfg : Config.t) : Variable.Set.t =
    [ Variable.tid_x; Variable.tid_y; Variable.tid_z ]
    |> List.filter (fun x -> not (Config.is_warp_uniform x cfg))
    |> Variable.Set.of_list

  (* Constraint generation 1.0 *)
  module V1Gen = struct
    let unique_tid_constraint_1 (thread_ids : nexp list) : bexp =
      (* Use the built-in distinct primitive to ensure all thread IDs are unique *)
      Distinct thread_ids

    let unique_tid_constraint_2 (thread_ids : nexp list) : bexp =
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

    let make (cfg : Config.t) : bexp =
      let thread_id : nexp =
        (* only generate variables for warp divergent tid *)
        let tid_x =
          if Config.is_warp_uniform Variable.tid_x cfg then Num 0
          else Var Variable.tid_x
        in
        let tid_y =
          if Config.is_warp_uniform Variable.tid_y cfg then Num 0
          else n_mult (Var Variable.tid_y) (Num cfg.block_dim.x)
        in
        let tid_z =
          if Config.is_warp_uniform Variable.tid_z cfg then Num 0
          else
            n_mult (Var Variable.tid_z)
              (Num (cfg.block_dim.x * cfg.block_dim.y))
        in
        n_plus tid_x (n_plus tid_y tid_z)
      in
      let thread_ids =
        thread_id |> Proj.proj_n |> Proj.run cfg.threads_per_warp (tids cfg)
      in
      let warp_ids =
        thread_ids
        |> List.map (fun tid -> n_div tid (Num cfg.threads_per_warp))
        |> Array.of_list
      in
      let c1 = unique_tid_constraint_2 thread_ids in
      let c2 = same_warp_constraint cfg warp_ids in
      b_and c1 c2
  end

  (* Constraint generation 2.0 - Linear Thread ID Architecture *)
  module V2Gen = struct
    let make (cfg : Config.t) : bexp =
      let warp_id_var = Variable.from_name "$warp_id" in

      (* Hoisted: determine which coordinates need constraints *)
      let coord_generators =
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
        |> List.filter (fun (var, _) -> not (Config.is_warp_uniform var cfg))
      in

      (* Generate constraints for each thread *)
      let thread_constraints =
        List.init cfg.threads_per_warp (fun uid ->
            let suffix = string_of_int uid in
            let thread_id =
              n_plus
                (n_mult (Var warp_id_var) (Num cfg.threads_per_warp))
                (Num uid)
            in

            coord_generators
            |> List.map (fun (var, coord_fn) ->
                   let tid_var =
                     Variable.update_name (fun n -> n ^ "$" ^ suffix) var
                   in
                   let coord_expr = coord_fn thread_id in
                   n_eq (Var tid_var) coord_expr)
            |> b_and_ex)
      in

      (* Add warp_id bounds constraint: 0 <= warp_id < total_warps *)
      let warp_bounds =
        clamp ~lower:(Num 0) ~value:(Var warp_id_var)
          ~upper:(Num (Config.total_warps cfg))
      in

      b_and_ex (warp_bounds :: thread_constraints)
  end

  let tid_bounds_constraint (cfg : Config.t) : bexp =
    (* only generate variables for warp divergent tid *)
    let bounds_for_thread (suffix : string) : bexp =
      [
        (Variable.tid_x, cfg.block_dim.x);
        (Variable.tid_y, cfg.block_dim.y);
        (Variable.tid_z, cfg.block_dim.z);
      ]
      |> List.filter (fun (x, _) -> not (Config.is_warp_uniform x cfg))
      |> List.map (fun (x, d) ->
             let x = Variable.update_name (fun n -> n ^ "$" ^ suffix) x in
             clamp ~lower:(Num 0) ~value:(Var x) ~upper:(Num d))
      |> b_and_ex
    in
    List.init cfg.threads_per_warp (fun i ->
        bounds_for_thread (string_of_int i))
    |> b_and_ex

  let to_bexp (strategy : t) (cfg : Config.t) : bexp =
    let strategy_constraint =
      match strategy with V1 -> V1Gen.make cfg | V2 -> V2Gen.make cfg
    in
    b_and strategy_constraint (tid_bounds_constraint cfg)
end

let encode_ua (cfg : Config.t) (locals : Variable.Set.t) (cond : bexp)
    (index : nexp) : nexp =
  let index = n_div index (Num (Config.memory_segments_bits cfg)) in
  let index =
    (* replicate index per each thread *)
    Proj.run cfg.threads_per_warp locals (fun ctx ->
        (Proj.proj_b cond ctx, Proj.proj_n index ctx))
    (* for each replciated index *)
    |> List.fold_left
         (fun ((accum, visited) : nexp * (bexp * nexp) list) (p : bexp * nexp)
            ->
           ( n_plus (n_if (cond_dissimilar p visited) (Num 1) (Num 0)) accum,
             p :: visited ))
         (* total cost = 0, visited = [] *)
         (Num 0, [])
    |>
    (* take only the accumulated value *)
    fst
  in
  index

let thread_locals_list (cfg : Config.t) : Variable.t list =
  Variable.tid_list |> List.filter (fun x -> not (Config.is_warp_uniform x cfg))

let thread_locals_set (cfg : Config.t) : Variable.Set.t =
  cfg |> thread_locals_list |> Variable.Set.of_list

let ua ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ?(generator = Constraints.V2) (cfg : Config.t) (locals : Variable.Set.t)
    (cond : bexp) (index : nexp) : int option =
  let open Gen_z3.IntGen in
  let locals =
    Variable.Set.union
      (Variable.Set.diff locals Variable.tid_set)
      (thread_locals_set cfg)
  in
  let formula = encode_ua cfg locals cond index in
  optimize_expr strategy ~pre:(Constraints.to_bexp generator cfg) formula
  |> Result.to_option

module Comparison = struct
  type t =
    | Equal (* cost = expected *)
    | LessEqual (* cost <= expected *)
    | GreaterEqual (* cost >= expected *)
    | Less (* cost < expected *)
    | Greater (* cost > expected *)

  let to_string : t -> string = function
    | Equal -> "="
    | LessEqual -> "<="
    | GreaterEqual -> ">="
    | Less -> "<"
    | Greater -> ">"

  let to_relation : t -> nexp -> nexp -> bexp = function
    | Equal -> n_eq
    | LessEqual -> n_le
    | GreaterEqual -> n_ge
    | Less -> n_lt
    | Greater -> n_gt
end

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
    thread_context : bexp;
    global_context : bexp;
    index : nexp;
    comparison : Comparison.t;
    expected_cost : nexp;
  }

  let to_string (thm : t) : string =
    Printf.sprintf "thread=%s; global=%s |- cost(%s) %s %s"
      (b_to_string thm.thread_context)
      (b_to_string thm.global_context)
      (n_to_string thm.index)
      (Comparison.to_string thm.comparison)
      (n_to_string thm.expected_cost)

  let prove ?(generator = Constraints.V1) (thm : t) : ProofResult.t =
    let open Gen_z3.IntGen in
    let locals =
      Variable.Set.union
        (Variable.Set.diff thm.locals Variable.tid_set)
        (thread_locals_set thm.cfg)
    in
    let index = thm.index in
    let actual_cost = encode_ua thm.cfg locals thm.thread_context index in
    let comparison_fn = Comparison.to_relation thm.comparison in
    let goal = comparison_fn actual_cost thm.expected_cost in
    (* Prove that NOT(actual_cost comparison expected_cost) is UNSAT *)
    let constraint_system =
      b_and_ex
        [
          thm.global_context; Constraints.to_bexp generator thm.cfg; b_not goal;
        ]
    in
    let open Gen_z3.Solver in
    match solve constraint_system with
    | Unsat -> ProofResult.Proved
    | Sat model -> ProofResult.Counterexample model
    | Unknown msg -> ProofResult.Unknown msg
end
