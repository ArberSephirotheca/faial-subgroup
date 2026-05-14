open Stage0
open Protocols
open Drf
open Exp

let rec eval_n (lookup : string -> int option) : nexp -> int option = function
  | Num n -> Some n
  | Var v -> lookup (Variable.name v)
  | Binary (op, a, b) ->
    (match eval_n lookup a, eval_n lookup b with
     | Some va, Some vb ->
       (try Some (N_binary.eval op va vb) with Division_by_zero -> None)
     | _ -> None)
  | Unary (op, a) -> Option.map (N_unary.eval op) (eval_n lookup a)
  | NCall _ -> None
  | NIf (b, t, f) ->
    (match eval_b lookup b with
     | Some true -> eval_n lookup t
     | Some false -> eval_n lookup f
     | None -> None)
  | Other _ -> None
  | CastInt b ->
    (match eval_b lookup b with
     | Some v -> Some (if v then 1 else 0)
     | None -> None)

and eval_b (lookup : string -> int option) : bexp -> bool option = function
  | Bool b -> Some b
  | NRel (op, a, b) ->
    (match eval_n lookup a, eval_n lookup b with
     | Some va, Some vb -> Some (N_rel.eval op va vb)
     | _ -> None)
  | BRel (op, a, b) ->
    (match eval_b lookup a, eval_b lookup b with
     | Some va, Some vb -> Some (B_rel.eval op va vb)
     | _ -> None)
  | BNot b -> Option.map not (eval_b lookup b)
  (* [bvumul_noovfl] is the BV no-overflow guard the pool attaches to
     multiplicative shapes. Under natural-number reasoning it's
     trivially true, which is what the abductive sample evaluator
     uses; the BV gate gets the real constraint from the encoder. *)
  | Pred ("bvumul_noovfl", _) -> Some true
  | Pred _ -> None
  | CastBool n -> Option.map (fun v -> v <> 0) (eval_n lookup n)
  | Distinct xs ->
    let vs = List.map (eval_n lookup) xs in
    if List.exists Option.is_none vs then None
    else
      let ints = List.map Option.get vs in
      Some (List.length (List.sort_uniq Int.compare ints) = List.length ints)

let launch_config_set : Variable.Set.t =
  let open Variable in
  Set.union (Set.union tid_set bid_set) (Set.union bdim_set gdim_set)

let int_params (k : Kernel.t) : Variable.t list =
  Params.to_list (Params.union_left k.global_variables k.local_variables)
  |> List.filter_map (fun (v, ty) ->
    if C_type.is_int ty && not (Variable.Set.mem v launch_config_set)
    then Some v else None)
  |> List.sort_uniq Variable.compare

(* Signedness for any [Variable.t] reachable from [k]. CUDA built-in
   launch-config variables (threadIdx, blockIdx, blockDim, gridDim)
   are unsigned int. Other variables are looked up in [k]'s declared
   types via [Params]; absent or non-int → default [Signed]. *)
let signedness_of (k : Kernel.t) (v : Variable.t) : Signedness.t =
  if Variable.Set.mem v launch_config_set then Signedness.Unsigned
  else
    let p = Params.union_left k.global_variables k.local_variables in
    match Params.find_opt v p with
    | Some (_, ty) when C_type.is_unsigned ty -> Signedness.Unsigned
    | _ -> Signedness.Signed

(* Pool combinators. Each step is [bexp list -> bexp list] and prepends
   to the accumulator; order is irrelevant downstream. *)

let push (b : bexp) (acc : bexp list) : bexp list = b :: acc

let push_each (xs : 'a list) (f : 'a -> bexp) (acc : bexp list) : bexp list =
  List.fold_left (fun acc x -> f x :: acc) acc xs

let push_pairs ?(when_ = fun _ _ -> true)
    (xs : 'a list) (f : 'a -> 'a -> bexp) (acc : bexp list) : bexp list =
  List.fold_left (fun acc a ->
    List.fold_left (fun acc b ->
      if when_ a b then f a b :: acc else acc) acc xs) acc xs

let push_prod (xs : 'a list) (ys : 'b list)
    (f : 'a -> 'b -> bexp) (acc : bexp list) : bexp list =
  List.fold_left (fun acc x ->
    List.fold_left (fun acc y -> f x y :: acc) acc ys) acc xs

(* "Any-unsigned wins" combinator. Mirrors C's usual arithmetic
   conversions: a binary op whose operands differ in signedness is
   performed unsigned. *)
let mix_sign (s1 : Signedness.t) (s2 : Signedness.t) : Signedness.t =
  match s1, s2 with
  | Unsigned, _ | _, Unsigned -> Unsigned
  | Signed, Signed -> Signed

(* When [scope] is provided, parameters and dim variables not present
   in the set are filtered out before pool construction. Currently
   only used by the per-fragment scoping path (off by default in the
   genie binary) — passing no [scope] yields the legacy whole-kernel
   pool. *)
let build_pool ?(scope : Variable.Set.t option) (k : Kernel.t) : bexp list =
  let in_scope v =
    match scope with
    | None -> true
    | Some s -> Variable.Set.mem v s
  in
  let params = int_params k |> List.filter in_scope in
  let all_dims =
    let open Variable in
    [ bdim_x; bdim_y; bdim_z; gdim_x; gdim_y; gdim_z ]
    |> List.filter in_scope
  in
  let lt a b = Variable.compare a b < 0 in
  let ne a b = Variable.compare a b <> 0 in
  let sign = signedness_of k in
  (* No-overflow guard on an unsigned product. The BV gate would
     otherwise accept models where [a * b] wraps in BV32 (e.g.
     [4 * 2^30] becoming 0), letting classically-UNSAT preconditions
     pass via wraparound. Under natural-number reasoning the guard
     is trivially true (see [Bv64Gen.mk_umul_no_overflow] and the
     [Pred ("bvumul_noovfl", _) -> Some true] arm in [eval_b]). *)
  let no_ovfl (a : nexp) (b : nexp) : bexp =
    Pred ("bvumul_noovfl", [ a; b ])
  in
  (* Conjoin [v >= 0] when [v] is a signed-typed kernel param. The
     abductive pool emits predicates of the form [v >=u unsigned_rhs]
     for mixed-signedness pairs (any-unsigned-wins per [mix_sign]);
     without this guard the BV gate accepts models where [v] is
     signed-negative and its unsigned reinterpretation
     ([0xFFFFFFFE...]) is trivially [>=u] any small RHS. Operationally
     vacuous because kernel-size parameters are non-negative at
     runtime. Inlined to [NRel (Ge Signed, v, 0)] by [Predicates],
     so the BV / Int encoders need no special case. *)
  let nonneg_if_signed (v : nexp) (s : Signedness.t) : bexp list =
    match s with
    | Signed -> [ Pred ("nonneg", [ v ]) ]
    | Unsigned -> []
  in
  let guard (extras : bexp list) (b : bexp) : bexp =
    List.fold_left b_and b extras
  in
  (* All [all_dims] are CUDA built-ins (unsigned int). Any binary op
     that mixes a kernel param [p] with a dim is unsigned-dominated. *)
  let per_param acc p =
    let v : nexp = Var p in
    let sp = sign p in
    let s_pd = mix_sign sp Unsigned in
    let nn = nonneg_if_signed v sp in
    acc
    |> push       (NRel (Gt sp, v, Num 0))
    |> push_each  all_dims (fun d ->
         (* Single-dim bound under [>=u]: the signed-negative
            reinterpretation makes [v] huge under unsigned compare,
            so the guard is load-bearing. *)
         guard nn (NRel (Ge s_pd, v, Var d)))
    |> push_each  all_dims (fun d ->
         (* Divisibility [v %u d == 0]: a signed-negative [v]'s
            BV-unsigned modulo can equal 0 for some [d]. Guard. *)
         guard nn (NRel (Eq, Binary (Mod Unsigned, v, Var d), Num 0)))
    |> push_each  all_dims (fun d ->
         (* Single-dim equality [v == d]: bit-equality. The signed-
            negative [v] (high-bit pattern) can't match an unsigned
            dim in [1, dim_upper_cap]; no guard needed. *)
         NRel (Eq, v, Var d))
    |> push_pairs ~when_:lt all_dims (fun a b ->
         guard nn
           (b_and
              (NRel (Ge s_pd, v, Binary (Mult Unsigned, Var a, Var b)))
              (no_ovfl (Var a) (Var b))))
    |> push_pairs ~when_:lt all_dims (fun a b ->
         (* EqProduct [v == dim_a * dim_b]: bit-equality on the
            product. With [no_ovfl] but unbounded dims (dim upper
            bounds are a separate pool entry, not co-selected here),
            the product can reach values whose bit pattern matches a
            signed-negative [v] (e.g. [d_a = 2^31, d_b = 1] gives
            product [2^31] which equals signed [v = -2^31]). The
            guard rules out that reinterpretation. *)
         guard nn
           (b_and
              (NRel (Eq, v, Binary (Mult Unsigned, Var a, Var b)))
              (no_ovfl (Var a) (Var b))))
    |> push_prod  all_dims [ 2; 4 ] (fun d c ->
         guard nn
           (b_and
              (NRel (Ge s_pd, v, Binary (Mult Unsigned, Num c, Var d)))
              (no_ovfl (Num c) (Var d))))
  in
  (* Dim upper bounds. Launch dimensions are typically pinned to a
     specific value by the launch literal, but at synthesised
     pseudo-kernels where the literal is symbolic, the kernel often
     requires [dim <= K] for some kernel-specific constant K (e.g.
     bm3d's shared-memory stride pattern needs [blockDim.x <= 8]). The
     constants below are common GPU block / tile sizes. *)
  let dim_upper_caps = [ 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024 ] in
  []
  |> (fun acc -> List.fold_left per_param acc params)
  |> push_pairs ~when_:ne params (fun a b ->
       let cmp = NRel (Ge (mix_sign (sign a) (sign b)), Var a, Var b) in
       (* Cross-param [a >= b] may mix signed and unsigned; guard each
          signed operand against the negative-reinterpretation hole. *)
       let extras =
         nonneg_if_signed (Var a) (sign a)
         @ nonneg_if_signed (Var b) (sign b)
       in
       List.fold_left b_and cmp extras)
  |> push_prod all_dims dim_upper_caps (fun d k ->
       NRel (Le Unsigned, Var d, Num k))

let build_pool_union (ks : Kernel.t list) : bexp list =
  ks |> List.concat_map build_pool |> List.sort_uniq Exp.b_compare

(* Per-fragment vocabulary keyed by (kernel_name, proof_id). Used by
   the experimental per-fragment scoping path; currently off in the
   genie binary because it didn't show measurable benefit over the
   whole-kernel pool. Kept here so the path stays reachable if we
   want to revisit with a pinned Z3 random seed (see [genie.md]). *)
module FragmentKey = struct
  type t = string * int
  let compare (a : t) (b : t) : int =
    let c = String.compare (fst a) (fst b) in
    if c <> 0 then c else Int.compare (snd a) (snd b)
end

module FragmentScopes = Map.Make (FragmentKey)
module FragmentSet = Set.Make (FragmentKey)

let fragment_scopes (analyses : Analysis.t list)
    : Variable.Set.t FragmentScopes.t =
  List.fold_left (fun acc (a : Analysis.t) ->
    List.fold_left (fun acc (s : Solve_drf.Solution.t) ->
      let key = (s.proof.kernel_name, s.proof.id) in
      FragmentScopes.add key (Symbexp.Proof.free_names s.proof) acc)
      acc a.report)
    FragmentScopes.empty analyses

let build_pool_tagged
    (kernels : Kernel.t list)
    (scopes : Variable.Set.t FragmentScopes.t)
    : (bexp * FragmentSet.t) list =
  let kernel_of_name kn =
    List.find_opt (fun (k : Kernel.t) -> k.name = kn) kernels
  in
  let module BexpMap = Map.Make (struct
      type t = bexp
      let compare = Exp.b_compare
    end)
  in
  FragmentScopes.fold (fun key scope acc ->
    let kn, _ = key in
    match kernel_of_name kn with
    | None -> acc
    | Some k ->
      let pool = build_pool ~scope k in
      List.fold_left (fun acc b ->
        BexpMap.update b (function
          | None -> Some (FragmentSet.singleton key)
          | Some fs -> Some (FragmentSet.add key fs)) acc)
        acc pool)
    scopes BexpMap.empty
  |> BexpMap.bindings

type t = {
  ctx : Z3.context;
  opt : Z3.Optimize.optimize;
  candidates : (bexp * Z3.Expr.expr * FragmentSet.t) list;
}

let create_from_pool (pool : bexp list) : t =
  Phase_timer.measure "abduction/create" (fun () ->
    let ctx = Z3.mk_context [] in
    let opt = Z3.Optimize.mk_opt ctx in
    let group = Z3.Symbol.mk_string ctx "minimize" in
    let candidates =
      List.mapi (fun i b ->
        let sel = Z3.Boolean.mk_const_s ctx ("b_" ^ string_of_int i) in
        let _ : Z3.Optimize.handle =
          Z3.Optimize.add_soft opt (Z3.Boolean.mk_not ctx sel) "1" group
        in
        (b, sel, FragmentSet.empty))
        pool
    in
    { ctx; opt; candidates })

let create_from_tagged_pool (pool : (bexp * FragmentSet.t) list) : t =
  Phase_timer.measure "abduction/create" (fun () ->
    let ctx = Z3.mk_context [] in
    let opt = Z3.Optimize.mk_opt ctx in
    let group = Z3.Symbol.mk_string ctx "minimize" in
    let candidates =
      List.mapi (fun i (b, tag) ->
        let sel = Z3.Boolean.mk_const_s ctx ("b_" ^ string_of_int i) in
        let _ : Z3.Optimize.handle =
          Z3.Optimize.add_soft opt (Z3.Boolean.mk_not ctx sel) "1" group
        in
        (b, sel, tag))
        pool
    in
    { ctx; opt; candidates })

let create (k : Kernel.t) : t = create_from_pool (build_pool k)

let create_for_kernels (ks : Kernel.t list) : t =
  create_from_pool (build_pool_union ks)

let create_for_kernels_scoped
    (kernels : Kernel.t list)
    (scopes : Variable.Set.t FragmentScopes.t) : t =
  create_from_tagged_pool (build_pool_tagged kernels scopes)

let witness_lookup (vars : (string * string) list) : string -> int option =
  let table = Hashtbl.create 32 in
  List.iter (fun (k, v) ->
    match int_of_string_opt (String.trim v) with
    | Some n -> Hashtbl.replace table k n
    | None -> ())
    vars;
  fun name -> Hashtbl.find_opt table name

(* When [fragment] is provided, only candidates whose tag set contains
   that fragment (or whose tag is empty, i.e. scope-unaware) are
   eligible to reject the witness. Without [fragment], every candidate
   that the witness falsifies is eligible, matching the legacy
   non-scoped behaviour. *)
let add_sample ?(fragment : FragmentKey.t option = None) (s : t)
    (vars : (string * string) list) : int =
  let lookup = witness_lookup vars in
  let eligible tag =
    match fragment with
    | None -> true
    | Some key -> FragmentSet.is_empty tag || FragmentSet.mem key tag
  in
  let bad =
    List.filter_map (fun (c, sel, tag) ->
      if not (eligible tag) then None
      else match eval_b lookup (Predicates.b_inline c) with
        | Some false -> Some sel
        | _ -> None)
      s.candidates
  in
  match bad with
  | [] -> 0
  | _ ->
    Phase_timer.measure "abduction/add" (fun () ->
      Z3.Optimize.add s.opt [ Z3.Boolean.mk_or s.ctx bad ]);
    List.length bad

(* CEGIS rejection: ban a specific selector combination from future
   solutions by asserting [¬(s_1 ∧ ... ∧ s_n)]. Returns the number of
   selectors that were resolved; if no [chosen] bexp is found in the
   pool the call is a no-op. *)
let reject_combination (s : t) (chosen : bexp list) : int =
  let neg_selectors =
    List.filter_map (fun c ->
      List.find_opt (fun (cand, _, _) -> Exp.b_compare cand c = 0) s.candidates
      |> Option.map (fun (_, sel, _) -> Z3.Boolean.mk_not s.ctx sel))
      chosen
  in
  match neg_selectors with
  | [] -> 0
  | _ ->
    Phase_timer.measure "abduction/add" (fun () ->
      Z3.Optimize.add s.opt [ Z3.Boolean.mk_or s.ctx neg_selectors ]);
    List.length neg_selectors

let add_all (analyses : Analysis.t list) (session : t) : int =
  List.fold_left (fun acc (a : Analysis.t) ->
    List.fold_left (fun acc (s : Solve_drf.Solution.t) ->
      match s.outcome with
      | Solve_drf.Outcome.Racy w ->
        acc + add_sample session w.globals.variables
      | _ -> acc)
      acc a.report)
    0 analyses

let solve (s : t) : bexp list option =
  match Phase_timer.measure "abduction/check" (fun () -> Z3.Optimize.check s.opt) with
  | Z3.Solver.SATISFIABLE ->
    Z3.Optimize.get_model s.opt
    |> Option.map (fun m ->
      List.filter_map (fun (c, sel, _tag) ->
        match Z3.Model.eval m sel false with
        | Some v when Z3.Boolean.get_bool_value v = Z3enums.L_TRUE -> Some c
        | _ -> None)
        s.candidates)
  | _ -> None
