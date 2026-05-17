(** Re-tag a plain [Infer_stmt.Read] as [Infer_stmt.Atomic] when its
    target variable feeds — through a chain of single-source copy
    declarations and assignments — into the [expected] argument of an
    [atomicCAS]-class atomic on the same [(array, index)]. Models the
    standard CAS-spin idiom:

      T ret = *p;
      while (val < ret) { T old = ret;
                          ret = atomicCAS(p, old, val);
                          if (ret == old) break; }

    Under faial-drf's per-access mode model the plain [Read] of [*p]
    races with the concurrent atomic write on [*p]. The hardware
    revalidates the seed via the CAS on every iteration, so the seed
    read is benign *as long as we can prove* that its value flows
    into the CAS's [expected] slot — meaning the seed is exactly the
    value the CAS compares against, and a successful CAS implies the
    seed was up-to-date at the moment of commit. We don't silence
    plain reads on the same address that flow elsewhere (e.g. into
    a branch condition); those would be genuine fast-path-off-stale-
    snapshot bugs.

    Per-function detection: the rewrite runs on each [Infer_stmt.t]
    before [Infer_stmt.to_stmt] / [Scoped.Kernel.from_imp] / the call
    inliner. atomicCAS-cuda's pattern is self-contained inside the
    [__device__] helper (seed read and CAS in the same body), so
    per-function detection is sufficient. Cross-function (caller-seed
    / callee-atomic) would need a post-inline pass — out of scope for
    now since the Imp→Scoped translation drops the explicit access
    targets needed by the alias-closure step. *)

open Stage0
open Protocols
open Infer_stmt

module VarSet = Variable.Set
module VarMap = Variable.Map

(* ----- 1. Alias-closure of copy chains ------------------------------ *)

(* Build a [Variable.t -> Variable.t] map from "single-source copy"
   shapes:
   - [Decl { var = W; init = Some (NExp (Var V)); _ }] (or the bool
     wrapper around [Var V]),
   - [Assign { var = W; data = NExp (Var V); _ }] (or the bool wrapper).

   Multi-source initialisers or compound assignments intentionally
   don't alias — they introduce derived values, not copies. *)
let copy_source : Infer_exp.t -> Variable.t option = function
  | Infer_exp.NExp (Infer_exp.Var v) -> Some v
  | Infer_exp.BExp _ | Infer_exp.NExp _ | Infer_exp.Unknown _ -> None

(* Flow-insensitive multi-map: every variable W that is ever a copy
   target of some V (e.g. [Decl W = V] or [W = V]) adds V to the set
   [alias[W]]. The same W may be reassigned to different sources at
   different program points (e.g. [ret] is initialised from a seed
   read at the top and reassigned from the CAS return inside the
   loop); both sources need to be in the set so the seed→expected
   chain survives the reassignment. False positives are bounded —
   spurious extra members of [alias[W]] only add upstream candidates
   to the closure, which at worst expands the re-tag set but never
   removes an already-tagged race (atomic↔atomic is safe under the
   race model). *)
let add_alias (w : Variable.t) (v : Variable.t)
    (acc : VarSet.t VarMap.t) : VarSet.t VarMap.t =
  let bucket =
    match VarMap.find_opt w acc with
    | Some s -> VarSet.add v s
    | None -> VarSet.singleton v
  in
  VarMap.add w bucket acc

let rec collect_aliases (acc : VarSet.t VarMap.t) (s : t) :
    VarSet.t VarMap.t =
  match s with
  | Skip | Sync _ | SyncOp _ | Assert _ | Read _ | Atomic _ | Write _
  | LocationAlias _ | Call _ | Break | Continue | Return _ ->
      acc
  | Decl { var = w; init = Some e; _ } -> (
      match copy_source e with
      | Some v -> add_alias w v acc
      | None -> acc)
  | Decl { init = None; _ } -> acc
  | Assign { var = w; data = e; _ } -> (
      match copy_source e with
      | Some v -> add_alias w v acc
      | None -> acc)
  | Seq (a, b) -> collect_aliases (collect_aliases acc a) b
  | If (_, p, q) -> collect_aliases (collect_aliases acc p) q
  | While (_, p) | DoWhile (_, p) -> collect_aliases acc p
  | For { init; inc; body; cond = _ } ->
      collect_aliases (collect_aliases (collect_aliases acc init) inc) body

(* [alias[W]] is the set of variables W has ever copied from. Starting
   from the atomic's [expected_vars] (variables that feed the CAS, e.g.
   [{old}]), walk forward through [alias] to find every upstream
   variable whose value transitively flowed into the seed. Those are
   the Read-access targets we may re-tag. *)
let alias_closure ~(alias : VarSet.t VarMap.t) (seed : VarSet.t) :
    VarSet.t =
  let rec fix (frontier : VarSet.t) (seen : VarSet.t) : VarSet.t =
    if VarSet.is_empty frontier then seen
    else
      let next =
        VarSet.fold
          (fun v acc ->
            match VarMap.find_opt v alias with
            | Some sources ->
                VarSet.fold
                  (fun w acc ->
                    if VarSet.mem w seen then acc else VarSet.add w acc)
                  sources acc
            | None -> acc)
          frontier VarSet.empty
      in
      fix next (VarSet.union seen next)
  in
  fix seed seed

(* ----- 2. Free variables of an Infer_exp.t ------------------------- *)

let rec free_vars_n (acc : VarSet.t) : Infer_exp.n -> VarSet.t = function
  | Var v -> VarSet.add v acc
  | Num _ -> acc
  | Unary (_, e) -> free_vars acc e
  | Binary (_, l, r) -> free_vars (free_vars acc l) r
  | NCall (_, e) -> free_vars acc e
  | NIf (c, l, r) -> free_vars (free_vars (free_vars acc c) l) r
  | Other e -> free_vars acc e

and free_vars_b (acc : VarSet.t) : Infer_exp.b -> VarSet.t = function
  | Bool _ -> acc
  | NRel (_, l, r) -> free_vars (free_vars acc l) r
  | BRel (_, l, r) -> free_vars (free_vars acc l) r
  | BNot e -> free_vars acc e
  | Pred (_, e) -> free_vars acc e

and free_vars (acc : VarSet.t) : Infer_exp.t -> VarSet.t = function
  | NExp n -> free_vars_n acc n
  | BExp b -> free_vars_b acc b
  | Unknown _ -> acc

(* ----- 3. Address fingerprint -------------------------------------- *)

let address_key (array : Variable.t) (index : Infer_exp.t list) : string =
  let parts = List.map Infer_exp.to_string index in
  Variable.name array ^ "[" ^ String.concat ";" parts ^ "]"

(* ----- 4. Seed-target index ---------------------------------------- *)

(* Recognise a CAS by name: [atomicCAS] (CUDA, device), plus the
   block / system scoped variants and WGSL's atomicExchangeWeak (which
   w_to_imp emits with name [atomicExchangeWeak] when [compare = Some
   _]). The downstream [expected = Some _] check is what actually
   gates the rewrite; the name check is a defence-in-depth filter
   against non-CAS atomics whose [expected] might accidentally be
   populated by a future code path. *)
let is_cas (name : string) : bool =
  let starts s prefix =
    let lp = String.length prefix in
    String.length s >= lp && String.sub s 0 lp = prefix
  in
  starts name "atomicCAS" || starts name "atomicExchangeWeak"

(* Build (array,index)-fingerprint -> (seed targets, the matched
   Atomic.t) by walking every Atomic with [expected = Some _] and an
   atomicCAS-class name, computing alias-closure of the expected
   expression's free vars. *)
let seed_index ~(alias : VarSet.t VarMap.t) (s : t) :
    (VarSet.t * Atomic.t) Common.StringMap.t =
  let module SM = Common.StringMap in
  let rec walk acc = function
    | Skip | Sync _ | SyncOp _ | Assert _ | Read _ | Write _
    | LocationAlias _ | Call _ | Break | Continue | Return _ | Decl _
    | Assign _ ->
        acc
    | Atomic { atomic; array; index; expected = Some e; _ }
      when is_cas (Variable.name atomic.name) ->
        let key = address_key array index in
        let seeds =
          alias_closure ~alias (free_vars VarSet.empty e)
        in
        let merged =
          match SM.find_opt key acc with
          | Some (existing, _) -> VarSet.union existing seeds
          | None -> seeds
        in
        SM.add key (merged, atomic) acc
    | Atomic _ -> acc
    | Seq (a, b) -> walk (walk acc a) b
    | If (_, p, q) -> walk (walk acc p) q
    | While (_, p) | DoWhile (_, p) -> walk acc p
    | For { init; inc; body; cond = _ } ->
        walk (walk (walk acc init) inc) body
  in
  walk SM.empty s

(* ----- 5. Rewrite -------------------------------------------------- *)

let rec rewrite_with ~(seeds : (VarSet.t * Atomic.t) Common.StringMap.t)
    (s : t) : t =
  let rew = rewrite_with ~seeds in
  match s with
  | Skip | Sync _ | SyncOp _ | Assert _ | Atomic _ | Write _
  | LocationAlias _ | Call _ | Break | Continue | Return _ | Decl _
  | Assign _ ->
      s
  | Read { target = Some (ty, t); array; index } as r -> (
      let key = address_key array index in
      match Common.StringMap.find_opt key seeds with
      | Some (seed_set, atomic) when VarSet.mem t seed_set ->
          Atomic
            { target = t; ty; atomic; array; index;
              expected = None; increment = None }
      | _ -> r)
  | Read _ as r -> r (* read with no target — no seed to match *)
  | Seq (a, b) -> Seq (rew a, rew b)
  | If (c, p, q) -> If (c, rew p, rew q)
  | While (c, p) -> While (c, rew p)
  | DoWhile (c, p) -> DoWhile (c, rew p)
  | For { init; cond; inc; body } ->
      For { init = rew init; cond; inc = rew inc; body = rew body }

(** Top-level entry. Idempotent: a second call has no effect because
    re-tagged reads are now [Atomic], not [Read], so the index step
    won't enqueue them and the rewrite step won't visit them. *)
let rewrite (s : t) : t =
  let alias : VarSet.t VarMap.t = collect_aliases VarMap.empty s in
  let seeds = seed_index ~alias s in
  if Common.StringMap.is_empty seeds then s else rewrite_with ~seeds s
