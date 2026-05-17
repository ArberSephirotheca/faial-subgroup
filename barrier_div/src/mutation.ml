open Protocols

(* Mutation operators for the synchronization-property dataset.

   Each operator is a pure transformation Kernel.t -> Kernel.t list. Two
   relabel functions assign output labels for each property independently
   ([ws_relabel] for well-sync, [bd_relabel] for barrier-divergence). The
   per-property rule justifications live in operator comments and reference
   the inference rules in documentation/well-sync.md and
   documentation/barrier-div.md. *)

module Label = struct
  type t = WellSync | IllSync

  let to_string : t -> string = function
    | WellSync -> "well_sync"
    | IllSync -> "ill_sync"

  let of_string : string -> t = function
    | "well" | "well_sync" | "wellsync" | "WellSync" -> WellSync
    | "ill" | "ill_sync" | "illsync" | "IllSync" -> IllSync
    | s -> invalid_arg ("unknown label: " ^ s)

  (* Pair of labels carried through mutation: one per property. *)
  type pair = { well_sync : t; barrier_div : t }

  let pair_to_filename (p : pair) : string =
    Printf.sprintf "ws-%s.bd-%s"
      (to_string p.well_sync) (to_string p.barrier_div)
end

type t = {
  name : string;
  description : string;
  ws_relabel : Label.t -> Label.t;
  bd_relabel : Label.t -> Label.t;
  apply : Kernel.t -> Kernel.t list;
}

let relabel_pair (op : t) (p : Label.pair) : Label.pair =
  {
    well_sync = op.ws_relabel p.well_sync;
    barrier_div = op.bd_relabel p.barrier_div;
  }

(* ----- helpers ----- *)

let kernel_used_vars (k : Kernel.t) : Variable.Set.t =
  Variable.Set.union
    (Kernel.parameter_set k)
    (Code.free_names k.code Variable.Set.empty)

let fresh (k : Kernel.t) (base : string) : Variable.t =
  Variable.fresh (kernel_used_vars k) (Variable.from_name base)

let has_sync (c : Code.t) : bool =
  Code.exists (function Code.Sync _ -> true | _ -> false) c

let identity : Label.t -> Label.t = fun l -> l
let force_ill : Label.t -> Label.t = fun _ -> Label.IllSync

(* ----- Property-preserving operators (identity on both labels) -----

   For each, monotonicity of the analysis state preserves every pre-existing
   barrier site's verification condition truth value: WellSync inputs stay
   WellSync, IllSync inputs stay IllSync. The same justification applies to
   both properties since the operator's effect on Γ is shared. *)

(* Replace each Sync s with Seq(Sync s, Sync s).
   Both occurrences emit the same Γ ⊢ sync ↝ obl(Γ); the obligation set
   doubles up on each site without changing per-site truth. Holds for
   both properties (same Γ, same obligation, same VC). *)
let duplicate_sync : t =
  let rec dup : Code.t -> Code.t = function
    | Sync s -> Seq (Sync s, Sync s)
    | (Skip | Access _) as p -> p
    | Seq (p, q) -> Seq (dup p, dup q)
    | If (b, p, q) -> If (b, dup p, dup q)
    | Decl d -> Decl { d with body = dup d.body }
    | Loop l -> Loop { l with body = dup l.body }
  in
  {
    name = "duplicate_sync";
    description = "each Sync s becomes Seq(Sync s, Sync s)";
    ws_relabel = identity;
    bd_relabel = identity;
    apply = (fun k -> [ { k with code = dup k.code } ]);
  }

(* Wrap k.code in Decl(x, body) for fresh x not mentioned in body.
   [Decl] places x in L ∩ P, but no guard reaching a sync mentions x, so
   D and U at every existing site are unchanged under both properties. *)
let prepend_unused_decl : t =
  {
    name = "prepend_unused_decl";
    description = "wrap body in Decl(x, body) for fresh unused x";
    ws_relabel = identity;
    bd_relabel = identity;
    apply =
      (fun k ->
        let x = fresh k "mut_unused" in
        [
          {
            k with
            code = Decl { var = x; ty = C_type.int; pre = None; body = k.code };
          };
        ]);
  }

(* Wrap k.code in If(tid.x < blockDim.x, body, Skip).
   [If-Div] adds (tid.x < blockDim.x) to D. The arch precondition forces
   tid.x < blockDim.x in both T1 and T2 (well-sync) and in both threads
   of the same group (barrier-div), so the conjunct cannot create a
   disagreement under either property. *)
let wrap_arch_if : t =
  let cond : Exp.bexp = Exp.n_lt (Var Variable.tid_x) (Var Variable.bdim_x) in
  {
    name = "wrap_arch_if";
    description = "wrap body in if (tid.x < blockDim.x) { body }";
    ws_relabel = identity;
    bd_relabel = identity;
    apply = (fun k -> [ { k with code = If (cond, k.code, Skip) } ]);
  }

(* Wrap k.code in Loop(i ∈ [0, N)) for fresh kernel parameter N.
   [Loop-Unif] adds i to S and (0 ≤ i ∧ i < N) to U; D is unchanged.
   N is in S under both properties (kernel parameters are shared); the
   range guard is uniform under both. *)
let wrap_uniform_loop : t =
  {
    name = "wrap_uniform_loop";
    description = "wrap body in for(int i = 0; i < N; ++i) for fresh global N";
    ws_relabel = identity;
    bd_relabel = identity;
    apply =
      (fun k ->
        let n = fresh k "mut_N" in
        let used' = Variable.Set.add n (kernel_used_vars k) in
        let i = Variable.fresh used' (Variable.from_name "mut_i") in
        let r = Range.make ~lower_bound:(Num 0) i (Var n) in
        [
          {
            k with
            code = Loop { range = r; body = k.code };
            global_variables =
              Params.add n C_type.int k.global_variables;
          };
        ]);
  }

(* ----- WS-and-BD-breaking operators -----

   New projectable variable enters D under both properties. WellSync inputs
   become IllSync; IllSync inputs stay IllSync (pre-existing counter-model
   survives, strengthened by the new constraint). *)

(* Wrap k.code in Decl(x, If(x > 0, body, Skip)) for fresh x.
   [Decl] places x in L ∩ P (under both properties' P); [If-Div] adds
   (x > 0) to D. Take any pair of valuations agreeing on every variable
   except x, with x ↦ 1 in one and x ↦ 0 in the other: D differs and the
   VC fails. *)
let wrap_decl_if : t =
  {
    name = "wrap_decl_if";
    description = "wrap body in { int x; if (x > 0) body }";
    ws_relabel = force_ill;
    bd_relabel = force_ill;
    apply =
      (fun k ->
        if not (has_sync k.code) then []
        else
          let x = fresh k "mut_x" in
          let cond = Exp.n_gt (Var x) (Num 0) in
          let inner : Code.t = If (cond, k.code, Skip) in
          [
            {
              k with
              code = Decl { var = x; ty = C_type.int; pre = None; body = inner };
            };
          ]);
  }

(* Wrap k.code in Decl(n, Loop(i ∈ [0, n)) body) for fresh n, i.
   [Decl] places n in L ∩ P; [Loop-Div] places i in L ∩ S and adds
   (0 ≤ i ∧ i < n) to D. With n ↦ k > 0 in one valuation and n ↦ 0 in
   the other (i shared), D differs and the VC fails. *)
let wrap_decl_loop : t =
  {
    name = "wrap_decl_loop";
    description =
      "wrap body in { int n; for (int i = 0; i < n; ++i) body }";
    ws_relabel = force_ill;
    bd_relabel = force_ill;
    apply =
      (fun k ->
        if not (has_sync k.code) then []
        else
          let n = fresh k "mut_n" in
          let used' = Variable.Set.add n (kernel_used_vars k) in
          let i = Variable.fresh used' (Variable.from_name "mut_i") in
          let r = Range.make ~lower_bound:(Num 0) i (Var n) in
          let inner : Code.t = Loop { range = r; body = k.code } in
          [
            {
              k with
              code = Decl { var = n; ty = C_type.int; pre = None; body = inner };
            };
          ]);
  }

(* ----- BD-only-breaking operators -----

   tid is in S under well-sync (T1 and T2 are the same thread) and in P
   under barrier-div (T1 and T2 are distinct threads). A guard / loop
   range mentioning tid is therefore property-asymmetric: the conjunct
   it adds to D is shared across well-sync's two executions but can
   differ between barrier-div's two threads. *)

(* Wrap k.code in If(tid.x < N, body, Skip) for fresh kernel parameter N.
   Well-sync: tid.x and N both in S, so the conjunct (tid.x < N) is the
   same in T1 and T2 — VC preserved.
   Barrier-div: tid.x in P, N in S. With N = 1, tid.x$T1 = 0 (guard true)
   and tid.x$T2 = 1 (guard false), the VC fails. *)
let wrap_tid_if : t =
  {
    name = "wrap_tid_if";
    description = "wrap body in if (tid.x < N) { body } for fresh global N";
    ws_relabel = identity;
    bd_relabel = force_ill;
    apply =
      (fun k ->
        if not (has_sync k.code) then []
        else
          let n = fresh k "mut_N" in
          let cond : Exp.bexp = Exp.n_lt (Var Variable.tid_x) (Var n) in
          [
            {
              k with
              code = If (cond, k.code, Skip);
              global_variables =
                Params.add n C_type.int k.global_variables;
            };
          ]);
  }

(* Wrap k.code in Loop(i ∈ [0, tid.x)) body.
   Well-sync: tid.x in S, so the iteration sequence is identical in T1
   and T2 — [Loop-Div] still classifies the range as divergent (because
   tid.x ∈ L), but the resulting D-conjunct evaluates the same under T1
   and T2.
   Barrier-div: tid.x in P. Two threads with distinct tid.x have distinct
   loop ranges; at any iteration past the smaller of the two, one thread
   is enabled and the other is not — the VC fails. *)
let wrap_tid_loop : t =
  {
    name = "wrap_tid_loop";
    description = "wrap body in for (int i = 0; i < tid.x; ++i) body";
    ws_relabel = identity;
    bd_relabel = force_ill;
    apply =
      (fun k ->
        if not (has_sync k.code) then []
        else
          let i = fresh k "mut_i" in
          let r = Range.make ~lower_bound:(Num 0) i (Var Variable.tid_x) in
          [ { k with code = Loop { range = r; body = k.code } } ]);
  }

let all : t list =
  [
    duplicate_sync;
    prepend_unused_decl;
    wrap_arch_if;
    wrap_uniform_loop;
    wrap_decl_if;
    wrap_decl_loop;
    wrap_tid_if;
    wrap_tid_loop;
  ]
