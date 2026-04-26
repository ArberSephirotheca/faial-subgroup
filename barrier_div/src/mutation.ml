open Protocols

(* Mutation operators for the well-synchronization analysis dataset.

   Each operator is a pure transformation Kernel.t -> Kernel.t list. The
   Label assigned to a mutant is determined from the operator's [effect]
   field applied to the input label; the rule-based justification of each
   effect lives in the operator's comment and references the inference
   rules in documentation/well-sync.md. *)

module Label = struct
  type t = WellSync | IllSync

  let to_string : t -> string = function
    | WellSync -> "well_sync"
    | IllSync -> "ill_sync"
end

type t = {
  name : string;
  description : string;
  relabel : Label.t -> Label.t;
  apply : Kernel.t -> Kernel.t list;
}

(* ----- helpers ----- *)

let kernel_used_vars (k : Kernel.t) : Variable.Set.t =
  Variable.Set.union
    (Kernel.parameter_set k)
    (Code.free_names k.code Variable.Set.empty)

let fresh (k : Kernel.t) (base : string) : Variable.t =
  Variable.fresh (kernel_used_vars k) (Variable.from_name base)

(* ----- W-preserving operators -----

   For each, the input label and the output label coincide. The argument
   is in two parts: (a) for [WellSync] inputs the property (WS-Γ) at every
   pre-existing barrier site is unaffected; (b) for [IllSync] inputs the
   pre-existing counter-model still witnesses non-determinism in the
   mutant. Both fall out of monotonicity of the analysis state — the
   operators only weaken or extend Γ in ways that preserve every
   pre-existing obligation's truth value. *)

(* Replace each Sync s with Seq(Sync s, Sync s).
   Both occurrences emit the same Γ ⊢ sync ↝ obl(Γ); the obligation set
   doubles up on each site without changing per-site truth. *)
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
    relabel = (fun l -> l);
    apply = (fun k -> [ { k with code = dup k.code } ]);
  }

(* Wrap k.code in Decl(x, body) for fresh x not mentioned in body.
   [Decl] places x in L ∩ P, but no guard reaches a sync mentions x, so
   D and U at every existing site are unchanged. *)
let prepend_unused_decl : t =
  {
    name = "prepend_unused_decl";
    description = "wrap body in Decl(x, body) for fresh unused x";
    relabel = (fun l -> l);
    apply =
      (fun k ->
        let x = fresh k "mut_unused" in
        [ { k with code = Decl { var = x; ty = C_type.int; body = k.code } } ]);
  }

(* Wrap k.code in If(b, body, Skip) where fv(b) ⊆ arch.
   [If-Div] (since arch ⊆ L) extends D with b. b mentions only S-variables,
   so its truth value coincides in σ and σ' under σ ≈_P σ'; the new
   conjunct is a function of shared state and (WS-Γ) is preserved. *)
let wrap_arch_if : t =
  let cond : Exp.bexp = Exp.n_lt (Var Variable.tid_x) (Var Variable.bdim_x) in
  {
    name = "wrap_arch_if";
    description = "wrap body in if (tid.x < blockDim.x) { body }";
    relabel = (fun l -> l);
    apply = (fun k -> [ { k with code = If (cond, k.code, Skip) } ]);
  }

(* Wrap k.code in Loop(i ∈ [0, N)) for fresh kernel parameter N and
   binder i. [Loop-Unif] adds i to S and (0 ≤ i ∧ i < N) to U; D is
   unchanged. The new uniform conjunct constrains both σ and σ' equally
   given that they share i. *)
let wrap_uniform_loop : t =
  {
    name = "wrap_uniform_loop";
    description = "wrap body in for(int i = 0; i < N; ++i) for fresh global N";
    relabel = (fun l -> l);
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

(* ----- W-breaking operators -----

   For [WellSync] input these produce [IllSync] output: a fresh
   projectable variable enters D, with the mutant's verification
   condition admitting a counter-model. For [IllSync] input the mutant
   is still ill-synchronised — the pre-existing counter-model survives,
   strengthened by the new constraint. The effect is therefore "force
   to IllSync" regardless of input. *)

(* Wrap k.code in Decl(x, If(x > 0, body, Skip)) for fresh x.
   [Decl] places x in L ∩ P; [If-Div] adds (x > 0) to D. Take any
   pair of valuations agreeing on every variable except x, with
   x ↦ 1 in one and x ↦ 0 in the other: D differs and (WS-Γ) fails. *)
let wrap_decl_if : t =
  {
    name = "wrap_decl_if";
    description = "wrap body in { int x; if (x > 0) body }";
    relabel = (fun _ -> Label.IllSync);
    apply =
      (fun k ->
        let x = fresh k "mut_x" in
        let cond = Exp.n_gt (Var x) (Num 0) in
        let inner : Code.t = If (cond, k.code, Skip) in
        [
          {
            k with
            code = Decl { var = x; ty = C_type.int; body = inner };
          };
        ]);
  }

(* Wrap k.code in Decl(n, Loop(i ∈ [0, n)) body) for fresh n, i.
   [Decl] places n in L ∩ P; [Loop-Div] places i in L ∩ S and adds
   (0 ≤ i ∧ i < n) to D. With n ↦ k > 0 in one valuation and n ↦ 0
   in the other (i shared), D differs and (WS-Γ) fails. *)
let wrap_decl_loop : t =
  {
    name = "wrap_decl_loop";
    description =
      "wrap body in { int n; for (int i = 0; i < n; ++i) body }";
    relabel = (fun _ -> Label.IllSync);
    apply =
      (fun k ->
        let n = fresh k "mut_n" in
        let used' = Variable.Set.add n (kernel_used_vars k) in
        let i = Variable.fresh used' (Variable.from_name "mut_i") in
        let r = Range.make ~lower_bound:(Num 0) i (Var n) in
        let inner : Code.t = Loop { range = r; body = k.code } in
        [
          {
            k with
            code = Decl { var = n; ty = C_type.int; body = inner };
          };
        ]);
  }

let all : t list =
  [
    duplicate_sync;
    prepend_unused_decl;
    wrap_arch_if;
    wrap_uniform_loop;
    wrap_decl_if;
    wrap_decl_loop;
  ]
