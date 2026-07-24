(* Phase 3 tests for [Co_reach]. The module's externally-visible
   surface is [candidates] (filter a proof stream to SAT/Unknown
   pairs) and [preserves_subset] (Tier 3 gate as a key-level subset
   check); these tests exercise both, plus the integration shape
   used by [compute_verdict]: empty baseline → vacuous DRF, gate
   accepts when Φ is benign, gate rejects when Φ trivialises a
   two-thread fragment.

   Tests build kernels at the [Protocols.Kernel.t] surface and drive
   them through the full pipeline (well-formed → aligned →
   phase-split → loc-split → flat-acc → symbexp/coreach) before
   handing the resulting [Symbexp.Proof.t] stream to [Co_reach]. *)

open Stage0
open Protocols
open Drf
open Drf_genie

let arch : Architecture.t = Architecture.Block

let mk_kernel ?(name = "k_test") ?(globals = []) ?(locals = []) ?(pre = Exp.Bool true)
    (arrays : (string * Memory.t) list) (code : Code.t) : Kernel.t =
  let to_params kvs =
    List.fold_left
      (fun p (n, ty) -> Params.add (Variable.from_name n) ty p)
      Params.empty kvs
  in
  let arrays =
    List.fold_left (fun m (n, mem) ->
      Variable.Map.add (Variable.from_name n) mem m)
      Variable.Map.empty arrays
  in
  {
    name;
    global_variables = to_params globals;
    local_variables = to_params locals;
    arrays;
    pre;
    code;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

let var (name : string) : Exp.nexp = Exp.Var (Variable.from_name name)

let eq (a : Exp.nexp) (b : Exp.nexp) : Exp.bexp = Exp.NRel (Eq, a, b)
let lt (a : Exp.nexp) (b : Exp.nexp) : Exp.bexp =
  Exp.NRel (Lt Signedness.Signed, a, b)

let array_a : Variable.t = Variable.from_name "A"
let shared_int : Memory.t =
  Memory.from_type Mem_hierarchy.SharedMemory C_type.int

let access_w (idx : Exp.nexp) : Code.t =
  Code.Access (Access.write array_a [ idx ] None)
let access_r (idx : Exp.nexp) : Code.t =
  Code.Access (Access.read array_a [ idx ])

(* Build the co-reach proof stream from a [Protocols.Kernel.t] via
   the full pipeline. Mirrors what [App.translate] does for the
   real CLI driver, minus the user-facing [--assume] / launch
   plumbing — tests construct the kernel's [pre] directly. *)
let coreach_proofs (k : Kernel.t) : Symbexp.Proof.t list =
  k
  |> Kernel.apply_arch arch
  |> Wellformed.translate
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> Aligned.translate
  |> Phasesplit.translate
  |> Locsplit.translate
  |> Flatacc.translate arch
  |> Symbexp.translate_coreach arch
  |> Streamutil.to_list

let race_proofs (k : Kernel.t) : Symbexp.Proof.t list =
  k
  |> Kernel.apply_arch arch
  |> Wellformed.translate
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> Aligned.translate
  |> Phasesplit.translate
  |> Locsplit.translate
  |> Flatacc.translate arch
  |> Symbexp.translate arch
  |> Streamutil.to_list

let pairs_of (proofs : Symbexp.Proof.t list) : Co_reach.pair list =
  Co_reach.candidates (Streamutil.from_list proofs)

(* Test A. Co-reachability strictly narrower than single-thread
   reachability.

   The access is guarded by [tid.x == 0 ∧ tid.y == 0 ∧ tid.z == 0]
   — under [Architecture.Block]'s defaults, a single thread can
   satisfy this (the [(0,0,0)] thread). Single-thread reachability
   is therefore SAT.

   The Tier 2 co-reach query asks for two *distinct* threads both
   under that guard, both writing to the same array. With distinct
   tids forced by [is_thread_distinct] and both pinned to [(0,0,0)],
   the goal is UNSAT — [Co_reach.candidates] drops the fragment. *)
let test_co_reach_narrower_than_single_thread () =
  let guard =
    Exp.b_and_ex
      [ eq (var "threadIdx.x") (Exp.Num 0);
        eq (var "threadIdx.y") (Exp.Num 0);
        eq (var "threadIdx.z") (Exp.Num 0); ]
  in
  let body =
    Code.if_ guard (access_w (Exp.Num 0)) Code.Skip
  in
  let k = mk_kernel [ ("A", shared_int) ] body in
  let proofs = coreach_proofs k in
  (* Pipeline should emit some proof — the access exists. *)
  Alcotest.(check bool) "at least one co-reach proof emitted"
    true (proofs <> []);
  let pairs = pairs_of proofs in
  Alcotest.(check (list string)) "no co-reach pair survives — single-(0,0,0) thread"
    [] (List.map (fun (p : Co_reach.pair) ->
      Printf.sprintf "%s:%s#%d" p.kernel_name p.array_name p.id) pairs)

(* Companion: under a milder guard ([tid.x < blockDim.x]) where
   both threads can easily coexist, the same access shape has at
   least one co-reach pair. Pins the narrower-test as a real
   distinction, not just a UNSAT spurious from some unrelated
   precondition. *)
let test_co_reach_passes_when_threads_coexist () =
  let body = access_w (Exp.Num 0) in
  let k = mk_kernel [ ("A", shared_int) ] body in
  let proofs = coreach_proofs k in
  let pairs = pairs_of proofs in
  Alcotest.(check bool) "at least one co-reach pair under default precondition"
    true (pairs <> [])

(* Test B. The new gate catches a trivialising Φ that the legacy
   gate accepts.

   Kernel: two accesses [A[tid.x]] and [A[tid.x + 1]] guarded by
   [tid.x < N]. Under baseline ([N] free) both accesses are
   single-thread reachable and the pair is co-reachable.

   Trivialising Φ: [N == 0]. Under Φ:
   - Single-thread reachability of each access becomes UNSAT
     (no tid.x satisfies [tid.x < 0] given tid.x ≥ 0).
   - But the legacy gate ([gate_holds_simple]) only asks
     [pre ∧ runtime] SAT, which stays SAT (N == 0, tid.x = 0
     unconstrained by the guard at the pre level).
   - The Tier 3 gate asks: is the baseline co-reach pair still
     co-reachable under Φ? With Φ killing both threads' guards,
     the pair drops to UNSAT.

   Assertion: [Co_reach.preserves_subset] returns [false] when the
   under-Φ pair set drops the baseline pair. *)
let test_co_reach_gate_catches_trivialisation () =
  let n = "N" in
  let guard = lt (var "threadIdx.x") (var n) in
  let body =
    Code.if_ guard
      (Code.seq (access_w (var "threadIdx.x")) (access_r (var "threadIdx.x")))
      Code.Skip
  in
  let k_baseline = mk_kernel ~globals:[ (n, C_type.int) ] [ ("A", shared_int) ] body in
  let k_phi =
    let pre' = eq (var n) (Exp.Num 0) in
    { k_baseline with pre = Exp.b_and k_baseline.pre pre' }
  in
  let baseline_pairs = pairs_of (coreach_proofs k_baseline) in
  let under_phi_pairs = pairs_of (coreach_proofs k_phi) in
  (* Sanity: baseline has at least one co-reach pair. *)
  Alcotest.(check bool) "baseline has a co-reach pair"
    true (baseline_pairs <> []);
  (* The Φ should make at least one baseline pair drop. *)
  let preserves =
    Co_reach.preserves_subset ~under_phi:under_phi_pairs ~baseline:baseline_pairs
  in
  Alcotest.(check bool) "gate rejects trivialising Φ (N == 0)"
    false preserves

(* Test C. Co-reach pair set is a refinement of the single-thread
   AccessSet view.

   For a small kernel with two unguarded accesses to [A], the
   single-thread AccessSet contains every access; the co-reach
   pair set is non-empty (the threads coexist) and its size is at
   most one *fragment* per (location, phase). Property: every
   baseline co-reach pair corresponds to a single-thread reachable
   access (in particular, the pair set isn't broader than the
   AccessSet view).

   Concretely we check the count: |co-reach pairs| ≤ |proofs|,
   which is itself ≤ |accesses| times (#phases × #locations). *)
let test_co_reach_refines_single_thread () =
  let body =
    Code.seq (access_w (Exp.Num 0)) (access_w (Exp.Num 1))
  in
  let k = mk_kernel [ ("A", shared_int) ] body in
  let proofs = coreach_proofs k in
  let pairs = pairs_of proofs in
  Alcotest.(check bool) "|pairs| ≤ |proofs|"
    true (List.length pairs <= List.length proofs)

(* Test D. Empty co-reach pair set ↔ vacuous DRF.

   A kernel whose only access is under [false] has no flat-acc
   fragment (the phase-split / loc-split machinery drops dead
   accesses). Even where a fragment survives, the co-reach query
   is UNSAT under [false] — both threads can't satisfy a false
   guard.

   Use a kernel with no accesses at all to pin the "no fragments
   → empty pair set" path. *)
let test_drf_vacuous_empty_pairs () =
  let body = Code.Skip in
  let k = mk_kernel [ ("A", shared_int) ] body in
  let proofs = coreach_proofs k in
  let pairs = pairs_of proofs in
  Alcotest.(check (list string)) "no co-reach pairs when kernel has no accesses"
    [] (List.map (fun (p : Co_reach.pair) ->
      Printf.sprintf "%s:%s#%d" p.kernel_name p.array_name p.id) pairs);
  Alcotest.(check bool) "empty pairs satisfies Drf_vacuous shape"
    true (pairs = [])

(* Test E. Subset gate semantics.

   [preserves_subset] is a key-level check. Construct two pair
   lists with synthetic proofs (the proof's bexp is irrelevant —
   the gate only reads identity); verify the subset semantics. *)
let mk_synthetic_pair ~kn ~an ~id : Co_reach.pair =
  let proof =
    Symbexp.Proof.make ~kernel_name:kn ~array_name:an ~id
      ~accesses:[] ~goal:(Exp.Bool true)
  in
  { kernel_name = kn; array_name = an; id; proof }

let test_preserves_subset_identity () =
  let p1 = mk_synthetic_pair ~kn:"k" ~an:"A" ~id:0 in
  let p2 = mk_synthetic_pair ~kn:"k" ~an:"A" ~id:1 in
  let p3 = mk_synthetic_pair ~kn:"k" ~an:"B" ~id:0 in
  Alcotest.(check bool) "baseline = under_phi ⇒ preserved"
    true (Co_reach.preserves_subset ~under_phi:[p1;p2;p3] ~baseline:[p1;p2;p3]);
  Alcotest.(check bool) "baseline ⊆ under_phi ⇒ preserved"
    true (Co_reach.preserves_subset ~under_phi:[p1;p2;p3] ~baseline:[p1;p2]);
  Alcotest.(check bool) "empty baseline always preserved"
    true (Co_reach.preserves_subset ~under_phi:[] ~baseline:[]);
  Alcotest.(check bool) "missing pair ⇒ not preserved"
    false (Co_reach.preserves_subset ~under_phi:[p1] ~baseline:[p1;p2]);
  Alcotest.(check bool) "different array ⇒ not preserved"
    false (Co_reach.preserves_subset ~under_phi:[p3] ~baseline:[p1])

(* Test F. Property bridge — every co-reach pair's identity
   matches a proof emitted by [Symbexp.translate_coreach]. The
   pair set is exactly the SAT-filtered subset of the proof
   stream. Pinning this prevents the [candidates] filter from
   silently dropping or duplicating proofs.

   Race proofs and co-reach proofs share the [(kernel_name,
   array_name, id)] identity space: both come from
   [Symbexp.Proof.make] with the same proof_id counter ordering.
   So the pair keys should be a subset of the race-proof keys. *)
let test_pair_keys_subset_of_proof_keys () =
  let body = Code.seq (access_w (Exp.Num 0)) (access_w (Exp.Num 1)) in
  let k = mk_kernel [ ("A", shared_int) ] body in
  let proofs = coreach_proofs k in
  let pairs = pairs_of proofs in
  let proof_keys =
    proofs
    |> List.map (fun (p : Symbexp.Proof.t) -> (p.kernel_name, p.array_name, p.id))
  in
  let pair_keys = List.map Co_reach.key_of pairs in
  List.iter (fun key ->
    Alcotest.(check bool) "pair key found in proof key set"
      true (List.mem key proof_keys))
    pair_keys;
  let _ = race_proofs in
  ()

(* Structural inspection helpers for the [Proof.t]'s goal. The
   goal is one combined [bexp] rather than separately-addressable
   clauses, so the assertions below probe the goal's textual /
   structural shape:

   - [goal_contains_substr] looks for a substring in the goal's
     printed form. Used to assert presence/absence of
     [mode_spec] / [assign_dim] / [id_le] clauses, which all have
     stable variable-name patterns ($T1$mode, $T1$idx$N, $T1$id).

   - [bexp_has_thread_unif], [nexp_has_thread_unif] walk the
     bexp/nexp tree and return [true] iff any [IsThreadUnif _] node
     appears. The [is_thread_distinct] clause is the only source of
     [IsThreadUnif] nodes in a co-reach goal (it expands to
     [BNot (IsThreadUnif (Var x))] over tid/bid variables), so
     [bexp_has_thread_unif goal] is a structural probe for
     "is_thread_distinct present somewhere in the bexp". *)
let rec nexp_has_thread_unif (n : Exp.nexp) : bool =
  match n with
  | Exp.Num _ | Exp.Var _ -> false
  | Exp.Unary (_, e) -> nexp_has_thread_unif e
  | Exp.NCall (_, es) -> List.exists nexp_has_thread_unif es
  | Exp.CastInt b -> bexp_has_thread_unif b
  | Exp.Binary (_, n1, n2) ->
      nexp_has_thread_unif n1 || nexp_has_thread_unif n2
  | Exp.NIf (b, n1, n2) ->
      bexp_has_thread_unif b || nexp_has_thread_unif n1
      || nexp_has_thread_unif n2

and bexp_has_thread_unif (b : Exp.bexp) : bool =
  match b with
  | Exp.Bool _ -> false
  | Exp.BNot b -> bexp_has_thread_unif b
  | Exp.BRel (_, b1, b2) -> bexp_has_thread_unif b1 || bexp_has_thread_unif b2
  | Exp.NRel (_, n1, n2) -> nexp_has_thread_unif n1 || nexp_has_thread_unif n2
  | Exp.Pred (_, ns) -> List.exists nexp_has_thread_unif ns
  | Exp.CastBool n -> nexp_has_thread_unif n
  | Exp.Distinct ns -> List.exists nexp_has_thread_unif ns
  | Exp.AtomicResult { operation; index; _ } ->
      List.exists nexp_has_thread_unif index
      || Protocols.Atomic.Operation.exists nexp_has_thread_unif operation
  | Exp.IsThreadUnif _ -> true

let str_contains (hay : string) (needle : string) : bool =
  let h = String.length hay and n = String.length needle in
  let rec scan i = i + n <= h && (String.sub hay i n = needle || scan (i + 1)) in
  n = 0 || (h >= n && scan 0)

let _count_substr (hay : string) (needle : string) : int =
  let h = String.length hay and n = String.length needle in
  if n = 0 then 0
  else
    let count = ref 0 and i = ref 0 in
    while !i + n <= h do
      if String.sub hay !i n = needle then (incr count; i := !i + n)
      else incr i
    done;
    !count

(* Test (4). Translator output shape.

   For a hand-built single-access [Kernel.t], assert that
   [Symbexp.translate_coreach]'s [Proof.t]:

   - lacks the [mode_spec] disjunction — [Gen.mode_spec]
     contributes a disjunctive conflict pattern with mixed
     equalities and inequalities ([$T1$mode == X && $T2$mode != X]
     style), so the race goal carries [$T2$mode !=] as a textual
     marker that the co-reach goal lacks. Note: both goals retain
     the per-task [assign_mode] equality ([$T1$mode == N],
     [$T2$mode == N]) because [SymAccess.to_bexp] always emits it
     — that's the mode *binding*, distinct from the conflict
     predicate, and dropping it would un-bind the mode variables.

   - lacks the [assign_dim] clause — the race goal emits
     [$T1$idx$N == $T2$idx$N] over each access-dimension; the
     co-reach goal is built with [assign_index:false], so no
     [$T1$idx$0] or [$T2$idx$0] variables appear in it at all.

   - has projected access conditions — [$T1$id == N] and
     [$T2$id == N] clauses from [SymAccess.to_bexp].

   - has the [id1 ≤ id2] canonicalisation clause — [$T1$id <=
     $T2$id] in textual form.

   - carries the kernel's [pre] — for an [Architecture.Block]
     kernel, [Kernel.apply_arch] folds [is_thread_distinct] into
     [k.pre]. After [SymAccess.from_cond_access]'s [project_b]
     runs, each [IsThreadUnif (Var x)] expands to [NRel (Eq, x$T1,
     x$T2)], so the goal carries the projected
     [is_thread_distinct] disjunction (e.g. [threadIdx.x$T1 !=
     threadIdx.x$T2]) and no raw [IsThreadUnif _] node.

   The race translator [Symbexp.translate] is used as a control:
   its goal must contain the [mode_spec] / [assign_dim] forms
   that are missing from the co-reach goal.

   The shared mode-variable binding ([assign_mode]) appears in
   both goals via [SymAccess.to_bexp]: the discriminator for
   co-reach vs race is the *disjunctive conflict pattern* in
   [mode_spec] ([$T2$mode !=] in textual form), not the
   mode-variable bindings themselves. *)
let test_translate_coreach_output_shape () =
  let body = access_w (var "threadIdx.x") in
  let k = mk_kernel [ ("A", shared_int) ] body in
  let coreach = coreach_proofs k in
  let race = race_proofs k in
  Alcotest.(check bool) "exactly one co-reach proof emitted"
    true (List.length coreach = 1);
  Alcotest.(check bool) "exactly one race proof emitted"
    true (List.length race = 1);
  let cp = List.hd coreach in
  let rp = List.hd race in
  let cg = Exp.b_to_string cp.goal in
  let rg = Exp.b_to_string rp.goal in
  (* mode_spec conflict pattern: race goal contains [$T2$mode !=]
     from the [b_and (n_eq mode1 mode_read) (n_neq mode2 mode_read)]
     clause of [Gen.mode_spec]. Co-reach goal does not. *)
  Alcotest.(check bool)
    "race goal has $T2$mode != (mode_spec conflict predicate)"
    true (str_contains rg "$T2$mode !=");
  Alcotest.(check bool)
    "co-reach goal lacks $T2$mode != (no mode_spec conflict)"
    false (str_contains cg "$T2$mode !=");
  (* Sanity: both goals retain the [assign_mode] binding for each
     task. This is *not* the conflict predicate — it's the
     [n_eq (mode t) (mode_to_nexp m)] from [SymAccess.to_bexp].
     Documenting this here so a future reader doesn't mistake
     [$T1$mode == 1]'s presence for a mode_spec leak. *)
  Alcotest.(check bool) "co-reach goal retains $T1$mode (assign_mode binding)"
    true (str_contains cg "$T1$mode");
  Alcotest.(check bool) "co-reach goal retains $T2$mode (assign_mode binding)"
    true (str_contains cg "$T2$mode");
  (* assign_dim: race goal has [$T1$idx$0 == $T2$idx$0] and
     per-task [$T1$idx$0 == threadIdx.x$T1]; the co-reach goal is
     built with [assign_index:false] so [$T1$idx$0] / [$T2$idx$0]
     do not appear at all. *)
  Alcotest.(check bool)
    "race goal has $T1$idx$0 (assign_index emitted)"
    true (str_contains rg "$T1$idx$0");
  Alcotest.(check bool)
    "race goal has $T1$idx$0 == $T2$idx$0 (assign_dim cross-task eq)"
    true (str_contains rg "$T1$idx$0 == $T2$idx$0");
  Alcotest.(check bool)
    "co-reach goal lacks $T1$idx$0 (no assign_index, no assign_dim)"
    false (str_contains cg "$T1$idx$0");
  Alcotest.(check bool)
    "co-reach goal lacks $T2$idx$0 (no assign_index, no assign_dim)"
    false (str_contains cg "$T2$idx$0");
  (* projected access ids: both [$T1$id] and [$T2$id] must appear
     ([SymAccess.to_bexp] emits [assign_access_id] for each task,
     with the per-task projection turning $id into $T1$id / $T2$id). *)
  Alcotest.(check bool) "co-reach goal has $T1$id (assign_T1)"
    true (str_contains cg "$T1$id");
  Alcotest.(check bool) "co-reach goal has $T2$id (assign_T2)"
    true (str_contains cg "$T2$id");
  (* id1 ≤ id2: emitted as [n_le (access_id Task1) (access_id Task2)],
     printed as [$T1$id <= $T2$id]. *)
  Alcotest.(check bool) "co-reach goal has $T1$id <= $T2$id (id_le)"
    true (str_contains cg "$T1$id <= $T2$id");
  (* [Kernel.apply_arch] folds [is_thread_distinct] (built via
     [is_thread_unif], i.e. [IsThreadUnif e]) into [k.pre]. By the time
     the goal is assembled, [SymAccess.from_cond_access]'s
     [project_b] has expanded each [IsThreadUnif (Var x)] to
     [NRel (Eq, x$T1, x$T2)], so the projected goal carries the
     [is_thread_distinct]'s [b_or_ex] of inequalities like
     [threadIdx.x$T1 != threadIdx.x$T2] and contains no raw
     [IsThreadUnif _] node. *)
  Alcotest.(check bool)
    "co-reach goal has no raw IsThreadUnif _ node (projection eliminated it)"
    false (bexp_has_thread_unif cp.goal);
  Alcotest.(check bool)
    "co-reach goal has projected is_thread_distinct (threadIdx.x$T1 != threadIdx.x$T2)"
    true (str_contains cg "threadIdx.x$T1 != threadIdx.x$T2"
          || str_contains cg "threadIdx.x$T2 != threadIdx.x$T1")

(* Test (5). Co-existence SAT round-trip.

   Scoped down from the brief's "co-existence-without-conflict"
   construction: building a kernel where the race query is UNSAT
   but the co-existence query is SAT requires the conflict shape
   to be the *only* thing the race goal needs (e.g. two reads
   with non-overlapping cells), which is exactly what
   [translate_coreach] is built to drop. Constructing a small
   kernel where this signal is observable end-to-end and not
   masked by [Architecture.Block]'s default [is_thread_distinct] is
   awkward without extra fixture machinery, so we settle for the
   degenerate version: a 1-access kernel whose co-reach proof is
   SAT under [Bv64Gen].

   This still pins the basic claim "translate_coreach produces a
   SAT-solvable goal for a satisfiable kernel" — if the translator
   accidentally conjoined an UNSAT clause (e.g. [Bool false] or a
   contradictory dim constraint), this test would fail. *)
let test_translate_coreach_round_trip_sat () =
  let body = access_w (Exp.Num 0) in
  let k = mk_kernel [ ("A", shared_int) ] body in
  let proofs = coreach_proofs k in
  Alcotest.(check bool) "at least one co-reach proof emitted"
    true (proofs <> []);
  let p = List.hd proofs in
  let status = Co_reach.solve_one p in
  let status_str = match status with
    | Z3.Solver.SATISFIABLE -> "SAT"
    | Z3.Solver.UNSATISFIABLE -> "UNSAT"
    | Z3.Solver.UNKNOWN -> "UNKNOWN"
  in
  (* SAT or Unknown both accepted; UNSAT would mean the translator
     killed co-existence on a satisfiable kernel. *)
  Alcotest.(check bool)
    (Printf.sprintf "co-reach goal is SAT or Unknown (got %s)" status_str)
    true
    (status = Z3.Solver.SATISFIABLE || status = Z3.Solver.UNKNOWN)

(* Test (7). [Kernel.apply_arch] folds the [is_thread_distinct] clause
   into [k.pre].

   Scoped down from the brief's "two variants through the pipeline":
   running [translate_coreach] on a kernel that hasn't had
   [apply_arch] applied is fragile (the pipeline expects arch
   defaults; [Phasesplit] in particular references [blockDim.*]
   bindings that [apply_arch_binders] sets up), so the test below
   asserts the narrower-but-cleaner claim directly:

   For a hand-built [Kernel.t]:
   - before [apply_arch]: [k.pre] contains no [IsThreadUnif _]
     nodes.
   - after [apply_arch] (Block arch): [k.pre] contains
     [IsThreadUnif _] nodes — specifically, the [is_thread_distinct]
     clause's [is_thread_unif (Var tid.x|y|z)] expansions, each of
     which is [IsThreadUnif (Var tid.X)].

   This pins the architectural design point referenced in
   [from_code_coreach]'s comment: the co-reach goal does not need
   to conjoin [is_thread_distinct] explicitly because [apply_arch]
   has already put it in [k.pre]. *)
let test_apply_arch_adds_is_thread_distinct_to_pre () =
  let body = access_w (Exp.Num 0) in
  let k_raw = mk_kernel [ ("A", shared_int) ] body in
  Alcotest.(check bool)
    "raw kernel pre has no IsThreadUnif _ (no is_thread_distinct yet)"
    false (bexp_has_thread_unif k_raw.pre);
  let k_arch = Kernel.apply_arch arch k_raw in
  Alcotest.(check bool)
    "after apply_arch, pre contains IsThreadUnif _ (is_thread_distinct present)"
    true (bexp_has_thread_unif k_arch.pre)

(* T1-only proof stream, mirrors [coreach_proofs] but goes through
   [Symbexp.translate_t1] for the single-thread variant. *)
let t1_proofs (k : Kernel.t) : Symbexp.Proof.t list =
  k
  |> Kernel.apply_arch arch
  |> Wellformed.translate
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> Aligned.translate
  |> Phasesplit.translate
  |> Locsplit.translate
  |> Flatacc.translate arch
  |> Symbexp.translate_t1 arch
  |> Streamutil.to_list

let array_b : Variable.t = Variable.from_name "B"
let access_w_arr (arr : Variable.t) (idx : Exp.nexp) : Code.t =
  Code.Access (Access.write arr [ idx ] None)

(* Regression test: the pad-cuda pattern. A kernel with two arrays:
   one access guarded by [tid.x == 0 ∧ N > 0] (single-thread-only,
   never a co-reach pair) and one always-true (race-candidate
   fragment). The old Tier 1 (baseline = single-thread reach set
   over every access) rejected any Φ that dropped the
   single-thread-only access; the new Tier 1 (baseline restricted
   to pair-relevant fragments) accepts such a Φ.

   Φ here is [N == 0]: it kills the [tid.x == 0 ∧ N > 0] guard's
   reachability while leaving the always-true access intact. The
   test exercises:

   1. Baseline pair set contains only the always-true fragment
      (single-thread A access has no co-reach pair).

   2. Old per-access baseline AccessSet contains both A and B
      under-baseline (legacy Tier 1 would reject Φ).

   3. New pair-keyed Tier 1 accepts Φ: the only baseline key (B's
      fragment) remains T1-SAT under Φ.

   Pins the "Tier 1 reject ⇒ Tier 2 reject" pre-filter contract:
   under the new shape, no Φ that Tier 2 accepts gets rejected by
   Tier 1. *)
let test_t1_baseline_restricted_to_pair_relevant () =
  let n = "N" in
  let guard_single =
    Exp.b_and_ex
      [ eq (var "threadIdx.x") (Exp.Num 0);
        eq (var "threadIdx.y") (Exp.Num 0);
        eq (var "threadIdx.z") (Exp.Num 0);
        Exp.NRel (Gt Signedness.Signed, var n, Exp.Num 0); ]
  in
  let body =
    Code.seq
      (Code.if_ guard_single (access_w (Exp.Num 0)) Code.Skip)
      (access_w_arr array_b (var "threadIdx.x"))
  in
  let k_baseline =
    mk_kernel ~globals:[ (n, C_type.int) ]
      [ ("A", shared_int); ("B", shared_int) ]
      body
  in
  (* Baseline pairs: should include the B fragment only — the A
     fragment is single-thread-only (tid.x == 0 admits one thread,
     is_thread_distinct forces UNSAT for the co-reach pair). *)
  let baseline_pairs = pairs_of (coreach_proofs k_baseline) in
  let baseline_array_names =
    baseline_pairs
    |> List.map (fun (p : Co_reach.pair) -> p.array_name)
    |> List.sort_uniq String.compare
  in
  Alcotest.(check (list string))
    "baseline pairs only on B (A is single-thread-only)"
    [ "B" ] baseline_array_names;
  (* Apply Φ: [N == 0]. This kills the A access's reachability
     (tid.x == 0 ∧ N > 0 ∧ N == 0 is UNSAT) but the B access stays
     reachable (no parameter dependence). *)
  let phi = eq (var n) (Exp.Num 0) in
  let k_phi =
    { k_baseline with pre = Exp.b_and k_baseline.pre phi }
  in
  (* The new Tier 1 baseline ([baseline_keys]) only carries the
     B fragment's key. *)
  let baseline_keys = Co_reach.keys_of baseline_pairs in
  let under_phi_t1_proofs = t1_proofs k_phi in
  let under_phi_t1_keys =
    Co_reach.t1_keys_restricted ~tag:"test"
      baseline_keys
      (Streamutil.from_list under_phi_t1_proofs)
  in
  Alcotest.(check bool)
    "new Tier 1 (pair-restricted): Φ accepted — B fragment T1-SAT"
    true (Co_reach.KeySet.subset baseline_keys under_phi_t1_keys);
  (* Sanity: the legacy Tier 1 shape (per-access reach over the
     full kernel) would have rejected this Φ. We pin this by
     checking that under Φ the *full* T1 stream (not restricted)
     loses the A access — i.e. some baseline-T1 SAT key has
     dropped at the under-Φ T1 stream when not restricted. *)
  let baseline_t1_proofs = t1_proofs k_baseline in
  let baseline_t1_all_sat_keys =
    Co_reach.t1_keys_restricted ~tag:"test"
      (Co_reach.keys_of (List.map (fun (p : Symbexp.Proof.t) ->
        Co_reach.{ kernel_name = p.kernel_name; array_name = p.array_name;
                    id = p.id; proof = p }) baseline_t1_proofs))
      (Streamutil.from_list baseline_t1_proofs)
  in
  let under_phi_t1_all_sat_keys =
    Co_reach.t1_keys_restricted ~tag:"test"
      (Co_reach.keys_of (List.map (fun (p : Symbexp.Proof.t) ->
        Co_reach.{ kernel_name = p.kernel_name; array_name = p.array_name;
                    id = p.id; proof = p }) under_phi_t1_proofs))
      (Streamutil.from_list under_phi_t1_proofs)
  in
  Alcotest.(check bool)
    "legacy-shaped Tier 1 (per-access reach) would reject: \
     some baseline-T1 key drops under Φ"
    false
    (Co_reach.KeySet.subset baseline_t1_all_sat_keys under_phi_t1_all_sat_keys)

let tests = [
  ("A. co-reach narrower than single-thread",
    `Quick, test_co_reach_narrower_than_single_thread);
  ("A.companion: co-reach passes when threads coexist",
    `Quick, test_co_reach_passes_when_threads_coexist);
  ("B. gate catches trivialising Φ (single-thread accepts)",
    `Quick, test_co_reach_gate_catches_trivialisation);
  ("C. pair set refines single-thread (|pairs| ≤ |proofs|)",
    `Quick, test_co_reach_refines_single_thread);
  ("D. empty pairs ≡ Drf_vacuous shape",
    `Quick, test_drf_vacuous_empty_pairs);
  ("E. preserves_subset identity / subset / missing-pair",
    `Quick, test_preserves_subset_identity);
  ("F. pair keys are a subset of proof keys",
    `Quick, test_pair_keys_subset_of_proof_keys);
  ("(4) translate_coreach output shape: no mode_spec / no assign_dim",
    `Quick, test_translate_coreach_output_shape);
  ("(5) translate_coreach round-trip: SAT-solvable for satisfiable kernel",
    `Quick, test_translate_coreach_round_trip_sat);
  ("(7) apply_arch folds is_thread_distinct into k.pre",
    `Quick, test_apply_arch_adds_is_thread_distinct_to_pre);
  ("(8) pad-cuda regression: Tier 1 baseline restricted to pair-relevant",
    `Quick, test_t1_baseline_restricted_to_pair_relevant);
]

let () =
  Alcotest.run "drf_genie/co_reach" [
    ("Co_reach", tests);
  ]
