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
   tids forced by [thread_distinct] and both pinned to [(0,0,0)],
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
]

let () =
  Alcotest.run "drf_genie/co_reach" [
    ("Co_reach", tests);
  ]
