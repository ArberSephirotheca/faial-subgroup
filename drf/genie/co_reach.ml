(* Tier 2/3 reachability: pair-level co-existence of two threads
   under a kernel's preconditions, derived from the symbexp proof
   stream.

   A single [Symbexp.Proof.t] encodes one race-query fragment of one
   kernel — a location/phase slice. The DRF solver evaluates its
   [goal], which conjoins [pre ∧ assign_T1 ∧ assign_T2 ∧ same_addr ∧
   mode_spec ∧ id_le]. A SAT goal is a race; UNSAT is DRF for that
   fragment.

   Co-reachability strips the conflict shape. [Symbexp.translate_coreach]
   produces a parallel stream where each proof's [goal] is
   [pre ∧ assign_T1 ∧ assign_T2 ∧ id_le ∧ is_thread_distinct] — two
   distinct threads, each reaching some access in the fragment,
   neither required to collide nor to use a conflicting mode. SAT
   here means "the two-thread universe for this fragment is
   non-empty under the precondition"; UNSAT means the precondition
   killed the co-existence (e.g. [blockDim.x == 1] under a
   tid-discriminating guard).

   Tier 3 (the per-CEGAR-round gate) compares two pair sets:
   - [baseline] — pairs that are co-reachable under the kernel's
     pre alone, before any abductive Φ is added.
   - [under_phi] — pairs that remain co-reachable after Φ is folded
     into the kernel's pre.

   The gate accepts when [baseline ⊆ under_phi] keyed by
   [(kernel_name, array_name, id)]: every baseline race-candidate
   fragment's two-thread reachability is preserved. If a baseline
   pair drops out under Φ, Φ trivialised it (the abductive
   "cleared" the race by making one of the two threads vanish, not
   by removing the conflict) and the gate rejects.

   The accept-on-Unknown stance matches the existing CEGAR gate:
   when Z3 returns Unknown on the under-Φ co-reach query, we treat
   the pair as preserved. Rejecting on Unknown would let an
   incomplete-solver timeout falsely flag a legitimate clearance. *)

open Stage0
open Protocols
open Drf

(* Pair identity. One pair per [Symbexp.Proof.t] in the kernel's
   stream — the proof's [(kernel_name, array_name, id)] is the
   natural compound key.

   [proof] carries the co-reach goal (built via
   [Symbexp.translate_coreach]) so the gate can re-evaluate the
   same fragment under different preconditions without recomputing
   the symbexp encoding. *)
type pair = {
  kernel_name : string;
  array_name : string;
  id : int;
  proof : Symbexp.Proof.t;
}

let key_of (p : pair) : string * string * int =
  (p.kernel_name, p.array_name, p.id)

module Key = struct
  type t = string * string * int
  let compare ((kn1, an1, i1) : t) ((kn2, an2, i2) : t) : int =
    let c = String.compare kn1 kn2 in
    if c <> 0 then c
    else
      let c = String.compare an1 an2 in
      if c <> 0 then c
      else Int.compare i1 i2
end

module KeySet = Set.Make (Key)

(* Tier 2 candidate extraction. Consume a co-reach proof stream,
   solve each proof for satisfiability, and keep the SAT/Unknown
   ones as pairs.

   UNSAT proofs are dropped — no co-reachability for that fragment
   under the pre. Unknown is accepted (matches the CEGAR gate's
   accept-on-Unknown stance): an Unknown gate result on a fragment
   is folded into the pair set; if that fragment was in the
   baseline, the under-Φ subset check still passes; if it wasn't,
   the under-Φ set gains a conservative entry. Either way, the
   gate stays permissive.

   Encoder choice: go straight to [Bv64Gen]. The co-reach goal's
   bit-width assumptions match the BV encoder, and bypassing the
   try-IntGen-then-fall-back-to-BV dance keeps the encoding cost
   bounded.

   The [Z3.Error] catch mirrors [compute_verdict]'s top-level
   handler: when Z3 exhausts its memory cap (default ~6 GB) the
   binding raises [Z3.Error]; we propagate by treating the offending
   proof as a non-candidate (its co-reach status is opaque, so it
   adds no constraint to the gate). *)

(* When [FAIAL_COREACH_DUMP] is set (and not "0"/empty), each
   per-proof solve emits one line on stderr tagged with the call
   site label ([baseline] or [under-phi]) so a side-by-side diff
   identifies which pairs Φ drops. Mirrors the [FAIAL_PHASE_LOG]
   convention in [Phase_timer]. *)
let dump_enabled : bool =
  match Sys.getenv_opt "FAIAL_COREACH_DUMP" with
  | None | Some "" | Some "0" -> false
  | _ -> true

let status_to_string : Z3.Solver.status -> string = function
  | Z3.Solver.SATISFIABLE -> "sat"
  | Z3.Solver.UNSATISFIABLE -> "unsat"
  | Z3.Solver.UNKNOWN -> "unknown"

let dump_line ~(tag : string) (p : Symbexp.Proof.t)
    (status : Z3.Solver.status) : unit =
  if dump_enabled then begin
    Printf.eprintf
      "[coreach-dump] tag=%s kernel=%s array=%s id=%d result=%s goal=%s\n"
      tag p.kernel_name p.array_name p.id
      (status_to_string status)
      (Exp.b_to_string (Formula.to_bexp p.formula));
    flush stderr
  end

(* Per-proof [Z3.mk_context] / [Z3.Solver.mk_simple_solver]
   allocation, NOT a shared context across the per-stream sweep.
   The shape looks wasteful next to [Reachability.make_check_slot]
   in [reachability.ml], which allocates one ctx + solver per
   per-kernel sweep and runs the per-class loop via
   [push] / [add] / [check] / [pop]: gate/solve there sits at
   ~18 ms/call while coreach-solve here sits at 130-140 ms/call,
   so the temptation is to fold this loop onto the same pattern.
   Don't.

   Instrumentation: split a per-call [genie/co-reach-solve] into
   sub-phases [encode] (Bv64Gen.b_to_expr) and [sat]
   (Z3.Solver.check). On a 60s sample of a coreach-heavy kernel
   (1663+ per-stream proofs), [encode] is 0.074s cumulative;
   [sat] is 54.6s; ctx allocation plus residual is ~2.5s. The
   per-call cost is the Z3 SAT check itself, intrinsic to the
   per-proof goal and not amortisable by sharing the context.

   Variants tried on the known-good corpus:

   (a) Shared ctx + shared solver, push/check/pop per proof
       (mirrors make_check_slot). Helped the coreach-heavy
       kernel: per-call dropped from ~143 ms to ~126 ms
       (~15% throughput). Hurt several others: a kernel with
       only 16 coreach proofs went from 22s to 38s wall-clock
       (+70%); every Z3 phase ([co-reach-solve], [t1-solve],
       [baseline-coreach], [gate]) doubled per-call. Z3 in
       incremental push/pop mode applies different
       preprocessing than fresh-context one-shot, and on the
       cross-thread BV goals this incremental preprocessing is
       slower per goal. Corpus total: +12% wall-clock.

   (b) Shared ctx, fresh solver per proof, no push/pop.
       Recovered the regressed kernels but lost the heavy-
       kernel win: sharing the context alone amortises a small
       fraction of total per-call cost, and creating a fresh
       solver per proof inside a shared context still pays most
       of (a)'s allocation. Several moderate-proof-count kernels
       regressed ~10-15%.

   Neither variant is a net corpus win. Per-proof ctx stays.
   To make coreach-heavy kernels faster, attack the SAT check
   itself: parallel solving across independent per-proof
   queries, a tactic tuned for cross-thread BV goals, or an
   upstream pre-filter that decides proofs whose verdict no
   [--assume] can shift without invoking Z3 (mirroring
   [Reachability.is_parameter_touching]'s structural
   classification). *)
let solve_one ?(timeout : int option = None) (p : Symbexp.Proof.t)
    : Z3.Solver.status =
  let options =
    [ ("model", "false"); ("proof", "false") ]
    @ (match timeout with
       | Some t -> [ ("timeout", string_of_int t) ]
       | None -> [])
  in
  let ctx = Z3.mk_context options in
  let solver = Z3.Solver.mk_simple_solver ctx in
  let expr = Gen_z3.Bv64Gen.b_to_expr ctx p.formula in
  Z3.Solver.add solver [ expr ];
  Z3.Solver.check solver []

let candidates ?(tag : string = "baseline") ?(timeout : int option = None)
    ?(logic : string option = None)
    (stream : Symbexp.Proof.t Streamutil.stream) : pair list =
  let _ = logic in
  stream
  |> Streamutil.to_list
  |> List.filter_map (fun (p : Symbexp.Proof.t) ->
    let status =
      try
        Phase_timer.measure "genie/co-reach-solve" (fun () ->
          solve_one ~timeout p)
      with Z3.Error _ -> Z3.Solver.UNKNOWN
    in
    dump_line ~tag p status;
    match status with
    | Z3.Solver.SATISFIABLE | Z3.Solver.UNKNOWN ->
      Some {
        kernel_name = p.kernel_name;
        array_name = p.array_name;
        id = p.id;
        proof = p;
      }
    | Z3.Solver.UNSATISFIABLE -> None)

(* Tier 3 gate. Returns [true] when every [baseline] pair is still
   co-reachable under Φ — i.e. its key is in [under_phi]. Returns
   [false] when at least one baseline pair drops out under Φ,
   signalling that Φ trivialised the kernel's two-thread universe
   for some fragment.

   Keyed on [(kernel_name, array_name, id)]: the proof carries the
   full bexp but the gate only needs identity, since both [baseline]
   and [under_phi] were computed under the same arch and same flat-
   acc plumbing — same proof identities, different precondition
   sets. *)
let preserves_subset ~(under_phi : pair list) ~(baseline : pair list) : bool =
  let under_set =
    List.fold_left (fun acc p -> KeySet.add (key_of p) acc)
      KeySet.empty under_phi
  in
  List.for_all (fun p -> KeySet.mem (key_of p) under_set) baseline

(* Targeted gate. Solve a proof stream under Φ but only emit pairs
   whose key is in [baseline_keys]; proofs outside that set don't
   need to be evaluated because they can't affect the subset check.

   The full [candidates] is used to build the baseline pair set once
   per kernel; this variant is for the per-CEGAR-round gate, where
   the baseline is fixed and pairs outside it are dead work. The
   per-round cost on a kernel with N baseline pairs is therefore
   one SAT call per baseline pair, not one per stream proof. *)
let candidates_restricted ?(tag : string = "under-phi")
    ?(timeout : int option = None)
    (baseline_keys : KeySet.t)
    (stream : Symbexp.Proof.t Streamutil.stream) : pair list =
  stream
  |> Streamutil.to_list
  |> List.filter_map (fun (p : Symbexp.Proof.t) ->
    let key = (p.kernel_name, p.array_name, p.id) in
    if not (KeySet.mem key baseline_keys) then None
    else
      let status =
        try
          Phase_timer.measure "genie/co-reach-solve" (fun () ->
            solve_one ~timeout p)
        with Z3.Error _ -> Z3.Solver.UNKNOWN
      in
      dump_line ~tag p status;
      match status with
      | Z3.Solver.SATISFIABLE | Z3.Solver.UNKNOWN ->
        Some {
          kernel_name = p.kernel_name;
          array_name = p.array_name;
          id = p.id;
          proof = p;
        }
      | Z3.Solver.UNSATISFIABLE -> None)

(* Build [baseline_keys : KeySet.t] from a [baseline : pair list]. *)
let keys_of (pairs : pair list) : KeySet.t =
  List.fold_left (fun acc p -> KeySet.add (key_of p) acc)
    KeySet.empty pairs

(* Tier 1 pre-filter (pair-aware). Given a stream of T1-only proofs
   ([Symbexp.translate_t1]) and the baseline pair-key set, solve the
   T1 goal for each proof whose key is in [baseline_keys] and collect
   the SAT/Unknown keys. The caller compares this to [baseline_keys]
   itself.

   Sound as a Tier 2 pre-filter: a baseline pair preserved at Tier 2
   has both T1 and T2 conjuncts SAT under Φ, so its T1-only conjunct
   is also SAT — i.e. T1-UNSAT implies Tier 2 also drops the pair.
   The pre-filter therefore never rejects a Φ that Tier 2 would
   accept, and rejecting at Tier 1 is a witness that Tier 2 would
   reject too.

   Cheaper than [candidates_restricted]: the T1 goal drops the
   second-thread conjunct and the [id_le] canonicalisation, so each
   per-proof solve is over a smaller bexp. *)
let t1_keys_restricted ?(tag : string = "tier1")
    ?(timeout : int option = None)
    (baseline_keys : KeySet.t)
    (stream : Symbexp.Proof.t Streamutil.stream) : KeySet.t =
  stream
  |> Streamutil.to_list
  |> List.fold_left (fun acc (p : Symbexp.Proof.t) ->
    let key = (p.kernel_name, p.array_name, p.id) in
    if not (KeySet.mem key baseline_keys) then acc
    else
      let status =
        try
          Phase_timer.measure "genie/t1-solve" (fun () ->
            solve_one ~timeout p)
        with Z3.Error _ -> Z3.Solver.UNKNOWN
      in
      dump_line ~tag p status;
      match status with
      | Z3.Solver.SATISFIABLE | Z3.Solver.UNKNOWN -> KeySet.add key acc
      | Z3.Solver.UNSATISFIABLE -> acc)
    KeySet.empty

(* Pretty-print a pair set as a sorted list of keys for debug /
   test output. *)
let keys_to_string (pairs : pair list) : string =
  pairs
  |> List.map (fun p ->
    let (kn, an, id) = key_of p in
    Printf.sprintf "%s:%s#%d" kn an id)
  |> List.sort String.compare
  |> String.concat ", "
