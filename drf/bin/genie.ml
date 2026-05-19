open Stage0
open Protocols
open Protocols_parsing
open Drf
open Drf_genie
open Cmdliner

type verdict_source = Source_baseline | Source_abductive | Source_blanket

let source_to_string = function
  | Source_baseline -> "baseline"
  | Source_abductive -> "abductive"
  | Source_blanket -> "blanket"

type verdict =
  | Drf of { source : verdict_source; assumes : (string * Exp.bexp list) list }
  | Drf_vacuous
  | Racy

(* [--assume "BEXP"] or [--assume "KERNEL:BEXP"]. The optional prefix
   targets a single kernel by name; without it, the clause applies
   to every kernel whose declared params plus the launch-config dims
   cover the clause's free variables. The prefix must look like a C
   identifier; [:] is reserved as the separator because the bexp
   grammar uses no [:] tokens. *)
let conv_assume =
  let looks_like_ident s =
    s <> ""
    && String.for_all (fun c ->
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c = '_')
      s
  in
  let parse_bexp s =
    match Parsers.BExpParser.of_string s with
    | Ok b -> Ok b
    | Error msg -> Error (`Msg msg)
  in
  let parse s =
    match String.index_opt s ':' with
    | None ->
      (match parse_bexp s with
       | Ok b -> Ok (None, b)
       | Error e -> Error e)
    | Some i ->
      let prefix = String.sub s 0 i |> String.trim in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      if looks_like_ident prefix then
        match parse_bexp rest with
        | Ok b -> Ok (Some prefix, b)
        | Error e -> Error e
      else
        (match parse_bexp s with
         | Ok b -> Ok (None, b)
         | Error e -> Error e)
  in
  let print ppf = function
    | (Some n, b) -> Format.fprintf ppf "%s:%s" n (Exp.b_to_string b)
    | (None, b) -> Format.fprintf ppf "%s" (Exp.b_to_string b)
  in
  Arg.conv (parse, print)

let conv_tactic =
  let parse s =
    match Parsers.TacticParser.of_string s with
    | Ok t -> Ok t
    | Error msg -> Error (`Msg msg)
  in
  let print ppf (t : Gen_z3.Tactic.t) =
    Format.fprintf ppf "%s" (Gen_z3.Tactic.to_string t)
  in
  Arg.conv (parse, print)

let default_solve_tactic : Gen_z3.Tactic.t =
  Gen_z3.Tactic.and_then_ex
    [ Tactic "simplify"; Tactic "solve-eqs"; Tactic "bv" ]

let int_params (k : Kernel.t) : Variable.t list =
  Params.to_list k.global_variables
  |> List.filter_map (fun (v, ty) ->
      if C_type.is_int ty && not (Variable.is_launch_config v)
      then Some v else None)

(* Reduce a [bexp]'s free-variable set to a single signedness by
   "any-unsigned wins" — matches C's usual arithmetic conversions
   for the operator that would coerce these operands. *)
let bexp_signedness (sign : Variable.t -> Signedness.t) (b : Exp.bexp)
    : Signedness.t =
  let fvs = Exp.b_free_names b Variable.Set.empty in
  Variable.Set.fold (fun v acc ->
    match acc, sign v with
    | Signedness.Unsigned, _ | _, Signedness.Unsigned -> Signedness.Unsigned
    | _, _ -> Signedness.Signed)
    fvs Signedness.Signed

let all_safe (rs : Analysis.t list) : bool =
  List.for_all Analysis.is_safe rs

let access_set_of (app : App.t) : Reachability.AccessSet.t =
  app.kernels |> App.only_kernel app
  |> List.concat_map (fun k ->
    k
    |> Reachability.prepare_kernel
         ~assumes:(App.assumes_of k app)
         ~assume_dims:app.assume_dims
         ~params:app.params
    |> Reachability.check_kernel ?timeout:app.timeout)
  |> Reachability.reachable_set

(* Build the co-reach proof stream for one kernel under the
   current [app] state. The stream encodes — for each per-location
   flat-acc fragment — a two-thread existential without the
   conflict / same-address constraints; SAT means the fragment's
   two-thread reachability is still live under the precondition
   stack [app] carries. *)
let coreach_stream_of (arch : Architecture.t) (app : App.t)
    (k : Kernel.t) : Symbexp.Proof.t Streamutil.stream =
  k
  |> App.translate arch app
  |> Symbexp.translate_coreach arch

(* Build the single-thread (T1-only) proof stream for one kernel
   under the current [app] state. Each fragment's goal asserts
   [pre ∧ runtime ∧ (∃T1 reaches some access)] — the Tier 1
   pre-filter's shape. Fragment identities ([kernel_name, array_name,
   id]) line up with [coreach_stream_of] since both run the same
   [Streamutil.mapi] over the same flat-acc stream. *)
let t1_stream_of (arch : Architecture.t) (app : App.t)
    (k : Kernel.t) : Symbexp.Proof.t Streamutil.stream =
  k
  |> App.translate arch app
  |> Symbexp.translate_t1 arch

(* Build [baseline] or [under-Φ] pair sets from an [app]. The
   precondition stack that drives the SAT outcome of each fragment
   is whatever [app] carries: [k.pre] plus [app.assumes] (which
   already includes any user [--assume]s and any abductive Φ folded
   in by [app_with_extras]). *)
let coreach_pairs_of (app : App.t) : Co_reach.pair list =
  app.kernels |> App.only_kernel app
  |> List.concat_map (fun (k : Kernel.t) ->
    List.concat_map (fun arch ->
      coreach_stream_of arch app k
      |> Co_reach.candidates ~tag:"baseline" ~timeout:app.timeout
           ~logic:app.logic)
      app.archs)

(* Per-round under-Φ pair set, restricted to baseline keys. Drops
   the SAT work for any fragment whose key isn't in the baseline:
   such fragments can't affect [preserves_subset]'s outcome, so
   solving them is dead work. Halves the under-Φ cost on kernels
   where baseline rejects most fragments (typical for kernels with
   many phase splits but few race-candidate fragments). *)
let coreach_pairs_restricted_of (baseline_keys : Co_reach.KeySet.t)
    (app : App.t) : Co_reach.pair list =
  app.kernels |> App.only_kernel app
  |> List.concat_map (fun (k : Kernel.t) ->
    List.concat_map (fun arch ->
      coreach_stream_of arch app k
      |> Co_reach.candidates_restricted ~tag:"under-phi"
           ~timeout:app.timeout baseline_keys)
      app.archs)

(* Per-round under-Φ T1 (single-thread) SAT keys, restricted to
   baseline keys. The Tier 1 baseline is [baseline_keys] (the set
   of pair-relevant fragments), and the under-Φ accept condition
   is "every baseline key remains T1-SAT". *)
let t1_keys_restricted_of (baseline_keys : Co_reach.KeySet.t)
    (app : App.t) : Co_reach.KeySet.t =
  app.kernels |> App.only_kernel app
  |> List.fold_left (fun acc (k : Kernel.t) ->
    List.fold_left (fun acc arch ->
      t1_stream_of arch app k
      |> Co_reach.t1_keys_restricted ~tag:"tier1"
           ~timeout:app.timeout baseline_keys
      |> Co_reach.KeySet.union acc)
      acc app.archs)
    Co_reach.KeySet.empty

(* Tier 1 pre-filter (legacy, per-access shape). For each access
   [a], [Reachability.check_kernel] (Slot-grouped, one Z3
   push/check/pop per path-cond equivalence class) asks whether
   [k.pre ∧ Φ ∧ path_cond(a)] is SAT; the resulting under-Φ
   reachable set must cover the baseline-reachable set.

   Retained for reference. The default Tier 3 driver uses
   [gate_holds_t1_pairs] instead because the access-set baseline is
   too strict — a single-thread access (e.g. guarded by
   [threadIdx.x == 0]) is baseline-reachable but never participates
   in a co-reach pair, so a Φ that drops it would be rejected here
   yet accepted by Tier 2, breaking the "Tier 1 reject ⇒ Tier 2
   reject" pre-filter contract. *)
let[@warning "-32"] gate_holds_per_access
    (baseline : Reachability.AccessSet.t) (app : App.t) : bool =
  Phase_timer.measure "genie/gate" (fun () ->
    Stats.incr "gate_checks";
    Reachability.AccessSet.subset baseline (access_set_of app))

(* Tier 1 pre-filter (pair-aware). Restricts the single-thread
   reachability question to fragments that participate in some
   baseline co-reach pair, so a Φ that drops a baseline-reachable
   access only matters when that access actually feeds a Tier 2
   pair. The earlier per-access shape would reject Φ for any
   baseline-reachable access dropping out — including single-thread
   accesses guarded by [threadIdx.x == 0] that never form a co-reach
   pair, breaking the "Tier 1 reject ⇒ Tier 2 reject" pre-filter
   contract.

   For each baseline pair key, ask "is the fragment's T1-only goal
   still SAT under Φ?" — i.e. [pre ∧ Φ ∧ (∃T1 reaches some access)].
   Reject Φ if any baseline key drops out of the T1-SAT set.
   Cheaper per call than Tier 2: the T1 goal drops the second-thread
   conjunct and the [id_le] canonicalisation. *)
let gate_holds_t1_pairs (baseline_keys : Co_reach.KeySet.t)
    (app : App.t) : bool =
  Phase_timer.measure "genie/gate" (fun () ->
    Stats.incr "gate_checks";
    let under_phi = t1_keys_restricted_of baseline_keys app in
    Co_reach.KeySet.subset baseline_keys under_phi)

let gate_holds_simple (_baseline : Reachability.AccessSet.t)
    (app : App.t) : bool =
  Phase_timer.measure "genie/gate" (fun () ->
    Stats.incr "gate_checks";
    app.kernels |> App.only_kernel app
    |> List.for_all (fun k ->
      k
      |> Reachability.prepare_kernel
           ~assumes:(App.assumes_of k app)
           ~assume_dims:app.assume_dims
           ~params:app.params
      |> Reachability.preconditions_satisfiable ?timeout:app.timeout))

(* Cached gate. Per kernel we keep one Z3 context, one solver with
   the base encoding ([kernel.pre + runtime] under
   [prepare_kernel ~assumes:[]]) permanently added, and the
   substitution that translates [Kernel.inline_globals]'s effect on
   bexps. Per gate call the round's assumes are substituted through
   the same map, then pushed onto the solver, checked, and popped —
   keeping Z3's learned clauses alive across CEGAR rounds.

   The cache key includes a structural fingerprint of [pre] and
   [code] alongside the kernel name. Keying on name alone is
   unsound: in practice [synthesise_launches] can emit several
   [Kernel.t] values sharing a synth name
   ([<orig>__launch_<file>_<line>]) but with different bodies, for
   example when [aop-cuda]'s [prepare_svd_kernel__launch_main_938]
   appears seven times with four distinct body sizes from CUB
   template expansion. Under a name-only key those would collapse
   into one slot and later gate calls would query the *first*
   kernel's encoded pre+runtime instead of their own. The fingerprint
   restores per-distinct-kernel slots; same-name same-structure
   kernels still share, preserving the [--gate-cache] benefit. *)
module Gate_cache = struct
  type key = string * int * int
  let key_of (k : Kernel.t) : key =
    (k.name, Hashtbl.hash k.pre, Hashtbl.hash k.code)

  type t = (key, Reachability.Slot.t) Hashtbl.t

  let create () : t = Hashtbl.create 8

  let get_or_init (cache : t) ~(timeout : int option)
      ~(assume_dims : bool) ~(params : (string * int) list)
      (k : Kernel.t) : Reachability.Slot.t =
    let key = key_of k in
    match Hashtbl.find_opt cache key with
    | Some s -> s
    | None ->
      let s =
        Reachability.make_slot ~timeout ~assume_dims ~params k
      in
      Hashtbl.add cache key s;
      s
end

let gate_holds_cached (cache : Gate_cache.t)
    (_baseline : Reachability.AccessSet.t) (app : App.t) : bool =
  app.kernels |> App.only_kernel app
  |> List.for_all (fun k ->
    let slot =
      Gate_cache.get_or_init cache
        ~timeout:app.timeout
        ~assume_dims:app.assume_dims
        ~params:app.params
        k
    in
    Reachability.preconditions_satisfiable_delta slot (App.assumes_of k app))

let[@warning "-32"] gate_holds = gate_holds_simple

(* Tier 3 gate. The baseline pair set is fixed for the kernel(s) and
   carried in [baseline]; per-round the gate rebuilds the under-Φ
   pair set from the current [app] (whose [assumes] include the
   candidate Φ) and checks the baseline is a subset of it (keyed by
   [(kernel_name, array_name, id)]).

   The under-Φ rebuild is per-round; no caching. The dominant cost
   inside [Co_reach.candidates] is the per-proof solve, which already
   uses [Solve_drf.solve] (the same machinery the DRF baseline uses).
   Caching the under-Φ pair set would require a cache key over
   [app.assumes]; bumping that per round defeats the cache. The
   straight rebuild gives the design's "sharp drop in gate cost"
   precisely because Φ trivialisations turn baseline-SAT proofs into
   under-Φ UNSAT proofs cheaply (the BV solver short-circuits on the
   contradiction); when Φ doesn't trivialise, the under-Φ solve is
   the same cost as the baseline one — but only on pairs the
   baseline picked, which is already a filter by SAT-ability. *)
let gate_holds_pairs (baseline : Co_reach.pair list) (app : App.t) : bool =
  Phase_timer.measure "genie/gate" (fun () ->
    Stats.incr "gate_checks";
    let baseline_keys = Co_reach.keys_of baseline in
    let under_phi = coreach_pairs_restricted_of baseline_keys app in
    Co_reach.preserves_subset ~under_phi ~baseline)

(* Per-kernel extras. Keyed by [Kernel.name]. Each kernel's clauses
   are conjoined onto that kernel's own pre — no cross-kernel
   flattening — matching App.t.assumes's shape. *)
type per_kernel_extras = (string * Exp.bexp list) list

(* Merge [extras] into [app.assumes] kernel-by-kernel, returning a
   new app. Kernels absent from [extras] keep their existing assumes
   unchanged. *)
let app_with_extras (extras : per_kernel_extras) (app : App.t) : App.t =
  let lookup kn =
    List.find_opt (fun (n, _) -> n = kn) extras
    |> Option.map snd
    |> Option.value ~default:[]
  in
  let assumes' =
    List.map (fun (kn, bs) -> (kn, bs @ lookup kn)) app.assumes
  in
  { app with assumes = assumes' }

let is_extras_empty (extras : per_kernel_extras) : bool =
  List.for_all (fun (_, bs) -> bs = []) extras

let extras_flatten (extras : per_kernel_extras) : (string * Exp.bexp) list =
  List.concat_map (fun (kn, bs) -> List.map (fun b -> (kn, b)) bs) extras

let extras_group (pairs : (string * Exp.bexp) list) : per_kernel_extras =
  List.fold_left (fun acc (kn, b) ->
    let existing =
      List.find_opt (fun (n, _) -> n = kn) acc |> Option.map snd |> Option.value ~default:[]
    in
    let others = List.filter (fun (n, _) -> n <> kn) acc in
    (kn, existing @ [ b ]) :: others)
    [] pairs
  |> List.rev

(* Concatenate two [per_kernel_extras] kernel-by-kernel, preserving
   [first] before [second] for any kernel that appears in both. Used
   to merge IR-derived dim pins (see [usage_constrained_kernel])
   with abductive / blanket clauses so both flow into the same
   [Discovered: --assume ...] output. *)
let merge_extras (first : per_kernel_extras) (second : per_kernel_extras)
    : per_kernel_extras =
  let lookup l kn =
    List.find_opt (fun (n, _) -> n = kn) l
    |> Option.map snd
    |> Option.value ~default:[]
  in
  let names_in_first = List.map fst first in
  let names_only_in_second =
    List.filter_map (fun (n, _) ->
      if List.mem n names_in_first then None else Some n) second
  in
  List.map (fun kn -> (kn, lookup first kn @ lookup second kn))
    (names_in_first @ names_only_in_second)

let run_assuming (extras : per_kernel_extras) (app : App.t) : Analysis.t list =
  Phase_timer.measure "genie/race" (fun () ->
    Stats.incr "race_queries";
    app |> app_with_extras extras |> App.run)

let verifies_drf_only (app : App.t) (extras : per_kernel_extras) : bool =
  app
  |> run_assuming extras
  |> all_safe

(* Drop-clause shrink, per-kernel. For each [(kn, b)] flattened pair,
   try removing it from [extras]; if the kernel set still clears DRF,
   the pair is droppable.

   The [_baseline] parameter is polymorphic and unused — shrinking
   is purely a DRF check, independent of which baseline representation
   the gate uses ([AccessSet.t] under [--legacy-gate], [Co_reach.pair
   list] under the default Tier 3 gate). *)
let shrink_linear (_baseline : 'a) (app : App.t)
    (extras : per_kernel_extras) : per_kernel_extras =
  Phase_timer.measure "genie/shrink" (fun () ->
    let flat = extras_flatten extras in
    let rec loop kept remaining =
      match remaining with
      | [] -> kept
      | c :: rest ->
        if verifies_drf_only app (extras_group (kept @ rest))
        then loop kept rest
        else loop (kept @ [ c ]) rest
    in
    loop [] flat |> extras_group)

(* UNSAT-core shrink: run the DRF pipeline once with [extras] added as
   tracked Z3 assumptions (named [extra_<id>]) instead of conjoining
   them into [kernel.pre]. Each per-proof outcome is either DRF (with
   the subset of extras the Z3 unsat-core mentions) or Racy /
   Unknown. The minimal set is the UNION of cores across all proofs.

   Replaces the O(N) drop-clause loop in [shrink_linear] with one
   pipeline run; the speedup is roughly the number of extras (~18
   for is-cuda).

   Falls back to [None] if any proof returns Racy / Unknown, or if
   the union is empty under non-empty [extras] (defensive: would
   imply the formula was already UNSAT without any extra). The
   caller should use [shrink_linear] as the fallback. *)
module IntSet = Set.Make (Int)

let shrink_via_core (_baseline : 'a) (app : App.t)
    (extras : per_kernel_extras) : per_kernel_extras option =
  if is_extras_empty extras then Some extras
  else
    (* Tag each kernel's clauses with kernel-local integer IDs. The
       same ID across two different kernels names two different
       clauses; that's fine because each kernel's [assert_and_track]
       happens in its own Z3 context. *)
    let core_extras : (string * (int * Exp.bexp) list) list =
      List.map (fun (kn, bs) -> (kn, List.mapi (fun i b -> (i, b)) bs))
        extras
    in
    let analyses = App.run { app with core_extras } in
    if not (all_safe analyses) then None
    else
      (* Group cores by kernel: each [Analysis.t]'s proofs share a
         kernel, so their cores all live in the same ID space. *)
      let per_kernel_needed : (string * IntSet.t) list =
        analyses
        |> List.map (fun (a : Analysis.t) ->
          let ids =
            a.report
            |> List.concat_map (fun (s : Solve_drf.Solution.t) ->
                match s.outcome with
                | Solve_drf.Outcome.Drf_with_core c -> c
                | _ -> [])
            |> IntSet.of_list
          in
          (Protocols.Kernel.name a.kernel, ids))
      in
      (* Empty-core guard, applied across all kernels. With non-empty
         [extras] but every kernel reporting an empty core, the
         caller should fall back to [shrink_linear] (see the
         [shrink_linear]-fallback comment in the dropped version of
         this function). *)
      if List.for_all (fun (_, s) -> IntSet.is_empty s) per_kernel_needed
      then None
      else
        (* Walk each kernel's tagged list in input order and keep
           those whose ID the core mentions. *)
        let kept : per_kernel_extras =
          List.map (fun (kn, tagged) ->
            let needed =
              List.find_opt (fun (n, _) -> n = kn) per_kernel_needed
              |> Option.map snd
              |> Option.value ~default:IntSet.empty
            in
            let kept_bs =
              tagged
              |> List.filter_map (fun (id, b) ->
                if IntSet.mem id needed then Some b else None)
            in
            (kn, kept_bs))
            core_extras
        in
        Some kept

let shrink ~(use_core : bool) (baseline : 'a)
    (app : App.t) (extras : per_kernel_extras) : per_kernel_extras =
  if use_core then
    match shrink_via_core baseline app extras with
    | Some kept -> kept
    | None -> shrink_linear baseline app extras
  else
    shrink_linear baseline app extras

(* Per-clause weakening lattice. Replacing an equality with one of its
   one-sided variants admits strictly more models; if the kernel still
   clears DRF and passes the gate under the weaker variant, prefer it.
   In the bm3d-style synthesis miss [size == gridDim.x ∧ size == bdim*gdim],
   weakening the first clause to [size >= gridDim.x] breaks the
   conjunction's implied [bdim.x == 1] and lets the gate accept.

   [sign] is the per-kernel signedness of [b]'s free variables: each
   clause comes from a specific kernel's [build_pool] and its
   signedness is resolved against that kernel. *)
let weaken_clause (sign : Variable.t -> Signedness.t)
    : Exp.bexp -> Exp.bexp list = function
  | Exp.NRel (Eq, e1, e2) as b ->
    let s = bexp_signedness sign b in
    [ Exp.NRel (Ge s, e1, e2); Exp.NRel (Le s, e1, e2) ]
  | _ -> []

(* Walk [extras] per-kernel; for each clause, if some weaker variant
   keeps the predicate [check] true, swap it in. The signedness used
   for an Eq's weakening is the producing kernel's. *)
let weaken_for_gate
    (app : App.t)
    (check : per_kernel_extras -> bool)
    (extras : per_kernel_extras) : per_kernel_extras =
  Phase_timer.measure "genie/weaken" (fun () ->
    let kernel_by_name (kn : string) : Kernel.t option =
      List.find_opt (fun (k : Kernel.t) -> Kernel.name k = kn) app.kernels
    in
    List.map (fun (kn, bs) ->
      match kernel_by_name kn with
      | None -> (kn, bs)
      | Some k ->
        let sign v = Abduction.signedness_of k v in
        let others_unchanged =
          List.filter (fun (n, _) -> n <> kn) extras
        in
        let rec loop acc = function
          | [] -> acc
          | c :: rest ->
            let weakers = weaken_clause sign c in
            let candidate w =
              let updated = (kn, acc @ (w :: rest)) :: others_unchanged in
              check updated
            in
            let best = List.find_opt candidate weakers in
            let kept = match best with Some w -> w | None -> c in
            loop (acc @ [ kept ]) rest
        in
        (kn, loop [] bs))
      extras)

(* Abductive search with weakening and CEGIS-style gate-rejection
   feedback. On each iteration:
     - Solve MaxSAT for a minimum-cardinality clearance.
     - Run faial; if still racy, add witnesses, re-solve.
     - If DRF: shrink, then check the gate. If gate accepts, return.
       If gate rejects, try clause-wise weakening; if that recovers,
       return the weakened set. Otherwise add [¬extras] to the session
       (CEGIS) and re-solve.

   Per-Φ acceptance is a three-tier short-circuit ordered by cost:
   Tier 1 (single-thread reach preservation, [pre_filter]) →
   Tier 2 (co-reach pair-subset gate, [gate_check]) →
   DRF query. Each tier rejects strictly more Φs than the next,
   so running cheaper tiers first does not change the accepted set —
   it only short-circuits Φs the later tiers would also reject.

   [pre_filter] defaults to the trivial accept; the legacy gate path
   carries its single-thread reach check in [gate_check] itself, so
   it leaves [pre_filter] unset. The default Tier 3 path supplies a
   dedicated [pre_filter] derived from [Reachability.AccessSet]. *)
let abductive_loop
    ?(iter_cap = 32)
    ?(scope_of : (string -> Variable.Set.t option) option)
    ?(prune_candidate : (string -> Exp.bexp -> bool) option)
    ?(pre_filter : (App.t -> bool) = fun _ -> true)
    ?(non_trivial : (per_kernel_extras -> bool) option)
    ~(use_core_shrink : bool)
    ~(gate_check : 'baseline -> App.t -> bool)
    (app : App.t)
    (baseline : 'baseline)
    : per_kernel_extras option =
  let kernels = App.only_kernel app app.kernels in
  if kernels = [] then None
  else
    let session =
      Abduction.create_for_kernels ?scope_of ?prune_candidate kernels
    in
    Stats.set "pool_size" (List.length session.candidates);
    let solve s = Phase_timer.measure "genie/maxsat" (fun () ->
      Stats.incr "maxsat_solves"; Abduction.solve s)
    in
    let drf_and_gate (extras : per_kernel_extras) =
      let app' = app_with_extras extras app in
      Cegar.check_three_tier
        ~tier1:(fun () -> pre_filter app')
        ~tier2:(fun () -> gate_check baseline app')
        ~drf:(fun () -> verifies_drf_only app extras)
    in
    let shrink' = shrink ~use_core:use_core_shrink in
    (* Non-triviality: at least one access must remain reachable under
       the proposed clearance. A clearance that empties the access
       set is vacuous DRF (e.g. forces an effective blockDim.x = 0 so
       no thread runs an access). Implemented as one Z3 query per
       kernel asking "any access reachable?", rather than O(accesses)
       per-access queries. Only invoked at acceptance.

       Callers with a pre-built per-kernel [Any_access_slot] pool
       (the Tier 3 path: [compute_verdict_new] uses the same slots
       it built for [prune_candidate]) override via [~non_trivial],
       turning each check into a slot push/check/pop instead of a
       full re-encoding. The default impl below is the
       slot-less fallback used when no override is supplied. *)
    let non_trivial : per_kernel_extras -> bool =
      match non_trivial with
      | Some f -> f
      | None ->
        fun extras ->
          let app' = app_with_extras extras app in
          app'.kernels |> App.only_kernel app'
          |> List.exists (fun k ->
            k
            |> Reachability.prepare_kernel
                 ~assumes:(App.assumes_of k app')
                 ~assume_dims:app'.assume_dims
                 ~params:app'.params
            |> Reachability.any_access_reachable ?timeout:app'.timeout)
    in
    let try_finalize (extras : per_kernel_extras) : per_kernel_extras option =
      let minimal = shrink' baseline app extras in
      let app' = app_with_extras minimal app in
      if pre_filter app' && gate_check baseline app' && non_trivial minimal
      then Some minimal
      else
        let weakened = weaken_for_gate app drf_and_gate minimal in
        let weakened_min = shrink' baseline app weakened in
        let app'' = app_with_extras weakened_min app in
        if pre_filter app'' && gate_check baseline app'' && non_trivial weakened_min
        then Some weakened_min
        else None
    in
    let rec loop iter (extras : per_kernel_extras) =
      Stats.set "cti_rounds" iter;
      if iter >= iter_cap then None
      else
        let result = run_assuming extras app in
        if all_safe result then
          match try_finalize extras with
          | Some final -> Some final
          | None ->
            (* Gate rejected even after weakening — ban this exact
               per-kernel combination and re-solve. *)
            Stats.incr "rejections";
            if Abduction.reject_combination session extras = 0 then None
            else
              (match solve session with
               | None -> None
               | Some new_extras -> loop (iter + 1) new_extras)
        else
          let added = Abduction.add_all result session in
          Stats.incr ~by:added "samples_added";
          if added = 0 then None
          else
            match solve session with
            | None -> None
            | Some new_extras -> loop (iter + 1) new_extras
    in
    loop 0 []

(* Per-kernel blanket. Each kernel's blanket entries are built from
   its own [int_params] and its own signedness function — every
   clause references variables scoped to one kernel. *)
let blanket_extras (app : App.t) : per_kernel_extras =
  let dims = [
    Variable.bdim_x; Variable.bdim_y; Variable.bdim_z;
    Variable.gdim_x; Variable.gdim_y; Variable.gdim_z;
  ] in
  app.kernels
  |> List.map (fun (k : Kernel.t) ->
    let params = int_params k in
    let sign v = Abduction.signedness_of k v in
    let signs =
      params |> List.map (fun v ->
        Exp.NRel (Gt (sign v), Exp.Var v, Exp.Num 0))
    in
    let bounds =
      params |> List.concat_map (fun p ->
        List.map (fun d ->
          Exp.NRel (Ge Unsigned, Exp.Var p, Exp.Var d)) dims)
    in
    (Kernel.name k, signs @ bounds))

(* Inline unary predicates ([nonneg], [pow2], [uintN]) before
   emission so the user-visible [--assume KERNEL:BEXP] reproduces
   the round-trip through the bexp parser. The parser disambiguates
   predicate calls from [NCall] (function call in [nexp]) by requiring
   2+ comma-separated arguments; 1-arg predicates would otherwise
   collide with [NCall]. After [Predicates.b_inline] the only [Pred]
   nodes that remain are the genuinely n-ary ones ([bvumul_noovfl]). *)
let format_assume_flags (extras : per_kernel_extras) : string =
  extras
  |> List.concat_map (fun (kn, bs) ->
       List.map (fun b ->
         Printf.sprintf "--assume \"%s:%s\"" kn
           (b |> Predicates.b_inline |> Exp.b_to_string)) bs)
  |> String.concat " "

(* Use-derived dim bounds. For each axis (x, y, z) and level (thread,
   block): if the corresponding index variable is referenced in the
   kernel code, the dim is constrained to [>= 2]; otherwise the dim is
   pinned to [== 1]. The [>= 2] half is what stops abductive from
   landing on trivialising clearances (e.g. bm3d's
   [size == gridDim.x ∧ size == blockDim.x * gridDim.x] entailing
   [blockDim.x == 1]).

   Each candidate is pre-flight SAT-checked against the kernel's
   prepared pre. Constraints that conflict with an existing pin
   (most commonly a launch literal — [bm3d]'s launch site pins
   [blockDim.y == 1] via [--assume-launch]) are dropped.

   Returns the augmented kernel and the list of pins that were
   actually added (a strict subset of the six candidates). The pins
   are first-class assumptions and the caller threads them into the
   verdict so they surface in [Discovered: --assume ...]. *)
let usage_constrained_kernel
    ~(gate_timeout_ms : int)
    ~(params : (string * int) list)
    (k : Kernel.t) : Kernel.t * Exp.bexp list =
  let used = Code.free_names k.code Variable.Set.empty in
  let probe0 =
    Reachability.prepare_kernel ~assumes:[] ~assume_dims:false ~params k
  in
  let open Variable in
  let _, k_final, pins_rev =
    [ tid_x, bdim_x; tid_y, bdim_y; tid_z, bdim_z;
      bid_x, gdim_x; bid_y, gdim_y; bid_z, gdim_z ]
    |> List.fold_left (fun (probe, k_acc, pins) (idx, dim) ->
      let candidate =
        if Set.mem idx used
        (* [dim] is a CUDA built-in (unsigned int), so the comparison
           is unsigned. *)
        then Exp.NRel (Ge Unsigned, Var dim, Num 2)
        else Exp.NRel (Eq, Var dim, Num 1)
      in
      let probe' = Kernel.add_pre candidate probe in
      (* Reject-on-Unknown here. A pin accepted on Unknown can
         silently make [k.pre] unsatisfiable when the truth was Unsat,
         making later axis pre-flights and downstream gate queries
         answer against a contradictory pre, and the answer would
         vary with Z3 timing. The optimistic accept-on-Unknown
         policy used by the CEGAR gate is intentional there but
         wrong for this optimisation step. The same policy makes the
         [gate_timeout_ms] cap safe: a slow axis query that exceeds
         the budget returns UNKNOWN, drops the pin, and the kernel
         is analysed without that extra prune. *)
      match
        Reachability.preconditions_check ~timeout:gate_timeout_ms probe'
      with
      | Reachability.Pre_sat ->
        (probe', Kernel.add_pre candidate k_acc, candidate :: pins)
      | Reachability.Pre_unsat | Reachability.Pre_unknown ->
        (probe, k_acc, pins))
      (probe0, k, [])
  in
  (k_final, List.rev pins_rev)

(* Z3 raises [Z3.Error "max. memory exceeded"] when a query exhausts
   its memory cap (default ~6 GB). Treat it as an inconclusive result —
   we couldn't prove DRF, so report [Racy] and let the caller decide.

   [usage_pins] are the dim pins folded into each kernel's pre by
   [usage_constrained_kernel]. They are real assumptions used in
   reaching the verdict, so they're merged with any abductive /
   blanket clauses into the [Drf] payload's [assumes] for the
   user-visible [Discovered: --assume ...] line. *)
(* Tier 3 driver. The default [compute_verdict] path; the legacy
   [AccessSet]-based gate is retained via [compute_verdict_legacy]
   below and exposed through [--legacy-gate].

   [baseline_pairs] is the Tier 2 pair set built from the co-reach
   proof stream under the kernel's baseline pre (no Φ). Empty
   [baseline_pairs] means no fragment had a SAT co-reach goal — no
   two-thread universe exists under the baseline — and the kernel
   is vacuously DRF. *)
let compute_verdict_new ~(use_core_shrink : bool) ~(iter_cap : int)
    ~(prune_timeout_ms : int)
    ~(usage_pins : per_kernel_extras)
    (app : App.t) : verdict =
  let drf source clauses =
    Drf { source; assumes = merge_extras usage_pins clauses }
  in
  (* Per-[compute_verdict] caches for the Tier 1 (single-thread reach
     preservation) and Tier 2 (co-reach pair-subset) gates, keyed by
     the normalised [app.assumes]. Created fresh here so each
     [compute_verdict] call gets a clean cache; discarded on return.
     The abductive loop re-evaluates the same Φ across
     round-accept / shrink / weaken / re-shrink, so these are the
     hits we expect to collect. *)
  let tier1_cache : bool Tier_cache.t = Tier_cache.create () in
  let tier2_cache : bool Tier_cache.t = Tier_cache.create () in
  let baseline_pairs =
    Phase_timer.measure "genie/baseline-coreach"
      (fun () -> coreach_pairs_of app)
  in
  (* Tier 1 baseline: the pair-relevant fragment keys. Restricting
     the Tier 1 baseline to fragments that feed some Tier 2 pair
     preserves the "Tier 1 reject ⇒ Tier 2 reject" pre-filter
     contract: a baseline pair preserved at Tier 2 has both T1 and
     T2 conjuncts SAT under Φ, so its T1-only conjunct is also SAT;
     equivalently, any baseline key whose T1 goal becomes UNSAT
     under Φ would also drop at Tier 2.

     The earlier baseline ([Reachability.AccessSet] over all kernel
     accesses) was too strict — a single-thread access guarded by
     e.g. [threadIdx.x == 0] is reachable but doesn't form a Tier 2
     pair, so a Φ that drops it would be rejected at Tier 1 yet
     accepted at Tier 2. *)
  let baseline_keys = Co_reach.keys_of baseline_pairs in
  (* Wrap each tier predicate with a Φ-keyed cache lookup. Hits skip
     [Phase_timer.measure] / [Stats.incr "gate_checks"] so the
     hit/miss counters drive the cost picture. *)
  let cached_tier1 (app' : App.t) : bool =
    match Tier_cache.find_opt tier1_cache app'.assumes with
    | Some v -> Stats.incr "tier1_gate_hits"; v
    | None ->
      Stats.incr "tier1_gate_misses";
      let v = gate_holds_t1_pairs baseline_keys app' in
      Tier_cache.add tier1_cache app'.assumes v;
      v
  in
  let cached_tier2 (_baseline : Co_reach.pair list) (app' : App.t) : bool =
    match Tier_cache.find_opt tier2_cache app'.assumes with
    | Some v -> Stats.incr "tier2_gate_hits"; v
    | None ->
      Stats.incr "tier2_gate_misses";
      let v = gate_holds_pairs baseline_pairs app' in
      Tier_cache.add tier2_cache app'.assumes v;
      v
  in
  let gate_check = cached_tier2 in
  let pre_filter = cached_tier1 in
  let baseline =
    Phase_timer.measure "genie/baseline" (fun () ->
      Stats.incr "race_queries"; App.run app)
  in
  if all_safe baseline then
    if baseline_pairs = [] then Drf_vacuous
    else drf Source_baseline []
  else
    let scopes =
      List.map (fun (k : Kernel.t) ->
        (Kernel.name k, Access_partition.abductive_scope k))
        (App.only_kernel app app.kernels)
    in
    let scope_of (kn : string) : Variable.Set.t option =
      List.assoc_opt kn scopes
    in
    (* Drop pool candidates that on their own make [kn]'s access set
       empty. Including such a clause in any Φ would yield vacuous DRF
       (the [non_trivial] check at acceptance would reject it), so
       letting MaxSAT propose them only wastes CEGAR rounds on Φs the
       gate / non-triviality check will reject anyway.

       Each kernel gets one prepared slot (encoded once) and per-
       candidate queries push/check/pop on it. Direct
       [Reachability.any_access_reachable] would re-encode the kernel
       per candidate; for pools in the thousands that dominates wall
       time. *)
    (* Per-candidate prune queries are capped by [prune_timeout_ms]
       (CLI: [--prune-timeout-ms], default 500). [build_pool] emits
       O(P × D + P²) candidates per kernel; on heavy inlined bodies a
       handful of pathological candidates can each take seconds to
       decide. Queries that hit the cap return [UNKNOWN], which
       [any_access_reachable_delta] already maps to "keep". Independent
       of [app.timeout] (the global per-verdict cap), which is typically
       too loose for per-call pruning. *)
    let slots =
      Phase_timer.measure "genie/prune-prep" (fun () ->
        List.map (fun (k : Kernel.t) ->
          let prepared =
            Reachability.prepare_kernel
              ~assumes:(App.assumes_of k app)
              ~assume_dims:app.assume_dims
              ~params:app.params
              k
          in
          (Kernel.name k,
           Reachability.make_any_access_slot
             ~timeout:prune_timeout_ms prepared))
          (App.only_kernel app app.kernels))
    in
    let prune_candidate (kn : string) (b : Exp.bexp) : bool =
      match List.assoc_opt kn slots with
      | None | Some None -> true
      | Some (Some slot) -> Reachability.any_access_reachable_delta slot b
    in
    (* Slot-amortised non-triviality. Mirrors [prune_candidate]: push
       the kernel's Φ clauses (conjoined) as a delta on the persistent
       solver, check, pop. Skips the [prepare_kernel] +
       [any_access_reachable] full-encoding round-trip that the
       slot-less fallback in [abductive_loop] pays per call. A kernel
       without a slot has no accesses to keep reachable, so it cannot
       contribute non-triviality: skip it.

       Empty per-kernel clause list yields [b_and_ex [] = True], which
       pushes a no-op and returns whether the slot's base goal is
       satisfiable, i.e. whether any access is reachable under the
       baseline. That matches the slot-less fallback's behaviour for
       a kernel the proposed Φ does not touch. *)
    let non_trivial (extras : per_kernel_extras) : bool =
      App.only_kernel app app.kernels
      |> List.exists (fun k ->
        let kn = Protocols.Kernel.name k in
        let delta_clauses =
          List.assoc_opt kn extras |> Option.value ~default:[]
        in
        match List.assoc_opt kn slots with
        | None | Some None -> false
        | Some (Some slot) ->
          let delta = Exp.b_and_ex delta_clauses in
          Reachability.any_access_reachable_delta slot delta)
    in
    match Phase_timer.measure "genie/abductive" (fun () ->
            abductive_loop ~iter_cap ~scope_of ~prune_candidate ~pre_filter
              ~non_trivial ~use_core_shrink ~gate_check app baseline_pairs)
    with
    | Some minimal -> drf Source_abductive minimal
    | None ->
      Phase_timer.measure "genie/blanket" (fun () ->
        Stats.set "blanket_attempted" 1;
        let blanket = blanket_extras app in
        if is_extras_empty blanket || not (verifies_drf_only app blanket)
        then Racy
        else
          let minimal =
            shrink ~use_core:use_core_shrink baseline_pairs app blanket
          in
          let app' = app_with_extras minimal app in
          let blanket_non_trivial = non_trivial minimal in
          if pre_filter app' && gate_check baseline_pairs app'
             && blanket_non_trivial
          then drf Source_blanket minimal
          else Racy)

(* Legacy [AccessSet]-based gate path. Retained behind [--legacy-gate]
   for one release cycle so a kernel whose verdict regresses unexpectedly
   under the Tier 3 gate can be re-run with the previous semantics. *)
let compute_verdict_legacy ~(use_core_shrink : bool) ~(iter_cap : int)
    ~(cached_gate : bool) ~(usage_pins : per_kernel_extras)
    (app : App.t) : verdict =
  let drf source clauses =
    Drf { source; assumes = merge_extras usage_pins clauses }
  in
  let gate_check =
    if cached_gate then
      let cache = Gate_cache.create () in
      gate_holds_cached cache
    else
      gate_holds_simple
  in
  let baseline_reachable =
    Phase_timer.measure "genie/baseline-reach"
      (fun () -> access_set_of app)
  in
  let baseline =
    Phase_timer.measure "genie/baseline" (fun () ->
      Stats.incr "race_queries"; App.run app)
  in
  if all_safe baseline then
    if Reachability.AccessSet.is_empty baseline_reachable then Drf_vacuous
    else drf Source_baseline []
  else
    let scopes =
      List.map (fun (k : Kernel.t) ->
        (Kernel.name k, Access_partition.abductive_scope k))
        (App.only_kernel app app.kernels)
    in
    let scope_of (kn : string) : Variable.Set.t option =
      List.assoc_opt kn scopes
    in
    match Phase_timer.measure "genie/abductive" (fun () ->
            abductive_loop ~iter_cap ~scope_of ~use_core_shrink ~gate_check
              app baseline_reachable)
    with
    | Some minimal -> drf Source_abductive minimal
    | None ->
      Phase_timer.measure "genie/blanket" (fun () ->
        Stats.set "blanket_attempted" 1;
        let blanket = blanket_extras app in
        if is_extras_empty blanket || not (verifies_drf_only app blanket)
        then Racy
        else
          let minimal =
            shrink ~use_core:use_core_shrink baseline_reachable app blanket
          in
          let app' = app_with_extras minimal app in
          let blanket_non_trivial =
            app'.kernels |> App.only_kernel app'
            |> List.exists (fun k ->
              k
              |> Reachability.prepare_kernel
                   ~assumes:(App.assumes_of k app')
                   ~assume_dims:app'.assume_dims
                   ~params:app'.params
              |> Reachability.any_access_reachable ?timeout:app'.timeout)
          in
          if gate_check baseline_reachable app' && blanket_non_trivial
          then drf Source_blanket minimal
          else Racy)

let compute_verdict ~(use_core_shrink : bool) ~(iter_cap : int)
    ~(cached_gate : bool) ~(legacy_gate : bool)
    ~(prune_timeout_ms : int)
    ~(usage_pins : per_kernel_extras)
    (app : App.t) : verdict =
  (* Stats and Phase_timer are module-level globals. Reset at entry so
     a second [compute_verdict] in the same process (test harness,
     batch wrapper, future LSP integration) doesn't see accumulated
     counters from the prior call. *)
  Stats.reset ();
  Phase_timer.reset ();
  try
    if legacy_gate
    then compute_verdict_legacy ~use_core_shrink ~iter_cap ~cached_gate
           ~usage_pins app
    else compute_verdict_new ~use_core_shrink ~iter_cap ~prune_timeout_ms
           ~usage_pins app
  with Z3.Error _ -> Racy

let report_prose (v : verdict) : unit =
  match v with
  | Drf_vacuous ->
    print_endline
      "Baseline preconditions are unsatisfiable — kernel is vacuously DRF.";
    print_endline
      "Check that the kernel and any user --assume flags are mutually \
       satisfiable."
  | Drf { source; assumes } ->
    let preamble = match source with
      | Source_baseline -> "DRF under baseline."
      | Source_abductive -> "DRF after abductive refinement."
      | Source_blanket -> "DRF after blanket fallback."
    in
    print_endline preamble;
    if is_extras_empty assumes
    then print_endline "No --assume needed."
    else print_endline ("Discovered: " ^ format_assume_flags assumes)
  | Racy ->
    print_endline
      "Racy; either a real race or a modelling gap (or vacuous DRF rejected)."

let report_json (app : App.t) (v : verdict) : unit =
  (* The [assumes] field is per-kernel: an [Assoc] from kernel name
     to the list of clauses discovered for that kernel. A kernel with
     no clauses appears with an empty list, so consumers can iterate
     without missing entries. *)
  let assumes_to_json (extras : (string * Exp.bexp list) list) : Yojson.Basic.t =
    `Assoc (List.map (fun (kn, bs) ->
      (kn, `List (List.map (fun b -> `String (Exp.b_to_string b)) bs)))
      extras)
  in
  let verdict_str, source_json, assumes_json = match v with
    | Drf { source; assumes } ->
      "drf",
      `String (source_to_string source),
      assumes_to_json assumes
    | Drf_vacuous -> "drf_vacuous", `Null, `Assoc []
    | Racy -> "racy", `Null, `Assoc []
  in
  let status = match v with Racy -> "racy" | _ -> "drf" in
  let kernels =
    App.only_kernel app app.kernels
    |> List.map (fun (k : Kernel.t) ->
      `Assoc [
        ("kernel_name", `String k.name);
        ("status", `String status);
      ])
  in
  `Assoc [
    ("verdict", `String verdict_str);
    ("source", source_json);
    ("assumes", assumes_json);
    ("kernels", `List kernels);
    ("phase_times", Phase_timer.to_json ());
    ("genie_stats", Stats.to_json ());
    ("argv",
     `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x)));
    ("executable_name", `String Sys.executable_name);
    ("z3_version", `String Z3.Version.to_string);
  ]
  |> Yojson.Basic.to_string
  |> print_endline

let main =
  let doc = "Search for assume-constraints that make a CUDA kernel DRF." in
  let info = Cmd.info "faial-genie" ~doc in
  Cmd.v info
  @@
  let open Cmdliner.Term.Syntax in
  let+ filename =
    Arg.(required & pos 0 (some file) None
         & info [] ~docv:"FILENAME"
             ~doc:"Path to the GPU program.")
  and+ timeout =
    Arg.(value & opt (some int) None
         & info [ "t"; "timeout" ] ~docv:"MS"
             ~doc:"Per-iteration solver timeout in milliseconds.")
  and+ logic =
    Arg.(value & opt (some string) None
         & info [ "logic" ] ~doc:"Z3 logic.")
  and+ solve_tactic =
    let default_doc =
      Gen_z3.Tactic.to_string default_solve_tactic
    in
    Arg.(value & opt (some conv_tactic) (Some default_solve_tactic)
         & info [ "solve-tactic" ] ~docv:"TACTIC"
             ~doc:("Z3 tactic expression for the race-query solver. \
                    Default: " ^ default_doc))
  and+ includes =
    Arg.(value & opt_all string []
         & info [ "I"; "include-dir" ] ~docv:"DIR"
             ~doc:"Add to include search path.")
  and+ params =
    Arg.(value & opt_all (pair ~sep:'=' string int) []
         & info [ "p"; "param" ] ~docv:"K=V"
             ~doc:"Set integer parameter.")
  and+ macros =
    Arg.(value & opt_all string []
         & info [ "D"; "macro" ] ~docv:"NAME[=VAL]"
             ~doc:"Define macro.")
  and+ cu_to_json =
    Arg.(value & opt string "cu-to-json"
         & info [ "cu-to-json" ] ~docv:"PATH"
             ~doc:"Path to cu-to-json.")
  and+ ignore_parsing_errors =
    Arg.(value & flag
         & info [ "ignore-parsing-errors" ] ~doc:"Ignore parsing errors.")
  and+ ignore_calls =
    Arg.(value & flag
         & info [ "ignore-calls" ] ~doc:"Skip kernel-call inlining.")
  and+ ignore_asserts =
    Arg.(value & flag
         & info [ "ignore-asserts" ] ~doc:"Ignore asserts.")
  and+ only_kernel =
    Arg.(value & opt (some string) None
         & info [ "kernel" ] ~doc:"Only check a specific kernel.")
  and+ extra_assumes =
    Arg.(value & opt_all conv_assume []
         & info [ "assume" ] ~docv:"[KERNEL:]BEXP"
             ~doc:"Pre-condition. With no prefix, applied to every kernel \
                   whose declared params plus the launch-config dims \
                   cover the clause's free variables. With a [KERNEL:] \
                   prefix the clause is scoped to a specific kernel by \
                   name; names not matching any kernel are silently \
                   ignored. May be repeated.")
  and+ output_json =
    Arg.(value & flag
         & info [ "json" ] ~doc:"Output result as a single JSON object.")
  and+ list_kernels =
    Arg.(value & flag
         & info [ "list-kernels" ]
             ~doc:"Print one kernel name per line on stdout, then exit. \
                   No analysis is run. Duplicate names in the parsed \
                   list are uniquified with a [_N] suffix so each \
                   printed name addresses a distinct kernel under \
                   [--assume KERNEL:...] and [--kernel].")
  and+ show_signature =
    Arg.(value & flag
         & info [ "show-signature" ]
             ~doc:"Modify [--list-kernels] output to also print each \
                   kernel's parameters with declared C type and \
                   ([signed] | [unsigned]) annotation.")
  and+ use_core_shrink =
    Arg.(value & flag
         & info [ "shrink-core" ]
             ~doc:"Use UNSAT-core extraction to shrink the abductive \
                   precondition in one Z3 call, instead of the default \
                   linear drop-clause loop. Falls back to the linear \
                   path on any racy / unknown subproof.")
  and+ cached_gate =
    Arg.(value & flag
         & info [ "gate-cache" ]
             ~doc:"Reuse a single Z3 context and solver per kernel \
                   across abductive rounds (push/pop on the assertion \
                   stack), preserving learned clauses. Disable to fall \
                   back to a fresh context per gate call. Effective \
                   only with [--legacy-gate]; the default Tier 3 gate \
                   rebuilds the under-Φ pair set per round and does \
                   not share a Z3 slot across rounds.")
  and+ legacy_gate =
    Arg.(value & flag
         & info [ "legacy-gate" ]
             ~doc:"Use the Phase 2 single-thread reachability gate \
                   (per-kernel [AccessSet] subset check) instead of \
                   the default Phase 3 pair-level gate. Retained as a \
                   single-release escape hatch: if a kernel's verdict \
                   regresses under the new gate, [--legacy-gate] \
                   reproduces the previous semantics. Slated for \
                   removal after one release cycle.")
  and+ seed =
    Arg.(value & opt (some int) None
         & info [ "seed" ] ~docv:"N"
             ~doc:"Pin Z3's [smt.random_seed] and [sat.random_seed] to $(docv) \
                   for reproducible solver behaviour across runs. When unset, \
                   Z3 picks its own seed.")
  and+ iter_cap =
    Arg.(value & opt int 32
         & info [ "iter-cap" ] ~docv:"N"
             ~doc:"Cap the number of CEGAR rounds in the abductive loop. \
                   Default is 32. Raise to give the MaxSAT search more \
                   budget on kernels whose abductive Φ candidates keep \
                   getting rejected by the gate.")
  and+ prune_timeout_ms =
    Arg.(value & opt int 500
         & info [ "prune-timeout-ms" ] ~docv:"MS"
             ~doc:"Per-candidate Z3 timeout for [prune_candidate] during \
                   abductive pool construction. On a heavy inlined body, \
                   a small tail of candidates can each take seconds to \
                   decide and dominate [abduction/create]'s wall time. \
                   Queries hitting the cap return UNKNOWN, which is \
                   already treated as 'keep'; the surviving pool grows \
                   by however many timed out and MaxSAT carries them \
                   downstream. Default is 500. Lower to bound the \
                   prune step harder, accepting a larger MaxSAT \
                   workload in return.")
  and+ gate_timeout_ms =
    Arg.(value & opt int 500
         & info [ "gate-timeout-ms" ] ~docv:"MS"
             ~doc:"Per-axis Z3 timeout for [usage_constrained_kernel]'s \
                   pin pre-flight. The pin discovery step issues 6 \
                   satisfiability queries per kernel (one per \
                   tid.{x,y,z} / bid.{x,y,z} axis). On kernels whose \
                   [k.pre] is structurally heavy (e.g. inlined device \
                   functions with control flow), a single axis can \
                   burn seconds of wall time. Queries hitting the cap \
                   return UNKNOWN, which already drops the pin in the \
                   conservative reject-on-Unknown policy, so the cap \
                   trades a possible prune for bounded gate cost. \
                   Default is 500. Set to 0 to disable the cap.")
  in
  seed |> Option.iter (fun n ->
    let s = string_of_int n in
    Z3.set_global_param "smt.random_seed" s;
    Z3.set_global_param "sat.random_seed" s);
  let archs = [ Architecture.Block ] in
  let app =
    App.parse
      ~filename ~timeout
      ~show_proofs:false ~show_proto:false ~show_wf:false ~show_align:false
      ~show_delin:false ~show_phase_split:false ~show_loc_split:false
      ~show_flat_acc:false ~show_symbexp:false
      ~logic ~solve_tactic
      ~ge_index:[] ~le_index:[] ~eq_index:[]
      ~only_array:None ~only_kernel
      ~only_true_data_races:false
      ~thread_idx_1:None ~thread_idx_2:None
      ~block_idx_1:None ~block_idx_2:None
      ~archs
      ~inline_calls:(not ignore_calls)
      ~ignore_parsing_errors
      ~includes
      ~block_dim:None ~grid_dim:None
      ~params
      ~macros
      ~cu_to_json
      ~all_dims:true
      ~ignore_asserts
      ~log_delinearize:false
      ~assume_delin:true
      ~assumes:extra_assumes
      ~assume_dims:false
      ~assume_launch:true
      ~cbor:true
      ~stop_at:None
  in
  if list_kernels then begin
    app.kernels
    |> List.iter (fun k ->
      if show_signature
      then print_endline (Protocols.Kernel.signature_string k)
      else print_endline (Protocols.Kernel.name k));
    Ok ()
  end else
    let app, usage_pins =
      (* Compute use-derived dim pins only for kernels [App.only_kernel]
         would analyse. Each [usage_constrained_kernel] call issues 6
         Z3 SAT queries (one per dim axis); on heavily-templated
         launch sites (e.g. 11 wrappers from a [switch] over
         [log2_elements]) paying for the unfiltered list dwarfs the
         abductive search itself and can exhaust budgets before any
         output appears. Skipped kernels keep their unpinned [k.pre]
         and contribute an empty pin list, preserving the [(name, [])]
         entry shape that [report_json]'s [assumes] consumer expects. *)
      let selected_names =
        App.only_kernel app app.kernels
        |> List.map Protocols.Kernel.name
      in
      let kernels_with_pins =
        List.map (fun k ->
          if List.mem (Protocols.Kernel.name k) selected_names then
            usage_constrained_kernel ~gate_timeout_ms ~params:app.params k
          else
            (k, []))
          app.kernels
      in
      let kernels = List.map fst kernels_with_pins in
      let pins =
        List.map (fun (k, ps) -> (Protocols.Kernel.name k, ps))
          kernels_with_pins
      in
      { app with kernels }, pins
    in
    let v =
      compute_verdict ~use_core_shrink ~iter_cap ~cached_gate ~legacy_gate
        ~prune_timeout_ms ~usage_pins app
    in
    if output_json then report_json app v else report_prose v;
    Ok ()

let () = exit (Cmd.eval_result main)
