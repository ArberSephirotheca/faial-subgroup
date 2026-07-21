(* Per-[compute_verdict] Φ-keyed result cache for the Tier 1 and
   Tier 2 gate predicates.

   Motivation: the abductive loop re-evaluates the same Φ across
   round-accept → shrink → re-check → weaken → re-shrink → re-check
   sequences. Each gate evaluation runs Z3 work proportional to the
   number of accesses / pair fragments; caching the [bool] outcome
   keyed by Φ avoids the redundant SMT calls.

   Key shape: the assumptions carried by [App.t] are an
   [Assumption.t list]. The normalised key renders each assumption
   through [Assumption.to_string], sorts them lexicographically, and
   concatenates. Two semantically-equivalent Φs that differ only in
   clause order share a key. *)

open Protocols

type key = string

type 'a t = (key, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create 32

(* Render an [Assumption.t list] into a canonical string: each clause
   through [Assumption.to_string], sorted, joined by a "\x03" byte that
   does not appear in identifier names or [to_string] output. *)
let key_of (assumptions : Assumption.t list) : key =
  assumptions
  |> List.map Assumption.to_string
  |> List.sort String.compare
  |> String.concat "\x03"

(* [lookup_or_compute cache assumptions f]: returns the cached value for
   the normalised key; on miss, runs [f ()], stores its result, and
   returns it. The cache's hit/miss counters are *not* incremented
   here — the caller is responsible for the bookkeeping so it can use
   per-tier stat keys. *)
let lookup_or_compute (cache : 'a t) (assumptions : Assumption.t list)
    (f : unit -> 'a) : 'a =
  let k = key_of assumptions in
  match Hashtbl.find_opt cache k with
  | Some v -> v
  | None ->
    let v = f () in
    Hashtbl.add cache k v;
    v

(* [find_opt cache assumptions]: cache-only lookup, no compute fallback.
   Used by callers that want to distinguish hit from miss explicitly
   (for incrementing per-tier hit/miss counters). *)
let find_opt (cache : 'a t) (assumptions : Assumption.t list) : 'a option =
  Hashtbl.find_opt cache (key_of assumptions)

let add (cache : 'a t) (assumptions : Assumption.t list) (v : 'a) : unit =
  Hashtbl.replace cache (key_of assumptions) v

let size (cache : 'a t) : int = Hashtbl.length cache
