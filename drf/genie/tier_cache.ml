(* Per-[compute_verdict] Φ-keyed result cache for the Tier 1 and
   Tier 2 gate predicates.

   Motivation: the abductive loop re-evaluates the same Φ across
   round-accept → shrink → re-check → weaken → re-shrink → re-check
   sequences. Each gate evaluation runs Z3 work proportional to the
   number of accesses / pair fragments; caching the [bool] outcome
   keyed by Φ avoids the redundant SMT calls.

   Key shape: the assumes carried by [App.t] is
   [(kernel_name, bexp list) list]. The normalised key is the same
   list with each kernel's [bexp]s rendered through [Exp.b_to_string]
   and sorted lexicographically, then the per-kernel list is sorted
   by kernel name. Two semantically-equivalent Φs that differ only in
   clause order share a key. *)

open Protocols

type key = string

type 'a t = (key, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create 32

(* Render an [(kn, bexp list) list] into a canonical string. Within
   each kernel the clauses are sorted by their [b_to_string] form;
   across kernels, the pairs are sorted by kernel name. The "\x01"
   / "\x02" separators are byte sequences that don't appear in
   identifier names or in [b_to_string] output. *)
let key_of (assumes : (string * Exp.bexp list) list) : key =
  assumes
  |> List.map (fun (kn, bs) ->
    let bs_str =
      bs
      |> List.map Exp.b_to_string
      |> List.sort String.compare
      |> String.concat "\x01"
    in
    (kn, bs_str))
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map (fun (kn, bs_str) -> kn ^ "\x02" ^ bs_str)
  |> String.concat "\x03"

(* [lookup_or_compute cache assumes f]: returns the cached value for
   the normalised key; on miss, runs [f ()], stores its result, and
   returns it. The cache's hit/miss counters are *not* incremented
   here — the caller is responsible for the bookkeeping so it can use
   per-tier stat keys. *)
let lookup_or_compute (cache : 'a t)
    (assumes : (string * Exp.bexp list) list)
    (f : unit -> 'a) : 'a =
  let k = key_of assumes in
  match Hashtbl.find_opt cache k with
  | Some v -> v
  | None ->
    let v = f () in
    Hashtbl.add cache k v;
    v

(* [find_opt cache assumes]: cache-only lookup, no compute fallback.
   Used by callers that want to distinguish hit from miss explicitly
   (for incrementing per-tier hit/miss counters). *)
let find_opt (cache : 'a t)
    (assumes : (string * Exp.bexp list) list) : 'a option =
  Hashtbl.find_opt cache (key_of assumes)

let add (cache : 'a t)
    (assumes : (string * Exp.bexp list) list) (v : 'a) : unit =
  Hashtbl.replace cache (key_of assumes) v

let size (cache : 'a t) : int = Hashtbl.length cache
