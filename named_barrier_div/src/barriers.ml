(* barriers(s) — the set of barrier ids syntactically reachable inside
   [s]. Used by [Adv-Skip] in tier 2: a non-matching task with head
   [sync m; s'] can be left in place only if the firing id [n] does
   not appear anywhere in [s'] — otherwise firing [n] now would
   prematurely advance other tasks while a future arrival at [n] is
   still possible.

   A barrier id is the [Exp.nexp] from [Sync.t.id]. We use syntactic
   comparison; symbolic refinement (SMT-based equality under a pc)
   is future work. *)

open Protocols

module Id = struct
  type t = Exp.nexp

  let of_sync (s : Sync.t) : t = s.id
  let equal (a : t) (b : t) : bool = a = b
  let compare = compare
  let to_string = Exp.n_to_string
end

module IdSet = Set.Make (Id)

(* Whether a Sync.t is a *blocking* rendezvous — what NBD treats as a
   sync. The non-blocking modes (Arrive, Wait, ArriveAndDrop) are
   handled as no-ops by tier 1 and don't contribute to [barriers]. *)
let is_blocking (s : Sync.t) : bool =
  match s.mode with
  | Sync.Mode.ArriveAndWait -> true
  | Sync.Mode.Arrive | Sync.Mode.Wait | Sync.Mode.ArriveAndDrop -> false

let rec of_code (c : Code.t) : IdSet.t =
  match c with
  | Skip | Access _ -> IdSet.empty
  | Sync s when is_blocking s -> IdSet.singleton (Id.of_sync s)
  | Sync _ -> IdSet.empty
  | If (_, p, q) -> IdSet.union (of_code p) (of_code q)
  | Seq (p, q) -> IdSet.union (of_code p) (of_code q)
  | Loop { body; _ } -> of_code body
  | Decl { body; _ } -> of_code body
