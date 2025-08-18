type t =
  | BankConflicts
  | UncoalescedAccesses
  | UncoalescedAccesses2
  | CountAccesses
  | ActiveThreads

let to_string : t -> string = function
  | BankConflicts -> "bc"
  | UncoalescedAccesses -> "ua"
  | UncoalescedAccesses2 -> "ua2"
  | CountAccesses -> "count"
  | ActiveThreads -> "active"

let values : t list =
  [
    BankConflicts;
    UncoalescedAccesses;
    UncoalescedAccesses2;
    CountAccesses;
    ActiveThreads;
  ]

let choices : (string * t) list = values |> List.map (fun x -> (to_string x, x))
let min_uncoalesced_accesses : int = 1
let min_bank_conflicts : int = 0
let max_count_accesses : int = 1

let max_uncoalesced_accesses ~thread_count : int =
  assert (thread_count >= 0);
  thread_count

let max_bank_conflicts ~thread_count ~bank_count : int =
  assert (thread_count >= 0 && bank_count >= 0);
  (* calculate the maximum number of transactions *)
  let max_transactions = min thread_count bank_count in
  (* don't return negative numbers *)
  max (max_transactions - 1) 0

let max_cost ~thread_count ~bank_count : t -> int = function
  | BankConflicts -> max_bank_conflicts ~thread_count ~bank_count
  | UncoalescedAccesses -> max_uncoalesced_accesses ~thread_count
  | UncoalescedAccesses2 -> max_uncoalesced_accesses ~thread_count
  | CountAccesses -> max_count_accesses
  | ActiveThreads -> thread_count

let max_cost_from (cfg : Config.t) : t -> int =
  max_cost ~thread_count:cfg.threads_per_warp ~bank_count:cfg.bank_count

let min_cost (m : t) : int =
  match m with
  | BankConflicts -> min_bank_conflicts
  | UncoalescedAccesses -> min_uncoalesced_accesses
  | UncoalescedAccesses2 -> min_uncoalesced_accesses
  | CountAccesses -> 1
  | ActiveThreads -> 0

let supports_memory (memory : Protocols.Memory.t) (metric : t) : bool =
  match metric with
  | BankConflicts -> Protocols.Memory.is_shared memory
  | UncoalescedAccesses -> Protocols.Memory.is_global memory
  | UncoalescedAccesses2 -> Protocols.Memory.is_global memory
  | CountAccesses -> true
  | ActiveThreads -> true

let supported_arrays (arrays : Protocols.Memory.t Protocols.Variable.Map.t)
    (metric : t) : Protocols.Variable.Set.t =
  Protocols.Variable.Map.fold
    (fun var memory acc ->
      if supports_memory memory metric then Protocols.Variable.Set.add var acc
      else acc)
    arrays Protocols.Variable.Set.empty
