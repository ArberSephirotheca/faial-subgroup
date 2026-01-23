open Protocols
open Vectors
module IntMap = Stage0.Common.IntMap
module Task = Transaction.Task

type t =
  | BankConflicts
  | UncoalescedAccesses
  | UncoalescedAccessesSat
  | CountAccesses
  | ActiveThreads

let to_string : t -> string = function
  | BankConflicts -> "bc"
  | UncoalescedAccesses -> "ua"
  | UncoalescedAccessesSat -> "ua-sat"
  | CountAccesses -> "count"
  | ActiveThreads -> "active"

let values : t list =
  [
    BankConflicts;
    UncoalescedAccesses;
    UncoalescedAccessesSat;
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
  | UncoalescedAccesses | UncoalescedAccessesSat ->
      max_uncoalesced_accesses ~thread_count
  | CountAccesses -> max_count_accesses
  | ActiveThreads -> thread_count

let max_cost_from (cfg : Config.t) : t -> int =
  max_cost ~thread_count:cfg.threads_per_warp ~bank_count:cfg.bank_count

let min_cost (m : t) : int =
  match m with
  | BankConflicts -> min_bank_conflicts
  | UncoalescedAccesses | UncoalescedAccessesSat -> min_uncoalesced_accesses
  | CountAccesses -> 1
  | ActiveThreads -> 0

let supports_memory (memory : Protocols.Memory.t) (metric : t) : bool =
  match metric with
  | BankConflicts -> Protocols.Memory.is_shared memory
  | UncoalescedAccesses | UncoalescedAccessesSat ->
      Protocols.Memory.is_global memory
  | CountAccesses | ActiveThreads -> true

let supported_arrays (arrays : Protocols.Memory.t Protocols.Variable.Map.t)
    (metric : t) : Protocols.Variable.Set.t =
  Protocols.Variable.Map.fold
    (fun var memory acc ->
      if supports_memory memory metric then Protocols.Variable.Set.add var acc
      else acc)
    arrays Protocols.Variable.Set.empty

(* ---- *)

module TransactionMap = struct
  type t = Transaction.t IntMap.t

  let max (m : t) : Transaction.t =
    IntMap.fold
      (fun _ tsx1 tsx2 -> Transaction.max tsx1 tsx2)
      m Transaction.zero

  let count (w : t) : int = IntMap.cardinal w
  let max_transaction_count (w : t) : int = max w |> Transaction.count
  let to_string (w : t) : string = max w |> Transaction.to_string

  let make (to_transaction_id : Task.t -> int) (indices : int array)
      (enabled : bool array) (tids : Dim3.t array) : t =
    let tid_idx = Array.combine tids indices in
    Array.combine tid_idx enabled
    |> Array.fold_left
         (fun res ((id, index), enabled) ->
           if enabled then
             let task = Task.{ id; index } in
             let bid = to_transaction_id task in
             IntMap.update bid
               (fun o ->
                 Some
                   (let m =
                      match o with Some m -> m | None -> Transaction.make bid
                    in
                    Transaction.add task m))
               res
           else res)
         IntMap.empty
end

let bank_conflicts (bank_count : int) (indices : int array)
    (enabled : bool array) (tids : Dim3.t array) : Cost.t =
  let to_bid (tsk : Task.t) : int = Stage0.Common.modulo tsk.index bank_count in
  let w = TransactionMap.make to_bid indices enabled tids in
  let state = TransactionMap.max w in
  (* we need to get the maximum, because all threads may be disabled,
     in which case, we would get a transaction count of 0 and therefore
     a cost of -1 *)
  Cost.make ~value:(max (Transaction.count state - 1) 0) ~state ~exact:true ()

let uncoalesced (indices : int array) (enabled : bool array)
    (tids : Dim3.t array) : Cost.t =
  let warp_count = Array.length tids in
  let tsx_map =
    let to_tsx_id (tsk : Task.t) : int = tsk.index / warp_count in
    TransactionMap.make to_tsx_id indices enabled tids
  in
  let state =
    tsx_map |> IntMap.bindings
    |> List.map (fun (_, e) -> Transaction.choose e)
    |> Transaction.from_list 0
  in
  Cost.make ~value:(IntMap.cardinal tsx_map) ~state ~exact:true ()

let run ?(verbose = false) ~bank_count (m : t) (indices : NMap.t)
    (enabled : BMap.t) (tids : Dim3.t array) : (Cost.t, string) Result.t =
  let a_indices = NMap.to_array indices in
  let a_enabled = BMap.to_array enabled in
  let is_valid : bool =
    Array.combine a_indices a_enabled
    |> Array.for_all (fun (idx, enabled) -> (not enabled) || idx >= 0)
  in
  if is_valid then begin
    let cost =
      match m with
      | BankConflicts -> bank_conflicts bank_count a_indices a_enabled tids
      | UncoalescedAccesses -> uncoalesced a_indices a_enabled tids
      | UncoalescedAccessesSat -> uncoalesced a_indices a_enabled tids
      | CountAccesses -> Cost.from_int ~value:1 ~exact:true ()
      | ActiveThreads ->
          Cost.from_int ~value:(BMap.count true enabled) ~exact:true ()
    in
    (if verbose then
       Array.map2
         (fun idx enabled -> if enabled then string_of_int idx else "_")
         a_indices a_enabled
       |> Array.to_list |> String.concat ", "
       |> fun x -> print_endline ("[" ^ x ^ "] -> " ^ Cost.to_string cost));
    Ok cost
  end
  else Error "index out of bounds"
