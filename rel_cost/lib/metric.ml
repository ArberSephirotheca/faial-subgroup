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

module type Spec = sig
  (*type t*)
  val min_cost : int
  val max_cost : int -> Config.t -> int
  val supports_memory : Protocols.Memory.t -> bool
  val run: Config.t -> NMap.t -> BMap.t -> Dim3.t array -> Cost.t
end

module BankConflicts = struct
  let min_cost = 0

  let max_cost (thread_count : int) (cfg : Config.t) : int =
    assert (thread_count >= 0 && cfg.bank_count >= 0);
    (* calculate the maximum number of transactions *)
    let max_transactions = min thread_count cfg.bank_count in
    (* don't return negative numbers *)
    max (max_transactions - 1) 0

  let supports_memory memory = Protocols.Memory.is_shared memory

  let run (cfg : Config.t) (indices : NMap.t)
      (enabled : BMap.t) (tids : Dim3.t array) : Cost.t =
    let bank_count = cfg.bank_count in
    let indices = NMap.to_array indices in
    let enabled = BMap.to_array enabled in
    let to_bid (tsk : Task.t) : int = Stage0.Common.modulo tsk.index bank_count in
    let w = TransactionMap.make to_bid indices enabled tids in
    let state = TransactionMap.max w in
    (* we need to get the maximum, because all threads may be disabled,
      in which case, we would get a transaction count of 0 and therefore
      a cost of -1 *)
    Cost.make ~value:(max (Transaction.count state - 1) 0) ~state ~exact:true ()

end

module UncoalescedAccesses = struct
  let min_cost = 1

  let max_cost (thread_count : int) (_ : Config.t) =
    assert (thread_count >= 0);
    thread_count

  let supports_memory memory = Protocols.Memory.is_global memory

  let run (_ : Config.t) (indices : NMap.t) (enabled : BMap.t)
      (tids : Dim3.t array) : Cost.t =
    let indices = NMap.to_array indices in
    let enabled = BMap.to_array enabled in
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

end

module CountAccesses = struct
  let min_cost = 1

  let max_cost _ _ = 1

  let supports_memory _ = true

  let run (_ : Config.t) (_ :  NMap.t) (_ : BMap.t) (_ : Dim3.t array) : Cost.t =
    Cost.from_int ~value:1 ~exact:true ()
end

module ActiveThreads = struct
  let min_cost = 0

  let max_cost thread_count _ =
    assert (thread_count >= 0);
    thread_count

  let supports_memory _ = true

  let run (_ : Config.t) (_ :  NMap.t) (enabled : BMap.t) (_ : Dim3.t array) : Cost.t =
    Cost.from_int ~value:(BMap.count true enabled) ~exact:true ()
end

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

let to_spec : t -> (module Spec) =
  function
  | BankConflicts -> (module BankConflicts)
  | UncoalescedAccesses | UncoalescedAccessesSat -> (module UncoalescedAccesses)
  | CountAccesses -> (module CountAccesses)
  | ActiveThreads -> (module ActiveThreads)


let max_cost (thread_count : int) (cfg: Config.t) (m : t) : int =
  let module S = (val to_spec m : Spec) in
  S.max_cost thread_count cfg

let min_cost (m : t) : int =
  let module S = (val to_spec m : Spec) in
  S.min_cost

let supports_memory (memory : Protocols.Memory.t) (metric : t) : bool =
  let module S = (val to_spec metric : Spec) in
  S.supports_memory memory

let supported_arrays (arrays : Protocols.Memory.t Protocols.Variable.Map.t)
    (metric : t) : Protocols.Variable.Set.t =
  Protocols.Variable.Map.fold
    (fun var memory acc ->
      if supports_memory memory metric then Protocols.Variable.Set.add var acc
      else acc)
    arrays Protocols.Variable.Set.empty

(* ---- *)

let run ?(verbose = false) (config : Config.t) (m : t) (indices : NMap.t)
    (enabled : BMap.t) (tids : Dim3.t array) : (Cost.t, string) Result.t =
  let a_indices = NMap.to_array indices in
  let a_enabled = BMap.to_array enabled in
  let is_valid : bool =
    Array.combine a_indices a_enabled
    |> Array.for_all (fun (idx, enabled) -> (not enabled) || idx >= 0)
  in
  if is_valid then begin
    let cost =
      let module S = (val to_spec m : Spec) in
      S.run config indices enabled tids
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
