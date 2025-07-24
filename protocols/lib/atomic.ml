let atomic_inc : Variable.t = Variable.from_name "atomicInc"

(*
  list of
  atomic{Inc,Dec,Add,Sub,And,Or,Xor,Min,Max,CAS,Exch}
 *)
let device_scope_list : Variable.t list =
  [
    "Inc"; "Dec"; "Add"; "Sub"; "And"; "Or"; "Xor"; "Min"; "Max"; "CAS"; "Exch";
  ]
  (* atomicInc *)
  |> List.map (fun x -> "atomic" ^ x)
  |> List.map Variable.from_name

(*
  set of
  atomic{Inc,Dec,Add,Sub,And,Or,Xor,Min,Max,CAS,Exch}
 *)
let device_scope_set : Variable.Set.t =
  device_scope_list |> Variable.Set.of_list

(*
  list of
  atomic{Inc,Dec,Add,Sub,And,Or,Xor,Min,Max,CAS,Exch}_block
 *)
let block_scope_list : Variable.t list =
  device_scope_list
  |> List.map (Variable.update_name (fun name -> name ^ "_block"))

(*
  set of
  atomic{Inc,Dec,Add,Sub,And,Or,Xor,Min,Max,CAS,Exch}_block
 *)
let block_scope_set : Variable.Set.t = block_scope_list |> Variable.Set.of_list

(*
  list of
  atomic{Inc,Dec,Add,Sub,And,Or,Xor,Min,Max,CAS,Exch}_system
 *)
let system_scope_list : Variable.t list =
  device_scope_list
  |> List.map (Variable.update_name (fun name -> name ^ "_system"))

(*
  set of
  atomic{Inc,Dec,Add,Sub,And,Or,Xor,Min,Max,CAS,Exch}_system
 *)
let system_scope_set : Variable.Set.t =
  system_scope_list |> Variable.Set.of_list

let list : Variable.t list =
  device_scope_list @ block_scope_list @ system_scope_list

let set : Variable.Set.t =
  device_scope_set
  |> Variable.Set.union block_scope_set
  |> Variable.Set.union system_scope_set

let is_valid (x : Variable.t) : bool = Variable.Set.mem x set

module Scope = struct
  type t =
    | Device (* visible to any thread in the same device *)
    | Block (* visible to any thread in the same block *)
    | System (* visible to any thread in the same host (multi-gpu) *)

  let from_variable (x : Variable.t) : t option =
    if Variable.Set.mem x device_scope_set then Some Device
    else if Variable.Set.mem x block_scope_set then Some Block
    else if Variable.Set.mem x system_scope_set then Some System
    else None

  let to_string : t -> string = function
    | Device -> "device"
    | Block -> "block"
    | System -> "system"
end

type t = { name : Variable.t; scope : Scope.t }

let from_name (x : Variable.t) : t option =
  x |> Scope.from_variable |> Option.map (fun s -> { name = x; scope = s })

let to_string (a : t) : string = a.name |> Variable.name
