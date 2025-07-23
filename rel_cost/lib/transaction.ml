open Protocols

module IntSet = Stage0.Common.IntSet

module Task = struct
  type t = {
    id: Dim3.t;
    index: int;
  }

  let to_string (e:t) : string =
    "[" ^ string_of_int e.index ^ "]:" ^ Dim3.to_string e.id
end

type t = {
  bank: int;
  accesses: Task.t list;
  indices: IntSet.t;
  count: int;
}

let choose_opt (e:t) : Task.t option =
  match e.accesses with
  | x :: _ -> Some x
  | [] -> None

let choose (e:t) : Task.t =
  choose_opt e |> Option.get

let to_string (e:t) : string =
  let accs =
    e.accesses
    |> List.map Task.to_string
    |> String.concat ", "
  in
  let bc = IntSet.cardinal e.indices |> string_of_int in
  let bank = string_of_int e.bank in
  bc ^ " @ " ^ bank ^ " " ^ accs

let make (bank:int) : t =
  {bank; accesses=[]; indices=IntSet.empty; count=0}

let zero : t =
  make 0

let count (e:t) : int =
  e.count

let max (lhs:t) (rhs:t) : t =
  if lhs.count >= rhs.count then
    lhs
  else
    rhs

let add (task:Task.t) (tsx:t) : t =
  if IntSet.mem task.index tsx.indices then
    tsx
  else
    let indices = IntSet.add task.index tsx.indices in
    { tsx with
      accesses = task :: tsx.accesses;
      indices;
      count = tsx.count + 1;
    }

let from_list (bank:int) : Task.t list -> t =
  List.fold_left (fun tsx tsk -> add tsk tsx) (make bank)

