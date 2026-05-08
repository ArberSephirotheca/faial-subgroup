open Protocols

type t = Modifier.t Variable.Map.t

let empty : t = Variable.Map.empty

let add (x : Variable.t) (m : Modifier.t) (s : t) : t =
  Variable.Map.add x m s

let find (x : Variable.t) (s : t) : Modifier.t =
  match Variable.Map.find_opt x s with
  | Some m -> m
  | None -> Modifier.Unif

let locals (s : t) : Variable.Set.t =
  Variable.Map.fold
    (fun x m acc ->
      if Modifier.equal m Local then Variable.Set.add x acc else acc)
    s Variable.Set.empty

let mentions_local (s : t) (vars : Variable.Set.t) : bool =
  Variable.Set.exists (fun x -> Modifier.equal (find x s) Local) vars
