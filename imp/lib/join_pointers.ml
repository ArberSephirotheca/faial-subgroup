open Stage0
open Protocols
open State.Syntax

type env = Pointer.t Variable.Map.t
type 'a state = (env, 'a) State.t

let binds : Stmt.t -> Variable.Set.t =
  let add (s : Stmt.t) (acc : Variable.Set.t) : Variable.Set.t =
    match s with
    | Decl d -> Variable.Set.add d.var acc
    | Assign a -> Variable.Set.add a.var acc
    | LocationAlias a -> Variable.Set.add a.target acc
    | Atomic a -> Variable.Set.add a.target acc
    | Read { target = Some (_, x); _ } -> Variable.Set.add x acc
    | Call { result = Some (x, _); _ } -> Variable.Set.add x acc
    | For (r, _) -> Variable.Set.add r.var acc
    | Read _ | Call _ | Skip | Seq _ | Sync _ | Assert _ | Write _ | If _
    | Star _ ->
        acc
  in
  fun s -> Stmt.fold add s Variable.Set.empty

let forget (s : Stmt.t) : unit state =
  State.update (Variable.Set.fold Variable.Map.remove (binds s))

let bind_pointer ((target : Variable.t), (pointer : Pointer.t)) : unit state =
  State.update (Variable.Map.add target pointer)

let alias ((target : Variable.t), (pointer : Pointer.t)) : Stmt.t =
  LocationAlias { target; pointer }

let resolve (p : Pointer.t) : Pointer.t state =
  let* env = State.get in
  return (Pointer.subst_bases (fun x -> Variable.Map.find_opt x env) p)

let branch (m : 'a state) : ('a * env) state =
  let* before = State.get in
  let* x = m in
  let* after = State.get in
  let* () = State.put before in
  return (x, after)

let join ~(cond : Exp.bexp) ~(rebound : Variable.Set.t) ~(before : env)
    ~(if_true : env) ~(if_false : env) : (Variable.t * Pointer.t) list =
  let outlives (p : Pointer.t) : bool =
    Variable.Set.disjoint (Pointer.free_names p Variable.Set.empty) rebound
  in
  Variable.Map.fold
    (fun x on_true acc ->
      match Variable.Map.find_opt x if_false with
      | None -> acc
      | Some on_false ->
          let p =
            if on_true = on_false then on_true
            else Pointer.select ~cond ~if_true:on_true ~if_false:on_false
          in
          if Variable.Map.find_opt x before = Some p || not (outlives p) then
            acc
          else (x, p) :: acc)
    if_true []

let rec walk (s : Stmt.t) : Stmt.t state =
  match s with
  | Seq (s1, s2) ->
      let* s1 = walk s1 in
      let* s2 = walk s2 in
      return (Stmt.Seq (s1, s2))
  | If (cond, s1, s2) ->
      let* before = State.get in
      let* s1, if_true = branch (walk s1) in
      let* s2, if_false = branch (walk s2) in
      let s : Stmt.t = If (cond, s1, s2) in
      let joined = join ~cond ~rebound:(binds s) ~before ~if_true ~if_false in
      let* () = forget s in
      let* () = State.list_iter bind_pointer joined in
      return (Stmt.seq s (Stmt.from_list (List.map alias joined)))
  | For (r, body) ->
      let* () = forget s in
      let* body, _ = branch (walk body) in
      return (Stmt.For (r, body))
  | Star body ->
      let* () = forget s in
      let* body, _ = branch (walk body) in
      return (Stmt.Star body)
  | LocationAlias { target; pointer } ->
      let* pointer = resolve pointer in
      let* () = bind_pointer (target, pointer) in
      return s
  | Skip | Sync _ | Assert _ | Write _ -> return s
  | Decl _ | Assign _ | Read _ | Atomic _ | Call _ ->
      let* () = forget s in
      return s

let from_stmt (s : Stmt.t) : Stmt.t =
  State.run_result (walk s) Variable.Map.empty
