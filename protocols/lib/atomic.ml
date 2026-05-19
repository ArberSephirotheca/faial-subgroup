open Stage0

module Scope = struct
  type t =
    | Device (* visible to any thread in the same device *)
    | Block (* visible to any thread in the same block *)
    | System (* visible to any thread in the same host (multi-gpu) *)

  let to_string : t -> string = function
    | Device -> ""
    | Block -> "_block"
    | System -> "_system"
end

module Operation = struct
  (* CUDA atomic operations and WGSL counterparts. Parametric over the
     operand expression type so the same shape works at both the
     Infer_exp and Exp layers; [map] converts operands across layers
     at [Infer_stmt.to_stmt]. The operand under each constructor is
     [None] when the frontend couldn't lower it (e.g. a non-trivial
     expression), [Some _] otherwise. *)
  type 'a t =
    | Add of 'a option
    | Sub of 'a option
    | Inc of 'a option
    | Dec of 'a option
    | And of 'a option
    | Or of 'a option
    | Xor of 'a option
    | Min of 'a option
    | Max of 'a option
    | Exch of 'a option
    | CAS of { expected : 'a option; new_val : 'a option }

  let to_string : 'a t -> string = function
    | Add _ -> "atomicAdd"
    | Sub _ -> "atomicSub"
    | Inc _ -> "atomicInc"
    | Dec _ -> "atomicDec"
    | And _ -> "atomicAnd"
    | Or _ -> "atomicOr"
    | Xor _ -> "atomicXor"
    | Min _ -> "atomicMin"
    | Max _ -> "atomicMax"
    | Exch _ -> "atomicExch"
    | CAS _ -> "atomicCAS"

  let map (f : 'a -> 'b) : 'a t -> 'b t = function
    | Add e -> Add (Option.map f e)
    | Sub e -> Sub (Option.map f e)
    | Inc e -> Inc (Option.map f e)
    | Dec e -> Dec (Option.map f e)
    | And e -> And (Option.map f e)
    | Or e -> Or (Option.map f e)
    | Xor e -> Xor (Option.map f e)
    | Min e -> Min (Option.map f e)
    | Max e -> Max (Option.map f e)
    | Exch e -> Exch (Option.map f e)
    | CAS { expected; new_val } ->
        CAS
          { expected = Option.map f expected; new_val = Option.map f new_val }

  (* Stateful counterpart to [map]: each operand is converted by the
     state-monad-valued [f]. Used at [Infer_stmt.to_stmt] to lift the
     [Infer_exp.t -> Exp.nexp] conversion through the unknowns state. *)
  let map_state (f : 'a -> ('s, 'b) Stage0.State.t) (op : 'a t) :
      ('s, 'b t) Stage0.State.t =
    let open Stage0.State.Syntax in
    let opt = Stage0.State.option_map f in
    match op with
    | Add e -> let* e = opt e in return (Add e)
    | Sub e -> let* e = opt e in return (Sub e)
    | Inc e -> let* e = opt e in return (Inc e)
    | Dec e -> let* e = opt e in return (Dec e)
    | And e -> let* e = opt e in return (And e)
    | Or e -> let* e = opt e in return (Or e)
    | Xor e -> let* e = opt e in return (Xor e)
    | Min e -> let* e = opt e in return (Min e)
    | Max e -> let* e = opt e in return (Max e)
    | Exch e -> let* e = opt e in return (Exch e)
    | CAS { expected; new_val } ->
        let* expected = opt expected in
        let* new_val = opt new_val in
        return (CAS { expected; new_val })

  (* Operands flattened to a list in a canonical order. Used by
     [fold] / [exists] / [compare] to traverse uniformly without
     re-stating each variant's payload shape. *)
  let to_list : 'a t -> 'a option list = function
    | Add e | Sub e | Inc e | Dec e | And e | Or e | Xor e | Min e | Max e
    | Exch e ->
        [ e ]
    | CAS { expected; new_val } -> [ expected; new_val ]

  (* Variant tag for compare. Keep stable across versions. *)
  let tag_int : 'a t -> int = function
    | Add _ -> 0
    | Sub _ -> 1
    | Inc _ -> 2
    | Dec _ -> 3
    | And _ -> 4
    | Or _ -> 5
    | Xor _ -> 6
    | Min _ -> 7
    | Max _ -> 8
    | Exch _ -> 9
    | CAS _ -> 10

  let compare (cmp : 'a -> 'a -> int) (a : 'a t) (b : 'a t) : int =
    let c = Int.compare (tag_int a) (tag_int b) in
    if c <> 0 then c
    else List.compare (Option.compare cmp) (to_list a) (to_list b)

  let fold (f : 'a -> 'b -> 'b) (op : 'a t) (init : 'b) : 'b =
    List.fold_left
      (fun acc e -> match e with Some e -> f e acc | None -> acc)
      init (to_list op)

  let exists (f : 'a -> bool) (op : 'a t) : bool =
    List.exists (function Some e -> f e | None -> false) (to_list op)

  (* Parse the unscoped operation name (e.g. "atomicAdd"). Scope
     suffixes ("_block" / "_system") are stripped by [from_name]
     before calling this. Operands start as [None]; the caller
     populates them from the surrounding call site. *)
  let from_unscoped_name : string -> _ t option = function
    | "atomicAdd" -> Some (Add None)
    | "atomicSub" -> Some (Sub None)
    | "atomicInc" -> Some (Inc None)
    | "atomicDec" -> Some (Dec None)
    | "atomicAnd" -> Some (And None)
    | "atomicOr" -> Some (Or None)
    | "atomicXor" -> Some (Xor None)
    | "atomicMin" -> Some (Min None)
    | "atomicMax" -> Some (Max None)
    | "atomicExch" -> Some (Exch None)
    | "atomicCAS" -> Some (CAS { expected = None; new_val = None })
    | _ -> None
end

type 'a t = {
  operation : 'a Operation.t;
  scope : Scope.t;
  location : Location.t option;
}

let map (f : 'a -> 'b) (a : 'a t) : 'b t =
  { a with operation = Operation.map f a.operation }

let map_state (f : 'a -> ('s, 'b) Stage0.State.t) (a : 'a t) :
    ('s, 'b t) Stage0.State.t =
  let open Stage0.State.Syntax in
  let* operation = Operation.map_state f a.operation in
  return { a with operation }

(* Split a fully-qualified atomic name (e.g. "atomicAdd_block") into
   the unscoped operation name and the scope. *)
let split_scope (name : string) : (string * Scope.t) option =
  let strip suffix =
    let ls = String.length suffix in
    let ln = String.length name in
    if ln >= ls && String.sub name (ln - ls) ls = suffix then
      Some (String.sub name 0 (ln - ls))
    else None
  in
  match strip "_block" with
  | Some unscoped -> Some (unscoped, Scope.Block)
  | None -> (
      match strip "_system" with
      | Some unscoped -> Some (unscoped, Scope.System)
      | None -> Some (name, Scope.Device))

let from_name (x : Variable.t) : 'a t option =
  match split_scope (Variable.name x) with
  | Some (unscoped, scope) -> (
      match Operation.from_unscoped_name unscoped with
      | Some operation ->
          Some { operation; scope; location = Variable.location_opt x }
      | None -> None)
  | None -> None

let is_valid (x : Variable.t) : bool =
  match split_scope (Variable.name x) with
  | Some (unscoped, _) -> Operation.from_unscoped_name unscoped <> None
  | None -> false

let to_string (a : 'a t) : string =
  Operation.to_string a.operation ^ Scope.to_string a.scope
