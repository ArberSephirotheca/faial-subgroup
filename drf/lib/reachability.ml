(* Per-access reachability at the protocol level.

   We walk [Protocols.Code.t] directly — one entry per [Access] node
   in the source — accumulating the path condition (the AND of
   enclosing [If] guards and [Loop] range conditions). For each
   access, the reachability question is:

     ∃ thread state: kernel.pre ∧ base ∧ runtime ∧ path_cond

   where [base] is the architecture's per-thread constraints
   ([Architecture.Defaults.base] — tid bounds, dim bounds, etc.)
   *without* [thread_distinct] (which is a two-task DRF artifact;
   reachability is a single-thread question). [runtime] is the
   kernel's parameter typing.

   SAT (with model) = the access is reachable.
   UNSAT            = the precondition admits no thread state that
                      reaches this access.

   The check operates on the parsed protocol; the caller is
   responsible for applying any user [--assume] flags and other
   precondition layering via [prepare_kernel] before calling
   [check_kernel]. *)

open Stage0
open Protocols
open Exp

module AccessId = struct
  type t = {
    kernel_name : string;
    array_name : string;
    access_index : int;
    location : Location.t;
  }

  let compare (a : t) (b : t) : int =
    let c = String.compare a.kernel_name b.kernel_name in
    if c <> 0 then c
    else
      let c = String.compare a.array_name b.array_name in
      if c <> 0 then c
      else
        let c = Int.compare a.access_index b.access_index in
        if c <> 0 then c
        else
          String.compare
            (Location.to_string a.location)
            (Location.to_string b.location)

  let to_string (a : t) : string =
    Printf.sprintf "%s:%s[%d]@%s" a.kernel_name a.array_name a.access_index
      (Location.to_string a.location)
end

module AccessSet = Set.Make (AccessId)

module Status = struct
  type t =
    | Reachable of Z3.Model.model
    | Unreachable
    | Unknown of string

  let to_string : t -> string = function
    | Reachable _ -> "reachable"
    | Unreachable -> "unreachable"
    | Unknown msg -> "unknown(" ^ msg ^ ")"
end

module Witness = struct
  type t = {
    id : AccessId.t;
    access : Access.t;
    model : Z3.Model.model;
  }

  let location (w : t) : Location.t = w.id.location
end

type entry = {
  id : AccessId.t;
  access : Access.t;
  status : Status.t;
}

(* Apply the same precondition layering App.translate's "map" phase
   does, minus the [apply_arch] distinct clause. Result: a protocol
   kernel whose [pre] includes user [--assume]s, [--assume-dims],
   inlined globals, and [Architecture.Defaults.base] — but no
   [thread_distinct] (which would require the [Other] constructor
   for the second-thread reduction we don't do here). *)
let prepare_kernel
    ?(arch = Architecture.Block)
    ~(assumes : Exp.bexp list)
    ~(assume_dims : bool)
    ~(params : (string * int) list)
    (k : Kernel.t) : Kernel.t =
  let d = Architecture.to_defaults arch in
  k
  |> (fun k -> List.fold_left (fun k b -> Kernel.add_pre b k) k assumes)
  |> (if assume_dims then Kernel.add_dim_assumptions else Fun.id)
  |> Kernel.inline_globals params
  |> Kernel.apply_arch_binders d
  |> Kernel.add_pre Architecture.Defaults.base
  |> Kernel.add_missing_binders
  |> Kernel.opt

(* Walk [Code.t] collecting one (access, path_cond) per [Access]
   node. [path_cond] is the AND of all enclosing [If] guards and
   [Loop] range conditions. Order of accesses follows the source. *)
let walk (code : Code.t) : (Access.t * bexp) list =
  let rec aux (env : bexp) (acc : (Access.t * bexp) list) : Code.t -> (Access.t * bexp) list = function
    | Code.Access a -> (a, env) :: acc
    | Code.Sync _ | Code.Skip -> acc
    | Code.If (b, p, q) ->
      let acc = aux (b_and env b) acc p in
      aux (b_and env (b_not b)) acc q
    | Code.Loop { range; body } ->
      aux (b_and env (Range.to_cond range)) acc body
    | Code.Seq (p, q) ->
      let acc = aux env acc p in
      aux env acc q
    | Code.Decl { body; _ } -> aux env acc body
  in
  aux (Bool true) [] code |> List.rev

let check_kernel ?(timeout = 0) (k : Kernel.t) : entry list =
  let runtime =
    Params.to_bexp (Params.union_left k.global_variables k.local_variables)
  in
  walk k.code
  |> List.mapi (fun i (access, path_cond) ->
    let goal = b_and_ex [ k.pre; runtime; path_cond ] in
    let goal = Predicates.b_inline goal in
    let status : Status.t =
      match Gen_z3.Bv64Gen.solve ~timeout goal with
      | Ok (Gen_z3.Solver.Sat m) -> Reachable m
      | Ok Gen_z3.Solver.Unsat -> Unreachable
      | Error msg -> Unknown msg
    in
    let id : AccessId.t =
      {
        kernel_name = k.name;
        array_name = Variable.name (Access.array access);
        access_index = i;
        location = Access.location access;
      }
    in
    { id; access; status })

(* Simpler "gate" variant: a single SAT query per kernel asking
   whether [k.pre ∧ runtime] is satisfiable. UNSAT = the
   accumulated preconditions contradict each other (or the kernel
   context); SAT = at least one thread state is admitted. Does
   not visit individual accesses, so cost is O(1) Z3 calls per
   kernel rather than O(accesses).

   Misses cases where the constraints render only *specific*
   accesses unreachable while others remain (the per-access gate
   below catches those). On the HeCBench dataset the two variants
   produce the same verdict on every observed clearance. *)
let preconditions_satisfiable ?(timeout = 0) (k : Kernel.t) : bool =
  let runtime =
    Params.to_bexp (Params.union_left k.global_variables k.local_variables)
  in
  let goal = Exp.b_and k.pre runtime |> Predicates.b_inline in
  match
    Phase_timer.measure "gate/solve" (fun () ->
      Gen_z3.Bv64Gen.solve ~timeout goal)
  with
  | Ok (Gen_z3.Solver.Sat _) -> true
  | Ok Gen_z3.Solver.Unsat -> false
  | Error _ -> true (* on Unknown, accept rather than reject *)

let reachable_set (entries : entry list) : AccessSet.t =
  entries
  |> List.filter_map (fun e ->
    match e.status with Reachable _ -> Some e.id | _ -> None)
  |> AccessSet.of_list

let witnesses (entries : entry list) : Witness.t list =
  entries
  |> List.filter_map (fun e ->
    match e.status with
    | Reachable m -> Some Witness.{ id = e.id; access = e.access; model = m }
    | _ -> None)
