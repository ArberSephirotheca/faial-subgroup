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

(* Variant of [prepare_kernel] that returns the same prepared kernel
   plus the [(name, value)] substitutions [inline_globals] applied to
   reach it. The caller can reuse those bindings to substitute the
   same values into separately-supplied bexps (e.g. a CEGAR round's
   precondition pushed into a cached gate solver), keeping delta
   assertions consistent with the cached base encoding. *)
let prepare_kernel_with_kvs
    ?(arch = Architecture.Block)
    ~(assumes : Exp.bexp list)
    ~(assume_dims : bool)
    ~(params : (string * int) list)
    (k : Kernel.t) : Kernel.t * (string * int) list =
  let d = Architecture.to_defaults arch in
  let k = List.fold_left (fun k b -> Kernel.add_pre b k) k assumes in
  let k = if assume_dims then Kernel.add_dim_assumptions k else k in
  let k = Kernel.assign_globals params k in
  let dim_kvs =
    let to_dim name = function
      | Some d -> Dim3.to_assoc ~prefix:(name ^ ".") d
      | None -> []
    in
    to_dim "blockDim" k.block_dim @ to_dim "gridDim" k.grid_dim
  in
  let k = Kernel.assign_globals dim_kvs k in
  let inferred_kvs =
    Kernel.constants k
    |> List.filter (fun (x, _) ->
      Params.mem (Variable.from_name x) k.global_variables)
  in
  let k = Kernel.assign_globals inferred_kvs k in
  let k =
    k
    |> Kernel.apply_arch_binders d
    |> Kernel.add_pre Architecture.Defaults.base
    |> Kernel.add_missing_binders
    |> Kernel.opt
  in
  (k, params @ dim_kvs @ inferred_kvs)

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

(* Incremental gate. Each kernel keeps a persistent [(ctx, solver)]
   where the base encoding ([kernel.pre + runtime] under
   [prepare_kernel ~assumes:[]]) has been added once and stays in
   the solver across rounds. The [subst] captures the [(name,
   value)] bindings [prepare_kernel] applied — globals from
   [--param], blockDim/gridDim literals, and constants
   [inline_inferred] discovered in the base pre. Per round we
   substitute those values into the round's assumes before pushing,
   so the delta references the same symbols the base does. *)
module Slot = struct
  type t = {
    ctx : Z3.context;
    solver : Z3.Solver.solver;
    subst : Subst.SubstAssoc.t;
  }
end

let make_slot ?(arch = Architecture.Block)
    ~(assume_dims : bool) ~(params : (string * int) list)
    ~(timeout : int option) (k : Kernel.t) : Slot.t =
  let base, kvs =
    prepare_kernel_with_kvs ~arch ~assumes:[] ~assume_dims ~params k
  in
  let runtime =
    Params.to_bexp
      (Params.union_left base.global_variables base.local_variables)
  in
  let base_goal = Exp.b_and base.pre runtime |> Predicates.b_inline in
  let args =
    match timeout with
    | Some t -> [ ("timeout", string_of_int t) ]
    | None -> []
  in
  let ctx = Z3.mk_context args in
  let solver = Z3.Solver.mk_solver ctx None in
  Z3.Solver.add solver [ Gen_z3.Bv64Gen.b_to_expr ctx base_goal ];
  let subst =
    kvs
    |> List.map (fun (n, v) -> (n, Exp.Num v))
    |> Subst.SubstAssoc.make
  in
  Slot.{ ctx; solver; subst }

(* Check whether [base ∧ delta_assumes] is satisfiable, where the
   base is the slot's preloaded assertion and [delta_assumes] are
   the round's preconditions substituted through [slot.subst]. The
   substituted delta is pushed, checked, then popped so the solver
   returns to its base state but keeps any learned clauses. *)
let preconditions_satisfiable_delta
    (slot : Slot.t) (delta_assumes : Exp.bexp list) : bool =
  let delta =
    delta_assumes
    |> List.map (Subst.ReplaceAssoc.b_subst slot.subst)
    |> List.map Predicates.b_inline
    |> Exp.b_and_ex
  in
  Z3.Solver.push slot.solver;
  Z3.Solver.add slot.solver [ Gen_z3.Bv64Gen.b_to_expr slot.ctx delta ];
  let result =
    Phase_timer.measure "gate/solve" (fun () ->
      Z3.Solver.check slot.solver [])
  in
  Z3.Solver.pop slot.solver 1;
  match result with
  | Z3.Solver.SATISFIABLE -> true
  | Z3.Solver.UNSATISFIABLE -> false
  | Z3.Solver.UNKNOWN -> true

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
