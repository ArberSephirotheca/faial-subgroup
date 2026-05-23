open Stage0
open Protocols
open Delin

let list_to_string (f : 'a -> string) (l : 'a list): string =
  "[" ^ (l |> List.map f |> String.concat "; ") ^ "]"

type t = {
  indices : Exp.nexp list;
  dims : Exp.nexp list;
  conditions : Exp.bexp list;
}

let to_string ({ indices; dims; conditions } : t) : string =
  Printf.sprintf "{ indices = %s; dims = %s; conditions = %s }"
    (list_to_string Exp.n_to_string indices)
    (list_to_string Exp.n_to_string dims)
    (list_to_string Exp.b_to_string conditions)

(* A [BoundGenerator] decides, per delinearised access, which inner-axis
   bounds [0 <= i_k < d_k] make it into [t.conditions]. The two type
   members are independent:

   - [scope] carries lexical-scope information (e.g., loop-induction
     ranges). It is threaded through the rewriter recursion and extended
     at each [Loop] entry via [add_range]. Lifetime: spans the whole
     rewrite, scoped by the surrounding [Loop]s.

   - [t] is the per-access accumulator built at the access site via
     [create scope], grown by [add_bound], and drained by [get_bounds].
     Lifetime: one [from_exp] call. The rewriter does not thread it
     across accesses.

   Two implementations live in this file: [AllBounds] emits the standard
   [0 <= i < d] conjunction for every axis; [Maslov] emits only the bounds
   it cannot prove statically. *)
module type BoundGenerator = sig
  type scope
  val initial_scope : scope
  val add_range :
    globals:Variable.Set.t -> Range.t -> scope -> scope

  type t
  val create : scope -> t
  val add_bound : t -> Expr.t -> Expr.t -> t
  val get_bounds : t -> Exp.bexp list
end

(* Build the standard [0 <= i] /\ [i < d] conjunction from an inner-axis
   index expression and its dimension. *)
let make_bound (i : Expr.t) (d : Expr.t) : Exp.bexp =
  let open Exp in
  b_and (n_le (Num 0) (Expr.to_nexp i)) (n_lt (Expr.to_nexp i) (Expr.to_nexp d))

module AllBounds : BoundGenerator = struct
  type scope = unit
  let initial_scope = ()
  let add_range ~globals:_ _ () = ()

  type t = Exp.bexp list
  let create () = []
  let add_bound bs i d = make_bound i d :: bs
  let get_bounds bs = bs
end

(* Maslov-style elision: keep only the bounds that cannot be proved from
   the enclosing loops' [Range.t]s. The provability check recognises one
   pattern: [i] is a single induction variable [v] whose loop has
   [lower_bound = Num 0] and whose [upper_bound + 1] normalises to [d]
   under the [globals] in scope at the loop's entry. *)
module Maslov : BoundGenerator = struct
  type scope = Expr.t Lazy.t Variable.Map.t
  let initial_scope = Variable.Map.empty

  (* Lazy [ub + 1] in [Expr.t] form. Built once per loop entry, capturing
     the [globals] in scope there, then reused across every emit-site
     inside the loop body. *)
  let cache_entry ~globals (upper_bound : Exp.nexp) : Expr.t Lazy.t =
    lazy (Expr.( + ) (Expr.from_nexp ~globals upper_bound) (Expr.of_int 1))

  let add_range ~globals (r : Range.t) (s : scope) : scope =
    match r.lower_bound with
    | Exp.Num 0 -> Variable.Map.add r.var (cache_entry ~globals r.upper_bound) s
    | _ -> s

  type t = { ranges : scope; bounds : Exp.bexp list }
  let create ranges = { ranges; bounds = [] }

  (* [Some v] iff [i] is a single bare induction variable [v] with
     coefficient 1: one polynomial term, one factor with exponent 1, and
     that factor classifies as [Atom.Induction (Var v)]. *)
  let as_single_induction_var (i : Expr.t) : Variable.t option =
    let ( let* ) = Option.bind in
    let* t = match Expr.to_list i with
      | [t] when Term.coeff t = 1 -> Some t
      | _ -> None
    in
    let* a = match Term.factors t with
      | [(a, 1)] -> Some a
      | _ -> None
    in
    Atom.as_induction_var a

  let provable ~(ranges : scope) (i : Expr.t) (d : Expr.t) : bool =
    if Variable.Map.is_empty ranges then false
    else
      let ( let* ) = Option.bind in
      let outcome =
        let* v = as_single_induction_var i in
        let* lhs = Variable.Map.find_opt v ranges in
        Some (Expr.compare (Lazy.force lhs) d = 0)
      in
      Option.value outcome ~default:false

  let add_bound (s : t) (i : Expr.t) (d : Expr.t) : t =
    if provable ~ranges:s.ranges i d then s
    else { s with bounds = make_bound i d :: s.bounds }

  let get_bounds s = s.bounds
end

(* The rewriter, parameterised over a polynomial driver and a
   bound-generation strategy. *)
(* The bound-check oracle threaded through the rewriter. Given the
   enclosing loop-scope conjuncts and a candidate bound, returns true
   iff [kernel.pre /\ runtime /\ scope ==> bound]. A constant-true
   oracle bypasses the check (preserves the pre-existing
   assume-bounds behaviour); the Z3-backed oracle from
   [Bound_check.entails] makes the bounds proof obligations the
   rewriter discharges before committing to delinearisation. *)
type bound_oracle = scope:Exp.bexp list -> bound:Exp.bexp -> bool

let trivially_true_oracle : bound_oracle =
  fun ~scope:_ ~bound:_ -> true

module Make (A : DelinAlgorithm) (G : BoundGenerator) : sig
  val from_exp :
    globals:Variable.Set.t ->
    scope:G.scope ->
    loop_scope:Exp.bexp list ->
    check:bound_oracle ->
    size_params:Term.t list ->
    Expr.t ->
    t option
  val rewrite_kernel :
    check:bound_oracle -> Aligned.Kernel.t -> Aligned.Kernel.t
end = struct
  let from_exp ~(globals : Variable.Set.t) ~(scope : G.scope)
      ~(loop_scope : Exp.bexp list) ~(check : bound_oracle)
      ~(size_params : Term.t list) (expr : Expr.t) : t option =
    let ( let* ) = Option.bind in
    let* (idx, _) = Seq.uncons (A.candidates ~globals ~size_params expr) in
    let inner_is = match idx.Index.indices with
      | _ :: rest -> rest
      | [] -> failwith "from_exp: empty indices list"
    in
    (* [fold_right] so that bounds end up in axis order in
       [get_bounds], since [add_bound] in the standard
       implementations prepends. *)
    let final =
      List.fold_right
        (fun (d, i) acc -> G.add_bound acc i d)
        (List.combine idx.dims inner_is)
        (G.create scope)
    in
    let all_bounds = idx.conditions @ G.get_bounds final in
    if List.for_all (fun b -> check ~scope:loop_scope ~bound:b) all_bounds
    then
      Some {
        indices = List.map Expr.to_nexp idx.indices;
        dims = List.map Expr.to_nexp idx.dims;
        conditions = all_bounds;
      }
    else
      None

  let get_accesses (unsync : Unsynced.t) : Exp.nexp list list Variable.Map.t =
    let open Unsynced in
    let rec walk = function
      | Skip | Assert _ -> Fun.id
      | Access {array; index; _} -> Variable.Map.add_to_list array index
      | Cond (_, u) -> walk u
      | Loop (_, u) -> walk u
      | Seq (u, v) -> Fun.compose (walk u) (walk v)
    in walk unsync Variable.Map.empty

  (* Walk the code computing, per array, whether every access site
     produces a successful [from_exp] result in its own scope. An array
     is "viable" iff every access to it delinearises cleanly. This
     enforces the per-array shape-unification invariant the verifier
     assumes ([Flatacc.Code.dim] uses one index-length value for the
     entire array, so mixed-arity per-array IR breaks the alias check).
     Arrays that already have multi-index accesses are skipped (no
     entry in [size_params_map]) so they remain non-viable. *)
  let viable_arrays
      ~(globals : Variable.Set.t)
      ~(scope : G.scope)
      ~(loop_scope : Exp.bexp list)
      ~(check : bound_oracle)
      ~(size_params_map : Term.t list Variable.Map.t)
      (unsync : Unsynced.t) : Variable.Set.t =
    let open Unsynced in
    let rec walk (scope : G.scope) (loop_scope : Exp.bexp list)
        (failed : Variable.Set.t) : Unsynced.t -> Variable.Set.t = function
      | Access { array; index = [a]; _ }
        when not (Variable.Set.mem array failed) ->
        (match Variable.Map.find_opt array size_params_map with
         | None -> Variable.Set.add array failed
         | Some size_params ->
           let a = Expr.from_nexp ~globals a in
           match
             from_exp ~globals ~scope ~loop_scope ~check ~size_params a
           with
           | Some _ -> failed
           | None -> Variable.Set.add array failed)
      | Access _ -> failed
      | Skip | Assert _ -> failed
      | Cond (_, b) -> walk scope loop_scope failed b
      | Loop (r, b) ->
        let scope' = G.add_range ~globals r scope in
        let loop_scope' = Range.to_cond r :: loop_scope in
        walk scope' loop_scope' failed b
      | Seq (a, b) ->
        walk scope loop_scope (walk scope loop_scope failed a) b
    in
    let failed = walk scope loop_scope Variable.Set.empty unsync in
    Variable.Map.fold (fun arr _ viable ->
      if Variable.Set.mem arr failed then viable
      else Variable.Set.add arr viable)
      size_params_map Variable.Set.empty

  let rewrite_unsync
      ~(globals : Variable.Set.t)
      ~(scope : G.scope)
      ~(loop_scope : Exp.bexp list)
      ~(check : bound_oracle)
      (unsync : Unsynced.t) : Unsynced.t =
    let open Unsynced in
    (* Sub-phases measured separately so the JSON phase_times shows
       where delin time actually goes; they sum to ~all of
       [rewrite_unsync] (modulo glue). *)
    let accs =
      Phase_timer.measure "delin/get-accesses" (fun () -> get_accesses unsync)
    in
    let size_params_map = Phase_timer.measure "delin/dims" (fun () ->
      accs
      |> Variable.Map.filter_map (fun _ accesses ->
        (* Skip arrays that already have multi-index accesses; nothing
           to delinearise. Returning [None] drops the array from
           [size_params_map], so the array is excluded from viability
           and its accesses pass through unchanged. *)
        accesses
        |> List.fold_left (fun acc -> function
          | [a] -> Option.map (fun xs -> Expr.from_nexp ~globals a :: xs) acc
          | _ -> None
        ) (Some [])
        |> Option.map size_params_all))
    in
    let viable = Phase_timer.measure "delin/viability" (fun () ->
      viable_arrays ~globals ~scope ~loop_scope ~check ~size_params_map
        unsync)
    in
    let rec walk (scope : G.scope) (loop_scope : Exp.bexp list)
        : Unsynced.t -> Unsynced.t = function
      | Access ({ array; index = [a]; _ } as acc)
        when Variable.Set.mem array viable ->
        let size_params = Variable.Map.find array size_params_map in
        let a = Expr.from_nexp ~globals a in
        (match
           Phase_timer.measure "delin/from-exp" (fun () ->
             from_exp ~globals ~scope ~loop_scope ~check ~size_params a)
         with
         | Some t ->
           let body = Unsynced.Access { acc with index = t.indices } in
           List.fold_right
             (fun c b -> Unsynced.Seq (Assert c, b))
             t.conditions
             body
         | None ->
           (* Defensive: viable means every access succeeded in the
              first walk. If something changed between the two walks
              (shouldn't, [from_exp] is pure), fall through to linear. *)
           Access acc)
      | Access _ as code -> code
      | Cond (p, b) -> Cond (p, walk scope loop_scope b)
      | Loop (r, b) ->
        let scope' = G.add_range ~globals r scope in
        let loop_scope' = Range.to_cond r :: loop_scope in
        Loop (r, walk scope' loop_scope' b)
      | Seq (a, b) ->
        Seq (walk scope loop_scope a, walk scope loop_scope b)
      | code -> code
    in
    Phase_timer.measure "delin/rewrite"
      (fun () -> walk scope loop_scope unsync)

  let rec rewrite_aligned
      ~(globals : Variable.Set.t)
      ~(scope : G.scope)
      ~(loop_scope : Exp.bexp list)
      ~(check : bound_oracle)
      : Aligned.Code.t -> Aligned.Code.t =
    let open Aligned.Code in
    function
    | Sync c -> Sync (rewrite_unsync ~globals ~scope ~loop_scope ~check c)
    | Loop ({ range; body; _ } as loop) ->
      let globals' = Variable.Set.add range.var globals in
      let scope' = G.add_range ~globals:globals' range scope in
      let loop_scope' = Range.to_cond range :: loop_scope in
      Loop { loop with body =
        rewrite_aligned ~globals:globals' ~scope:scope'
          ~loop_scope:loop_scope' ~check body }
    | Seq (a, b) ->
      Seq
        ( rewrite_aligned ~globals ~scope ~loop_scope ~check a,
          rewrite_aligned ~globals ~scope ~loop_scope ~check b )

  let rewrite_kernel ~(check : bound_oracle) (kernel : Aligned.Kernel.t)
      : Aligned.Kernel.t =
    let globals = Params.to_set kernel.global_variables in
    { kernel with code =
        rewrite_aligned ~globals ~scope:G.initial_scope ~loop_scope:[]
          ~check kernel.code }
end

module All = Make (Greedy) (AllBounds)
module Maslov_elide = Make (Greedy) (Maslov)
