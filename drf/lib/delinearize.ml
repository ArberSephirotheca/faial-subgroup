open Protocols

module Phase_timer = Stage0.Phase_timer
module Stats = Stage0.Stats

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
  val add_bound : t -> Poly.t -> Poly.t -> t
  val get_bounds : t -> Exp.bexp list
end

(* Build the standard [0 <= i] /\ [i < d] conjunction from an inner-axis
   index expression and its dimension. *)
let make_bound (i : Poly.t) (d : Poly.t) : Exp.bexp =
  let open Exp in
  b_and (n_le (Num 0) (Poly.to_nexp i)) (n_lt (Poly.to_nexp i) (Poly.to_nexp d))

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
  type scope = Poly.t Lazy.t Variable.Map.t
  let initial_scope = Variable.Map.empty

  (* Lazy [ub + 1] in [Poly.t] form. Built once per loop entry, capturing
     the [globals] in scope there, then reused across every emit-site
     inside the loop body. *)
  let cache_entry ~globals (upper_bound : Exp.nexp) : Poly.t Lazy.t =
    lazy (Poly.( + ) (Poly.from_nexp ~globals upper_bound) (Poly.of_int 1))

  let add_range ~globals (r : Range.t) (s : scope) : scope =
    match r.lower_bound with
    | Exp.Num 0 -> Variable.Map.add r.var (cache_entry ~globals r.upper_bound) s
    | _ -> s

  type t = { ranges : scope; bounds : Exp.bexp list }
  let create ranges = { ranges; bounds = [] }

  (* [Some v] iff [i] is a single bare induction variable [v] with
     coefficient 1: one polynomial term, one factor with exponent 1, and
     that factor classifies as [Indet.Induction (Var v)]. *)
  let as_single_induction_var (i : Poly.t) : Variable.t option =
    let ( let* ) = Option.bind in
    let* t = match Poly.to_list i with
      | [t] when Mono.coeff t = 1 -> Some t
      | _ -> None
    in
    let* a = match Mono.factors t with
      | [(a, 1)] -> Some a
      | _ -> None
    in
    Indet.as_induction_var a

  let provable ~(ranges : scope) (i : Poly.t) (d : Poly.t) : bool =
    if Variable.Map.is_empty ranges then false
    else
      let ( let* ) = Option.bind in
      let outcome =
        let* v = as_single_induction_var i in
        let* lhs = Variable.Map.find_opt v ranges in
        Some (Poly.compare (Lazy.force lhs) d = 0)
      in
      Option.value outcome ~default:false

  let add_bound (s : t) (i : Poly.t) (d : Poly.t) : t =
    if provable ~ranges:s.ranges i d then s
    else { s with bounds = make_bound i d :: s.bounds }

  let get_bounds s = s.bounds
end

type bound_oracle = scope:Exp.bexp list -> bound:Exp.bexp -> bool

let trivially_true_oracle : bound_oracle =
  fun ~scope:_ ~bound:_ -> true

module Make (A : Algorithm.S) (G : BoundGenerator) : sig
  val from_exp :
    scope:G.scope ->
    loop_scope:Exp.bexp list ->
    check:bound_oracle ->
    radix:Poly.t list ->
    Poly.t ->
    t option
  val rewrite_kernel :
    rewrite_access:bool ->
    assume:bool ->
    check:bound_oracle ->
    Aligned.Kernel.t ->
    Aligned.Kernel.t
end = struct
  let from_exp ~(scope : G.scope)
      ~(loop_scope : Exp.bexp list) ~(check : bound_oracle)
      ~(radix : Poly.t list) (expr : Poly.t) : t option =
    let ( let* ) = Option.bind in
    let* idx = A.delinearize ~radix expr in
    let inner_is = match (idx : Subscript.t).numeral with
      | _ :: rest -> rest
      | [] -> failwith "from_exp: empty numeral list"
    in
    let final =
      List.fold_right
        (fun (d, i) acc -> G.add_bound acc i d)
        (List.combine idx.radix inner_is)
        (G.create scope)
    in
    let all_bounds = G.get_bounds final in
    if check ~scope:loop_scope ~bound:(Exp.b_and_ex all_bounds)
    then
      Some {
        indices = List.map Poly.to_nexp idx.numeral;
        dims = List.map Poly.to_nexp idx.radix;
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

  let viable_arrays
      ~(globals : Variable.Set.t)
      ~(scope : G.scope)
      ~(loop_scope : Exp.bexp list)
      ~(check : bound_oracle)
      ~(radix_map : Poly.t list Variable.Map.t)
      (unsync : Unsynced.t) : Variable.Set.t =
    let open Unsynced in
    let rec walk (scope : G.scope) (loop_scope : Exp.bexp list)
        (failed : Variable.Set.t) : Unsynced.t -> Variable.Set.t = function
      | Access { array; index = [a]; _ }
        when not (Variable.Set.mem array failed) ->
        (match Variable.Map.find_opt array radix_map with
         | None -> Variable.Set.add array failed
         | Some radix ->
           let a = Poly.from_nexp ~globals a in
           match
             from_exp ~scope ~loop_scope ~check ~radix a
           with
           | Some _ -> failed
           | None -> Variable.Set.add array failed)
      | Access _ -> failed
      | Skip | Assert _ -> failed
      | Cond (_, b) -> walk scope loop_scope failed b
      | Loop (Norm_range.Plain r, b) ->
        let scope' = G.add_range ~globals r scope in
        let loop_scope' = Range.to_cond r :: loop_scope in
        walk scope' loop_scope' failed b
      | Loop (Norm_range.Index _, b) ->
        (* loops are normalized only after delinearization *)
        walk scope loop_scope failed b
      | Seq (a, b) ->
        walk scope loop_scope (walk scope loop_scope failed a) b
    in
    let failed = walk scope loop_scope Variable.Set.empty unsync in
    Variable.Map.fold (fun arr _ viable ->
      if Variable.Set.mem arr failed then viable
      else Variable.Set.add arr viable)
      radix_map Variable.Set.empty

  let rewrite_unsync
      ~(globals : Variable.Set.t)
      ~(scope : G.scope)
      ~(loop_scope : Exp.bexp list)
      ~(check : bound_oracle)
      ~(rewrite_access : bool)
      ~(assume : bool)
      (unsync : Unsynced.t) : Unsynced.t =
    let open Unsynced in
    let accs =
      Phase_timer.measure "delin/get-accesses" (fun () -> get_accesses unsync)
    in
    (* Per array, the single-index access polynomials, or [None] if any
       access is multi-index (nothing to delinearise). *)
    let acc_polys_map =
      accs
      |> Variable.Map.filter_map (fun _ accesses ->
        accesses
        |> List.fold_left (fun acc -> function
          | [a] -> Option.map (fun xs -> Poly.from_nexp ~globals a :: xs) acc
          | _ -> None
        ) (Some []))
    in
    (* Infer one shared radix per array jointly from all its accesses;
       [yields]'s first result is the biggest radix that decodes every
       access. Arrays with no such radix are dropped (non-viable). *)
    let radix_map = Phase_timer.measure "delin/dims" (fun () ->
      acc_polys_map
      |> Variable.Map.filter_map (fun _ polys ->
        let size_params = Shape.size_params_all polys in
        A.yields ~globals ~size_params polys
        |> Seq.uncons
        |> Option.map (fun ((radix, _), _) -> radix)))
    in
    let viable = Phase_timer.measure "delin/viability" (fun () ->
      viable_arrays ~globals ~scope ~loop_scope ~check ~radix_map
        unsync)
    in
    let committed_stat, refused_stat =
      if assume then "delin/assumed", "delin/vacuity-refused"
      else "delin/sound-rewritten", "delin/unprovable"
    in
    radix_map
    |> Variable.Map.iter (fun array _ ->
      if Variable.Set.mem array viable
      then Stats.incr committed_stat
      else Stats.incr refused_stat);
    let rec walk (scope : G.scope) (loop_scope : Exp.bexp list)
        : Unsynced.t -> Unsynced.t = function
      | Access ({ array; index = [a]; _ } as acc)
        when Variable.Set.mem array viable ->
        let radix = Variable.Map.find array radix_map in
        let a = Poly.from_nexp ~globals a in
        (match
           Phase_timer.measure "delin/from-exp" (fun () ->
             from_exp ~scope ~loop_scope ~check ~radix a)
         with
         | Some t ->
           (* [rewrite_access] off: keep the original 1D access and emit
              only the recovered per-axis bounds. Isolates the bounds'
              contribution from the multidimensional rewrite. *)
           let body =
             if rewrite_access
             then Unsynced.Access { acc with index = t.indices }
             else Unsynced.Access acc
           in
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
      | Loop (Norm_range.Plain r, b) ->
        let scope' = G.add_range ~globals r scope in
        let loop_scope' = Range.to_cond r :: loop_scope in
        Loop (Norm_range.Plain r, walk scope' loop_scope' b)
      | Loop ((Norm_range.Index _ as r), b) ->
        (* loops are normalized only after delinearization *)
        Loop (r, walk scope loop_scope b)
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
      ~(rewrite_access : bool)
      ~(assume : bool)
      : Aligned.Code.t -> Aligned.Code.t =
    let open Aligned.Code in
    function
    | Sync c ->
      Sync (rewrite_unsync ~globals ~scope ~loop_scope ~check
              ~rewrite_access ~assume c)
    | Loop ({ range; body; _ } as loop) ->
      let globals =
        if Variable.Set.subset (Range.free_names range Variable.Set.empty) globals
        then Variable.Set.add range.var globals
        else globals
      in
      let scope = G.add_range ~globals range scope in
      let loop_scope = Range.to_cond range :: loop_scope in
      Loop { loop with body =
        rewrite_aligned ~globals ~scope
          ~loop_scope ~check ~rewrite_access ~assume body }
    | Seq (a, b) ->
      Seq
        ( rewrite_aligned ~globals ~scope ~loop_scope ~check
            ~rewrite_access ~assume a,
          rewrite_aligned ~globals ~scope ~loop_scope ~check
            ~rewrite_access ~assume b )

  let rewrite_kernel ~(rewrite_access : bool) ~(assume : bool)
      ~(check : bound_oracle) (kernel : Aligned.Kernel.t) : Aligned.Kernel.t =
    let globals = Params.to_set kernel.global_variables in
    { kernel with code =
        rewrite_aligned ~globals ~scope:G.initial_scope ~loop_scope:[]
          ~check ~rewrite_access ~assume kernel.code }
end

module All = Make (Greedy) (AllBounds)
module Maslov_elide = Make (Greedy) (Maslov)

module Algo = struct
  type t =
    | Greedy
    | Ics15
    | Ics15_opt
    | Cramer

  let default = Ics15_opt

  let to_string = function
    | Greedy -> "greedy"
    | Ics15 -> "ics15"
    | Ics15_opt -> "ics15-opt"
    | Cramer -> "cramer"

  (* Name/value pairs for [Cmdliner.Arg.enum]. *)
  let enum =
    [
      ("greedy", Greedy);
      ("ics15", Ics15);
      ("ics15-opt", Ics15_opt);
      ("cramer", Cramer);
    ]

  let to_module : t -> (module Algorithm.S) = function
    | Greedy -> (module Greedy)
    | Ics15 -> (module Ics15)
    | Ics15_opt -> (module Ics15_opt)
    | Cramer -> (module Cramer)
end

(* Entry point from the DRF pipeline. *)
let translate ~(enabled : bool) ~(rewrite : bool) ~(elide : bool)
    ~(check_vacuosity : bool) ~(algo : Algo.t)
    (kernels : Aligned.Kernel.t Stage0.Streamutil.stream) :
    Aligned.Kernel.t Stage0.Streamutil.stream =
  if not enabled then kernels
  else
    let algo_mod : (module Algorithm.S) = Algo.to_module algo in
    let bg : (module BoundGenerator) =
      if elide then (module Maslov) else (module AllBounds)
    in
    let module A = (val algo_mod) in
    let module G = (val bg) in
    let module M = Make (A) (G) in
    let rewrite_one =
      if check_vacuosity then
        (* When checking for vacuosity, we test the generated bounds until
            we find the first non-vacuous one. *)
        fun (kernel : Aligned.Kernel.t) ->
          let open Exp in
          let runtime =
            Params.to_bexp
              (Params.union_left kernel.global_variables kernel.local_variables)
          in
          let base = b_and kernel.pre runtime in
          Gen_z3.CachedSolver.with_assertion base (fun s ->
            let check ~scope ~bound =
              Gen_z3.CachedSolver.is_possible s (b_and (b_and_ex scope) bound)
            in
            M.rewrite_kernel ~rewrite_access:rewrite ~assume:enabled ~check kernel)
      else
        (* Default: no vacuosity checks are done. *)
        fun (kernel : Aligned.Kernel.t) ->
          M.rewrite_kernel ~rewrite_access:rewrite ~assume:enabled
            ~check:trivially_true_oracle kernel
    in
    Stage0.Streamutil.map rewrite_one kernels
