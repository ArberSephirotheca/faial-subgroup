(* Static classification of a kernel's accesses by whether their
   reachability or DRF-relevant variables include kernel parameters,
   versus only CUDA built-ins ([threadIdx], [blockIdx], [blockDim],
   [gridDim]) and literals.

   An access whose path condition AND index expression mention no
   kernel parameter is invariant under any [--assume KERNEL:BEXP]
   clauses the abductive search synthesises: those clauses range
   over kernel parameters by construction, and an access free of
   parameter references cannot have its reachability OR address
   touched by them. Such accesses live under [Parameter_free]. The
   remaining [Parameter_touching] entries record the parameter
   subset across the access's full surface — path condition plus
   index — so the caller can union those subsets and scope the
   abductive pool to variables that can affect at least one access.
   Restricting to path-condition free names alone would drop
   load-bearing pool candidates of the form [param == dim] that
   bind an index-resident parameter to a launch dim. *)

open Protocols
open Exp

(* [Parameter_free.statically_reachable] is the bool the gate uses
   for these entries (since their reachability is parameter-
   independent). Kept always [true] in this phase, matching the
   current gate's accept-on-Unknown stance; a sharper static check
   via [Reachability.preconditions_check] is deferred. *)
type access_class =
  | Parameter_free of { statically_reachable : bool }
  | Parameter_touching of {
      params : Variable.Set.t;
      path_cond : bexp;
    }

type entry = {
  access : Access.t;
  path_cond : bexp;
  klass : access_class;
}

let classify (kernel_params : Variable.Set.t) (access : Access.t)
    (path_cond : bexp) : access_class =
  let fvs =
    b_free_names path_cond Variable.Set.empty
    |> Access.free_names access
  in
  let touched = Variable.Set.inter fvs kernel_params in
  if Variable.Set.is_empty touched
  then Parameter_free { statically_reachable = true }
  else Parameter_touching { params = touched; path_cond }

let kernel_param_set (k : Kernel.t) : Variable.Set.t =
  Variable.Set.union
    (Params.to_set k.global_variables)
    (Params.to_set k.local_variables)
  |> (fun s -> Variable.Set.diff s Variable.runtime_set)

let partition (k : Kernel.t) : entry list =
  let params = kernel_param_set k in
  Reachability.walk k.code
  |> List.map (fun (access, path_cond) ->
    { access; path_cond; klass = classify params access path_cond })

let parameter_universe (entries : entry list) : Variable.Set.t =
  List.fold_left (fun acc e ->
    match e.klass with
    | Parameter_free _ -> acc
    | Parameter_touching { params; _ } -> Variable.Set.union acc params)
    Variable.Set.empty entries

(* The launch-dim built-ins ([blockDim.*], [gridDim.*]) of every axis
   any access actually navigates. Used by [Abduction] to weight pool
   candidates: a candidate whose dim references fall outside this set
   is paying selector cost on an axis the kernel does not address, so
   MaxSAT can demote it without losing recall under [--assume-launch]
   (where the wrapper [k.pre] already pins unused dims).

   An access navigates an axis when its index expression mentions any
   of that axis four launch-config built-ins ([threadIdx.x],
   [blockIdx.x], [blockDim.x], [gridDim.x] for the x axis, analogous
   for y and z). Once an axis is touched, both [blockDim.axis] and
   [gridDim.axis] are added: candidates of the shape
   [param >= blockDim.axis * gridDim.axis] (per-thread spacing across
   the launch) need both, and including only one would demote the
   cross-dim product. Tid/bid axes are not added because [build_pool]
   never uses them as candidate RHS.

   Path conditions are intentionally excluded. A path condition like
   [threadIdx.x < N] is always present on tid-indexed accesses and
   would flood the axis set; the index itself is the sharper signal
   for which dims the kernel addresses. *)
let accessed_dims (k : Kernel.t) : Variable.Set.t =
  let open Variable in
  let touched =
    partition k
    |> List.fold_left (fun acc e -> Access.free_names e.access acc)
         Set.empty
  in
  let touches (axis_vars : Variable.t list) : bool =
    List.exists (fun v -> Set.mem v touched) axis_vars
  in
  let add_if (cond : bool) (dims : Variable.t list)
      (acc : Set.t) : Set.t =
    if cond then List.fold_left (fun s v -> Set.add v s) acc dims
    else acc
  in
  Set.empty
  |> add_if (touches [ tid_x; bid_x; bdim_x; gdim_x ]) [ bdim_x; gdim_x ]
  |> add_if (touches [ tid_y; bid_y; bdim_y; gdim_y ]) [ bdim_y; gdim_y ]
  |> add_if (touches [ tid_z; bid_z; bdim_z; gdim_z ]) [ bdim_z; gdim_z ]

(* Variables the abductive pool must be free to range over. The
   kernel-parameter part of the scope is taken from anywhere a
   parameter could affect a DRF query: in [k.pre] (typically a
   launch argument tied to a dim via [assume_launch], e.g.
   [gridDim.x == __faial_launch_arg_1]), in some access's path
   condition, or in some access's index. Parameters absent from all
   three cannot influence any DRF query and are dropped.

   The six dim built-ins ([bdim_x..z], [gdim_x..z]) are always
   included because [build_pool] uses them as RHS in shapes like
   [param >= dim] and [dim <= K]; filtering them by scope would
   discard load-bearing dim-cap candidates that don't reference any
   kernel parameter at all.

   The filter applied to [k.pre]'s free names is thread-invariance:
   Φ from abductive synthesis is conjoined into [k.pre] and so
   constrains every thread uniformly, which means its variables must
   not be thread-divergent. Thread indices ([threadIdx.*],
   [blockIdx.*]) are the only thread-divergent launch-config
   built-ins; dim built-ins and kernel parameters are uniform per
   launch. Stating this as an explicit exclusion of
   [Variable.id_set] (rather than relying on tids being
   absent from [globals]/[locals]) keeps the filter correct under a
   future IR where every free variable, including thread indices, is
   bound in the kernel's parameter sets. *)
let abductive_universe (k : Kernel.t) : Variable.Set.t =
  Variable.Set.union (kernel_param_set k) Variable.runtime_set
  |> (fun s -> Variable.Set.diff s Variable.id_set)

(* Kernel params that [k.pre] equates to a launch-config dim
   (e.g. [gridDim.x == N], or its symmetric [N == gridDim.x]).
   Such params are load-bearing for the abductive search even
   when no access references them directly: candidates of the
   shape [v == dim] / [v >= dim] / [v >= dim_a * dim_b] tie them
   back to the kernel's launch shape. Other pre-only params (the
   wrapper's loop counters [i] and [repeat], synthesised from a
   host-side [for (i = 0; i < repeat; ...)] around the launch
   site) appear in [k.pre] only as a non-equality bound like
   [i < repeat]; they don't shape any access, and including them
   pads the abductive pool with cross-param candidates of the
   form [repeat >= Win] that exercise Z3's nonlinear-arithmetic
   solver on the irrelevant axis. *)
let pre_dim_equated (pre : bexp) : Variable.Set.t =
  let dims = Variable.runtime_set in
  let rec conjuncts (b : bexp) : bexp list =
    match b with
    | BRel (BAnd, b1, b2) -> conjuncts b1 @ conjuncts b2
    | _ -> [ b ]
  in
  let pair_of = function
    | NRel (Eq, Var a, Var b) -> Some (a, b)
    | _ -> None
  in
  conjuncts pre
  |> List.fold_left (fun acc b ->
       match pair_of b with
       | Some (a, b) when Variable.Set.mem a dims
                          && not (Variable.Set.mem b dims) ->
           Variable.Set.add b acc
       | Some (a, b) when Variable.Set.mem b dims
                          && not (Variable.Set.mem a dims) ->
           Variable.Set.add a acc
       | _ -> acc)
       Variable.Set.empty

let abductive_scope (k : Kernel.t) : Variable.Set.t =
  let entries = partition k in
  let from_code = parameter_universe entries in
  let kp = kernel_param_set k in
  let dim_equated = pre_dim_equated k.pre in
  (* Filter [from_pre] to drop kernel params that appear in [k.pre]
     only in non-equality preconditions (the wrapper-loop-counter
     case [i < repeat]). Dim built-ins always pass through. *)
  let from_pre =
    Variable.Set.inter
      (b_free_names k.pre Variable.Set.empty)
      (abductive_universe k)
    |> Variable.Set.filter (fun v ->
         not (Variable.Set.mem v kp)
         || Variable.Set.mem v from_code
         || Variable.Set.mem v dim_equated)
  in
  let open Variable in
  Set.union from_code from_pre
  |> Set.union
       (Set.of_list [ bdim_x; bdim_y; bdim_z; gdim_x; gdim_y; gdim_z ])
