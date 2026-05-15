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
  |> (fun s -> Variable.Set.diff s Variable.launch_config_set)

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
   kernel parameter at all. *)
let abductive_scope (k : Kernel.t) : Variable.Set.t =
  let params = kernel_param_set k in
  let entries = partition k in
  let from_code = parameter_universe entries in
  let from_pre =
    Variable.Set.inter (b_free_names k.pre Variable.Set.empty) params
  in
  let open Variable in
  Set.union from_code from_pre
  |> Set.union
       (Set.of_list [ bdim_x; bdim_y; bdim_z; gdim_x; gdim_y; gdim_z ])
