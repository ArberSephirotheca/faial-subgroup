(* Static classification of a kernel's accesses by whether their path
   condition depends on kernel parameters or only on CUDA built-ins
   (threadIdx, blockIdx, blockDim, gridDim) and literals.

   An access whose path condition mentions no kernel parameter cannot
   have its reachability changed by [--assume KERNEL:BEXP] clauses
   synthesised by the abductive search — those clauses range over
   kernel parameters by construction. Such accesses live under
   [Parameter_free]. The remaining [Parameter_touching] entries
   record the exact parameter subset their path condition references,
   which the caller can union to scope the abductive pool to the
   variables that can actually act on at least one access. *)

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

let classify (kernel_params : Variable.Set.t) (path_cond : bexp) : access_class =
  let fvs = b_free_names path_cond Variable.Set.empty in
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
    { access; path_cond; klass = classify params path_cond })

let parameter_universe (entries : entry list) : Variable.Set.t =
  List.fold_left (fun acc e ->
    match e.klass with
    | Parameter_free _ -> acc
    | Parameter_touching { params; _ } -> Variable.Set.union acc params)
    Variable.Set.empty entries
