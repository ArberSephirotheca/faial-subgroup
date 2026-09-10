module Subgroup_solver = Drf.Subgroup_solver
module Subgroup_uniformity = Drf.Subgroup_uniformity
module Solve_drf = Drf.Solve_drf
module Verdict = Drf.Analysis.Verdict

type ordinary = Drf.Analysis.t = {
  kernel : Protocols.Kernel.t;
  report : Solve_drf.Solution.t list;
  vacuous : Protocols.Exp.bexp option;
}

type subgroup = {
  kernel : Inference.Subgroup_matrix.Kernel.t;
  memory : Subgroup_solver.memory_outcome;
  uniformity : Subgroup_uniformity.function_result;
  vacuous : Protocols.Exp.bexp option;
}

type t = Ordinary of ordinary | Subgroup of subgroup

let ordinary_is_safe = Drf.Analysis.is_safe
let ordinary_verdict = Drf.Analysis.verdict

let subgroup_full_verdict (a : subgroup) : Subgroup_uniformity.full_verdict =
  Subgroup_uniformity.compose
    (Subgroup_solver.uniformity_memory_component
       (Subgroup_solver.memory_verdict a.memory))
    (Subgroup_uniformity.function_verdict a.uniformity)

let subgroup_is_safe (a : subgroup) : bool =
  match a.vacuous with
  | Some _ -> false
  | None -> (
      match subgroup_full_verdict a with
      | Subgroup_uniformity.Drf_full -> true
      | Subgroup_uniformity.Not_drf -> false)

let is_safe : t -> bool = function
  | Ordinary ordinary -> ordinary_is_safe ordinary
  | Subgroup subgroup -> subgroup_is_safe subgroup
