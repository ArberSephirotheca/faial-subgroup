module Subgroup_solver = Drf.Subgroup_solver
module Subgroup_uniformity = Drf.Subgroup_uniformity

module Verdict = struct
  type t = Drf | Racy | Timeout | Vacuous

  let to_string : t -> string = function
    | Drf -> "drf"
    | Racy -> "racy"
    | Timeout -> "timeout"
    | Vacuous -> "vacuous"
end

type legacy = {
  kernel : Protocols.Kernel.t;
  report : Solve_drf.Solution.t list;
  vacuous : Protocols.Exp.bexp option;
}

type subgroup = {
  kernel : Inference.Subgroup_matrix.Kernel.t;
  memory : Subgroup_solver.memory_outcome;
  uniformity : Subgroup_uniformity.function_result;
}

type t = Legacy of legacy | Subgroup of subgroup

let legacy_is_safe (a : legacy) : bool =
  a.report |> List.for_all Solve_drf.Solution.is_safe

let legacy_verdict (a : legacy) : Verdict.t =
  match a.vacuous with
  | Some _ -> Verdict.Vacuous
  | None ->
      let has_race, has_unknown =
        List.fold_left
          (fun (race, unknown) (s : Solve_drf.Solution.t) ->
            match s.outcome with
            | Solve_drf.Outcome.Racy _ -> (true, unknown)
            | Solve_drf.Outcome.Unknown -> (race, true)
            | Solve_drf.Outcome.Drf | Solve_drf.Outcome.Drf_with_core _ ->
                (race, unknown))
          (false, false) a.report
      in
      if has_race then Verdict.Racy
      else if has_unknown then Verdict.Timeout
      else Verdict.Drf

let subgroup_memory_component (memory : Subgroup_solver.memory_outcome) :
    Subgroup_uniformity.memory_component =
  match Subgroup_solver.memory_verdict memory with
  | Subgroup_solver.Memory_drf -> Subgroup_uniformity.Memory_drf
  | Subgroup_solver.Memory_racy | Subgroup_solver.Memory_unknown
  | Subgroup_solver.Memory_timeout | Subgroup_solver.Memory_unsupported ->
      Subgroup_uniformity.Memory_not_drf

let subgroup_full_verdict (a : subgroup) : Subgroup_uniformity.full_verdict =
  Subgroup_uniformity.compose
    (subgroup_memory_component a.memory)
    (Subgroup_uniformity.function_verdict a.uniformity)

let subgroup_is_safe (a : subgroup) : bool =
  match subgroup_full_verdict a with
  | Subgroup_uniformity.Drf_full -> true
  | Subgroup_uniformity.Not_drf -> false

let is_safe : t -> bool = function
  | Legacy legacy -> legacy_is_safe legacy
  | Subgroup subgroup -> subgroup_is_safe subgroup
