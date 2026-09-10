open Protocols
module SS = Inference.Subgroup_source

type error = Unsupported_vacuity_check

let error_to_string : error -> string = function
  | Unsupported_vacuity_check ->
      "subgroup ordinary-memory delinearization does not yet support "
      ^ "--delin-avoid-vacuous"

let group_effects_by_array (effects : SS.ordinary_memory_effect list) :
    SS.ordinary_memory_effect list Variable.Map.t =
  List.fold_left
    (fun groups (memory_effect : SS.ordinary_memory_effect) ->
      Variable.Map.add_to_list memory_effect.access.array memory_effect groups)
    Variable.Map.empty effects

let one_dimensional_polys ~(globals : Variable.Set.t)
    (effects : SS.ordinary_memory_effect list) : Poly.t list option =
  List.fold_left
    (fun result (memory_effect : SS.ordinary_memory_effect) ->
      match (result, memory_effect.access.index) with
      | Some polys, [ index ] -> Some (Poly.from_nexp ~globals index :: polys)
      | None, _ | _, _ -> None)
    (Some []) effects

let rewrite_with_algorithm (module A : Algorithm.S) ~(rewrite_access : bool)
    ~(globals : Variable.Set.t) (effects : SS.ordinary_memory_effect list) :
    SS.ordinary_memory_effect list =
  let module M = Delinearize.Make (A) (Delinearize.AllBounds) in
  let groups = group_effects_by_array effects in
  let candidate_radixes =
    Variable.Map.filter_map
      (fun _ grouped ->
        let ( let* ) = Option.bind in
        let* polys = one_dimensional_polys ~globals grouped in
        let size_params = Shape.size_params_all polys in
        A.yields ~globals ~size_params polys
        |> Seq.uncons
        |> Option.map (fun ((radix, _), _) -> radix))
      groups
  in
  let from_index radix index =
    M.from_exp ~scope:Delinearize.AllBounds.initial_scope ~loop_scope:[]
      ~check:(fun ~scope:_ ~bound:_ -> true)
      ~radix
      (Poly.from_nexp ~globals index)
  in
  let viable_radixes =
    Variable.Map.filter
      (fun array radix ->
        Variable.Map.find array groups
        |> List.for_all (fun (memory_effect : SS.ordinary_memory_effect) ->
            match memory_effect.access.index with
            | [ index ] -> Option.is_some (from_index radix index)
            | _ -> false))
      candidate_radixes
  in
  List.map
    (fun (memory_effect : SS.ordinary_memory_effect) ->
      match
        ( Variable.Map.find_opt memory_effect.access.array viable_radixes,
          memory_effect.access.index )
      with
      | Some radix, [ index ] -> (
          match from_index radix index with
          | None -> memory_effect
          | Some delinearized ->
              let access =
                if rewrite_access then
                  { memory_effect.access with index = delinearized.indices }
                else memory_effect.access
              in
              {
                memory_effect with
                access;
                source_conditions =
                  Exp.dedup_conditions
                    (delinearized.conditions @ memory_effect.source_conditions);
              })
      | _ -> memory_effect)
    effects

let rewrite_weak ~(rewrite_access : bool) ~(in_range : bool)
    ~(globals : Variable.Set.t) (effects : SS.ordinary_memory_effect list) :
    SS.ordinary_memory_effect list =
  let groups = group_effects_by_array effects in
  let decompositions =
    Variable.Map.filter_map
      (fun _ grouped ->
        let ( let* ) = Option.bind in
        let* indices = one_dimensional_polys ~globals grouped in
        let frame, bound =
          Pv_decomp.analyze ~globals ~in_range (List.map Poly.to_nexp indices)
        in
        Some (frame, bound))
      groups
  in
  List.map
    (fun (memory_effect : SS.ordinary_memory_effect) ->
      match
        ( Variable.Map.find_opt memory_effect.access.array decompositions,
          memory_effect.access.index )
      with
      | Some (frame, bound), [ index ] ->
          let access =
            if rewrite_access then
              {
                memory_effect.access with
                index = Pv_decomp.subscripts ~globals ~frame index;
              }
            else memory_effect.access
          in
          {
            memory_effect with
            access;
            source_conditions =
              Exp.dedup_conditions (bound :: memory_effect.source_conditions);
          }
      | _ -> memory_effect)
    effects

let rewrite ~(enabled : bool) ~(rewrite_access : bool) ~(check_vacuity : bool)
    ~(algo : Delinearize.Algo.t) ~(weak_in_range : bool)
    ~(globals : Variable.Set.t) (effects : SS.ordinary_memory_effect list) :
    (SS.ordinary_memory_effect list, error) result =
  if not enabled then Ok effects
  else if check_vacuity then Error Unsupported_vacuity_check
  else
    match algo with
    | Delinearize.Algo.Weak ->
        Ok
          (rewrite_weak ~rewrite_access ~in_range:weak_in_range ~globals effects)
    | _ ->
        let module A = (val Delinearize.Algo.to_module algo) in
        Ok (rewrite_with_algorithm (module A) ~rewrite_access ~globals effects)
