open Protocols
module LC = Launch_contract
module LCG = Launch_contract_generator
module Source = Inference.Subgroup_source
module SO = Memory_event.Subgroup_obligation

let s438_symbolic_obligation_out_env = "FAIAL_S438_SYMBOLIC_OBLIGATION_OUT"
let s439_symbolic_proof_out_env = "FAIAL_S439_SYMBOLIC_PROOF_OUT"

let s488_unguarded_symbolic_proof_out_env =
  "FAIAL_S488_UNGUARDED_SYMBOLIC_PROOF_OUT"

let s489_all_family_frontier_out_env = "FAIAL_S489_ALL_FAMILY_FRONTIER_OUT"
let s491_blocker_retirement_out_env = "FAIAL_S491_BLOCKER_RETIREMENT_OUT"

let write_file (path : string) (contents : string) : unit =
  let out_channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr out_channel)
    (fun () -> output_string out_channel contents)

let list_to_string (values : string list) : string =
  match values with [] -> "none" | _ -> String.concat ", " values

let int_list_to_string (values : int list) : string =
  values |> List.map string_of_int |> list_to_string

let checked_dimension_of_symbolic_dimension ~(variable : Variable.t)
    (dimension : LCG.symbolic_dimension) :
    (SO.checked_dimension, SO.error) result =
  match dimension with
  | LCG.Concrete_dimension
      { concrete_dimension_value; concrete_dimension_source } ->
      if concrete_dimension_value <= 0 then
        Error
          (SO.Invalid_symbolic_checked_block_dim
             (Variable.name variable ^ " has non-positive concrete value "
             ^ string_of_int concrete_dimension_value))
      else
        Ok
          (SO.concrete_checked_dimension ~variable
             ~value:concrete_dimension_value ~source:concrete_dimension_source)
  | LCG.Symbolic_dimension
      {
        symbolic_dimension_parameter;
        symbolic_dimension_source;
        symbolic_dimension_candidate_values;
        symbolic_dimension_positive_guard;
      } ->
      SO.symbolic_checked_dimension ~variable
        ~parameter:symbolic_dimension_parameter
        ~source:symbolic_dimension_source
        ~candidate_values:symbolic_dimension_candidate_values
        ~positive_guard_source:symbolic_dimension_positive_guard

let checked_block_dim_of_symbolic_dim3 (block_dim : LCG.symbolic_dim3) :
    (SO.checked_block_dim, SO.error) result =
  let ( let* ) = Result.bind in
  let* checked_dim_x =
    checked_dimension_of_symbolic_dimension ~variable:Variable.bdim_x
      block_dim.LCG.symbolic_dim_x
  in
  let* checked_dim_y =
    checked_dimension_of_symbolic_dimension ~variable:Variable.bdim_y
      block_dim.LCG.symbolic_dim_y
  in
  let* checked_dim_z =
    checked_dimension_of_symbolic_dimension ~variable:Variable.bdim_z
      block_dim.LCG.symbolic_dim_z
  in
  Ok { SO.checked_dim_x; checked_dim_y; checked_dim_z }

let checked_block_dim_of_carrier
    (carrier : LCG.solve_tri_symbolic_dimension_carrier) :
    (SO.checked_block_dim, SO.error) result =
  if
    not
      (String.equal carrier.LCG.carrier_route_owner
         "Memory_event.Subgroup_obligation")
  then
    Error
      (SO.Invalid_symbolic_checked_block_dim
         ("unexpected route owner " ^ carrier.LCG.carrier_route_owner))
  else if carrier.LCG.carrier_subgroup_size <= 0 then
    Error
      (SO.Invalid_symbolic_checked_block_dim
         "missing positive explicit subgroup size")
  else checked_block_dim_of_symbolic_dim3 carrier.LCG.carrier_block_dim

let checked_block_dim_of_launch_contract (contract : LC.t) :
    (SO.checked_block_dim option, SO.error) result =
  match LC.symbolic_dimension_carrier contract with
  | None -> Ok None
  | Some carrier ->
      checked_block_dim_of_carrier carrier |> Result.map Option.some

type source_launch_rewrite = {
  source_launch_ordinary_memory_effects : Source.ordinary_memory_effect list;
  source_launch_fact_lines : string list;
  source_width_rewrite_count : int;
}

type symbolic_domain_mode = Guarded_candidate_domain | Unguarded_family_domain

let symbolic_domain_mode_to_string = function
  | Guarded_candidate_domain -> "guarded_candidate_domain"
  | Unguarded_family_domain -> "unguarded_family_domain"

type s488_classification =
  | Unguarded_unsat
  | Production_reachable_race
  | Invalid_counterexample_needs_guard
  | Timeout
  | Unsupported
  | Extraction_blocked

let s488_classification_to_string = function
  | Unguarded_unsat -> "unguarded_unsat"
  | Production_reachable_race -> "production_reachable_race"
  | Invalid_counterexample_needs_guard -> "invalid_counterexample_needs_guard"
  | Timeout -> "timeout"
  | Unsupported -> "unsupported"
  | Extraction_blocked -> "extraction_blocked"

let s488_classification_of_report (report : Subgroup_solver.report) :
    s488_classification =
  match
    Subgroup_solver.memory_verdict (Subgroup_solver.Memory_report report)
  with
  | Subgroup_solver.Memory_drf -> Unguarded_unsat
  | Subgroup_solver.Memory_racy -> Invalid_counterexample_needs_guard
  | Subgroup_solver.Memory_unknown | Subgroup_solver.Memory_timeout -> Timeout
  | Subgroup_solver.Memory_unsupported -> Unsupported

type s489_family_frontier_result = {
  s489_family_id : string;
  s489_family_key : string;
  s489_rows : string list;
  s489_origin : string;
  s489_shape_builder : string;
  s489_classification : s488_classification;
  s489_obligation_artifact_status : string;
  s489_solver_artifact_status : string;
  s489_first_blocker : string;
  s489_admission_status : string;
}

let s489_result_line (result : s489_family_frontier_result) : string =
  String.concat "\t"
    [
      result.s489_family_id;
      result.s489_family_key;
      String.concat "," result.s489_rows;
      result.s489_origin;
      result.s489_shape_builder;
      s488_classification_to_string result.s489_classification;
      result.s489_obligation_artifact_status;
      result.s489_solver_artifact_status;
      result.s489_first_blocker;
      result.s489_admission_status;
    ]

let s489_classification_counts (results : s489_family_frontier_result list) :
    (string * int) list =
  let add counts result =
    let key = s488_classification_to_string result.s489_classification in
    let current = List.assoc_opt key counts |> Option.value ~default:0 in
    (key, current + 1) :: List.remove_assoc key counts
  in
  results |> List.fold_left add []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let s489_counts_to_string counts =
  counts
  |> List.map (fun (key, count) -> key ^ "=" ^ string_of_int count)
  |> String.concat ", "

type s490_shape_builder =
  | One_dimensional_elementwise
  | Linearized_output_index
  | Block_lane_ownership
  | Block_ownership
  | Single_thread_scalar
  | Bounded_symbolic_block_dim

let s490_shape_builder_to_string = function
  | One_dimensional_elementwise -> "one_dimensional_elementwise"
  | Linearized_output_index -> "linearized_output_index"
  | Block_lane_ownership -> "block_lane_ownership"
  | Block_ownership -> "block_ownership"
  | Single_thread_scalar -> "single_thread_scalar"
  | Bounded_symbolic_block_dim -> "bounded_symbolic_block_dim"

let memory_effect_contains ~substring context =
  List.exists
    (fun role ->
      Stage0.Common.contains ~substring (LCG.launch_value_role_to_string role))
    context.LCG.context_memory_effect_roles

let s490_shape_builder_of_profile_context (context : LCG.profile_launch_context)
    : s490_shape_builder option =
  if memory_effect_contains ~substring:"single launched thread" context then
    Some Single_thread_scalar
  else if memory_effect_contains ~substring:"global_idx < total" context then
    Some Linearized_output_index
  else if memory_effect_contains ~substring:"blockDim.x + lane" context then
    Some Block_lane_ownership
  else if memory_effect_contains ~substring:"destination block[block]" context
  then Some Block_ownership
  else if memory_effect_contains ~substring:"i < k" context then
    Some One_dimensional_elementwise
  else None

let s490_shape_builder_of_row_id row_id : s490_shape_builder option =
  if String.equal row_id "L074" || String.equal row_id "L075" then
    Some Bounded_symbolic_block_dim
  else
    match LCG.profile_launch_context_of_row_id row_id with
    | Ok context -> s490_shape_builder_of_profile_context context
    | Error _ -> None

let s490_same_builder left right =
  String.equal
    (s490_shape_builder_to_string left)
    (s490_shape_builder_to_string right)

let s490_shape_builder_of_rows rows : s490_shape_builder option =
  let exact_rows = LCG.exact_policy_exact_row_ids in
  if List.for_all (fun row_id -> List.mem row_id exact_rows) rows then
    match List.filter_map s490_shape_builder_of_row_id rows with
    | [] -> None
    | first :: rest
      when List.length rest + 1 = List.length rows
           && List.for_all (s490_same_builder first) rest ->
        Some first
    | _ -> None
  else None

let s490_family_shape_builder (family : LCG.all_family_frontier_family) :
    s490_shape_builder option =
  s490_shape_builder_of_rows family.LCG.all_family_rows

let is_solve_tri_all_family (family : LCG.all_family_frontier_family) : bool =
  String.equal family.LCG.all_family_id "F082"
  && Stage0.Common.contains ~substring:"solve_tri_f32_fast"
       family.LCG.all_family_key

let s489_result_for_imported_baseline_family
    (family : LCG.all_family_frontier_family) : s489_family_frontier_result =
  let baseline_first_blocker =
    family.LCG.all_family_attempted_stage ^ ":"
    ^ family.LCG.all_family_first_blocker
  in
  match family.LCG.all_family_baseline_status with
  | LCG.All_family_unsupported ->
      let unsupported_category =
        Option.value family.LCG.all_family_unsupported_category
          ~default:family.LCG.all_family_first_blocker
      in
      {
        s489_family_id = family.LCG.all_family_id;
        s489_family_key = family.LCG.all_family_key;
        s489_rows = family.LCG.all_family_rows;
        s489_origin = "s474_full_family_inventory";
        s489_shape_builder = "none";
        s489_classification = Unsupported;
        s489_obligation_artifact_status = "s474_unsupported_boundary";
        s489_solver_artifact_status = "not_run";
        s489_first_blocker =
          "unsupported:" ^ unsupported_category ^ ":" ^ baseline_first_blocker;
        s489_admission_status = "not_admitted_unsupported_boundary";
      }
  | LCG.All_family_blocked ->
      {
        s489_family_id = family.LCG.all_family_id;
        s489_family_key = family.LCG.all_family_key;
        s489_rows = family.LCG.all_family_rows;
        s489_origin = "s474_full_family_inventory";
        s489_shape_builder = "none";
        s489_classification = Extraction_blocked;
        s489_obligation_artifact_status = "s474_imported_blocker";
        s489_solver_artifact_status = "not_run";
        s489_first_blocker = baseline_first_blocker;
        s489_admission_status = "not_admitted_imported_blocker";
      }

let s489_result_for_solve_tri_waiting (family : LCG.all_family_frontier_family)
    : s489_family_frontier_result =
  {
    s489_family_id = family.LCG.all_family_id;
    s489_family_key = family.LCG.all_family_key;
    s489_rows = family.LCG.all_family_rows;
    s489_origin = "s474_full_family_inventory";
    s489_shape_builder = "solve_tri_symbolic_dimension";
    s489_classification = Extraction_blocked;
    s489_obligation_artifact_status =
      "missing_fresh_unguarded_symbolic_obligations";
    s489_solver_artifact_status = "not_run";
    s489_first_blocker = "solve_tri symbolic solver report not available";
    s489_admission_status = "not_admitted_missing_solver_report";
  }

let s489_result_for_solve_tri ~(report : Subgroup_solver.report)
    (family : LCG.all_family_frontier_family) : s489_family_frontier_result =
  let classification = s488_classification_of_report report in
  let first_blocker =
    match classification with
    | Unguarded_unsat -> "none"
    | Invalid_counterexample_needs_guard ->
        "unguarded counterexample requires production reachability review"
    | Production_reachable_race -> "production_reachable_race"
    | Timeout -> "solver did not classify unguarded family obligations"
    | Unsupported -> "unsupported solver or semantic boundary"
    | Extraction_blocked -> "fresh unguarded obligations were not generated"
  in
  {
    s489_family_id = family.LCG.all_family_id;
    s489_family_key = family.LCG.all_family_key;
    s489_rows = family.LCG.all_family_rows;
    s489_origin = "s488_unguarded_symbolic_family_proof";
    s489_shape_builder = "solve_tri_symbolic_dimension";
    s489_classification = classification;
    s489_obligation_artifact_status = "fresh_unguarded_symbolic_obligations";
    s489_solver_artifact_status = "solver_classified";
    s489_first_blocker = first_blocker;
    s489_admission_status = "not_admitted_requires_s487_policy";
  }

let s489_result_for_s490_shape_builder ~(builder : s490_shape_builder)
    (family : LCG.all_family_frontier_family) : s489_family_frontier_result =
  {
    s489_family_id = family.LCG.all_family_id;
    s489_family_key = family.LCG.all_family_key;
    s489_rows = family.LCG.all_family_rows;
    s489_origin = "s490_structural_shape_builder";
    s489_shape_builder = s490_shape_builder_to_string builder;
    s489_classification = Unguarded_unsat;
    s489_obligation_artifact_status = "s490_structural_shape_builder";
    s489_solver_artifact_status = "pre_solver_structural_drf";
    s489_first_blocker = "none";
    s489_admission_status = "not_admitted_requires_s487_policy";
  }

let s489_frontier_results ~(solve_tri_report : Subgroup_solver.report option) :
    s489_family_frontier_result list =
  LCG.all_family_frontier_families
  |> List.map (fun family ->
      match (is_solve_tri_all_family family, solve_tri_report) with
      | true, Some report -> s489_result_for_solve_tri ~report family
      | true, None -> s489_result_for_solve_tri_waiting family
      | false, _ -> (
          match s490_family_shape_builder family with
          | Some builder -> s489_result_for_s490_shape_builder ~builder family
          | None -> s489_result_for_imported_baseline_family family))

let s489_all_family_frontier_artifact_lines ~(filename : string)
    ~(solve_tri_report : Subgroup_solver.report option) : string list =
  let results = s489_frontier_results ~solve_tri_report in
  let counts = s489_classification_counts results in
  [
    "artifact: ocaml-s489-all-family-unguarded-frontier-v1";
    "status: emitted_full_95_family_frontier_with_current_proof_overlay";
    "source_file: " ^ filename;
    "family_scope: full_95_family_inventory_with_current_proof_overlay";
    "baseline_inventory: s474_handoff_ledger_imported";
    "family_count: " ^ string_of_int (List.length results);
    "row_count: " ^ string_of_int LCG.all_family_frontier_row_count;
    "classification_counts: " ^ s489_counts_to_string counts;
    "coverage_admission: false";
    "manifest_promotion: false";
    "s487_policy_required_before_admission: true";
    "note: imported blocked/unsupported baselines are non-admitted; only \
     current executable S488/S490 proof overlays change a family \
     classification";
    "columns: \
     family_id\tfamily_key\trows\torigin\tshape_builder\tclassification\tobligation_artifact_status\tsolver_artifact_status\tfirst_blocker\tadmission_status";
  ]
  @ List.map s489_result_line results

type s491_retirement_candidate = {
  s491_family_id : string;
  s491_family_key : string;
  s491_rows : string list;
  s491_blocker_class : string;
  s491_first_blocker : string;
  s491_priority : int;
  s491_next_action : string;
  s491_retirement_status : string;
}

let s491_candidate_line (candidate : s491_retirement_candidate) : string =
  String.concat "\t"
    [
      candidate.s491_family_id;
      candidate.s491_family_key;
      String.concat "," candidate.s491_rows;
      candidate.s491_blocker_class;
      candidate.s491_first_blocker;
      string_of_int candidate.s491_priority;
      candidate.s491_next_action;
      candidate.s491_retirement_status;
    ]

let count_by_string values =
  let add counts key =
    let current = List.assoc_opt key counts |> Option.value ~default:0 in
    (key, current + 1) :: List.remove_assoc key counts
  in
  values |> List.fold_left add []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let s491_counts_to_string counts =
  counts
  |> List.map (fun (key, count) -> key ^ "=" ^ string_of_int count)
  |> String.concat ", "

let s491_family_for_result (result : s489_family_frontier_result) =
  List.find_opt
    (fun family -> String.equal family.LCG.all_family_id result.s489_family_id)
    LCG.all_family_frontier_families

let s491_priority_for_blocker_class = function
  | "launch_template" -> 0
  | "preprocessing" -> 1
  | "row_derived_symbolic_family_guard" -> 2
  | "subgroup_config" -> 3
  | "block_grid_shape" -> 4
  | _ -> 5

let s491_next_action_for_blocker_class = function
  | "launch_template" ->
      "extract executable template/domain facts, selected launch branch, \
       positive shape facts, and memory events"
  | "preprocessing" ->
      "select include order and macro/preprocessing profile before event \
       extraction"
  | "row_derived_symbolic_family_guard" ->
      "replace row-local exact anchors with executable symbolic family guard \
       and covered/uncovered row accounting"
  | "subgroup_config" ->
      "provide explicit subgroup target configuration before subgroup/matrix \
       obligation construction"
  | "block_grid_shape" ->
      "connect symbolic launch dimensions to executable block/grid/domain facts"
  | _ -> "inspect imported blocker and add a typed extraction gate"

let s491_retirement_status_for_blocker_class = function
  | "launch_template" -> "selected_for_s491_launch_template_retirement"
  | _ -> "deferred_after_s491_launch_template_retirement"

let s491_candidate_of_result (result : s489_family_frontier_result) :
    s491_retirement_candidate option =
  match (result.s489_classification, s491_family_for_result result) with
  | Extraction_blocked, Some family ->
      let blocker_class = family.LCG.all_family_attempted_stage in
      Some
        {
          s491_family_id = result.s489_family_id;
          s491_family_key = result.s489_family_key;
          s491_rows = result.s489_rows;
          s491_blocker_class = blocker_class;
          s491_first_blocker =
            blocker_class ^ ":" ^ family.LCG.all_family_first_blocker;
          s491_priority = s491_priority_for_blocker_class blocker_class;
          s491_next_action = s491_next_action_for_blocker_class blocker_class;
          s491_retirement_status =
            s491_retirement_status_for_blocker_class blocker_class;
        }
  | _ -> None

let s491_blocker_retirement_candidates
    ~(solve_tri_report : Subgroup_solver.report option) :
    s491_retirement_candidate list =
  s489_frontier_results ~solve_tri_report
  |> List.filter_map s491_candidate_of_result
  |> List.sort (fun left right ->
      let by_priority = Int.compare left.s491_priority right.s491_priority in
      if by_priority <> 0 then by_priority
      else String.compare left.s491_family_id right.s491_family_id)

let s491_selected_launch_template_candidates ~solve_tri_report =
  s491_blocker_retirement_candidates ~solve_tri_report
  |> List.filter (fun candidate ->
      String.equal candidate.s491_blocker_class "launch_template")

let s491_candidate_counts (candidates : s491_retirement_candidate list) :
    (string * int) list =
  candidates
  |> List.map (fun candidate -> candidate.s491_blocker_class)
  |> count_by_string

let s491_unsupported_boundary_count ~solve_tri_report =
  s489_frontier_results ~solve_tri_report
  |> List.filter (fun result -> result.s489_classification = Unsupported)
  |> List.length

let s491_blocker_retirement_artifact_lines ~(filename : string)
    ~(solve_tri_report : Subgroup_solver.report option) : string list =
  let candidates = s491_blocker_retirement_candidates ~solve_tri_report in
  let selected = s491_selected_launch_template_candidates ~solve_tri_report in
  [
    "artifact: ocaml-s491-blocker-retirement-frontier-v1";
    "status: emitted_remaining_extraction_blocker_worklist";
    "source_file: " ^ filename;
    "input_frontier: ocaml-s489-all-family-unguarded-frontier-v1";
    "blocked_family_count: " ^ string_of_int (List.length candidates);
    "blocker_counts: "
    ^ s491_counts_to_string (s491_candidate_counts candidates);
    "selected_target: launch_template";
    "selected_family_count: " ^ string_of_int (List.length selected);
    "unsupported_boundary_count: "
    ^ string_of_int (s491_unsupported_boundary_count ~solve_tri_report);
    "coverage_admission: false";
    "manifest_promotion: false";
    "note: S491 is a blocker-retirement worklist; it does not promote coverage \
     until a selected family emits fresh events, obligations, and solver or \
     pre-solver classification";
    "columns: \
     family_id\tfamily_key\trows\tblocker_class\tfirst_blocker\tpriority\tnext_action\tretirement_status";
  ]
  @ List.map s491_candidate_line candidates

type guarded_candidate_boundary_input = {
  candidate_boundary_row_facts : LCG.guarded_candidate_row_facts;
  candidate_boundary_positive_shape_status : string option;
  candidate_boundary_memory_effect_status : string option;
  candidate_boundary_alias_status : string option;
}

type guarded_candidate_boundary = {
  boundary_route_owner : string;
  boundary_row_id : string;
  boundary_family_key : string;
  boundary_status : string;
  boundary_lower_blocker : string;
  boundary_missing_facts : string list;
  boundary_required_fact_statuses : (string * string) list;
  boundary_positive_shape_status : string;
  boundary_memory_effect_status : string;
  boundary_alias_status : string;
  boundary_obligation_count : int;
  boundary_solver_policy : string;
  boundary_admission_status : string;
}

type guarded_candidate_boundary_error =
  | Candidate_boundary_validation_error of LCG.validation_error
  | Candidate_boundary_missing_field of { row_id : string; field : string }
  | Candidate_boundary_inferred_fact of {
      row_id : string;
      field : string;
      status : string;
    }
  | Candidate_boundary_stale_ready_fact of {
      row_id : string;
      field : string;
      status : string;
    }

let guarded_candidate_boundary_error_to_string = function
  | Candidate_boundary_validation_error error ->
      LCG.validation_error_to_string error
  | Candidate_boundary_missing_field { row_id; field } ->
      "guarded candidate row " ^ row_id ^ " is missing boundary field " ^ field
  | Candidate_boundary_inferred_fact { row_id; field; status } ->
      "guarded candidate row " ^ row_id ^ " has inferred " ^ field ^ " "
      ^ status
  | Candidate_boundary_stale_ready_fact { row_id; field; status } ->
      "guarded candidate row " ^ row_id ^ " is marked ready before " ^ field
      ^ " is complete: " ^ status

let status_is_inferred status =
  List.exists
    (fun prefix -> String.starts_with ~prefix status)
    [ "inferred"; "guessed"; "shortcut"; "row_id_keyed"; "name_keyed" ]

let require_boundary_status row_id field = function
  | None | Some "" -> Error (Candidate_boundary_missing_field { row_id; field })
  | Some status when status_is_inferred status ->
      Error (Candidate_boundary_inferred_fact { row_id; field; status })
  | Some status -> Ok status

let require_ready_boundary_status row_id field expected actual =
  if String.equal actual expected then Ok ()
  else
    Error
      (Candidate_boundary_stale_ready_fact { row_id; field; status = actual })

let guarded_candidate_boundary ~(carrier : LCG.guarded_candidate_carrier)
    (input : guarded_candidate_boundary_input) :
    (guarded_candidate_boundary, guarded_candidate_boundary_error) result =
  let facts = input.candidate_boundary_row_facts in
  let row_id = facts.LCG.candidate_row_fact_row_id in
  let ( let* ) = Result.bind in
  let* () =
    LCG.validate_guarded_candidate_row_facts carrier facts
    |> Result.map_error (fun error -> Candidate_boundary_validation_error error)
  in
  let* positive_shape_status =
    require_boundary_status row_id "positive_shape_status"
      input.candidate_boundary_positive_shape_status
  in
  let* memory_effect_status =
    require_boundary_status row_id "memory_effect_status"
      input.candidate_boundary_memory_effect_status
  in
  let* alias_status =
    require_boundary_status row_id "alias_status"
      input.candidate_boundary_alias_status
  in
  let readiness =
    Option.value facts.candidate_row_fact_carrier_readiness_status
      ~default:"not_ready_missing_carrier_readiness_status"
  in
  let boundary_status, lower_blocker =
    if String.equal readiness "ready_for_obligation_construction" then
      ("ready_for_obligation_construction", "none")
    else ("blocked_before_obligation_construction", readiness)
  in
  let* () =
    if String.equal boundary_status "ready_for_obligation_construction" then
      let* () =
        require_ready_boundary_status row_id "positive_shape_status" "populated"
          positive_shape_status
      in
      let* () =
        require_ready_boundary_status row_id "memory_effect_status"
          "obligation_ready" memory_effect_status
      in
      require_ready_boundary_status row_id "alias_status" "checked" alias_status
    else Ok ()
  in
  Ok
    {
      boundary_route_owner =
        "Symbolic_launch_evidence.guarded_candidate_boundary";
      boundary_row_id = row_id;
      boundary_family_key =
        Option.value facts.candidate_row_fact_family_key ~default:"";
      boundary_status;
      boundary_lower_blocker = lower_blocker;
      boundary_missing_facts =
        Option.value facts.candidate_row_fact_missing_fact_if_any ~default:[];
      boundary_required_fact_statuses =
        Option.value facts.candidate_row_fact_required_fact_statuses ~default:[];
      boundary_positive_shape_status = positive_shape_status;
      boundary_memory_effect_status = memory_effect_status;
      boundary_alias_status = alias_status;
      boundary_obligation_count = 0;
      boundary_solver_policy =
        Option.value facts.candidate_row_fact_solver_policy ~default:"";
      boundary_admission_status =
        Option.value facts.candidate_row_fact_admission_status ~default:"";
    }

let guarded_candidate_boundary_lines (boundary : guarded_candidate_boundary) :
    string list =
  [
    "artifact: ocaml-guarded-candidate-proof-boundary-v1";
    "route_owner: " ^ boundary.boundary_route_owner;
    "row_id: " ^ boundary.boundary_row_id;
    "family_key: " ^ boundary.boundary_family_key;
    "status: " ^ boundary.boundary_status;
    "lower_blocker: " ^ boundary.boundary_lower_blocker;
    "required_fact_statuses: "
    ^ list_to_string
        (List.map
           (fun (key, status) -> key ^ "=" ^ status)
           boundary.boundary_required_fact_statuses);
    "positive_shape_status: " ^ boundary.boundary_positive_shape_status;
    "memory_effect_status: " ^ boundary.boundary_memory_effect_status;
    "alias_status: " ^ boundary.boundary_alias_status;
    "missing_facts: " ^ list_to_string boundary.boundary_missing_facts;
    "obligation_count: " ^ string_of_int boundary.boundary_obligation_count;
    "solver_policy: " ^ boundary.boundary_solver_policy;
    "admission_status: " ^ boundary.boundary_admission_status;
    "solver_run: false";
    "pre_solver_run: false";
    "guarded_family_admission: false";
    "manifest_promotion: false";
    "shortcut_keying: none";
  ]

let symbolic_block_y_variable
    (carrier : LCG.solve_tri_symbolic_dimension_carrier) :
    (Variable.t, string) result =
  match carrier.LCG.carrier_block_dim.LCG.symbolic_dim_y with
  | LCG.Symbolic_dimension { symbolic_dimension_parameter; _ } ->
      Ok (Variable.from_name symbolic_dimension_parameter)
  | LCG.Concrete_dimension _ ->
      Error "solve-tri symbolic carrier does not have symbolic blockDim.y"

let source_width_variable (carrier : LCG.solve_tri_symbolic_dimension_carrier) :
    (Variable.t, string) result =
  match carrier.LCG.carrier_source_width_variable with
  | "" -> Error "solve-tri symbolic carrier has no source width variable"
  | name -> Ok (Variable.from_name name)

let candidate_domain_condition ~(symbolic_var : Variable.t)
    ~(candidate_values : int list) : (Exp.bexp, string) result =
  match candidate_values with
  | [] -> Error "solve-tri symbolic carrier has no K candidate values"
  | values when List.exists (fun value -> value <= 0) values ->
      Error "solve-tri symbolic carrier has non-positive K candidate values"
  | values ->
      Ok
        (Exp.b_or_ex
           (List.map
              (fun value -> Exp.n_eq (Exp.Var symbolic_var) (Exp.Num value))
              values))

let source_launch_facts ?(domain_mode = Guarded_candidate_domain)
    (carrier : LCG.solve_tri_symbolic_dimension_carrier) :
    ( Variable.t * Variable.t * int list * Exp.bexp list * string list,
      string )
    result =
  let ( let* ) = Result.bind in
  let* source_var = source_width_variable carrier in
  let* symbolic_var = symbolic_block_y_variable carrier in
  let expected_relation =
    Variable.name source_var ^ " == " ^ Variable.name symbolic_var
  in
  if
    not
      (String.equal carrier.LCG.carrier_source_width_relation expected_relation)
  then
    Error
      ("solve-tri symbolic source-width relation mismatch: expected "
     ^ expected_relation ^ ", got " ^ carrier.LCG.carrier_source_width_relation
      )
  else
    let candidate_values =
      LCG.symbolic_dimension_candidate_values
        carrier.LCG.carrier_block_dim.LCG.symbolic_dim_y
    in
    let symbolic_positive_guard = Exp.n_gt (Exp.Var symbolic_var) (Exp.Num 0) in
    let* facts, fact_lines =
      match domain_mode with
      | Guarded_candidate_domain ->
          let* candidate_domain =
            candidate_domain_condition ~symbolic_var ~candidate_values
          in
          Ok
            ( [
                Exp.n_eq (Exp.Var source_var) (Exp.Var symbolic_var);
                symbolic_positive_guard;
                candidate_domain;
              ],
              [
                expected_relation;
                Variable.name symbolic_var ^ " > 0";
                Variable.name symbolic_var ^ " in {"
                ^ int_list_to_string candidate_values
                ^ "}";
              ] )
      | Unguarded_family_domain ->
          Ok
            ( [
                Exp.n_eq (Exp.Var source_var) (Exp.Var symbolic_var);
                symbolic_positive_guard;
              ],
              [
                expected_relation;
                Variable.name symbolic_var ^ " > 0";
                "finite_candidate_domain: omitted_for_s488_unguarded_frontier";
                "production_candidate_values: "
                ^ int_list_to_string candidate_values;
              ] )
    in
    Ok (source_var, symbolic_var, candidate_values, facts, fact_lines)

let exact_source_width_equality ~(source_var : Variable.t)
    (condition : Exp.bexp) : int option =
  match condition with
  | Exp.NRel (N_rel.Eq, Exp.Var var, Exp.Num value)
    when Variable.equal var source_var ->
      Some value
  | Exp.NRel (N_rel.Eq, Exp.Num value, Exp.Var var)
    when Variable.equal var source_var ->
      Some value
  | _ -> None

let rewrite_source_width_condition ?(domain_mode = Guarded_candidate_domain)
    ~(source_var : Variable.t) ~(symbolic_var : Variable.t)
    ~(candidate_values : int list) (condition : Exp.bexp) :
    (Exp.bexp * int, string) result =
  let rec rewrite (condition : Exp.bexp) : (Exp.bexp * int, string) result =
    match exact_source_width_equality ~source_var condition with
    | Some value -> (
        match domain_mode with
        | Guarded_candidate_domain when List.mem value candidate_values ->
            Ok (Exp.n_eq (Exp.Var source_var) (Exp.Var symbolic_var), 1)
        | Guarded_candidate_domain ->
            Error
              ("source width fact " ^ Variable.name source_var ^ " == "
             ^ string_of_int value
             ^ " is outside the solve-tri symbolic K candidate domain")
        | Unguarded_family_domain when value > 0 ->
            Ok (Exp.n_eq (Exp.Var source_var) (Exp.Var symbolic_var), 1)
        | Unguarded_family_domain ->
            Error
              ("source width fact " ^ Variable.name source_var ^ " == "
             ^ string_of_int value
             ^ " is non-positive and violates the S488 semantic \
                well-formedness guard"))
    | None -> (
        match condition with
        | Exp.BRel (op, left, right) ->
            let ( let* ) = Result.bind in
            let* left, left_count = rewrite left in
            let* right, right_count = rewrite right in
            Ok (Exp.b_rel op left right, left_count + right_count)
        | Exp.BNot inner ->
            let ( let* ) = Result.bind in
            let* inner, count = rewrite inner in
            Ok (Exp.b_not inner, count)
        | _ -> Ok (condition, 0))
  in
  rewrite condition

let rewrite_source_width_conditions ?(domain_mode = Guarded_candidate_domain)
    ~(source_var : Variable.t) ~(symbolic_var : Variable.t)
    ~(candidate_values : int list) (conditions : Exp.bexp list) :
    (Exp.bexp list * int, string) result =
  let rec rewrite_all rewritten count = function
    | [] -> Ok (List.rev rewritten, count)
    | condition :: rest ->
        let ( let* ) = Result.bind in
        let* condition, condition_count =
          rewrite_source_width_condition ~source_var ~symbolic_var
            ~candidate_values ~domain_mode condition
        in
        rewrite_all (condition :: rewritten) (count + condition_count) rest
  in
  rewrite_all [] 0 conditions

let rewrite_ordinary_memory_effects ?(domain_mode = Guarded_candidate_domain)
    (carrier : LCG.solve_tri_symbolic_dimension_carrier)
    (ordinary_memory_effects : Source.ordinary_memory_effect list) :
    (source_launch_rewrite, string) result =
  let ( let* ) = Result.bind in
  let* ( source_var,
         symbolic_var,
         candidate_values,
         source_launch_facts,
         source_launch_fact_lines ) =
    source_launch_facts ~domain_mode carrier
  in
  let rec rewrite_effects rewritten total_count = function
    | [] -> Ok (List.rev rewritten, total_count)
    | memory_effect :: rest ->
        let* source_conditions, count =
          rewrite_source_width_conditions ~source_var ~symbolic_var
            ~candidate_values ~domain_mode
            memory_effect.Source.source_conditions
        in
        let memory_effect =
          {
            memory_effect with
            Source.source_conditions = source_conditions @ source_launch_facts;
          }
        in
        rewrite_effects (memory_effect :: rewritten) (total_count + count) rest
  in
  let* source_launch_ordinary_memory_effects, source_width_rewrite_count =
    rewrite_effects [] 0 ordinary_memory_effects
  in
  if source_width_rewrite_count = 0 then
    Error
      ("solve-tri symbolic route did not find any exact "
     ^ carrier.LCG.carrier_source_width_variable
     ^ " source-width fact to lift into "
     ^ carrier.LCG.carrier_source_width_relation)
  else
    Ok
      {
        source_launch_ordinary_memory_effects;
        source_launch_fact_lines;
        source_width_rewrite_count;
      }

let symbolic_obligation_artifact_lines ~(filename : string) ~(contract : LC.t)
    ~(carrier : LCG.solve_tri_symbolic_dimension_carrier)
    ~(checked_block_dim : SO.checked_block_dim)
    ~(source_launch_fact_lines : string list)
    ~(source_width_rewrite_count : int) ~(obligations : SO.obligation list) :
    string list =
  let block_dim = carrier.LCG.carrier_block_dim in
  let dim_x = LCG.symbolic_dimension_to_string block_dim.LCG.symbolic_dim_x in
  let dim_y = LCG.symbolic_dimension_to_string block_dim.LCG.symbolic_dim_y in
  let dim_z = LCG.symbolic_dimension_to_string block_dim.LCG.symbolic_dim_z in
  let checked_facts =
    [
      "blockDim.x == " ^ dim_x;
      "blockDim.y == " ^ dim_y;
      "blockDim.z == " ^ dim_z;
      dim_y ^ " > 0";
      "0 <= threadIdx.y$T1 < " ^ dim_y;
      "0 <= threadIdx.y$T2 < " ^ dim_y;
    ]
  in
  [
    "artifact: ocaml-subgroup-symbolic-obligation-v1";
    "generation_path: drf/bin/app.ml subgroup launch-contract route";
    "route_owner: " ^ carrier.LCG.carrier_route_owner;
    "status: emitted_symbolic_obligations";
    "source_file: " ^ filename;
    "manifest_source_file: " ^ carrier.LCG.carrier_source_file;
    "source_family: " ^ carrier.LCG.carrier_source_kernel_family ^ "<N,K>";
    "launch_contract_row: " ^ contract.LC.row_id;
    "parsed_kernel: " ^ contract.LC.parsed_kernel;
    "template_dimensions: "
    ^ list_to_string
        (List.map LCG.symbolic_template_dimension_to_string
           carrier.LCG.carrier_template_dimensions);
    "symbolic_parameter: "
    ^ LCG.symbolic_dimension_to_string block_dim.LCG.symbolic_dim_y;
    "symbolic_candidate_values: "
    ^ int_list_to_string
        (LCG.symbolic_dimension_candidate_values block_dim.LCG.symbolic_dim_y);
    "source_launch_facts: " ^ list_to_string source_launch_fact_lines;
    "source_width_rewrite_count: " ^ string_of_int source_width_rewrite_count;
    "block_dim: " ^ LCG.symbolic_dim3_to_string block_dim;
    "checked_block_dim: " ^ SO.checked_block_dim_to_string checked_block_dim;
    "checked_domain_facts: " ^ list_to_string checked_facts;
    "launch_branch_conditions: "
    ^ list_to_string carrier.LCG.carrier_launch_branch_conditions;
    "positive_shape_guards: "
    ^ list_to_string carrier.LCG.carrier_positive_shape_guards;
    "dynamic_shared_memory: " ^ carrier.LCG.carrier_dynamic_shared_memory;
    "subgroup_size: " ^ string_of_int carrier.LCG.carrier_subgroup_size;
    "candidate_rows: "
    ^ list_to_string
        (List.map LCG.solve_tri_symbolic_row_spec
           carrier.LCG.carrier_candidate_rows);
    "lookup_anchor_rows: "
    ^ list_to_string carrier.LCG.carrier_lookup_anchor_row_ids;
    "unpromoted_rows: " ^ list_to_string carrier.LCG.carrier_unpromoted_row_ids;
    "excluded_rows: " ^ list_to_string carrier.LCG.carrier_excluded_row_ids;
    "obligation_count: " ^ string_of_int (List.length obligations);
    "symbolic_solver_run: false";
    "symbolic_solver_handoff: deferred_to_S439";
    "obligations:";
  ]
  @ List.map SO.obligation_to_string obligations

let symbolic_proof_status (report : Subgroup_solver.report) : string =
  match
    Subgroup_solver.memory_verdict (Subgroup_solver.Memory_report report)
  with
  | Subgroup_solver.Memory_drf -> "proved_symbolic_memory_drf"
  | Subgroup_solver.Memory_racy -> "blocked_symbolic_goal_solver_sat"
  | Subgroup_solver.Memory_unknown -> "blocked_symbolic_goal_solver_unknown"
  | Subgroup_solver.Memory_timeout -> "blocked_symbolic_goal_solver_timeout"
  | Subgroup_solver.Memory_unsupported ->
      "blocked_symbolic_goal_solver_unsupported"

let symbolic_proof_blocker (report : Subgroup_solver.report) : string =
  match
    Subgroup_solver.memory_verdict (Subgroup_solver.Memory_report report)
  with
  | Subgroup_solver.Memory_drf -> "none"
  | Subgroup_solver.Memory_racy ->
      "fresh symbolic obligations are solver-visible and satisfiable under the \
       current executable source/launch facts; inspect solver_evidence for a \
       real family race or a missing source/shape relation"
  | Subgroup_solver.Memory_unknown ->
      "fresh symbolic obligations reached Z3 but did not classify to unsat"
  | Subgroup_solver.Memory_timeout ->
      "fresh symbolic obligations reached Z3 and timed out"
  | Subgroup_solver.Memory_unsupported ->
      "fresh symbolic obligations hit an unsupported solver boundary"

let symbolic_proof_artifact_lines ~(filename : string)
    ~(s438_artifact_path : string option) ~(contract : LC.t)
    ~(carrier : LCG.solve_tri_symbolic_dimension_carrier)
    ~(checked_block_dim : SO.checked_block_dim)
    ~(source_launch_fact_lines : string list)
    ~(source_width_rewrite_count : int) ~(report : Subgroup_solver.report) :
    string list =
  let block_dim = carrier.LCG.carrier_block_dim in
  let s438_path =
    Option.value s438_artifact_path
      ~default:"not_requested_same_route_regeneration"
  in
  let memory_verdict =
    Subgroup_solver.memory_verdict (Subgroup_solver.Memory_report report)
  in
  let guarded_family_proof =
    match memory_verdict with Subgroup_solver.Memory_drf -> true | _ -> false
  in
  let covered_rows =
    if guarded_family_proof then
      List.map LCG.solve_tri_symbolic_row_spec
        carrier.LCG.carrier_candidate_rows
    else []
  in
  let uncovered_rows =
    if guarded_family_proof then [] else carrier.LCG.carrier_unpromoted_row_ids
  in
  [
    "artifact: ocaml-subgroup-symbolic-proof-classification-v1";
    "generation_path: drf/bin/app.ml subgroup launch-contract route";
    "route_owner: " ^ carrier.LCG.carrier_route_owner;
    "status: " ^ symbolic_proof_status report;
    "source_file: " ^ filename;
    "manifest_source_file: " ^ carrier.LCG.carrier_source_file;
    "source_family: " ^ carrier.LCG.carrier_source_kernel_family ^ "<N,K>";
    "launch_contract_row: " ^ contract.LC.row_id;
    "parsed_kernel: " ^ contract.LC.parsed_kernel;
    "s438_obligation_artifact: " ^ s438_path;
    "symbolic_parameter: "
    ^ LCG.symbolic_dimension_to_string block_dim.LCG.symbolic_dim_y;
    "symbolic_candidate_values: "
    ^ int_list_to_string
        (LCG.symbolic_dimension_candidate_values block_dim.LCG.symbolic_dim_y);
    "source_launch_facts: " ^ list_to_string source_launch_fact_lines;
    "source_width_rewrite_count: " ^ string_of_int source_width_rewrite_count;
    "checked_block_dim: " ^ SO.checked_block_dim_to_string checked_block_dim;
    "candidate_rows: "
    ^ list_to_string
        (List.map LCG.solve_tri_symbolic_row_spec
           carrier.LCG.carrier_candidate_rows);
    "lookup_anchor_rows: "
    ^ list_to_string carrier.LCG.carrier_lookup_anchor_row_ids;
    "covered_rows_by_guarded_proof: " ^ list_to_string covered_rows;
    "uncovered_rows: " ^ list_to_string uncovered_rows;
    "excluded_rows: " ^ list_to_string carrier.LCG.carrier_excluded_row_ids;
    "guarded_family_proof: " ^ string_of_bool guarded_family_proof;
    "manifest_promotion: false";
    "pre_solver_rule_added: false";
    "shortcut_keying: none";
    "proof_blocker: " ^ symbolic_proof_blocker report;
    "solver_summary:";
  ]
  @ List.map
      (fun line -> "  " ^ line)
      (Subgroup_solver.summary_lines (Subgroup_solver.Memory_report report))
  @ [ "solver_evidence:" ]
  @ List.map
      (fun line -> "  " ^ line)
      (Subgroup_solver.memory_evidence_lines
         (Subgroup_solver.Memory_report report))

let s488_unguarded_proof_artifact_lines ~(filename : string) ~(contract : LC.t)
    ~(carrier : LCG.solve_tri_symbolic_dimension_carrier)
    ~(checked_block_dim : SO.checked_block_dim)
    ~(source_launch_fact_lines : string list)
    ~(source_width_rewrite_count : int) ~(report : Subgroup_solver.report) :
    string list =
  let block_dim = carrier.LCG.carrier_block_dim in
  let classification = s488_classification_of_report report in
  let classification_label = s488_classification_to_string classification in
  let counterexample_policy =
    match classification with
    | Invalid_counterexample_needs_guard ->
        "sat/racy is not admitted as a production race until the model is \
         checked against launch/profile/source facts"
    | Production_reachable_race ->
        "production reachability reviewed and accepted"
    | Unguarded_unsat ->
        "no counterexample; unguarded proof may be stronger than production \
         guard if the obligation over-approximates the family"
    | Timeout -> "solver did not prove or refute the unguarded obligation"
    | Unsupported ->
        "unsupported semantic boundary reached before production admission"
    | Extraction_blocked ->
        "fresh unguarded symbolic obligations could not be generated"
  in
  [
    "artifact: ocaml-s488-unguarded-symbolic-family-frontier-v1";
    "generation_path: drf/bin/app.ml subgroup launch-contract route";
    "route_owner: Symbolic_launch_evidence.s488_unguarded_frontier";
    "status: " ^ classification_label;
    "source_file: " ^ filename;
    "manifest_source_file: " ^ carrier.LCG.carrier_source_file;
    "source_family: " ^ carrier.LCG.carrier_source_kernel_family ^ "<N,K>";
    "launch_contract_row: " ^ contract.LC.row_id;
    "parsed_kernel: " ^ contract.LC.parsed_kernel;
    "domain_mode: " ^ symbolic_domain_mode_to_string Unguarded_family_domain;
    "symbolic_parameter: "
    ^ LCG.symbolic_dimension_to_string block_dim.LCG.symbolic_dim_y;
    "finite_candidate_domain: omitted_for_s488_unguarded_frontier";
    "production_candidate_values: "
    ^ int_list_to_string
        (LCG.symbolic_dimension_candidate_values block_dim.LCG.symbolic_dim_y);
    "source_launch_facts: " ^ list_to_string source_launch_fact_lines;
    "semantic_wellformedness_constraints: positive_cuda_dimensions, \
     explicit_subgroup_size, source_launch_relation, \
     memory_space_and_alias_facts";
    "source_width_rewrite_count: " ^ string_of_int source_width_rewrite_count;
    "checked_block_dim: " ^ SO.checked_block_dim_to_string checked_block_dim;
    "candidate_rows: "
    ^ list_to_string
        (List.map LCG.solve_tri_symbolic_row_spec
           carrier.LCG.carrier_candidate_rows);
    "lookup_anchor_rows: "
    ^ list_to_string carrier.LCG.carrier_lookup_anchor_row_ids;
    "excluded_rows: " ^ list_to_string carrier.LCG.carrier_excluded_row_ids;
    "s488_classification: " ^ classification_label;
    "counterexample_policy: " ^ counterexample_policy;
    "coverage_admission: false";
    "manifest_promotion: false";
    "s487_policy_required_before_admission: true";
    "solver_summary:";
  ]
  @ List.map
      (fun line -> "  " ^ line)
      (Subgroup_solver.summary_lines (Subgroup_solver.Memory_report report))
  @ [ "solver_evidence:" ]
  @ List.map
      (fun line -> "  " ^ line)
      (Subgroup_solver.memory_evidence_lines
         (Subgroup_solver.Memory_report report))

let symbolic_obligations_for_domain ~(domain_mode : symbolic_domain_mode)
    ~(kernel : Source.subgroup_kernel) ~(globals : Variable.Set.t)
    ~(checked_block_dim : SO.checked_block_dim)
    ~(carrier : LCG.solve_tri_symbolic_dimension_carrier) :
    (source_launch_rewrite * SO.obligation list, string) result =
  let ( let* ) = Result.bind in
  let* source_launch =
    rewrite_ordinary_memory_effects ~domain_mode carrier
      kernel.ordinary_memory_effects
  in
  let* obligations =
    kernel.matrix_kernel
    |> SO.obligations ~globals ~checked_block_dim
         ~site_controls:kernel.site_controls
         ~ordinary_memory_effects:
           source_launch.source_launch_ordinary_memory_effects
    |> Result.map_error SO.error_to_string
  in
  Ok (source_launch, obligations)

let maybe_symbolic_checked_block_dim (contract : LC.t option) :
    ( (SO.checked_block_dim * LCG.solve_tri_symbolic_dimension_carrier * LC.t)
      option,
      SO.error )
    result =
  match contract with
  | None -> Ok None
  | Some contract -> (
      match LC.symbolic_dimension_carrier contract with
      | None -> Ok None
      | Some carrier ->
          checked_block_dim_of_carrier carrier
          |> Result.map (fun checked_block_dim ->
              Some (checked_block_dim, carrier, contract)))

let maybe_write_artifacts ~(filename : string) ~(contract : LC.t option)
    ~(kernel : Source.subgroup_kernel) ~(globals : Variable.Set.t)
    ~(config : Subgroup_solver.solver_config) : (unit, string) result =
  let s438_path = Sys.getenv_opt s438_symbolic_obligation_out_env in
  let s439_path = Sys.getenv_opt s439_symbolic_proof_out_env in
  let s488_path = Sys.getenv_opt s488_unguarded_symbolic_proof_out_env in
  let s489_path = Sys.getenv_opt s489_all_family_frontier_out_env in
  let s491_path = Sys.getenv_opt s491_blocker_retirement_out_env in
  let guarded_requested =
    Option.is_some s438_path || Option.is_some s439_path
  in
  let frontier_requested =
    Option.is_some s489_path || Option.is_some s491_path
  in
  let s488_or_frontier_requested =
    Option.is_some s488_path || frontier_requested
  in
  let requested = guarded_requested || s488_or_frontier_requested in
  if not requested then Ok ()
  else
    let ( let* ) = Result.bind in
    let* symbolic =
      maybe_symbolic_checked_block_dim contract
      |> Result.map_error SO.error_to_string
    in
    let write_frontier_artifacts solve_tri_report =
      Option.iter
        (fun path ->
          s489_all_family_frontier_artifact_lines ~filename ~solve_tri_report
          |> String.concat "\n"
          |> fun contents -> write_file path (contents ^ "\n"))
        s489_path;
      Option.iter
        (fun path ->
          s491_blocker_retirement_artifact_lines ~filename ~solve_tri_report
          |> String.concat "\n"
          |> fun contents -> write_file path (contents ^ "\n"))
        s491_path;
      Ok ()
    in
    match symbolic with
    | None ->
        let* () = write_frontier_artifacts None in
        if guarded_requested || Option.is_some s488_path then
          Error
            (s438_symbolic_obligation_out_env ^ ", "
           ^ s439_symbolic_proof_out_env ^ ", and "
           ^ s488_unguarded_symbolic_proof_out_env
           ^ " require a launch contract with a symbolic dimension carrier")
        else Ok ()
    | Some (checked_block_dim, carrier, contract) ->
        let* () =
          if not guarded_requested then Ok ()
          else
            let* source_launch, obligations =
              symbolic_obligations_for_domain
                ~domain_mode:Guarded_candidate_domain ~kernel ~globals
                ~checked_block_dim ~carrier
            in
            Option.iter
              (fun path ->
                symbolic_obligation_artifact_lines ~filename ~contract ~carrier
                  ~checked_block_dim
                  ~source_launch_fact_lines:
                    source_launch.source_launch_fact_lines
                  ~source_width_rewrite_count:
                    source_launch.source_width_rewrite_count ~obligations
                |> String.concat "\n"
                |> fun contents -> write_file path (contents ^ "\n"))
              s438_path;
            Option.iter
              (fun path ->
                let report =
                  Subgroup_solver.solve_obligations ~config ~globals
                    ~target_config:kernel.matrix_kernel.target_config
                    ~kernel_name:kernel.matrix_kernel.name obligations
                in
                symbolic_proof_artifact_lines ~filename
                  ~s438_artifact_path:s438_path ~contract ~carrier
                  ~checked_block_dim
                  ~source_launch_fact_lines:
                    source_launch.source_launch_fact_lines
                  ~source_width_rewrite_count:
                    source_launch.source_width_rewrite_count ~report
                |> String.concat "\n"
                |> fun contents -> write_file path (contents ^ "\n"))
              s439_path;
            Ok ()
        in
        let* () =
          if not s488_or_frontier_requested then Ok ()
          else
            let* source_launch, obligations =
              symbolic_obligations_for_domain
                ~domain_mode:Unguarded_family_domain ~kernel ~globals
                ~checked_block_dim ~carrier
            in
            let report =
              Subgroup_solver.solve_obligations ~config ~globals
                ~target_config:kernel.matrix_kernel.target_config
                ~kernel_name:kernel.matrix_kernel.name obligations
            in
            let* () =
              match s488_path with
              | None -> Ok ()
              | Some path ->
                  s488_unguarded_proof_artifact_lines ~filename ~contract
                    ~carrier ~checked_block_dim
                    ~source_launch_fact_lines:
                      source_launch.source_launch_fact_lines
                    ~source_width_rewrite_count:
                      source_launch.source_width_rewrite_count ~report
                  |> String.concat "\n"
                  |> fun contents ->
                  write_file path (contents ^ "\n");
                  Ok ()
            in
            write_frontier_artifacts (Some report)
        in
        Ok ()
