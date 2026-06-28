open Protocols
module LC = Launch_contract
module LCG = Launch_contract_generator
module Source = Inference.Subgroup_source
module SO = Memory_event.Subgroup_obligation

let s438_symbolic_obligation_out_env = "FAIAL_S438_SYMBOLIC_OBLIGATION_OUT"
let s439_symbolic_proof_out_env = "FAIAL_S439_SYMBOLIC_PROOF_OUT"

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

let symbolic_obligation_artifact_lines ~(filename : string) ~(contract : LC.t)
    ~(carrier : LCG.solve_tri_symbolic_dimension_carrier)
    ~(checked_block_dim : SO.checked_block_dim)
    ~(obligations : SO.obligation list) : string list =
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
       current executable goal; the goal carries blockDim.y == K and K > 0, \
       but source memory conditions remain exact-row facts and do not yet \
       encode the guarded family relation between symbolic K and source k"
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
    ~(report : Subgroup_solver.report) : string list =
  let block_dim = carrier.LCG.carrier_block_dim in
  let s438_path =
    Option.value s438_artifact_path
      ~default:"not_requested_same_route_regeneration"
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
    "checked_block_dim: " ^ SO.checked_block_dim_to_string checked_block_dim;
    "candidate_rows: "
    ^ list_to_string
        (List.map LCG.solve_tri_symbolic_row_spec
           carrier.LCG.carrier_candidate_rows);
    "lookup_anchor_rows: "
    ^ list_to_string carrier.LCG.carrier_lookup_anchor_row_ids;
    "covered_rows_by_guarded_proof: none";
    "uncovered_rows: " ^ list_to_string carrier.LCG.carrier_unpromoted_row_ids;
    "excluded_rows: " ^ list_to_string carrier.LCG.carrier_excluded_row_ids;
    "guarded_family_proof: false";
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
  let requested = Option.is_some s438_path || Option.is_some s439_path in
  if not requested then Ok ()
  else
    let ( let* ) = Result.bind in
    let* symbolic =
      maybe_symbolic_checked_block_dim contract
      |> Result.map_error SO.error_to_string
    in
    match symbolic with
    | None ->
        Error
          (s438_symbolic_obligation_out_env ^ " and "
         ^ s439_symbolic_proof_out_env
         ^ " require a launch contract with a symbolic dimension carrier")
    | Some (checked_block_dim, carrier, contract) ->
        let* obligations =
          kernel.matrix_kernel
          |> SO.obligations ~globals ~checked_block_dim
               ~site_controls:kernel.site_controls
               ~ordinary_memory_effects:kernel.ordinary_memory_effects
          |> Result.map_error SO.error_to_string
        in
        Option.iter
          (fun path ->
            symbolic_obligation_artifact_lines ~filename ~contract ~carrier
              ~checked_block_dim ~obligations
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
              ~checked_block_dim ~report
            |> String.concat "\n"
            |> fun contents -> write_file path (contents ^ "\n"))
          s439_path;
        Ok ()
