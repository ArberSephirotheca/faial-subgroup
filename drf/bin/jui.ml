open Stage0
open Protocols
module App_analysis = Analysis
open Drf
open Solve_drf

let render_ordinary (analysis : App_analysis.ordinary) : Yojson.Basic.t =
  let kernel_name = analysis.kernel.name in
  let solutions = analysis.report in
  let unknowns, errors =
    solutions
    |> List.filter_map (fun s ->
        let open Solution in
        match s.outcome with
        | Drf | Drf_with_core _ -> None
        | Unknown -> Some (Either.Left s.proof)
        | Racy w -> Some (Either.Right (s.proof, w)))
    |> Common.either_split
  in
  let logics : Yojson.Basic.t list =
    solutions
    |> List.map (fun s ->
        let open Solution in
        Option.value ~default:"DEFAULT" s.logic)
    |> Common.StringSet.of_list |> Common.StringSet.to_list
    |> List.sort String.compare
    |> List.map (fun x -> `String x)
  in
  let approx_analysis (w : Witness.t) =
    let dd = if Variable.Set.cardinal w.data_approx > 0 then "DD" else "DI" in
    let cd =
      if Variable.Set.cardinal w.control_approx > 0 then "CD" else "CI"
    in
    cd ^ dd
  in
  `Assoc
    [
      ("kernel_name", `String kernel_name);
      ( "status",
        `String
          (App_analysis.Verdict.to_string
             (App_analysis.ordinary_verdict analysis)) );
      ("unknowns", `List (List.map Symbexp.Proof.to_json unknowns));
      ("logics", `List logics);
      ( "errors",
        `List
          (List.map
             (fun (p, w) ->
               `Assoc
                 [
                   ("summary", Symbexp.Proof.to_json p);
                   ("counter_example", Witness.to_json w);
                   ("approx_analysis", `String (approx_analysis w));
                 ])
             errors) );
    ]

let subgroup_counts_to_json (counts : Drf.Subgroup_solver.Counts.t) :
    Yojson.Basic.t =
  `Assoc
    [
      ("total", `Int counts.total);
      ("racy", `Int counts.racy);
      ("unknown", `Int counts.unknown);
      ("timeout", `Int counts.timeout);
      ("unsupported", `Int counts.unsupported);
      ("pre_solver_unsat", `Int counts.pre_solver_unsat);
    ]

let render_subgroup (analysis : App_analysis.subgroup) : Yojson.Basic.t =
  let module Solver = Drf.Subgroup_solver in
  let module Uniformity = Drf.Subgroup_uniformity in
  let memory_verdict = Solver.memory_verdict analysis.memory in
  let subgroup_verdict = Uniformity.function_verdict analysis.uniformity in
  let full_verdict = App_analysis.subgroup_full_verdict analysis in
  let full_verdict_string =
    match analysis.vacuous with
    | Some _ -> "vacuous"
    | None -> Uniformity.full_verdict_to_string full_verdict
  in
  `Assoc
    [
      ("kernel_name", `String analysis.kernel.name);
      ( "target_config",
        `String
          (Inference.Subgroup_matrix.Target_config.to_string
             analysis.kernel.target_config) );
      ("status", `String full_verdict_string);
      ("mem_drf", `String (Solver.memory_verdict_to_string memory_verdict));
      ( "subgroup_uniformity",
        `String (Uniformity.verdict_to_string subgroup_verdict) );
      ("drf_full", `String full_verdict_string);
      ( "memory_checks",
        subgroup_counts_to_json (Solver.memory_counts analysis.memory) );
      ( "evidence",
        `List
          (List.map
             (fun line -> `String line)
             (Solver.memory_evidence_lines analysis.memory)) );
      ( "subgroup_sites",
        `List
          (List.map
             (fun site -> `String (Uniformity.site_result_to_string site))
             (Uniformity.sites analysis.uniformity)) );
    ]

let render ~(rejected : Imp.Rejected_kernel.t list)
    (output : App_analysis.t list) : unit =
  let kernels =
    output
    |> List.map (function
      | App_analysis.Ordinary analysis -> render_ordinary analysis
      | App_analysis.Subgroup analysis -> render_subgroup analysis)
  in
  let rejected = List.map Imp.Rejected_kernel.to_json rejected in
  `Assoc
    [
      ("kernels", `List kernels);
      ("rejected", `List rejected);
      ("phase_times", Phase_timer.to_json ());
      ("stats", Stats.to_json ());
      ( "argv",
        `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x)) );
      ("executable_name", `String Sys.executable_name);
      ("z3_version", `String Z3.Version.to_string);
      ("commit", `String Build_info.commit);
      ("tree", `String Build_info.tree);
    ]
  |> Yojson.Basic.to_string |> print_endline
