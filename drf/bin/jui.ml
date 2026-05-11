open Stage0
open Protocols
open Drf
open Solve_drf

let render (output : Analysis.t list) : unit =
  let kernels =
    output
    |> List.map (fun analysis ->
        let open Analysis in
        let kernel_name = analysis.kernel.name in
        let solutions = analysis.report in
        let unknowns, errors =
          solutions
          |> List.filter_map (fun s ->
              let open Solution in
              match s.outcome with
              | Drf -> None
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
          let dd =
            if Variable.Set.cardinal w.data_approx > 0 then "DD" else "DI"
          in
          let cd =
            if Variable.Set.cardinal w.control_approx > 0 then "CD" else "CI"
          in
          cd ^ dd
        in
        let is_ok = List.length unknowns + List.length errors = 0 in
        `Assoc
          [
            ("kernel_name", `String kernel_name);
            ("status", `String (if is_ok then "drf" else "racy"));
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
          ])
  in
  `Assoc
    [
      ("kernels", `List kernels);
      ("phase_times", Phase_timer.to_json ());
      ( "argv",
        `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x)) );
      ("executable_name", `String Sys.executable_name);
      ("z3_version", `String Z3.Version.to_string);
    ]
  |> Yojson.Basic.to_string |> print_endline
