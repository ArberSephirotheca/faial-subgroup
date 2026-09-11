open Protocols
module SM = Inference.Subgroup_matrix
module Memory = Drf.Memory_event.Subgroup_obligation
module Solver = Drf.Subgroup_solver
module Symbolic_launch_evidence = Drf.Symbolic_launch_evidence
module Uniformity = Drf.Subgroup_uniformity

let var (name : string) : Variable.t = Variable.from_name name

let located_var ~(line : int) (name : string) : Variable.t =
  let location =
    Stage0.Location.make ~filename:"test_subgroup_solver.cu"
      ~line:(Stage0.Index.from_base1 line)
      ~interval:
        (Stage0.Interval.from_range
           ~start:(Stage0.Index.from_base1 1)
           ~length:1)
  in
  Variable.make ~name ~location ()

let variable_set (names : string list) : Variable.Set.t =
  List.fold_left
    (fun vars name -> Variable.Set.add (var name) vars)
    Variable.Set.empty names

let expect_matrix_ok (type a) (result : (a, string) result) : a =
  match result with Ok value -> value | Error msg -> Alcotest.fail msg

let expect_uniformity_ok (type a) (result : (a, Uniformity.error) result) : a =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (Uniformity.error_to_string error)

let expect_memory_ok (type a) (result : (a, Memory.error) result) : a =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (Memory.error_to_string error)

let subgroup_config ?(size = 32) () : SM.Target_config.t =
  SM.Target_config.subgroup_size_exn size |> SM.Target_config.cuda_x_contiguous

let base_access ?(mode = `Write) ?(array = "tile") () : Access.t =
  match mode with
  | `Read -> Access.read (var array) [ Exp.Var (var "base") ]
  | `Write -> Access.write (var array) [ Exp.Var (var "base") ] None

let conditional_access : Memory.conditional_access =
  {
    origin = Memory.Ordinary_write;
    source_site = None;
    source_order = None;
    access = base_access ();
    condition = Exp.Bool true;
    subgroup_phase = Memory.Subgroup_phase_key.root;
  }

let obligation ?(id = 0) ?(goal = Exp.Bool false) () : Memory.obligation =
  {
    id;
    phase_id = 0;
    array_name = "tile";
    left = conditional_access;
    right = conditional_access;
    goal;
  }

let strided_owner_condition ?(index_name = "elem_idx")
    ?(modulus = Exp.Var Variable.bdim_x) () : Exp.bexp =
  Exp.n_eq
    (Exp.n_mod
       (Exp.n_minus (Exp.Var (var index_name)) (Exp.Var Variable.tid_x))
       modulus)
    (Exp.Num 0)

let strided_access ?(index_name = "elem_idx") () : Access.t =
  Access.write (var "q_shmem") [ Exp.Var (var index_name) ] None

let affine_strided_access ?(index_name = "elem_idx") ?(offset_name = "C") () :
    Access.t =
  Access.write (var "q_shmem")
    [ Exp.n_minus (Exp.Var (var index_name)) (Exp.Var (var offset_name)) ]
    None

let strided_conditional_access ?index_name ?condition () :
    Memory.conditional_access =
  let index_name = Option.value index_name ~default:"elem_idx" in
  let condition =
    Option.value condition ~default:(strided_owner_condition ~index_name ())
  in
  {
    origin = Memory.Ordinary_write;
    source_site = None;
    source_order = None;
    access = strided_access ~index_name ();
    condition;
    subgroup_phase = Memory.Subgroup_phase_key.root;
  }

let affine_strided_conditional_access ?index_name ?offset_name ?condition () :
    Memory.conditional_access =
  let index_name = Option.value index_name ~default:"elem_idx" in
  let condition =
    Option.value condition ~default:(strided_owner_condition ~index_name ())
  in
  {
    origin = Memory.Ordinary_write;
    source_site = None;
    source_order = None;
    access = affine_strided_access ~index_name ?offset_name ();
    condition;
    subgroup_phase = Memory.Subgroup_phase_key.root;
  }

let strided_obligation ?(id = 20) ?left_index_name ?right_index_name
    ?left_condition ?right_condition ?(goal = Exp.Bool true) () :
    Memory.obligation =
  let left =
    strided_conditional_access ?index_name:left_index_name
      ?condition:left_condition ()
  in
  let right =
    strided_conditional_access ?index_name:right_index_name
      ?condition:right_condition ()
  in
  { id; phase_id = 0; array_name = "q_shmem"; left; right; goal }

let affine_strided_obligation ?(id = 21) ?left_index_name ?right_index_name
    ?left_offset_name ?right_offset_name ?left_condition ?right_condition
    ?(goal = Exp.Bool true) () : Memory.obligation =
  let left =
    affine_strided_conditional_access ?index_name:left_index_name
      ?offset_name:left_offset_name ?condition:left_condition ()
  in
  let right =
    affine_strided_conditional_access ?index_name:right_index_name
      ?offset_name:right_offset_name ?condition:right_condition ()
  in
  { id; phase_id = 0; array_name = "q_shmem"; left; right; goal }

let path_condition_obligation ?(id = 30) ~left_condition ~right_condition () :
    Memory.obligation =
  let access = Access.write (var "scratch") [ Exp.Num 0 ] None in
  let left = { conditional_access with access; condition = left_condition } in
  let right = { conditional_access with access; condition = right_condition } in
  {
    id;
    phase_id = 0;
    array_name = "scratch";
    left;
    right;
    goal = Exp.Bool true;
  }

let subgroup_row_owner_condition ?(row_name = "row_idx")
    ?(subgroup_name = "subgroup_id") () : Exp.bexp =
  let row = Exp.Var (var row_name) in
  let subgroup_id = Exp.Var (var subgroup_name) in
  Exp.b_and_ex
    [
      Exp.n_eq subgroup_id (Exp.n_div (Exp.Var Variable.tid_x) (Exp.Num 32));
      Exp.n_eq (Exp.n_mod (Exp.n_minus row subgroup_id) (Exp.Num 2)) (Exp.Num 0);
      Exp.n_le subgroup_id row;
      Exp.n_lt row (Exp.Num 16);
    ]

let subgroup_row_access ?(mode = `Write) ?(row_name = "row_idx") () : Access.t =
  match mode with
  | `Read -> Access.read (var "row_buffer") [ Exp.Var (var row_name) ]
  | `Write -> Access.write (var "row_buffer") [ Exp.Var (var row_name) ] None

let subgroup_row_conditional_access ?(mode = `Write) ?row_name ?condition () :
    Memory.conditional_access =
  let condition =
    Option.value condition ~default:(subgroup_row_owner_condition ?row_name ())
  in
  {
    conditional_access with
    origin =
      (match mode with
      | `Read -> Memory.Ordinary_read
      | `Write -> Memory.Ordinary_write);
    access = subgroup_row_access ~mode ?row_name ();
    condition;
  }

let subgroup_row_fixed_lane_obligation () : Memory.obligation =
  let lane_id = Exp.Var (var "lane_id") in
  let owner = subgroup_row_owner_condition () in
  let left = subgroup_row_conditional_access ~mode:`Read ~condition:owner () in
  let right_condition =
    Exp.b_and owner
      (Exp.b_and
         (Exp.n_eq lane_id (Exp.n_mod (Exp.Var Variable.tid_x) (Exp.Num 32)))
         (Exp.n_eq lane_id (Exp.Num 0)))
  in
  let right =
    subgroup_row_conditional_access ~mode:`Write ~condition:right_condition ()
  in
  {
    id = 40;
    phase_id = 4;
    array_name = "row_buffer";
    left;
    right;
    goal = Exp.Bool true;
  }

let lane_vector_condition ?(elem_name = "elem_base") ?(lane_name = "lane_id")
    ?(include_bound = true) () : Exp.bexp =
  let elem = Exp.Var (var elem_name) in
  let lane = Exp.Var (var lane_name) in
  let lane_base = Exp.n_mult lane (Exp.Num 4) in
  let bounds = if include_bound then [ Exp.n_lt elem (Exp.Num 64) ] else [] in
  Exp.b_and_ex
    ([
       Exp.n_eq lane (Exp.n_mod (Exp.Var Variable.tid_x) (Exp.Num 32));
       Exp.n_eq
         (Exp.n_mod (Exp.n_minus elem lane_base) (Exp.Num (32 * 4)))
         (Exp.Num 0);
       Exp.n_le lane_base elem;
     ]
    @ bounds)

let actual_like_lane_vector_condition ?(elem_name = "elem_base")
    ?(lane_name = "lane_id") ?(tid_name = "tid") ?(warp_name = "warp_id")
    ?(row_name = "q_tile_row") ?(include_bound = true) () : Exp.bexp =
  let elem = Exp.Var (var elem_name) in
  let lane = Exp.Var (var lane_name) in
  let tid = Exp.Var (var tid_name) in
  let warp = Exp.Var (var warp_name) in
  let row = Exp.Var (var row_name) in
  let warp_size = Exp.Var (var "WARP_SIZE") in
  let num_subgroups = Exp.Var (var "NUM_SUBGROUPS") in
  let lane_base = Exp.n_mult lane (Exp.Num 4) in
  let bounds =
    if include_bound then [ Exp.n_lt elem (Exp.Var (var "HEAD_DIM_V")) ] else []
  in
  Exp.b_and_ex
    ([
       Exp.n_eq (Exp.Var (var "HEAD_DIM_V")) (Exp.Num 64);
       Exp.n_eq (Exp.Var (var "WG_SIZE")) (Exp.Num 64);
       Exp.n_eq warp_size (Exp.Num 32);
       Exp.n_eq (Exp.Var (var "Q_TILE")) (Exp.Num 16);
       Exp.n_eq num_subgroups (Exp.n_div (Exp.Var (var "WG_SIZE")) warp_size);
       Exp.n_eq lane (Exp.n_mod tid warp_size);
       Exp.n_eq tid (Exp.Var Variable.tid_x);
       Exp.n_eq warp (Exp.n_div tid warp_size);
       Exp.n_lt row (Exp.Var (var "Q_TILE"));
       Exp.n_eq (Exp.n_mod (Exp.n_minus row warp) num_subgroups) (Exp.Num 0);
       Exp.n_le warp row;
       Exp.n_eq
         (Exp.n_mod
            (Exp.n_minus elem lane_base)
            (Exp.n_mult warp_size (Exp.Num 4)))
         (Exp.Num 0);
       Exp.n_le lane_base elem;
     ]
    @ bounds)

let lane_vector_access ?(row_name = "row_base") ?(elem_name = "elem_base")
    ?(component = 0) () : Access.t =
  Access.write (var "vector_buffer")
    [
      Exp.n_plus
        (Exp.n_plus (Exp.Var (var row_name)) (Exp.Var (var elem_name)))
        (Exp.Num component);
    ]
    None

let lane_vector_row_stride_condition ?(row_name = "row_base")
    ?(q_row_name = "q_tile_row") ?(base_name = "dst_global_offset")
    ?(stride_name = "dst2_stride") ?(head_dim_name = "HEAD_DIM_V")
    ?(heads_name = "params.n_heads") () : Exp.bexp =
  let row_base = Exp.Var (var row_name) in
  let q_row = Exp.Var (var q_row_name) in
  let base = Exp.Var (var base_name) in
  let stride = Exp.Var (var stride_name) in
  let head_dim = Exp.Var (var head_dim_name) in
  let heads = Exp.Var (var heads_name) in
  Exp.b_and_ex
    [
      Exp.n_eq row_base (Exp.n_plus base (Exp.n_mult q_row stride));
      Exp.n_eq stride (Exp.n_mult head_dim heads);
      Exp.n_eq head_dim (Exp.Num 64);
    ]

let lane_vector_component_obligation ?(left_component = 2)
    ?(right_component = 3) ?(elem_name = "elem_base") ?(row_name = "row_base")
    ?(include_bound = true) ?(actual_like = false) () : Memory.obligation =
  let condition =
    Exp.b_and
      (if actual_like then
         actual_like_lane_vector_condition ~elem_name ~include_bound ()
       else lane_vector_condition ~elem_name ~include_bound ())
      (lane_vector_row_stride_condition ~row_name ())
  in
  let left =
    {
      conditional_access with
      origin = Memory.Ordinary_write;
      access =
        lane_vector_access ~row_name ~elem_name ~component:left_component ();
      condition;
    }
  in
  let right =
    {
      conditional_access with
      origin = Memory.Ordinary_write;
      access =
        lane_vector_access ~row_name ~elem_name ~component:right_component ();
      condition;
    }
  in
  {
    id = 42;
    phase_id = 7;
    array_name = "vector_buffer";
    left;
    right;
    goal = Exp.Bool true;
  }

let lane_vector_component_obligation_with_locations () : Memory.obligation =
  let access_var = located_var ~line:10 in
  let guard_var = located_var ~line:20 in
  let elem = Exp.Var (guard_var "elem_base") in
  let lane = Exp.Var (guard_var "lane_id") in
  let row_base = Exp.Var (guard_var "row_base") in
  let q_row = Exp.Var (guard_var "q_tile_row") in
  let lane_base = Exp.n_mult lane (Exp.Num 4) in
  let condition =
    Exp.b_and_ex
      [
        Exp.n_eq lane (Exp.n_mod (Exp.Var Variable.tid_x) (Exp.Num 32));
        Exp.n_eq
          (Exp.n_mod (Exp.n_minus elem lane_base) (Exp.Num 128))
          (Exp.Num 0);
        Exp.n_le lane_base elem;
        Exp.n_lt elem (Exp.Var (guard_var "HEAD_DIM_V"));
        Exp.n_eq row_base
          (Exp.n_plus
             (Exp.Var (guard_var "dst_global_offset"))
             (Exp.n_mult q_row (Exp.Var (guard_var "dst2_stride"))));
        Exp.n_eq
          (Exp.Var (guard_var "dst2_stride"))
          (Exp.n_mult
             (Exp.Var (guard_var "HEAD_DIM_V"))
             (Exp.Var (guard_var "params.n_heads")));
        Exp.n_eq (Exp.Var (guard_var "HEAD_DIM_V")) (Exp.Num 64);
      ]
  in
  let access component =
    Access.write (var "vector_buffer")
      [
        Exp.n_plus
          (Exp.n_plus
             (Exp.Var (access_var "row_base"))
             (Exp.Var (access_var "elem_base")))
          (Exp.Num component);
      ]
      None
  in
  {
    id = 44;
    phase_id = 7;
    array_name = "vector_buffer";
    left =
      {
        conditional_access with
        origin = Memory.Ordinary_write;
        access = access 2;
        condition;
      };
    right =
      {
        conditional_access with
        origin = Memory.Ordinary_write;
        access = access 3;
        condition;
      };
    goal = Exp.Bool true;
  }

let lane_vector_obligation_without_row_stride () : Memory.obligation =
  let left =
    {
      conditional_access with
      origin = Memory.Ordinary_write;
      access = lane_vector_access ();
      condition = lane_vector_condition ();
    }
  in
  let right =
    {
      conditional_access with
      origin = Memory.Ordinary_write;
      access = lane_vector_access ();
      condition = lane_vector_condition ();
    }
  in
  {
    id = 41;
    phase_id = 7;
    array_name = "vector_buffer";
    left;
    right;
    goal = Exp.Bool true;
  }

let expect_pre_solver_none (name : string)
    (classification : Solver.classification option) : unit =
  match classification with
  | None -> ()
  | Some classification ->
      Alcotest.fail
        (Printf.sprintf "%s unexpectedly discharged as %s" name
           (Solver.classification_to_string classification))

let expect_one_dimensional_strided_discharge
    (classification : Solver.classification option) : unit =
  match classification with
  | Some (Solver.Pre_solver_unsat reason) ->
      Alcotest.(check string)
        "reason" "one-dimensional strided thread ownership" reason
  | Some classification ->
      Alcotest.fail
        ("unexpected classification: "
        ^ Solver.classification_to_string classification)
  | None -> Alcotest.fail "expected one-dimensional strided discharge"

let expect_contradictory_path_discharge
    (classification : Solver.classification option) : unit =
  match classification with
  | Some (Solver.Pre_solver_unsat reason) ->
      Alcotest.(check string)
        "reason" "contradictory path-condition filter" reason
  | Some classification ->
      Alcotest.fail
        ("unexpected classification: "
        ^ Solver.classification_to_string classification)
  | None -> Alcotest.fail "expected contradictory path-condition discharge"

let expect_pre_solver_discharge ~(reason : string)
    (classification : Solver.classification option) : unit =
  match classification with
  | Some (Solver.Pre_solver_unsat actual) ->
      Alcotest.(check string) "reason" reason actual
  | Some classification ->
      Alcotest.fail
        ("unexpected classification: "
        ^ Solver.classification_to_string classification)
  | None -> Alcotest.fail ("expected pre-solver discharge: " ^ reason)

let rectangular (access : Access.t) : SM.Matrix.footprint =
  SM.Matrix.rectangular ~base:access ~rows:(Exp.Num 16) ~cols:(Exp.Num 8)
    ~leading_dimension:(Exp.Num 32) ~layout:SM.Matrix.Row_major ~row:(var "row")
    ~col:(var "col")
  |> expect_matrix_ok

let matrix_store (site_id : int) : SM.Stmt.t =
  let site = SM.Site.make ~label:"store_matrix_sync" site_id in
  let collective =
    SM.Matrix.store_matrix_sync site (rectangular (base_access ()))
    |> expect_matrix_ok
  in
  SM.Stmt.Matrix_collective collective

let kernel ?(target_config = subgroup_config ()) ?(name = "kernel")
    (body : SM.Stmt.t list) : SM.Kernel.t =
  SM.Kernel.make ~target_config ~name body

let test_sat_unsat_solver_classifications_are_distinct () : unit =
  let unsat =
    Solver.evidence_of_obligation (obligation ~id:1 ~goal:(Exp.Bool false) ())
  in
  let sat =
    Solver.evidence_of_obligation (obligation ~id:2 ~goal:(Exp.Bool true) ())
  in
  Alcotest.(check string)
    "unsat is memory DRF evidence" "solver=unsat(drf)"
    (Solver.classification_to_string unsat.classification);
  Alcotest.(check string)
    "sat is racy evidence" "solver=sat(racy)"
    (Solver.classification_to_string sat.classification)

let test_timeout_and_unknown_reasons_are_not_collapsed () : unit =
  let timeout = Solver.classification_of_unknown_reason "timeout" in
  let unknown = Solver.classification_of_unknown_reason "incomplete" in
  Alcotest.(check string)
    "timeout reason" "solver=timeout(reason=timeout)"
    (Solver.classification_to_string timeout);
  Alcotest.(check string)
    "generic unknown reason" "solver=unknown(reason=incomplete)"
    (Solver.classification_to_string unknown)

let report_with_classification (classification : Solver.classification) :
    Solver.report =
  {
    kernel_name = "s488_classification_test";
    config = Solver.default_config;
    z3_version = "test";
    obligations =
      [
        Solver.evidence_of_obligation ~classification:(Some classification)
          (obligation ());
      ];
  }

let test_s488_classification_labels_follow_solver_verdict () : unit =
  let check expected classification =
    Alcotest.(check string)
      expected expected
      (Symbolic_launch_evidence.s488_classification_to_string
         (Symbolic_launch_evidence.s488_classification_of_report
            (report_with_classification classification)))
  in
  check "unguarded_unsat" Solver.Solver_unsat_drf;
  check "invalid_counterexample_needs_guard" Solver.Solver_sat_racy;
  check "timeout" (Solver.Solver_timeout "unit timeout");
  check "timeout" (Solver.Solver_unknown "unit unknown");
  check "unsupported" (Solver.Unsupported "unit unsupported")

let s489_family_result family_id results =
  match
    List.find_opt
      (fun result ->
        String.equal result.Symbolic_launch_evidence.s489_family_id family_id)
      results
  with
  | Some result -> result
  | None -> Alcotest.fail ("missing S489 family " ^ family_id)

let s489_count label counts =
  List.assoc_opt label counts |> Option.value ~default:0

let test_s489_frontier_classifies_solve_tri_and_blocks_unbuilt_families () :
    unit =
  let report = report_with_classification Solver.Solver_unsat_drf in
  let results =
    Symbolic_launch_evidence.s489_frontier_results
      ~solve_tri_report:(Some report)
  in
  Alcotest.(check int) "full imported family inventory" 95 (List.length results);
  let counts = Symbolic_launch_evidence.s489_classification_counts results in
  Alcotest.(check int)
    "current proof overlays solved" 33
    (s489_count "unguarded_unsat" counts);
  Alcotest.(check int)
    "remaining extraction blockers" 54
    (s489_count "extraction_blocked" counts);
  Alcotest.(check int)
    "unsupported boundaries" 8
    (s489_count "unsupported" counts);
  let solve_tri = s489_family_result "F082" results in
  Alcotest.(check string)
    "solve_tri classification" "unguarded_unsat"
    (Symbolic_launch_evidence.s488_classification_to_string
       solve_tri.Symbolic_launch_evidence.s489_classification);
  Alcotest.(check string)
    "solve_tri solver status" "solver_classified"
    solve_tri.Symbolic_launch_evidence.s489_solver_artifact_status;
  let clamp = s489_family_result "F011" results in
  Alcotest.(check string)
    "clamp structural classification" "unguarded_unsat"
    (Symbolic_launch_evidence.s488_classification_to_string
       clamp.Symbolic_launch_evidence.s489_classification);
  Alcotest.(check string)
    "clamp shape builder" "one_dimensional_elementwise"
    clamp.Symbolic_launch_evidence.s489_shape_builder;
  Alcotest.(check string)
    "clamp structural solver status" "pre_solver_structural_drf"
    clamp.Symbolic_launch_evidence.s489_solver_artifact_status;
  let atomic = s489_family_result "F040" results in
  Alcotest.(check string)
    "atomic unsupported classification" "unsupported"
    (Symbolic_launch_evidence.s488_classification_to_string
       atomic.Symbolic_launch_evidence.s489_classification);
  Alcotest.(check bool)
    "atomic blocker is preserved" true
    (Stage0.Common.contains ~substring:"atomic"
       atomic.Symbolic_launch_evidence.s489_first_blocker);
  let artifact =
    Symbolic_launch_evidence.s489_all_family_frontier_artifact_lines
      ~filename:"unit_s489.cu" ~solve_tri_report:(Some report)
    |> String.concat "\n"
  in
  Alcotest.(check bool)
    "artifact marker" true
    (Stage0.Common.contains
       ~substring:"artifact: ocaml-s489-all-family-unguarded-frontier-v1"
       artifact);
  Alcotest.(check bool)
    "family count" true
    (Stage0.Common.contains ~substring:"family_count: 95" artifact);
  Alcotest.(check bool)
    "row count" true
    (Stage0.Common.contains ~substring:"row_count: 146" artifact);
  Alcotest.(check bool)
    "classification counts" true
    (Stage0.Common.contains
       ~substring:
         "classification_counts: extraction_blocked=54, unguarded_unsat=33, \
          unsupported=8"
       artifact)

let test_s489_frontier_without_symbolic_report_marks_all_blocked () : unit =
  let results =
    Symbolic_launch_evidence.s489_frontier_results ~solve_tri_report:None
  in
  Alcotest.(check int) "full imported family inventory" 95 (List.length results);
  let counts = Symbolic_launch_evidence.s489_classification_counts results in
  Alcotest.(check int)
    "blocked families including solve_tri" 55
    (s489_count "extraction_blocked" counts);
  Alcotest.(check int)
    "structural families solved without report" 32
    (s489_count "unguarded_unsat" counts);
  Alcotest.(check int)
    "unsupported boundaries remain explicit" 8
    (s489_count "unsupported" counts);
  let solve_tri = s489_family_result "F082" results in
  Alcotest.(check string)
    "solve_tri waits for report" "extraction_blocked"
    (Symbolic_launch_evidence.s488_classification_to_string
       solve_tri.Symbolic_launch_evidence.s489_classification);
  let im2col = s489_family_result "F055" results in
  Alcotest.(check string)
    "im2col builder" "bounded_symbolic_block_dim"
    im2col.Symbolic_launch_evidence.s489_shape_builder;
  let artifact =
    Symbolic_launch_evidence.s489_all_family_frontier_artifact_lines
      ~filename:"unit_s489_no_report.cu" ~solve_tri_report:None
    |> String.concat "\n"
  in
  Alcotest.(check bool)
    "blocked count is recorded" true
    (Stage0.Common.contains
       ~substring:
         "classification_counts: extraction_blocked=55, unguarded_unsat=32, \
          unsupported=8"
       artifact)

let s491_candidate family_id candidates =
  match
    List.find_opt
      (fun candidate ->
        String.equal candidate.Symbolic_launch_evidence.s491_family_id family_id)
      candidates
  with
  | Some candidate -> candidate
  | None -> Alcotest.fail ("missing S491 candidate " ^ family_id)

let s491_count label counts =
  List.assoc_opt label counts |> Option.value ~default:0

let test_s491_blocker_retirement_frontier_targets_launch_template () : unit =
  let report = report_with_classification Solver.Solver_unsat_drf in
  let candidates =
    Symbolic_launch_evidence.s491_blocker_retirement_candidates
      ~solve_tri_report:(Some report)
  in
  Alcotest.(check int) "remaining blocked families" 54 (List.length candidates);
  let counts = Symbolic_launch_evidence.s491_candidate_counts candidates in
  Alcotest.(check int)
    "launch-template blockers" 22
    (s491_count "launch_template" counts);
  Alcotest.(check int)
    "preprocessing blockers" 28
    (s491_count "preprocessing" counts);
  Alcotest.(check int)
    "row-derived symbolic blockers" 3
    (s491_count "row_derived_symbolic_family_guard" counts);
  Alcotest.(check int)
    "subgroup-config blockers" 1
    (s491_count "subgroup_config" counts);
  let selected =
    Symbolic_launch_evidence.s491_selected_launch_template_candidates
      ~solve_tri_report:(Some report)
  in
  Alcotest.(check int)
    "selected launch-template worklist" 22 (List.length selected);
  let allreduce = s491_candidate "F003" selected in
  Alcotest.(check string)
    "F003 selected blocker" "launch_template:template_argument_status"
    allreduce.Symbolic_launch_evidence.s491_first_blocker;
  Alcotest.(check bool)
    "F011 already structurally discharged" false
    (List.exists
       (fun candidate ->
         String.equal candidate.Symbolic_launch_evidence.s491_family_id "F011")
       candidates);
  let gla = s491_candidate "F054" candidates in
  Alcotest.(check string)
    "GLA remains symbolic-family blocker" "row_derived_symbolic_family_guard"
    gla.Symbolic_launch_evidence.s491_blocker_class;
  let artifact =
    Symbolic_launch_evidence.s491_blocker_retirement_artifact_lines
      ~filename:"unit_s491.cu" ~solve_tri_report:(Some report)
    |> String.concat "\n"
  in
  Alcotest.(check bool)
    "artifact marker" true
    (Stage0.Common.contains
       ~substring:"artifact: ocaml-s491-blocker-retirement-frontier-v1" artifact);
  Alcotest.(check bool)
    "blocker counts" true
    (Stage0.Common.contains
       ~substring:
         "blocker_counts: launch_template=22, preprocessing=28, \
          row_derived_symbolic_family_guard=3, subgroup_config=1"
       artifact);
  Alcotest.(check bool)
    "selected count" true
    (Stage0.Common.contains ~substring:"selected_family_count: 22" artifact)

let test_solver_normalizes_transitive_numeric_constants () : unit =
  let wg_size = var "WG_SIZE" in
  let warp_size = var "WARP_SIZE" in
  let num_subgroups = var "NUM_SUBGROUPS" in
  let goal =
    Exp.b_and_ex
      [
        Exp.n_eq (Exp.Var wg_size) (Exp.Num 64);
        Exp.n_eq (Exp.Var warp_size) (Exp.Num 32);
        Exp.n_eq (Exp.Var num_subgroups)
          (Exp.n_div (Exp.Var wg_size) (Exp.Var warp_size));
        Exp.n_neq (Exp.Var num_subgroups) (Exp.Num 2);
      ]
  in
  let normalized = Solver.normalize_goal_for_solver goal in
  Alcotest.(check string)
    "normalizes transitive constants to contradiction" "false"
    (Exp.b_to_string normalized);
  Alcotest.(check string)
    "solver sees normalized contradiction" "solver=unsat(drf)"
    (Solver.classification_to_string (Solver.solve_goal goal))

let symbolic_k_vector_goal ?(include_upper_guard = false) () : Exp.bexp =
  let k = Exp.Var (var "K") in
  let bdim_x = Exp.Var Variable.bdim_x in
  let bdim_y = Exp.Var Variable.bdim_y in
  let bdim_z = Exp.Var Variable.bdim_z in
  let tx1 = Exp.Var (var "threadIdx.x$T1") in
  let ty1 = Exp.Var (var "threadIdx.y$T1") in
  let tz1 = Exp.Var (var "threadIdx.z$T1") in
  let tx2 = Exp.Var (var "threadIdx.x$T2") in
  let ty2 = Exp.Var (var "threadIdx.y$T2") in
  let tz2 = Exp.Var (var "threadIdx.z$T2") in
  let row_major tx ty = Exp.n_plus (Exp.n_mult tx (Exp.Num 32)) ty in
  let upper_guard =
    if include_upper_guard then [ Exp.n_le k (Exp.Num 32) ] else []
  in
  Exp.b_and_ex
    ([
       Exp.n_eq bdim_x (Exp.Num 32);
       Exp.n_eq bdim_y k;
       Exp.n_eq bdim_z (Exp.Num 1);
       Exp.n_gt k (Exp.Num 0);
       Exp.n_le (Exp.Num 0) tx1;
       Exp.n_lt tx1 bdim_x;
       Exp.n_le (Exp.Num 0) ty1;
       Exp.n_lt ty1 k;
       Exp.n_eq tz1 (Exp.Num 0);
       Exp.n_le (Exp.Num 0) tx2;
       Exp.n_lt tx2 bdim_x;
       Exp.n_le (Exp.Num 0) ty2;
       Exp.n_lt ty2 k;
       Exp.n_eq tz2 (Exp.Num 0);
       Exp.b_or_ex [ Exp.n_neq tx1 tx2; Exp.n_neq ty1 ty2 ];
       Exp.n_eq (row_major tx1 ty1) (row_major tx2 ty2);
     ]
    @ upper_guard)

let test_symbolic_k_goal_requires_executable_upper_guard () : unit =
  let underconstrained_goal = symbolic_k_vector_goal () in
  let obligation = obligation ~id:9 ~goal:underconstrained_goal () in
  Solver.pre_solver_classification obligation
  |> expect_pre_solver_none "symbolic K without concrete checked block dim";
  Alcotest.(check string)
    "underconstrained symbolic K stays solver-visible" "solver=sat(racy)"
    (Solver.classification_to_string (Solver.solve_goal underconstrained_goal));
  Alcotest.(check string)
    "guarded symbolic K closes the vector ownership proof" "solver=unsat(drf)"
    (Solver.classification_to_string
       (Solver.solve_goal (symbolic_k_vector_goal ~include_upper_guard:true ())))

let test_unsupported_memory_boundary_is_not_solver_unknown () : unit =
  let outcome =
    Error
      (Memory.Invalid_symbolic_checked_block_dim
         "missing subgroup ordering dimensions")
    |> Solver.solve_obligation_result ~kernel_name:"missing_block_dim"
  in
  Alcotest.(check string)
    "unsupported memory verdict" "unsupported"
    (Solver.memory_verdict_to_string (Solver.memory_verdict outcome));
  let summary = Solver.summary_lines outcome |> String.concat "\n" in
  Alcotest.(check bool)
    "summary records unsupported boundary" true
    (Stage0.Common.contains ~substring:"unsupported(reason=" summary);
  Alcotest.(check bool)
    "summary does not call unsupported unknown" false
    (Stage0.Common.contains ~substring:"mem_drf: unknown" summary)

let unsupported_outcome ~(reason : string) : Solver.memory_outcome =
  Solver.Memory_unsupported
    {
      kernel_name = "repeated_sync";
      config = Solver.default_config;
      z3_version = Z3.Version.to_string;
      reason;
    }

let test_loop_protocol_replaces_only_repeated_site_boundary () : unit =
  let protocol =
    Solver.loop_protocol_outcome ~kernel_name:"repeated_sync"
      ~classifications:[ Solver.Solver_unsat_drf ]
      ~evidence:[ "loop-aware protocol proof" ]
      ()
  in
  let repeated =
    unsupported_outcome
      ~reason:
        "kernel contains a memory-ordering barrier at repeating site; dynamic \
         invocation matching is not yet supported"
  in
  let resolved =
    Solver.resolve_repeated_site_with_loop_protocol ~protocol repeated
  in
  Alcotest.(check string)
    "loop protocol supplies repeated-site verdict" "drf"
    (Solver.memory_verdict_to_string (Solver.memory_verdict resolved));
  let evidence = Solver.memory_evidence_lines resolved |> String.concat "\n" in
  Alcotest.(check bool)
    "loop protocol evidence is retained" true
    (Stage0.Common.contains ~substring:"loop-aware protocol proof" evidence);
  let missing_dims =
    unsupported_outcome
      ~reason:"missing checked block dimensions for subgroup memory ordering"
  in
  let unresolved =
    Solver.resolve_repeated_site_with_loop_protocol ~protocol missing_dims
  in
  Alcotest.(check string)
    "unrelated unsupported boundary is retained" "unsupported"
    (Solver.memory_verdict_to_string (Solver.memory_verdict unresolved))

let test_loop_protocol_preserves_racy_verdict () : unit =
  let protocol =
    Solver.loop_protocol_outcome ~kernel_name:"repeated_sync"
      ~classifications:[ Solver.Solver_sat_racy ]
      ~evidence:[ "loop-aware protocol race" ]
      ()
  in
  let repeated =
    unsupported_outcome
      ~reason:
        "kernel contains a memory-ordering barrier at repeating site; dynamic \
         invocation matching is not yet supported"
  in
  let resolved =
    Solver.resolve_repeated_site_with_loop_protocol ~protocol repeated
  in
  Alcotest.(check string)
    "loop protocol retains potential race" "not_drf"
    (Solver.memory_verdict_to_string (Solver.memory_verdict resolved))

let test_symbolic_obligation_evidence_is_deterministic () : unit =
  let left =
    {
      conditional_access with
      source_site = Some "ordinary#1[write]@fixture.cu:10:3";
    }
  in
  let right =
    {
      conditional_access with
      source_site = Some "ordinary#2[write]@fixture.cu:11:3";
    }
  in
  let report =
    Solver.solve_obligations ~kernel_name:"fixture"
      [
        {
          (obligation ~id:7 ~goal:(Exp.n_eq (Exp.Num 1) (Exp.Num 1)) ()) with
          left;
          right;
        };
      ]
  in
  let lines = Solver.summary_lines (Solver.Memory_report report) in
  Alcotest.(check bool)
    "summary includes solver config" true
    (List.exists
       (Stage0.Common.contains ~substring:"solver_config: logic=default")
       lines);
  Alcotest.(check bool)
    "summary includes memory check counts" true
    (List.exists
       (String.equal
          "memory_checks: 1 total, 1 racy, 0 unknown, 0 timeout, 0 \
           unsupported, 0 pre_solver_unsat")
       lines);
  Alcotest.(check bool)
    "obligation line includes symbolic goal" true
    (List.exists
       (Stage0.Common.contains ~substring:"obligation#7 phase=0 array=tile")
       lines);
  Alcotest.(check bool)
    "obligation line includes left source site" true
    (List.exists
       (Stage0.Common.contains
          ~substring:"left_site=ordinary#1[write]@fixture.cu:10:3")
       lines);
  Alcotest.(check bool)
    "obligation line includes right source site" true
    (List.exists
       (Stage0.Common.contains
          ~substring:"right_site=ordinary#2[write]@fixture.cu:11:3")
       lines)

let test_pre_solver_unsat_is_reserved_but_not_a_rule () : unit =
  let evidence =
    Solver.pre_solver_unsat_evidence ~reason:"unit test marker"
      (obligation ~id:8 ())
  in
  Alcotest.(check string)
    "pre-solver classification" "pre_solver=unsat(reason=unit test marker)"
    (Solver.classification_to_string evidence.classification);
  Alcotest.(check bool)
    "pre-solver unsat remains memory-safe evidence" true
    (Solver.classification_is_memory_drf evidence.classification)

let test_pre_solver_discharges_contradictory_path_conditions () : unit =
  let global_flag = var "params.max_bias" in
  let globals = Variable.Set.add global_flag Variable.Set.empty in
  let obligation =
    path_condition_obligation
      ~left_condition:(Exp.n_le (Exp.Var global_flag) (Exp.Num 0))
      ~right_condition:(Exp.n_gt (Exp.Var global_flag) (Exp.Num 0))
      ()
  in
  Solver.pre_solver_classification ~globals obligation
  |> expect_contradictory_path_discharge;
  let report =
    Solver.solve_obligations ~kernel_name:"contradictory_path" ~globals
      [ obligation ]
  in
  let evidence =
    match report.obligations with
    | [ evidence ] -> evidence
    | _ -> Alcotest.fail "expected one pre-solver evidence row"
  in
  Alcotest.(check string)
    "classification"
    "pre_solver=unsat(reason=contradictory path-condition filter)"
    (Solver.classification_to_string evidence.classification);
  let summary =
    Solver.summary_lines (Solver.Memory_report report) |> String.concat "\n"
  in
  Alcotest.(check bool)
    "summary counts one contradictory-path discharge" true
    (Stage0.Common.contains ~substring:"1 pre_solver_unsat" summary)

let test_pre_solver_keeps_task_local_complements_solver_visible () : unit =
  let local_flag = var "local_flag" in
  path_condition_obligation
    ~left_condition:(Exp.n_le (Exp.Var local_flag) (Exp.Num 0))
    ~right_condition:(Exp.n_gt (Exp.Var local_flag) (Exp.Num 0))
    ()
  |> Solver.pre_solver_classification
  |> expect_pre_solver_none "task-local complementary guards"

let test_pre_solver_keeps_changed_path_condition_shape_visible () : unit =
  let global_flag = var "params.max_bias" in
  let globals = Variable.Set.add global_flag Variable.Set.empty in
  path_condition_obligation
    ~left_condition:(Exp.n_le (Exp.Var global_flag) (Exp.Num 0))
    ~right_condition:(Exp.n_lt (Exp.Var global_flag) (Exp.Num 0))
    ()
  |> Solver.pre_solver_classification ~globals
  |> expect_pre_solver_none "changed path-condition shape"

let test_pre_solver_discharges_one_dimensional_strided_owner () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let obligation = strided_obligation () in
  let report =
    Solver.solve_obligations ~kernel_name:"strided_owner" ~block_dim
      [ obligation ]
  in
  let evidence =
    match report.obligations with
    | [ evidence ] -> evidence
    | _ -> Alcotest.fail "expected one pre-solver evidence row"
  in
  Alcotest.(check string)
    "classification"
    "pre_solver=unsat(reason=one-dimensional strided thread ownership)"
    (Solver.classification_to_string evidence.classification);
  let summary =
    Solver.summary_lines (Solver.Memory_report report) |> String.concat "\n"
  in
  Alcotest.(check bool)
    "summary counts one pre-solver discharge" true
    (Stage0.Common.contains ~substring:"1 pre_solver_unsat" summary)

let test_pre_solver_leaves_changed_shape_solver_visible () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let obligation = strided_obligation ~right_index_name:"other_elem_idx" () in
  Solver.pre_solver_classification ~block_dim obligation
  |> expect_pre_solver_none "changed index base";
  let report =
    Solver.solve_obligations ~kernel_name:"changed_shape" ~block_dim
      [ obligation ]
  in
  let evidence =
    match report.obligations with
    | [ evidence ] -> evidence
    | _ -> Alcotest.fail "expected one solver evidence row"
  in
  Alcotest.(check string)
    "no-match still reaches solver" "solver=sat(racy)"
    (Solver.classification_to_string evidence.classification)

let test_pre_solver_rejects_multidimensional_block () : unit =
  let block_dim = Dim3.make ~x:64 ~y:2 () in
  Solver.pre_solver_classification ~block_dim (strided_obligation ())
  |> expect_pre_solver_none "multidimensional x-only ownership"

let test_pre_solver_rejects_invalid_stride_bound () : unit =
  let block_dim = Dim3.make ~x:0 () in
  Solver.pre_solver_classification ~block_dim (strided_obligation ())
  |> expect_pre_solver_none "zero-sized stride"

let test_pre_solver_is_not_tied_to_fixture_variable_names () : unit =
  let block_dim = Dim3.make ~x:64 () in
  strided_obligation ~left_index_name:"renamed_idx"
    ~right_index_name:"renamed_idx" ()
  |> Solver.pre_solver_classification ~block_dim
  |> expect_one_dimensional_strided_discharge

let test_pre_solver_discharges_affine_strided_owner_with_global_offset () : unit
    =
  let block_dim = Dim3.make ~x:64 () in
  let globals = variable_set [ "C" ] in
  affine_strided_obligation ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_one_dimensional_strided_discharge

let test_pre_solver_keeps_affine_strided_owner_with_local_offset_visible () :
    unit =
  let block_dim = Dim3.make ~x:64 () in
  affine_strided_obligation ()
  |> Solver.pre_solver_classification ~block_dim
  |> expect_pre_solver_none "affine strided owner with task-local offset"

let test_pre_solver_keeps_affine_strided_owner_with_changed_offset_visible () :
    unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals = variable_set [ "C"; "D" ] in
  affine_strided_obligation ~right_offset_name:"D" ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_pre_solver_none "affine strided owner with changed offset"

let test_pre_solver_affine_strided_alpha_renaming () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals = variable_set [ "C" ] in
  affine_strided_obligation ~left_index_name:"renamed_idx"
    ~right_index_name:"renamed_idx" ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_one_dimensional_strided_discharge

let test_pre_solver_keeps_subgroup_row_fixed_lane_visible () : unit =
  let block_dim = Dim3.make ~x:64 () in
  subgroup_row_fixed_lane_obligation ()
  |> Solver.pre_solver_classification ~block_dim
  |> expect_pre_solver_none "subgroup-row fixed-lane read/write"

let test_pre_solver_keeps_lane_vector_without_row_stride_visible () : unit =
  let block_dim = Dim3.make ~x:64 () in
  lane_vector_obligation_without_row_stride ()
  |> Solver.pre_solver_classification ~block_dim
  |> expect_pre_solver_none "lane-vector ownership without row stride"

let test_pre_solver_discharges_lane_vector_row_stride_component () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals =
    variable_set [ "dst_global_offset"; "dst2_stride"; "HEAD_DIM_V"; "params" ]
  in
  lane_vector_component_obligation ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_pre_solver_discharge
       ~reason:"hierarchical subgroup row/lane-vector ownership"

let test_pre_solver_keeps_lane_vector_same_component_visible () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals =
    variable_set [ "dst_global_offset"; "dst2_stride"; "HEAD_DIM_V"; "params" ]
  in
  lane_vector_component_obligation ~left_component:2 ~right_component:2 ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_pre_solver_none "lane-vector same component"

let test_pre_solver_keeps_lane_vector_without_global_stride_visible () : unit =
  let block_dim = Dim3.make ~x:64 () in
  lane_vector_component_obligation ()
  |> Solver.pre_solver_classification ~block_dim
  |> expect_pre_solver_none "lane-vector row stride without globals"

let test_pre_solver_rejects_lane_vector_missing_bound () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals =
    variable_set [ "dst_global_offset"; "dst2_stride"; "HEAD_DIM_V"; "params" ]
  in
  lane_vector_component_obligation ~include_bound:false ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_pre_solver_none "lane-vector missing loop bound"

let test_pre_solver_lane_vector_alpha_renaming () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals =
    variable_set [ "dst_global_offset"; "dst2_stride"; "HEAD_DIM_V"; "params" ]
  in
  lane_vector_component_obligation ~row_name:"rb" ~elem_name:"eb" ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_pre_solver_discharge
       ~reason:"hierarchical subgroup row/lane-vector ownership"

let test_pre_solver_lane_vector_actual_like_aliases () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals =
    variable_set
      [
        "dst_global_offset";
        "dst2_stride";
        "HEAD_DIM_V";
        "params";
        "WARP_SIZE";
        "WG_SIZE";
        "NUM_SUBGROUPS";
        "Q_TILE";
      ]
  in
  lane_vector_component_obligation ~actual_like:true ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_pre_solver_discharge
       ~reason:"hierarchical subgroup row/lane-vector ownership"

let test_pre_solver_lane_vector_projected_goal_aliases () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals =
    variable_set
      [
        "dst_global_offset";
        "dst2_stride";
        "HEAD_DIM_V";
        "params";
        "WARP_SIZE";
        "WG_SIZE";
        "NUM_SUBGROUPS";
        "Q_TILE";
      ]
  in
  let source = lane_vector_component_obligation ~actual_like:true () in
  let goal =
    Memory.obligation_goal ~globals ~block_dim (subgroup_config ()) source.left
      source.right
    |> expect_memory_ok
  in
  let hidden_access = base_access () in
  let obligation =
    {
      source with
      left = { source.left with access = hidden_access };
      right = { source.right with access = hidden_access };
      goal;
    }
  in
  Solver.pre_solver_classification ~globals ~block_dim obligation
  |> expect_pre_solver_discharge
       ~reason:"hierarchical subgroup row/lane-vector ownership"

let test_pre_solver_lane_vector_location_insensitive_names () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let globals =
    variable_set [ "dst_global_offset"; "dst2_stride"; "HEAD_DIM_V"; "params" ]
  in
  lane_vector_component_obligation_with_locations ()
  |> Solver.pre_solver_classification ~globals ~block_dim
  |> expect_pre_solver_discharge
       ~reason:"hierarchical subgroup row/lane-vector ownership"

let test_uniformity_ub_remains_separate_from_memory_drf () : unit =
  let memory =
    Solver.solve_obligations ~kernel_name:"uniformity_ub"
      [ obligation ~goal:(Exp.Bool false) () ]
  in
  let subgroup_kernel = kernel ~name:"uniformity_ub" [ matrix_store 30 ] in
  let control =
    Uniformity.control
      ~conditions:[ Exp.n_lt (Exp.Var Variable.tid_x) (Exp.Num 32) ]
  in
  let uniformity =
    Uniformity.check_kernel ~site_controls:[ (30, control) ] subgroup_kernel
    |> expect_uniformity_ok
  in
  let lines = Solver.summary_lines ~uniformity (Solver.Memory_report memory) in
  Alcotest.(check bool)
    "memory remains DRF" true
    (List.exists (String.equal "mem_drf: drf") lines);
  Alcotest.(check bool)
    "uniformity UB remains separate" true
    (List.exists (String.equal "subgroup_uniformity: undefined_behavior") lines);
  Alcotest.(check bool)
    "full verdict composes both components" true
    (List.exists (String.equal "drf_full: not_drf") lines)

let tests : unit Alcotest.test_case list =
  [
    ( "sat and unsat classifications",
      `Quick,
      test_sat_unsat_solver_classifications_are_distinct );
    ( "timeout and unknown classifications",
      `Quick,
      test_timeout_and_unknown_reasons_are_not_collapsed );
    ( "S488 classification labels",
      `Quick,
      test_s488_classification_labels_follow_solver_verdict );
    ( "S489 frontier classification",
      `Quick,
      test_s489_frontier_classifies_solve_tri_and_blocks_unbuilt_families );
    ( "S489 no-report frontier",
      `Quick,
      test_s489_frontier_without_symbolic_report_marks_all_blocked );
    ( "S491 blocker-retirement frontier",
      `Quick,
      test_s491_blocker_retirement_frontier_targets_launch_template );
    ( "transitive numeric constant normalization",
      `Quick,
      test_solver_normalizes_transitive_numeric_constants );
    ( "symbolic K proof needs executable guard",
      `Quick,
      test_symbolic_k_goal_requires_executable_upper_guard );
    ( "unsupported memory boundary",
      `Quick,
      test_unsupported_memory_boundary_is_not_solver_unknown );
    ( "loop protocol repeated-site boundary",
      `Quick,
      test_loop_protocol_replaces_only_repeated_site_boundary );
    ( "loop protocol racy verdict",
      `Quick,
      test_loop_protocol_preserves_racy_verdict );
    ( "symbolic obligation evidence",
      `Quick,
      test_symbolic_obligation_evidence_is_deterministic );
    ( "reserved pre-solver classification",
      `Quick,
      test_pre_solver_unsat_is_reserved_but_not_a_rule );
    ( "contradictory path-condition pre-solver discharge",
      `Quick,
      test_pre_solver_discharges_contradictory_path_conditions );
    ( "task-local path-condition no-match",
      `Quick,
      test_pre_solver_keeps_task_local_complements_solver_visible );
    ( "changed path-condition shape no-match",
      `Quick,
      test_pre_solver_keeps_changed_path_condition_shape_visible );
    ( "one-dimensional strided pre-solver discharge",
      `Quick,
      test_pre_solver_discharges_one_dimensional_strided_owner );
    ( "changed-shape no-match reaches solver",
      `Quick,
      test_pre_solver_leaves_changed_shape_solver_visible );
    ( "multidimensional no-match",
      `Quick,
      test_pre_solver_rejects_multidimensional_block );
    ( "invalid stride no-match",
      `Quick,
      test_pre_solver_rejects_invalid_stride_bound );
    ( "alpha-renamed strided ownership",
      `Quick,
      test_pre_solver_is_not_tied_to_fixture_variable_names );
    ( "affine strided ownership with global offset",
      `Quick,
      test_pre_solver_discharges_affine_strided_owner_with_global_offset );
    ( "affine strided ownership rejects task-local offset",
      `Quick,
      test_pre_solver_keeps_affine_strided_owner_with_local_offset_visible );
    ( "affine strided ownership rejects changed offset",
      `Quick,
      test_pre_solver_keeps_affine_strided_owner_with_changed_offset_visible );
    ( "affine strided ownership alpha-renamed discharge",
      `Quick,
      test_pre_solver_affine_strided_alpha_renaming );
    ( "subgroup-row fixed-lane no-match",
      `Quick,
      test_pre_solver_keeps_subgroup_row_fixed_lane_visible );
    ( "lane-vector without row-stride no-match",
      `Quick,
      test_pre_solver_keeps_lane_vector_without_row_stride_visible );
    ( "lane-vector row-stride component discharge",
      `Quick,
      test_pre_solver_discharges_lane_vector_row_stride_component );
    ( "lane-vector same component no-match",
      `Quick,
      test_pre_solver_keeps_lane_vector_same_component_visible );
    ( "lane-vector missing globals no-match",
      `Quick,
      test_pre_solver_keeps_lane_vector_without_global_stride_visible );
    ( "lane-vector missing bound no-match",
      `Quick,
      test_pre_solver_rejects_lane_vector_missing_bound );
    ( "lane-vector alpha-renamed discharge",
      `Quick,
      test_pre_solver_lane_vector_alpha_renaming );
    ( "lane-vector actual-like aliases",
      `Quick,
      test_pre_solver_lane_vector_actual_like_aliases );
    ( "lane-vector projected goal aliases",
      `Quick,
      test_pre_solver_lane_vector_projected_goal_aliases );
    ( "lane-vector location-insensitive names",
      `Quick,
      test_pre_solver_lane_vector_location_insensitive_names );
    ( "uniformity UB component",
      `Quick,
      test_uniformity_ub_remains_separate_from_memory_drf );
  ]

let () = Alcotest.run "Subgroup_solver" [ ("subgroup_solver", tests) ]
