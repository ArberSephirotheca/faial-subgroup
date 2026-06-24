open Stage0
open Protocols
module Flatacc = Drf.Flatacc
module Locsplit = Drf.Locsplit
module Memory_event = Drf.Memory_event
module Subgroup_event = Drf.Memory_event.Subgroup_event
module Subgroup_obligation = Drf.Memory_event.Subgroup_obligation
module Symbexp = Drf.Symbexp
module Unsync = Drf.Unsync
module SM = Inference.Subgroup_matrix
module SS = Inference.Subgroup_source

let var (name : string) : Variable.t = Variable.from_name name
let nvar (name : string) : Exp.nexp = Exp.Var (var name)

let located_var ~(line : int) (name : string) : Variable.t =
  let location =
    Location.make ~filename:"test_memory_event.cu" ~line:(Index.from_base1 line)
      ~interval:(Interval.from_range ~start:(Index.from_base1 1) ~length:1)
  in
  Variable.make ~name ~location

let cond_access ~(access : Access.t) ~(cond : Exp.bexp) : Flatacc.CondAccess.t =
  { access; cond }

let write_access ?(array = var "buf") ?(index = nvar "i") () : Access.t =
  Access.write array [ index ] None

let read_access ?(array = var "buf") ?(index = nvar "i") () : Access.t =
  Access.read array [ index ]

let sample_kernel ?(array = var "buf") ?(name = "ordinary_kernel")
    ?(runtime = Exp.n_eq (nvar "runtime_flag") (Exp.Num 1))
    ?(pre = Exp.n_gt (nvar "limit") (Exp.Num 0))
    ?(exact = Variable.Set.of_list [ var "i"; var "guard"; var "runtime_flag" ])
    ?(approx = Variable.Set.of_list [ var "approx_idx" ]) () : Flatacc.Kernel.t
    =
  let code =
    [
      cond_access
        ~access:(write_access ~array ~index:(nvar "i") ())
        ~cond:(Exp.n_gt (nvar "guard") (Exp.Num 0));
      cond_access
        ~access:
          (read_access ~array
             ~index:(Exp.n_plus (nvar "approx_idx") (Exp.Num 1))
             ())
        ~cond:(Exp.n_lt (nvar "approx_idx") (Exp.Num 8));
    ]
  in
  {
    name;
    array_name = Variable.name array;
    approx_local_variables = approx;
    exact_local_variables = exact;
    code;
    pre;
    runtime;
  }

let phase_range_kernel () : Flatacc.Kernel.t =
  let t = var "t" in
  let dst = var "dst" in
  let range =
    Range.
      {
        var = t;
        ty = C_type.int;
        dir = Increase;
        lower_bound = Exp.Var Variable.tid_x;
        upper_bound = Exp.Num 127;
        step = Step.plus (Exp.Num 64);
      }
  in
  let kernel : Locsplit.Kernel.t =
    {
      name = "phase_range";
      array_name = "dst";
      global_variables = Params.empty;
      local_variables = Params.empty;
      ranges = [ range ];
      code = Unsync.Access (Access.write dst [ Exp.Var t ] None);
    }
  in
  match Flatacc.Kernel.from_loc_split Architecture.Block kernel with
  | Some kernel -> kernel
  | None -> Alcotest.fail "expected flat-access kernel"

let proof_accesses (proof : Symbexp.Proof.t) : string list =
  List.map Symbexp.AccessSummary.to_string proof.accesses

let labels_to_strings (labels : (string * string) list) : string list =
  List.map (fun (name, label) -> name ^ "=" ^ label) labels

let check_proof_equal (label : string) (expected : Symbexp.Proof.t)
    (actual : Symbexp.Proof.t) : unit =
  Alcotest.(check int) (label ^ " id") expected.id actual.id;
  Alcotest.(check string)
    (label ^ " kernel") expected.kernel_name actual.kernel_name;
  Alcotest.(check string)
    (label ^ " array") expected.array_name actual.array_name;
  Alcotest.(check string)
    (label ^ " goal")
    (Exp.b_to_string expected.goal)
    (Exp.b_to_string actual.goal);
  Alcotest.(check (list string)) (label ^ " decls") expected.decls actual.decls;
  Alcotest.(check (list string))
    (label ^ " labels")
    (labels_to_strings expected.labels)
    (labels_to_strings actual.labels);
  Alcotest.(check (list string))
    (label ^ " accesses") (proof_accesses expected) (proof_accesses actual)

let check_proof_stream_equal (label : string)
    (expected : Symbexp.Proof.t Streamutil.stream)
    (actual : Symbexp.Proof.t Streamutil.stream) : unit =
  let expected = Streamutil.to_list expected in
  let actual = Streamutil.to_list actual in
  Alcotest.(check int)
    (label ^ " proof count") (List.length expected) (List.length actual);
  let rec compare_proofs idx expected actual =
    match (expected, actual) with
    | [], [] -> ()
    | expected_proof :: expected_tail, actual_proof :: actual_tail ->
        check_proof_equal
          (Printf.sprintf "%s proof[%d]" label idx)
          expected_proof actual_proof;
        compare_proofs (idx + 1) expected_tail actual_tail
    | _ -> Alcotest.fail (label ^ " proof count mismatch")
  in
  compare_proofs 0 expected actual

let expect_substring ~(label : string) ~(needle : string) (haystack : string) :
    unit =
  Alcotest.(check bool) label true (Common.contains ~substring:needle haystack)

let expect_ok (type a) (result : (a, string) result) : a =
  match result with Ok value -> value | Error msg -> Alcotest.fail msg

let expect_subgroup_event_ok (type a)
    (result : (a, Subgroup_event.error) result) : a =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (Subgroup_event.error_to_string error)

let expect_subgroup_obligation_ok (type a)
    (result : (a, Subgroup_obligation.error) result) : a =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (Subgroup_obligation.error_to_string error)

let subgroup_config ?(size = 32) () : SM.Target_config.t =
  SM.Target_config.subgroup_size_exn size |> SM.Target_config.cuda_x_contiguous

let checked_block_dim ?(x = 64) ?(y = 1) ?(z = 1) () : Dim3.t =
  Dim3.make ~x ~y ~z ()

let source_site_control ?source_order ?(conditions = [])
    ?(memory_conditions = []) ?(uniform_vars = Variable.Set.empty) site_id :
    SS.site_control =
  let source_order = Option.value source_order ~default:site_id in
  { site_id; source_order; conditions; memory_conditions; uniform_vars }

let ordinary_site ?(id = 0) ?source_order ?(label = "ordinary") () :
    SS.ordinary_memory_site =
  let source_order = Option.value source_order ~default:id in
  { id; source_order; label; location = None }

let ordinary_phase ?(workgroup = 0) ?(subgroup = []) () :
    SS.ordinary_memory_phase =
  { workgroup; subgroup }

let ordinary_effect ?(kind = SS.Ordinary_write) ?(site = ordinary_site ())
    ?(access = write_access ()) ?(source_conditions = []) ?runtime_condition
    ?(phase = ordinary_phase ()) ?(target_config = subgroup_config ()) () :
    SS.ordinary_memory_effect =
  {
    kind;
    site;
    access;
    source_conditions;
    runtime_condition;
    phase;
    target_config;
  }

let rectangular (access : Access.t) : SM.Matrix.footprint =
  SM.Matrix.rectangular ~base:access ~rows:(Exp.Num 16) ~cols:(Exp.Num 8)
    ~leading_dimension:(Exp.Num 32) ~layout:SM.Matrix.Row_major ~row:(var "row")
    ~col:(var "col")
  |> expect_ok

let matrix_store (site_id : int) : SM.Stmt.t =
  let site = SM.Site.make ~label:"store_matrix_sync" site_id in
  let collective =
    SM.Matrix.store_matrix_sync site (rectangular (write_access ()))
    |> expect_ok
  in
  SM.Stmt.Matrix_collective collective

let matrix_load (site_id : int) : SM.Stmt.t =
  let site = SM.Site.make ~label:"load_matrix_sync" site_id in
  let collective =
    SM.Matrix.load_matrix_sync site (rectangular (read_access ())) |> expect_ok
  in
  SM.Stmt.Matrix_collective collective

let subgroup_kernel ?(target_config = subgroup_config ()) ?(body = [])
    ?(site_controls = []) ?(uniform_vars = Variable.Set.empty)
    ?(memory_globals = Variable.Set.empty) ?(ordinary_memory_effects = [])
    (name : string) : SS.subgroup_kernel =
  let matrix_kernel = SM.Kernel.make ~target_config ~name body in
  {
    matrix_kernel;
    site_controls;
    uniform_vars;
    memory_globals;
    ordinary_memory_effects;
  }

let test_ordinary_obligation_matches_symbexp_proof () : unit =
  let kernel = sample_kernel () in
  let actual =
    Memory_event.Ordinary_obligation.from_flat Architecture.Block 5 kernel
  in
  let expected = Symbexp.Proof.from_flat Architecture.Block 5 kernel in
  check_proof_equal "ordinary obligation" expected actual

let test_ordinary_sanity_check_matches_symbexp_without_index_assignment () :
    unit =
  let kernel = sample_kernel () in
  let actual =
    Memory_event.Ordinary_obligation.from_flat ~assign_index:false
      Architecture.Block 6 kernel
  in
  let expected =
    Symbexp.Proof.from_flat ~assign_index:false Architecture.Block 6 kernel
  in
  check_proof_equal "ordinary sanity check" expected actual

let sample_kernel_stream () : Flatacc.Kernel.t Streamutil.stream =
  Streamutil.from_list
    [
      sample_kernel ~name:"ordinary_stream_a" ();
      sample_kernel ~name:"ordinary_stream_b" ~array:(var "other_buf")
        ~pre:(Exp.n_ge (nvar "limit") (Exp.Num 2))
        ();
    ]

let test_memory_event_translate_stream_matches_symbexp_translate () : unit =
  check_proof_stream_equal "memory event translate"
    (Symbexp.translate Architecture.Block (sample_kernel_stream ()))
    (Memory_event.translate Architecture.Block (sample_kernel_stream ()))

let test_memory_event_sanity_stream_matches_symbexp_sanity_check () : unit =
  check_proof_stream_equal "memory event sanity"
    (Symbexp.sanity_check Architecture.Block (sample_kernel_stream ()))
    (Memory_event.sanity_check Architecture.Block (sample_kernel_stream ()))

let test_ordinary_events_preserve_phase_location_and_origin () : unit =
  let array = located_var ~line:42 "located_buf" in
  let kernel = sample_kernel ~array () in
  let phase = Memory_event.Ordinary_phase.from_flat ~phase_id:7 kernel in
  Alcotest.(check int) "phase id" 7 phase.phase_id;
  Alcotest.(check string) "array name" "located_buf" phase.array_name;
  match phase.events with
  | first :: _ ->
      Alcotest.(check int) "event phase id" 7 first.phase_id;
      Alcotest.(check string)
        "event origin" "ordinary"
        (Memory_event.Ordinary_event.origin_to_string first.origin);
      Alcotest.(check int)
        "event source line" 42
        (Memory_event.Ordinary_event.location first
        |> Location.line |> Index.to_base1)
  | [] -> Alcotest.fail "expected ordinary events"

let test_ordinary_goal_covers_projection_and_ordering_constraints () : unit =
  let kernel = sample_kernel () in
  let proof =
    Memory_event.Ordinary_obligation.from_flat Architecture.Block 8 kernel
  in
  let goal = Exp.b_to_string proof.goal in
  List.iter
    (fun (label, needle) -> expect_substring ~label ~needle goal)
    [
      ("precondition remains unprojected", "limit > 0");
      ("guard is projected for task 1", "guard$T1 > 0");
      ("runtime is projected for task 1", "runtime_flag$T1 == 1");
      ("index assignment projects task 1", "$T1$idx$0 == i$T1");
      ("index equality compares tasks", "$T1$idx$0 == $T2$idx$0");
      ("non-negative index guard", "$T1$idx$0 >= 0");
      ("access id ordering", "$T1$id <= $T2$id");
      ("mode variable for task 1", "$T1$mode");
      ("mode variable for task 2", "$T2$mode");
    ]

let test_phase_level_range_binder_is_task_local () : unit =
  let kernel = phase_range_kernel () in
  let t = var "t" in
  Alcotest.(check bool)
    "range binder is exact local" true
    (Variable.Set.mem t kernel.exact_local_variables);
  Alcotest.(check bool)
    "range binder is not approximate" false
    (Variable.Set.mem t kernel.approx_local_variables);
  Alcotest.(check string)
    "phase range moved out of global pre" "true"
    (Exp.b_to_string kernel.pre);
  let code = Flatacc.Code.to_list kernel.code in
  let access =
    match code with
    | [ access ] -> access
    | _ -> Alcotest.fail "expected one flattened access"
  in
  let range_fact =
    Exp.n_eq
      (Exp.n_mod
         (Exp.n_minus (Exp.Var t) (Exp.Var Variable.tid_x))
         (Exp.Num 64))
      (Exp.Num 0)
  in
  Alcotest.(check bool)
    "range condition is access-local" true
    (List.exists (( = ) range_fact) (Exp.b_and_split access.cond));
  let proof =
    Memory_event.Ordinary_obligation.from_flat Architecture.Block 10 kernel
  in
  let goal = Exp.b_to_string proof.goal in
  List.iter
    (fun (label, needle) -> expect_substring ~label ~needle goal)
    [
      ("task 1 range binder projected", "t$T1");
      ("task 2 range binder projected", "t$T2");
      ("task 1 thread base projected", "threadIdx.x$T1");
      ("task 2 thread base projected", "threadIdx.x$T2");
      ("task 1 index uses local t", "$T1$idx$0 == t$T1");
      ("task 2 index uses local t", "$T2$idx$0 == t$T2");
    ]

let test_read_read_ordinary_mode_conflict_remains_unsat_shape () : unit =
  let array = var "read_only" in
  let kernel =
    {
      (sample_kernel ~array ()) with
      code =
        [
          cond_access
            ~access:(read_access ~array ~index:(nvar "i") ())
            ~cond:Exp.b_true;
        ];
    }
  in
  let actual =
    Memory_event.Ordinary_obligation.from_flat Architecture.Block 9 kernel
  in
  let expected = Symbexp.Proof.from_flat Architecture.Block 9 kernel in
  check_proof_equal "read/read mode conflict" expected actual;
  let goal = Exp.b_to_string actual.goal in
  expect_substring ~label:"read event assigns read mode" ~needle:"$T1$mode == 0"
    goal;
  expect_substring ~label:"mode conflict excludes read/read"
    ~needle:"$T2$mode != 0" goal

let test_subgroup_event_stream_preserves_phase_domains () : unit =
  let kernel =
    subgroup_kernel "phase_events"
      ~body:
        [
          matrix_store 1;
          SM.Stmt.subgroup_barrier (SM.Site.make ~label:"syncwarp" 2);
          matrix_load 3;
          SM.Stmt.workgroup_barrier (SM.Site.make ~label:"syncthreads" 4);
          matrix_store 5;
        ]
  in
  let unified =
    Subgroup_event.from_subgroup_kernel kernel |> expect_subgroup_event_ok
  in
  let memories = Subgroup_event.memory_events unified in
  let boundaries = Subgroup_event.boundary_events unified in
  Alcotest.(check int) "three matrix memory events" 3 (List.length memories);
  Alcotest.(check int)
    "five subgroup/matrix boundaries" 5 (List.length boundaries);
  let first, second, third =
    match memories with
    | [ first; second; third ] -> (first, second, third)
    | _ -> Alcotest.fail "expected three matrix memory events"
  in
  Alcotest.(check string)
    "first memory phase" "W0/S[]"
    (Subgroup_event.Phase.to_string first.phase);
  Alcotest.(check string)
    "subgroup barrier advances only S" "W0/S[1;2]"
    (Subgroup_event.Phase.to_string second.phase);
  Alcotest.(check string)
    "workgroup barrier advances W" "W1/S[1;2;3]"
    (Subgroup_event.Phase.to_string third.phase);
  let workgroup =
    match
      boundaries
      |> List.find_opt (fun (boundary : Subgroup_event.boundary) ->
          match boundary.kind with
          | Subgroup_event.Workgroup_barrier -> true
          | _ -> false)
    with
    | Some boundary -> boundary
    | None -> Alcotest.fail "expected workgroup boundary"
  in
  Alcotest.(check string)
    "workgroup boundary before phase" "W0/S[1;2;3]"
    (Subgroup_event.Phase.to_string workgroup.phase_before);
  Alcotest.(check string)
    "workgroup boundary after phase" "W1/S[1;2;3]"
    (Subgroup_event.Phase.to_string workgroup.phase_after)

let test_subgroup_matrix_event_preserves_footprint_and_controls () : unit =
  let warp_id = var "warp_id" in
  let site_controls =
    [
      source_site_control 40 ~source_order:20
        ~conditions:[ Exp.n_gt (Exp.Var Variable.tid_x) (Exp.Num 0) ]
        ~memory_conditions:
          [ Exp.n_eq (Exp.Var (var "base")) (Exp.Var Variable.tid_x) ]
        ~uniform_vars:(Variable.Set.singleton warp_id);
    ]
  in
  let kernel =
    subgroup_kernel "matrix_event" ~site_controls ~body:[ matrix_store 40 ]
  in
  let unified =
    Subgroup_event.from_subgroup_kernel kernel |> expect_subgroup_event_ok
  in
  let memory =
    match Subgroup_event.memory_events unified with
    | [ memory ] -> memory
    | _ -> Alcotest.fail "expected one matrix memory event"
  in
  let boundary =
    match Subgroup_event.boundary_events unified with
    | [ boundary ] -> boundary
    | _ -> Alcotest.fail "expected one matrix boundary event"
  in
  Alcotest.(check string)
    "matrix origin" "matrix_store"
    (Subgroup_event.memory_origin_to_string memory.origin);
  Alcotest.(check (option int))
    "matrix source order" (Some 20) memory.source_order;
  Alcotest.(check (option int))
    "matrix site" (Some 40)
    (Option.map SM.Site.id memory.matrix_site);
  begin match memory.footprint with
  | Some
      (SM.Matrix.Rectangular { rows = Exp.Num 16; cols = Exp.Num 8; layout; _ })
    ->
      Alcotest.(check string)
        "rectangular layout" "row_major"
        (SM.Matrix.layout_to_string layout)
  | Some footprint ->
      Alcotest.fail
        ("expected rectangular footprint, got "
        ^ SM.Matrix.footprint_to_string footprint)
  | None -> Alcotest.fail "expected matrix footprint"
  end;
  let condition = Exp.b_to_string memory.condition in
  expect_substring ~label:"memory condition is carried"
    ~needle:"base == threadIdx.x" condition;
  expect_substring ~label:"row bounds are carried" ~needle:"row < 16" condition;
  Alcotest.(check string)
    "boundary kind" "matrix_collective:store_matrix_sync"
    (Subgroup_event.boundary_kind_to_string boundary.kind);
  Alcotest.(check int)
    "boundary control count" 1
    (List.length boundary.control_conditions);
  Alcotest.(check int)
    "boundary memory-control count" 1
    (List.length boundary.memory_conditions);
  Alcotest.(check bool)
    "boundary carries uniform vars" true
    (Variable.Set.mem warp_id boundary.uniform_vars)

let test_subgroup_ordinary_event_preserves_source_metadata () : unit =
  let memory_globals = Variable.Set.singleton (var "params") in
  let uniform_vars = Variable.Set.singleton (var "warp_id") in
  let ordinary =
    ordinary_effect ~kind:SS.Ordinary_read
      ~site:(ordinary_site ~id:7 ~source_order:10 ~label:"ordinary_read" ())
      ~access:(read_access ~array:(var "dst") ~index:(nvar "i") ())
      ~source_conditions:[ Exp.n_lt (nvar "i") (Exp.Num 16) ]
      ~runtime_condition:(Exp.n_eq (nvar "runtime_flag") (Exp.Num 1))
      ~phase:(ordinary_phase ~workgroup:2 ~subgroup:[ 10 ] ())
      ()
  in
  let kernel =
    subgroup_kernel "ordinary_event" ~memory_globals ~uniform_vars
      ~ordinary_memory_effects:[ ordinary ]
  in
  let unified =
    Subgroup_event.from_subgroup_kernel kernel |> expect_subgroup_event_ok
  in
  let memory =
    match Subgroup_event.memory_events unified with
    | [ memory ] -> memory
    | _ -> Alcotest.fail "expected one ordinary memory event"
  in
  Alcotest.(check string)
    "ordinary origin" "ordinary_read"
    (Subgroup_event.memory_origin_to_string memory.origin);
  Alcotest.(check (option int))
    "ordinary source order" (Some 10) memory.source_order;
  Alcotest.(check string)
    "ordinary phase" "W2/S[10]"
    (Subgroup_event.Phase.to_string memory.phase);
  Alcotest.(check bool)
    "memory globals carried" true
    (Variable.Set.mem (var "params") unified.memory_globals);
  Alcotest.(check bool)
    "uniform vars carried" true
    (Variable.Set.mem (var "warp_id") unified.uniform_vars);
  Alcotest.(check (option string))
    "ordinary has no matrix site" None
    (Option.map SM.Site.to_string memory.matrix_site);
  Alcotest.(check (option string))
    "ordinary has no matrix footprint" None
    (Option.map SM.Matrix.footprint_to_string memory.footprint);
  let condition = Exp.b_to_string memory.condition in
  expect_substring ~label:"ordinary source guard carried" ~needle:"i < 16"
    condition;
  expect_substring ~label:"ordinary runtime carried" ~needle:"runtime_flag == 1"
    condition

let test_subgroup_event_requires_explicit_target_config () : unit =
  let kernel =
    subgroup_kernel "missing_config"
      ~target_config:SM.Target_config.missing_cuda
      ~body:[ SM.Stmt.subgroup_barrier (SM.Site.make ~label:"syncwarp" 1) ]
  in
  match Subgroup_event.from_subgroup_kernel kernel with
  | Ok _ -> Alcotest.fail "missing subgroup target config unexpectedly worked"
  | Error error ->
      expect_substring ~label:"missing target config error"
        ~needle:"needs explicit subgroup target configuration"
        (Subgroup_event.error_to_string error)

let test_subgroup_event_rejects_ordinary_target_config_mismatch () : unit =
  let ordinary =
    ordinary_effect ~target_config:(subgroup_config ~size:16 ()) ()
  in
  let kernel =
    subgroup_kernel "target_mismatch"
      ~target_config:(subgroup_config ~size:32 ())
      ~ordinary_memory_effects:[ ordinary ]
  in
  match Subgroup_event.from_subgroup_kernel kernel with
  | Ok _ -> Alcotest.fail "ordinary target-config mismatch unexpectedly worked"
  | Error error ->
      expect_substring ~label:"target config mismatch error"
        ~needle:"target configuration mismatch"
        (Subgroup_event.error_to_string error)

let test_subgroup_obligations_match_direct_owner_api () : unit =
  let memory_globals = Variable.Set.singleton (var "params") in
  let site_controls =
    [
      source_site_control 40 ~source_order:20
        ~memory_conditions:
          [ Exp.n_eq (Exp.Var (var "base")) (Exp.Var Variable.tid_x) ];
    ]
  in
  let ordinary =
    ordinary_effect ~kind:SS.Ordinary_read
      ~access:(read_access ~array:(var "tile") ~index:(nvar "base") ())
      ~source_conditions:[ Exp.n_lt (nvar "base") (Exp.Num 16) ]
      ()
  in
  let kernel =
    subgroup_kernel "unified_obligation" ~memory_globals ~site_controls
      ~ordinary_memory_effects:[ ordinary ]
      ~body:[ matrix_store 40 ]
  in
  let block_dim = checked_block_dim () in
  let unified =
    Subgroup_event.from_subgroup_kernel kernel |> expect_subgroup_event_ok
  in
  let unified_obligations =
    Subgroup_obligation.obligations_of_events ~globals:memory_globals ~block_dim
      unified
    |> expect_subgroup_obligation_ok
  in
  let direct_owner_obligations =
    Subgroup_obligation.obligations ~globals:memory_globals ~block_dim
      ~site_controls ~ordinary_memory_effects:[ ordinary ] kernel.matrix_kernel
    |> expect_subgroup_obligation_ok
  in
  Alcotest.(check (list string))
    "unified obligations match direct owner API"
    (List.map Subgroup_obligation.obligation_to_string direct_owner_obligations)
    (List.map Subgroup_obligation.obligation_to_string unified_obligations)

let tests : unit Alcotest.test_case list =
  [
    ( "ordinary obligation matches symbexp",
      `Quick,
      test_ordinary_obligation_matches_symbexp_proof );
    ( "ordinary sanity check matches symbexp",
      `Quick,
      test_ordinary_sanity_check_matches_symbexp_without_index_assignment );
    ( "memory event translate stream matches symbexp",
      `Quick,
      test_memory_event_translate_stream_matches_symbexp_translate );
    ( "memory event sanity stream matches symbexp",
      `Quick,
      test_memory_event_sanity_stream_matches_symbexp_sanity_check );
    ( "ordinary events preserve phase and location",
      `Quick,
      test_ordinary_events_preserve_phase_location_and_origin );
    ( "ordinary goal constraints",
      `Quick,
      test_ordinary_goal_covers_projection_and_ordering_constraints );
    ( "phase range binder is task-local",
      `Quick,
      test_phase_level_range_binder_is_task_local );
    ( "read/read mode conflict shape",
      `Quick,
      test_read_read_ordinary_mode_conflict_remains_unsat_shape );
    ( "subgroup event phase domains",
      `Quick,
      test_subgroup_event_stream_preserves_phase_domains );
    ( "subgroup matrix event footprint and controls",
      `Quick,
      test_subgroup_matrix_event_preserves_footprint_and_controls );
    ( "subgroup ordinary event source metadata",
      `Quick,
      test_subgroup_ordinary_event_preserves_source_metadata );
    ( "subgroup event missing config",
      `Quick,
      test_subgroup_event_requires_explicit_target_config );
    ( "subgroup event ordinary config mismatch",
      `Quick,
      test_subgroup_event_rejects_ordinary_target_config_mismatch );
    ( "subgroup obligations match direct owner API",
      `Quick,
      test_subgroup_obligations_match_direct_owner_api );
  ]

let () = Alcotest.run "Memory_event" [ ("memory_event", tests) ]
