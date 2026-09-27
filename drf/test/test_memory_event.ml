open Stage0
open Protocols
module Flatacc = Drf.Flatacc
module Ordinary_solver = Drf.Ordinary_solver
module Subgroup_event = Drf.Memory_event.Subgroup_event
module Subgroup_obligation = Drf.Memory_event.Subgroup_obligation
module Symbexp = Drf.Symbexp
module SM = Inference.Subgroup_matrix
module SS = Inference.Subgroup_source

let var (name : string) : Variable.t = Variable.from_name name
let nvar (name : string) : Exp.nexp = Exp.Var (var name)

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

let owned_range_kernel ?(array = var "dst") ?(owner = var "t")
    ?(index = Exp.Var (var "t")) ?(extra_exact = Variable.Set.empty) () :
    Flatacc.Kernel.t =
  {
    name = "owned_range";
    array_name = Variable.name array;
    approx_local_variables = Variable.Set.empty;
    exact_local_variables =
      Variable.Set.add owner
        (Variable.Set.add Variable.tid_x
           (Variable.Set.add Variable.tid_y
              (Variable.Set.add Variable.tid_z extra_exact)));
    code =
      [
        cond_access
          ~access:(Access.write array [ index ] None)
          ~cond:
            (Exp.n_eq
               (Exp.n_mod
                  (Exp.n_minus (Exp.Var owner) (Exp.Var Variable.tid_x))
                  (Exp.Num 64))
               (Exp.Num 0));
      ];
    pre = Exp.Bool true;
    runtime = Exp.Bool true;
  }

let expect_substring ~(label : string) ~(needle : string) (haystack : string) :
    unit =
  if not (Common.contains ~substring:needle haystack) then
    Alcotest.failf "%s: expected %S in %S" label needle haystack

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
    ?(memory_conditions = []) ?(uniform_vars = Variable.Set.empty)
    ?(numeric_aliases = Variable.Map.empty) site_id : SS.site_control =
  let source_order = Option.value source_order ~default:site_id in
  {
    site_id;
    source_order;
    conditions;
    memory_conditions;
    uniform_vars;
    numeric_aliases;
  }

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

let subgroup_collective (site_id : int) : SM.Stmt.t =
  let site = SM.Site.make ~label:"ballot" site_id in
  let collective =
    SM.Collective.make site
      (SM.Collective.Ballot_payload
         { result = var "ballot_result"; predicate = None })
  in
  SM.Stmt.Subgroup_collective collective

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
    launch_precondition = Exp.Bool true;
    launch_dimensions = Variable.Map.empty;
  }

let test_ordinary_goal_covers_projection_and_ordering_constraints () : unit =
  let kernel = sample_kernel () in
  let proof = Symbexp.Proof.from_flat Architecture.Block 8 kernel in
  let goal = Exp.b_to_string (Formula.to_bexp proof.formula) in
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

let expect_ordinary_pre_solver_discharge ~(reason : string)
    (classification : Ordinary_solver.classification option) : unit =
  match classification with
  | Some (Ordinary_solver.Pre_solver_unsat actual) ->
      Alcotest.(check string) "reason" reason actual
  | None -> Alcotest.fail ("expected ordinary pre-solver discharge: " ^ reason)

let expect_ordinary_pre_solver_none (label : string)
    (classification : Ordinary_solver.classification option) : unit =
  match classification with
  | None -> ()
  | Some classification ->
      Alcotest.fail
        (label ^ " unexpectedly discharged as "
        ^ Ordinary_solver.classification_to_string classification)

let test_ordinary_pre_solver_discharges_direct_strided_owner () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let proof =
    owned_range_kernel () |> Symbexp.Proof.from_flat Architecture.Block 11
  in
  Ordinary_solver.pre_solver_classification ~block_dim proof
  |> expect_ordinary_pre_solver_discharge
       ~reason:"one-dimensional strided thread ownership"

let test_ordinary_pre_solver_discharges_affine_global_offset_owner () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let c = var "C" in
  let t = var "t" in
  let proof =
    owned_range_kernel ~index:(Exp.n_minus (Exp.Var t) (Exp.Var c)) ()
    |> Symbexp.Proof.from_flat Architecture.Block 12
  in
  Ordinary_solver.pre_solver_classification ~block_dim proof
  |> expect_ordinary_pre_solver_discharge
       ~reason:"one-dimensional strided thread ownership"

let test_ordinary_pre_solver_keeps_affine_local_offset_visible () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let c = var "C" in
  let t = var "t" in
  let proof =
    owned_range_kernel
      ~index:(Exp.n_minus (Exp.Var t) (Exp.Var c))
      ~extra_exact:(Variable.Set.singleton c) ()
    |> Symbexp.Proof.from_flat Architecture.Block 13
  in
  Ordinary_solver.pre_solver_classification ~block_dim proof
  |> expect_ordinary_pre_solver_none "ordinary affine local offset"

let test_ordinary_pre_solver_keeps_changed_owner_shape_visible () : unit =
  let block_dim = Dim3.make ~x:64 () in
  let proof =
    owned_range_kernel ~index:(Exp.Var (var "other_t")) ()
    |> Symbexp.Proof.from_flat Architecture.Block 14
  in
  Ordinary_solver.pre_solver_classification ~block_dim proof
  |> expect_ordinary_pre_solver_none "ordinary changed owner shape"

let test_ordinary_pre_solver_rejects_multidimensional_block () : unit =
  let block_dim = Dim3.make ~x:64 ~y:2 () in
  let proof =
    owned_range_kernel () |> Symbexp.Proof.from_flat Architecture.Block 15
  in
  Ordinary_solver.pre_solver_classification ~block_dim proof
  |> expect_ordinary_pre_solver_none "ordinary multidimensional block"

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
  let actual = Symbexp.Proof.from_flat Architecture.Block 9 kernel in
  let goal = Exp.b_to_string (Formula.to_bexp actual.formula) in
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
          subgroup_collective 3;
          matrix_load 4;
          SM.Stmt.workgroup_barrier (SM.Site.make ~label:"syncthreads" 5);
          matrix_store 6;
        ]
  in
  let unified =
    Subgroup_event.from_subgroup_kernel kernel |> expect_subgroup_event_ok
  in
  let memories = Subgroup_event.memory_events unified in
  let boundaries = Subgroup_event.boundary_events unified in
  Alcotest.(check int)
    "matrix collectives add no memory events" 0 (List.length memories);
  Alcotest.(check int)
    "six collective/barrier events" 6 (List.length boundaries);
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
    "workgroup boundary before phase" "W0/S[2]"
    (Subgroup_event.Phase.to_string workgroup.phase_before);
  Alcotest.(check string)
    "workgroup boundary after phase" "W1/S[2]"
    (Subgroup_event.Phase.to_string workgroup.phase_after);
  let collective =
    match
      boundaries
      |> List.find_opt (fun (boundary : Subgroup_event.boundary) ->
          match boundary.kind with
          | Subgroup_event.Subgroup_collective -> true
          | _ -> false)
    with
    | Some boundary -> boundary
    | None -> Alcotest.fail "expected subgroup collective event"
  in
  Alcotest.(check string)
    "collective leaves phase unchanged"
    (Subgroup_event.Phase.to_string collective.phase_before)
    (Subgroup_event.Phase.to_string collective.phase_after)

let test_matrix_collective_is_participation_only () : unit =
  let warp_id = var "warp_id" in
  let site_controls =
    [
      source_site_control 40 ~source_order:20
        ~conditions:[ Exp.n_gt (Exp.Var Variable.tid_x) (Exp.Num 0) ]
        ~uniform_vars:(Variable.Set.singleton warp_id);
    ]
  in
  let kernel =
    subgroup_kernel "matrix_event" ~site_controls ~body:[ matrix_store 40 ]
  in
  let unified =
    Subgroup_event.from_subgroup_kernel kernel |> expect_subgroup_event_ok
  in
  Alcotest.(check int)
    "no matrix memory event" 0
    (List.length (Subgroup_event.memory_events unified));
  let boundary =
    match Subgroup_event.boundary_events unified with
    | [ boundary ] -> boundary
    | _ -> Alcotest.fail "expected one matrix boundary event"
  in
  Alcotest.(check string)
    "boundary kind" "matrix_collective:store_matrix_sync"
    (Subgroup_event.boundary_kind_to_string boundary.kind);
  Alcotest.(check int)
    "boundary control count" 1
    (List.length boundary.control_conditions);
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

let expect_repeated_site_unsupported ~(label : string) (stmt : SM.Stmt.t) : unit
    =
  let kernel = subgroup_kernel label ~body:[ stmt ] in
  match Subgroup_event.from_subgroup_kernel kernel with
  | Ok _ -> Alcotest.fail (label ^ " unexpectedly accepted a repeating site")
  | Error error ->
      let message = Subgroup_event.error_to_string error in
      expect_substring ~label ~needle:"repeating site" message;
      expect_substring ~label ~needle:"dynamic invocation matching" message

let test_subgroup_event_rejects_repeated_memory_barrier () : unit =
  SM.Site.make ~label:"syncwarp" ~may_repeat:true 7
  |> SM.Stmt.subgroup_barrier
  |> expect_repeated_site_unsupported ~label:"repeated_barrier"

let test_subgroup_event_accepts_repeated_matrix_collective () : unit =
  let site = SM.Site.make ~label:"store_matrix_sync" ~may_repeat:true 8 in
  let collective =
    SM.Matrix.store_matrix_sync site (rectangular (write_access ()))
    |> expect_ok
  in
  let kernel =
    subgroup_kernel "repeated_matrix_store"
      ~body:[ SM.Stmt.Matrix_collective collective ]
  in
  let unified =
    Subgroup_event.from_subgroup_kernel kernel |> expect_subgroup_event_ok
  in
  Alcotest.(check int)
    "no matrix memory events" 0
    (List.length (Subgroup_event.memory_events unified));
  Alcotest.(check int)
    "one participation boundary" 1
    (List.length (Subgroup_event.boundary_events unified))

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
    ( "ordinary goal constraints",
      `Quick,
      test_ordinary_goal_covers_projection_and_ordering_constraints );
    ( "ordinary direct strided ownership discharge",
      `Quick,
      test_ordinary_pre_solver_discharges_direct_strided_owner );
    ( "ordinary affine global-offset ownership discharge",
      `Quick,
      test_ordinary_pre_solver_discharges_affine_global_offset_owner );
    ( "ordinary affine local-offset ownership no-match",
      `Quick,
      test_ordinary_pre_solver_keeps_affine_local_offset_visible );
    ( "ordinary changed owner shape no-match",
      `Quick,
      test_ordinary_pre_solver_keeps_changed_owner_shape_visible );
    ( "ordinary multidimensional ownership no-match",
      `Quick,
      test_ordinary_pre_solver_rejects_multidimensional_block );
    ( "read/read mode conflict shape",
      `Quick,
      test_read_read_ordinary_mode_conflict_remains_unsat_shape );
    ( "subgroup event phase domains",
      `Quick,
      test_subgroup_event_stream_preserves_phase_domains );
    ( "matrix collective is participation only",
      `Quick,
      test_matrix_collective_is_participation_only );
    ( "subgroup ordinary event source metadata",
      `Quick,
      test_subgroup_ordinary_event_preserves_source_metadata );
    ( "subgroup event missing config",
      `Quick,
      test_subgroup_event_requires_explicit_target_config );
    ( "subgroup event ordinary config mismatch",
      `Quick,
      test_subgroup_event_rejects_ordinary_target_config_mismatch );
    ( "subgroup event repeated memory barrier",
      `Quick,
      test_subgroup_event_rejects_repeated_memory_barrier );
    ( "subgroup event repeated matrix collective",
      `Quick,
      test_subgroup_event_accepts_repeated_matrix_collective );
    ( "subgroup obligations match direct owner API",
      `Quick,
      test_subgroup_obligations_match_direct_owner_api );
  ]

let () = Alcotest.run "Memory_event" [ ("memory_event", tests) ]
