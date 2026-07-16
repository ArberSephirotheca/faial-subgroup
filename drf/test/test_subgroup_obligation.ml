open Protocols
module LC = Drf.Launch_contract
module LCG = Drf.Launch_contract_generator
module SM = Inference.Subgroup_matrix
module Source = Inference.Subgroup_source
module Memory = Drf.Memory_event.Subgroup_obligation
module Symbolic_launch_evidence = Drf.Symbolic_launch_evidence

let var (name : string) : Variable.t = Variable.from_name name

let expect_ok (type a) (result : (a, string) result) : a =
  match result with Ok value -> value | Error msg -> Alcotest.fail msg

let expect_memory_ok (type a) (result : (a, Memory.error) result) : a =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (Memory.error_to_string error)

let expect_launch_contract_ok (type a) (result : (a, LC.error) result) : a =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (LC.error_to_string error)

let write_optional_symbolic_obligation_artifact (obligation : Memory.obligation)
    : unit =
  match Sys.getenv_opt "FAIAL_S437_SYMBOLIC_OBLIGATION_OUT" with
  | None -> ()
  | Some path ->
      let out_channel = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr out_channel)
        (fun () ->
          output_string out_channel (Memory.obligation_to_string obligation);
          output_char out_channel '\n')

let subgroup_config () : SM.Target_config.t =
  SM.Target_config.subgroup_size_exn 32 |> SM.Target_config.cuda_x_contiguous

let base_access ?(mode = `Write) ?(array = "tile") () : Access.t =
  match mode with
  | `Read -> Access.read (var array) [ Exp.Var (var "base") ]
  | `Write -> Access.write (var array) [ Exp.Var (var "base") ] None

let ordinary_site ?(id = 0) ?source_order ?(label = "ordinary_write") () :
    Source.ordinary_memory_site =
  let source_order = Option.value source_order ~default:id in
  { id; source_order; label; location = None }

let ordinary_phase ?(workgroup = 0) ?(subgroup = []) () :
    Source.ordinary_memory_phase =
  { workgroup; subgroup }

let ordinary_effect ?(kind = Source.Ordinary_write) ?(site = ordinary_site ())
    ?(access = base_access ()) ?(source_conditions = []) ?runtime_condition
    ?(phase = ordinary_phase ()) ?(target_config = subgroup_config ()) () :
    Source.ordinary_memory_effect =
  {
    kind;
    site;
    access;
    source_conditions;
    runtime_condition;
    phase;
    target_config;
  }

let source_site_control ?source_order ?(conditions = [])
    ?(memory_conditions = []) ?(uniform_vars = Variable.Set.empty)
    ?(numeric_aliases = Variable.Map.empty) site_id : Source.site_control =
  let source_order = Option.value source_order ~default:site_id in
  {
    site_id;
    source_order;
    conditions;
    memory_conditions;
    uniform_vars;
    numeric_aliases;
  }

let rectangular (access : Access.t) : SM.Matrix.footprint =
  SM.Matrix.rectangular ~base:access ~rows:(Exp.Num 16) ~cols:(Exp.Num 8)
    ~leading_dimension:(Exp.Num 32) ~layout:SM.Matrix.Row_major ~row:(var "row")
    ~col:(var "col")
  |> expect_ok

let checked_block_dim ?(x = 64) ?(y = 1) ?(z = 1) () : Dim3.t =
  Dim3.make ~x ~y ~z ()

let variable_set (names : string list) : Variable.Set.t =
  List.fold_left
    (fun vars name -> Variable.Set.add (var name) vars)
    Variable.Set.empty names

let matrix_goal_assignments ~(block_dim : Dim3.t) ~(t1 : Dim3.t) ~(t2 : Dim3.t)
    : (string * int) list =
  [
    ("blockIdx.x", 0);
    ("blockIdx.y", 0);
    ("blockIdx.z", 0);
    ("gridDim.x", 1);
    ("gridDim.y", 1);
    ("gridDim.z", 1);
    ("blockDim.x", block_dim.x);
    ("blockDim.y", block_dim.y);
    ("blockDim.z", block_dim.z);
    ("threadIdx.x$T1", t1.x);
    ("threadIdx.y$T1", t1.y);
    ("threadIdx.z$T1", t1.z);
    ("threadIdx.x$T2", t2.x);
    ("threadIdx.y$T2", t2.y);
    ("threadIdx.z$T2", t2.z);
    ("base$T1", 0);
    ("base$T2", 0);
    ("row$T1", 0);
    ("row$T2", 0);
    ("col$T1", 0);
    ("col$T2", 0);
  ]

let thread_goal_assignments ~(block_dim : Dim3.t) ~(t1 : Dim3.t) ~(t2 : Dim3.t)
    : (string * int) list =
  [
    ("blockIdx.x", 0);
    ("blockIdx.y", 0);
    ("blockIdx.z", 0);
    ("gridDim.x", 1);
    ("gridDim.y", 1);
    ("gridDim.z", 1);
    ("blockDim.x", block_dim.x);
    ("blockDim.y", block_dim.y);
    ("blockDim.z", block_dim.z);
    ("threadIdx.x$T1", t1.x);
    ("threadIdx.y$T1", t1.y);
    ("threadIdx.z$T1", t1.z);
    ("threadIdx.x$T2", t2.x);
    ("threadIdx.y$T2", t2.y);
    ("threadIdx.z$T2", t2.z);
  ]

let eval_goal (assignments : (string * int) list) (goal : Exp.bexp) : bool =
  let subst =
    assignments
    |> List.map (fun (name, value) -> (name, Exp.Num value))
    |> Subst.SubstAssoc.make
  in
  let grounded = Subst.ReplaceAssoc.b_subst subst goal in
  match Exp.b_eval_res grounded with
  | Ok value -> value
  | Error error ->
      Alcotest.fail
        (Printf.sprintf "failed to evaluate %s: %s" (Exp.b_to_string grounded)
           error)

let matrix_store (site_id : int) : SM.Stmt.t =
  let site = SM.Site.make ~label:"store_matrix_sync" site_id in
  let collective =
    SM.Matrix.store_matrix_sync site (rectangular (base_access ())) |> expect_ok
  in
  SM.Stmt.Matrix_collective collective

let matrix_load (site_id : int) : SM.Stmt.t =
  let site = SM.Site.make ~label:"load_matrix_sync" site_id in
  let collective =
    SM.Matrix.load_matrix_sync site (rectangular (base_access ~mode:`Read ()))
    |> expect_ok
  in
  SM.Stmt.Matrix_collective collective

let single_access (site_id : int) : Memory.conditional_access =
  let phased =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"single"
      [ matrix_store site_id ]
    |> Memory.phases_of_kernel
  in
  match phased.phases with
  | [ { accesses = [ access ]; _ } ] -> access
  | _ -> Alcotest.fail "expected one matrix memory access"

let single_store_obligation ~(block_dim : Dim3.t) () : Memory.obligation =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"obligation"
      [ matrix_store 40 ]
  in
  match Memory.obligations ~block_dim kernel |> expect_memory_ok with
  | [ obligation ] -> obligation
  | _ -> Alcotest.fail "expected one self-pair matrix obligation"

let ordinary_threadidx_self_obligation ~(block_dim : Dim3.t) () :
    Memory.obligation =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"ordinary_domain"
      []
  in
  let ordinary =
    ordinary_effect
      ~access:(Access.write (var "tile") [ Exp.Var Variable.tid_x ] None)
      ()
  in
  match
    Memory.obligations ~block_dim ~ordinary_memory_effects:[ ordinary ] kernel
    |> expect_memory_ok
  with
  | [ obligation ] -> obligation
  | obligations ->
      Alcotest.fail
        (Printf.sprintf "expected one ordinary self-pair obligation, got %d"
           (List.length obligations))

let test_subgroup_barrier_advances_only_subgroup_phase () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"sg"
      [
        matrix_store 1;
        SM.Stmt.subgroup_barrier (SM.Site.make ~label:"syncwarp" 2);
        matrix_load 3;
      ]
  in
  let phased = Memory.phases_of_kernel kernel in
  match phased.phases with
  | [ phase ] ->
      Alcotest.(check int) "same workgroup phase" 0 phase.id;
      Alcotest.(check int) "two matrix effects" 2 (List.length phase.accesses);
      let left, right =
        match phase.accesses with
        | [ left; right ] -> (left, right)
        | _ -> Alcotest.fail "expected two accesses"
      in
      Alcotest.(check string)
        "first access before subgroup boundaries" "S[]"
        (Memory.Subgroup_phase_key.to_string left.subgroup_phase);
      Alcotest.(check string)
        "second access after matrix site and syncwarp" "S[1;2]"
        (Memory.Subgroup_phase_key.to_string right.subgroup_phase)
  | _ -> Alcotest.fail "subgroup barrier must not split workgroup phase"

let test_workgroup_barrier_splits_workgroup_phase () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"wg"
      [
        matrix_store 1;
        SM.Stmt.workgroup_barrier (SM.Site.make ~label:"syncthreads" 2);
        matrix_load 3;
      ]
  in
  let phased = Memory.phases_of_kernel kernel in
  match phased.phases with
  | [ before; after ] ->
      Alcotest.(check int) "before phase id" 0 before.id;
      Alcotest.(check int) "after phase id" 1 after.id;
      Alcotest.(check int) "before access count" 1 (List.length before.accesses);
      Alcotest.(check int) "after access count" 1 (List.length after.accesses)
  | _ -> Alcotest.fail "workgroup barrier must split workgroup phases"

let test_same_subgroup_phase_keeps_candidate_visible () : unit =
  let access = single_access 10 in
  let condition =
    Memory.not_ordered_by_subgroup_condition (subgroup_config ()) access access
    |> expect_memory_ok
  in
  Alcotest.(check bool)
    "same matrix site is ordered by negating same-subgroup" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 / 32"
       (Exp.b_to_string condition));
  let plain_left = { access with collective_site = None } in
  let plain_right = { access with collective_site = None } in
  let plain_condition =
    Memory.not_ordered_by_subgroup_condition (subgroup_config ()) plain_left
      plain_right
    |> expect_memory_ok
  in
  Alcotest.(check string)
    "plain same subgroup phase remains visible" "true"
    (Exp.b_to_string plain_condition)

let test_different_subgroup_phase_requires_different_subgroups () : unit =
  let left = single_access 20 in
  let right =
    { left with collective_site = Some 21; subgroup_phase = [ 20 ] }
  in
  let condition =
    Memory.not_ordered_by_subgroup_condition (subgroup_config ()) left right
    |> expect_memory_ok
  in
  let rendered = Exp.b_to_string condition in
  Alcotest.(check bool)
    "condition keeps different subgroups solver-visible" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 / 32" rendered);
  Alcotest.(check bool)
    "condition is the negated same-subgroup predicate" true
    (Stage0.Common.contains ~substring:"!=" rendered)

let test_missing_config_fails_when_subgroup_reasoning_is_needed () : unit =
  let left = single_access 30 in
  let right =
    { left with collective_site = Some 31; subgroup_phase = [ 30 ] }
  in
  match
    Memory.not_ordered_by_subgroup_condition SM.Target_config.missing_cuda left
      right
  with
  | Ok _ -> Alcotest.fail "missing subgroup config unexpectedly succeeded"
  | Error error ->
      Alcotest.(check bool)
        "error explains missing lane mapping" true
        (Stage0.Common.contains
           ~substring:"missing explicit subgroup lane mapping"
           (Memory.error_to_string error))

let test_obligation_consumes_rectangular_matrix_footprint () : unit =
  let obligation =
    single_store_obligation ~block_dim:(checked_block_dim ()) ()
  in
  let goal = Exp.b_to_string obligation.goal in
  Alcotest.(check string) "array" "tile" obligation.array_name;
  Alcotest.(check bool)
    "goal uses row witness from indexed rectangular footprint" true
    (Stage0.Common.contains ~substring:"row$T1 * 32" goal);
  Alcotest.(check bool)
    "goal uses column witness from indexed rectangular footprint" true
    (Stage0.Common.contains ~substring:"col$T1" goal);
  Alcotest.(check bool)
    "goal keeps row bounds condition" true
    (Stage0.Common.contains ~substring:"row$T1 < 16" goal);
  Alcotest.(check bool)
    "same matrix collective site suppresses only same subgroup" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 / 32" goal)

let test_matrix_site_memory_conditions_feed_obligation () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"matrix_condition"
      [ matrix_store 40 ]
  in
  let site_controls =
    [
      source_site_control 40
        ~memory_conditions:
          [ Exp.n_eq (Exp.Var (var "base")) (Exp.Var Variable.tid_x) ];
    ]
  in
  match
    Memory.obligations ~block_dim:(checked_block_dim ()) ~site_controls kernel
    |> expect_memory_ok
  with
  | [ obligation ] ->
      let goal = Exp.b_to_string obligation.goal in
      Alcotest.(check bool)
        "matrix source memory condition is projected on the left task" true
        (Stage0.Common.contains ~substring:"base$T1 == threadIdx.x$T1" goal);
      Alcotest.(check bool)
        "matrix source memory condition is projected on the right task" true
        (Stage0.Common.contains ~substring:"base$T2 == threadIdx.x$T2" goal)
  | obligations ->
      Alcotest.fail
        (Printf.sprintf "expected one matrix obligation, got %d"
           (List.length obligations))

let test_missing_block_dim_fails_for_subgroup_ordered_obligation () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"obligation"
      [ matrix_store 40 ]
  in
  match Memory.obligations kernel with
  | Ok _ -> Alcotest.fail "missing checked block dimensions unexpectedly worked"
  | Error error ->
      Alcotest.(check bool)
        "error explains missing checked block dimensions" true
        (Stage0.Common.contains ~substring:"missing checked block dimensions"
           (Memory.error_to_string error))

let test_ordinary_source_memory_effect_generates_obligation () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"ordinary_memory"
      []
  in
  let ordinary =
    ordinary_effect
      ~access:(Access.write (var "dst") [ Exp.Var (var "i") ] None)
      ~source_conditions:[ Exp.n_lt (Exp.Var (var "i")) (Exp.Num 16) ]
      ()
  in
  match
    Memory.obligations ~block_dim:(checked_block_dim ())
      ~ordinary_memory_effects:[ ordinary ] kernel
    |> expect_memory_ok
  with
  | [ obligation ] ->
      Alcotest.(check string) "array" "dst" obligation.array_name;
      Alcotest.(check string)
        "ordinary origin" "ordinary_write"
        (Memory.access_origin_to_string obligation.left.origin);
      Alcotest.(check string)
        "ordinary phase" "S[]"
        (Memory.Subgroup_phase_key.to_string obligation.left.subgroup_phase);
      let goal = Exp.b_to_string obligation.goal in
      Alcotest.(check bool)
        "ordinary source guard is projected" true
        (Stage0.Common.contains ~substring:"i$T1 < 16" goal);
      Alcotest.(check bool)
        "ordinary index is projected" true
        (Stage0.Common.contains ~substring:"i$T1 == i$T2" goal)
  | obligations ->
      Alcotest.fail
        (Printf.sprintf "expected one ordinary obligation, got %d"
           (List.length obligations))

let test_ordinary_arithmetic_facts_project_to_obligation () : unit =
  let block_dim = checked_block_dim ~x:64 () in
  let owner = var "owner" in
  let idx = var "idx" in
  let ordinary =
    ordinary_effect
      ~access:(Access.write (var "dst") [ Exp.Var idx ] None)
      ~source_conditions:
        [
          Exp.n_eq (Exp.Var owner) (Exp.Var Variable.tid_x);
          Exp.n_eq
            (Exp.n_mod
               (Exp.n_minus (Exp.Var idx) (Exp.Var owner))
               (Exp.Var Variable.bdim_x))
            (Exp.Num 0);
          Exp.n_le (Exp.Var owner) (Exp.Var idx);
        ]
      ()
  in
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ())
      ~name:"ordinary_arithmetic_facts" []
  in
  match
    Memory.obligations ~block_dim ~ordinary_memory_effects:[ ordinary ] kernel
    |> expect_memory_ok
  with
  | [ obligation ] ->
      let goal = Exp.b_to_string obligation.goal in
      Alcotest.(check bool)
        "projects owner fact for T1" true
        (Stage0.Common.contains ~substring:"owner$T1 == threadIdx.x$T1" goal);
      Alcotest.(check bool)
        "projects owner fact for T2" true
        (Stage0.Common.contains ~substring:"owner$T2 == threadIdx.x$T2" goal);
      Alcotest.(check bool)
        "projects stride fact for T1" true
        (Stage0.Common.contains ~substring:"idx$T1 - owner$T1" goal
        && Stage0.Common.contains ~substring:"% blockDim.x" goal);
      Alcotest.(check bool)
        "projects range fact for T2" true
        (Stage0.Common.contains ~substring:"owner$T2 <= idx$T2" goal);
      let assignments =
        thread_goal_assignments ~block_dim
          ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
          ~t2:(Dim3.make ~x:1 ~y:0 ~z:0 ())
        @ [ ("idx$T1", 0); ("idx$T2", 0); ("owner$T1", 0); ("owner$T2", 1) ]
      in
      Alcotest.(check bool)
        "projected ownership rejects impossible same-index owners" false
        (eval_goal assignments obligation.goal)
  | obligations ->
      Alcotest.fail
        (Printf.sprintf "expected one ordinary obligation, got %d"
           (List.length obligations))

let test_memory_globals_project_parameter_members_without_task_suffix () : unit
    =
  let block_dim = checked_block_dim ~x:64 () in
  let globals =
    variable_set [ "params"; "batch_idx"; "dst2_stride"; "dst_global_offset" ]
  in
  let row_base = Exp.Var (var "row_base") in
  let q_tile_row = Exp.Var (var "q_tile_row") in
  let elem_base = Exp.Var (var "elem_base") in
  let ordinary =
    ordinary_effect
      ~access:(Access.write (var "dst") [ Exp.n_plus row_base elem_base ] None)
      ~source_conditions:
        [
          Exp.n_eq row_base
            (Exp.n_plus
               (Exp.Var (var "dst_global_offset"))
               (Exp.n_mult q_tile_row (Exp.Var (var "dst2_stride"))));
          Exp.n_eq
            (Exp.Var (var "dst_global_offset"))
            (Exp.n_plus
               (Exp.Var (var "params.offset_dst"))
               (Exp.n_mult
                  (Exp.Var (var "batch_idx"))
                  (Exp.Var (var "dst2_stride"))));
          Exp.n_eq (Exp.Var (var "batch_idx")) (Exp.Var Variable.bid_x);
          Exp.n_eq
            (Exp.Var (var "dst2_stride"))
            (Exp.n_mult (Exp.Var (var "params.n_heads")) (Exp.Num 64));
          Exp.n_eq elem_base (Exp.n_mult (Exp.Var (var "lane_id")) (Exp.Num 4));
        ]
      ()
  in
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ())
      ~name:"ordinary_memory_globals" []
  in
  match
    Memory.obligations ~globals ~block_dim ~ordinary_memory_effects:[ ordinary ]
      kernel
    |> expect_memory_ok
  with
  | [ obligation ] ->
      let goal = Exp.b_to_string obligation.goal in
      List.iter
        (fun substring ->
          Alcotest.(check bool)
            ("keeps global unsuffixed: " ^ substring)
            true
            (Stage0.Common.contains ~substring goal))
        [
          "params.offset_dst";
          "params.n_heads";
          "blockIdx.x";
          "dst2_stride";
          "batch_idx";
          "dst_global_offset";
        ];
      List.iter
        (fun substring ->
          Alcotest.(check bool)
            ("does not task-project global: " ^ substring)
            false
            (Stage0.Common.contains ~substring goal))
        [
          "params.offset_dst$T";
          "params.n_heads$T";
          "blockIdx.x$T";
          "dst2_stride$T";
          "batch_idx$T";
          "dst_global_offset$T";
        ];
      List.iter
        (fun substring ->
          Alcotest.(check bool)
            ("keeps subgroup-owned local projected: " ^ substring)
            true
            (Stage0.Common.contains ~substring goal))
        [
          "row_base$T1";
          "row_base$T2";
          "q_tile_row$T1";
          "q_tile_row$T2";
          "elem_base$T1";
          "elem_base$T2";
          "lane_id$T1";
          "lane_id$T2";
        ]
  | obligations ->
      Alcotest.fail
        (Printf.sprintf "expected one ordinary obligation, got %d"
           (List.length obligations))

let test_ordinary_and_matrix_effects_share_workgroup_phase () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"ordinary_matrix"
      [ matrix_store 41 ]
  in
  let ordinary =
    ordinary_effect ~kind:Source.Ordinary_read
      ~access:(Access.read (var "tile") [ Exp.Var (var "base") ])
      ()
  in
  let obligations =
    Memory.obligations ~block_dim:(checked_block_dim ())
      ~ordinary_memory_effects:[ ordinary ] kernel
    |> expect_memory_ok
  in
  Alcotest.(check int)
    "matrix self plus matrix/ordinary read/write pair" 2
    (List.length obligations);
  Alcotest.(check bool)
    "ordinary source effect participates with matrix effect" true
    (List.exists
       (fun (obligation : Memory.obligation) ->
         String.equal
           (Memory.access_origin_to_string obligation.left.origin)
           "matrix_store"
         && String.equal
              (Memory.access_origin_to_string obligation.right.origin)
              "ordinary_read")
       obligations)

let test_ordinary_effects_in_different_subgroup_phases_keep_cross_subgroups_visible
    () : unit =
  let block_dim = checked_block_dim () in
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"ordinary_ordering"
      []
  in
  let left =
    ordinary_effect ~site:(ordinary_site ~id:1 ())
      ~access:(Access.write (var "tile") [ Exp.Var (var "base") ] None)
      ()
  in
  let right =
    ordinary_effect ~site:(ordinary_site ~id:2 ())
      ~access:(Access.write (var "tile") [ Exp.Var (var "base") ] None)
      ~phase:(ordinary_phase ~subgroup:[ 10 ] ())
      ()
  in
  let obligations =
    Memory.obligations ~block_dim ~ordinary_memory_effects:[ left; right ]
      kernel
    |> expect_memory_ok
  in
  let cross =
    match
      obligations
      |> List.find_opt (fun (obligation : Memory.obligation) ->
          let left_phase =
            Memory.Subgroup_phase_key.to_string obligation.left.subgroup_phase
          in
          let right_phase =
            Memory.Subgroup_phase_key.to_string obligation.right.subgroup_phase
          in
          String.equal
            (Memory.access_origin_to_string obligation.left.origin)
            "ordinary_write"
          && String.equal
               (Memory.access_origin_to_string obligation.right.origin)
               "ordinary_write"
          && ((String.equal left_phase "S[]" && String.equal right_phase "S[10]")
             || String.equal left_phase "S[10]"
                && String.equal right_phase "S[]"))
    with
    | Some obligation -> obligation
    | None ->
        Alcotest.fail
          ("expected ordinary cross-phase obligation, got:\n"
          ^ (obligations
            |> List.map Memory.obligation_to_string
            |> String.concat "\n"))
  in
  let goal = Exp.b_to_string cross.goal in
  Alcotest.(check bool)
    "cross-phase ordinary pair is ordered only within the same subgroup" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 / 32" goal
    && Stage0.Common.contains ~substring:"!=" goal);
  let same_subgroup =
    matrix_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:1 ~y:0 ~z:0 ())
  in
  Alcotest.(check bool)
    "different subgroup phase suppresses same-subgroup ordinary pair" false
    (eval_goal same_subgroup cross.goal);
  let different_subgroup =
    matrix_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:32 ~y:0 ~z:0 ())
  in
  Alcotest.(check bool)
    "different subgroup phase keeps different-subgroup ordinary pair visible"
    true
    (eval_goal different_subgroup cross.goal)

let test_row_max_read_write_phase_ordering_is_same_subgroup_only () : unit =
  let block_dim = checked_block_dim () in
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"row_max_ordering"
      []
  in
  let read =
    ordinary_effect ~kind:Source.Ordinary_read
      ~site:(ordinary_site ~id:1 ~label:"row_max_read" ())
      ~access:(Access.read (var "row_max_shmem") [ Exp.Var (var "q_tile_row") ])
      ()
  in
  let write =
    ordinary_effect ~kind:Source.Ordinary_write
      ~site:(ordinary_site ~id:2 ~label:"row_max_write" ())
      ~access:
        (Access.write (var "row_max_shmem") [ Exp.Var (var "q_tile_row") ] None)
      ~phase:(ordinary_phase ~subgroup:[ 10; 11 ] ())
      ()
  in
  let obligations =
    Memory.obligations ~block_dim ~ordinary_memory_effects:[ read; write ]
      kernel
    |> expect_memory_ok
  in
  let read_write =
    match
      obligations
      |> List.find_opt (fun (obligation : Memory.obligation) ->
          String.equal obligation.array_name "row_max_shmem"
          && String.equal
               (Memory.access_origin_to_string obligation.left.origin)
               "ordinary_read"
          && String.equal
               (Memory.access_origin_to_string obligation.right.origin)
               "ordinary_write")
    with
    | Some obligation -> obligation
    | None ->
        Alcotest.fail
          ("expected row_max read/write obligation, got:\n"
          ^ (obligations
            |> List.map Memory.obligation_to_string
            |> String.concat "\n"))
  in
  let goal = Exp.b_to_string read_write.goal in
  Alcotest.(check bool)
    "read/write pair is guarded by same-subgroup negation" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 / 32" goal
    && Stage0.Common.contains ~substring:"!=" goal);
  let same_subgroup =
    thread_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:1 ~y:0 ~z:0 ())
    @ [ ("q_tile_row$T1", 0); ("q_tile_row$T2", 0) ]
  in
  Alcotest.(check bool)
    "subgroup phase ordering suppresses same-subgroup read/write pair" false
    (eval_goal same_subgroup read_write.goal);
  let different_subgroup =
    thread_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:32 ~y:0 ~z:0 ())
    @ [ ("q_tile_row$T1", 0); ("q_tile_row$T2", 0) ]
  in
  Alcotest.(check bool)
    "different subgroup read/write pair remains solver-visible" true
    (eval_goal different_subgroup read_write.goal)

let test_obligation_carries_source_order_metadata () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ()) ~name:"source_order"
      [ matrix_store 41 ]
  in
  let ordinary =
    ordinary_effect ~kind:Source.Ordinary_read
      ~site:(ordinary_site ~id:7 ~source_order:10 ())
      ~access:(Access.read (var "tile") [ Exp.Var (var "base") ])
      ()
  in
  let obligations =
    Memory.obligations ~block_dim:(checked_block_dim ())
      ~site_controls:[ source_site_control ~source_order:20 41 ]
      ~ordinary_memory_effects:[ ordinary ] kernel
    |> expect_memory_ok
  in
  match
    obligations
    |> List.find_opt (fun (obligation : Memory.obligation) ->
        String.equal
          (Memory.access_origin_to_string obligation.left.origin)
          "matrix_store"
        && String.equal
             (Memory.access_origin_to_string obligation.right.origin)
             "ordinary_read")
  with
  | Some obligation ->
      Alcotest.(check (option int))
        "left matrix source order" (Some 20) obligation.left.source_order;
      Alcotest.(check (option int))
        "right ordinary source order" (Some 10) obligation.right.source_order
  | None ->
      Alcotest.fail
        ("expected matrix/ordinary source-order obligation, got:\n"
        ^ (obligations
          |> List.map Memory.obligation_to_string
          |> String.concat "\n"))

let test_obligation_constrains_checked_invocation_domain () : unit =
  let obligation =
    single_store_obligation ~block_dim:(checked_block_dim ()) ()
  in
  let goal = Exp.b_to_string obligation.goal in
  Alcotest.(check bool)
    "goal fixes checked blockDim.x" true
    (Stage0.Common.contains ~substring:"blockDim.x == 64" goal);
  Alcotest.(check bool)
    "goal fixes checked blockDim.y" true
    (Stage0.Common.contains ~substring:"blockDim.y == 1" goal);
  Alcotest.(check bool)
    "goal bounds projected T1 x" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 < blockDim.x" goal);
  Alcotest.(check bool)
    "goal bounds projected T2 y" true
    (Stage0.Common.contains ~substring:"threadIdx.y$T2 < blockDim.y" goal)

let test_ordinary_obligation_constrains_checked_invocation_domain () : unit =
  let obligation =
    ordinary_threadidx_self_obligation ~block_dim:(checked_block_dim ()) ()
  in
  let goal = Exp.b_to_string obligation.goal in
  Alcotest.(check bool)
    "ordinary goal fixes checked blockDim.x" true
    (Stage0.Common.contains ~substring:"blockDim.x == 64" goal);
  Alcotest.(check bool)
    "ordinary goal fixes checked blockDim.y" true
    (Stage0.Common.contains ~substring:"blockDim.y == 1" goal);
  Alcotest.(check bool)
    "ordinary goal fixes checked blockDim.z" true
    (Stage0.Common.contains ~substring:"blockDim.z == 1" goal);
  Alcotest.(check bool)
    "ordinary goal bounds projected T1 x" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 < blockDim.x" goal);
  Alcotest.(check bool)
    "ordinary goal bounds projected T2 z" true
    (Stage0.Common.contains ~substring:"threadIdx.z$T2 < blockDim.z" goal)

let test_launch_assertions_supply_symbolic_checked_invocation_domain () : unit =
  let launch_x = Exp.Var (var "launch_block_x") in
  let launch_dimensions =
    Variable.Map.empty
    |> Variable.Map.add Variable.bdim_x launch_x
    |> Variable.Map.add Variable.bdim_y (Exp.Num 1)
    |> Variable.Map.add Variable.bdim_z (Exp.Num 1)
  in
  let checked_block_dim =
    match Memory.checked_block_dim_of_launch_dimensions launch_dimensions with
    | Ok (Some checked_block_dim) -> checked_block_dim
    | Ok None -> Alcotest.fail "launch dimensions did not produce a domain"
    | Error error -> Alcotest.fail (Memory.error_to_string error)
  in
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ())
      ~name:"launch_checked_domain" []
  in
  let ordinary =
    ordinary_effect
      ~access:(Access.write (var "tile") [ Exp.Var Variable.tid_x ] None)
      ()
  in
  let obligation =
    match
      Memory.obligations ~checked_block_dim
        ~ordinary_memory_effects:[ ordinary ] kernel
      |> expect_memory_ok
    with
    | [ obligation ] -> obligation
    | obligations ->
        Alcotest.fail
          (Printf.sprintf "expected one launch-domain obligation, got %d"
             (List.length obligations))
  in
  let rendered = Memory.obligation_to_string obligation in
  Alcotest.(check bool)
    "launch x remains symbolic" true
    (Stage0.Common.contains ~substring:"blockDim.x == launch_block_x" rendered);
  Alcotest.(check bool)
    "launch x is positive" true
    (Stage0.Common.contains ~substring:"launch_block_x > 0" rendered);
  Alcotest.(check bool)
    "T1 x is bounded by launch x" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 < launch_block_x"
       rendered);
  Alcotest.(check bool)
    "T2 x is bounded by launch x" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T2 < launch_block_x"
       rendered)

let test_partial_launch_checked_invocation_domain_fails () : unit =
  let launch_dimensions = Variable.Map.singleton Variable.bdim_x (Exp.Num 64) in
  match Memory.checked_block_dim_of_launch_dimensions launch_dimensions with
  | Error (Memory.Incomplete_launch_checked_block_dim missing) ->
      Alcotest.(check (list string))
        "missing launch axes"
        [ "blockDim.y"; "blockDim.z" ]
        missing
  | Error error -> Alcotest.fail (Memory.error_to_string error)
  | Ok _ -> Alcotest.fail "partial launch dimensions were accepted"

let test_launch_checked_precondition_rejects_zero_dimension () : unit =
  let launch_dimensions =
    Variable.Map.empty
    |> Variable.Map.add Variable.bdim_x (Exp.Num 0)
    |> Variable.Map.add Variable.bdim_y (Exp.Num 1)
    |> Variable.Map.add Variable.bdim_z (Exp.Num 1)
  in
  let checked_block_dim =
    match Memory.checked_block_dim_of_launch_dimensions launch_dimensions with
    | Ok (Some checked_block_dim) -> checked_block_dim
    | Ok None -> Alcotest.fail "launch dimensions did not produce a domain"
    | Error error -> Alcotest.fail (Memory.error_to_string error)
  in
  let precondition = Memory.checked_block_dim_precondition checked_block_dim in
  Alcotest.(check bool)
    "zero launch dimension is unsatisfiable" false
    (eval_goal
       [ ("blockDim.x", 0); ("blockDim.y", 1); ("blockDim.z", 1) ]
       precondition)

let test_domain_rejects_impossible_yz_subgroup_escape () : unit =
  let block_dim = checked_block_dim ~x:64 () in
  let obligation = single_store_obligation ~block_dim () in
  let assignments =
    matrix_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:0 ~y:1 ~z:0 ())
  in
  Alcotest.(check bool)
    "1-D block rejects y-mismatch escape from same subgroup ordering" false
    (eval_goal assignments obligation.goal)

let test_ordinary_domain_rejects_impossible_same_phase_yz_escape () : unit =
  let block_dim = checked_block_dim ~x:64 () in
  let obligation = ordinary_threadidx_self_obligation ~block_dim () in
  let assignments =
    thread_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:0 ~y:1 ~z:0 ())
  in
  Alcotest.(check bool)
    "1-D block rejects ordinary same-phase y-mismatch escape" false
    (eval_goal assignments obligation.goal)

let test_domain_rejects_x_at_block_bound_escape () : unit =
  let block_dim = checked_block_dim ~x:32 () in
  let obligation = single_store_obligation ~block_dim () in
  let assignments =
    matrix_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:32 ~y:0 ~z:0 ())
  in
  Alcotest.(check bool)
    "blockDim.x == subgroup size rejects x-at-bound escape" false
    (eval_goal assignments obligation.goal)

let test_valid_cross_subgroup_pair_remains_visible () : unit =
  let block_dim = checked_block_dim ~x:64 () in
  let obligation = single_store_obligation ~block_dim () in
  let assignments =
    matrix_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:32 ~y:0 ~z:0 ())
  in
  Alcotest.(check bool)
    "valid different-subgroup matrix pair remains solver-visible" true
    (eval_goal assignments obligation.goal)

let test_obligation_constrains_block_index_domain () : unit =
  let block_dim = checked_block_dim ~x:64 () in
  let obligation = single_store_obligation ~block_dim () in
  let rendered = Exp.b_to_string obligation.goal in
  Alcotest.(check bool)
    "goal has nonnegative block x" true
    (Stage0.Common.contains ~substring:"blockIdx.x >= 0" rendered);
  Alcotest.(check bool)
    "goal bounds block x by grid x" true
    (Stage0.Common.contains ~substring:"blockIdx.x < gridDim.x" rendered);
  let assignments =
    matrix_goal_assignments ~block_dim
      ~t1:(Dim3.make ~x:0 ~y:0 ~z:0 ())
      ~t2:(Dim3.make ~x:32 ~y:0 ~z:0 ())
    |> List.map (fun (name, value) ->
        if String.equal name "blockIdx.x" then (name, 1) else (name, value))
  in
  Alcotest.(check bool)
    "blockIdx.x == gridDim.x is outside CUDA launch domain" false
    (eval_goal assignments obligation.goal)

let symbolic_checked_block_dim_of_l117 () : Memory.checked_block_dim =
  let l117 = LC.of_row_id "L117" |> expect_launch_contract_ok in
  match Symbolic_launch_evidence.checked_block_dim_of_launch_contract l117 with
  | Ok (Some checked_block_dim) -> checked_block_dim
  | Ok None -> Alcotest.fail "expected L117 symbolic checked block dimensions"
  | Error error -> Alcotest.fail (Memory.error_to_string error)

let test_solve_tri_carrier_emits_symbolic_checked_obligation () : unit =
  let symbolic_checked_block_dim = symbolic_checked_block_dim_of_l117 () in
  Alcotest.(check string)
    "symbolic checked block dim" "[32, K, 1]"
    (Memory.checked_block_dim_to_string symbolic_checked_block_dim);
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ())
      ~name:"symbolic_checked_domain" []
  in
  let ordinary =
    ordinary_effect
      ~access:(Access.write (var "tile") [ Exp.Var Variable.tid_y ] None)
      ()
  in
  let obligations =
    Memory.obligations ~checked_block_dim:symbolic_checked_block_dim
      ~ordinary_memory_effects:[ ordinary ] kernel
    |> expect_memory_ok
  in
  let obligation =
    match obligations with
    | [ obligation ] -> obligation
    | obligations ->
        Alcotest.fail
          (Printf.sprintf "expected one symbolic K obligation, got %d"
             (List.length obligations))
  in
  write_optional_symbolic_obligation_artifact obligation;
  let rendered = Memory.obligation_to_string obligation in
  Alcotest.(check bool)
    "rendered obligation contains blockDim.y = K" true
    (Stage0.Common.contains ~substring:"blockDim.y == K" rendered);
  Alcotest.(check bool)
    "rendered obligation contains positive K guard" true
    (Stage0.Common.contains ~substring:"K > 0" rendered);
  Alcotest.(check bool)
    "rendered obligation bounds T1 y by K" true
    (Stage0.Common.contains ~substring:"threadIdx.y$T1 < K" rendered);
  Alcotest.(check bool)
    "rendered obligation bounds T2 y by K" true
    (Stage0.Common.contains ~substring:"threadIdx.y$T2 < K" rendered);
  Alcotest.(check bool)
    "rendered obligation preserves concrete x fact" true
    (Stage0.Common.contains ~substring:"blockDim.x == 32" rendered)

let test_symbolic_checked_block_dim_requires_subgroup_size () : unit =
  let carrier =
    {
      LC.solve_tri_symbolic_dimension_carrier with
      LCG.carrier_subgroup_size = 0;
    }
  in
  match Symbolic_launch_evidence.checked_block_dim_of_carrier carrier with
  | Ok _ -> Alcotest.fail "missing subgroup size unexpectedly worked"
  | Error (Memory.Invalid_symbolic_checked_block_dim reason) ->
      Alcotest.(check bool)
        "error names subgroup size" true
        (Stage0.Common.contains ~substring:"subgroup size" reason)
  | Error error -> Alcotest.fail (Memory.error_to_string error)

let test_ordinary_launch_contract_has_no_symbolic_checked_block_dim () : unit =
  let l072 = LC.of_row_id "L072" |> expect_launch_contract_ok in
  match Symbolic_launch_evidence.checked_block_dim_of_launch_contract l072 with
  | Ok None -> ()
  | Ok (Some _) ->
      Alcotest.fail "ordinary row unexpectedly had symbolic checked block dim"
  | Error error -> Alcotest.fail (Memory.error_to_string error)

let test_symbolic_launch_evidence_rewrites_source_width () : unit =
  let memory_effect =
    ordinary_effect
      ~source_conditions:
        [
          Exp.n_eq (Exp.Var (var "k")) (Exp.Num 32);
          Exp.n_eq (Exp.Var (var "n")) (Exp.Num 64);
        ]
      ()
  in
  let rewrite =
    Symbolic_launch_evidence.rewrite_ordinary_memory_effects
      LC.solve_tri_symbolic_dimension_carrier [ memory_effect ]
    |> expect_ok
  in
  Alcotest.(check int)
    "rewrite count" 1
    rewrite.Symbolic_launch_evidence.source_width_rewrite_count;
  let rewritten_effect =
    match rewrite.source_launch_ordinary_memory_effects with
    | [ memory_effect ] -> memory_effect
    | memory_effects ->
        Alcotest.fail
          (Printf.sprintf "expected one rewritten effect, got %d"
             (List.length memory_effects))
  in
  let rendered =
    Exp.b_and_ex rewritten_effect.Source.source_conditions |> Exp.b_to_string
  in
  Alcotest.(check bool)
    "rewrites exact source width to k == K" true
    (Stage0.Common.contains ~substring:"k == K" rendered);
  Alcotest.(check bool)
    "adds finite K candidate domain" true
    (Stage0.Common.contains ~substring:"K == 32" rendered);
  Alcotest.(check bool)
    "removes concrete row-local k fact" false
    (Stage0.Common.contains ~substring:"k == 32" rendered)

let test_symbolic_launch_evidence_rewrites_source_width_unguarded () : unit =
  let memory_effect =
    ordinary_effect
      ~source_conditions:
        [
          Exp.n_eq (Exp.Var (var "k")) (Exp.Num 128);
          Exp.n_eq (Exp.Var (var "n")) (Exp.Num 64);
        ]
      ()
  in
  let rewrite =
    Symbolic_launch_evidence.rewrite_ordinary_memory_effects
      ~domain_mode:Symbolic_launch_evidence.Unguarded_family_domain
      LC.solve_tri_symbolic_dimension_carrier [ memory_effect ]
    |> expect_ok
  in
  Alcotest.(check int)
    "rewrite count" 1
    rewrite.Symbolic_launch_evidence.source_width_rewrite_count;
  let rewritten_effect =
    match rewrite.source_launch_ordinary_memory_effects with
    | [ memory_effect ] -> memory_effect
    | memory_effects ->
        Alcotest.fail
          (Printf.sprintf "expected one rewritten effect, got %d"
             (List.length memory_effects))
  in
  let rendered =
    Exp.b_and_ex rewritten_effect.Source.source_conditions |> Exp.b_to_string
  in
  let facts = String.concat "\n" rewrite.source_launch_fact_lines in
  Alcotest.(check bool)
    "rewrites exact source width outside production candidates" true
    (Stage0.Common.contains ~substring:"k == K" rendered);
  Alcotest.(check bool)
    "keeps semantic positive K guard" true
    (Stage0.Common.contains ~substring:"K > 0" rendered);
  Alcotest.(check bool)
    "omits finite candidate domain" false
    (Stage0.Common.contains ~substring:"K == 32" rendered);
  Alcotest.(check bool)
    "records omitted candidate domain" true
    (Stage0.Common.contains ~substring:"omitted_for_s488_unguarded_frontier"
       facts)

let test_symbolic_launch_evidence_requires_source_width_fact () : unit =
  let memory_effect =
    ordinary_effect
      ~source_conditions:[ Exp.n_eq (Exp.Var (var "n")) (Exp.Num 64) ]
      ()
  in
  match
    Symbolic_launch_evidence.rewrite_ordinary_memory_effects
      LC.solve_tri_symbolic_dimension_carrier [ memory_effect ]
  with
  | Ok _ -> Alcotest.fail "missing source-width fact unexpectedly worked"
  | Error reason ->
      Alcotest.(check bool)
        "error names missing source-width fact" true
        (Stage0.Common.contains ~substring:"did not find" reason)

let test_subgroup_precondition_projects_to_both_tasks () : unit =
  let kernel =
    SM.Kernel.make ~target_config:(subgroup_config ())
      ~name:"precondition_projection" []
  in
  let memory_effect =
    ordinary_effect ~access:(Access.write (var "tile") [ Exp.Num 0 ] None) ()
  in
  let precondition =
    Exp.n_lt (Exp.Var Variable.tid_x) (Exp.Var (var "limit"))
  in
  let obligation =
    Memory.obligations
      ~globals:(Variable.Set.singleton (var "limit"))
      ~precondition
      ~block_dim:(checked_block_dim ~x:64 ())
      ~ordinary_memory_effects:[ memory_effect ] kernel
    |> expect_memory_ok
    |> function
    | [ obligation ] -> obligation
    | obligations ->
        Alcotest.failf "expected one obligation, got %d"
          (List.length obligations)
  in
  let goal = Exp.b_to_string obligation.goal in
  Alcotest.(check bool)
    "task 1 precondition" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T1 < limit" goal);
  Alcotest.(check bool)
    "task 2 precondition" true
    (Stage0.Common.contains ~substring:"threadIdx.x$T2 < limit" goal)

let tests : unit Alcotest.test_case list =
  [
    ( "subgroup barrier advances only subgroup phase",
      `Quick,
      test_subgroup_barrier_advances_only_subgroup_phase );
    ( "workgroup barrier splits workgroup phase",
      `Quick,
      test_workgroup_barrier_splits_workgroup_phase );
    ( "same subgroup phase visibility",
      `Quick,
      test_same_subgroup_phase_keeps_candidate_visible );
    ( "different subgroup phase visibility",
      `Quick,
      test_different_subgroup_phase_requires_different_subgroups );
    ( "missing config fails",
      `Quick,
      test_missing_config_fails_when_subgroup_reasoning_is_needed );
    ( "rectangular matrix footprint obligation",
      `Quick,
      test_obligation_consumes_rectangular_matrix_footprint );
    ( "matrix site memory conditions feed obligations",
      `Quick,
      test_matrix_site_memory_conditions_feed_obligation );
    ( "missing checked block dimensions",
      `Quick,
      test_missing_block_dim_fails_for_subgroup_ordered_obligation );
    ( "ordinary source memory effect obligation",
      `Quick,
      test_ordinary_source_memory_effect_generates_obligation );
    ( "ordinary arithmetic facts project to obligation",
      `Quick,
      test_ordinary_arithmetic_facts_project_to_obligation );
    ( "memory globals project parameter members without task suffix",
      `Quick,
      test_memory_globals_project_parameter_members_without_task_suffix );
    ( "ordinary and matrix effects share workgroup phase",
      `Quick,
      test_ordinary_and_matrix_effects_share_workgroup_phase );
    ( "ordinary cross-subgroup visibility",
      `Quick,
      test_ordinary_effects_in_different_subgroup_phases_keep_cross_subgroups_visible
    );
    ( "row_max read/write subgroup ordering",
      `Quick,
      test_row_max_read_write_phase_ordering_is_same_subgroup_only );
    ( "source order metadata on obligations",
      `Quick,
      test_obligation_carries_source_order_metadata );
    ( "checked invocation domain constraints",
      `Quick,
      test_obligation_constrains_checked_invocation_domain );
    ( "ordinary checked invocation domain constraints",
      `Quick,
      test_ordinary_obligation_constrains_checked_invocation_domain );
    ( "launch assertions supply symbolic checked invocation domain",
      `Quick,
      test_launch_assertions_supply_symbolic_checked_invocation_domain );
    ( "partial launch checked invocation domain fails",
      `Quick,
      test_partial_launch_checked_invocation_domain_fails );
    ( "launch checked precondition rejects zero dimension",
      `Quick,
      test_launch_checked_precondition_rejects_zero_dimension );
    ( "reject impossible y/z subgroup escape",
      `Quick,
      test_domain_rejects_impossible_yz_subgroup_escape );
    ( "reject ordinary impossible y/z same-phase escape",
      `Quick,
      test_ordinary_domain_rejects_impossible_same_phase_yz_escape );
    ( "reject x at block bound escape",
      `Quick,
      test_domain_rejects_x_at_block_bound_escape );
    ( "valid cross-subgroup visibility",
      `Quick,
      test_valid_cross_subgroup_pair_remains_visible );
    ( "block index invocation domain",
      `Quick,
      test_obligation_constrains_block_index_domain );
    ( "solve-tri carrier emits symbolic checked obligation",
      `Quick,
      test_solve_tri_carrier_emits_symbolic_checked_obligation );
    ( "symbolic checked block dim requires subgroup size",
      `Quick,
      test_symbolic_checked_block_dim_requires_subgroup_size );
    ( "ordinary launch contract has no symbolic checked block dim",
      `Quick,
      test_ordinary_launch_contract_has_no_symbolic_checked_block_dim );
    ( "symbolic launch evidence rewrites source width",
      `Quick,
      test_symbolic_launch_evidence_rewrites_source_width );
    ( "symbolic launch evidence rewrites source width unguarded",
      `Quick,
      test_symbolic_launch_evidence_rewrites_source_width_unguarded );
    ( "symbolic launch evidence requires source width fact",
      `Quick,
      test_symbolic_launch_evidence_requires_source_width_fact );
    ( "subgroup precondition projects to both tasks",
      `Quick,
      test_subgroup_precondition_projects_to_both_tasks );
  ]

let () = Alcotest.run "Subgroup_obligation" [ ("subgroup_obligation", tests) ]
