open Protocols
module Decl_expr = Inference.Decl_expr
module D_lang = Inference.D_lang
module J_type = Inference.J_type
module SM = Inference.Subgroup_matrix
module Source = Inference.Subgroup_source
module Ty_variable = Inference.Ty_variable
module Uniformity = Drf.Subgroup_uniformity

let var (name : string) : Variable.t = Variable.from_name name

let ident ?(kind = Decl_expr.Kind.Var) ?(ty = J_type.int) (name : string) :
    D_lang.Expr.t =
  Ident { name = Variable.from_name name; ty; kind }

let ty_var ?(ty = J_type.int) (name : string) : Ty_variable.t =
  Ty_variable.make ~name:(var name) ~ty

let decl ?(ty = J_type.int) (name : string) (expr : D_lang.Expr.t) :
    D_lang.Decl.t =
  D_lang.Decl.from_expr (ty_var ~ty name) expr

let undef_decl ?(ty = J_type.int) (name : string) : D_lang.Decl.t =
  D_lang.Decl.from_undef (ty_var ~ty name)

let bin ?(ty = J_type.int) (lhs : D_lang.Expr.t) (opcode : string)
    (rhs : D_lang.Expr.t) : D_lang.Expr.t =
  D_lang.Expr.BinaryOperator { lhs; opcode; rhs; ty }

let assign (name : string) (rhs : D_lang.Expr.t) : D_lang.Stmt.t =
  D_lang.Stmt.SExpr (bin (ident name) "=" rhs)

let call_expr (name : string) (args : D_lang.Expr.t list) : D_lang.Expr.t =
  CallExpr
    {
      func = ident ~kind:Decl_expr.Kind.Function ~ty:J_type.void name;
      args;
      ty = J_type.void;
    }

let thread_x_lt_32 () : D_lang.Expr.t =
  D_lang.Expr.BinaryOperator
    {
      lhs = ident "threadIdx.x";
      opcode = "<";
      rhs = D_lang.Expr.IntegerLiteral 32;
      ty = J_type.bool;
    }

let thread_x_mod_32 () : D_lang.Expr.t =
  D_lang.Expr.BinaryOperator
    {
      lhs = ident "threadIdx.x";
      opcode = "%";
      rhs = D_lang.Expr.IntegerLiteral 32;
      ty = J_type.int;
    }

let member_expr ?(ty = J_type.int) (base : string) (field : string) :
    D_lang.Expr.t =
  D_lang.Expr.MemberExpr { base = ident base; name = field; ty }

let block_x_eq_0 () : D_lang.Expr.t =
  D_lang.Expr.BinaryOperator
    {
      lhs = member_expr "blockIdx" "x";
      opcode = "==";
      rhs = D_lang.Expr.IntegerLiteral 0;
      ty = J_type.bool;
    }

let expect_uniformity_ok (type a) (result : (a, Uniformity.error) result) : a =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.fail (Uniformity.error_to_string error)

let expect_matrix_ok (type a) (result : (a, string) result) : a =
  match result with Ok value -> value | Error msg -> Alcotest.fail msg

let subgroup_config () : SM.Target_config.t =
  SM.Target_config.subgroup_size_exn 32 |> SM.Target_config.cuda_x_contiguous

let site ?(label = "site") (id : int) : SM.Site.t = SM.Site.make ~label id

let collective_result (site : SM.Site.t) (result : string) : SM.Stmt.t =
  let collective =
    SM.Collective.make site
      (SM.Collective.Ballot_payload { result = var result; predicate = None })
  in
  SM.Stmt.Subgroup_collective collective

let syncwarp_stmt : D_lang.Stmt.t =
  D_lang.Stmt.SExpr (call_expr "__syncwarp" [])

let kernel_param ?(ty = J_type.int) (name : string) : D_lang.Param.t =
  D_lang.Param.make
    ~ty_var:(Ty_variable.make ~name:(var name) ~ty)
    ~is_used:true ~is_shared:false

let base_access ?(mode = `Write) ?(array = "tile") () : Access.t =
  match mode with
  | `Read -> Access.read (var array) [ Exp.Var (var "base") ]
  | `Write -> Access.write (var array) [ Exp.Var (var "base") ] None

let rectangular (access : Access.t) : SM.Matrix.footprint =
  SM.Matrix.rectangular ~base:access ~rows:(Exp.Num 16) ~cols:(Exp.Num 8)
    ~leading_dimension:(Exp.Num 32) ~layout:SM.Matrix.Row_major ~row:(var "row")
    ~col:(var "col")
  |> expect_matrix_ok

let matrix_store (site_id : int) : SM.Stmt.t =
  let site = site ~label:"store_matrix_sync" site_id in
  let collective =
    SM.Matrix.store_matrix_sync site (rectangular (base_access ()))
    |> expect_matrix_ok
  in
  SM.Stmt.Matrix_collective collective

let kernel ?(target_config = subgroup_config ()) ?(name = "kernel")
    (body : SM.Stmt.t list) : SM.Kernel.t =
  SM.Kernel.make ~target_config ~name body

let single_site_result (result : Uniformity.function_result) :
    Uniformity.site_result =
  match Uniformity.sites result with
  | [ site ] -> site
  | sites ->
      Alcotest.fail
        (Printf.sprintf "expected one site result, got %d" (List.length sites))

let source_site_control (control : Source.site_control) :
    SM.Site.id * Uniformity.control =
  ( control.site_id,
    Uniformity.control_with_uniform_vars ~conditions:control.conditions
      ~uniform_vars:control.uniform_vars )

let test_top_level_subgroup_and_matrix_sites_are_uniform () : unit =
  let kernel =
    kernel
      [ SM.Stmt.subgroup_barrier (site ~label:"syncwarp" 1); matrix_store 2 ]
  in
  let result = Uniformity.check_kernel kernel |> expect_uniformity_ok in
  Alcotest.(check int) "site count" 2 (List.length (Uniformity.sites result));
  Alcotest.(check string)
    "uniform verdict" "drf"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result));
  Alcotest.(check string)
    "full verdict" "drf"
    (Uniformity.full_verdict_to_string
       (Uniformity.compose Uniformity.Memory_drf
          (Uniformity.function_verdict result)))

let test_thread_index_control_is_undefined_behavior () : unit =
  let controlled_site = site ~label:"nested_store_matrix_sync" 10 in
  let kernel =
    kernel
      [
        SM.Stmt.Matrix_collective
          (SM.Matrix.store_matrix_sync controlled_site
             (rectangular (base_access ()))
          |> expect_matrix_ok);
      ]
  in
  let control =
    Uniformity.control
      ~conditions:[ Exp.n_lt (Exp.Var Variable.tid_x) (Exp.Num 32) ]
  in
  let result =
    Uniformity.check_kernel ~site_controls:[ (10, control) ] kernel
    |> expect_uniformity_ok
  in
  let site = single_site_result result in
  Alcotest.(check int)
    "control depth" 1
    (Uniformity.site_result_control_depth site);
  Alcotest.(check string)
    "site outcome" "ub(reason=non_uniform_control)"
    (Uniformity.outcome_to_string (Uniformity.site_result_outcome site));
  Alcotest.(check string)
    "uniformity verdict" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result));
  Alcotest.(check string)
    "full verdict with memory DRF" "not_drf"
    (Uniformity.full_verdict_to_string
       (Uniformity.compose Uniformity.Memory_drf
          (Uniformity.function_verdict result)))

let test_subgroup_index_control_is_uniform_with_explicit_config () : unit =
  let kernel = kernel [ matrix_store 11 ] in
  let subgroup_index = Exp.n_div (Exp.Var Variable.tid_x) (Exp.Num 32) in
  let control =
    Uniformity.control
      ~conditions:[ Exp.n_eq subgroup_index (Exp.Var (var "subgroup")) ]
  in
  let result =
    Uniformity.check_kernel
      ~uniform_vars:(Variable.Set.singleton (var "subgroup"))
      ~site_controls:[ (11, control) ]
      kernel
    |> expect_uniformity_ok
  in
  Alcotest.(check string)
    "subgroup-index control is uniform" "drf"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_subgroup_index_control_requires_config () : unit =
  let kernel =
    kernel ~target_config:SM.Target_config.missing_cuda [ matrix_store 12 ]
  in
  let control =
    Uniformity.control
      ~conditions:
        [
          Exp.n_eq (Exp.n_div (Exp.Var Variable.tid_x) (Exp.Num 32)) (Exp.Num 0);
        ]
  in
  match Uniformity.check_kernel ~site_controls:[ (12, control) ] kernel with
  | Ok _ -> Alcotest.fail "missing subgroup config unexpectedly worked"
  | Error error ->
      Alcotest.(check bool)
        "error explains missing subgroup mapping" true
        (Stage0.Common.contains
           ~substring:"missing explicit subgroup lane mapping"
           (Uniformity.error_to_string error))

let test_thread_yz_control_requires_config () : unit =
  let kernel =
    kernel ~target_config:SM.Target_config.missing_cuda
      [ matrix_store 15; matrix_store 16 ]
  in
  let y_control =
    Uniformity.control
      ~conditions:[ Exp.n_eq (Exp.Var Variable.tid_y) (Exp.Num 0) ]
  in
  let z_control =
    Uniformity.control
      ~conditions:[ Exp.n_eq (Exp.Var Variable.tid_z) (Exp.Num 0) ]
  in
  let result =
    Uniformity.check_kernel
      ~site_controls:[ (15, y_control); (16, z_control) ]
      kernel
    |> expect_uniformity_ok
  in
  let outcomes =
    List.map Uniformity.site_result_outcome (Uniformity.sites result)
  in
  Alcotest.(check int) "site count" 2 (List.length outcomes);
  Alcotest.(check bool)
    "missing config rejects both y/z controls" true
    (List.for_all
       (function
         | Uniformity.Site_undefined_behavior Uniformity.Non_uniform_control ->
             true
         | Uniformity.Site_drf -> false)
       outcomes);
  Alcotest.(check string)
    "uniformity verdict" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_thread_yz_control_is_uniform_with_explicit_config () : unit =
  let kernel = kernel [ matrix_store 17; matrix_store 18 ] in
  let y_control =
    Uniformity.control
      ~conditions:[ Exp.n_eq (Exp.Var Variable.tid_y) (Exp.Num 0) ]
  in
  let z_control =
    Uniformity.control
      ~conditions:[ Exp.n_eq (Exp.Var Variable.tid_z) (Exp.Num 0) ]
  in
  let result =
    Uniformity.check_kernel
      ~site_controls:[ (17, y_control); (18, z_control) ]
      kernel
    |> expect_uniformity_ok
  in
  Alcotest.(check string)
    "configured y/z controls are uniform" "drf"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_lane_and_unknown_local_control_are_non_uniform () : unit =
  let kernel = kernel [ matrix_store 13; matrix_store 14 ] in
  let lane_control =
    Uniformity.control
      ~conditions:
        [
          Exp.n_eq (Exp.n_mod (Exp.Var Variable.tid_x) (Exp.Num 32)) (Exp.Num 0);
        ]
  in
  let unknown_control =
    Uniformity.control
      ~conditions:[ Exp.n_eq (Exp.Var (var "unknown_local")) (Exp.Num 0) ]
  in
  let result =
    Uniformity.check_kernel
      ~site_controls:[ (13, lane_control); (14, unknown_control) ]
      kernel
    |> expect_uniformity_ok
  in
  let outcomes =
    List.map Uniformity.site_result_outcome (Uniformity.sites result)
  in
  Alcotest.(check int) "site count" 2 (List.length outcomes);
  Alcotest.(check bool)
    "both controls reject" true
    (List.for_all
       (function
         | Uniformity.Site_undefined_behavior Uniformity.Non_uniform_control ->
             true
         | Uniformity.Site_drf -> false)
       outcomes)

let test_subgroup_collective_result_control_is_non_uniform () : unit =
  let kernel =
    kernel
      [ collective_result (site ~label:"ballot" 20) "mask"; matrix_store 21 ]
  in
  let control =
    Uniformity.control
      ~conditions:[ Exp.n_eq (Exp.Var (var "mask")) (Exp.Num 0) ]
  in
  let result =
    Uniformity.check_kernel ~site_controls:[ (21, control) ] kernel
    |> expect_uniformity_ok
  in
  let store_site =
    match
      Uniformity.sites result
      |> List.find_opt (fun site ->
          Int.equal (SM.Site.id (Uniformity.site_result_site site)) 21)
    with
    | Some site -> site
    | None -> Alcotest.fail "missing store site"
  in
  Alcotest.(check string)
    "subgroup result is varying" "ub(reason=non_uniform_control)"
    (Uniformity.outcome_to_string (Uniformity.site_result_outcome store_site))

let test_source_thread_x_guard_reaches_uniformity_checker () : unit =
  let condition = thread_x_lt_32 () in
  let source_kernel : D_lang.Kernel.t =
    {
      ty = "void ()";
      name = "guarded_syncwarp";
      code =
        IfStmt
          {
            cond = condition;
            then_stmt = SExpr (call_expr "__syncwarp" []);
            else_stmt = Skip;
          };
      type_params = [];
      template_args = [];
      params = [];
      attribute = D_lang.KernelAttr.Default;
    }
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel source_kernel ]
  with
  | Ok [ Source.Subgroup_matrix subgroup ] ->
      let site_controls = List.map source_site_control subgroup.site_controls in
      let result =
        Uniformity.check_kernel ~site_controls
          ~uniform_vars:subgroup.uniform_vars subgroup.matrix_kernel
        |> expect_uniformity_ok
      in
      let site = single_site_result result in
      Alcotest.(check int)
        "source control depth" 1
        (Uniformity.site_result_control_depth site);
      Alcotest.(check string)
        "source-guarded subgroup op is UB" "undefined_behavior"
        (Uniformity.verdict_to_string (Uniformity.function_verdict result));
      Alcotest.(check string)
        "full verdict rejects source-guarded subgroup op" "not_drf"
        (Uniformity.full_verdict_to_string
           (Uniformity.compose Uniformity.Memory_drf
              (Uniformity.function_verdict result)))
  | Ok _ -> Alcotest.fail "guarded subgroup route returned unexpected kernels"
  | Error error -> Alcotest.fail (Source.error_to_string error)

let check_source_guarded_syncwarp_rejects (name : string) (code : D_lang.Stmt.t)
    : unit =
  let source_kernel : D_lang.Kernel.t =
    {
      ty = "void ()";
      name;
      code;
      type_params = [];
      template_args = [];
      params = [];
      attribute = D_lang.KernelAttr.Default;
    }
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel source_kernel ]
  with
  | Ok [ Source.Subgroup_matrix subgroup ] ->
      let site_controls = List.map source_site_control subgroup.site_controls in
      let result =
        Uniformity.check_kernel ~site_controls
          ~uniform_vars:subgroup.uniform_vars subgroup.matrix_kernel
        |> expect_uniformity_ok
      in
      let site = single_site_result result in
      Alcotest.(check bool)
        "source control reaches uniformity" true
        (Uniformity.site_result_control_depth site > 0);
      Alcotest.(check string)
        "guarded subgroup op is UB" "undefined_behavior"
        (Uniformity.verdict_to_string (Uniformity.function_verdict result));
      Alcotest.(check string)
        "full verdict rejects guarded subgroup op" "not_drf"
        (Uniformity.full_verdict_to_string
           (Uniformity.compose Uniformity.Memory_drf
              (Uniformity.function_verdict result)))
  | Ok _ -> Alcotest.fail "guarded subgroup route returned unexpected kernels"
  | Error error -> Alcotest.fail (Source.error_to_string error)

let test_source_uniform_kernel_param_member_control_accepts () : unit =
  let source_kernel : D_lang.Kernel.t =
    {
      ty = "void ()";
      name = "uniform_param_loop_syncwarp";
      code =
        WhileStmt
          {
            cond =
              BinaryOperator
                {
                  lhs = member_expr "params" "seq_len_kv";
                  opcode = ">";
                  rhs = IntegerLiteral 0;
                  ty = J_type.bool;
                };
            body = syncwarp_stmt;
          };
      type_params = [];
      template_args = [];
      params = [ kernel_param "params" ];
      attribute = D_lang.KernelAttr.Default;
    }
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel source_kernel ]
  with
  | Ok [ Source.Subgroup_matrix subgroup ] ->
      let site_controls = List.map source_site_control subgroup.site_controls in
      let result =
        Uniformity.check_kernel ~site_controls
          ~uniform_vars:subgroup.uniform_vars subgroup.matrix_kernel
        |> expect_uniformity_ok
      in
      Alcotest.(check string)
        "uniform param member control accepts" "drf"
        (Uniformity.verdict_to_string (Uniformity.function_verdict result))
  | Ok _ -> Alcotest.fail "guarded subgroup route returned unexpected kernels"
  | Error error -> Alcotest.fail (Source.error_to_string error)

let subgroup_id_alias_program ~(warp_size : int) : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
          ];
        ForStmt
          {
            init =
              Some (D_lang.ForInit.Decls [ decl "kv_block" (ident "warp_id") ]);
            cond =
              Some
                (bin ~ty:J_type.bool (ident "kv_block") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration
      (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral warp_size));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "subgroup_id_alias_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let check_source_program_uniformity (program : D_lang.Program.t) :
    Uniformity.function_result =
  match Source.route_program ~target_config:(subgroup_config ()) program with
  | Ok [ Source.Subgroup_matrix subgroup ] ->
      let site_controls = List.map source_site_control subgroup.site_controls in
      Uniformity.check_kernel ~site_controls ~uniform_vars:subgroup.uniform_vars
        subgroup.matrix_kernel
      |> expect_uniformity_ok
  | Ok _ -> Alcotest.fail "source route returned unexpected kernels"
  | Error error -> Alcotest.fail (Source.error_to_string error)

let check_subgroup_id_alias_uniformity ~(warp_size : int) :
    Uniformity.function_result =
  check_source_program_uniformity (subgroup_id_alias_program ~warp_size)

let test_source_subgroup_id_alias_control_accepts () : unit =
  let result = check_subgroup_id_alias_uniformity ~warp_size:32 in
  let site = single_site_result result in
  Alcotest.(check bool)
    "subgroup-id alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "subgroup-id alias is uniform" "drf"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_subgroup_id_alias_requires_matching_size () : unit =
  let result = check_subgroup_id_alias_uniformity ~warp_size:16 in
  Alcotest.(check string)
    "mismatched subgroup-id divisor rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let stale_subgroup_id_alias_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
          ];
        assign "warp_id" (ident "lane_id");
        ForStmt
          {
            init = None;
            cond =
              Some
                (bin ~ty:J_type.bool (ident "warp_id") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "stale_subgroup_alias_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let non_uniform_control_assignment_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
          ];
        IfStmt
          {
            cond = thread_x_lt_32 ();
            then_stmt =
              assign "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            else_stmt = Skip;
          };
        ForStmt
          {
            init = None;
            cond =
              Some
                (bin ~ty:J_type.bool (ident "warp_id") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "divergent_assignment_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let mixed_branch_subgroup_id_alias_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
          ];
        IfStmt
          {
            cond = block_x_eq_0 ();
            then_stmt = assign "warp_id" (ident "lane_id");
            else_stmt =
              assign "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
          };
        ForStmt
          {
            init = None;
            cond =
              Some
                (bin ~ty:J_type.bool (ident "warp_id") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "mixed_branch_subgroup_alias_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let one_path_branch_introduced_alias_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            undef_decl "warp_id";
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
          ];
        IfStmt
          {
            cond = block_x_eq_0 ();
            then_stmt =
              assign "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            else_stmt = assign "warp_id" (ident "lane_id");
          };
        ForStmt
          {
            init = None;
            cond =
              Some
                (bin ~ty:J_type.bool (ident "warp_id") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "one_path_branch_introduced_alias_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let else_branch_site_snapshot_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            undef_decl "warp_id";
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
          ];
        IfStmt
          {
            cond = block_x_eq_0 ();
            then_stmt =
              assign "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            else_stmt =
              ForStmt
                {
                  init = None;
                  cond =
                    Some
                      (bin ~ty:J_type.bool (ident "warp_id") "<"
                         (D_lang.Expr.IntegerLiteral 2));
                  inc = D_lang.Stmt.Skip;
                  body = syncwarp_stmt;
                };
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "else_branch_site_snapshot_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let loop_readded_subgroup_id_alias_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
          ];
        ForStmt
          {
            init = None;
            cond = Some (block_x_eq_0 ());
            body = assign "warp_id" (ident "lane_id");
            inc = assign "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
          };
        ForStmt
          {
            init = None;
            cond =
              Some
                (bin ~ty:J_type.bool (ident "warp_id") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "loop_readded_subgroup_alias_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let one_path_loop_introduced_alias_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [ decl "tid" (member_expr "threadIdx" "x"); undef_decl "warp_id" ];
        ForStmt
          {
            init = None;
            cond = Some (block_x_eq_0 ());
            body = assign "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            inc = D_lang.Stmt.Skip;
          };
        ForStmt
          {
            init = None;
            cond =
              Some
                (bin ~ty:J_type.bool (ident "warp_id") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "one_path_loop_introduced_alias_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let one_path_loop_increment_alias_program () : D_lang.Program.t =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [ decl "tid" (member_expr "threadIdx" "x"); undef_decl "warp_id" ];
        ForStmt
          {
            init = None;
            cond = Some (block_x_eq_0 ());
            body = D_lang.Stmt.Skip;
            inc = assign "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
          };
        ForStmt
          {
            init = None;
            cond =
              Some
                (bin ~ty:J_type.bool (ident "warp_id") "<"
                   (D_lang.Expr.IntegerLiteral 2));
            inc = D_lang.Stmt.Skip;
            body = syncwarp_stmt;
          };
      ]
  in
  [
    D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
    D_lang.Def.Kernel
      {
        ty = "void ()";
        name = "one_path_loop_increment_alias_syncwarp";
        code;
        type_params = [];
        template_args = [];
        params = [];
        attribute = D_lang.KernelAttr.Default;
      };
  ]

let test_source_subgroup_id_alias_reassignment_rejects () : unit =
  let result =
    check_source_program_uniformity (stale_subgroup_id_alias_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "reassigned subgroup-id alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "lane reassignment rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_non_uniform_control_assignment_rejects () : unit =
  let result =
    check_source_program_uniformity (non_uniform_control_assignment_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "divergent assignment control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "threadIdx.x-guarded assignment rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_mixed_branch_subgroup_id_alias_rejects () : unit =
  let result =
    check_source_program_uniformity (mixed_branch_subgroup_id_alias_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "mixed-branch alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "mixed branch rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_one_path_branch_introduced_alias_rejects () : unit =
  let result =
    check_source_program_uniformity
      (one_path_branch_introduced_alias_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "one-path branch alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "one-path branch introduction rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_else_branch_site_snapshot_rejects () : unit =
  let result =
    check_source_program_uniformity (else_branch_site_snapshot_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "else-branch alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "else-branch sibling alias contamination rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_loop_body_invalidation_rejects () : unit =
  let result =
    check_source_program_uniformity (loop_readded_subgroup_id_alias_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "loop-readded alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "loop body invalidation rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_one_path_loop_introduced_alias_rejects () : unit =
  let result =
    check_source_program_uniformity (one_path_loop_introduced_alias_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "one-path loop alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "one-path loop introduction rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_one_path_loop_increment_alias_rejects () : unit =
  let result =
    check_source_program_uniformity (one_path_loop_increment_alias_program ())
  in
  let site = single_site_result result in
  Alcotest.(check bool)
    "one-path loop increment alias control reaches checker" true
    (Uniformity.site_result_control_depth site > 0);
  Alcotest.(check string)
    "one-path loop increment rejects" "undefined_behavior"
    (Uniformity.verdict_to_string (Uniformity.function_verdict result))

let test_source_loop_and_switch_controls_reach_uniformity_checker () : unit =
  check_source_guarded_syncwarp_rejects "while_guarded_syncwarp"
    (D_lang.Stmt.WhileStmt { cond = thread_x_lt_32 (); body = syncwarp_stmt });
  check_source_guarded_syncwarp_rejects "switch_guarded_syncwarp"
    (D_lang.Stmt.SwitchStmt
       {
         cond = thread_x_mod_32 ();
         body = D_lang.Stmt.DefaultStmt syncwarp_stmt;
       })

let test_full_verdict_composition_keeps_components_distinct () : unit =
  Alcotest.(check string)
    "memory failure blocks full DRF" "not_drf"
    (Uniformity.full_verdict_to_string
       (Uniformity.compose Uniformity.Memory_not_drf
          Uniformity.Subgroup_uniformity_drf));
  Alcotest.(check string)
    "uniformity UB blocks full DRF" "not_drf"
    (Uniformity.full_verdict_to_string
       (Uniformity.compose Uniformity.Memory_drf
          Uniformity.Subgroup_uniformity_undefined_behavior));
  Alcotest.(check string)
    "both components pass" "drf"
    (Uniformity.full_verdict_to_string
       (Uniformity.compose Uniformity.Memory_drf
          Uniformity.Subgroup_uniformity_drf))

let test_summary_uses_rust_oracle_component_names () : unit =
  let result =
    Uniformity.check_kernel (kernel [ matrix_store 30 ]) |> expect_uniformity_ok
  in
  let summary = Uniformity.summary_lines ~memory:Uniformity.Memory_drf result in
  Alcotest.(check bool)
    "summary includes subgroup_uniformity" true
    (List.exists (String.equal "subgroup_uniformity: drf") summary);
  Alcotest.(check bool)
    "summary includes drf_full" true
    (List.exists (String.equal "drf_full: drf") summary);
  Alcotest.(check bool)
    "summary includes site evidence" true
    (List.exists
       (Stage0.Common.contains ~substring:"site#30[store_matrix_sync]")
       summary)

let tests : unit Alcotest.test_case list =
  [
    ( "top-level subgroup and matrix sites",
      `Quick,
      test_top_level_subgroup_and_matrix_sites_are_uniform );
    ( "thread-indexed control rejects",
      `Quick,
      test_thread_index_control_is_undefined_behavior );
    ( "subgroup index control accepts",
      `Quick,
      test_subgroup_index_control_is_uniform_with_explicit_config );
    ( "subgroup index control requires config",
      `Quick,
      test_subgroup_index_control_requires_config );
    ( "thread y/z control requires config",
      `Quick,
      test_thread_yz_control_requires_config );
    ( "thread y/z control accepts with config",
      `Quick,
      test_thread_yz_control_is_uniform_with_explicit_config );
    ( "lane and unknown local reject",
      `Quick,
      test_lane_and_unknown_local_control_are_non_uniform );
    ( "subgroup result control rejects",
      `Quick,
      test_subgroup_collective_result_control_is_non_uniform );
    ( "source threadIdx.x guard rejects",
      `Quick,
      test_source_thread_x_guard_reaches_uniformity_checker );
    ( "source uniform param member control accepts",
      `Quick,
      test_source_uniform_kernel_param_member_control_accepts );
    ( "source subgroup-id alias control accepts",
      `Quick,
      test_source_subgroup_id_alias_control_accepts );
    ( "source subgroup-id alias requires matching size",
      `Quick,
      test_source_subgroup_id_alias_requires_matching_size );
    ( "source subgroup-id alias reassignment rejects",
      `Quick,
      test_source_subgroup_id_alias_reassignment_rejects );
    ( "source non-uniform control assignment rejects",
      `Quick,
      test_source_non_uniform_control_assignment_rejects );
    ( "source mixed-branch subgroup-id alias rejects",
      `Quick,
      test_source_mixed_branch_subgroup_id_alias_rejects );
    ( "source one-path branch introduced alias rejects",
      `Quick,
      test_source_one_path_branch_introduced_alias_rejects );
    ( "source else-branch site snapshot rejects",
      `Quick,
      test_source_else_branch_site_snapshot_rejects );
    ( "source loop body invalidation rejects",
      `Quick,
      test_source_loop_body_invalidation_rejects );
    ( "source one-path loop introduced alias rejects",
      `Quick,
      test_source_one_path_loop_introduced_alias_rejects );
    ( "source one-path loop increment alias rejects",
      `Quick,
      test_source_one_path_loop_increment_alias_rejects );
    ( "source loop/switch controls reject",
      `Quick,
      test_source_loop_and_switch_controls_reach_uniformity_checker );
    ( "full verdict composition",
      `Quick,
      test_full_verdict_composition_keeps_components_distinct );
    ( "summary component names",
      `Quick,
      test_summary_uses_rust_oracle_component_names );
  ]

let () = Alcotest.run "Subgroup_uniformity" [ ("subgroup_uniformity", tests) ]
