open Inference
open Protocols
module Source = Subgroup_source
module SM = Subgroup_matrix

let ty (name : string) : J_type.t = J_type.from_string name

let ident ?(kind = Decl_expr.Kind.Var) ?(ty = J_type.int) (name : string) :
    D_lang.Expr.t =
  Ident { name = Variable.from_name name; ty; kind }

let member_expr ?(ty = J_type.int) (base : string) (field : string) :
    D_lang.Expr.t =
  D_lang.Expr.MemberExpr { base = ident base; name = field; ty }

let call_expr (name : string) (args : D_lang.Expr.t list) : D_lang.Expr.t =
  CallExpr
    {
      func = ident ~kind:Decl_expr.Kind.Function ~ty:J_type.void name;
      args;
      ty = J_type.void;
    }

let var (name : string) : Variable.t = Variable.from_name name

let ty_var ?(ty = J_type.int) (name : string) : Ty_variable.t =
  Ty_variable.make ~name:(var name) ~ty

let kernel_param ?(ty = J_type.int) (name : string) : D_lang.Param.t =
  D_lang.Param.make ~ty_var:(ty_var ~ty name) ~is_used:true ~is_shared:false

let decl ?(ty = J_type.int) (name : string) (expr : D_lang.Expr.t) :
    D_lang.Decl.t =
  D_lang.Decl.from_expr (ty_var ~ty name) expr

let undef_decl ?(ty = J_type.int) (name : string) : D_lang.Decl.t =
  D_lang.Decl.from_undef (ty_var ~ty name)

let kernel ?(attribute = D_lang.KernelAttr.Default) ?(params = [])
    ?(type_params = []) ?(ty = "void ()") (name : string) (code : D_lang.Stmt.t)
    : D_lang.Kernel.t =
  { ty; name; code; type_params; params; attribute }

let pointer_ty : J_type.t = ty "float *"
let half_pointer_ty : J_type.t = ty "half *"

let subgroup_config () : SM.Target_config.t =
  SM.Target_config.subgroup_size_exn 32 |> SM.Target_config.cuda_x_contiguous

let pointer_offset (base : string) (offset : string) : D_lang.Expr.t =
  D_lang.Expr.BinaryOperator
    {
      lhs = ident ~ty:pointer_ty base;
      opcode = "+";
      rhs = ident offset;
      ty = pointer_ty;
    }

let matrix_a_fragment_type : J_type.t =
  ty
    "nvcuda::wmma::fragment<nvcuda::wmma::matrix_a, 16, 16, 16, float, \
     nvcuda::wmma::row_major>"

let accumulator_fragment_type : J_type.t =
  ty "nvcuda::wmma::fragment<nvcuda::wmma::accumulator, 16, 8, 16, float>"

let desugared_accumulator_fragment_type : J_type.t =
  J_type.from_json
    (`Assoc
       [
         ("qualType", `String "acc_frag_t");
         ( "desugaredQualType",
           `String
             "nvcuda::wmma::fragment<nvcuda::wmma::accumulator, 16, 8, 16, \
              float>" );
       ])

let expect_route_ok (result : (Source.routed_kernel list, Source.error) result)
    : Source.routed_kernel list =
  match result with
  | Ok kernels -> kernels
  | Error error -> Alcotest.fail (Source.error_to_string error)

let expect_unsupported_matrix_call ~(op : string) ~(reason : string)
    (code : D_lang.Stmt.t) : unit =
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "malformed_wmma" code) ]
  with
  | Error
      (Source.Unsupported_matrix_call { op = actual_op; reason = actual; _ }) ->
      Alcotest.(check string) "matrix op" op actual_op;
      Alcotest.(check bool)
        "matrix error reason" true
        (Stage0.Common.contains ~substring:reason actual)
  | Error error -> Alcotest.fail (Source.error_to_string error)
  | Ok _ -> Alcotest.fail "malformed matrix call was accepted"

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

let block_x_eq_0 () : D_lang.Expr.t =
  D_lang.Expr.BinaryOperator
    {
      lhs = member_expr "blockIdx" "x";
      opcode = "==";
      rhs = D_lang.Expr.IntegerLiteral 0;
      ty = J_type.bool;
    }

let bin ?(ty = J_type.int) (lhs : D_lang.Expr.t) (opcode : string)
    (rhs : D_lang.Expr.t) : D_lang.Expr.t =
  D_lang.Expr.BinaryOperator { lhs; opcode; rhs; ty }

let assign (name : string) (rhs : D_lang.Expr.t) : D_lang.Stmt.t =
  D_lang.Stmt.SExpr (bin (ident name) "=" rhs)

let pointer_assign (name : string) (rhs : D_lang.Expr.t) : D_lang.Stmt.t =
  D_lang.Stmt.SExpr (bin ~ty:pointer_ty (ident ~ty:pointer_ty name) "=" rhs)

let syncwarp_stmt : D_lang.Stmt.t =
  D_lang.Stmt.SExpr (call_expr "__syncwarp" [])

let syncthreads_stmt : D_lang.Stmt.t =
  D_lang.Stmt.SExpr (call_expr "__syncthreads" [])

let subscript ?(ty = J_type.int) (name : string) (index : D_lang.Expr.t list) :
    D_lang.d_subscript =
  D_lang.make_subscript ~name:(var name) ~index ~ty
    ~location:Stage0.Location.empty

let read_stmt ?(target = "tmp") (source : D_lang.d_subscript) : D_lang.Stmt.t =
  D_lang.Stmt.ReadAccessStmt { target = var target; source; ty = C_type.int }

let write_stmt ?payload (target : D_lang.d_subscript) : D_lang.Stmt.t =
  D_lang.Stmt.WriteAccessStmt { target; source = ident "value"; payload }

let atomic_add () : Atomic.t =
  match Atomic.from_name (var "atomicAdd") with
  | Some atomic -> atomic
  | None -> Alcotest.fail "atomicAdd should be a known Faial atomic"

let atomic_stmt ?(target = "old") (source : D_lang.d_subscript) : D_lang.Stmt.t
    =
  D_lang.Stmt.AtomicAccessStmt
    { target = var target; source; atomic = atomic_add (); ty = C_type.int }

let expect_single_site_control (name : string) (code : D_lang.Stmt.t) :
    Source.site_control =
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel name code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      match subgroup.site_controls with
      | [ control ] -> control
      | controls ->
          Alcotest.fail
            (Printf.sprintf "expected one site control, got %d"
               (List.length controls)))
  | _ -> Alcotest.fail "guarded subgroup op did not route to subgroup carrier"

let site_control_text (control : Source.site_control) : string =
  control.conditions |> List.map Exp.b_to_string |> String.concat "\n"

let site_control_memory_text (control : Source.site_control) : string =
  Source.site_control_memory_conditions control
  |> List.map Exp.b_to_string |> String.concat "\n"

let site_control_has_uniform_var (name : string) (control : Source.site_control)
    : bool =
  Variable.Set.mem (var name) control.uniform_vars

let variable_set_has (name : string) (vars : Variable.Set.t) : bool =
  Variable.Set.mem (var name) vars

let test_non_wmma_routes_to_ordinary_imp_without_subgroup_config () : unit =
  let code =
    D_lang.Stmt.WriteAccessStmt
      {
        target =
          D_lang.make_subscript ~name:(var "dst")
            ~index:[ ident "i" ]
            ~ty:J_type.int ~location:Stage0.Location.empty;
        source = ident "tid";
        payload = None;
      }
  in
  match
    Source.route_program [ D_lang.Def.Kernel (kernel "plain_cuda" code) ]
    |> expect_route_ok
  with
  | [ Source.Ordinary_imp kernel ] ->
      Alcotest.(check string) "ordinary kernel name" "plain_cuda" kernel.name
  | _ -> Alcotest.fail "non-WMMA kernel did not stay on ordinary Imp route"

let test_missing_config_fails_only_on_subgroup_path () : unit =
  let code = D_lang.Stmt.SExpr (call_expr "__syncwarp" []) in
  match Source.route_program [ D_lang.Def.Kernel (kernel "syncwarp" code) ] with
  | Error (Source.Missing_subgroup_config { kernel }) ->
      Alcotest.(check string) "subgroup kernel name" "syncwarp" kernel
  | Ok _ -> Alcotest.fail "subgroup path unexpectedly accepted missing config"
  | Error error -> Alcotest.fail (Source.error_to_string error)

let test_wmma_source_generates_site_summary () : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let frag_c = ident ~ty:desugared_accumulator_fragment_type "frag_c" in
  let code =
    D_lang.Stmt.from_list
      [
        SExpr (call_expr "__syncwarp" []);
        SExpr
          (call_expr "load_matrix_sync"
             [ frag_a; pointer_offset "tile" "d"; IntegerLiteral 16 ]);
        SExpr (call_expr "mma_sync" [ frag_c; frag_a; frag_a; frag_c ]);
        DeclStmt
          [
            decl ~ty:half_pointer_ty "pv_tile"
              (pointer_offset "pv_shmem" "warp_offset");
          ];
        SExpr
          (call_expr "store_matrix_sync"
             [
               ident ~ty:half_pointer_ty "pv_tile";
               frag_c;
               IntegerLiteral 8;
               ident "nvcuda::wmma::mem_row_major";
             ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "flash_attn_wmma_mirror_kernel" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      let kernel = subgroup.matrix_kernel in
      let summary = Source.kernel_site_summary kernel in
      let rendered = String.concat "\n" summary in
      Alcotest.(check bool)
        "subgroup path records a subgroup boundary" true
        (SM.Kernel.has_subgroup_boundary kernel);
      Alcotest.(check int)
        "summary includes header plus four sites" 5 (List.length summary);
      Alcotest.(check bool)
        "summary keeps explicit target config" true
        (Stage0.Common.contains
           ~substring:"cuda-like(threadIdx.x-contiguous(size=32))" rendered);
      Alcotest.(check bool)
        "summary keeps subgroup barrier site" true
        (Stage0.Common.contains ~substring:"site#0[__syncwarp]" rendered);
      Alcotest.(check bool)
        "summary keeps matrix load footprint" true
        (Stage0.Common.contains
           ~substring:"tile[d] footprint<16x16, ldm=16, row_major" rendered);
      Alcotest.(check bool)
        "summary keeps matrix store footprint" true
        (Stage0.Common.contains
           ~substring:"pv_shmem[warp_offset] footprint<16x8, ldm=8, row_major"
           rendered)
  | _ -> Alcotest.fail "WMMA kernel did not route to subgroup/matrix carrier"

let test_wmma_artifact_summary_preserves_matrix_footprints () : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let frag_c = ident ~ty:desugared_accumulator_fragment_type "frag_c" in
  let code =
    D_lang.Stmt.from_list
      [
        SExpr
          (call_expr "load_matrix_sync"
             [ frag_a; pointer_offset "tile" "d"; IntegerLiteral 16 ]);
        SExpr
          (call_expr "store_matrix_sync"
             [
               ident ~ty:half_pointer_ty "dst";
               frag_c;
               IntegerLiteral 8;
               ident "nvcuda::wmma::mem_row_major";
             ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "artifact_probe" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      let kernel = subgroup.matrix_kernel in
      let artifact = Source.kernel_artifact_summary kernel in
      let footprints = Source.kernel_matrix_footprint_summary kernel in
      let rendered = String.concat "\n" artifact in
      Alcotest.(check int)
        "matrix footprint summary has load and store effects" 2
        (List.length footprints);
      Alcotest.(check bool)
        "artifact declares stable format version" true
        (Stage0.Common.contains ~substring:"ocaml-subgroup-matrix-v1" rendered);
      Alcotest.(check bool)
        "artifact keeps load indexed access" true
        (Stage0.Common.contains ~substring:"indexed=ro tile[" rendered);
      Alcotest.(check bool)
        "artifact keeps store indexed access" true
        (Stage0.Common.contains ~substring:"indexed=rw dst[" rendered);
      Alcotest.(check bool)
        "artifact keeps rectangular bounds witnesses" true
        (Stage0.Common.contains ~substring:"matrix_row_0" rendered
        && Stage0.Common.contains ~substring:"matrix_col_1" rendered)
  | _ -> Alcotest.fail "artifact probe did not route to subgroup/matrix carrier"

let test_subgroup_path_records_ordinary_memory_effects () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        SExpr (call_expr "__syncwarp" []);
        write_stmt (subscript "dst" [ ident "i" ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "subgroup_with_ordinary_memory" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      match subgroup.ordinary_memory_effects with
      | [ memory_effect ] ->
          Alcotest.(check string)
            "ordinary memory effect" "write dst[i]"
            (Source.ordinary_memory_effect_to_string memory_effect);
          Alcotest.(check string)
            "ordinary structured access" "rw dst[i]"
            (Access.to_string memory_effect.access);
          Alcotest.(check int) "ordinary source site id" 0 memory_effect.site.id;
          Alcotest.(check int)
            "ordinary source order follows syncwarp site" 1
            memory_effect.site.source_order;
          Alcotest.(check int)
            "ordinary workgroup phase" 0 memory_effect.phase.workgroup;
          Alcotest.(check (list int))
            "ordinary subgroup phase follows syncwarp" [ 0 ]
            memory_effect.phase.subgroup;
          Alcotest.(check string)
            "ordinary target config"
            "cuda-like(threadIdx.x-contiguous(size=32))"
            (SM.Target_config.to_string memory_effect.target_config)
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected one ordinary memory effect, got %d"
               (List.length effects)))
  | _ -> Alcotest.fail "subgroup kernel did not route to subgroup carrier"

let test_source_order_tracks_interleaved_subgroup_and_ordinary_events () : unit
    =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let code =
    D_lang.Stmt.from_list
      [
        syncwarp_stmt;
        write_stmt (subscript "tile" [ ident "i" ]);
        SExpr
          (call_expr "load_matrix_sync"
             [ frag_a; pointer_offset "tile" "d"; IntegerLiteral 16 ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "source_order_interleaving" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      match (subgroup.site_controls, subgroup.ordinary_memory_effects) with
      | [ sync_control; load_control ], [ ordinary ] ->
          Alcotest.(check int)
            "__syncwarp source order" 0 sync_control.source_order;
          Alcotest.(check int)
            "ordinary source order" 1 ordinary.site.source_order;
          Alcotest.(check int) "matrix source order" 2 load_control.source_order
      | controls, effects ->
          Alcotest.fail
            (Printf.sprintf
               "expected two site controls and one ordinary effect, got %d \
                controls and %d effects"
               (List.length controls) (List.length effects)))
  | _ -> Alcotest.fail "source-order kernel did not route to subgroup carrier"

let test_ordinary_memory_effects_preserve_control_and_phases () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        IfStmt
          {
            cond = thread_x_lt_32 ();
            then_stmt = write_stmt (subscript "dst" [ ident "i" ]);
            else_stmt = D_lang.Stmt.Skip;
          };
        syncthreads_stmt;
        read_stmt (subscript "dst" [ ident "j" ]);
        syncwarp_stmt;
        atomic_stmt (subscript "dst" [ ident "k" ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "ordinary_metadata" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> begin
      match subgroup.ordinary_memory_effects with
      | [ write; read; atomic ] ->
          Alcotest.(check string)
            "write effect" "write dst[i]"
            (Source.ordinary_memory_effect_to_string write);
          Alcotest.(check string)
            "write control" "threadIdx.x < 32"
            (write.source_conditions |> List.map Exp.b_to_string
           |> String.concat "\n");
          Alcotest.(check int) "write W phase" 0 write.phase.workgroup;
          Alcotest.(check (list int)) "write S phase" [] write.phase.subgroup;
          Alcotest.(check string)
            "read effect" "read dst[j]"
            (Source.ordinary_memory_effect_to_string read);
          Alcotest.(check int) "read W phase" 1 read.phase.workgroup;
          Alcotest.(check (list int)) "read S phase" [] read.phase.subgroup;
          Alcotest.(check string)
            "atomic effect" "atomic dst[k]"
            (Source.ordinary_memory_effect_to_string atomic);
          Alcotest.(check int) "atomic W phase" 1 atomic.phase.workgroup;
          Alcotest.(check (list int))
            "atomic S phase" [ 1 ] atomic.phase.subgroup;
          Alcotest.(check bool)
            "ordinary summary exposes metadata" true
            (Stage0.Common.contains ~substring:"phase=W1/S[1]"
               (Source.ordinary_memory_effect_summary atomic))
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected three ordinary memory effects, got %d"
               (List.length effects))
    end
  | _ -> Alcotest.fail "ordinary metadata kernel did not route to subgroup"

let test_warp_helpers_advance_ordinary_memory_subgroup_phase () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        read_stmt ~target:"prev_max"
          (subscript "row_max_shmem" [ ident "q_tile_row" ]);
        assign "final_max"
          (call_expr "warp_max"
             [ call_expr "fmaxf" [ ident "final_max"; ident "term" ] ]);
        assign "total_exp_term" (call_expr "warp_sum" [ ident "cur_p" ]);
        write_stmt (subscript "row_max_shmem" [ ident "q_tile_row" ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "warp_helper_ordering" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> begin
      let site_summary =
        Source.kernel_site_summary subgroup.matrix_kernel |> String.concat "\n"
      in
      Alcotest.(check bool)
        "records warp_max as subgroup collective" true
        (Stage0.Common.contains ~substring:"site#0[warp_max]" site_summary
        && Stage0.Common.contains ~substring:"collective<reduce:max>"
             site_summary);
      Alcotest.(check bool)
        "records warp_sum as subgroup collective" true
        (Stage0.Common.contains ~substring:"site#1[warp_sum]" site_summary
        && Stage0.Common.contains ~substring:"collective<reduce:add>"
             site_summary);
      match subgroup.ordinary_memory_effects with
      | [ read; write ] ->
          Alcotest.(check int)
            "read source order before helpers" 0 read.site.source_order;
          Alcotest.(check (list int))
            "read sees pre-helper subgroup phase" [] read.phase.subgroup;
          Alcotest.(check int)
            "write source order after helpers" 3 write.site.source_order;
          Alcotest.(check (list int))
            "write sees both helper subgroup boundaries" [ 0; 1 ]
            write.phase.subgroup
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected read/write ordinary effects, got %d"
               (List.length effects))
    end
  | _ -> Alcotest.fail "warp helper kernel did not route to subgroup"

let test_ordinary_memory_effects_preserve_loop_ownership_facts () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt [ decl "tid" (member_expr "threadIdx" "x") ];
        ForStmt
          {
            init = Some (D_lang.ForInit.Decls [ decl "i" (ident "tid") ]);
            cond =
              Some
                (bin ~ty:J_type.bool (ident "i") "<"
                   (D_lang.Expr.IntegerLiteral 16));
            inc = assign "i" (bin (ident "i") "+" (ident "WG_SIZE"));
            body = write_stmt (subscript "dst" [ ident "i" ]);
          };
        syncwarp_stmt;
      ]
  in
  let program =
    [
      D_lang.Def.Declaration (decl "WG_SIZE" (D_lang.Expr.IntegerLiteral 64));
      D_lang.Def.Kernel (kernel "ordinary_loop_ownership" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> begin
      match subgroup.ordinary_memory_effects with
      | [ write ] ->
          let conditions =
            write.source_conditions |> List.map Exp.b_to_string
            |> String.concat "\n"
          in
          Alcotest.(check bool)
            "records scalar alias for thread coordinate" true
            (Stage0.Common.contains ~substring:"tid == threadIdx.x" conditions);
          Alcotest.(check bool)
            "records loop stride ownership" true
            (Stage0.Common.contains ~substring:"i - tid" conditions
            && Stage0.Common.contains ~substring:"% WG_SIZE" conditions);
          Alcotest.(check bool)
            "records loop lower ownership" true
            (Stage0.Common.contains ~substring:"tid <= i" conditions);
          Alcotest.(check bool)
            "records loop bound guard" true
            (Stage0.Common.contains ~substring:"i < 16" conditions);
          Alcotest.(check bool)
            "loop variable init alias does not leak as invariant" false
            (Stage0.Common.contains ~substring:"i == tid" conditions)
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected one ordinary memory effect, got %d"
               (List.length effects))
    end
  | _ -> Alcotest.fail "ordinary loop kernel did not route to subgroup"

let test_ordinary_memory_effects_preserve_varying_local_aliases () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
            decl "q_tile_row" (bin (ident "tid") "/" (ident "WARP_SIZE"));
          ];
        ForStmt
          {
            init =
              Some (D_lang.ForInit.Decls [ decl "elem_idx" (ident "lane_id") ]);
            cond =
              Some
                (bin ~ty:J_type.bool (ident "elem_idx") "<" (ident "HEAD_DIM_V"));
            inc =
              assign "elem_idx" (bin (ident "elem_idx") "+" (ident "WARP_SIZE"));
            body =
              D_lang.Stmt.from_list
                [
                  DeclStmt
                    [
                      decl "idx"
                        (bin
                           (bin (ident "q_tile_row") "*" (ident "HEAD_DIM_V"))
                           "+" (ident "elem_idx"));
                    ];
                  write_stmt (subscript "dst" [ ident "idx" ]);
                ];
          };
        syncwarp_stmt;
      ]
  in
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Declaration (decl "HEAD_DIM_V" (D_lang.Expr.IntegerLiteral 64));
      D_lang.Def.Kernel (kernel "ordinary_varying_alias" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> begin
      match subgroup.ordinary_memory_effects with
      | [ write ] ->
          let conditions =
            write.source_conditions |> List.map Exp.b_to_string
            |> String.concat "\n"
          in
          Alcotest.(check bool)
            "records alias assigned under lane-varying loop control" true
            (Stage0.Common.contains ~substring:"idx ==" conditions
            && Stage0.Common.contains ~substring:"q_tile_row * HEAD_DIM_V"
                 conditions
            && Stage0.Common.contains ~substring:"elem_idx" conditions);
          Alcotest.(check bool)
            "records lane-vector loop ownership" true
            (Stage0.Common.contains ~substring:"elem_idx - lane_id" conditions
            && Stage0.Common.contains ~substring:"% WARP_SIZE" conditions);
          Alcotest.(check bool)
            "records lane alias dependency" true
            (Stage0.Common.contains ~substring:"lane_id == (tid % WARP_SIZE)"
               conditions)
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected one ordinary memory effect, got %d"
               (List.length effects))
    end
  | _ -> Alcotest.fail "ordinary varying alias kernel did not route to subgroup"

let test_ordinary_memory_drops_aliases_depending_on_reassigned_scalar () : unit
    =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "lane" (bin (ident "tid") "%" (ident "WARP_SIZE"));
            decl "base_idx" (ident "lane");
            decl "idx"
              (bin (ident "base_idx") "+" (D_lang.Expr.IntegerLiteral 1));
          ];
        assign "lane" (D_lang.Expr.IntegerLiteral 0);
        write_stmt (subscript "dst" [ ident "idx" ]);
        syncwarp_stmt;
      ]
  in
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "ordinary_reassigned_dependency" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> begin
      match subgroup.ordinary_memory_effects with
      | [ write ] ->
          let conditions =
            write.source_conditions |> List.map Exp.b_to_string
            |> String.concat "\n"
          in
          Alcotest.(check bool)
            "drops stale transitive idx alias" false
            (Stage0.Common.contains ~substring:"idx ==" conditions);
          Alcotest.(check bool)
            "drops stale base_idx alias" false
            (Stage0.Common.contains ~substring:"base_idx ==" conditions)
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected one ordinary memory effect, got %d"
               (List.length effects))
    end
  | _ ->
      Alcotest.fail
        "ordinary reassigned dependency kernel did not route to subgroup"

let test_ordinary_memory_branch_only_alias_does_not_escape () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [ decl "tid" (member_expr "threadIdx" "x"); decl "idx" (ident "tid") ];
        IfStmt
          {
            cond = block_x_eq_0 ();
            then_stmt = assign "idx" (D_lang.Expr.IntegerLiteral 0);
            else_stmt = Skip;
          };
        write_stmt (subscript "dst" [ ident "idx" ]);
        syncwarp_stmt;
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "ordinary_branch_alias" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> begin
      match subgroup.ordinary_memory_effects with
      | [ write ] ->
          let conditions =
            write.source_conditions |> List.map Exp.b_to_string
            |> String.concat "\n"
          in
          Alcotest.(check bool)
            "drops branch-only idx alias" false
            (Stage0.Common.contains ~substring:"idx ==" conditions);
          Alcotest.(check bool)
            "drops dependency reachable only through idx" false
            (Stage0.Common.contains ~substring:"tid ==" conditions)
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected one ordinary memory effect, got %d"
               (List.length effects))
    end
  | _ -> Alcotest.fail "ordinary branch alias kernel did not route to subgroup"

let test_memory_globals_track_task_invariant_dst_arithmetic () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
            decl "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            decl "batch_idx"
              (bin
                 (member_expr "blockIdx" "x")
                 "/"
                 (member_expr "params" "n_heads"));
            decl "dst2_stride"
              (bin (ident "HEAD_DIM_V") "*" (member_expr "params" "n_heads"));
            decl "dst3_stride"
              (bin (ident "dst2_stride") "*" (member_expr "params" "seq_len_q"));
            decl "dst_batch_offset"
              (bin
                 (member_expr "params" "offset_dst")
                 "+"
                 (bin (ident "batch_idx") "*" (ident "dst3_stride")));
            decl "dst_global_offset"
              (bin (ident "dst_batch_offset") "+"
                 (bin
                    (member_expr "params" "head_idx")
                    "*" (ident "dst2_stride")));
          ];
        ForStmt
          {
            init =
              Some
                (D_lang.ForInit.Decls [ decl "q_tile_row" (ident "warp_id") ]);
            cond =
              Some
                (bin ~ty:J_type.bool (ident "q_tile_row") "<" (ident "Q_TILE"));
            inc =
              assign "q_tile_row"
                (bin (ident "q_tile_row") "+" (ident "NUM_SUBGROUPS"));
            body =
              D_lang.Stmt.from_list
                [
                  DeclStmt
                    [
                      decl "row_base"
                        (bin
                           (ident "dst_global_offset")
                           "+"
                           (bin (ident "q_tile_row") "*" (ident "dst2_stride")));
                    ];
                  ForStmt
                    {
                      init =
                        Some
                          (D_lang.ForInit.Decls
                             [
                               decl "elem_base"
                                 (bin (ident "lane_id") "*"
                                    (D_lang.Expr.IntegerLiteral 4));
                             ]);
                      cond =
                        Some
                          (bin ~ty:J_type.bool (ident "elem_base") "<"
                             (ident "HEAD_DIM_V"));
                      inc =
                        assign "elem_base"
                          (bin (ident "elem_base") "+"
                             (bin (ident "WARP_SIZE") "*"
                                (D_lang.Expr.IntegerLiteral 4)));
                      body =
                        write_stmt
                          (subscript "dst"
                             [ bin (ident "row_base") "+" (ident "elem_base") ]);
                    };
                ];
          };
        syncwarp_stmt;
      ]
  in
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Declaration (decl "HEAD_DIM_V" (D_lang.Expr.IntegerLiteral 64));
      D_lang.Def.Declaration (decl "Q_TILE" (D_lang.Expr.IntegerLiteral 16));
      D_lang.Def.Declaration
        (decl "NUM_SUBGROUPS" (D_lang.Expr.IntegerLiteral 2));
      D_lang.Def.Kernel
        (kernel
           ~params:
             [
               kernel_param ~ty:(ty "FlashParams") "params";
               kernel_param ~ty:half_pointer_ty "dst";
             ]
           "dst_memory_globals" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      let globals = subgroup.memory_globals in
      List.iter
        (fun name ->
          Alcotest.(check bool)
            ("memory-global " ^ name) true
            (variable_set_has name globals))
        [
          "params";
          "dst";
          "batch_idx";
          "dst2_stride";
          "dst3_stride";
          "dst_batch_offset";
          "dst_global_offset";
        ];
      List.iter
        (fun name ->
          Alcotest.(check bool)
            ("task-local " ^ name) false
            (variable_set_has name globals))
        [ "tid"; "lane_id"; "warp_id"; "q_tile_row"; "elem_base"; "row_base" ];
      let artifact_lines = Source.subgroup_kernel_artifact_summary subgroup in
      let artifact = artifact_lines |> String.concat "\n" in
      let memory_global_line =
        artifact_lines
        |> List.find_opt (String.starts_with ~prefix:"memory_globals:")
        |> Option.value ~default:""
      in
      Alcotest.(check bool)
        "artifact records memory globals" true
        (Stage0.Common.contains ~substring:"memory_globals:" artifact
        && Stage0.Common.contains ~substring:"dst2_stride" artifact);
      Alcotest.(check bool)
        "artifact does not promote subgroup-owned rows" false
        (Stage0.Common.contains ~substring:"row_base" memory_global_line);
      match subgroup.ordinary_memory_effects with
      | [ write ] ->
          let conditions =
            write.source_conditions |> List.map Exp.b_to_string
            |> String.concat "\n"
          in
          Alcotest.(check bool)
            "records division definedness for dst parameter stride" true
            (Stage0.Common.contains ~substring:"params.n_heads != 0" conditions)
      | effects ->
          Alcotest.fail
            (Printf.sprintf "expected one dst memory effect, got %d"
               (List.length effects)))
  | _ -> Alcotest.fail "dst arithmetic kernel did not route to subgroup"

let test_memory_globals_join_drops_thread_dependent_branch_assignment () : unit
    =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "idx" (member_expr "params" "offset_dst");
          ];
        IfStmt
          {
            cond = block_x_eq_0 ();
            then_stmt = assign "idx" (member_expr "params" "n_heads");
            else_stmt = assign "idx" (ident "tid");
          };
        write_stmt (subscript "dst" [ ident "idx" ]);
        syncwarp_stmt;
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [
        D_lang.Def.Kernel
          (kernel
             ~params:
               [
                 kernel_param ~ty:(ty "FlashParams") "params";
                 kernel_param ~ty:half_pointer_ty "dst";
               ]
             "branch_memory_globals" code);
      ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      Alcotest.(check bool)
        "params remains memory-global" true
        (variable_set_has "params" subgroup.memory_globals);
      Alcotest.(check bool)
        "branch join drops thread-dependent idx" false
        (variable_set_has "idx" subgroup.memory_globals)
  | _ -> Alcotest.fail "branch memory-global kernel did not route to subgroup"

let test_matrix_memory_conditions_drop_aliases_depending_on_reassigned_scalar ()
    : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "lane" (bin (ident "tid") "%" (ident "WARP_SIZE"));
            decl "base_idx" (ident "lane");
            decl "idx"
              (bin (ident "base_idx") "+" (D_lang.Expr.IntegerLiteral 1));
          ];
        assign "lane" (D_lang.Expr.IntegerLiteral 0);
        SExpr
          (call_expr "load_matrix_sync"
             [ frag_a; pointer_offset "tile" "idx"; IntegerLiteral 16 ]);
      ]
  in
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "matrix_reassigned_dependency" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> begin
      match subgroup.site_controls with
      | [ control ] ->
          let conditions = site_control_memory_text control in
          Alcotest.(check bool)
            "drops stale transitive idx memory alias" false
            (Stage0.Common.contains ~substring:"idx ==" conditions);
          Alcotest.(check bool)
            "drops stale base_idx memory alias" false
            (Stage0.Common.contains ~substring:"base_idx ==" conditions)
      | controls ->
          Alcotest.fail
            (Printf.sprintf "expected one matrix site control, got %d"
               (List.length controls))
    end
  | _ ->
      Alcotest.fail
        "matrix reassigned dependency kernel did not route to subgroup"

let test_matrix_pointer_alias_depending_on_reassigned_scalar_fails () : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "lane" (bin (ident "tid") "%" (ident "WARP_SIZE"));
            decl "base_idx" (ident "lane");
            decl "idx"
              (bin (ident "base_idx") "+" (D_lang.Expr.IntegerLiteral 1));
            decl ~ty:pointer_ty "tile_ptr" (pointer_offset "tile" "idx");
          ];
        assign "lane" (D_lang.Expr.IntegerLiteral 0);
        SExpr
          (call_expr "load_matrix_sync"
             [ frag_a; ident ~ty:pointer_ty "tile_ptr"; IntegerLiteral 16 ]);
      ]
  in
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "matrix_reassigned_pointer_alias" code);
    ]
  in
  match Source.route_program ~target_config:(subgroup_config ()) program with
  | Error (Source.Unsupported_expression { context; expr }) ->
      Alcotest.(check bool)
        "reports invalidated pointer alias" true
        (Stage0.Common.contains ~substring:"invalidated pointer alias" context);
      Alcotest.(check bool)
        "reports pointer local" true
        (Stage0.Common.contains ~substring:"tile_ptr" expr)
  | Error error -> Alcotest.fail (Source.error_to_string error)
  | Ok _ -> Alcotest.fail "stale pointer alias was accepted"

let test_sibling_branch_pointer_alias_does_not_leak () : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "idx" (D_lang.Expr.IntegerLiteral 4);
            undef_decl ~ty:pointer_ty "tile_ptr";
          ];
        IfStmt
          {
            cond = block_x_eq_0 ();
            then_stmt = pointer_assign "tile_ptr" (pointer_offset "tile" "idx");
            else_stmt =
              SExpr
                (call_expr "load_matrix_sync"
                   [
                     frag_a; ident ~ty:pointer_ty "tile_ptr"; IntegerLiteral 16;
                   ]);
          };
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "sibling_pointer_alias_leak" code) ]
  with
  | Error (Source.Unsupported_expression { context; expr }) ->
      Alcotest.(check bool)
        "reports invalidated sibling pointer alias" true
        (Stage0.Common.contains ~substring:"invalidated pointer alias" context);
      Alcotest.(check bool)
        "reports sibling pointer local" true
        (Stage0.Common.contains ~substring:"tile_ptr" expr)
  | Error error -> Alcotest.fail (Source.error_to_string error)
  | Ok _ -> Alcotest.fail "sibling pointer alias leak was accepted"

let test_one_path_pointer_alias_does_not_escape_if () : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "idx" (D_lang.Expr.IntegerLiteral 4);
            undef_decl ~ty:pointer_ty "tile_ptr";
          ];
        IfStmt
          {
            cond = block_x_eq_0 ();
            then_stmt = pointer_assign "tile_ptr" (pointer_offset "tile" "idx");
            else_stmt = D_lang.Stmt.Skip;
          };
        SExpr
          (call_expr "load_matrix_sync"
             [ frag_a; ident ~ty:pointer_ty "tile_ptr"; IntegerLiteral 16 ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "one_path_pointer_alias_if" code) ]
  with
  | Error (Source.Unsupported_expression { context; expr }) ->
      Alcotest.(check bool)
        "reports invalidated one-path pointer alias" true
        (Stage0.Common.contains ~substring:"invalidated pointer alias" context);
      Alcotest.(check bool)
        "reports one-path pointer local" true
        (Stage0.Common.contains ~substring:"tile_ptr" expr)
  | Error error -> Alcotest.fail (Source.error_to_string error)
  | Ok _ -> Alcotest.fail "one-path pointer alias escaped if join"

let test_unsupported_ordinary_memory_index_fails_explicitly () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        syncwarp_stmt;
        write_stmt
          (subscript "dst" [ call_expr "opaque_index" [ ident "threadIdx.x" ] ]);
      ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "unsupported_ordinary_index" code) ]
  with
  | Error (Source.Unsupported_expression { context; _ }) ->
      Alcotest.(check string)
        "unsupported ordinary memory context" "ordinary write access index"
        context
  | Error error -> Alcotest.fail (Source.error_to_string error)
  | Ok _ -> Alcotest.fail "unsupported ordinary memory index was accepted"

let test_thread_x_guard_records_site_control () : unit =
  let condition = thread_x_lt_32 () in
  let code =
    D_lang.Stmt.IfStmt
      {
        cond = condition;
        then_stmt = D_lang.Stmt.SExpr (call_expr "__syncwarp" []);
        else_stmt = D_lang.Stmt.Skip;
      }
  in
  match
    Source.route_program ~target_config:(subgroup_config ())
      [ D_lang.Def.Kernel (kernel "guarded_syncwarp" code) ]
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      match subgroup.site_controls with
      | [ control ] ->
          Alcotest.(check int) "guarded site id" 0 control.site_id;
          Alcotest.(check int)
            "one enclosing condition" 1
            (List.length control.conditions);
          Alcotest.(check string)
            "thread-x guard" "threadIdx.x < 32"
            (Exp.b_to_string (List.hd control.conditions))
      | controls ->
          Alcotest.fail
            (Printf.sprintf "expected one site control, got %d"
               (List.length controls)))
  | _ -> Alcotest.fail "guarded subgroup op did not route to subgroup carrier"

let test_subgroup_id_alias_records_uniform_vars () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "subgroup_id_alias_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      Alcotest.(check bool)
        "warp_id is subgroup-uniform" true
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      Alcotest.(check bool)
        "kv_block inherits subgroup-uniform init" true
        (Variable.Set.mem (var "kv_block") subgroup.uniform_vars);
      Alcotest.(check bool)
        "thread-x alias itself stays non-uniform" false
        (Variable.Set.mem (var "tid") subgroup.uniform_vars);
      Alcotest.(check bool)
        "lane alias stays non-uniform" false
        (Variable.Set.mem (var "lane_id") subgroup.uniform_vars);
      let artifact =
        Source.subgroup_kernel_artifact_summary subgroup |> String.concat "\n"
      in
      Alcotest.(check bool)
        "artifact records source-uniform vars" true
        (Stage0.Common.contains ~substring:"uniform_vars:" artifact
        && Stage0.Common.contains ~substring:"warp_id" artifact);
      Alcotest.(check bool)
        "artifact records subgroup site controls" true
        (Stage0.Common.contains ~substring:"site_controls:" artifact
        && Stage0.Common.contains ~substring:"kv_block < 2" artifact)
  | _ ->
      Alcotest.fail "subgroup-id alias kernel did not route to subgroup carrier"

let test_subgroup_id_alias_reassignment_invalidates_uniform_vars () : unit =
  let code =
    D_lang.Stmt.from_list
      [
        DeclStmt
          [
            decl "tid" (member_expr "threadIdx" "x");
            decl "warp_id" (bin (ident "tid") "/" (ident "WARP_SIZE"));
            decl "lane_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
          ];
        assign "warp_id" (bin (ident "tid") "%" (ident "WARP_SIZE"));
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "stale_subgroup_alias_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      Alcotest.(check bool)
        "warp_id reassignment clears source-uniform fact" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      Alcotest.(check bool)
        "lane_id stays non-uniform" false
        (Variable.Set.mem (var "lane_id") subgroup.uniform_vars);
      let artifact =
        Source.subgroup_kernel_artifact_summary subgroup |> String.concat "\n"
      in
      Alcotest.(check bool)
        "artifact keeps reassigned control visible" true
        (Stage0.Common.contains ~substring:"warp_id < 2" artifact)
  | _ ->
      Alcotest.fail
        "reassigned subgroup alias did not route to subgroup carrier"

let test_non_uniform_control_assignment_invalidates_uniform_vars () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "divergent_assignment_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      Alcotest.(check bool)
        "threadIdx.x-guarded assignment clears source-uniform fact" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      let artifact =
        Source.subgroup_kernel_artifact_summary subgroup |> String.concat "\n"
      in
      Alcotest.(check bool)
        "artifact keeps later subgroup control visible" true
        (Stage0.Common.contains ~substring:"warp_id < 2" artifact)
  | _ ->
      Alcotest.fail
        "divergent assignment kernel did not route to subgroup carrier"

let test_mixed_branch_subgroup_id_alias_invalidates_uniform_vars () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "mixed_branch_subgroup_alias_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      Alcotest.(check bool)
        "mixed branch clears source-uniform fact" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      let artifact =
        Source.subgroup_kernel_artifact_summary subgroup |> String.concat "\n"
      in
      Alcotest.(check bool)
        "artifact keeps mixed-branch control visible" true
        (Stage0.Common.contains ~substring:"warp_id < 2" artifact)
  | _ ->
      Alcotest.fail
        "mixed-branch subgroup alias did not route to subgroup carrier"

let test_one_path_branch_introduced_alias_does_not_escape () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel
        (kernel "one_path_branch_introduced_alias_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      Alcotest.(check bool)
        "one-path branch fact does not escape globally" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      match subgroup.site_controls with
      | [ control ] ->
          Alcotest.(check bool)
            "later site snapshot does not inherit one-path fact" false
            (site_control_has_uniform_var "warp_id" control)
      | controls ->
          Alcotest.fail
            (Printf.sprintf "expected one later site control, got %d"
               (List.length controls)))
  | _ -> Alcotest.fail "one-path branch alias did not route to subgroup carrier"

let test_else_branch_site_snapshot_does_not_inherit_then_alias () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "else_branch_site_snapshot_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      Alcotest.(check bool)
        "then-only branch fact does not escape globally" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      match subgroup.site_controls with
      | [ control ] ->
          Alcotest.(check bool)
            "else site snapshot does not inherit then-only fact" false
            (site_control_has_uniform_var "warp_id" control);
          Alcotest.(check bool)
            "else site keeps lane-varying control visible" true
            (Stage0.Common.contains ~substring:"warp_id < 2"
               (site_control_text control))
      | controls ->
          Alcotest.fail
            (Printf.sprintf "expected one else-branch site control, got %d"
               (List.length controls)))
  | _ ->
      Alcotest.fail
        "else-branch site snapshot test did not route to subgroup carrier"

let test_loop_body_invalidation_beats_uniform_increment () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "loop_readded_subgroup_alias_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] ->
      Alcotest.(check bool)
        "loop body invalidation clears source-uniform fact" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars)
  | _ ->
      Alcotest.fail
        "loop-readded subgroup alias did not route to subgroup carrier"

let test_one_path_loop_introduced_alias_does_not_escape () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "one_path_loop_introduced_alias_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      Alcotest.(check bool)
        "one-path loop fact does not escape globally" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      match subgroup.site_controls with
      | [ control ] ->
          Alcotest.(check bool)
            "post-loop site snapshot does not inherit one-path fact" false
            (site_control_has_uniform_var "warp_id" control)
      | controls ->
          Alcotest.fail
            (Printf.sprintf "expected one later site control, got %d"
               (List.length controls)))
  | _ -> Alcotest.fail "one-path loop alias did not route to subgroup carrier"

let test_one_path_loop_increment_alias_does_not_escape () : unit =
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
  let program =
    [
      D_lang.Def.Declaration (decl "WARP_SIZE" (D_lang.Expr.IntegerLiteral 32));
      D_lang.Def.Kernel (kernel "one_path_loop_increment_alias_syncwarp" code);
    ]
  in
  match
    Source.route_program ~target_config:(subgroup_config ()) program
    |> expect_route_ok
  with
  | [ Source.Subgroup_matrix subgroup ] -> (
      Alcotest.(check bool)
        "one-path loop increment fact does not escape globally" false
        (Variable.Set.mem (var "warp_id") subgroup.uniform_vars);
      match subgroup.site_controls with
      | [ control ] ->
          Alcotest.(check bool)
            "post-loop site snapshot does not inherit increment fact" false
            (site_control_has_uniform_var "warp_id" control)
      | controls ->
          Alcotest.fail
            (Printf.sprintf "expected one later site control, got %d"
               (List.length controls)))
  | _ ->
      Alcotest.fail
        "one-path loop increment alias did not route to subgroup carrier"

let test_loop_and_switch_controls_record_site_control () : unit =
  let for_control =
    expect_single_site_control "for_guarded_syncwarp"
      (D_lang.Stmt.ForStmt
         {
           init = None;
           cond = Some (thread_x_lt_32 ());
           inc = D_lang.Stmt.Skip;
           body = syncwarp_stmt;
         })
  in
  Alcotest.(check bool)
    "for guard records threadIdx.x control" true
    (Stage0.Common.contains ~substring:"threadIdx.x"
       (site_control_text for_control));
  let while_control =
    expect_single_site_control "while_guarded_syncwarp"
      (D_lang.Stmt.WhileStmt { cond = thread_x_lt_32 (); body = syncwarp_stmt })
  in
  Alcotest.(check bool)
    "while guard records threadIdx.x control" true
    (Stage0.Common.contains ~substring:"threadIdx.x"
       (site_control_text while_control));
  let do_control =
    expect_single_site_control "do_guarded_syncwarp"
      (D_lang.Stmt.DoStmt { cond = thread_x_lt_32 (); body = syncwarp_stmt })
  in
  Alcotest.(check bool)
    "do-while guard records threadIdx.x control" true
    (Stage0.Common.contains ~substring:"threadIdx.x"
       (site_control_text do_control));
  let switch_control =
    expect_single_site_control "switch_guarded_syncwarp"
      (D_lang.Stmt.SwitchStmt
         {
           cond = thread_x_mod_32 ();
           body = D_lang.Stmt.DefaultStmt syncwarp_stmt;
         })
  in
  Alcotest.(check bool)
    "switch default records lane control" true
    (Stage0.Common.contains ~substring:"threadIdx.x"
       (site_control_text switch_control)
    && Stage0.Common.contains ~substring:"% 32"
         (site_control_text switch_control));
  let case_control =
    expect_single_site_control "case_guarded_syncwarp"
      (D_lang.Stmt.CaseStmt { case = thread_x_mod_32 (); body = syncwarp_stmt })
  in
  Alcotest.(check bool)
    "case guard records lane control" true
    (Stage0.Common.contains ~substring:"threadIdx.x"
       (site_control_text case_control)
    && Stage0.Common.contains ~substring:"% 32" (site_control_text case_control)
    )

let test_malformed_fill_fragment_and_mma_sync_fail () : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let frag_c = ident ~ty:accumulator_fragment_type "frag_c" in
  expect_unsupported_matrix_call ~op:"fill_fragment"
    ~reason:"unexpected WMMA argument shape"
    (D_lang.Stmt.SExpr (call_expr "fill_fragment" [ frag_c ]));
  expect_unsupported_matrix_call ~op:"fill_fragment"
    ~reason:"unexpected WMMA argument shape"
    (D_lang.Stmt.SExpr
       (call_expr "fill_fragment"
          [ frag_c; D_lang.Expr.IntegerLiteral 0; D_lang.Expr.IntegerLiteral 1 ]));
  expect_unsupported_matrix_call ~op:"mma_sync"
    ~reason:"unexpected WMMA argument shape"
    (D_lang.Stmt.SExpr (call_expr "mma_sync" [ frag_c ]));
  expect_unsupported_matrix_call ~op:"mma_sync"
    ~reason:"expected an nvcuda::wmma::fragment argument"
    (D_lang.Stmt.SExpr
       (call_expr "mma_sync" [ frag_c; frag_a; ident "not_fragment"; frag_c ]))

let test_extra_load_store_matrix_arguments_fail () : unit =
  let frag_a = ident ~ty:matrix_a_fragment_type "frag_a" in
  let frag_c = ident ~ty:accumulator_fragment_type "frag_c" in
  expect_unsupported_matrix_call ~op:"load_matrix_sync"
    ~reason:"unexpected WMMA argument shape"
    (D_lang.Stmt.SExpr
       (call_expr "load_matrix_sync"
          [
            frag_a;
            pointer_offset "tile" "d";
            D_lang.Expr.IntegerLiteral 16;
            ident "nvcuda::wmma::mem_row_major";
          ]));
  expect_unsupported_matrix_call ~op:"store_matrix_sync"
    ~reason:"unexpected WMMA argument shape"
    (D_lang.Stmt.SExpr
       (call_expr "store_matrix_sync"
          [
            ident ~ty:half_pointer_ty "dst";
            frag_c;
            D_lang.Expr.IntegerLiteral 8;
            ident "nvcuda::wmma::mem_row_major";
            D_lang.Expr.IntegerLiteral 0;
          ]))

let tests : unit Alcotest.test_case list =
  [
    ( "non-WMMA routes to ordinary Imp without subgroup config",
      `Quick,
      test_non_wmma_routes_to_ordinary_imp_without_subgroup_config );
    ( "missing config fails only on subgroup path",
      `Quick,
      test_missing_config_fails_only_on_subgroup_path );
    ( "WMMA source generates site summary",
      `Quick,
      test_wmma_source_generates_site_summary );
    ( "WMMA artifact summary preserves matrix footprints",
      `Quick,
      test_wmma_artifact_summary_preserves_matrix_footprints );
    ( "subgroup path records ordinary memory effects",
      `Quick,
      test_subgroup_path_records_ordinary_memory_effects );
    ( "source order tracks interleaved events",
      `Quick,
      test_source_order_tracks_interleaved_subgroup_and_ordinary_events );
    ( "ordinary memory effects preserve control and phases",
      `Quick,
      test_ordinary_memory_effects_preserve_control_and_phases );
    ( "warp helpers advance ordinary memory subgroup phase",
      `Quick,
      test_warp_helpers_advance_ordinary_memory_subgroup_phase );
    ( "ordinary memory effects preserve loop ownership facts",
      `Quick,
      test_ordinary_memory_effects_preserve_loop_ownership_facts );
    ( "ordinary memory effects preserve varying local aliases",
      `Quick,
      test_ordinary_memory_effects_preserve_varying_local_aliases );
    ( "ordinary memory drops aliases depending on reassigned scalar",
      `Quick,
      test_ordinary_memory_drops_aliases_depending_on_reassigned_scalar );
    ( "ordinary memory branch-only alias does not escape",
      `Quick,
      test_ordinary_memory_branch_only_alias_does_not_escape );
    ( "memory globals track task-invariant dst arithmetic",
      `Quick,
      test_memory_globals_track_task_invariant_dst_arithmetic );
    ( "memory globals join drops thread-dependent branch assignment",
      `Quick,
      test_memory_globals_join_drops_thread_dependent_branch_assignment );
    ( "matrix memory conditions drop aliases depending on reassigned scalar",
      `Quick,
      test_matrix_memory_conditions_drop_aliases_depending_on_reassigned_scalar
    );
    ( "matrix pointer alias depending on reassigned scalar fails",
      `Quick,
      test_matrix_pointer_alias_depending_on_reassigned_scalar_fails );
    ( "sibling branch pointer alias does not leak",
      `Quick,
      test_sibling_branch_pointer_alias_does_not_leak );
    ( "one-path pointer alias does not escape if",
      `Quick,
      test_one_path_pointer_alias_does_not_escape_if );
    ( "unsupported ordinary memory index fails explicitly",
      `Quick,
      test_unsupported_ordinary_memory_index_fails_explicitly );
    ( "threadIdx.x guard records site control",
      `Quick,
      test_thread_x_guard_records_site_control );
    ( "subgroup-id alias records uniform vars",
      `Quick,
      test_subgroup_id_alias_records_uniform_vars );
    ( "subgroup-id reassignment invalidates uniform vars",
      `Quick,
      test_subgroup_id_alias_reassignment_invalidates_uniform_vars );
    ( "non-uniform control assignment invalidates uniform vars",
      `Quick,
      test_non_uniform_control_assignment_invalidates_uniform_vars );
    ( "mixed-branch subgroup-id alias invalidates uniform vars",
      `Quick,
      test_mixed_branch_subgroup_id_alias_invalidates_uniform_vars );
    ( "one-path branch introduced alias does not escape",
      `Quick,
      test_one_path_branch_introduced_alias_does_not_escape );
    ( "else branch site snapshot does not inherit sibling alias",
      `Quick,
      test_else_branch_site_snapshot_does_not_inherit_then_alias );
    ( "loop body invalidation beats uniform increment",
      `Quick,
      test_loop_body_invalidation_beats_uniform_increment );
    ( "one-path loop introduced alias does not escape",
      `Quick,
      test_one_path_loop_introduced_alias_does_not_escape );
    ( "one-path loop increment alias does not escape",
      `Quick,
      test_one_path_loop_increment_alias_does_not_escape );
    ( "loop and switch controls record site control",
      `Quick,
      test_loop_and_switch_controls_record_site_control );
    ( "malformed fill_fragment and mma_sync fail",
      `Quick,
      test_malformed_fill_fragment_and_mma_sync_fail );
    ( "extra load/store matrix arguments fail",
      `Quick,
      test_extra_load_store_matrix_arguments_fail );
  ]

let () = Alcotest.run "Subgroup_source" [ ("subgroup_source", tests) ]
