open Inference
open Protocols
module Source = Subgroup_source
module SM = Subgroup_matrix
module Memory = Drf.Memory_event.Subgroup_obligation
module Solver = Drf.Subgroup_solver

let var = Variable.from_name

let call name args : D_lang.Expr.t =
  CallExpr
    {
      func = D_lang.Expr.ident ~kind:Decl_expr.Kind.Function (var name);
      args;
      ty = J_type.int;
    }

let num n = D_lang.Expr.IntegerLiteral n

let address =
  D_lang.make_subscript ~path:(Field_path.root (var "x"))
    ~index:[ num 0 ]
    ~ty:J_type.int ~location:Stage0.Location.empty ()

let write : D_lang.Stmt.t =
  IfStmt
    {
      cond =
        BinaryOperator
          {
            lhs = D_lang.Expr.ident Variable.tid_x;
            opcode = "==";
            rhs = num 0;
            ty = J_type.bool;
          };
      then_stmt =
        WriteAccessStmt
          { target = address; source = num 1; payload = None; guard = None };
      else_stmt = Skip;
    }

let read : D_lang.Stmt.t =
  ReadAccessStmt
    { target = var "tmp"; source = address; ty = Ty.int; guard = None }

let result_call name args : D_lang.Stmt.t =
  SExpr
    (BinaryOperator
       {
         lhs = D_lang.Expr.ident (var "value");
         opcode = "=";
         rhs = call name args;
         ty = J_type.int;
       })

let reduce = result_call "warp_reduce_sum" [ num 0 ]
let shuffle = result_call "__shfl_sync" [ num (-1); num 0; num 0 ]
let barrier = D_lang.Stmt.SExpr (call "__syncwarp" [])
let block_barrier = D_lang.Stmt.SExpr (call "__syncthreads" [])

let fragment =
  D_lang.Expr.ident (var "frag")
    ~ty:
      (Ty.of_c_string
         "nvcuda::wmma::fragment<nvcuda::wmma::accumulator, 16, 16, 16, float>")

let matrix =
  D_lang.Stmt.SExpr (call "mma_sync" [ fragment; fragment; fragment; fragment ])

let check_memory ?(threads = 32) expected statements () =
  let kernel : D_lang.Kernel.t =
    {
      id = Imp.Function_id.make ~name:"memory_ordering" ~ty:"void ()" ();
      decl_id = None;
      returns_location = false;
      code = D_lang.Stmt.from_list statements;
      type_params = [];
      template_args = [];
      params = [];
      attribute = D_lang.KernelAttr.Default;
    }
  in
  let target_config =
    SM.Target_config.subgroup_size_exn 32 |> SM.Target_config.cuda_x_contiguous
  in
  let block_dim = Dim3.make ~x:threads () in
  match Source.route_program ~target_config [ D_lang.Def.Kernel kernel ] with
  | Error error -> Alcotest.fail (Source.error_to_string error)
  | Ok [ Source.Subgroup_matrix subgroup ] ->
      Alcotest.(check int)
        "both memory accesses retained" 2
        (List.length subgroup.ordinary_memory_effects);
      let result =
        Memory.obligations ~block_dim ~globals:subgroup.memory_globals
          ~site_controls:subgroup.site_controls
          ~ordinary_memory_effects:subgroup.ordinary_memory_effects
          subgroup.matrix_kernel
        |> Solver.solve_obligation_result ~kernel_name:(D_lang.Kernel.name kernel)
             ~globals:subgroup.memory_globals ~block_dim
      in
      Alcotest.(check string)
        "memory verdict" expected
        (Solver.memory_verdict_to_string (Solver.memory_verdict result))
  | Ok _ -> Alcotest.fail "expected subgroup route"

let () =
  Alcotest.run "Subgroup memory ordering"
    [
      ( "source_to_solver",
        [
          Alcotest.test_case "unsynchronized accesses race" `Quick
            (check_memory "not_drf" [ reduce; write; read ]);
          Alcotest.test_case "reduction does not order memory" `Quick
            (check_memory "not_drf" [ write; reduce; read ]);
          Alcotest.test_case "shuffle does not order memory" `Quick
            (check_memory "not_drf" [ write; shuffle; read ]);
          Alcotest.test_case "warp barrier orders memory" `Quick
            (check_memory "drf" [ write; barrier; read ]);
          Alcotest.test_case "matrix collective does not order ordinary memory"
            `Quick
            (check_memory "not_drf" [ write; matrix; read ]);
          Alcotest.test_case "shuffle preserves preceding warp barrier" `Quick
            (check_memory "drf" [ write; barrier; shuffle; read ]);
          Alcotest.test_case "shuffle preserves following warp barrier" `Quick
            (check_memory "drf" [ write; shuffle; barrier; read ]);
          Alcotest.test_case "warp barrier does not order different warps"
            `Quick
            (check_memory ~threads:64 "not_drf"
               [ write; shuffle; barrier; read ]);
          Alcotest.test_case "block barrier orders different warps" `Quick
            (check_memory ~threads:64 "drf"
               [ write; shuffle; block_barrier; read ]);
        ] );
    ]
