open Protocols
open Exp
open Imp
open Kernel
module StringMap = Stage0.Common.StringMap

(* Helper functions *)
let var (name : string) : Variable.t = Variable.from_name name

let kernel ?(ty = "") ?(return = None) (name : string)
    (parameters : Kernel.ParameterList.t) (code : Scoped.Code.t) :
    Scoped.Kernel.t =
  {
    Scoped.Kernel.name;
    ty;
    parameters;
    global_arrays = Variable.Map.empty;
    global_variables = Params.empty;
    code;
    return;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

(* Alcotest testable types *)
let scoped_kernel_testable : Scoped.Kernel.t Alcotest.testable =
  let pp fmt (k : Scoped.Kernel.t) =
    Format.fprintf fmt "%s" (Scoped.Code.to_string k.Scoped.Kernel.code)
  in
  let equal (k1 : Scoped.Kernel.t) (k2 : Scoped.Kernel.t) =
    k1.name = k2.name && k1.code = k2.code
  in
  Alcotest.testable pp equal

(* Test helper function *)
let test_inline_expansion (name : string) (funcs : Scoped.Kernel.t StringMap.t)
    (input_kernel : Scoped.Kernel.t) (expected_kernel : Scoped.Kernel.t) =
  ( name,
    `Quick,
    fun () ->
      let actual = Inline_calls.inline funcs input_kernel in
      Alcotest.check scoped_kernel_testable name expected_kernel actual )

(* Test data *)
let inline_expansion_tests =
  let open Scoped.Code in
  [
    (* FAILING TEST: Shows expected behavior with parameter renaming *)
    test_inline_expansion
      "function call should rename parameters to avoid collision"
      (* funcs map: f(x,y) { decl z = x + y; return z; } *)
      (let x_param = var "x" in
       let y_param = var "y" in
       let z_var = var "z" in
       let func_body =
         decl_set z_var (Binary (Plus, Var x_param, Var y_param)) Skip
       in
       let func_kernel =
         kernel ~return:(Some (Var z_var)) "f"
           [
             Parameter.scalar x_param C_type.int;
             Parameter.scalar y_param C_type.int;
           ]
           func_body
       in
       let call_id = Call.kernel_id ~kernel:"f" ~ty:"" in
       StringMap.add call_id func_kernel StringMap.empty)
      (* input kernel: decl g = f(1, 2); A[x + y]; *)
      (let g_var = var "g" in
       let a_array = var "A" in
       let x_var = var "x" in
       let y_var = var "y" in
       let array_access =
         Access
           {
             array = a_array;
             index = [ Binary (Plus, Var x_var, Var y_var) ];
             mode = Write None;
           }
       in
       let call_stmt =
         Call
           ( {
               result = Some (g_var, C_type.int);
               kernel = "f";
               ty = "";
               args = [ Arg.Scalar (Num 1); Arg.Scalar (Num 2) ];
             },
             array_access )
       in
       kernel "main" [] (decl_unset x_var (decl_unset y_var call_stmt)))
      (* EXPECTED (but currently failing): Parameters renamed to x1, y1 *)
      (let x1_var = var "x1" in
       let y1_var = var "y1" in
       let g_var = var "g" in
       let z_var = var "z" in
       let a_array = var "A" in
       let x_var = var "x" in
       let y_var = var "y" in

       let expected_code =
         decl_unset x_var
           (decl_unset y_var
              (decl_set x1_var (Num 1)
                 (decl_set y1_var (Num 2)
                    (decl_set z_var
                       (Binary (Plus, Var x1_var, Var y1_var))
                       (decl_set g_var (Var z_var)
                          (Access
                             {
                               array = a_array;
                               index = [ Binary (Plus, Var x_var, Var y_var) ];
                               mode = Write None;
                             }))))))
       in
       kernel "main" [] expected_code);
  ]

let all_tests = [ ("inline expansions", inline_expansion_tests) ]
let () = Alcotest.run "Kernel Inline" all_tests
