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
         decl_set z_var (Binary (Plus Signedness.Signed, Var x_param, Var y_param)) Skip
       in
       let func_kernel =
         kernel ~return:(Some (Var z_var)) "f"
           [
             Parameter.scalar x_param Ty.int;
             Parameter.scalar y_param Ty.int;
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
           (Access.write a_array
              [ Binary (Plus Signedness.Signed, Var x_var, Var y_var) ] None)
       in
       let call_stmt =
         Call
           ( {
               result = Some (g_var, Ty.int);
               kernel = "f";
               ty = "";
               args = [ Num 1; Num 2 ];
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
                       (Binary (Plus Signedness.Signed, Var x1_var, Var y1_var))
                       (decl_set g_var (Var z_var)
                          (Access
                             (Access.write a_array
                                [
                                  Binary
                                    (Plus Signedness.Signed, Var x_var, Var y_var);
                                ]
                                None)))))))
       in
       kernel "main" [] expected_code);
  ]

let calling (name : string) (callees : string list) : Scoped.Kernel.t =
  let call_to (callee : string) (body : Scoped.Code.t) : Scoped.Code.t =
    Scoped.Code.Call
      ({ result = None; kernel = callee; ty = ""; args = [] }, body)
  in
  kernel name [] (List.fold_right call_to callees Scoped.Code.Skip)

let survivors (ks : Scoped.Kernel.t list) : string list =
  Inline_calls.inline_calls ks
  |> fst
  |> List.map (fun (k : Scoped.Kernel.t) -> k.Scoped.Kernel.name)

let rejections (ks : Scoped.Kernel.t list) : (string * string list) list =
  Inline_calls.inline_calls ks
  |> snd
  |> List.map (fun (r : Rejected_kernel.t) ->
      match r.reason with
      | Rejected_kernel.Reason.RecursiveCall { path } -> (r.kernel, path))

let test_fixpoint (name : string) (ks : Scoped.Kernel.t list)
    (expected_survivors : string list)
    (expected_rejections : (string * string list) list) =
  ( name,
    `Quick,
    fun () ->
      Alcotest.(check (list string)) (name ^ ": survivors")
        expected_survivors (survivors ks);
      Alcotest.(check (list (pair string (list string))))
        (name ^ ": rejections") expected_rejections (rejections ks) )

let fixpoint_tests =
  [
    test_fixpoint "an acyclic call graph resolves and rejects nothing"
      [ calling "k" [ "f" ]; calling "f" [] ]
      [ "f"; "k" ] [];
    test_fixpoint "a self-call rejects the callee and its caller"
      [ calling "k" [ "r" ]; calling "r" [ "r" ] ]
      []
      [ ("k", [ "k"; "r"; "r" ]); ("r", [ "r"; "r" ]) ];
    test_fixpoint "mutual recursion is the same condition on the graph"
      [ calling "k" [ "even" ]; calling "even" [ "odd" ];
        calling "odd" [ "even" ] ]
      []
      [
        ("even", [ "even"; "odd"; "even" ]);
        ("k", [ "k"; "even"; "odd"; "even" ]);
        ("odd", [ "odd"; "even"; "odd" ]);
      ];
    test_fixpoint "rejection follows reachability through a non-recursive callee"
      [ calling "k" [ "helper" ]; calling "helper" [ "r" ];
        calling "r" [ "r" ] ]
      []
      [
        ("helper", [ "helper"; "r"; "r" ]);
        ("k", [ "k"; "helper"; "r"; "r" ]);
        ("r", [ "r"; "r" ]);
      ];
    test_fixpoint "a kernel that shares no callee with a cycle is unaffected"
      [ calling "k1" [ "r" ]; calling "r" [ "r" ]; calling "k2" [ "f" ];
        calling "f" [] ]
      [ "f"; "k2" ]
      [ ("k1", [ "k1"; "r"; "r" ]); ("r", [ "r"; "r" ]) ];
  ]

let all_tests =
  [
    ("inline expansions", inline_expansion_tests);
    ("call-graph fixpoint", fixpoint_tests);
  ]

let () = Alcotest.run "Kernel Inline" all_tests
