open OUnit2
open Protocols
open Exp
open Rel_cost
open Rel_cost.Symbolic_metric_analysis

(* Factory function for Config objects *)
let make_config (threads_per_warp : int) : Config.t =
  Config.make ~bank_count:1 (* to simplify let's assume that there's 1 bank *)
    ~threads_per_warp
    ~block_dim:(Dim3.make ~x:threads_per_warp ())
      (* Allow enough space for all threads *)
    ~grid_dim:(Dim3.make ~x:1 ()) ()

(* Utility function to create variables *)
let var_ (name : string) : nexp = Var (Variable.from_name name)

(* Utility function to check if an expression is even *)
let is_even (e : nexp) : bexp = n_eq (Binary (Mod, e, Num 2)) (Num 0)

(* Pretty printer for (bexp * nexp) list *)
let pp_replicate_result (result : (bexp * nexp) list) : string =
  let pp_pair (b, n) =
    Printf.sprintf "(%s, %s)" (b_to_string b) (n_to_string n)
  in
  "[" ^ String.concat "; " (List.map pp_pair result) ^ "]"

(* Utility function that wraps replicate and provides better error messages *)
let assert_replicate ~(expected : (bexp * nexp) list) ~(threads_per_warp : int)
    ~(locals : Variable.Set.t) ~(cond : bexp) ~(index : nexp) : unit =
  let cfg = make_config threads_per_warp in
  let result = Proj.run_pair cfg locals cond index in
  assert_equal ~printer:pp_replicate_result expected result

(* Utility function that wraps encode_ua and provides better error messages *)
let assert_encode_ua ~(expected : nexp) ~(threads_per_warp : int)
    ~(locals : Variable.Set.t) ~(cond : bexp) ~(index : nexp) : unit =
  let cfg = make_config threads_per_warp in
  let result = encode_ua cfg locals cond index in
  assert_equal ~printer:n_to_string expected result

(* Utility function that wraps ua and provides better error messages *)
let assert_ua ?(strategy = Gen_z3.Optimizer.Strategy.Maximize)
    ~(expected : int option) ~(threads_per_warp : int)
    ~(locals : Variable.Set.t) ~(cond : bexp) ~(index : nexp) () : unit =
  let cfg = make_config threads_per_warp in
  let index_with_segments =
    n_mult index (Num (Config.memory_segments_bits cfg))
  in
  let result = ua ~strategy cfg locals cond index_with_segments in
  let formula = encode_ua cfg locals cond index_with_segments in
  let printer = function Some x -> string_of_int x | None -> "none" in
  let debug_msg =
    let warp_constraints_pretty =
      warp_constraints cfg |> b_and_split
      |> List.map (fun s -> "&& " ^ b_to_string s)
      |> String.concat "\n"
    in
    Printf.sprintf "Warp constraints:\n%s\nFormula: %s" warp_constraints_pretty
      (n_to_string formula)
  in
  assert_equal ~printer ~msg:debug_msg expected result

let tests =
  "test_symbolic_metric_analysis"
  >::: [
         ( "replicate_empty_locals" >:: fun _ ->
           (* Test with empty locals, cfg with 2 threads_per_warp *)
           assert_replicate
             ~expected:[ (b_true, Num 0); (b_true, Num 0) ]
             ~threads_per_warp:2 ~locals:Variable.Set.empty ~cond:b_true
             ~index:(Num 0) );
         ( "replicate_with_local_variable" >:: fun _ ->
           (* Test with local variable x *)
           let x = Variable.from_name "x" in
           let locals = Variable.Set.singleton x in
           assert_replicate
             ~expected:[ (b_true, var_ "x$0"); (b_true, var_ "x$1") ]
             ~threads_per_warp:2 ~locals ~cond:b_true ~index:(var_ "x") );
         ( "encode_ua_empty_locals" >:: fun _ ->
           (* Test encode_ua with empty locals, 2 threads accessing same index *)
           assert_encode_ua ~expected:(Num 1) ~threads_per_warp:2
             ~locals:Variable.Set.empty ~cond:b_true ~index:(Num 0) );
         ( "encode_ua_with_local_variable" >:: fun _ ->
           (* Test encode_ua with local variable x *)
           let x = Variable.from_name "x" in
           let locals = Variable.Set.singleton x in
           let expected =
             n_plus (Num 1)
               (NIf (n_neq (var_ "x$1") (var_ "x$0"), Num 1, Num 0))
           in
           assert_encode_ua ~expected ~threads_per_warp:2 ~locals ~cond:b_true
             ~index:(var_ "x") );
         ( "ua_empty_locals" >:: fun _ ->
           (* Test ua with empty locals, 2 threads accessing same index *)
           assert_ua ~expected:(Some 1) ~threads_per_warp:2
             ~locals:Variable.Set.empty ~cond:b_true ~index:(Num 0) () );
         ( "ua_with_local_variable" >:: fun _ ->
           (* Test ua with local variable x *)
           let x = Variable.from_name "x" in
           let locals = Variable.Set.singleton x in
           assert_ua ~expected:(Some 2) ~threads_per_warp:2 ~locals ~cond:b_true
             ~index:(var_ "x") () );
         ( "ua_threadIdx.x" >:: fun _ ->
           (* Test ua with both minimize and maximize strategies on threadIdx.x *)
           (* assert_ua automatically multiplies by memory segment size *)
           (* Expect 2 uncoalesced accesses since threads hit different memory segments *)

           (* Test minimize strategy *)
           assert_ua ~strategy:Gen_z3.Optimizer.Strategy.Minimize
             ~expected:(Some 2) ~threads_per_warp:2 ~locals:Variable.Set.empty
             ~cond:b_true ~index:(Var Variable.tid_x) ();

           (* Test maximize strategy *)
           assert_ua ~strategy:Gen_z3.Optimizer.Strategy.Maximize
             ~expected:(Some 2) ~threads_per_warp:2 ~locals:Variable.Set.empty
             ~cond:b_true ~index:(Var Variable.tid_x) ();

           (* Test with condition tidx % 2 == 0 (only even thread IDs) *)
           assert_ua ~strategy:Gen_z3.Optimizer.Strategy.Maximize
             ~expected:(Some 2)
             ~threads_per_warp:4
               (* Only threadIdx.x is a thread-local variable *)
             ~locals:Variable.Set.empty
             ~cond:(is_even (Var Variable.tid_x))
             ~index:(Var Variable.tid_x) () );
         ( "warp_constraints_enforces_bounds_and_uniqueness" >:: fun _ ->
           (* Test that warp_constraints prevents threads from having same coordinates within bounds *)
           let cfg = make_config 2 in
           let c = warp_constraints cfg in
           (* Is it possible for 2 tids to be equal? *)
           let contradiction =
             b_and c (n_eq (var_ "threadIdx.x$0") (var_ "threadIdx.x$1"))
           in
           let open Gen_z3.IntGen in
           let result = solve contradiction in
           assert_equal
             ~msg:
               "warp_constraints should make threadIdx.x$0 = threadIdx.x$1 \
                unsatisfiable when considering block bounds"
             Gen_z3.Solver.Unsat result );
         ( "cross_warp_unsoundness_test" >:: fun _ ->
           (* Test to expose unsoundness: threads from different warps should not be allowed *)
           let cfg =
             Config.make ~threads_per_warp:32
               ~block_dim:(Dim3.make ~x:64 ()) (* 2 warps: 0-31 and 32-63 *)
               ~grid_dim:(Dim3.make ~x:1 ()) ()
           in
           let c = warp_constraints cfg in
           (* Try to assign threads from different warps *)
           let cross_warp =
             b_and_ex
               [
                 c;
                 n_eq (var_ "threadIdx.x$0") (Num 0);
                 (* thread 0 = warp 0 *)
                 n_eq (var_ "threadIdx.x$1") (Num 32);
                 (* thread 32 = warp 1 *)
                 n_eq (var_ "threadIdx.y$0") (Num 0);
                 n_eq (var_ "threadIdx.y$1") (Num 0);
                 n_eq (var_ "threadIdx.z$0") (Num 0);
                 n_eq (var_ "threadIdx.z$1") (Num 0);
               ]
           in
           let open Gen_z3.IntGen in
           let result = solve cross_warp in
           (* This test SHOULD fail (return Sat) with current constraints, exposing the bug *)
           assert_equal
             ~msg:
               "EXPECTED TO FAIL: Current constraints allow threads from \
                different warps (this exposes unsoundness)"
             Gen_z3.Solver.Unsat result );
       ]

let _ = run_test_tt_main tests
