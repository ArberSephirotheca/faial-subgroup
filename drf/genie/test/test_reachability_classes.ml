open Protocols
open Drf_genie

(* Phase 2 pins: [Reachability.check_kernel]'s per-class query
   loop issues one Z3 call per syntactic path-condition equivalence
   class among the parameter-touching accesses, and zero Z3 calls
   for parameter-free accesses (whose reachability is independent
   of any [--assume]). The counter wrapper in [Z3_call_counter]
   bumps once per call; tests assert exact counts. *)

let mk_kernel ?(name = "k_test") ?(globals = []) ?(locals = [])
    (code : Code.t) : Kernel.t =
  let to_params kvs =
    List.fold_left
      (fun p (n, ty) -> Params.add (Variable.from_name n) ty p)
      Params.empty kvs
  in
  {
    name;
    global_variables = to_params globals;
    local_variables = to_params locals;
    arrays = Variable.Map.empty;
    pre = Exp.Bool true;
    code;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

let array_a : Variable.t = Variable.from_name "A"

let access (idx : Exp.nexp) : Code.t =
  Code.Access (Access.read array_a [ idx ])

let var (name : string) : Exp.nexp = Exp.Var (Variable.from_name name)

let lt (a : Exp.nexp) (b : Exp.nexp) : Exp.bexp =
  Exp.NRel (Lt Signedness.Signed, a, b)

let gt (a : Exp.nexp) (b : Exp.nexp) : Exp.bexp =
  Exp.NRel (Gt Signedness.Signed, a, b)

let seq_of (items : Code.t list) : Code.t =
  match items with
  | [] -> Code.Skip
  | [ x ] -> x
  | x :: rest -> List.fold_left Code.seq x rest

(* Test A. Five accesses sharing one path condition [N > 0] (a
   parameter-touching guard). One equivalence class → exactly one
   Z3 call regardless of access count. *)
let test_single_class_multiple_accesses () =
  let guard = gt (var "N") (Exp.Num 0) in
  let body =
    Code.if_ guard
      (seq_of
         [ access (Exp.Num 0); access (Exp.Num 1); access (Exp.Num 2);
           access (Exp.Num 3); access (Exp.Num 4) ])
      Code.Skip
  in
  let k = mk_kernel ~globals:[ ("N", Ty.int) ] body in
  let count, entries =
    Z3_call_counter.with_counter (fun () -> Reachability.check_kernel k)
  in
  Alcotest.(check int) "five entries returned" 5 (List.length entries);
  Alcotest.(check int) "exactly one Z3 call" 1 count

(* Test B. Three accesses, three distinct parameter-touching path
   conditions. Three classes → three Z3 calls. *)
let test_distinct_classes_count () =
  let acc1 =
    Code.if_ (gt (var "N") (Exp.Num 0)) (access (Exp.Num 0)) Code.Skip
  in
  let acc2 =
    Code.if_ (gt (var "N") (Exp.Num 1)) (access (Exp.Num 1)) Code.Skip
  in
  let acc3 = access (Exp.Num 2) in
  (* [acc3] sits under [Bool true] which mentions no params and so
     classifies as parameter-free. To make sure all three classes
     reach the Z3 path, give the third access a distinct parameter-
     touching guard. *)
  let acc3 =
    Code.if_ (gt (var "M") (Exp.Num 0)) acc3 Code.Skip
  in
  let body = seq_of [ acc1; acc2; acc3 ] in
  let k =
    mk_kernel
      ~globals:[ ("N", Ty.int); ("M", Ty.int) ]
      body
  in
  let count, entries =
    Z3_call_counter.with_counter (fun () -> Reachability.check_kernel k)
  in
  Alcotest.(check int) "three entries returned" 3 (List.length entries);
  Alcotest.(check int) "exactly three Z3 calls" 3 count

(* Test C. Three accesses, all guarded by [threadIdx.x < blockDim.x]
   — built-ins only, no kernel parameter. All entries classify as
   parameter-free and bypass Z3 entirely. *)
let test_parameter_free_skips_z3 () =
  let guard = lt (var "threadIdx.x") (var "blockDim.x") in
  let body =
    Code.if_ guard
      (seq_of [ access (Exp.Num 0); access (Exp.Num 1); access (Exp.Num 2) ])
      Code.Skip
  in
  let k = mk_kernel body in
  let count, entries =
    Z3_call_counter.with_counter (fun () -> Reachability.check_kernel k)
  in
  Alcotest.(check int) "three entries returned" 3 (List.length entries);
  Alcotest.(check int) "zero Z3 calls" 0 count;
  let reachable = Reachability.reachable_set entries in
  Alcotest.(check int) "all three reachable"
    3 (Reachability.AccessSet.cardinal reachable)

(* Test D. Mixed: two parameter-free accesses plus four parameter-
   touching accesses split across two classes. Z3 call count must
   equal the number of distinct parameter-touching classes (= 2),
   not the parameter-touching access count (= 4). *)
let test_mixed_count_equals_pt_classes () =
  let pf1 =
    Code.if_ (lt (var "threadIdx.x") (var "blockDim.x"))
      (access (Exp.Num 0)) Code.Skip
  in
  let pf2 = pf1 in
  let pt_class_1 =
    Code.if_ (gt (var "N") (Exp.Num 0))
      (seq_of [ access (Exp.Num 1); access (Exp.Num 2) ])
      Code.Skip
  in
  let pt_class_2 =
    Code.if_ (gt (var "N") (Exp.Num 5))
      (seq_of [ access (Exp.Num 3); access (Exp.Num 4) ])
      Code.Skip
  in
  let body = seq_of [ pf1; pf2; pt_class_1; pt_class_2 ] in
  let k = mk_kernel ~globals:[ ("N", Ty.int) ] body in
  let count, entries =
    Z3_call_counter.with_counter (fun () -> Reachability.check_kernel k)
  in
  Alcotest.(check int) "six entries returned" 6 (List.length entries);
  Alcotest.(check int) "two Z3 calls (one per parameter-touching class)"
    2 count

(* Test E. Semantic equivalence — the [AccessSet] returned by
   [check_kernel] is identical to a naive per-access evaluation of
   the same kernel. The naive variant builds one goal per access and
   solves independently; the new path groups, deduplicates, and
   skips parameter-free entries. Their reachable-set outputs must
   coincide.

   The kernel here mixes parameter-free and parameter-touching
   accesses, plus a known-unsatisfiable parameter-touching class
   ([N < 0 ∧ N > 100]) to exercise the [Unreachable] branch. *)
let naive_check_kernel (k : Kernel.t) : Reachability.AccessSet.t =
  let runtime =
    Params.to_bexp (Params.union_left k.global_variables k.local_variables)
  in
  Reachability.walk k.code
  |> List.mapi (fun i (access, path_cond) ->
    let goal = Exp.b_and_ex [ k.pre; runtime; path_cond ] in
    let goal = Predicates.b_inline goal in
    let reachable =
      match Gen_z3.Bv64Gen.solve (Formula.make goal) with
      | Ok (Gen_z3.Solver.Sat _) -> true
      | Ok Gen_z3.Solver.Unsat -> false
      | Error _ -> true (* accept-on-Unknown, matching the gate's stance *)
    in
    let id : Reachability.AccessId.t =
      {
        kernel_name = k.name;
        array_name = Variable.name (Access.array access);
        access_index = i;
        location = Access.location access;
      }
    in
    (id, reachable))
  |> List.filter_map (fun (id, r) -> if r then Some id else None)
  |> Reachability.AccessSet.of_list

let test_semantic_equivalence () =
  let pf =
    Code.if_ (lt (var "threadIdx.x") (var "blockDim.x"))
      (access (Exp.Num 0)) Code.Skip
  in
  let pt_sat =
    Code.if_ (gt (var "N") (Exp.Num 0))
      (seq_of [ access (Exp.Num 1); access (Exp.Num 2) ])
      Code.Skip
  in
  let pt_unsat =
    Code.if_
      (Exp.b_and
         (lt (var "N") (Exp.Num 0))
         (gt (var "N") (Exp.Num 100)))
      (access (Exp.Num 3)) Code.Skip
  in
  let body = seq_of [ pf; pt_sat; pt_unsat ] in
  let k = mk_kernel ~globals:[ ("N", Ty.int) ] body in
  let new_set = Reachability.reachable_set (Reachability.check_kernel k) in
  let naive_set = naive_check_kernel k in
  let to_sorted_list s =
    Reachability.AccessSet.elements s
    |> List.map Reachability.AccessId.to_string
    |> List.sort String.compare
  in
  Alcotest.(check (list string)) "reachable sets coincide"
    (to_sorted_list naive_set) (to_sorted_list new_set)

(* Second equivalence kernel: shared-memory style with several
   loop-and-guard layers. Exercises [walk]'s path-condition
   accumulation across nested constructs alongside the class
   grouping. *)
let test_semantic_equivalence_nested () =
  let i = Variable.from_name "i" in
  let range = Range.make i (var "N") in
  let loop =
    Code.loop range
      (Code.if_ (gt (var "M") (Exp.Num 0))
         (access (Exp.Var i))
         (access (Exp.n_plus (Exp.Var i) (Exp.Num 1))))
  in
  let extra =
    Code.if_ (lt (var "threadIdx.x") (var "blockDim.x"))
      (access (Exp.Num 99)) Code.Skip
  in
  let body = seq_of [ loop; extra ] in
  let k =
    mk_kernel
      ~globals:[ ("N", Ty.int); ("M", Ty.int) ]
      body
  in
  let new_set = Reachability.reachable_set (Reachability.check_kernel k) in
  let naive_set = naive_check_kernel k in
  let to_sorted_list s =
    Reachability.AccessSet.elements s
    |> List.map Reachability.AccessId.to_string
    |> List.sort String.compare
  in
  Alcotest.(check (list string)) "reachable sets coincide on nested kernel"
    (to_sorted_list naive_set) (to_sorted_list new_set)

let tests = [
  ("A. single class, multiple accesses -> 1 Z3 call",
    `Quick, test_single_class_multiple_accesses);
  ("B. three distinct classes -> 3 Z3 calls",
    `Quick, test_distinct_classes_count);
  ("C. all parameter-free -> 0 Z3 calls",
    `Quick, test_parameter_free_skips_z3);
  ("D. mixed -> count = parameter-touching class count",
    `Quick, test_mixed_count_equals_pt_classes);
  ("E. AccessSet matches naive per-access evaluation",
    `Quick, test_semantic_equivalence);
  ("E.nested AccessSet matches naive on nested kernel",
    `Quick, test_semantic_equivalence_nested);
]

let () =
  Alcotest.run "drf_genie/reachability_classes" [
    ("Reachability.check_kernel", tests);
  ]
