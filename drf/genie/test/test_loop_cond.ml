open Stage0
open Protocols
open Drf
open Drf_genie

(* A bubbled loop constraint reaches the race goal: [for i { A[i] = 1 }]
   races (two threads collide at a shared iteration), but the same loop
   guarded by [i == threadIdx.x] is race-free (a collision forces the two
   threads to share a thread id, contradicting distinctness). *)

let arch : Architecture.t = Architecture.Block

let mk_kernel ?(globals = []) ?(pre = Exp.Bool true)
    (arrays : (string * Memory.t) list) (code : Code.t) : Kernel.t =
  let to_params kvs =
    List.fold_left
      (fun p (n, ty) -> Params.add (Variable.from_name n) ty p)
      Params.empty kvs
  in
  let arrays =
    List.fold_left
      (fun m (n, mem) -> Variable.Map.add (Variable.from_name n) mem m)
      Variable.Map.empty arrays
  in
  {
    name = "k_test";
    global_variables = to_params globals;
    local_variables = Params.empty;
    arrays;
    pre;
    code;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

let var (name : string) : Exp.nexp = Exp.Var (Variable.from_name name)
let eq (a : Exp.nexp) (b : Exp.nexp) : Exp.bexp = Exp.NRel (Eq, a, b)
let array_a : Variable.t = Variable.from_name "A"
let shared_int : Memory.t = Memory.from_type Mem_hierarchy.SharedMemory C_type.int
let access_w (idx : Exp.nexp) : Code.t = Code.Access (Access.write array_a [ idx ] None)
let range_i : Range.t = Range.make (Variable.from_name "i") (var "N")

(* Collapse the block to one dimension so [threadIdx.x] is the whole
   thread identity that [thread_distinct] separates. *)
let one_d : Exp.bexp =
  Exp.b_and (eq (var "threadIdx.y") (Exp.Num 0)) (eq (var "threadIdx.z") (Exp.Num 0))

let race_proofs (k : Kernel.t) : Symbexp.Proof.t list =
  k
  |> Kernel.apply_arch arch
  |> Wellformed.translate
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> Aligned.translate
  |> Phasesplit.translate
  |> Locsplit.translate
  |> Flatacc.translate arch
  |> Symbexp.translate arch
  |> Streamutil.to_list

let any_sat (proofs : Symbexp.Proof.t list) : bool =
  List.exists (fun p -> Co_reach.solve_one p = Z3.Solver.SATISFIABLE) proofs

let all_unsat (proofs : Symbexp.Proof.t list) : bool =
  List.for_all (fun p -> Co_reach.solve_one p = Z3.Solver.UNSATISFIABLE) proofs

let test_racy_without_cond () =
  let k =
    mk_kernel ~globals:[ ("N", C_type.int) ] ~pre:one_d [ ("A", shared_int) ]
      (Code.loop range_i (access_w (var "i")))
  in
  let proofs = race_proofs k in
  Alcotest.(check bool) "race proofs emitted" true (proofs <> []);
  Alcotest.(check bool) "race is satisfiable" true (any_sat proofs)

let test_safe_with_cond () =
  let cond = eq (var "i") (var "threadIdx.x") in
  let k =
    mk_kernel ~globals:[ ("N", C_type.int) ] ~pre:one_d [ ("A", shared_int) ]
      (Code.loop ~cond range_i (access_w (var "i")))
  in
  let proofs = race_proofs k in
  Alcotest.(check bool) "race proofs still emitted" true (proofs <> []);
  Alcotest.(check bool) "race discharged by the loop constraint" true
    (all_unsat proofs)

let () =
  Alcotest.run "loop_cond"
    [
      ( "assembly",
        [
          Alcotest.test_case "racy without constraint" `Quick
            test_racy_without_cond;
          Alcotest.test_case "race-free with loop constraint" `Quick
            test_safe_with_cond;
        ] );
    ]
