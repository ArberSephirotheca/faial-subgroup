open Stage0
open Protocols
open Exp
open Drf

let arch : Architecture.t = Architecture.Block
let array_a : Variable.t = Variable.from_name "A"
let tid : nexp = Var Variable.tid_x
let n : Variable.t = Variable.from_name "n"

let shared_int : Memory.t =
  Memory.from_type Mem_hierarchy.SharedMemory C_type.int

let write (idx : nexp) : Code.t = Code.Access (Access.write array_a [ idx ] None)

let mk_kernel (pre : bexp) : Kernel.t =
  {
    name = "k_test";
    global_variables = Params.add n C_type.int Params.empty;
    local_variables = Params.empty;
    arrays = Variable.Map.add array_a shared_int Variable.Map.empty;
    pre;
    code = Code.seq (write tid) (write (n_plus tid (Num 1)));
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

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

let proof_pred_names (p : Symbexp.Proof.t) : string list =
  List.map (fun (x : Predicates.t) -> x.name) p.preds

(* [Predicates.b_inline] is a single pass, not a fixpoint, and
   [__is_pow2]'s body is itself a predicate call, [pow2]. So the one
   inline that [project_pre] applies rewrites [__is_pow2(n)] to
   [pow2(n)] and stops there, and the named predicate is what reaches
   the emitted query. A second inline anywhere between projection and
   [SymGoal.make] would expand [pow2] into its body, emptying the
   query's predicate list and replacing the call with a long
   disjunction over the powers of two. The solver is indifferent,
   since [Solve_drf] inlines the assembled goal anyway, so nothing
   comparing exit statuses can see the difference. *)
let test_named_predicate_reaches_the_goal () =
  let proofs = race_proofs (mk_kernel (Pred ("__is_pow2", [ Var n ]))) in
  Alcotest.(check bool) "at least one proof emitted" true (proofs <> []);
  Alcotest.(check bool) "a proof declares the pow2 predicate" true
    (List.exists (fun p -> List.mem "pow2" (proof_pred_names p)) proofs);
  Alcotest.(check bool) "the goal still calls pow2 rather than its body" true
    (List.exists
       (fun (p : Symbexp.Proof.t) ->
         Predicates.get_predicates (Formula.goal p.formula)
         |> List.exists (fun (x : Predicates.t) -> x.name = "pow2"))
       proofs)

(* A kernel carrying no predicate must not acquire one. *)
let test_no_predicate_when_none_given () =
  let proofs = race_proofs (mk_kernel (Bool true)) in
  Alcotest.(check bool) "at least one proof emitted" true (proofs <> []);
  Alcotest.(check bool) "no proof declares a predicate" true
    (List.for_all (fun p -> proof_pred_names p = []) proofs)

let () =
  Alcotest.run "goal_predicates"
    [
      ( "emitted query",
        [
          Alcotest.test_case "a named predicate survives into the proof" `Quick
            test_named_predicate_reaches_the_goal;
          Alcotest.test_case "no predicate is invented" `Quick
            test_no_predicate_when_none_given;
        ] );
    ]
