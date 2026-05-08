open Stage0
open Protocols
open Exp
open Barrier_div

let v (name : string) : Variable.t = Variable.from_name name

let mk_sync ?(array = "__syncthreads") ?(addend : nexp option = None)
    ?(participants = None) () : Sync.t =
  let id : nexp =
    match addend with
    | None -> Var (v array)
    | Some a -> Exp.n_plus (Var (v array)) a
  in
  {
    Sync.mode = Sync.Mode.ArriveAndWait;
    id;
    participants;
    loc = Some Location.empty;
  }

let cfg : Rel_cost.Config.t =
  let block_dim = Dim3.make ~x:32 () in
  let grid_dim = Dim3.one in
  Rel_cost.Config.make ~block_dim ~grid_dim ()

let locals = Variable.tid_set

let mk_thread (path_cond : bexp) : Thread.t =
  { Thread.path_cond; proto = Code.Skip }

(* tests *)

let test_empty_state_no_diagnostics () =
  let s : State.t = { phases = []; threads = [] } in
  Alcotest.(check int) "no diagnostics on empty state" 0
    (List.length (Diagnostic.of_state ~pre:(Bool true) cfg locals s))

let test_finished_phase_no_diagnostics () =
  (* arrive_cohort = true, count = 32 → exact match, no diagnostic *)
  let s = mk_sync () in
  let p = Phase.of_sync cfg { sync = s; rest = mk_thread (Bool true) } in
  let st : State.t = { phases = [ p ]; threads = [] } in
  Alcotest.(check int) "no diagnostics on finished phase" 0
    (List.length (Diagnostic.of_state ~pre:(Bool true) cfg locals st))

let test_missing_participants_diagnostic () =
  (* arrive_cohort = (tid<17), count = 32 → Missing_participants *)
  let s = mk_sync () in
  let pc = n_lt (Var Variable.tid_x) (Num 17) in
  let p = Phase.of_sync cfg { sync = s; rest = mk_thread pc } in
  let st : State.t = { phases = [ p ]; threads = [] } in
  match Diagnostic.of_state ~pre:(Bool true) cfg locals st with
  | [ Diagnostic.Missing_participants { expected = 32; _ } ] -> ()
  | results ->
      Alcotest.failf "expected one Missing_participants, got %d diagnostics"
        (List.length results)

let test_count_mismatch_diagnostic () =
  (* Two phases on same id, different counts → Count_mismatch *)
  let s_16 = mk_sync ~participants:(Some (Num 16)) () in
  let s_32 = mk_sync ~participants:(Some (Num 32)) () in
  let p1 =
    Phase.of_sync cfg { sync = s_16; rest = mk_thread (Bool true) }
  in
  let p2 =
    Phase.of_sync cfg { sync = s_32; rest = mk_thread (Bool true) }
  in
  let st : State.t = { phases = [ p1; p2 ]; threads = [] } in
  let results = Diagnostic.of_state ~pre:(Bool true) cfg locals st in
  let has_count_mismatch =
    List.exists (function Diagnostic.Count_mismatch _ -> true | _ -> false)
      results
  in
  Alcotest.(check bool) "Count_mismatch reported" true has_count_mismatch

let tests =
  [
    ("empty state", `Quick, test_empty_state_no_diagnostics);
    ("finished phase no diag", `Slow, test_finished_phase_no_diagnostics);
    ("missing participants", `Slow, test_missing_participants_diagnostic);
    ("count mismatch", `Slow, test_count_mismatch_diagnostic);
  ]

let () = Alcotest.run "diagnostic" [ ("classify", tests) ]
