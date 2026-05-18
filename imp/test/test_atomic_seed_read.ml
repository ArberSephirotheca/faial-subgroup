open Protocols
open Imp
open Infer_stmt

(* Helpers *)
let var (name : string) : Variable.t = Variable.from_name name

let atomic_cas : Infer_exp.t Atomic.t =
  Atomic.from_name (Variable.from_name "atomicCAS") |> Option.get

let atomic_add : Infer_exp.t Atomic.t =
  Atomic.from_name (Variable.from_name "atomicAdd") |> Option.get

let read_int ~target ~array : t =
  Read
    {
      target = Some (C_type.int, var target);
      array = var array;
      index = [ Infer_exp.NExp (Num 0) ];
    }

let atomic_with ~target ~array ~atomic () : t =
  Atomic
    {
      target = var target;
      ty = C_type.int;
      atomic;
      array = var array;
      index = [ Infer_exp.NExp (Num 0) ];
    }

(* CAS atomic with [expected] folded into the operation, mirroring
   what the frontends produce. *)
let cas_with_expected (expected : Infer_exp.t) : Infer_exp.t Atomic.t =
  {
    atomic_cas with
    operation = Atomic.Operation.CAS { expected = Some expected; new_val = None };
  }

let decl_set ~var:v ~init : t =
  Decl
    {
      var = var v;
      ty = C_type.int;
      init = Some (Infer_exp.NExp (Var (var init)));
    }

let assign ~var:v ~data : t =
  Assign { var = var v; ty = C_type.int; data = Infer_exp.NExp (Var (var data)) }

(* Walk a result Stmt.t and return a list of (kind, target-name) pairs
   for every Read / Atomic, in source order. *)
let rec access_summary : t -> (string * string) list = function
  | Skip | Sync _ | SyncOp _ | Assert _ | Write _ | LocationAlias _
  | Call _ | Break | Continue | Return _ | Decl _ | Assign _ ->
      []
  | Read { target = Some (_, t); _ } -> [ ("read", Variable.name t) ]
  | Read { target = None; _ } -> [ ("read", "_") ]
  | Atomic { target = t; atomic; _ } ->
      [ ("atomic:" ^ Atomic.Operation.to_string atomic.operation, Variable.name t) ]
  | Seq (a, b) -> access_summary a @ access_summary b
  | If (_, p, q) -> access_summary p @ access_summary q
  | While (_, p) | DoWhile (_, p) -> access_summary p
  | For { init; inc; body; _ } ->
      access_summary init @ access_summary inc @ access_summary body

let pairs : (string * string) list Alcotest.testable =
  Alcotest.list (Alcotest.pair Alcotest.string Alcotest.string)

(* Fixture 1: classic CAS-spin seed-read chain.
     int seed = *p;
     decl old = seed;
     atomicCAS on p with expected=old
   → seed read should be re-tagged as atomic. *)
let test_retag_through_one_alias () : unit =
  let body =
    from_list
      [
        read_int ~target:"seed" ~array:"p";
        decl_set ~var:"old" ~init:"seed";
        atomic_with ~target:"r" ~array:"p"
          ~atomic:(cas_with_expected (Infer_exp.NExp (Var (var "old")))) ();
      ]
  in
  let result = Atomic_seed_read.rewrite body in
  Alcotest.check pairs
    "seed read becomes atomic (cas), the CAS stays an atomic (cas)"
    [ ("atomic:atomicCAS", "seed"); ("atomic:atomicCAS", "r") ]
    (access_summary result)

(* Fixture 2: two-step alias chain.
     int seed = *p;
     decl mid = seed;
     decl old = mid;
     atomicCAS on p with expected=old. *)
let test_retag_through_two_aliases () : unit =
  let body =
    from_list
      [
        read_int ~target:"seed" ~array:"p";
        decl_set ~var:"mid" ~init:"seed";
        decl_set ~var:"old" ~init:"mid";
        atomic_with ~target:"r" ~array:"p"
          ~atomic:(cas_with_expected (Infer_exp.NExp (Var (var "old")))) ();
      ]
  in
  let result = Atomic_seed_read.rewrite body in
  Alcotest.check pairs "retag through two-deep alias chain"
    [ ("atomic:atomicCAS", "seed"); ("atomic:atomicCAS", "r") ]
    (access_summary result)

(* Fixture 3: ret reassigned inside a loop — the multi-source alias
   set must keep both the seed and the CAS-return as sources of [ret],
   otherwise the seed→expected chain breaks across iterations.
     int seed = *p;
     decl ret = seed;
     while (...) {
       decl old = ret;
       atomicCAS expected=old, target=cas_ret;
       assign ret = cas_ret;
     }
*)
let test_retag_with_loop_carry () : unit =
  let body =
    from_list
      [
        read_int ~target:"seed" ~array:"p";
        decl_set ~var:"ret" ~init:"seed";
        While
          ( Infer_exp.true_,
            from_list
              [
                decl_set ~var:"old" ~init:"ret";
                atomic_with ~target:"cas_ret" ~array:"p"
                  ~atomic:(cas_with_expected (Infer_exp.NExp (Var (var "old"))))
                  ();
                assign ~var:"ret" ~data:"cas_ret";
              ] );
      ]
  in
  let result = Atomic_seed_read.rewrite body in
  Alcotest.check pairs "loop-carried ret doesn't drop the seed link"
    [ ("atomic:atomicCAS", "seed"); ("atomic:atomicCAS", "cas_ret") ]
    (access_summary result)

(* Fixture 4: NEGATIVE — fast-path branch.
   The plain read on (p,[0]) is consumed only by a branch condition;
   the atomicCAS exists on the same address but its expected does NOT
   trace back to the seed's target. The read must stay a Read. *)
let test_no_retag_when_seed_doesnt_feed_cas () : unit =
  let body =
    from_list
      [
        read_int ~target:"snapshot" ~array:"p";
        (* snapshot is never aliased into the atomicCAS's expected. *)
        atomic_with ~target:"r" ~array:"p"
          ~atomic:
            (cas_with_expected (Infer_exp.NExp (Var (var "some_other_var"))))
          ();
      ]
  in
  let result = Atomic_seed_read.rewrite body in
  Alcotest.check pairs "fast-path branch: seed read stays a plain Read"
    [ ("read", "snapshot"); ("atomic:atomicCAS", "r") ]
    (access_summary result)

(* Fixture 5: NEGATIVE — non-CAS atomic.
   atomicAdd on the same address doesn't have an "expected" contract,
   so even if a plain read's target appears to alias into the atomic's
   not-actually-present expected, no re-tag fires. *)
let test_no_retag_for_non_cas_atomic () : unit =
  let body =
    from_list
      [
        read_int ~target:"seed" ~array:"p";
        atomic_with ~target:"r" ~array:"p" ~atomic:atomic_add ();
      ]
  in
  let result = Atomic_seed_read.rewrite body in
  Alcotest.check pairs "atomicAdd is not a CAS — seed read stays"
    [ ("read", "seed"); ("atomic:atomicAdd", "r") ]
    (access_summary result)

(* Fixture 6: NEGATIVE — different addresses.
   Plain read on (q,[0]); atomicCAS on (p,[0]) with expected from a
   chain into seed. Different arrays → no match. *)
let test_no_retag_when_addresses_differ () : unit =
  let body =
    from_list
      [
        read_int ~target:"seed" ~array:"q";
        decl_set ~var:"old" ~init:"seed";
        atomic_with ~target:"r" ~array:"p"
          ~atomic:(cas_with_expected (Infer_exp.NExp (Var (var "old")))) ();
      ]
  in
  let result = Atomic_seed_read.rewrite body in
  Alcotest.check pairs "different addresses: read on q stays a plain Read"
    [ ("read", "seed"); ("atomic:atomicCAS", "r") ]
    (access_summary result)

let () =
  Alcotest.run "Atomic_seed_read"
    [
      ( "rewrite",
        [
          ("retag through one alias", `Quick, test_retag_through_one_alias);
          ("retag through two aliases", `Quick, test_retag_through_two_aliases);
          ("retag with loop-carried ret", `Quick, test_retag_with_loop_carry);
          ( "no retag when seed doesn't feed CAS",
            `Quick,
            test_no_retag_when_seed_doesnt_feed_cas );
          ("no retag for non-CAS atomic", `Quick, test_no_retag_for_non_cas_atomic);
          ( "no retag when addresses differ",
            `Quick,
            test_no_retag_when_addresses_differ );
        ] );
    ]
