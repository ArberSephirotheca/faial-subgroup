open Barrier_div
open Stage0
open Protocols
open Exp

(* Convenience constructors. *)
let v (name : string) : Variable.t = Variable.from_name name
let var (name : string) : nexp = Var (v name)

let access : Code.t =
  Code.Access { array = v "a"; index = [ Num 0 ]; mode = Access.Mode.Read }

(* For testing, use the label as the array name so distinct labels
   produce distinct sync ids. Real kernels will use the __syncthreads
   sentinel or named-barrier variables. *)
let sync_at (label : string) : Sync.t =
  { Sync.mode = Sync.Mode.ArriveAndWait; id = Var (v label);
    participants = None; loc = Some Location.empty }

let mk_sync (label : string) : Code.t = Code.Sync (sync_at label)

(* head_of *)

let test_head_skip () =
  Alcotest.(check bool) "skip yields none" true
    (Thread.head_of Code.Skip = None)

let test_head_access_alone () =
  match Thread.head_of access with
  | Some (Code.Access _, Code.Skip) -> ()
  | _ -> Alcotest.fail "access alone should yield (Access, Skip)"

let test_head_seq_skip_left () =
  (* Skip; Access  →  head is Access, rest is Skip *)
  let proto = Code.Seq (Code.Skip, access) in
  match Thread.head_of proto with
  | Some (Code.Access _, Code.Skip) -> ()
  | _ -> Alcotest.fail "Skip; Access should yield (Access, Skip)"

let test_head_seq_access_then_sync () =
  (* Access; Sync  →  head is Access, rest is Sync *)
  let proto = Code.Seq (access, mk_sync "s1") in
  match Thread.head_of proto with
  | Some (Code.Access _, Code.Sync _) -> ()
  | _ -> Alcotest.fail "Access; Sync should yield (Access, Sync)"

let test_head_decl_transparent () =
  (* Decl { body = Access }  →  head_of descends through Decl *)
  let proto =
    Code.Decl { var = v "x"; ty = C_type.int; pre = None; body = access }
  in
  match Thread.head_of proto with
  | Some (Code.Access _, Code.Skip) -> ()
  | _ -> Alcotest.fail "decl-wrapped access should be transparent"

let test_head_decl_in_seq () =
  (* Seq (Decl { body = Access }, Sync)  →  head is Access, rest is Sync *)
  let proto =
    Code.Seq
      ( Code.Decl
          { var = v "x"; ty = C_type.int; pre = None; body = access },
        mk_sync "s1" )
  in
  match Thread.head_of proto with
  | Some (Code.Access _, Code.Sync _) -> ()
  | _ -> Alcotest.fail "decl in seq should be transparent"

(* step *)

let test_step_empty () =
  let t = { Thread.path_cond = Bool true; proto = Code.Skip } in
  match Thread.step t with
  | Terminated -> ()
  | _ -> Alcotest.fail "empty proto should terminate"

let test_step_access () =
  let t = { Thread.path_cond = Bool true; proto = access } in
  match Thread.step t with
  | Step [ t' ] when t'.proto = Code.Skip -> ()
  | _ -> Alcotest.fail "access should reduce to Step [skip]"

let test_step_sync () =
  let t = { Thread.path_cond = Bool true; proto = mk_sync "s1" } in
  match Thread.step t with
  | Sync { sync = s; rest = { proto = Code.Skip; _ } } when s.mode = Sync.Mode.ArriveAndWait ->
      ()
  | _ -> Alcotest.fail "sync should yield Sync action"

let test_step_if_forks () =
  (* if (x < 5) { skip } else { skip }  →  forks into two classes *)
  let cond = n_lt (var "x") (Num 5) in
  let proto = Code.If (cond, Code.Skip, Code.Skip) in
  let t = { Thread.path_cond = Bool true; proto } in
  match Thread.step t with
  | Step [ then_t; else_t ] ->
      Alcotest.(check bool) "then condition strengthens" true
        (then_t.path_cond = b_and (Bool true) cond);
      Alcotest.(check bool) "else condition strengthens" true
        (else_t.path_cond = b_and (Bool true) (b_not cond))
  | _ -> Alcotest.fail "if should fork into two classes"

let test_step_loop_forks () =
  (* for i in [0, K) skip   →   forks into active iteration and empty range *)
  let k = var "K" in
  let i = v "i" in
  let r = Range.{
    var = i;
    ty = C_type.int;
    lower_bound = Num 0;
    upper_bound = k;
    step = Plus (Num 1);
    dir = Increase;
  } in
  let proto = Code.Loop { range = r; body = Code.Skip } in
  let t = { Thread.path_cond = Bool true; proto } in
  match Thread.step t with
  | Step [ _active; _empty ] -> ()
  | _ -> Alcotest.fail "loop should fork into active and empty branches"

(* references *)

let test_references_present () =
  let s1 = sync_at "s1" in
  let proto = Code.Seq (access, Code.Sync s1) in
  let t = { Thread.path_cond = Bool true; proto } in
  Alcotest.(check bool) "references s1" true (Thread.references ~sync:s1 t)

let test_references_absent () =
  let s1 = sync_at "s1" in
  let s2 = { (sync_at "s1") with id = Var (v "other_barrier") } in
  let proto = Code.Sync s2 in
  let t = { Thread.path_cond = Bool true; proto } in
  Alcotest.(check bool) "does not reference s1" false
    (Thread.references ~sync:s1 t)

(* test groups *)

let head_of_tests = [
  ("skip yields none",         `Quick, test_head_skip);
  ("access alone",             `Quick, test_head_access_alone);
  ("Skip; Access",             `Quick, test_head_seq_skip_left);
  ("Access; Sync",             `Quick, test_head_seq_access_then_sync);
  ("Decl is transparent",      `Quick, test_head_decl_transparent);
  ("Decl in Seq is transparent", `Quick, test_head_decl_in_seq);
]

let step_tests = [
  ("empty proto",              `Quick, test_step_empty);
  ("access",                   `Quick, test_step_access);
  ("sync",                     `Quick, test_step_sync);
  ("if forks",                 `Quick, test_step_if_forks);
  ("loop forks",               `Quick, test_step_loop_forks);
]

let references_tests = [
  ("references present",       `Quick, test_references_present);
  ("references absent",        `Quick, test_references_absent);
]

let () =
  Alcotest.run "thread" [
    ("head_of", head_of_tests);
    ("step", step_tests);
    ("references", references_tests);
  ]
