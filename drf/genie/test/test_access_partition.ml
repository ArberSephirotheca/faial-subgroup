open Protocols
open Drf_genie

(* Helpers for building tiny kernels. The body is a single straight-
   line program with one or two [Code.Access] nodes wrapped in
   guards/loops; the surrounding kernel scaffolding is the minimum
   [Kernel.t] requires. *)

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

let is_param_free (e : Access_partition.entry) : bool =
  match e.klass with
  | Parameter_free _ -> true
  | Parameter_touching _ -> false

let params_of (e : Access_partition.entry) : Variable.Set.t =
  match e.klass with
  | Parameter_free _ -> Variable.Set.empty
  | Parameter_touching { params; _ } -> params

let set_of_names (names : string list) : Variable.Set.t =
  names |> List.map Variable.from_name |> Variable.Set.of_list

let check_set (msg : string) (expected : Variable.Set.t) (got : Variable.Set.t)
    : unit =
  let to_list s =
    s |> Variable.Set.elements |> List.map Variable.name |> List.sort compare
  in
  Alcotest.(check (list string)) msg (to_list expected) (to_list got)

(* A. Path cond uses only built-ins. *)
let test_a_pure_builtins () =
  let body =
    Code.if_ (lt (var "threadIdx.x") (var "blockDim.x"))
      (access (Exp.Num 0)) Code.Skip
  in
  let k = mk_kernel body in
  match Access_partition.partition k with
  | [ e ] ->
    Alcotest.(check bool) "classified parameter-free" true (is_param_free e)
  | es ->
    Alcotest.failf "expected 1 entry, got %d" (List.length es)

(* B. Path cond mixes built-in and a single param [N]. *)
let test_b_single_param () =
  let body =
    Code.if_ (lt (var "threadIdx.x") (var "N"))
      (access (Exp.Num 0)) Code.Skip
  in
  let k = mk_kernel ~globals:[ ("N", C_type.int) ] body in
  match Access_partition.partition k with
  | [ e ] ->
    Alcotest.(check bool) "classified parameter-touching"
      false (is_param_free e);
    check_set "params = {N}" (set_of_names [ "N" ]) (params_of e)
  | es ->
    Alcotest.failf "expected 1 entry, got %d" (List.length es)

(* C. Path cond mixes two params. *)
let test_c_two_params () =
  let body =
    Code.if_
      (Exp.b_and
         (lt (var "threadIdx.x") (var "N"))
         (gt (var "M") (Exp.Num 0)))
      (access (Exp.Num 0)) Code.Skip
  in
  let k =
    mk_kernel
      ~globals:[ ("N", C_type.int); ("M", C_type.int) ]
      body
  in
  match Access_partition.partition k with
  | [ e ] ->
    Alcotest.(check bool) "classified parameter-touching"
      false (is_param_free e);
    check_set "params = {N, M}" (set_of_names [ "N"; "M" ]) (params_of e)
  | es ->
    Alcotest.failf "expected 1 entry, got %d" (List.length es)

(* D. Nested guards: outer if uses N, inner if uses M; the access
   inside sits under the conjunction of both. *)
let test_d_nested_guards () =
  let inner =
    Code.if_ (gt (var "M") (Exp.Num 0))
      (access (Exp.Num 0)) Code.Skip
  in
  let body =
    Code.if_ (lt (var "threadIdx.x") (var "N")) inner Code.Skip
  in
  let k =
    mk_kernel
      ~globals:[ ("N", C_type.int); ("M", C_type.int) ]
      body
  in
  match Access_partition.partition k with
  | [ e ] ->
    Alcotest.(check bool) "classified parameter-touching"
      false (is_param_free e);
    check_set "params = {N, M}" (set_of_names [ "N"; "M" ]) (params_of e)
  | es ->
    Alcotest.failf "expected 1 entry, got %d" (List.length es)

(* E. Loop range bound depends on param N — the access inside picks up
   N through [Range.to_cond] in [walk]'s path condition. *)
let test_e_loop_param_bound () =
  let i = Variable.from_name "i" in
  let range = Range.make i (var "N") in
  let body = Code.loop range (access (Exp.Var i)) in
  let k = mk_kernel ~globals:[ ("N", C_type.int) ] body in
  match Access_partition.partition k with
  | [ e ] ->
    Alcotest.(check bool) "classified parameter-touching"
      false (is_param_free e);
    let ps = params_of e in
    Alcotest.(check bool) "params contains N"
      true (Variable.Set.mem (Variable.from_name "N") ps)
  | es ->
    Alcotest.failf "expected 1 entry, got %d" (List.length es)

(* F. Universe is the union across entries; empty access list and
   all-parameter-free both give an empty universe. *)
let test_f_parameter_universe () =
  (* Two accesses, one under N, one under M. *)
  let acc_n =
    Code.if_ (lt (var "threadIdx.x") (var "N"))
      (access (Exp.Num 0)) Code.Skip
  in
  let acc_m =
    Code.if_ (gt (var "M") (Exp.Num 0))
      (access (Exp.Num 1)) Code.Skip
  in
  let k_both =
    mk_kernel
      ~globals:[ ("N", C_type.int); ("M", C_type.int) ]
      (Code.seq acc_n acc_m)
  in
  let entries = Access_partition.partition k_both in
  Alcotest.(check int) "two entries" 2 (List.length entries);
  check_set "universe = {N, M}"
    (set_of_names [ "N"; "M" ])
    (Access_partition.parameter_universe entries);

  (* Empty access list. *)
  let k_empty = mk_kernel Code.Skip in
  let entries_empty = Access_partition.partition k_empty in
  Alcotest.(check int) "no entries" 0 (List.length entries_empty);
  check_set "empty universe on empty kernel"
    Variable.Set.empty
    (Access_partition.parameter_universe entries_empty);

  (* All accesses parameter-free. *)
  let pf_body =
    Code.if_ (lt (var "threadIdx.x") (var "blockDim.x"))
      (access (Exp.Num 0)) Code.Skip
  in
  let k_pf = mk_kernel pf_body in
  let entries_pf = Access_partition.partition k_pf in
  Alcotest.(check int) "one entry" 1 (List.length entries_pf);
  Alcotest.(check bool) "entry is parameter-free"
    true (List.for_all is_param_free entries_pf);
  check_set "empty universe when all parameter-free"
    Variable.Set.empty
    (Access_partition.parameter_universe entries_pf)

(* Parameter present only in the access index, not the path condition.
   The DRF solver reasons about address equality across two threads
   under the kernel's pre, so an abductive clause like [P == dim] is
   load-bearing when the index expression involves [P] even though
   the path condition does not — classifier must surface [P]. *)
let test_param_in_index_only () =
  let p = Variable.from_name "P" in
  let body =
    Code.if_ (lt (var "threadIdx.x") (var "blockDim.x"))
      (Code.Access
         (Access.read array_a
            [ Exp.n_plus (Exp.Var (Variable.from_name "threadIdx.x"))
                (Exp.Var p) ]))
      Code.Skip
  in
  let k = mk_kernel ~globals:[ ("P", C_type.int) ] body in
  match Access_partition.partition k with
  | [ e ] ->
    Alcotest.(check bool) "classified parameter-touching"
      false (is_param_free e);
    Alcotest.(check bool) "params contains P"
      true (Variable.Set.mem p (params_of e))
  | es ->
    Alcotest.failf "expected 1 entry, got %d" (List.length es)

(* Pins [Variable.is_thread_index] on the canonical six thread-
   divergent built-ins and on three negative cases (a dim built-in,
   a grid-dim built-in, and a freshly-named kernel parameter). The
   predicate is load-bearing for [Access_partition.abductive_scope]
   in [test_abductive_scope_excludes_thread_indices] below; pin its
   contract here so a future rename or mis-spelling fails loudly. *)
let test_is_thread_index () =
  let check name expected v =
    Alcotest.(check bool) name expected (Variable.is_thread_index v)
  in
  check "tid_x"  true  Variable.tid_x;
  check "tid_y"  true  Variable.tid_y;
  check "tid_z"  true  Variable.tid_z;
  check "bid_x"  true  Variable.bid_x;
  check "bid_y"  true  Variable.bid_y;
  check "bid_z"  true  Variable.bid_z;
  check "bdim_x" false Variable.bdim_x;
  check "gdim_z" false Variable.gdim_z;
  check "N"      false (Variable.from_name "N")

(* This test pins thread-invariance as the filter criterion for
   [Access_partition.abductive_scope], defending against a future IR
   change that adds thread indices to kernel parameter sets. The
   filter must be semantic ("is this variable uniform across
   threads?") rather than structural ("is this variable a kernel
   binder?"), because Φ from abductive synthesis is conjoined into
   [k.pre] and so constrains every thread uniformly — a Φ that
   mentions [threadIdx.x] would shrink the active thread set, which
   is a trivialising clearance.

   The kernel here simulates the post-IR-closure state by adding
   [threadIdx.x] to [local_variables] directly. [k.pre] references
   both [threadIdx.x] and a regular parameter [N]; the explicit
   thread-index exclusion in [abductive_universe] keeps [tid_x] out
   of [abductive_scope] regardless of where it is bound. *)
let test_abductive_scope_excludes_thread_indices () =
  let n = Variable.from_name "N" in
  let body =
    Code.if_ (lt (var "threadIdx.x") (var "N"))
      (access (Exp.Num 0)) Code.Skip
  in
  let k0 = mk_kernel ~globals:[ ("N", C_type.int) ] body in
  (* Splice [tid_x] into [local_variables] and reference both
     [tid_x] and [N] in [pre]. *)
  let local_variables =
    Params.add Variable.tid_x C_type.int k0.local_variables
  in
  let pre =
    Exp.b_and
      (lt (var "threadIdx.x") (Exp.Num 1024))
      (gt (var "N") (Exp.Num 0))
  in
  let k = { k0 with local_variables; pre } in
  let scope = Access_partition.abductive_scope k in
  Alcotest.(check bool) "N in scope"
    true (Variable.Set.mem n scope);
  Alcotest.(check bool) "tid_x not in scope"
    false (Variable.Set.mem Variable.tid_x scope);
  (* Sanity: dim built-ins remain in scope as before. *)
  List.iter
    (fun (label, v) ->
      Alcotest.(check bool) (label ^ " in scope")
        true (Variable.Set.mem v scope))
    [
      ("bdim_x", Variable.bdim_x);
      ("bdim_y", Variable.bdim_y);
      ("bdim_z", Variable.bdim_z);
      ("gdim_x", Variable.gdim_x);
      ("gdim_y", Variable.gdim_y);
      ("gdim_z", Variable.gdim_z);
    ]

(* Companion to the test above: pin the explicit thread-invariance
   contract on the helper that encodes it. [abductive_universe] is
   the set of variables whose pre-occurrences are kept by
   [abductive_scope]'s intersection with [b_free_names k.pre]; its
   exclusion of [Variable.thread_index_set] is what makes the filter
   robust to future IR changes that move [threadIdx.*]/[blockIdx.*]
   into [k.global_variables]/[k.local_variables]. *)
let test_abductive_universe_excludes_thread_indices () =
  (* Pre-stage the kernel parameters with [tid_x] *and* [bid_y]
     bound as locals, plus a regular param [N] and a dim built-in
     [bdim_x]. The expected universe is the dim built-ins union [N]
     — tids/bids are excluded by the explicit set diff. *)
  let body = access (Exp.Num 0) in
  let k0 = mk_kernel ~globals:[ ("N", C_type.int) ] body in
  let local_variables =
    k0.local_variables
    |> Params.add Variable.tid_x C_type.int
    |> Params.add Variable.bid_y C_type.int
    |> Params.add Variable.bdim_x C_type.int
  in
  let k = { k0 with local_variables } in
  let universe = Access_partition.abductive_universe k in
  Alcotest.(check bool) "N in universe"
    true (Variable.Set.mem (Variable.from_name "N") universe);
  Alcotest.(check bool) "tid_x not in universe"
    false (Variable.Set.mem Variable.tid_x universe);
  Alcotest.(check bool) "bid_y not in universe"
    false (Variable.Set.mem Variable.bid_y universe);
  Alcotest.(check bool) "bdim_x in universe"
    true (Variable.Set.mem Variable.bdim_x universe);
  Alcotest.(check bool) "gdim_z in universe"
    true (Variable.Set.mem Variable.gdim_z universe)

let tests = [
  ("A. pure built-ins -> parameter-free",   `Quick, test_a_pure_builtins);
  ("B. single param N",                     `Quick, test_b_single_param);
  ("C. two params N, M",                    `Quick, test_c_two_params);
  ("D. nested guards combine params",       `Quick, test_d_nested_guards);
  ("E. loop bound param contributes",       `Quick, test_e_loop_param_bound);
  ("F. parameter universe + edges",         `Quick, test_f_parameter_universe);
  ("param appears in index only",           `Quick, test_param_in_index_only);
  ("Variable.is_thread_index pins six tids/bids",
    `Quick, test_is_thread_index);
  ("abductive_scope excludes thread indices",
    `Quick, test_abductive_scope_excludes_thread_indices);
  ("abductive_universe excludes thread indices",
    `Quick, test_abductive_universe_excludes_thread_indices);
]

let () =
  Alcotest.run "drf_genie/access_partition" [
    ("Access_partition", tests);
  ]
