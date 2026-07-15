(*
 Given a flat kernel
 *)

open Stage0
open Protocols

let ( @ ) = Common.append_tr

open Common
open Exp
open Flatacc

module Ids = struct
  let prefix (t : Task.t) : string = "$" ^ Task.to_string t ^ "$"

  (* Variable representing index accessing the array *)
  let index (t : Task.t) (n : int) : string =
    prefix t ^ "idx$" ^ string_of_int n

  (* The access identifier *)
  let access_id (t : Task.t) : string = prefix t ^ "id"
end

module Gen = struct
  let var (x : string) : nexp = Var (Variable.from_name x)
  let index (t : Task.t) (n : int) : nexp = Ids.index t n |> var

  (* Access mode *)
  let mode (t : Task.t) : nexp = var (Ids.prefix t ^ "mode")
  let mode_read : nexp = Num 0
  let mode_write : nexp = Num 1
  let mode_atomic_dev : nexp = Num 2
  let mode_atomic_block : nexp = Num 3

  let mode_to_nexp (m : Access.Mode.t) : nexp =
    match m with
    | Read -> mode_read
    | Write _ -> mode_write
    | Atomic x -> (
        match x.scope with
        | Device | System -> mode_atomic_dev
        | Block -> mode_atomic_block)

  let assign_mode (t : Task.t) (m : Access.Mode.t) : bexp =
    n_eq (mode t) (mode_to_nexp m)

  (* Write-value signature. Two writes of the same statically-known
     value to the same location do not race (a benign data-race, see
     [Access.Mode.can_conflict]). To decide that inside the race goal
     rather than after solving a single model, each access carries its
     write value: a [Write (Some n)] sets [wknown = 1] and [wval = n];
     every other access (read, atomic, or a value-unknown write) sets
     [wknown = 0] so it is never paired off as a benign write. *)
  let wval (t : Task.t) : nexp = var (Ids.prefix t ^ "wval")
  let wknown (t : Task.t) : nexp = var (Ids.prefix t ^ "wknown")

  let assign_value (t : Task.t) (m : Access.Mode.t) : bexp =
    match m with
    | Write (Some n) -> b_and (n_eq (wknown t) (Num 1)) (n_eq (wval t) (Num n))
    | _ -> n_eq (wknown t) (Num 0)

  let access_id (t : Task.t) : nexp = Ids.access_id t |> var

  (* assign identifier of the conditional access *)
  let assign_access_id (t : Task.t) (aid : int) : bexp =
    n_eq (access_id t) (Num aid)

  let assign_index (op : N_rel.t) (t : Task.t) (idx : int) (n : nexp) : bexp =
    n_rel op (index t idx) n

  (* Constrains the indices to be all non-negative and match for both threads. *)
  let assign_dim (dim : int) : bexp =
    (* Make sure all indices match *)
    (* idx0$T1 = idx0$T2  /\ idx1$T1 = idx1$T2 ... /\ idxn$T1 = idxn$T2 /\
      idx0$T1 >= 0 /\ ... /\ idxn$T1 >= 0
    *)
    range (dim - 1)
    |> List.map (fun i ->
        let t1 = index Task1 i in
        let t2 = index Task2 i in
        b_and_ex [ n_eq t1 t2; n_ge t1 (Num 0) ])
    |> b_and_ex

  let project (t : Task.t) (x : Variable.t) : Variable.t =
    (* Add a suffix to all variables to make them unique. Use $ to ensure
      these variables did not come from C *)
    let task_suffix (t : Task.t) = "$" ^ Task.to_string t in
    Variable.update_name (fun n -> n ^ task_suffix t) x

  let mode_spec arch : bexp =
    let mode1 : nexp = mode Task1 in
    let mode2 : nexp = mode Task2 in
    [
      (* when first is a read, then the second cannot be a read *)
      b_and (n_eq mode1 mode_read) (n_neq mode2 mode_read);
      (* data-race when both are writes *)
      n_eq mode1 mode_write;
      (* when the first is an atomic dev *)
      (if Architecture.is_grid arch then
         b_and (n_eq mode1 mode_atomic_dev) (n_neq mode2 mode_atomic_dev)
       else
         b_and
           (n_eq mode1 mode_atomic_dev)
           (b_or (n_eq mode2 mode_read) (n_eq mode2 mode_write)));
      (if Architecture.is_grid arch then n_eq mode1 mode_atomic_block
       else
         b_and
           (n_eq mode1 mode_atomic_block)
           (b_or (n_eq mode2 mode_read) (n_eq mode2 mode_write)));
    ]
    |> b_or_ex
end

(*
  For each thread-local variable x generate x$1 and x$2 to represent the
  thread-local assignments of each thread.
 *)
let rec project_n (locals : Variable.Set.t) (t : Task.t) (n : nexp) : nexp =
  match n with
  | Num _ -> n
  | CastInt e -> CastInt (project_b locals t e)
  | Var x when Variable.Set.mem x locals -> Var (Gen.project t x)
  | Var _ -> n
  | Unary (o, e) -> Unary (o, project_n locals t e)
  | Binary (o, n1, n2) -> Binary (o, project_n locals t n1, project_n locals t n2)
  | NIf (b, n1, n2) ->
      NIf (project_b locals t b, project_n locals t n1, project_n locals t n2)
  | NCall (x, ns) -> NCall (x, List.map (project_n locals t) ns)
and project_b (locals : Variable.Set.t) (t : Task.t) (b : bexp) : bexp =
  match b with
  | CastBool e -> CastBool (project_n locals t e)
  | Pred (x, ns) -> Pred (x, List.map (project_n locals t) ns)
  | Bool _ -> b
  | BNot b -> BNot (project_b locals t b)
  | BRel (o, b1, b2) -> BRel (o, project_b locals t b1, project_b locals t b2)
  | NRel (o, n1, n2) -> NRel (o, project_n locals t n1, project_n locals t n2)
  | Distinct exprs -> Distinct (List.map (project_n locals t) exprs)
  | AtomicResult { target; array; index; operation } ->
      (* Rename [target] for the active task; project [index] and
         operand expressions. [array] is a global binding and
         stays unchanged. *)
      let target =
        if Variable.Set.mem target locals then Gen.project t target
        else target
      in
      let index = List.map (project_n locals t) index in
      let operation = Atomic.Operation.map (project_n locals t) operation in
      AtomicResult { target; array; index; operation }
  | ThreadUnif e ->
      (* Expand to [e_T1 = e_T2]; the equality is symmetric so the
         active task [t] doesn't affect the encoding. *)
      let _ = t in
      NRel (Eq, project_n locals Task1 e, project_n locals Task2 e)

let project_access (locals : Variable.Set.t) (t : Task.t) (ca : CondAccess.t) :
    CondAccess.t =
  let inline_acc (a : Access.t) = Access.map (project_n locals t) a in
  (* Inline cross-thread predicates ([__uniform_int] etc.) in the
     access condition before projection so [project_b]'s
     [ThreadUnif] case expands them to the per-pair equality. See
     the analogous comment on [project_pre]. *)
  let cond = Predicates.b_inline ca.cond in
  { access = inline_acc ca.access; cond = project_b locals t cond }

(* Project [k.pre] per-thread so the range conditions on hoisted-
   For binders (linearIndex etc.) constrain each task's projected
   copy rather than a single shared variable. Without this, the
   accesses' [linearIndex$T1]/[$T2] are free in the SMT while only
   a shared [linearIndex] from [pre] is constrained, and Z3 finds
   spurious same-address witnesses.

   Inline cross-thread predicates ([__uniform_int] /
   [__distinct_int]) before projecting, so [project_b]'s
   [ThreadUnif] case expands them to the per-pair equality
   [e$T1 == e$T2] (or its negation). Leaving the predicate as
   [Pred] until after projection would let [Predicates.b_inline]
   re-introduce a [ThreadUnif] downstream of [strip_cross_thread]
   with already-projected inner [Var]s; the bit-vector codegen
   rejects bare [ThreadUnif] nodes via [Gen_z3.b_to_expr]. *)
let project_pre (locals : Variable.Set.t) (pre : bexp) : bexp =
  let pre = Predicates.b_inline pre in
  b_and (project_b locals Task1 pre) (project_b locals Task2 pre)

(* Instantiates the hardware contracts for atomic operations as
   pair-wise cross-thread axioms over the race query.

   atomicCAS is linearisable: at most one thread per address sees
   [old == expected]. atomicAdd / atomicSub with a nonzero literal
   delta produce distinct returns across threads atomic-modifying
   the same cell. Both contracts are conjoined into the race
   query goal in single-thread bexp form ([$T1] / [$T2] suffixes
   baked in via [project_n]).

   Inputs come from [AtomicResult] markers carried in path
   conditions and [k.pre]: each marker holds [target] (the binding
   for the atomic's return value), [array] / [index] (the cell),
   and [operation] (kind plus captured operands). *)
module AtomicAxioms = struct
  type marker = {
    target : Variable.t;
    array : Variable.t;
    index : nexp list;
    operation : nexp Atomic.Operation.t;
  }

  let marker_compare (a : marker) (b : marker) : int =
    let c = Variable.compare a.target b.target in
    if c <> 0 then c
    else
      let c = Variable.compare a.array b.array in
      if c <> 0 then c
      else
        let c = List.compare n_compare a.index b.index in
        if c <> 0 then c
        else Atomic.Operation.compare n_compare a.operation b.operation

  (* Every [AtomicResult] marker reachable in a bexp. *)
  let rec collect_b (acc : marker list) (b : bexp) : marker list =
    match b with
    | AtomicResult { target; array; index; operation } ->
        { target; array; index; operation } :: acc
    | Bool _ -> acc
    | NRel (_, n1, n2) -> collect_n (collect_n acc n1) n2
    | BRel (_, b1, b2) -> collect_b (collect_b acc b1) b2
    | BNot b -> collect_b acc b
    | Pred (_, ns) -> List.fold_left collect_n acc ns
    | CastBool n -> collect_n acc n
    | Distinct ns -> List.fold_left collect_n acc ns
    | ThreadUnif n -> collect_n acc n

  and collect_n (acc : marker list) (n : nexp) : marker list =
    match n with
    | Var _ | Num _ -> acc
    | CastInt b -> collect_b acc b
    | Unary (_, e) -> collect_n acc e
    | Binary (_, n1, n2) -> collect_n (collect_n acc n1) n2
    | NIf (b, n1, n2) -> collect_n (collect_n (collect_b acc b) n1) n2
    | NCall (_, args) -> List.fold_left collect_n acc args

  let collect (k : Flatacc.Kernel.t) : marker list =
    let pre_markers = collect_b [] k.pre in
    let cond_markers =
      Flatacc.Code.to_list k.code
      |> List.fold_left
           (fun acc (ca : CondAccess.t) -> collect_b acc ca.cond)
           []
    in
    pre_markers @ cond_markers |> List.sort_uniq marker_compare

  (* Project a marker for task [t]: every local [Var] in [target] /
     [index] / [operation] gets the task suffix. *)
  let project_marker (locals : Variable.Set.t) (t : Task.t) (m : marker) :
      marker =
    let target =
      if Variable.Set.mem m.target locals then Gen.project t m.target
      else m.target
    in
    {
      target;
      array = m.array;
      index = List.map (project_n locals t) m.index;
      operation = Atomic.Operation.map (project_n locals t) m.operation;
    }

  (* For two atomicCAS accesses on the same array, returns
     [b_not (same_index ∧ target_T1 = expected_T1
                        ∧ target_T2 = expected_T2)].
     [None] when either operand is not a CAS with captured
     [expected], or the arrays differ. *)
  let cas_winner_axiom (locals : Variable.Set.t) (m1 : marker) (m2 : marker)
      : bexp option =
    if not (Variable.equal m1.array m2.array) then None
    else
      match m1.operation, m2.operation with
      | ( Atomic.Operation.CAS { expected = Some e1; _ },
          Atomic.Operation.CAS { expected = Some e2; _ } ) ->
          let p1 = project_marker locals Task1 m1 in
          let p2 = project_marker locals Task2 m2 in
          let same_index =
            if List.length p1.index <> List.length p2.index then None
            else
              Some
                (List.combine p1.index p2.index
                |> List.map (fun (i1, i2) -> n_eq i1 i2)
                |> b_and_ex)
          in
          let e1' = project_n locals Task1 e1 in
          let e2' = project_n locals Task2 e2 in
          let t1_won = n_eq (Var p1.target) e1' in
          let t2_won = n_eq (Var p2.target) e2' in
          Option.map
            (fun same_index ->
              b_not (b_and_ex [ same_index; t1_won; t2_won ]))
            same_index
      | _ -> None

  (* For two atomicAdd / atomicSub accesses on the same array
     with nonzero literal deltas, returns
       [same_index → target_T1 ≠ target_T2].
     [None] when either operation isn't Add/Sub with a nonzero
     literal, or arrays differ. atomicInc / atomicDec are
     excluded: their wrap semantics break distinctness when the
     counter wraps. *)
  let unique_return_axiom (locals : Variable.Set.t) (m1 : marker)
      (m2 : marker) : bexp option =
    if not (Variable.equal m1.array m2.array) then None
    else
      let is_nonzero_add_sub (op : nexp Atomic.Operation.t) : bool =
        match op with
        | Atomic.Operation.Add (Some (Num n))
        | Atomic.Operation.Sub (Some (Num n)) ->
            n <> 0
        | _ -> false
      in
      if not (is_nonzero_add_sub m1.operation && is_nonzero_add_sub m2.operation)
      then None
      else
        let p1 = project_marker locals Task1 m1 in
        let p2 = project_marker locals Task2 m2 in
        if List.length p1.index <> List.length p2.index then None
        else
          let same_index =
            List.combine p1.index p2.index
            |> List.map (fun (i1, i2) -> n_eq i1 i2)
            |> b_and_ex
          in
          let distinct_targets = b_not (n_eq (Var p1.target) (Var p2.target)) in
          Some (b_impl same_index distinct_targets)

  (* Conjunction of every applicable cross-thread axiom for the
     kernel's atomic accesses. Iterates ordered pairs of markers
     (including self-pairs, so a single site constrains T1 vs T2
     at that site). *)
  let axioms_of (k : Flatacc.Kernel.t) (locals : Variable.Set.t) : bexp =
    let markers = collect k in
    let pairs =
      List.concat_map (fun m1 -> List.map (fun m2 -> (m1, m2)) markers) markers
    in
    let mk = [ cas_winner_axiom; unique_return_axiom ] in
    pairs
    |> List.concat_map (fun (m1, m2) ->
           List.filter_map (fun f -> f locals m1 m2) mk)
    |> b_and_ex
end

(* Encoded memory-consistency-model assumptions over the race witness.
   Each assumption is projected over [Task1] and [Task2] and conjoined
   into the goal. *)
module MemoryModelAxioms = struct
  (* [same_warp ~warp_size] holds when the two tasks' linearised
     in-block thread ids fall into the same warp:
       (tid_x + bdim_x * tid_y + bdim_x * bdim_y * tid_z) / warp_size
     is equal across [Task1] and [Task2]. Under [warp_synchronous],
     same-warp pairs are implicitly barrier-ordered, so the race goal
     excludes them.

     [warp_size] is passed as a literal [Num] (not [Var "warpSize"])
     so the divisor doesn't introduce [Var / Var] non-linearity into
     the BV encoding. The [bdim_x * bdim_y] cross-term in the [tid_z]
     coefficient remains [Var * Var] unless [blockDim] is pinned
     (via [--assume-launch] or explicit [--block-dim]); in practice
     the warp-synchronous assumption is most useful when the launch
     dimensions are known. *)
  let same_warp ~(warp_size : int) : bexp =
    let v (t : Task.t) (x : Variable.t) : nexp = Var (Gen.project t x) in
    let linear (t : Task.t) : nexp =
      n_plus
        (v t Variable.tid_x)
        (n_plus
          (n_mult (v t Variable.tid_y) (Var Variable.bdim_x))
          (n_mult (v t Variable.tid_z)
            (n_mult (Var Variable.bdim_x) (Var Variable.bdim_y))))
    in
    n_eq
      (n_div (linear Task1) (Num warp_size))
      (n_div (linear Task2) (Num warp_size))

  let axiom_of (m : Memory_model.t) : bexp =
    if m.warp_synchronous then b_not (same_warp ~warp_size:32) else b_true
end

module SymAccess = struct
  (*

  Each task is represented as: the mode of access, the index of an
  n-dimensional access (eg, for [x][y], x=0 and y=1), and a location
  identifier.

  In SMT terms, we assign each field to a variable.
  For instance, we assign the conditional access id of task A, say 0, to a
  conditional-access-id variable, say $acc$T1, and the code generated becomes
  $acc$T1 = 0 to encode that task A's CondAccess.t id is 0.

  condition /\
  cond-acc id /\
  assign index 0 /\
  ...
  assign index n
  *)
  type t = { id : int; condition : bexp; access : Access.t }

  let to_string (a : t) : string =
    "{ access_id = " ^ string_of_int a.id ^ " condition = "
    ^ Exp.b_to_string a.condition
    ^ " access = " ^ Access.to_string a.access ^ " }"

  (* Given a task generator serialize a conditional access *)
  let to_bexp ?(assign_index = true) (t : Task.t) (a : t) : bexp =
    Gen.assign_access_id t a.id
    :: a.condition
    :: (* assign the pre-condition of the access *)
       Gen.assign_mode t a.access.mode
    :: (* assign the write-value signature (benign-write detection) *)
       Gen.assign_value t a.access.mode
    ::
    (* assign the mode *)
    (if assign_index then
       (* assign the values of the index *)
       List.mapi (Gen.assign_index N_rel.Eq t) a.access.index
     else [] (* otherwise do not generate *))
    |> b_and_ex

  (* When we lower the representation, we do not want to have source code
    locations, just an id. *)

  let from_cond_access (locals : Variable.Set.t) (t : Task.t) (idx : int)
      (ca : CondAccess.t) : t =
    let ca = project_access locals t ca in
    { id = idx; access = ca.access; condition = ca.cond }
end

let cond_access_to_bexp (locals : Variable.Set.t) (t : Task.t)
    (a : CondAccess.t) : bexp =
  let a = project_access locals t a in
  a.cond :: List.mapi (Gen.assign_index N_rel.Eq t) a.access.index |> b_and_ex

module AccessSummary = struct
  type t = {
    access : Access.t;
    condition : bexp;
    variables : Variable.Set.t;
    globals : Variable.Set.t;
    data_approx : Variable.Set.t;
    control_approx : Variable.Set.t;
  }

  let to_string (a : t) : string =
    "{access=" ^ Access.to_string a.access ^ ", variables=["
    ^ Variable.set_to_string a.variables
    ^ "], condition=" ^ b_to_string a.condition ^ ", data=["
    ^ Variable.set_to_string a.data_approx
    ^ "], ctrl=["
    ^ Variable.set_to_string a.control_approx
    ^ "], globals=["
    ^ Variable.set_to_string a.globals
    ^ "]}"
end

module Proof = struct
  type t = {
    id : int;
    kernel_name : string;
    array_name : string;
    preds : Predicates.t list;
    decls : string list;
    labels : (string * string) list;
    goal : bexp;
    accesses : AccessSummary.t list;
  }

  let add_goal (b : bexp) (p : t) : t = { p with goal = b_and p.goal b }

  let add_rel_index (o : N_rel.t) (idx : int list) (p : t) : t =
    let idx_eq =
      idx
      |> List.mapi (fun i v -> Gen.assign_index o Task1 i (Num v))
      |> b_and_ex
    in
    add_goal idx_eq p

  let assign_dim3 t ~tid ~bid : bexp =
    let gen_dim3 (x, y, z) (idx : Dim3.t) : bexp =
      b_and_ex
        [
          n_eq (Var (Gen.project t x)) (Num idx.x);
          n_eq (Var (Gen.project t y)) (Num idx.y);
          n_eq (Var (Gen.project t z)) (Num idx.z);
        ]
    in
    let tid : bexp =
      tid
      |> Option.map (gen_dim3 (Variable.tid_x, Variable.tid_y, Variable.tid_z))
      |> Option.value ~default:(Bool true)
    in
    let bid : bexp =
      bid
      |> Option.map (gen_dim3 (Variable.bid_x, Variable.bid_y, Variable.bid_z))
      |> Option.value ~default:(Bool true)
    in
    b_and bid tid

  let add ~tid ~bid : t -> t =
    add_goal (b_or (assign_dim3 Task1 ~tid ~bid) (assign_dim3 Task2 ~tid ~bid))

  let labels (p : t) : (string * string) list = p.labels

  let to_json (p : t) : Yojson.Basic.t =
    `Assoc
      [
        ("id", `Int p.id);
        ("kernel_name", `String p.kernel_name);
        ("array_name", `String p.array_name);
      ]

  let get ~access_id (p : t) : AccessSummary.t = List.nth p.accesses access_id

  (* Union of free variables across this fragment's access summaries.
     Each summary's [variables] covers the access expression, its path
     condition (which after [Phasesplit] inlines the kernel-wide pre)
     and the loop range conditions in scope. The result is the set of
     kernel-level variables this fragment's race query depends on. *)
  let free_names (p : t) : Variable.Set.t =
    List.fold_left
      (fun acc (a : AccessSummary.t) -> Variable.Set.union acc a.variables)
      Variable.Set.empty p.accesses

  let make :
      kernel_name:string ->
      array_name:string ->
      id:int ->
      accesses:AccessSummary.t list ->
      goal:bexp ->
      t =
   fun ~kernel_name ~array_name ~id ~accesses ~goal ->
    let goal =
      Constfold.b_opt goal
      (* Optimize the output expression *)
    in
    let fns =
      Exp.b_free_names goal Variable.Set.empty |> Variable.Set.elements
    in
    let decls = List.map Variable.name fns in
    let labels =
      List.filter_map
        (fun x ->
          Variable.label_opt x |> Option.map (fun l -> (Variable.name x, l)))
        fns
    in
    let preds = Predicates.get_predicates goal in
    { id; preds; decls; goal; array_name; kernel_name; labels; accesses }

  let to_s (p : t) : Indent.t list =
    let open Indent in
    let preds =
      let open Predicates in
      List.map (fun x -> x.name) p.preds |> String.concat ", "
    in
    [
      Line ("id: " ^ string_of_int p.id);
      Line ("array: " ^ p.array_name);
      Line ("kernel: " ^ p.kernel_name);
      Line ("predicates: " ^ preds ^ ";");
      Line ("decls: " ^ (p.decls |> String.concat ", ") ^ ";");
      Line
        ("accesses: "
        ^ (List.map AccessSummary.to_string p.accesses |> String.concat ", "));
      Line "goal:";
      Block (b_to_s p.goal);
      Line ";";
    ]

  let to_string (p : t) : string = to_s p |> Indent.to_string

  let from_code ?(assign_index = true) (arch : Architecture.t)
      (locals : Variable.Set.t) (runtime : bexp) (code : Flatacc.Code.t) : bexp
      =
    (*
      tid = access_1 \/
      tid = access_2 \/
      ...
      tid = access_n
    *)
    let assign_accesses (t : Task.t) : bexp =
      code |> Flatacc.Code.to_list (* get conditional accesses *)
      |> List.map (Flatacc.CondAccess.add_cond runtime)
      |> List.mapi (SymAccess.from_cond_access locals t)
         (* get symbolic access *)
      |> List.map (SymAccess.to_bexp ~assign_index t) (* generate code *)
      |> b_or_ex
    in
    b_and_ex
      [
        (* Assign the accesses of task 1 *)
        assign_accesses Task1;
        (* Assign the accesses of task 2 *)
        assign_accesses Task2;
        (* There is no need to try out all combinations of ids,
         so this contrain ensures that Task1 is never a larger access than
         Task2. *)
        n_le (Gen.access_id Task1) (Gen.access_id Task2);
        (*
        All indices of task1 are equal to the indices of task2
      *)
        Code.dim code |> Option.get |> Gen.assign_dim;
        (* mode spec *)
        Gen.mode_spec arch;
        (* Exclude benign data-races: two writes of the same
           statically-known value to the same location are not a
           harmful conflict. This is the symbolic counterpart of the
           [Write (Some x), Write (Some y) -> x <> y] case of
           [Access.Mode.can_conflict]. *)
        b_not
          (b_and_ex
             [
               n_eq (Gen.wknown Task1) (Num 1);
               n_eq (Gen.wknown Task2) (Num 1);
               n_eq (Gen.wval Task1) (Gen.wval Task2);
             ]);
      ]

  (* Co-reachability variant of [from_code]: builds the two-thread
     existential without the conflict ([mode_spec]) or same-address
     ([assign_dim]) constraints. The result asserts
     [pre ∧ runtime ∧ (∃T1.cond_i) ∧ (∃T2.cond_j) ∧ T1 ≠ T2]
     — i.e. two distinct threads each reach some access in the
     fragment, but they need not collide on the same word and need
     not have a conflicting mode pair. SAT means the kernel's
     two-thread reachability for this fragment is non-empty; UNSAT
     means even the underlying co-existence has been trivialised
     (e.g. [blockDim.x == 1] killed the second thread).

     The [Race] variant — full conflict goal — is what the DRF
     solver checks. The [Coreach] variant — this one — is what the
     genie gate checks to detect trivialising clearances.

     Built directly here (rather than as a post-hoc strip on the
     race goal) because the conjunction's structure isn't easily
     reversible: [b_and_ex] folds left-associative, so peeling the
     last two conjuncts off the race form would require pattern-
     matching on the optimised [bexp] shape. *)
  let from_code_coreach (_arch : Architecture.t) (locals : Variable.Set.t)
      (runtime : bexp) (code : Flatacc.Code.t) : bexp =
    let assign_accesses (t : Task.t) : bexp =
      code |> Flatacc.Code.to_list
      |> List.map (Flatacc.CondAccess.add_cond runtime)
      |> List.mapi (SymAccess.from_cond_access locals t)
      |> List.map (SymAccess.to_bexp ~assign_index:false t)
      |> b_or_ex
    in
    (* No explicit [thread_distinct] term: [Kernel.apply_arch]
       folds the arch's [distinct] clause into [k.pre] upstream,
       which [Phasesplit] then wraps as [Cond (pre, u)] over the
       kernel's unsynced code. Each [Flatacc.CondAccess.cond]
       therefore already carries [thread_distinct], and
       [SymAccess.from_cond_access] expands its [ThreadUnif]
       primitives per task — so [assign_accesses Task1] and
       [assign_accesses Task2] each carry a properly-projected
       distinct constraint without an additional explicit term. *)
    b_and_ex
      [
        assign_accesses Task1;
        assign_accesses Task2;
        n_le (Gen.access_id Task1) (Gen.access_id Task2);
      ]

  let from_flat_coreach (arch : Architecture.t) (proof_id : int)
      (k : Flatacc.Kernel.t) : t =
    let locals =
      Variable.Set.union k.exact_local_variables k.approx_local_variables
    in
    let goal =
      from_code_coreach arch locals k.runtime k.code
      |> b_and (project_pre locals k.pre)
      |> Predicates.strip_cross_thread
    in
    let pre_fns = Exp.b_free_names k.pre Variable.Set.empty in
    let accesses =
      List.map
        (fun (a : CondAccess.t) ->
          let open AccessSummary in
          let cond_fns = Exp.b_free_names a.cond Variable.Set.empty in
          let data_fns = Access.free_names a.access Variable.Set.empty in
          let ctrl_fns = Variable.Set.union pre_fns cond_fns in
          let all_fns = Variable.Set.union data_fns ctrl_fns in
          {
            access = a.access;
            condition = a.cond;
            variables = all_fns;
            globals = Variable.Set.diff all_fns locals;
            data_approx = Variable.Set.inter k.approx_local_variables data_fns;
            control_approx =
              Variable.Set.inter k.approx_local_variables ctrl_fns;
          })
        k.code
    in
    make ~id:proof_id ~kernel_name:k.name ~array_name:k.array_name ~goal
      ~accesses

  (* Single-thread variant of [from_code_coreach]: builds the
     one-thread existential. The result asserts
     [pre ∧ runtime ∧ (∃T1.cond_i)] — one thread reaches some access
     in the fragment. SAT means the fragment has at least one
     reachable access under the precondition; UNSAT means the
     precondition killed every access's reachability.

     This is the Tier 1 pre-filter shape for the pair-level gate.
     SAT of the two-thread co-reach goal implies SAT of this T1-only
     goal (T1's conjunct is a subset of the co-reach conjunction),
     so a baseline pair preserved by Φ at Tier 2 is also preserved
     here; equivalently, UNSAT here implies UNSAT at Tier 2 — i.e.
     the pre-filter is sound. *)
  let from_code_t1 (_arch : Architecture.t) (locals : Variable.Set.t)
      (runtime : bexp) (code : Flatacc.Code.t) : bexp =
    let assign_accesses (t : Task.t) : bexp =
      code |> Flatacc.Code.to_list
      |> List.map (Flatacc.CondAccess.add_cond runtime)
      |> List.mapi (SymAccess.from_cond_access locals t)
      |> List.map (SymAccess.to_bexp ~assign_index:false t)
      |> b_or_ex
    in
    assign_accesses Task1

  let from_flat_t1 (arch : Architecture.t) (proof_id : int)
      (k : Flatacc.Kernel.t) : t =
    let locals =
      Variable.Set.union k.exact_local_variables k.approx_local_variables
    in
    let goal =
      from_code_t1 arch locals k.runtime k.code
      |> b_and (project_pre locals k.pre)
      |> Predicates.strip_cross_thread
    in
    let pre_fns = Exp.b_free_names k.pre Variable.Set.empty in
    let accesses =
      List.map
        (fun (a : CondAccess.t) ->
          let open AccessSummary in
          let cond_fns = Exp.b_free_names a.cond Variable.Set.empty in
          let data_fns = Access.free_names a.access Variable.Set.empty in
          let ctrl_fns = Variable.Set.union pre_fns cond_fns in
          let all_fns = Variable.Set.union data_fns ctrl_fns in
          {
            access = a.access;
            condition = a.cond;
            variables = all_fns;
            globals = Variable.Set.diff all_fns locals;
            data_approx = Variable.Set.inter k.approx_local_variables data_fns;
            control_approx =
              Variable.Set.inter k.approx_local_variables ctrl_fns;
          })
        k.code
    in
    make ~id:proof_id ~kernel_name:k.name ~array_name:k.array_name ~goal
      ~accesses

  let from_flat ?(memory_model = Memory_model.default) ?(assign_index = true)
      (arch : Architecture.t) (proof_id : int) (k : Flatacc.Kernel.t) : t =
    let locals =
      Variable.Set.union k.exact_local_variables k.approx_local_variables
    in
    let atomic_axioms = AtomicAxioms.axioms_of k locals in
    let memory_model_axiom = MemoryModelAxioms.axiom_of memory_model in
    let goal =
      from_code ~assign_index arch locals k.runtime k.code
      |> b_and (project_pre locals k.pre)
      |> b_and atomic_axioms
      |> b_and memory_model_axiom
      |> Predicates.strip_cross_thread
    in
    let pre_fns = Exp.b_free_names k.pre Variable.Set.empty in
    let accesses =
      List.map
        (fun (a : CondAccess.t) ->
          let open AccessSummary in
          let cond_fns = Exp.b_free_names a.cond Variable.Set.empty in
          let data_fns = Access.free_names a.access Variable.Set.empty in
          let ctrl_fns = Variable.Set.union pre_fns cond_fns in
          let all_fns = Variable.Set.union data_fns ctrl_fns in
          {
            access = a.access;
            condition = b_and k.runtime a.cond;
            variables = all_fns;
            globals = Variable.Set.diff all_fns locals;
            data_approx = Variable.Set.inter k.approx_local_variables data_fns;
            control_approx =
              Variable.Set.inter k.approx_local_variables ctrl_fns;
          })
        k.code
    in
    make ~id:proof_id ~kernel_name:k.name ~array_name:k.array_name ~goal
      ~accesses
end

let add_rel_index (o : N_rel.t) (idx : int list) (s : Proof.t Streamutil.stream)
    : Proof.t Streamutil.stream =
  if idx = [] then s else Streamutil.map (Proof.add_rel_index o idx) s

let add ~tid ~bid : Proof.t Streamutil.stream -> Proof.t Streamutil.stream =
  Streamutil.map (Proof.add ~tid ~bid)

let translate ?(memory_model = Memory_model.default) (arch : Architecture.t)
    (stream : Flatacc.Kernel.t Streamutil.stream) : Proof.t Streamutil.stream =
  Streamutil.mapi (Proof.from_flat ~memory_model arch) stream

let sanity_check (arch : Architecture.t)
    (stream : Flatacc.Kernel.t Streamutil.stream) : Proof.t Streamutil.stream =
  Streamutil.mapi (Proof.from_flat ~assign_index:false arch) stream

let translate_coreach (arch : Architecture.t)
    (stream : Flatacc.Kernel.t Streamutil.stream) : Proof.t Streamutil.stream =
  Streamutil.mapi (Proof.from_flat_coreach arch) stream

let translate_t1 (arch : Architecture.t)
    (stream : Flatacc.Kernel.t Streamutil.stream) : Proof.t Streamutil.stream =
  Streamutil.mapi (Proof.from_flat_t1 arch) stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : Proof.t Streamutil.stream) : unit =
  print_endline "; symbexp";
  Streamutil.iter
    (fun (p : Proof.t) ->
      print_endline "; proof";
      Proof.to_string p |> print_endline)
    ks;
  print_endline "; end of symbexp"
