open Protocols
open Stage0

(* The properties this analysis can verify. All three share the same
   syntax-directed traversal in [Check.Barrier.of_code]; they differ in
   the obligation's frame (paired vs unary), the initial partition of
   architectural variables, and the goal precondition.

   - [Well_sync] (paired). Same thread, same launch, two executions T1/T2.
     Catches barriers whose reachability depends on thread-private state
     that may differ between executions (uninitialised locals, input-
     derived data). Projectable: user locals. All of arch is shared.

   - [Barrier_div] (paired). Two distinct threads in the same group at
     the same barrier. Catches the GPUVerify litmus pattern where threads
     of a warp disagree on a guard wrapping a barrier. Projectable: tid
     plus user locals. bid/bdim/gdim shared. tid$T1 != tid$T2 added as a
     precondition.

   - [Missing_participants] (unary). A single thread that fails to reach
     the barrier. Catches deadlocks at block-wide barriers when the cohort
     is partial — and, unlike [Barrier_div], also when the cohort is
     uniformly empty (no thread arrives, both peer-frames agree on
     missing). The obligation has no T1/T2 split: it asks
     [SAT(pre ∧ U ∧ ¬D)] over a single tid in the valid block range. *)
module Property = struct
  type t = Well_sync | Barrier_div | Missing_participants

  let to_string : t -> string = function
    | Well_sync -> "well-sync"
    | Barrier_div -> "barrier-div"
    | Missing_participants -> "missing-participants"

  (* Whether the obligation compares two thread frames (T1, T2) or
     reasons about a single one. Drives projection and witness shape. *)
  type frame = Paired | Unary

  let frame : t -> frame = function
    | Well_sync | Barrier_div -> Paired
    | Missing_participants -> Unary

  (* Architectural variables that may differ between T1 and T2 under this
     property's frame. Their guards are classified divergent and they are
     projected with $T1/$T2 suffixes. Empty for unary frames — there is
     no second frame to differ from. *)
  let arch_projectable : t -> Variable.Set.t = function
    | Well_sync -> Variable.Set.empty
    | Barrier_div -> Variable.tid_set
    | Missing_participants -> Variable.Set.empty

  (* Architectural variables guaranteed equal across T1 and T2 under this
     property's frame. Stay shared (single copy in the goal). For unary
     frames, every arch component is "shared" trivially. *)
  let arch_shared : t -> Variable.Set.t = function
    | Well_sync | Missing_participants ->
        Variable.tid_set
        |> Variable.Set.union Variable.bid_set
        |> Variable.Set.union Variable.bdim_set
        |> Variable.Set.union Variable.gdim_set
    | Barrier_div ->
        Variable.bid_set
        |> Variable.Set.union Variable.bdim_set
        |> Variable.Set.union Variable.gdim_set

  (* Extra precondition conjoined to the goal: "T1 and T2 are different
     threads of the same group" for Barrier_div; nothing for the others. *)
  let goal_precondition : t -> Exp.bexp = function
    | Well_sync | Missing_participants -> Bool true
    | Barrier_div -> Exp.thread_distinct Variable.tid_list
end

(* Path-condition state carried as we walk the kernel body.

   Two variable sets serve different roles:
   - [locals] classifies expressions as thread-divergent. An [if]/[for]
     guard is uniform iff it references no [locals]. Contains user locals
     plus decls plus divergent-loop binders, plus the property-specific
     subset of arch ([Property.arch_projectable]).
   - [projectable] names the variables that get a $T1/$T2 suffix at goal
     construction. Identical to [locals] minus binders that are shared by
     construction (loop binders), plus the property's projected arch. *)
module PathCondition = struct
  type t = {
    property : Property.t;
    locals : Variable.Set.t;
    projectable : Variable.Set.t;
    shared : Variable.Set.t;
    pre : Exp.bexp;
    divergent : Exp.bexp;
    uniform : Exp.bexp;
  }

  let make ~(property : Property.t) ~(locals : Variable.Set.t)
      ~(globals : Variable.Set.t) ~(pre : Exp.bexp) : t =
    let arch_proj = Property.arch_projectable property in
    let arch_shared = Property.arch_shared property in
    (* [locals] (as supplied by the kernel after [apply_arch_binders]) is
       tid plus user-declared locals. This set drives the U/D split: it
       happens to be exactly the right "may differ between T1 and T2"
       set for both properties — for barrier-div trivially, for
       well-sync conservatively (tid is shared across T1/T2 of the same
       thread, so tid-only guards landing in D are simply discharged
       trivially). *)
    let user_locals = Variable.Set.diff locals (Variable.Set.union arch_proj arch_shared) in
    let projectable = Variable.Set.union user_locals arch_proj in
    let shared = Variable.Set.union arch_shared globals in
    { property; locals; projectable; shared; pre;
      divergent = (Bool true : Exp.bexp);
      uniform = (Bool true : Exp.bexp) }

  (* Decl-introduced local. Classified as divergent and projectable: its
     value depends on the executing thread (and may be input-derived), so
     two same-thread executions of the same launch may disagree on it. *)
  let add_local (x : Variable.t) (c : t) : t =
    { c with
      locals = Variable.Set.add x c.locals;
      projectable = Variable.Set.add x c.projectable }

  (* Divergent-loop binder. Classified as local (its mention makes a sub-
     guard divergent, since the binder's value depends on iteration position)
     but shared between T1 and T2 — both same-thread executions traverse the
     same iteration sequence; any variation in the loop range arises from
     projectable variables in the bounds, captured separately. *)
  let add_divergent_binder (x : Variable.t) (c : t) : t =
    { c with
      locals = Variable.Set.add x c.locals;
      shared = Variable.Set.add x c.shared }

  (* Uniform-loop binder. Same value across T1 and T2 by the same argument
     as above; additionally, its mention does not make sub-guards divergent
     because the loop's range refers only to non-locals. *)
  let add_shared (x : Variable.t) (c : t) : t =
    { c with shared = Variable.Set.add x c.shared }

  let add_uniform (b : Exp.bexp) (c : t) : t =
    { c with uniform = Exp.b_and c.uniform b }

  let add_divergent (b : Exp.bexp) (c : t) : t =
    { c with divergent = Exp.b_and c.divergent b }

  let is_uniform (c : t) (b : Exp.bexp) : bool =
    let fns = Exp.b_free_names b Variable.Set.empty in
    Variable.Set.inter fns c.locals |> Variable.Set.is_empty

  let add_cond (b : Exp.bexp) (c : t) : t * t =
    if is_uniform c b then
      (add_uniform b c, add_uniform (Exp.b_not b) c)
    else
      (add_divergent b c, add_divergent (Exp.b_not b) c)

  let to_string (e : t) : string =
    Printf.sprintf
      "{property = %s; locals = {%s}; projectable = {%s}; shared = {%s}; pre = %s; divergent = %s; uniform = %s}"
      (Property.to_string e.property)
      (Variable.set_to_string e.locals)
      (Variable.set_to_string e.projectable)
      (Variable.set_to_string e.shared)
      (Exp.b_to_string e.pre)
      (Exp.b_to_string e.divergent)
      (Exp.b_to_string e.uniform)
end

module Check = struct
  module Barrier = struct
    type t = { sync : Sync.t; path_condition : PathCondition.t }

    let rec of_code (p : PathCondition.t) : Protocols.Code.t -> t Seq.t = function
      | Skip | Access _ -> Seq.empty
      | Sync sync -> Seq.return { sync; path_condition=p }
      | Seq (s1, s2) ->
        Seq.append (of_code p s1) (of_code p s2)
      | Decl { var; body; _ } -> of_code (PathCondition.add_local var p) body
      | If (b, s1, s2) ->
          let p_then, p_else = PathCondition.add_cond b p in
          Seq.append (of_code p_then s1) (of_code p_else s2)
      | Loop { range; body } ->
          let cond = Range.to_cond range in
          let p =
            if PathCondition.is_uniform p cond then
              p
              |> PathCondition.add_shared range.var
              |> PathCondition.add_uniform cond
            else
              p
              |> PathCondition.add_divergent_binder range.var
              |> PathCondition.add_divergent cond
          in
          of_code p body

    let to_string (b : t) : string =
      Printf.sprintf "%s %s"
        (Sync.to_string b.sync)
        (PathCondition.to_string b.path_condition)
  end
  type t = {
    property : Property.t;
    kernel_name: string;
    barriers: Barrier.t Seq.t;
  }

  let of_kernel ~(property : Property.t) (k : Protocols.Kernel.t) : t =
    (* The arch binders (blockDim/gridDim into globals, threadIdx into
       locals) and the [base] precondition (bounds, positivity,
       dim >= 1) must be applied by the caller before [inline_globals]
       runs — otherwise pinned launch dimensions don't get substituted
       in [pre] and the analysis sees them as free. See [check.ml]
       [preprocess]. *)
    let locals = Params.to_set k.local_variables in
    let globals = Params.to_set k.global_variables in
    let p = PathCondition.make ~property ~locals ~globals ~pre:k.pre in
    let barriers = Barrier.of_code p k.code in
    { property; barriers; kernel_name = k.name }

  let to_string (e : t) : string =
    let barriers_str =
      e.barriers
      |> Seq.map Barrier.to_string
      |> List.of_seq
      |> String.concat "\n  "
    in
    Printf.sprintf "kernel %s [%s]:\n  %s"
      e.kernel_name (Property.to_string e.property) barriers_str

  let print (c : t) : unit = to_string c |> print_endline
end

module Proj = struct
  type task = T1 | T2

  let task_to_string : task -> string = function T1 -> "T1" | T2 -> "T2"
  let other : task -> task = function T1 -> T2 | T2 -> T1

  let project (t : task) (x : Variable.t) : Variable.t =
    Variable.update_name (fun n -> n ^ "$" ^ task_to_string t) x

  let rec nexp (locals : Variable.Set.t) (t : task) (n : Exp.nexp) : Exp.nexp =
    let open Exp in
    match n with
    | Num _ -> n
    | CastInt e -> CastInt (bexp locals t e)
    | Var x when Variable.Set.mem x locals -> Var (project t x)
    | Var _ -> n
    | Unary (o, e) -> Unary (o, nexp locals t e)
    | Other e -> nexp locals (other t) e
    | Binary (o, n1, n2) -> Binary (o, nexp locals t n1, nexp locals t n2)
    | NIf (b, n1, n2) ->
        NIf (bexp locals t b, nexp locals t n1, nexp locals t n2)
    | NCall (x, n) -> NCall (x, nexp locals t n)

  and bexp (locals : Variable.Set.t) (t : task) (b : Exp.bexp) : Exp.bexp =
    let open Exp in
    match b with
    | Bool _ -> b
    | CastBool e -> CastBool (nexp locals t e)
    | Pred (x, ns) -> Pred (x, List.map (nexp locals t) ns)
    | BNot b -> BNot (bexp locals t b)
    | BRel (o, b1, b2) -> BRel (o, bexp locals t b1, bexp locals t b2)
    | NRel (o, n1, n2) -> NRel (o, nexp locals t n1, nexp locals t n2)
    | Distinct es -> Distinct (List.map (nexp locals t) es)
end

(* Stage 2: lower a Check into a Proof.t carrying a concrete bexp goal.

   A Proof mirrors drf/lib/symbexp.ml:Proof — same preds / decls / labels
   boilerplate via Proof.make, same UNSAT-is-safe convention on the goal. *)
module Proof = struct
  type t = {
    property : Property.t;
    id : int;
    kernel_name : string;
    barrier : Sync.t;
    preds : Predicates.t list;
    decls : string list;
    labels : (string * string) list;
    goal : Exp.bexp;
  }

  let make ~(property : Property.t) ~(kernel_name : string)
      ~(barrier : Sync.t) ~(id : int) ~(goal : Exp.bexp) : t =
    let goal = Constfold.b_opt goal in
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
    { property; id; preds; decls; goal; kernel_name; labels; barrier }

  let to_s (p : t) : Indent.t list =
    let open Indent in
    let preds_str =
      let open Predicates in
      List.map (fun x -> x.pred_name) p.preds |> String.concat ", "
    in
    let loc_str =
      match p.barrier.loc with
      | Some l -> Location.to_string l
      | None -> "<none>"
    in
    [
      Line ("id: " ^ string_of_int p.id);
      Line ("barrier: " ^ Sync.to_string p.barrier);
      Line ("location: " ^ loc_str);
      Line ("kernel: " ^ p.kernel_name);
      Line ("predicates: " ^ preds_str ^ ";");
      Line ("decls: " ^ (p.decls |> String.concat ", ") ^ ";");
      Line "goal:";
      Block (Exp.b_to_s p.goal);
      Line ";";
    ]

  let to_string (p : t) : string = to_s p |> Indent.to_string

  let print (p : t) : unit = to_string p |> print_endline

  let print_seq (s : t Seq.t) : unit = Seq.iter print s

  (* Goal construction. The shape depends on the property's frame:

     - [Paired] (well-sync, barrier-div): peer-frame disagreement over T1/T2.
       pre(T1) ∧ pre(T2) ∧ U(T1) ∧ U(T2) ∧ D(T1) ∧ ¬D(T2) ∧ extra.
       Variables in [c.projectable] are renamed with $T1/$T2 suffixes;
       everything in [c.shared] stays a single copy.

     - [Unary] (missing-participants): single-frame existence of a thread
       that fails to reach the barrier.
       pre ∧ U ∧ ¬D.
       No projection — there is no second frame, so [c.projectable]
       collapses into the same set of free variables and stays bare. *)
  let path_condition_to_goal (c : PathCondition.t) : Exp.bexp =
    (* Sanity: every free name must be either projectable or architectural.
       A stray free var means the protocol inference left something unbound
       — that's a bug upstream, not something for us to work around. *)
    let free =
      Variable.Set.empty
      |> Exp.b_free_names c.pre
      |> Exp.b_free_names c.divergent
      |> Exp.b_free_names c.uniform
    in
    let accounted = Variable.Set.union c.projectable c.shared in
    let stray = Variable.Set.diff free accounted in
    if not (Variable.Set.is_empty stray) then
      prerr_endline
        ("barrier_div: unaccounted free variables in path condition: "
         ^ Variable.set_to_string stray);
    match Property.frame c.property with
    | Paired ->
        let proj t b = Proj.bexp c.projectable t b in
        let pre1 = proj T1 c.pre in
        let pre2 = proj T2 c.pre in
        let d1 = proj T1 c.divergent in
        let d2 = proj T2 c.divergent in
        let u1 = proj T1 c.uniform in
        let u2 = proj T2 c.uniform in
        (* For barrier-div, project tid in the distinctness precondition
           too: the constraint is tid$T1 != tid$T2. *)
        let extra = proj T2 (Property.goal_precondition c.property) in
        Exp.b_and_ex [ pre1; pre2; u1; u2; d1; Exp.b_not d2; extra ]
    | Unary ->
        Exp.b_and_ex
          [ c.pre; c.uniform; Exp.b_not c.divergent;
            Property.goal_precondition c.property ]

  let of_check (c : Check.t) : t Seq.t =
    c.barriers
    |> Seq.mapi (fun id (b : Check.Barrier.t) ->
           make ~property:c.property ~kernel_name:c.kernel_name
             ~barrier:b.sync ~id
             ~goal:(path_condition_to_goal b.path_condition))

  let solve ?(solver = (module Gen_z3.Bv64Gen : Gen_z3.Z3_SOLVER)) ?timeout
      (p : t) : (Gen_z3.Solver.t, string) Result.t =
    let module S = (val solver) in
    S.solve ?timeout (Predicates.b_inline p.goal)

  module Witness = struct
    (* Two witness shapes, mirroring the two obligation frames:

       - [Paired]: a counter-model has T1/T2 columns of locals plus a
         shared globals column.
       - [Unary]: a counter-model is a single assignment of locals and
         globals — the failing thread's view. *)
    type t =
      | Paired of {
          t1_locals : (string * string) list;
          t2_locals : (string * string) list;
          globals : (string * string) list;
        }
      | Unary of {
          locals : (string * string) list;
          globals : (string * string) list;
        }

    let strip_suffix (suffix : string) (s : string) : string option =
      let n = String.length s in
      let m = String.length suffix in
      if n >= m && String.sub s (n - m) m = suffix then
        Some (String.sub s 0 (n - m))
      else None

    let parse (frame : Property.frame) (m : Z3.Model.model) : t =
      let open Z3 in
      let vars =
        Model.get_const_decls m
        |> List.map (fun d ->
            let name = FuncDecl.get_name d |> Symbol.get_string in
            let value =
              FuncDecl.apply d []
              |> (fun e -> Model.eval m e true)
              |> Option.map Expr.to_string
              |> Option.value ~default:"?"
              |> Gen_z3.Bv64Gen.parse_num
            in
            (name, value))
      in
      let sort = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) in
      match frame with
      | Property.Paired ->
          let t1_locals, rest =
            List.partition_map
              (fun (k, v) ->
                match strip_suffix "$T1" k with
                | Some k' -> Left (k', v)
                | None -> Right (k, v))
              vars
          in
          let t2_locals, globals =
            List.partition_map
              (fun (k, v) ->
                match strip_suffix "$T2" k with
                | Some k' -> Left (k', v)
                | None -> Right (k, v))
              rest
          in
          Paired
            { t1_locals = sort t1_locals; t2_locals = sort t2_locals;
              globals = sort globals }
      | Property.Unary ->
          (* Split between thread-local and uniform variables: any name
             that is a known thread-id component goes into [locals];
             everything else into [globals]. *)
          let is_local (k, _) =
            List.mem k [ "threadIdx.x"; "threadIdx.y"; "threadIdx.z" ]
          in
          let locals, globals = List.partition is_local vars in
          Unary { locals = sort locals; globals = sort globals }
  end
end
