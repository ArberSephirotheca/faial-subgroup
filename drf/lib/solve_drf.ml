open Stage0
open Protocols
open Gen_z3
open Exp
module Solver = Z3.Solver
module Expr = Z3.Expr
module Boolean = Z3.Boolean
module Arithmetic = Z3.Arithmetic
module Integer = Z3.Arithmetic.Integer
module Model = Z3.Model
module Symbol = Z3.Symbol
module FuncDecl = Z3.FuncDecl
module BitVector = Z3.BitVector
module StringMap = Common.StringMap

let gc_alloc_threshold : int64 =
  let mb =
    match Option.bind (Sys.getenv_opt "FAIAL_GC_MB") int_of_string_opt with
    | Some n -> n
    | None -> Defaults.gc_mb
  in
  Int64.mul (Int64.of_int mb) 1_000_000L

type json = Yojson.Basic.t

module Environ = struct
  open Common

  type t = { labels : string StringMap.t; variables : (string * string) list }

  let filter_variables (vars : Variable.Set.t) (env : t) : t =
    {
      env with
      variables =
        List.filter
          (fun (n, _) ->
            let n = Variable.from_name n in
            Variable.Set.mem n vars)
          env.variables;
    }

  let to_json (env : t) : json =
    `Assoc (List.map (fun (k, v) -> (k, `String v)) env.variables)

  let to_string (e : t) : string =
    let open Yojson.Basic in
    e |> to_json |> pretty_to_string

  let get (x : string) (e : t) : string option = List.assoc_opt x e.variables
  let label (x : string) (e : t) : string option = StringMap.find_opt x e.labels
  let variables (e : t) : (string * string) list = e.variables

  let labels (e : t) : (string * string) list =
    e.variables
    |> List.map (fun (k, v) -> (e |> label k |> Option.value ~default:k, v))

  let remove_structs (e : t) : t =
    {
      e with
      variables =
        e.variables
        |> List.filter (fun (k, _) -> String.index_opt k '.' |> Option.is_none);
    }

  let parse_structs (e : t) : string StringMap.t StringMap.t =
    e.variables
    |> List.fold_left
         (fun accum (k, v) ->
           match Common.split '.' k with
           | Some (id, field) ->
               StringMap.update id
                 (function
                   | None -> Some (StringMap.singleton field v)
                   | Some values -> Some (StringMap.add field v values))
                 accum
           | None -> accum)
         StringMap.empty

  let parse (labels : (string * string) list) (parse_num : string -> string)
      (m : Model.model) : t =
    (* [Model.get_const_decls] is not contractually ordered. Sort by
       key so JSON serialisation (and any downstream consumer that
       preserves list order) is stable across Z3 model emissions. *)
    let variables =
      Model.get_const_decls m
      |> List.map (fun d ->
          let key : string = FuncDecl.get_name d |> Symbol.get_string in
          let e : string =
            FuncDecl.apply d []
            |> (fun e -> Model.eval m e true)
            |> Option.map Expr.to_string |> Option.value ~default:"?"
          in
          (key, parse_num e))
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in
    let labels = StringMap.of_list labels in
    { labels; variables }
end

module Vec3 = struct
  type t = { x : string; y : string; z : string }

  let make ~x ~y ~z : t = { x; y; z }
  let default : t = { x = "?"; y = "?"; z = "?" }

  let to_assoc (v : t) : (string * string) list =
    [ ("x", v.x); ("y", v.y); ("z", v.z) ]

  let to_json (v : t) : json =
    `Assoc [ ("x", `String v.x); ("y", `String v.y); ("z", `String v.z) ]

  let to_string (v : t) =
    let open Yojson.Basic in
    to_json v |> pretty_to_string

  let parse (kvs : Environ.t) : t * t =
    let parse_vec (suffix : string) : t =
      let parse (x : string) : string =
        kvs
        |> Environ.get ("threadIdx." ^ x ^ "$T" ^ suffix)
        |> Option.value ~default:"0"
      in
      { x = parse "x"; y = parse "y"; z = parse "z" }
    in
    (parse_vec "1", parse_vec "2")

  let from_dim3 (d : Dim3.t) : t =
    let x = string_of_int d.x in
    let y = string_of_int d.y in
    let z = string_of_int d.z in
    make ~x ~y ~z
end

module TaskState = struct
  type t = { locals : Environ.t; access : Access.t }

  let can_conflict (x1 : t) (x2 : t) : bool =
    Access.can_conflict x1.access x2.access

  let filter_variables (vars : Variable.Set.t) (e : t) : t =
    { e with locals = Environ.filter_variables vars e.locals }

  let to_json (x : t) : json =
    `Assoc
      [
        ("locals", Environ.to_json x.locals);
        ("mode", `String (Access.Mode.to_string x.access.mode));
        ("location", Access.location x.access |> Location.to_json);
      ]

  let to_string (v : t) : string =
    let open Yojson.Basic in
    to_json v |> pretty_to_string
end

module Witness = struct
  type t = {
    proof_id : int;
    array_name : string;
    indices : string list;
    data_approx : Variable.Set.t;
    control_approx : Variable.Set.t;
    tasks : TaskState.t * TaskState.t;
    globals : Environ.t;
  }

  let filter_variables (vars : Variable.Set.t) (w : t) : t =
    let t1, t2 = w.tasks in
    {
      w with
      tasks =
        (TaskState.filter_variables vars t1, TaskState.filter_variables vars t2);
      globals = Environ.filter_variables vars w.globals;
    }

  let can_conflict (x : t) : bool =
    let t1, t2 = x.tasks in
    TaskState.can_conflict t1 t2

  let to_json (x : t) : json =
    let t1, t2 = x.tasks in
    `Assoc
      [
        ("task1", TaskState.to_json t1);
        ("task2", TaskState.to_json t2);
        ("indices", `List (List.map (fun x -> `String x) x.indices));
        ("globals", Environ.to_json x.globals);
      ]

  let to_string (v : t) : string =
    let open Yojson.Basic in
    to_json v |> pretty_to_string

  let parse_vec3 (d : Vec3.t) (prefix : string) (g : Environ.t) :
      Environ.t * Vec3.t =
    let env, globals =
      List.partition
        (fun (k, _) -> String.starts_with ~prefix:(prefix ^ ".") k)
        g.variables
    in
    let get ~default (x : string) : string =
      let z = List.assoc_opt (prefix ^ "." ^ x) env in
      Option.value z ~default
    in
    let v =
      Vec3.
        {
          x = get ~default:d.x "x";
          y = get ~default:d.y "y";
          z = get ~default:d.z "z";
        }
    in
    ({ g with variables = globals }, v)

  let parse_indices (e : Environ.t) : string list =
    let kvs = e.variables in
    (*
    $T1$idx$0: 1
    $T2$idx$0: 1
    *)
    (* get the maximum integer, in this case 0 *)
    let biggest_idx =
      List.split kvs |> fst
      |> List.filter (fun k -> Common.contains ~substring:"$idx$" k)
      |> List.map (fun k ->
          match Common.rsplit '$' k with
          | Some (_, idx) -> int_of_string idx
          | None -> failwith "unexpected")
      |> List.fold_left Int.max 0
    in
    (* Parse a single index, in this case 1 *)
    let parse_idx (idx : int) : string =
      let parse (tid : Task.t) : string option =
        List.assoc_opt (Symbexp.Ids.index tid idx) kvs
      in
      match parse Task1 with
      | Some v -> v
      | None -> (
          match parse Task2 with
          | Some v -> v
          | None -> failwith "Index malformed!")
    in
    (* Range over all indices *)
    Common.range biggest_idx
    (* And look them up using parse_idx *)
    |> List.map parse_idx

  let parse_meta (e : Environ.t) : Environ.t * string list =
    let kvs, env =
      List.partition
        (fun (k, _) -> String.starts_with ~prefix:"$" k)
        e.variables
    in
    ({ e with variables = env }, parse_indices { e with variables = kvs })

  let parse (parse_num : string -> string) ~proof (m : Model.model) : t =
    let env =
      let open Symbexp in
      Environ.parse (Proof.labels proof) parse_num m
    in
    let inst1, inst2 =
      let parse_inst_id (tid : Task.t) : int =
        env
        |> Environ.get (Symbexp.Ids.access_id tid)
        |> Option.get |> int_of_string
      in
      (parse_inst_id Task1, parse_inst_id Task2)
    in
    let a1 = Symbexp.Proof.get ~access_id:inst1 proof in
    let a2 = Symbexp.Proof.get ~access_id:inst2 proof in
    let all_vars = Variable.Set.union a1.variables a2.variables in
    (* put all special variables in kvs
      $T2$loc: 0
      $T1$mode: 0
      $T1$loc: 1
      $T2$mode: 1
      $T1$idx$0: 1
      $T2$idx$0: 1
    *)
    let env, idx = parse_meta env in
    let locals, globals =
      List.partition (fun (k, _) -> String.contains k '$') env.variables
    in
    let t1_locals, t2_locals =
      List.partition (fun (k, _) -> String.ends_with ~suffix:"$T1" k) locals
    in
    let globals = { env with variables = globals } in
    let labels_of suffix =
      StringMap.filter (fun x _ -> String.ends_with ~suffix x) globals.labels
    in
    let fix_var (x : string) : string =
      match Common.rsplit '$' x with Some (x, _) -> x | None -> x
    in
    let fix_labels (env : string StringMap.t) : string StringMap.t =
      StringMap.fold
        (fun (k : string) (v : string) (m : string StringMap.t) ->
          StringMap.add (fix_var k) v m)
        env StringMap.empty
    in
    let fix_locals : (string * string) list -> (string * string) list =
      List.map (fun (k, v) -> (fix_var k, v))
    in
    let t1_locals = fix_locals t1_locals in
    let t2_locals = fix_locals t2_locals in
    let t1_labels = fix_labels (labels_of "$T1") in
    let t2_labels = fix_labels (labels_of "$T2") in
    let t1 =
      TaskState.
        {
          locals = { variables = t1_locals; labels = t1_labels };
          access = a1.access;
        }
    in
    let t2 =
      TaskState.
        {
          locals = { variables = t2_locals; labels = t2_labels };
          access = a2.access;
        }
    in
    {
      proof_id = proof.id;
      array_name = proof.array_name;
      indices = idx;
      tasks = (t1, t2);
      globals;
      data_approx = Variable.Set.union a1.data_approx a2.data_approx;
      control_approx = Variable.Set.union a1.control_approx a2.control_approx;
    }
    (* Make sure that we only show the variables that we have defined *)
    |> filter_variables all_vars
end

(* Bexp-to-Z3 encoder pair: which [Gen_z3] codegen module to use
   ([IntGen] or [Bv64Gen]) plus the Z3 logic string (if any) to
   request when creating the solver. Lives at the top level so
   both the per-proof race solver and the pre-flight
   [check_bexp_sat] share one selection / fallback rule. *)
module Encoder = struct
  type t = {
    b_to_expr : Z3.context -> Exp.bexp -> Z3.Expr.expr;
    parse_num : string -> string;
    logic     : string option;
  }

  let intgen ~(logic : string option) : t =
    { b_to_expr = IntGen.b_to_expr;
      parse_num = IntGen.parse_num;
      logic }

  let bv64 () : t =
    { b_to_expr = Bv64Gen.b_to_expr;
      parse_num = Bv64Gen.parse_num;
      logic     = None }

  (* Starting encoder. Respect a user-requested BV logic; otherwise
     default to the arithmetic encoder. *)
  let initial ~(logic : string option) : t =
    match logic with
    | Some l when String.ends_with ~suffix:"BV" l -> bv64 ()
    | _ -> intgen ~logic

  let mk_solver (enc : t) (ctx : Z3.context) : Solver.solver =
    match enc.logic with
    | None -> Solver.mk_simple_solver ctx
    | Some l -> Solver.mk_solver_s ctx l
end

module Outcome = struct
  type t =
    | Drf
    | Drf_with_core of int list
    | Racy of Witness.t
    | Unknown

  let is_safe : t -> bool = function
    | Drf | Drf_with_core _ -> true
    | _ -> false

  let to_json : t -> json = function
    | Drf -> `String "drf"
    | Drf_with_core c ->
      `Assoc
        [ ("drf", `Bool true);
          ("core", `List (List.map (fun i -> `Int i) c)) ]
    | Unknown -> `String "unknown"
    | Racy w -> Witness.to_json w
end

module Solution = struct
  type t = {
    proof : Symbexp.Proof.t;
    outcome : Outcome.t;
    logic : string option;
  }

  let is_safe (x : t) : bool = Outcome.is_safe x.outcome

  (* The encoder choice is the top-level [Solve_drf.Encoder]: per
     proof in the stream we start with [Encoder.initial ~logic]
     (preferring the arithmetic [IntGen] encoder because it admits
     no wrap-around models) and fall back to [Encoder.bv64 ()] on
     [Not_implemented] for that single proof. The bexp-only
     pre-flight [check_bexp_sat] uses the same encoder pair. *)

  (*
    Example of retrieving values from a model.

    https://github.com/icra-team/icra/blob/ee3fd360ee75490277dd3fd05d92e1548db983e4/duet/pa/paSmt.ml
    *)
  let solve ?(timeout = None) ?(show_proofs = false) ?(logic = None)
      ?(solve_tactic : Gen_z3.Tactic.t option = None)
      ?(extras : (int * bexp) list = []) ?(deterministic = false)
      (ps : Symbexp.Proof.t Streamutil.stream) : t Streamutil.stream =
    (* User-requested BV logic warning fires once, not once per proof. *)
    (match logic with
     | Some l when String.ends_with ~suffix:"BV" l ->
       prerr_endline ("WARNING: user set bit-vector logic " ^ l)
     | _ -> ());
    Streamutil.map
      (fun (p : Symbexp.Proof.t) ->
        if
          Int64.compare
            (Z3.Statistics.get_estimated_alloc_size ())
            gc_alloc_threshold
          > 0
        then Gc.full_major ();
        let want_core = extras <> [] in
        let options =
          [ ("model", "true"); ("proof", "false") ]
          @ (if want_core then [ ("unsat_core", "true") ] else [])
          @
          match timeout with
          | Some timeout -> [ ("timeout", string_of_int timeout) ]
          | None -> []
        in
        (* When [want_core] is true the tactic-built solver is bypassed.
           Tactic solvers nominally accept unsat_core (the OCaml
           binding's probe confirms cores come back), but in the full
           genie pipeline they end up either spinning the abductive
           loop or running per-query slower than [mk_simple_solver].
           Until we have a reproducer, prefer the simple solver for
           the core path. *)
        let mk_solver_for (enc : Encoder.t) ctx =
          if want_core then
            (match enc.logic with
             | None -> Solver.mk_simple_solver ctx
             | Some logic -> Solver.mk_solver_s ctx logic)
          else
            match solve_tactic with
            | Some t -> Solver.mk_solver_t ctx (Gen_z3.Tactic.to_z3 ctx t)
            | None ->
              (match enc.logic with
               | None -> Solver.mk_simple_solver ctx
               | Some logic -> Solver.mk_solver_s ctx logic)
        in
        let trackers : (int * Z3.Expr.expr) list ref = ref [] in
        let solve_with (enc : Encoder.t) : Solver.solver =
          (* Create a solver under [enc] and add the proof's goal plus
             any tracked [extras]. May raise [Not_implemented] when
             [enc] is [intgen] and the goal needs BV-only operators.
             The tracker is named [extra_<id>] only because Z3 requires
             a [Symbol]; the [id] is what flows back through the
             unsat-core. *)
          let ctx = Z3.mk_context options in
          let s = mk_solver_for enc ctx in
          Solver.add s [ enc.b_to_expr ctx (Predicates.b_inline p.goal) ];
          let s =
            if deterministic && not want_core then (
              let text = Solver.to_string s in
              let ctx = Z3.mk_context options in
              let asserts = Z3.SMT.parse_smtlib2_string ctx text [] [] [] [] in
              let s = mk_solver_for enc ctx in
              Solver.add s (Z3.AST.ASTVector.to_expr_list asserts);
              s)
            else s
          in
          trackers :=
            List.map
              (fun (id, b) ->
                let track =
                  Z3.Boolean.mk_const_s ctx ("extra_" ^ string_of_int id)
                in
                let expr = enc.b_to_expr ctx (Predicates.b_inline b) in
                Solver.assert_and_track s expr track;
                (id, track))
              extras;
          s
        in
        let rec attempt tries =
        try
        let enc, s =
          let initial = Encoder.initial ~logic in
          try (initial, solve_with initial)
          with Not_implemented x ->
            prerr_endline
              ("WARNING: arithmetic solver cannot handle operator '" ^ x
             ^ "', falling back to bit-vector arithmetic for this proof.");
            let bv = Encoder.bv64 () in
            (bv, solve_with bv)
        in
        (if show_proofs then
          let title = "proof #" ^ string_of_int p.id in
          let body = Solver.to_string s ^ "(check-sat)\n(get-model)\n" in
          prerr_endline ("=== " ^ title ^ " ===");
          prerr_endline body
        );
        let r =
          let open Outcome in
          let stats_detail () =
            Printf.sprintf "  proof=%d\n%s"
              p.id (Solver.get_statistics s |> Z3.Statistics.to_string)
          in
          match
            Phase_timer.measure "z3-check" ~detail:stats_detail (fun () ->
              Solver.check s [])
          with
          | UNSATISFIABLE when want_core ->
            (* Parse [extra_<id>] tracker names back to the [id]
               we handed in. The unsat-core enumeration order is
               Z3-internal; sorting by integer makes the result
               deterministic for a given Z3 run. *)
            let core = Solver.get_unsat_core s in
            let core_ids =
              List.filter_map
                (fun ce ->
                  let str = Z3.Expr.to_string ce in
                  if String.starts_with ~prefix:"extra_" str then
                    int_of_string_opt
                      (String.sub str 6 (String.length str - 6))
                  else None)
                core
              |> List.sort_uniq Int.compare
            in
            let _ = !trackers in
            Drf_with_core core_ids
          | UNSATISFIABLE -> Drf
          | SATISFIABLE -> (
              match Solver.get_model s with
              | Some m ->
                  let w = Witness.parse enc.parse_num ~proof:p m in
                  (* The race goal excludes benign same-value writes, so any
                     satisfying model is a genuine conflict. *)
                  assert (Witness.can_conflict w);
                  Racy w
              | None -> failwith "INVALID")
          | UNKNOWN -> Unknown
        in
        { proof = p; outcome = r; logic = enc.logic }
        with Z3.Error msg ->
          if tries > 0 then (
            Gc.full_major ();
            attempt (tries - 1))
          else (
            prerr_endline
              (Printf.sprintf
                 "WARNING: Z3 error on proof %d (%s); treating as unknown" p.id
                 msg);
            { proof = p; outcome = Outcome.Unknown; logic = None })
        in
        attempt 1)
      ps
end
