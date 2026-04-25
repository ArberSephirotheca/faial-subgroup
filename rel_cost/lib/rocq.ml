(** Translate a faial Memory Access Protocol into a Coq [ProtoLet.t]
    definition usable by the formalism in [faial-cost-rocq]
    (see [src/Warp/ProtoLet.v] and [src/Warp/Copper.v]).

    {1 Output style}

    The traversal in [code_to_s] is parameterized by a {!Syntax.t}
    record so that the same translation can render either bare
    [Inductive] constructors ([ProtoLet.Skip], [ProtoLet.Seq], ...) or
    the [CNotations] surface syntax ([SKIP], [;;], [IF/THEN/ELSE], ...)
    without re-walking the AST.

    Currently only {!Syntax.constructor} is implemented; a notation
    backend can be added by populating a second {!Syntax.t} value.

    {1 Mapping}

    - [Skip], [Sync _]                → [ProtoLet.Skip]
    - [Access a]                      → [ProtoLet.Access <metric> <index>]
    - [Seq (p, q)]                    → [ProtoLet.Seq <p> <q>]
    - [If (b, p, q)]                  → [ProtoLet.ite <b> <p> <q>]
    - [Loop { range; body }]          → see {!loop_form}
    - [Decl { body; _ }]              → translates [body]; the declared
                                        variable surfaces as a free
                                        [Ident.t] declared in the section.
    - [Var threadIdx.x]               → [NExp.Tid]

    {1 Loop forms} *)

(** {2 Loop forms}

    Two ProtoLet shapes are emitted depending on the [Range]:

    - [Loop (RExp.make x lb ub) body] — direct constructor form. Used
      when the step is unit ([Plus (Num 1)]) and the bounds may be
      symbolic [NExp.t]. Note: ProtoLet's [Loop] semantics is
      *inclusive* on both ends, so this form iterates one extra time
      compared to C's [for (x = lb; x < ub; ++x)]. We emit it anyway
      to match the style of the existing Copper examples
      ([SYN_BRDIS], etc.); downstream proofs should be aware.

    - [for_ x lb (ub + 1) k (fun x => body)] — C-semantics
      ([x < ub']) strided loop. Used when [step = Plus (Num k)] with
      [k ≥ 2] reduces to a literal [nat] (folded expressions like
      [Num 1024 * Num 1] qualify via [Exp.n_eval_opt]). [for_]'s
      [lb] and [ub] are [NExp.t], so symbolic bounds — including the
      [for (i = blockIdx.x*blockDim.x + threadIdx.x; i < N; i += W)]
      grid-stride pattern — work directly. [stride] is still [nat].

      The [+1] adjustment compensates for faial's representation: at
      inference time [for (i = 0; i < N; …)] is stored with
      [upper_bound = N - 1] (see [imp/lib/for.ml:infer_bounds]), so
      faial's [ub] is *inclusive*. ProtoLet's [Loop] is also
      inclusive (so the symbolic-bounds path emits [ub] verbatim) but
      [for_] is C-exclusive — hence the [+1]. We fold [(N - 1) + 1]
      to [N] when [ub] is a literal, falling back to a syntactic
      [Bin Add ub (Num 1)] otherwise (the Rocq-side [smart_*]
      constructors will simplify what they can).

    Other range shapes — [Mult] step, [Decrease] direction,
    non-literal bounds with [k ≠ 1] — fall outside the supported
    subset and surface as a translation error. *)

open Stage0
open Protocols
open Exp

type error = string

exception Translation_error of error

(** {1 Identifier sanitization} *)

(** Coerce an arbitrary kernel name into a valid Coq identifier:
    keep [A-Za-z0-9_'], replace anything else with [_], and if the
    result starts with a digit prefix it with [_]. Empty input maps
    to ["_anon"]. Inference sometimes emits names like
    ["@AccessState98"] or ["blockDim.x"], both of which need
    rewriting. *)
let sanitize_name (name : string) : string =
  let buf = Buffer.create (String.length name) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' ->
          Buffer.add_char buf c
      | _ -> Buffer.add_char buf '_')
    name;
  let s = Buffer.contents buf in
  if s = "" then "_anon"
  else
    match s.[0] with '0' .. '9' -> "_" ^ s | _ -> s

(** {1 Operator translation} *)

let n_binary_to_op : N_binary.t -> (string, error) Result.t = function
  | Plus -> Ok "NOp.Add"
  | Minus -> Ok "NOp.Sub"
  | Mult -> Ok "NOp.Mult"
  | Div -> Ok "NOp.Div"
  | Mod -> Ok "NOp.Mod"
  | (BitOr | BitXOr | BitAnd | LeftShift | RightShift) as o ->
      Error ("unsupported binary operator: " ^ N_binary.to_string o)

let n_rel_to_op : N_rel.t -> string = function
  | Eq -> "ROp.Eq"
  | Neq -> "ROp.Neq"
  | Lt -> "ROp.Lt"
  | Le -> "ROp.Le"
  | Gt -> "ROp.Gt"
  | Ge -> "ROp.Ge"

let b_rel_to_op : B_rel.t -> string = function
  | BAnd -> "BOp.And"
  | BOr -> "BOp.Or"

(** {1 Translation environment}

    [env] tracks which OCaml [Variable.t] names are currently bound by
    a surrounding Coq lambda parameter of type [NExp.t] (introduced by
    [for_]'s [body : NExp.t -> t]). Lookups of [Var x] for such [x]
    emit the bare Coq identifier instead of [(NExp.Var x)] — the value
    is already an [NExp.t]. Outside this set, [Var x] is treated as a
    free [Ident.t] and wrapped with [NExp.Var]. *)
type env = Variable.Set.t

let env_empty : env = Variable.Set.empty

let env_bind (x : Variable.t) (e : env) : env = Variable.Set.add x e

(** {1 Expression translation} *)

let rec nexp_to_coq (env : env) (n : nexp) : (string, error) Result.t =
  let ( let* ) = Result.bind in
  match n with
  | Num k when k >= 0 -> Ok (Printf.sprintf "(NExp.Num %d)" k)
  | Num k -> Error (Printf.sprintf "negative numeric literal: %d" k)
  | Var x when Variable.equal x Variable.tid_x -> Ok "NExp.Tid"
  | Var x when Variable.is_tid x ->
      Error
        ("only threadIdx.x maps to NExp.Tid (the [Dim.T] axis); got: "
       ^ Variable.name x)
  | Var x when Variable.Set.mem x env ->
      (* Bound by an enclosing for_ lambda: already has type NExp.t. *)
      Ok (sanitize_name (Variable.name x))
  | Var x ->
      Ok (Printf.sprintf "(NExp.Var %s)" (sanitize_name (Variable.name x)))
  | Binary (op, e1, e2) ->
      let* op_s = n_binary_to_op op in
      let* e1_s = nexp_to_coq env e1 in
      let* e2_s = nexp_to_coq env e2 in
      Ok (Printf.sprintf "(NExp.Bin %s %s %s)" op_s e1_s e2_s)
  | (Unary _ | NCall _ | NIf _ | Other _ | CastInt _) as e ->
      Error ("unsupported nexp: " ^ Exp.n_to_string e)

let rec bexp_to_coq (env : env) (b : bexp) : (string, error) Result.t =
  let ( let* ) = Result.bind in
  match b with
  | Bool true -> Ok "(BExp.Bool true)"
  | Bool false -> Ok "(BExp.Bool false)"
  | NRel (op, e1, e2) ->
      let* e1_s = nexp_to_coq env e1 in
      let* e2_s = nexp_to_coq env e2 in
      Ok (Printf.sprintf "(BExp.Rel %s %s %s)" (n_rel_to_op op) e1_s e2_s)
  | BRel (op, b1, b2) ->
      let* b1_s = bexp_to_coq env b1 in
      let* b2_s = bexp_to_coq env b2 in
      Ok (Printf.sprintf "(BExp.Bin %s %s %s)" (b_rel_to_op op) b1_s b2_s)
  | BNot b ->
      let* b_s = bexp_to_coq env b in
      Ok (Printf.sprintf "(BExp.neg %s)" b_s)
  | (CastBool _ | Pred _ | Distinct _) as e ->
      Error ("unsupported bexp: " ^ Exp.b_to_string e)

(** {1 Range classification}

    A [Range.t] is dispatched to one of three forms before code is
    emitted. *)

(** Result of classifying a [Range.t] for emission. *)
type range_form =
  | Empty_loop
      (** Statically empty under literal bounds: [lb > ub] in faial's
          inclusive convention. The whole [Loop] is replaced by [Skip]. *)
  | Unit_stride of {
      var : string;
      lb : string;  (** Coq [NExp.t] string. *)
      ub : string;  (** Coq [NExp.t] string (faial-inclusive). *)
    }
      (** Unit-stride additive range; emitted as
          [Loop (RExp.make var lb ub) body]. ProtoLet's [Loop] is
          inclusive on both ends, matching faial's convention, so [ub]
          passes through unchanged. *)
  | Strided of {
      var : string;
      lb : string;  (** Coq [NExp.t] string. *)
      ub_excl : string;
          (** Coq [NExp.t] string for [ub + 1] (C-exclusive). *)
      stride : int;  (** Coq [nat] literal, [≥ 2]. *)
    }
      (** Additive range with literal stride [≥ 2]. Emitted as
          [for_ var lb ub_excl stride (fun var => body)]. Bounds are
          [NExp.t], so symbolic ones (Tid, free vars, …) are fine. *)

let classify_range (r : Range.t) : (range_form, error) Result.t =
  let ( let* ) = Result.bind in
  let* () =
    match r.dir with
    | Increase -> Ok ()
    | Decrease ->
        Error
          ("decreasing loops are not supported yet: " ^ Range.to_string r)
  in
  let var = sanitize_name (Variable.name r.var) in
  match r.step with
  | Mult _ ->
      Error
        ("multiplicative-step loops are not supported yet: "
       ^ Range.to_string r)
  | Plus stride_e -> (
      (* [Exp.n_eval_opt] folds whole expressions, so e.g.
         [Num 1024 * Num 1] qualifies as a literal stride. *)
      let lit n =
        match Exp.n_eval_opt n with Some k when k >= 0 -> Some k | _ -> None
      in
      let lb_lit = lit r.lower_bound in
      let ub_lit = lit r.upper_bound in
      let stride_lit = lit stride_e in
      (* Static empty-loop detection: only possible when both bounds
         are literals. Faial's stored [ub] is inclusive. *)
      let statically_empty =
        match (lb_lit, ub_lit) with
        | Some lb, Some ub -> lb > ub
        | _ -> false
      in
      if statically_empty then Ok Empty_loop
      else
        match stride_lit with
        | Some 1 ->
            let* lb = nexp_to_coq env_empty r.lower_bound in
            let* ub = nexp_to_coq env_empty r.upper_bound in
            Ok (Unit_stride { var; lb; ub })
        | Some k when k >= 2 ->
            let* lb = nexp_to_coq env_empty r.lower_bound in
            (* Fold [(Num k) + 1] to [Num (k+1)] when [ub] is a
               literal, avoiding a redundant [Bin Add _ (Num 1)] in
               the common literal-bound case. *)
            let* ub_excl =
              match ub_lit with
              | Some ub -> Ok (Printf.sprintf "(NExp.Num %d)" (ub + 1))
              | None ->
                  let* ub_s = nexp_to_coq env_empty r.upper_bound in
                  Ok
                    (Printf.sprintf
                       "(NExp.Bin NOp.Add %s (NExp.Num 1))" ub_s)
            in
            Ok (Strided { var; lb; ub_excl; stride = k })
        | Some k ->
            Error
              (Printf.sprintf
                 "stride must be ≥ 1 (got: %d in %s)" k (Range.to_string r))
        | None ->
            Error
              ("stride does not reduce to a literal nat: "
             ^ Range.to_string r))

(** {1 Metric choice}

    Selects which Coq [Metric.T] instance the generated [Access]
    nodes use. Each choice fixes:
    - the Coq expression spliced as the first argument of [Access]
      (a [Metric.T] value), and
    - any extra [Variable …] declarations the section needs to bind
      free parameters of that metric (e.g. [MemReads]'s sector size). *)
module Metric = struct
  type t =
    | MemReads
        (** [Warp.MemReads.T m] — number of distinct memory sectors
            touched per access. Sector size [m] surfaces as a section
            [Variable m : nat.]. Filters arrays to global memory in
            the binary (mirrors [Rel_cost.Metric.UncoalescedAccesses]). *)
    | ActiveThreads
        (** [Warp.Metric.CountEnabled.T] — number of enabled threads
            in the warp (address-independent). No parameters. Applies
            to any memory type. *)

  let to_string : t -> string = function
    | MemReads -> "mem-reads"
    | ActiveThreads -> "active"

  let choices : (string * t) list =
    [ ("mem-reads", MemReads); ("active", ActiveThreads) ]

  (** Coq expression of type [Metric.T] for the [Access] constructor. *)
  let to_coq_expr : t -> string = function
    | MemReads -> "(MemReads.T m)"
    | ActiveThreads -> "Metric.CountEnabled.T"

  (** Extra declarations to insert into the section preamble for free
      parameters of this metric. *)
  let preamble_lines : t -> Indent.t list =
    let open Indent in
    function
    | MemReads ->
        [
          Line "(* Sector size for the MemReads metric. *)";
          Line "Variable m : nat.";
          Line "";
        ]
    | ActiveThreads -> []
end

(** {1 Output backends}

    The {!Syntax.t} record bundles every choice of how a [Code]
    constructor is rendered, plus any per-backend preamble lines (extra
    [Require Import] / [Open Scope] declarations needed by the chosen
    rendering). The traversal in {!code_to_s} only sees this record —
    adding a notation backend is a matter of writing a second {!Syntax.t}
    value. *)
module Syntax = struct
  type t = {
    name : string;
        (** Backend tag, surfaced in the generated header comment. *)
    extra_preamble : string list;
        (** Extra header lines appended after the common imports. The
            [constructor] backend needs none; a notation backend would
            put [Import ProtoLet.CNotations.] and
            [Local Open Scope proto_scope.] here. *)
    skip : Indent.t list;
    access : metric:string -> index:string -> Indent.t list;
    seq : Indent.t list -> Indent.t list -> Indent.t list;
    ite :
      cond:string -> Indent.t list -> Indent.t list -> Indent.t list;
    loop :
      var:string -> lb:string -> ub:string -> Indent.t list -> Indent.t list;
        (** Inclusive [Loop] used for unit-stride additive ranges.
            Bounds are pre-translated [NExp.t] strings (faial
            inclusive matches Coq inclusive — no adjustment). *)
    for_ :
      var:string ->
      lb:string ->
      ub_excl:string ->
      stride:int ->
      Indent.t list ->
      Indent.t list;
        (** C-semantics strided [for_]. Bounds are pre-translated
            [NExp.t] strings; [ub_excl] is already C-exclusive (the
            classifier added [+1]). [stride] is a literal nat. The
            body is rendered inside [fun var => …] where [var] has
            type [NExp.t], so callers must have translated the body
            with [var] bound in the env. *)
  }

  (** Bare [Inductive] constructor backend.

      Every compound output is wrapped in a single set of parens so
      the traversal never has to worry about precedence. *)
  let constructor : t =
    let open Indent in
    {
      name = "constructor";
      extra_preamble = [];
      skip = [ Line "ProtoLet.Skip" ];
      access =
        (fun ~metric ~index ->
          [ Line (Printf.sprintf "(ProtoLet.Access %s %s)" metric index) ]);
      seq =
        (fun p q ->
          [ Line "(ProtoLet.Seq"; Block p; Block q; Line ")" ]);
      ite =
        (fun ~cond p q ->
          [
            Line (Printf.sprintf "(ProtoLet.ite %s" cond);
            Block p;
            Block q;
            Line ")";
          ]);
      loop =
        (fun ~var ~lb ~ub body ->
          [
            Line
              (Printf.sprintf "(ProtoLet.Loop (Base.RExp.make %s %s %s)"
                 var lb ub);
            Block body;
            Line ")";
          ]);
      for_ =
        (fun ~var ~lb ~ub_excl ~stride body ->
          [
            Line
              (Printf.sprintf "(ProtoLet.for_ %s %s %s %d (fun %s =>" var lb
                 ub_excl stride var);
            Block body;
            Line "))";
          ]);
    }
end

(** {1 Code translation} *)

type config = {
  metric : string;
      (** A Coq expression of type [Metric.T], e.g. ["MemReads.T m"].
          Inserted verbatim as the first argument of [Access]. *)
  syntax : Syntax.t;
}

let make_config ?(syntax = Syntax.constructor) ~metric () : config =
  { metric; syntax }

let rec code_to_s (cfg : config) (env : env) (c : Code.t) :
    (Indent.t list, error) Result.t =
  let ( let* ) = Result.bind in
  let s = cfg.syntax in
  match c with
  | Skip | Sync _ -> Ok s.skip
  | Access a ->
      let* idx =
        match a.index with
        | [ i ] -> nexp_to_coq env i
        | _ ->
            Error
              (Printf.sprintf
                 "multi-dimensional access not supported (linearize first): %s"
                 (Access.to_string a))
      in
      Ok (s.access ~metric:cfg.metric ~index:idx)
  | Seq (p, q) ->
      let* p_s = code_to_s cfg env p in
      let* q_s = code_to_s cfg env q in
      Ok (s.seq p_s q_s)
  | If (b, p, q) ->
      let* b_s = bexp_to_coq env b in
      let* p_s = code_to_s cfg env p in
      let* q_s = code_to_s cfg env q in
      Ok (s.ite ~cond:b_s p_s q_s)
  | Loop { range; body } -> (
      let* form = classify_range range in
      match form with
      | Empty_loop -> Ok s.skip
      | Unit_stride { var; lb; ub } ->
          let* body_s = code_to_s cfg env body in
          Ok (s.loop ~var ~lb ~ub body_s)
      | Strided { var; lb; ub_excl; stride } ->
          (* Inside the [for_] body, the loop variable becomes a Coq
             lambda parameter of type [NExp.t]. *)
          let env' = env_bind range.var env in
          let* body_s = code_to_s cfg env' body in
          Ok (s.for_ ~var ~lb ~ub_excl ~stride body_s))
  | Decl { body; _ } -> code_to_s cfg env body

(** {1 Kernel translation} *)

let common_header : string list =
  [
    "From Stdlib Require Import PeanoNat.";
    "Require Import Base.NExp.";
    "Require Import Base.BExp.";
    "Require Import Base.RExp.";
    "Require Import Base.NOp.";
    "Require Import Base.BOp.";
    "Require Import Base.ROp.";
    "Require Base.Ident.";
    "Require Dim.";
    "Require Warp.Metric.";
    "Require Warp.MemReads.";
    "Require Warp.ProtoLet.";
  ]

(** Sanitize a kernel name so it forms a valid Coq module identifier.
    First character must be a letter or underscore; we prefix with [K_]
    otherwise. *)
let sanitize_module_name (name : string) : string =
  let buf = Buffer.create (String.length name) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
         || (c >= '0' && c <= '9')
      then Buffer.add_char buf c
      else Buffer.add_char buf '_')
    name;
  let s = Buffer.contents buf in
  if String.length s = 0 then "K_anonymous"
  else
    let c = s.[0] in
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' then s
    else "K_" ^ s

(** Compute the set of [Variable.t]s that the translator actually
    emits as [Ident.t] in the generated Coq — i.e. exactly those that
    need a [Variable … : Ident.t.] declaration in the section.

    Mirrors the emission rules in {!code_to_s}:
    - A loop binder is emitted as [Ident.t] (first argument of
      [RExp.make] or [for_]) regardless of whether the body uses it.
    - Inside a [for_] body the loop variable is shadowed by a Coq
      lambda parameter of type [NExp.t]; references to it do not
      surface as [(NExp.Var _)], so it doesn't count there.
    - [Decl] binders are kept only when the body still references
      them (we drop the [Decl] wrapper but inherit any free [Var x]).
    - Free expression variables (in code, ranges, and [pre]) count
      whenever they aren't shadowed by an enclosing [for_] lambda.

    [threadIdx.x] is always excluded — it renders as [NExp.Tid]. *)
let used_idents (k : Kernel.t) : Variable.t list =
  (* Mirrors [classify_range]: a range emits the [for_] form (with a
     lambda binding for the loop variable) iff it is increasing, has
     additive step, and the stride reduces to a literal nat [≥ 2]. *)
  let is_for_form (r : Range.t) : bool =
    match (r.dir, r.step) with
    | Increase, Plus stride_e -> (
        match Exp.n_eval_opt stride_e with Some k -> k >= 2 | None -> false)
    | _ -> false
  in
  let add_unbound (env : Variable.Set.t) (x : Variable.t)
      (acc : Variable.Set.t) : Variable.Set.t =
    if Variable.Set.mem x env then acc else Variable.Set.add x acc
  in
  let nexp env acc e = Exp.n_fold (add_unbound env) e acc in
  let bexp env acc b = Exp.b_fold (add_unbound env) b acc in
  let rec on_code (env : Variable.Set.t) (acc : Variable.Set.t)
      (c : Code.t) : Variable.Set.t =
    match c with
    | Skip | Sync _ -> acc
    | Access a -> List.fold_left (nexp env) acc a.index
    | Seq (p, q) -> on_code env (on_code env acc p) q
    | If (b, p, q) ->
        let acc = bexp env acc b in
        on_code env (on_code env acc p) q
    | Loop { range; body } ->
        (* The binder is always emitted as Ident.t. *)
        let acc = Variable.Set.add range.var acc in
        (* Bounds are evaluated in the outer scope. *)
        let acc = nexp env acc range.lower_bound in
        let acc = nexp env acc range.upper_bound in
        let env' =
          if is_for_form range then Variable.Set.add range.var env else env
        in
        on_code env' acc body
    | Decl { body; _ } -> on_code env acc body
  in
  let acc = on_code Variable.Set.empty Variable.Set.empty k.code in
  let acc = bexp Variable.Set.empty acc k.pre in
  let acc = Variable.Set.remove Variable.tid_x acc in
  Variable.Set.elements acc

let from_kernel ?(syntax = Syntax.constructor) ?(metric = Metric.MemReads)
    (k : Kernel.t) : (Indent.t list, error) Result.t =
  let ( let* ) = Result.bind in
  let cfg = make_config ~syntax ~metric:(Metric.to_coq_expr metric) () in
  let* body = code_to_s cfg env_empty k.code in
  let mod_name = sanitize_module_name k.name in
  let idents = used_idents k in
  let var_idents =
    if idents = [] then []
    else
      let names =
        idents
        |> List.map (fun v -> sanitize_name (Variable.name v))
        |> String.concat " "
      in
      [ Indent.Line (Printf.sprintf "Variable %s : Ident.t." names) ]
  in
  let open Indent in
  Ok
    [
      Line ("Module " ^ mod_name ^ ".");
      Block
        ([
           Line "Section Def.";
           Block
             ([ Line "Context {D : Dim.T}."; Line "" ]
             @ Metric.preamble_lines metric
             @ (if var_idents = [] then []
                else
                  [ Line "(* Identifiers from the kernel. *)" ]
                  @ var_idents @ [ Line "" ])
             @ [ Line "Let kernel : ProtoLet.t :=" ]
             @ [ Block body ] @ [ Line "." ]);
           Line "End Def.";
         ]);
      Line ("End " ^ mod_name ^ ".");
    ]

let from_kernels ?(syntax = Syntax.constructor) ?(metric = Metric.MemReads)
    (ks : Kernel.t list) : (Indent.t list, error) Result.t =
  let ( let* ) = Result.bind in
  let* mods =
    List.fold_left
      (fun acc k ->
        let* acc = acc in
        let* m = from_kernel ~syntax ~metric k in
        Ok (acc @ m @ [ Indent.Line "" ]))
      (Ok []) ks
  in
  let header_comment =
    Printf.sprintf "(* Generated by faial-to-rocq (%s backend, %s metric). *)"
      syntax.name (Metric.to_string metric)
  in
  let header_lines =
    Indent.Line header_comment
    :: List.map (fun s -> Indent.Line s) common_header
    @ List.map (fun s -> Indent.Line s) syntax.extra_preamble
  in
  Ok (header_lines @ [ Indent.Line "" ] @ mods)

let to_string (l : Indent.t list) : string = Indent.to_string l
