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

    - [for_mul x lb count (fun x => body)] — base-2 ascending,
      body sees [lb * 2^x] for [x = 0..count-1]. Used when
      [step = Mult (Num 2)] and [dir = Increase] (e.g.
      [for (i = 1; i < N; i *= 2)] or [i <<= 1]). [count] must be
      a literal [nat]; we derive it from literal [lb] and [ub] by
      iterating [lb, lb*2, lb*4, …] until exceeding [ub]. Symbolic
      bounds aren't expressible (the Coq [count] argument is [nat]).
      [count = 0] elaborates Coq-side to [Skip].

    - [for_div x ub count (fun x => body)] — base-2 descending,
      body sees [ub / 2^x] for [x = 0..count-1]. Used when
      [step = Mult (Num 2)] and [dir = Decrease] (e.g.
      [for (i = N; i >= 1; i /= 2)] or [i >>= 1]). Same
      literal-bounds restriction as [for_mul].

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
  | Plus -> Ok "Add"
  | UPlus -> Ok "Add"
  | Minus -> Ok "Sub"
  | Mult -> Ok "Mult"
  | UMult -> Ok "Mult"
  | Div -> Ok "Div"
  | Mod -> Ok "Mod"
  | (BitOr | BitXOr | BitAnd | LeftShift | RightShift) as o ->
      Error ("unsupported binary operator: " ^ N_binary.to_string o)

(** ROp's [Eq], [Lt], [Gt] collide with [Datatypes.comparison]'s
    constructors. We emit [N_]-prefixed aliases (introduced as
    [Notation]s in {!from_kernel}) so the generated source parses
    unambiguously. The remaining three ([Neq], [Le], [Ge]) are
    aliased identically for visual symmetry. *)
let n_rel_to_op : N_rel.t -> string = function
  | Eq -> "NEq"
  | Neq -> "NNeq"
  | Lt -> "NLt"
  | ULt -> "NLt"
  | Le -> "NLe"
  | Gt -> "NGt"
  | Ge -> "NGe"

let b_rel_to_op : B_rel.t -> string = function
  | BAnd -> "And"
  | BOr -> "Or"

(** {1 Expression translation}

    Every program variable [x] gets a single section-level binding
    (built by {!from_kernel}); two coercions installed at the top of
    the section lift them into [NExp.t] at use sites:
    - Globals (thread-uniform kernel parameters) →
      [Variable x : nat.]; coerced via [NExp.Num : nat >-> NExp.t].
    - Locals (loop binders, [Decl]-bound names) →
      [Let x : Ident.t := Ident.make N.]; coerced via
      [NExp.Var : Ident.t >-> NExp.t].

    In both cases [Var x] renders as the bare sanitized name [x].
    [threadIdx.x] is special-cased to [NExp.Tid]. *)

(** Numeric literals are emitted as bare nats; the section-level
    [Coercion NExp.Num : nat >-> NExp.t] lifts them where an [NExp.t]
    is expected. Constructors are emitted unqualified ([Rel], [Bool],
    [Tid], [Add], …) and rely on the [Require Import]s in
    {!common_header}. Identifiers that would collide — [NExp.Bin] and
    [BExp.Bin] (same name, different inductive); [ROp.Eq], [ROp.Lt],
    [ROp.Gt] (collide with [Datatypes.comparison]) — are emitted via
    section-level [Notation] aliases ([NBin], [BBin], [NEq], …)
    introduced by {!from_kernel}. *)
let rec nexp_to_coq (n : nexp) : (string, error) Result.t =
  let ( let* ) = Result.bind in
  match n with
  | Num k when k >= 0 -> Ok (string_of_int k)
  | Num k -> Error (Printf.sprintf "negative numeric literal: %d" k)
  | Var x when Variable.equal x Variable.tid_x -> Ok "Tid"
  | Var x when Variable.is_tid x ->
      Error
        ("only threadIdx.x maps to NExp.Tid (the [Dim.T] axis); got: "
       ^ Variable.name x)
  | Var x -> Ok (sanitize_name (Variable.name x))
  | Binary (op, e1, e2) ->
      let* op_s = n_binary_to_op op in
      let* e1_s = nexp_to_coq e1 in
      let* e2_s = nexp_to_coq e2 in
      Ok (Printf.sprintf "(NBin %s %s %s)" op_s e1_s e2_s)
  | (Unary _ | NCall _ | NIf _ | Other _ | CastInt _) as e ->
      Error ("unsupported nexp: " ^ Exp.n_to_string e)

let rec bexp_to_coq (b : bexp) : (string, error) Result.t =
  let ( let* ) = Result.bind in
  match b with
  | Bool true -> Ok "(Bool true)"
  | Bool false -> Ok "(Bool false)"
  | NRel (op, e1, e2) ->
      let* e1_s = nexp_to_coq e1 in
      let* e2_s = nexp_to_coq e2 in
      Ok (Printf.sprintf "(Rel %s %s %s)" (n_rel_to_op op) e1_s e2_s)
  | BRel (op, b1, b2) ->
      let* b1_s = bexp_to_coq b1 in
      let* b2_s = bexp_to_coq b2 in
      Ok (Printf.sprintf "(BBin %s %s %s)" (b_rel_to_op op) b1_s b2_s)
  | BNot b ->
      let* b_s = bexp_to_coq b in
      Ok (Printf.sprintf "(neg %s)" b_s)
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
  | For_mul of {
      var : string;
      lb : string;  (** Coq [NExp.t] string. *)
      count : int;  (** Number of base-2 doublings, derived statically. *)
    }
      (** Base-2 ascending multiplicative range (step [Mult (Num 2)],
          [dir = Increase]). Emitted as
          [for_mul var lb count (fun var => body)]. *)
  | For_div of {
      var : string;
      ub : string;  (** Coq [NExp.t] string. *)
      count : int;  (** Number of base-2 halvings, derived statically. *)
    }
      (** Base-2 descending multiplicative range (step [Mult (Num 2)],
          [dir = Decrease]). Emitted as
          [for_div var ub count (fun var => body)]. *)

let classify_range (r : Range.t) : (range_form, error) Result.t =
  let ( let* ) = Result.bind in
  let var = sanitize_name (Variable.name r.var) in
  let lit n =
    match Exp.n_eval_opt n with Some k when k >= 0 -> Some k | _ -> None
  in
  match (r.dir, r.step) with
  | Decrease, Plus _ ->
      Error
        ("decreasing additive loops are not supported yet: "
       ^ Range.to_string r)
  | Increase, Mult stride_e -> (
      (* Base-2 ascending: [for_mul x lb count]. The body sees
         [lb * 2^k] for [k = 0..count-1]; [count] is derived by
         iterating [lb, lb*2, lb*4, …] while still [≤ ub]. Both
         bounds must be literal so [count] is a Coq nat. *)
      match (lit stride_e, lit r.lower_bound, lit r.upper_bound) with
      | Some 2, Some lb, Some ub when lb >= 1 ->
          let rec count_up cur n = if cur > ub then n else count_up (cur * 2) (n + 1) in
          let count = count_up lb 0 in
          if count = 0 then Ok Empty_loop
          else
            let* lb_s = nexp_to_coq r.lower_bound in
            Ok (For_mul { var; lb = lb_s; count })
      | Some 2, _, _ ->
          Error
            ("for_mul requires literal nat bounds with lb ≥ 1: "
           ^ Range.to_string r)
      | Some k, _, _ ->
          Error
            (Printf.sprintf
               "only base-2 multiplicative steps are supported (got: \
                %d in %s)"
               k (Range.to_string r))
      | None, _, _ ->
          Error
            ("multiplicative stride does not reduce to a literal nat: "
           ^ Range.to_string r))
  | Decrease, Mult stride_e -> (
      (* Base-2 descending: [for_div x ub count]. The body sees
         [ub / 2^k] for [k = 0..count-1]; [count] is derived by
         iterating [ub, ub/2, ub/4, …] while still [≥ lb]. *)
      match (lit stride_e, lit r.lower_bound, lit r.upper_bound) with
      | Some 2, Some lb, Some ub when lb >= 1 ->
          let rec count_down cur n =
            if cur < lb then n else count_down (cur / 2) (n + 1)
          in
          let count = count_down ub 0 in
          if count = 0 then Ok Empty_loop
          else
            let* ub_s = nexp_to_coq r.upper_bound in
            Ok (For_div { var; ub = ub_s; count })
      | Some 2, _, _ ->
          Error
            ("for_div requires literal nat bounds with lb ≥ 1: "
           ^ Range.to_string r)
      | Some k, _, _ ->
          Error
            (Printf.sprintf
               "only base-2 multiplicative steps are supported (got: \
                %d in %s)"
               k (Range.to_string r))
      | None, _, _ ->
          Error
            ("multiplicative stride does not reduce to a literal nat: "
           ^ Range.to_string r))
  | Increase, Plus stride_e -> (
      (* [lit] is hoisted to the top of [classify_range] and folds
         whole expressions, so e.g. [Num 1024 * Num 1] qualifies as a
         literal stride. *)
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
            let* lb = nexp_to_coq r.lower_bound in
            let* ub = nexp_to_coq r.upper_bound in
            Ok (Unit_stride { var; lb; ub })
        | Some k when k >= 2 ->
            let* lb = nexp_to_coq r.lower_bound in
            (* Fold [(Num k) + 1] to [Num (k+1)] when [ub] is a
               literal, avoiding a redundant [Bin Add _ (Num 1)] in
               the common literal-bound case. *)
            let* ub_excl =
              match ub_lit with
              | Some ub -> Ok (Printf.sprintf "(NExp.Num %d)" (ub + 1))
              | None ->
                  let* ub_s = nexp_to_coq r.upper_bound in
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
            [Variable m : nat.]. Default array filter: global memory
            (mirrors [Rel_cost.Metric.UncoalescedAccesses]). *)
    | ActiveThreads
        (** [Warp.Metric.CountEnabled.T] — number of enabled threads
            in the warp (address-independent). No parameters. Default
            array filter: none (applies to any memory type). *)
    | Parametric
        (** Abstract metric, surfaced as a section [Variable Met :
            Metric.T.]. The kernel becomes generic in the metric;
            specialization (to [MemReads.T m], [CountEnabled.T],
            [Tick.T], …) happens at the proof site. Default array
            filter: none — no metric is committed to, so no memory
            class is assumed. Pair with [--memory] to filter
            explicitly. *)

  let to_string : t -> string = function
    | MemReads -> "mem-reads"
    | ActiveThreads -> "active"
    | Parametric -> "parametric"

  let choices : (string * t) list =
    [
      ("mem-reads", MemReads);
      ("active", ActiveThreads);
      ("parametric", Parametric);
    ]

  (** All metric variants are hoisted into a single section-level
      binding named [M] of type [Metric.T] (see {!preamble_lines});
      [Access] then reads as [Access M <index>] regardless of which
      metric was selected. *)
  let to_coq_expr : t -> string = fun _ -> "M"

  (** Section preamble for the chosen metric:
      - [MemReads] introduces [Variable m : nat.] for the sector size
        and [Let M := MemReads.T m.] for the [Metric.T] binding;
      - [ActiveThreads] is concrete and parameter-free, so just
        [Let M := Metric.CountEnabled.T.];
      - [Parametric] leaves [M] abstract via [Variable M : Metric.T.]
        — the kernel becomes generic in the metric and is specialized
        at the proof site. *)
  let preamble_lines : t -> Indent.t list =
    let open Indent in
    function
    | MemReads ->
        [
          Line "(* MemReads metric, parameterized by sector size [m]. *)";
          Line "Variable m : nat.";
          Line "Let M : Metric.T := MemReads.T m.";
          Line "";
        ]
    | ActiveThreads ->
        [
          Line "(* Address-independent count of enabled threads. *)";
          Line "Let M : Metric.T := Metric.CountEnabled.T.";
          Line "";
        ]
    | Parametric ->
        [
          Line "(* Abstract metric — specialize at the proof site. *)";
          Line "Variable M : Metric.T.";
          Line "";
        ]

  (** Metric-specific [Require …] imports. The shared base
      ([NExp], [BExp], [RExp], [Ident], [Dim], [ProtoLet]) is in
      [common_header]; this list adds only what the chosen metric
      pulls in. *)
  let extra_imports : t -> string list = function
    | MemReads -> [ "Require Warp.MemReads." ]
    | ActiveThreads -> [ "Require Warp.Metric." ]
    | Parametric -> [ "Require Warp.Metric." ]
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
    cond : cond:string -> Indent.t list -> Indent.t list;
        (** Single-branch conditional ([Cond] constructor). Used when
            an [If] has a [Skip] arm — emits the surviving branch under
            the (possibly negated) condition. *)
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
    for_mul :
      var:string -> lb:string -> count:int -> Indent.t list -> Indent.t list;
        (** Base-2 ascending [for_mul]. [lb] is a pre-translated
            [NExp.t]; [count] is a literal nat. Body sees
            [lb * 2^var]. *)
    for_div :
      var:string -> ub:string -> count:int -> Indent.t list -> Indent.t list;
        (** Base-2 descending [for_div]. [ub] is a pre-translated
            [NExp.t]; [count] is a literal nat. Body sees
            [ub / 2^var]. *)
  }

  (** Bare [Inductive] constructor backend.

      Every compound output is wrapped in a single set of parens so
      the traversal never has to worry about precedence. *)
  let constructor : t =
    let open Indent in
    {
      name = "constructor";
      extra_preamble = [];
      skip = [ Line "Skip" ];
      access =
        (fun ~metric ~index ->
          [ Line (Printf.sprintf "(Access %s %s)" metric index) ]);
      seq = (fun p q -> [ Line "(Seq"; Block p; Block q; Line ")" ]);
      ite =
        (fun ~cond p q ->
          [ Line (Printf.sprintf "(ite %s" cond); Block p; Block q; Line ")" ]);
      cond =
        (fun ~cond body ->
          [ Line (Printf.sprintf "(Cond %s" cond); Block body; Line ")" ]);
      (* [var] points at the section-level
         [Let <var> : Ident.t := Ident.make N.] binding emitted by
         {!from_kernel}. The body uses bare [<var>] at every site,
         which resolves to either the lambda parameter (for_-family,
         shadowing the [Ident.t] in scope with an [NExp.t] of the
         same name) or — for unit-stride [Loop] — to the [Ident.t]
         binding, lifted to [NExp.t] by the [NExp.Var] coercion. *)
      loop =
        (fun ~var ~lb ~ub body ->
          [
            Line
              (Printf.sprintf "(Loop (RExp.make %s %s %s)" var lb ub);
            Block body;
            Line ")";
          ]);
      for_ =
        (fun ~var ~lb ~ub_excl ~stride body ->
          [
            Line
              (Printf.sprintf "(for_ %s %s %s %d (fun %s =>" var lb
                 ub_excl stride var);
            Block body;
            Line "))";
          ]);
      for_mul =
        (fun ~var ~lb ~count body ->
          [
            Line
              (Printf.sprintf "(for_mul %s %s %d (fun %s =>" var lb count
                 var);
            Block body;
            Line "))";
          ]);
      for_div =
        (fun ~var ~ub ~count body ->
          [
            Line
              (Printf.sprintf "(for_div %s %s %d (fun %s =>" var ub count
                 var);
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

(** [Code.t] nodes that translate to [ProtoLet.Skip] and can therefore
    be elided when they appear as a [Seq] or [If] arm. Recursive so
    that wrappers introduced by inference — [Decl] around a Skip body,
    [Seq] of two Skip-equivalent arms, etc. — are also recognized.
    [Access] is never unit. [Loop] is unit when the body is. *)
let rec is_unit_code : Code.t -> bool = function
  | Skip | Sync _ -> true
  | Access _ -> false
  | Decl { body; _ } -> is_unit_code body
  | Seq (p, q) -> is_unit_code p && is_unit_code q
  | If (_, p, q) -> is_unit_code p && is_unit_code q
  | Loop { body; _ } -> is_unit_code body

let rec code_to_s (cfg : config) (c : Code.t) : (Indent.t list, error) Result.t
    =
  let ( let* ) = Result.bind in
  let s = cfg.syntax in
  match c with
  | Skip | Sync _ -> Ok s.skip
  | Access a ->
      let* idx =
        match a.index with
        | [ i ] -> nexp_to_coq i
        | _ ->
            Error
              (Printf.sprintf
                 "multi-dimensional access not supported (linearize first): %s"
                 (Access.to_string a))
      in
      Ok (s.access ~metric:cfg.metric ~index:idx)
  | Seq (p, q) -> (
      (* [Code.opt] uses a smart [seq] that already collapses [Skip]
         operands, so this is mostly defensive — it also catches the
         [Sync ;; q] case (we render [Sync] as [Skip]). *)
      match (is_unit_code p, is_unit_code q) with
      | true, true -> Ok s.skip
      | true, false -> code_to_s cfg q
      | false, true -> code_to_s cfg p
      | false, false ->
          let* p_s = code_to_s cfg p in
          let* q_s = code_to_s cfg q in
          Ok (s.seq p_s q_s))
  | If (b, p, q) -> (
      let* b_s = bexp_to_coq b in
      (* [If b p Skip] simplifies to [Cond b p] (the single-branch
         conditional constructor); [If b Skip q] symmetrically to
         [Cond (neg b) q]. Avoids an [ite]-expanded [Seq (Cond b p)
         (Cond (neg b) Skip)] with a redundant trailing arm. *)
      match (is_unit_code p, is_unit_code q) with
      | true, true -> Ok s.skip
      | false, true ->
          let* p_s = code_to_s cfg p in
          Ok (s.cond ~cond:b_s p_s)
      | true, false ->
          let* q_s = code_to_s cfg q in
          let neg_b = Printf.sprintf "(BExp.neg %s)" b_s in
          Ok (s.cond ~cond:neg_b q_s)
      | false, false ->
          let* p_s = code_to_s cfg p in
          let* q_s = code_to_s cfg q in
          Ok (s.ite ~cond:b_s p_s q_s))
  | Loop { range; body } -> (
      let* form = classify_range range in
      let* body_s = code_to_s cfg body in
      match form with
      | Empty_loop -> Ok s.skip
      | Unit_stride { var; lb; ub } -> Ok (s.loop ~var ~lb ~ub body_s)
      | Strided { var; lb; ub_excl; stride } ->
          Ok (s.for_ ~var ~lb ~ub_excl ~stride body_s)
      | For_mul { var; lb; count } -> Ok (s.for_mul ~var ~lb ~count body_s)
      | For_div { var; ub; count } -> Ok (s.for_div ~var ~ub ~count body_s))
  | Decl { body; _ } -> code_to_s cfg body

(** {1 Kernel translation} *)

(** Imports needed regardless of the chosen metric. Metric-specific
    [Require Warp.MemReads]/[Require Warp.Metric] live in
    {!Metric.extra_imports} so the output only pulls in what the
    selected metric actually uses. *)
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
    "Require Import Warp.ProtoLet.";
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
    - A loop binder always gets a [Let _id] declaration (it's the
      [Ident.t] argument to [RExp.make] / [for_] / [for_mul] /
      [for_div]).
    - [Decl] binders survive only when the body still references
      them (we drop the [Decl] wrapper but inherit any free [Var x]).
    - Free expression variables in code, ranges, and [pre] are
      always added.

    [threadIdx.x] is excluded — it renders as [NExp.Tid]. The result
    feeds {!from_kernel}, which partitions it into thread-uniform
    parameters (rendered as [nat]) and the rest (rendered as
    [Ident.t]). *)
let used_idents (k : Kernel.t) : Variable.t list =
  let nexp acc e = Exp.n_free_names e acc in
  let bexp acc b = Exp.b_free_names b acc in
  let rec on_code (acc : Variable.Set.t) (c : Code.t) : Variable.Set.t =
    match c with
    | Skip | Sync _ -> acc
    | Access a -> List.fold_left nexp acc a.index
    | Seq (p, q) -> on_code (on_code acc p) q
    | If (b, p, q) -> on_code (on_code (bexp acc b) p) q
    | Loop { range; body } ->
        let acc = Variable.Set.add range.var acc in
        let acc = nexp acc range.lower_bound in
        let acc = nexp acc range.upper_bound in
        on_code acc body
    | Decl { body; _ } -> on_code acc body
  in
  let acc = on_code Variable.Set.empty k.code in
  let acc = bexp acc k.pre in
  let acc = Variable.Set.remove Variable.tid_x acc in
  Variable.Set.elements acc

let from_kernel ?(syntax = Syntax.constructor) ?(metric = Metric.MemReads)
    (k : Kernel.t) : (Indent.t list, error) Result.t =
  let ( let* ) = Result.bind in
  let cfg = make_config ~syntax ~metric:(Metric.to_coq_expr metric) () in
  let* body = code_to_s cfg k.code in
  let globals_in_kernel = Params.to_set k.global_variables in
  let used = used_idents k |> Variable.Set.of_list in
  let used_nats = Variable.Set.inter used globals_in_kernel in
  let used_locals = Variable.Set.diff used globals_in_kernel in
  let mod_name = sanitize_module_name k.name in
  (* Each [Variable.t] gets two section-level bindings — an
     underlying-typed declaration and an [NExp.t] alias of the same
     name as the source variable. The traversal in [code_to_s] always
     emits the bare sanitized name, so [Var x] resolves to the alias
     (or to a [for_]-family lambda parameter of the same name when
     applicable). *)
  (* Globals are declared as [nat]; the section-level coercion
     [NExp.Num : nat >-> NExp.t] handles the lifting at use sites. *)
  let global_decls =
    used_nats |> Variable.Set.elements
    |> List.map (fun v ->
           let n = sanitize_name (Variable.name v) in
           Indent.Line (Printf.sprintf "Variable %s : nat." n))
  in
  let global_decls =
    if global_decls = [] then []
    else
      [ Indent.Line "(* Thread-uniform parameters (kernel globals). *)" ]
      @ global_decls @ [ Indent.Line "" ]
  in
  (* Locals are declared as [Ident.t]; the section-level coercion
     [NExp.Var : Ident.t >-> NExp.t] lifts them to expression form
     where needed. Loop binders ([RExp.make], [for_], ...) take
     [Ident.t] directly, so they consume the binding unchanged. *)
  let local_decls =
    used_locals |> Variable.Set.elements
    |> List.mapi (fun i v -> (v, i))
    |> List.map (fun (v, i) ->
           let n = sanitize_name (Variable.name v) in
           Indent.Line
             (Printf.sprintf "Let %s : Ident.t := Ident.make %d." n i))
  in
  let local_decls =
    if local_decls = [] then []
    else
      [ Indent.Line "(* Thread-local identifiers from the kernel. *)" ]
      @ local_decls @ [ Indent.Line "" ]
  in
  let open Indent in
  Ok
    [
      Line ("Module " ^ mod_name ^ ".");
      Block
        ([
           Line "Section Def.";
           Block
             ([
                Line "Context {D : Dim.T}.";
                Line "";
                Line
                  "(* Lift kernel-globals (nat) and locals (Ident.t)";
                Line "   into NExp.t at use sites. *)";
                Line "Local Coercion NExp.Num : nat >-> NExp.t.";
                Line "Local Coercion NExp.Var : Ident.t >-> NExp.t.";
                Line "";
                Line
                  "(* Disambiguating aliases: [NExp.Bin]/[BExp.Bin] share a";
                Line
                  "   name, and [ROp.{Eq,Lt,Gt}] collide with";
                Line "   [Datatypes.comparison]. *)";
                Line "Local Notation NBin := NExp.Bin (only parsing).";
                Line "Local Notation BBin := BExp.Bin (only parsing).";
                Line "Local Notation NEq  := ROp.Eq   (only parsing).";
                Line "Local Notation NNeq := ROp.Neq  (only parsing).";
                Line "Local Notation NLt  := ROp.Lt   (only parsing).";
                Line "Local Notation NLe  := ROp.Le   (only parsing).";
                Line "Local Notation NGt  := ROp.Gt   (only parsing).";
                Line "Local Notation NGe  := ROp.Ge   (only parsing).";
                Line "";
              ]
             @ Metric.preamble_lines metric @ global_decls @ local_decls
             @ [ Line "Let kernel : ProtoLet.t :=" ]
             @ [ Block body ] @ [ Line "." ]);
           Line "End Def.";
         ]);
      Line ("End " ^ mod_name ^ ".");
    ]

(** [include_imports = false] suppresses both the common
    [Require …] header and the backend's [extra_preamble]. The
    generating-tool comment is still emitted so the output's
    provenance survives a copy-paste into another file.

    [extra_header_info] is a list of [(key, value)] pairs that the
    caller (typically the binary) uses to surface effective filter
    choices in the header — e.g.
    [[("memory", "global"); ("mode", "all")]]. [backend] and
    [metric] are pre-populated automatically. *)
let from_kernels ?(syntax = Syntax.constructor) ?(metric = Metric.MemReads)
    ?(include_imports = true) ?(extra_header_info : (string * string) list = [])
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
  let info =
    [ ("backend", syntax.name); ("metric", Metric.to_string metric) ]
    @ extra_header_info
  in
  let pad_to =
    info |> List.map (fun (k, _) -> String.length k) |> List.fold_left max 0
  in
  let header_lines =
    Indent.Line "(* Generated by faial-to-rocq."
    :: List.map
         (fun (k, v) ->
           let padding = String.make (pad_to - String.length k) ' ' in
           Indent.Line (Printf.sprintf "   - %s:%s %s" k padding v))
         info
    @ [ Indent.Line "*)" ]
  in
  let preamble =
    if include_imports then
      List.map (fun s -> Indent.Line s) common_header
      @ List.map (fun s -> Indent.Line s) (Metric.extra_imports metric)
      @ List.map (fun s -> Indent.Line s) syntax.extra_preamble
      @ [ Indent.Line "" ]
    else []
  in
  Ok (header_lines @ preamble @ mods)

let to_string (l : Indent.t list) : string = Indent.to_string l
