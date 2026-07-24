open Stage0
open Common
open Exp

type 'a codegen = { codegen_arg : string; codegen_body : 'a }

(* [body] takes the full argument list. Every entry currently registered
   in [all] is unary, but [bvumul_noovfl] and any future overflow-style
   guards take more than one [nexp]; representing the body as
   [nexp list -> bexp] keeps the registry uniform. *)
type t = { name : string; body : nexp list -> bexp }

let pred_to_codegen (pred : t) : bexp codegen =
  {
    codegen_arg = "x";
    codegen_body = pred.body [ Var (Variable.from_name "x") ];
  }

let all : t list =
  let unary (f : nexp -> bexp) : nexp list -> bexp = function
    | [ x ] -> f x
    | _ -> failwith "predicate expects a single argument"
  in
  let mk_uint size : t =
    {
      name = "uint" ^ string_of_int size;
      body =
        unary (fun x -> n_le x (Num (Common.pow ~base:2 size - 1)));
    }
  in
  let pow ~base : t =
    { name = "pow" ^ string_of_int base;
      body = unary (Range.pow ~base) }
  in
  let bvumul_noovfl : t =
    let max_unsigned = 1 lsl 32 in
    { name = "bvumul_noovfl";
      body = (function
        | [ Num k1; Num k2 ] -> Bool (k1 >= 0 && k2 >= 0 && k1 * k2 < max_unsigned)
        | [ _; _ ] as args -> Pred ("bvumul_noovfl", args)
        | _ -> failwith "bvumul_noovfl: expects exactly 2 arguments") }
  in
  (* Signedness-safety guard for the abductive pool. Lowers to
     [v >= 0] under signed comparison; kept as a named predicate so
     the assumes dump reads [nonneg(Win)] rather than [Win >= 0],
     communicating that the guard exists for signed-negative
     reinterpretation in the BV gate, not a launch-shape claim the
     user is meant to know. The guard is operationally vacuous
     because kernel-size parameters are non-negative at runtime, but
     mandatory at the SMT layer when the pool emits [v >=u rhs] over
     a signed [v]: without it, the BV gate accepts models where [v]
     is signed-negative and its unsigned reinterpretation
     ([0xFFFFFFFE...]) is trivially [>=u] any small RHS. The named-
     predicate form also keeps the lowering and any future encoder
     special-casing here in one place, parallel to [bvumul_noovfl]
     above. *)
  let nonneg : t =
    { name = "nonneg";
      body = (function
        | [ v ] -> NRel (Ge Signedness.Signed, v, Num 0)
        | _ -> failwith "nonneg: expects exactly 1 argument") }
  in
  (* C-call recogniser: [d_to_imp] lifts a call to this name into a
     [Pred] node whose body lowers at [b_inline] time. The thread
     uniformity intrinsics are not registered here: they are parsed
     straight into [IsThreadUnif], so no predicate body can rebuild a
     cross-thread primitive downstream of [strip_cross_thread]. *)
  let is_pow2 : t =
    { name = "__is_pow2";
      body = (function
        | [ n ] -> Pred ("pow2", [ n ])
        | _ -> failwith "__is_pow2: expects exactly 1 argument") }
  in
  [ pow ~base:2; pow ~base:3; mk_uint 32; mk_uint 16; mk_uint 8;
    bvumul_noovfl; nonneg; is_pow2 ]

let all_db : t StringMap.t =
  List.fold_left (fun m (p : t) -> StringMap.add p.name p m) StringMap.empty all

let call_opt (name : string) (ns : nexp list) : bexp option =
  StringMap.find_opt name all_db |> Option.map (fun (p : t) -> p.body ns)

let supported (name : string) : bool = StringMap.mem name all_db

(* Back-compat alias kept until external callers migrate. *)
let pred_call_opt = call_opt

let get_predicates (b : bexp) : t list =
  let rec get_names_b (b : bexp) (preds : StringSet.t) : StringSet.t =
    match b with
    | Pred (x, _) -> StringSet.add x preds
    | BRel (_, b1, b2) -> get_names_b b1 preds |> get_names_b b2
    | BNot b -> get_names_b b preds
    | NRel (_, n1, n2) -> get_names_n n1 preds |> get_names_n n2
    | Bool _ -> preds
    | CastBool e -> get_names_n e preds
    | Distinct exprs ->
        List.fold_left (fun acc expr -> get_names_n expr acc) preds exprs
    | AtomicResult { operation; index; _ } ->
        let preds =
          List.fold_left (fun acc e -> get_names_n e acc) preds index
        in
        Atomic.Operation.fold (fun e acc -> get_names_n e acc) operation preds
    | IsThreadUnif e -> get_names_n e preds
  and get_names_n (n : nexp) (ns : StringSet.t) : StringSet.t =
    match n with
    | Var _ | Num _ -> ns
    | Binary (_, n1, n2) -> get_names_n n1 ns |> get_names_n n2
    | NIf (b, n1, n2) -> get_names_b b ns |> get_names_n n1 |> get_names_n n2
    | NCall (_, ns') ->
        List.fold_left (fun acc n -> get_names_n n acc) ns ns'
    | Unary (_, n) -> get_names_n n ns
    | CastInt b -> get_names_b b ns
  in
  get_names_b b StringSet.empty
  |> StringSet.elements
  (* Predicate names not registered in [all_db] (e.g. [bvumul_noovfl],
     handled directly by the BV encoder) are skipped: [get_predicates]
     is consumed by codegen passes that need the inline body, so an
     absent body means "this predicate is opaque to the codegen". *)
  |> List.filter_map (fun n -> StringMap.find_opt n all_db)

let rec n_inline : nexp -> nexp = function
  | (Var _ | Num _) as n -> n
  | NCall (x, args) -> NCall (x, List.map n_inline args)
  | CastInt b -> CastInt (b_inline b)
  | Unary (o, e) -> Unary (o, n_inline e)
  | Binary (o, n1, n2) -> Binary (o, n_inline n1, n_inline n2)
  | NIf (b, n1, n2) -> NIf (b_inline b, n_inline n1, n_inline n2)

and b_inline : bexp -> bexp = function
  | Pred (x, ns) as p_orig ->
      let inlined = List.map n_inline ns in
      (* Predicates registered with a body inline to their body. Names
         not in the database (e.g. [bvumul_noovfl], which the BV encoder
         consumes directly via [mk_mul_no_overflow]) pass through with
         their arguments inlined. *)
      (match StringMap.find_opt x all_db with
       | Some p -> p.body inlined
       | None -> if inlined = ns then p_orig else Pred (x, inlined))
  | Bool _ as b -> b
  | CastBool e -> CastBool (n_inline e)
  | BNot b -> BNot (b_inline b)
  | NRel (o, n1, n2) -> NRel (o, n_inline n1, n_inline n2)
  | BRel (o, b1, b2) -> BRel (o, b_inline b1, b_inline b2)
  | Distinct exprs -> Distinct (List.map n_inline exprs)
  | AtomicResult { target; array; index; operation } ->
      AtomicResult
        {
          target;
          array;
          index = List.map n_inline index;
          operation = Atomic.Operation.map n_inline operation;
        }
  | IsThreadUnif e -> IsThreadUnif (n_inline e)

(* Replaces every cross-thread primitive ([AtomicResult],
   [IsThreadUnif]) with a fresh stable boolean encoded as
   [CastBool (Var "@<kind>:...")]. Each distinct primitive maps
   to the same variable so occurrences stay correlated within a
   query; distinct primitives stay independent. Used by
   single-thread analyses (genie's reachability) and by symbexp
   on goals where the cross-thread axiom is conjoined separately;
   in both cases Z3 picks the boolean freely. *)
let strip_cross_thread : bexp -> bexp =
  let fresh_atomic (target : Variable.t)
      (operation : nexp Atomic.Operation.t) : nexp =
    let operands =
      Atomic.Operation.to_list operation
      |> List.filter_map (Option.map n_to_string)
      |> String.concat ","
    in
    let name =
      "@atomic_result:" ^ Variable.name target ^ ":"
      ^ Atomic.Operation.to_string operation
      ^ if operands = "" then "" else "(" ^ operands ^ ")"
    in
    Var (Variable.from_name name)
  in
  let fresh_thread_unif (e : nexp) : nexp =
    Var (Variable.from_name ("@thread_unif:" ^ n_to_string e))
  in
  let rec rn (n : nexp) : nexp =
    match n with
    | Var _ | Num _ -> n
    | CastInt b -> CastInt (rb b)
    | Unary (o, e) -> Unary (o, rn e)
    | Binary (o, n1, n2) -> Binary (o, rn n1, rn n2)
    | NIf (b, n1, n2) -> NIf (rb b, rn n1, rn n2)
    | NCall (x, args) -> NCall (x, List.map rn args)
  and rb (b : bexp) : bexp =
    match b with
    | Bool _ -> b
    | NRel (o, n1, n2) -> NRel (o, rn n1, rn n2)
    | BRel (o, b1, b2) -> BRel (o, rb b1, rb b2)
    | BNot b -> BNot (rb b)
    | Pred (x, ns) -> Pred (x, List.map rn ns)
    | CastBool n -> CastBool (rn n)
    | Distinct ns -> Distinct (List.map rn ns)
    | AtomicResult { target; operation; _ } ->
        CastBool (fresh_atomic target operation)
    | IsThreadUnif e -> CastBool (fresh_thread_unif e)
  in
  rb
