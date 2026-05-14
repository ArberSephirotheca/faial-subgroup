open Stage0
open Common
open Exp

type 'a codegen = { codegen_arg : string; codegen_body : 'a }

(* [pred_body] takes the full argument list. Every predicate currently
   registered in [all_predicates] is unary, but [bvumul_noovfl] and any
   future overflow-style guards take more than one [nexp]; representing
   the body as [nexp list -> bexp] keeps the registry uniform. *)
type t = { pred_name : string; pred_body : nexp list -> bexp }

let pred_to_codegen (pred : t) : bexp codegen =
  {
    codegen_arg = "x";
    codegen_body = pred.pred_body [ Var (Variable.from_name "x") ];
  }

let all_predicates : t list =
  let unary (f : nexp -> bexp) : nexp list -> bexp = function
    | [ x ] -> f x
    | _ -> failwith "predicate expects a single argument"
  in
  let mk_uint size : t =
    {
      pred_name = "uint" ^ string_of_int size;
      pred_body =
        unary (fun x -> n_le x (Num (Common.pow ~base:2 size - 1)));
    }
  in
  let pow ~base : t =
    { pred_name = "pow" ^ string_of_int base;
      pred_body = unary (Range.pow ~base) }
  in
  (* Unsigned BV32 multiplication no-overflow check. The Bv64Gen and
     SignedBitVectorOps encoders consume [Pred ("bvumul_noovfl", ...)]
     directly via [mk_umul_no_overflow]; this body folds the case
     where both arguments are integer literals, so [b_inline] and
     [constfold] can collapse [bvumul_noovfl(c, d)] with both [c] and
     [d] [Num] to a [Bool] decision before Z3 sees it. When one
     argument is symbolic the body reconstructs the [Pred] with its
     inlined arguments, leaving the BV encoder to handle the
     no-overflow semantics. *)
  let bvumul_noovfl : t =
    let max_unsigned = 1 lsl 32 in
    { pred_name = "bvumul_noovfl";
      pred_body = (function
        | [ Num k1; Num k2 ] -> Bool (k1 >= 0 && k2 >= 0 && k1 * k2 < max_unsigned)
        | [ _; _ ] as args -> Pred ("bvumul_noovfl", args)
        | _ -> failwith "bvumul_noovfl: expects exactly 2 arguments") }
  in
  [ pow ~base:2; pow ~base:3; mk_uint 32; mk_uint 16; mk_uint 8; bvumul_noovfl ]

let make_pred_db (l : t list) : (string, t) Hashtbl.t =
  List.map (fun p -> (p.pred_name, p)) l |> Common.hashtbl_from_list

let all_predicates_db : (string, t) Hashtbl.t = make_pred_db all_predicates

let pred_call_opt (name : string) (ns : nexp list) : bexp option =
  match Hashtbl.find_opt all_predicates_db name with
  | Some p -> Some (p.pred_body ns)
  | None -> None

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
  and get_names_n (n : nexp) (ns : StringSet.t) : StringSet.t =
    match n with
    | Var _ | Num _ -> ns
    | Binary (_, n1, n2) -> get_names_n n1 ns |> get_names_n n2
    | NIf (b, n1, n2) -> get_names_b b ns |> get_names_n n1 |> get_names_n n2
    | NCall (_, n) | Other n | Unary (_, n) -> get_names_n n ns
    | CastInt b -> get_names_b b ns
  in
  get_names_b b StringSet.empty
  |> StringSet.elements
  (* Predicate names not registered in [all_predicates_db] (e.g.
     [bvumul_noovfl], handled directly by the BV encoder) are skipped:
     [get_predicates] is consumed by codegen passes that need the
     inline body, so an absent body means "this predicate is opaque
     to the codegen". *)
  |> List.filter_map (Hashtbl.find_opt all_predicates_db)

let rec n_inline : nexp -> nexp = function
  | (NCall _ | Var _ | Num _) as n -> n
  | CastInt b -> CastInt (b_inline b)
  | Other e -> Other (n_inline e)
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
      (match Hashtbl.find_opt all_predicates_db x with
       | Some p -> p.pred_body inlined
       | None -> if inlined = ns then p_orig else Pred (x, inlined))
  | Bool _ as b -> b
  | CastBool e -> CastBool (n_inline e)
  | BNot b -> BNot (b_inline b)
  | NRel (o, n1, n2) -> NRel (o, n_inline n1, n_inline n2)
  | BRel (o, b1, b2) -> BRel (o, b_inline b1, b_inline b2)
  | Distinct exprs -> Distinct (List.map n_inline exprs)
