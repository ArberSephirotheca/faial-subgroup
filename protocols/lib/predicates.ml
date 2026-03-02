open Stage0
open Common
open Exp

type 'a codegen = { codegen_arg : string; codegen_body : 'a }
type t = { pred_name : string; pred_body : nexp -> bexp }

let pred_to_codegen (pred : t) : bexp codegen =
  {
    codegen_arg = "x";
    codegen_body = pred.pred_body (Var (Variable.from_name "x"));
  }

let all_predicates : t list =
  let mk_uint size : t =
    {
      pred_name = "uint" ^ string_of_int size;
      pred_body = (fun x -> n_le x (Num (Common.pow ~base:2 size - 1)));
    }
  in
  let pow ~base : t =
    { pred_name = "pow" ^ string_of_int base; pred_body = Range.pow ~base }
  in
  [ pow ~base:2; pow ~base:3; mk_uint 32; mk_uint 16; mk_uint 8 ]

let make_pred_db (l : t list) : (string, t) Hashtbl.t =
  List.map (fun p -> (p.pred_name, p)) l |> Common.hashtbl_from_list

let all_predicates_db : (string, t) Hashtbl.t = make_pred_db all_predicates

let pred_call_opt (name : string) (n : nexp) : bexp option =
  match Hashtbl.find_opt all_predicates_db name with
  | Some p -> Some (p.pred_body n)
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
  |> List.map (Hashtbl.find all_predicates_db)

let rec n_inline : nexp -> nexp = function
  | (NCall _ | Var _ | Num _) as n -> n
  | CastInt b -> CastInt (b_inline b)
  | Other e -> Other (n_inline e)
  | Unary (o, e) -> Unary (o, n_inline e)
  | Binary (o, n1, n2) -> Binary (o, n_inline n1, n_inline n2)
  | NIf (b, n1, n2) -> NIf (b_inline b, n_inline n1, n_inline n2)

and b_inline : bexp -> bexp = function
  | Pred (x, n) ->
      let p = Hashtbl.find all_predicates_db x in
      p.pred_body (n_inline n)
  | Bool _ as b -> b
  | CastBool e -> CastBool (n_inline e)
  | BNot b -> BNot (b_inline b)
  | NRel (o, n1, n2) -> NRel (o, n_inline n1, n_inline n2)
  | BRel (o, b1, b2) -> BRel (o, b_inline b1, b_inline b2)
  | Distinct exprs -> Distinct (List.map n_inline exprs)
