open Exp

let rec norm (b : bexp) : bexp list =
  match b with
  | Pred _
  | BNot (Pred _)
  | BRel (BOr, _, _)
  | Bool _
  | BNot (CastBool _)
  | NRel _ | Distinct _
  | BNot (Distinct _)
  | AtomicResult _
  | BNot (AtomicResult _)
  | IsThreadUnif _
  | BNot (IsThreadUnif _) ->
      [ b ]
  | BRel (BAnd, b1, b2) -> List.append (norm b1) (norm b2)
  | BNot (Bool b) -> [ Bool (not b) ]
  | BNot (BRel (BAnd, b1, b2)) -> norm (b_or (b_not b1) (b_not b2))
  | BNot (BRel (BOr, b1, b2)) -> norm (b_and (b_not b1) (b_not b2))
  | BNot (NRel (Eq, n1, n2)) -> norm (n_neq n1 n2)
  | BNot (NRel (Neq, n1, n2)) -> norm (n_eq n1 n2)
  | BNot (NRel (Gt Signed, n1, n2)) -> norm (n_le n1 n2)
  | BNot (NRel (Gt Unsigned, n1, n2)) -> norm (n_ule n1 n2)
  | BNot (NRel (Lt Signed, n1, n2)) -> norm (n_ge n1 n2)
  | BNot (NRel (Lt Unsigned, n1, n2)) -> norm (n_uge n1 n2)
  | BNot (NRel (Le Signed, n1, n2)) -> norm (n_gt n1 n2)
  | BNot (NRel (Le Unsigned, n1, n2)) -> norm (n_ugt n1 n2)
  | BNot (NRel (Ge Signed, n1, n2)) -> norm (n_lt n1 n2)
  | BNot (NRel (Ge Unsigned, n1, n2)) -> norm (n_ult n1 n2)
  | BNot (BNot b) -> norm b
  | CastBool (CastInt b) -> norm b
  | CastBool _ -> [ b ]

let bexp_to_bool b = match b with Bool b -> Some b | _ -> None
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let rec n_opt (a : nexp) : nexp =
  match a with
  | Var _ | Num _ -> a
  | NCall (x, args) ->
      let folded = List.map n_opt args in
      (* Symmetric to [b_opt]'s [Pred] arm below: delegate to the
         [Functions] registry, which lowers an entry that has a body
         and folds a call whose arguments are all literal, so [min(i,
         j)] becomes an [NIf] and [log2(8)] becomes [Num 3]. A [None]
         leaves [NCall (x, folded)] for the Z3 encoder to treat as a
         UF application. *)
      (match Functions.call_opt x folded with
       | Some n -> n
       | None -> NCall (x, folded))
  | ReadResult r -> ReadResult { r with args = List.map n_opt r.args }
  | Convert c -> convert c.ty (n_opt c.arg)
  | Unary (BitNot, e) -> n_bit_not (n_opt e)
  | Unary (Negate, e) -> n_uminus (n_opt e)
  | CastInt b -> cast_int (b_opt b)
  | NIf (b, n1, n2) ->
      let b = b_opt b in
      let n1 = n_opt n1 in
      let n2 = n_opt n2 in
      n_if b n1 n2
  | Binary (b, a1, a2) ->
      let a1 = n_opt a1 in
      let a2 = n_opt a2 in
      n_bin b a1 a2

and b_opt (e : bexp) : bexp =
  match e with
  | Pred (x, es) ->
      let folded = List.map n_opt es in
      (* Try to evaluate the predicate when every argument folded to a
         literal [Num]. Otherwise leave it with folded arguments. *)
      if List.for_all (function Num _ -> true | _ -> false) folded then
        (match Predicates.pred_call_opt x folded with
         | Some b -> b_opt b
         | None -> Pred (x, folded))
      else Pred (x, folded)
  | CastBool e -> e |> n_opt |> cast_bool
  | Bool _ -> e
  | BRel (b, b1, b2) -> b_rel b (b_opt b1) (b_opt b2)
  | NRel (o, a1, a2) -> n_rel o (n_opt a1) (n_opt a2)
  | BNot b -> b_not (b_opt b)
  | Distinct exprs -> Distinct (List.map n_opt exprs)
  | AtomicResult { target; array; index; operation } ->
      AtomicResult
        {
          target;
          array;
          index = List.map n_opt index;
          operation = Atomic.Operation.map n_opt operation;
        }
  | IsThreadUnif e -> IsThreadUnif (n_opt e)

let r_opt (r : Range.t) : Range.t =
  {
    r with
    lower_bound = n_opt r.lower_bound;
    upper_bound = n_opt r.upper_bound;
  }

let a_opt (a : Access.t) : Access.t = { a with index = List.map n_opt a.index }
