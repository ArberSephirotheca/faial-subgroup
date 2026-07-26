open Exp

type subst = nexp Variable.Map.t

let empty : subst = Variable.Map.empty

let ( let* ) (x : 'a Seq.t) (f : 'a -> 'b Seq.t) : 'b Seq.t = Seq.concat_map f x

let is_hole_name (name : string) : bool =
  String.length name > 0 && name.[0] = '?'

let split_last_dot (s : string) : (string * string) option =
  match String.rindex_opt s '.' with
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> None

let is_commutative (op : N_binary.t) : bool =
  let open N_binary in
  match op with
  | Plus _ | Mult _ | BitAnd | BitOr | BitXOr -> true
  | Minus _ | Div _ | Mod _ | LeftShift | RightShift _ -> false

let rec n_equal (a : nexp) (b : nexp) : bool =
  match (a, b) with
  | Var x, Var y -> Variable.equal x y
  | Num n, Num m -> n = m
  | Binary (o1, a1, a2), Binary (o2, b1, b2) ->
      o1 = o2 && n_equal a1 b1 && n_equal a2 b2
  | Unary (o1, a1), Unary (o2, b1) -> o1 = o2 && n_equal a1 b1
  | NCall (f, xs), NCall (g, ys) ->
      String.equal f g
      && List.length xs = List.length ys
      && List.for_all2 n_equal xs ys
  | NIf (c1, a1, a2), NIf (c2, b1, b2) ->
      b_equal c1 c2 && n_equal a1 b1 && n_equal a2 b2
  | CastInt c1, CastInt c2 -> b_equal c1 c2
  | Convert c1, Convert c2 -> Scalar.equal c1.ty c2.ty && n_equal c1.arg c2.arg
  | _, _ -> false

and b_equal (a : bexp) (b : bexp) : bool =
  match (a, b) with
  | Bool x, Bool y -> x = y
  | NRel (o1, a1, a2), NRel (o2, b1, b2) ->
      o1 = o2 && n_equal a1 b1 && n_equal a2 b2
  | BRel (o1, a1, a2), BRel (o2, b1, b2) ->
      o1 = o2 && b_equal a1 b1 && b_equal a2 b2
  | BNot a1, BNot b1 -> b_equal a1 b1
  | Pred (f, xs), Pred (g, ys) ->
      String.equal f g
      && List.length xs = List.length ys
      && List.for_all2 n_equal xs ys
  | CastBool a1, CastBool b1 -> n_equal a1 b1
  | Distinct xs, Distinct ys ->
      List.length xs = List.length ys && List.for_all2 n_equal xs ys
  | IsThreadUnif a1, IsThreadUnif b1 -> n_equal a1 b1
  | _, _ -> false

type hole =
  | Plain of Variable.t
  | Field of Variable.t * string
  | Literal

let classify (v : Variable.t) : hole =
  let name = Variable.name v in
  if not (is_hole_name name) then Literal
  else
    match split_last_dot name with
    | Some (base, field) -> Field (Variable.from_name base, field)
    | None -> Plain v

let bind (key : Variable.t) (value : nexp) (s : subst) : subst Seq.t =
  match Variable.Map.find_opt key s with
  | Some bound -> if n_equal bound value then Seq.return s else Seq.empty
  | None -> Seq.return (Variable.Map.add key value s)

let rec match_nexp (pat : nexp) (subject : nexp) (s : subst) : subst Seq.t =
  match pat with
  | Var v -> (
      match classify v with
      | Plain key -> bind key subject s
      | Literal -> (
          match subject with
          | Var sv when Variable.equal v sv -> Seq.return s
          | _ -> Seq.empty)
      | Field (key, field) -> (
          match subject with
          | Var sv -> (
              match split_last_dot (Variable.name sv) with
              | Some (base, f) when String.equal f field ->
                  bind key (Var (Variable.from_name base)) s
              | _ -> Seq.empty)
          | _ -> Seq.empty))
  | Num n -> (
      match subject with Num m when n = m -> Seq.return s | _ -> Seq.empty)
  | Binary (op, p1, p2) -> (
      match subject with
      | Binary (op', s1, s2) when op = op' ->
          let in_order =
            let* s = match_nexp p1 s1 s in
            match_nexp p2 s2 s
          in
          if is_commutative op then
            let swapped =
              let* s = match_nexp p1 s2 s in
              match_nexp p2 s1 s
            in
            Seq.append in_order swapped
          else in_order
      | _ -> Seq.empty)
  | Unary (op, p) -> (
      match subject with
      | Unary (op', s1) when op = op' -> match_nexp p s1 s
      | _ -> Seq.empty)
  | NCall (name, pargs) -> (
      match subject with
      | NCall (name', sargs)
        when String.equal name name' && List.length pargs = List.length sargs ->
          match_list pargs sargs s
      | _ -> Seq.empty)
  | NIf (pb, p1, p2) -> (
      match subject with
      | NIf (sb, s1, s2) when b_equal pb sb ->
          let* s = match_nexp p1 s1 s in
          match_nexp p2 s2 s
      | _ -> Seq.empty)
  | ReadResult pr -> (
      match subject with
      | ReadResult sr
        when Variable.equal pr.array sr.array && pr.version = sr.version ->
          match_list pr.args sr.args s
      | _ -> Seq.empty)
  | Convert pc -> (
      match subject with
      | Convert sc when Scalar.equal pc.ty sc.ty -> match_nexp pc.arg sc.arg s
      | _ -> Seq.empty)
  | CastInt pb -> (
      match subject with
      | CastInt sb when b_equal pb sb -> Seq.return s
      | _ -> Seq.empty)

and match_list (pats : nexp list) (subjects : nexp list) (s : subst) :
    subst Seq.t =
  match (pats, subjects) with
  | [], [] -> Seq.return s
  | p :: ps, sub :: subs ->
      let* s = match_nexp p sub s in
      match_list ps subs s
  | _, _ -> Seq.empty

let matches (pat : nexp) (subject : nexp) : subst Seq.t =
  match_nexp pat subject empty

let rec instantiate (s : subst) (template : nexp) : nexp =
  match template with
  | Var v -> (
      match classify v with
      | Literal -> template
      | Plain key -> (
          match Variable.Map.find_opt key s with
          | Some n -> n
          | None -> failwith ("unbound hole " ^ Variable.name key))
      | Field (key, field) -> (
          match Variable.Map.find_opt key s with
          | Some (Var base) ->
              Var (Variable.update_name (fun n -> n ^ "." ^ field) base)
          | Some _ ->
              failwith
                ("field hole " ^ Variable.name key ^ " bound to a non-variable")
          | None -> failwith ("unbound hole " ^ Variable.name key)))
  | Num _ -> template
  | Binary (op, a, b) -> Binary (op, instantiate s a, instantiate s b)
  | Unary (op, a) -> Unary (op, instantiate s a)
  | NCall (name, args) -> NCall (name, List.map (instantiate s) args)
  | ReadResult r -> ReadResult { r with args = List.map (instantiate s) r.args }
  | Convert c -> Convert { c with arg = instantiate s c.arg }
  | NIf (b, a1, a2) ->
      NIf (instantiate_b s b, instantiate s a1, instantiate s a2)
  | CastInt b -> CastInt (instantiate_b s b)

and instantiate_b (s : subst) (template : bexp) : bexp =
  match template with
  | Bool _ -> template
  | NRel (op, a, b) -> NRel (op, instantiate s a, instantiate s b)
  | BRel (op, a, b) -> BRel (op, instantiate_b s a, instantiate_b s b)
  | BNot b -> BNot (instantiate_b s b)
  | Pred (name, args) -> Pred (name, List.map (instantiate s) args)
  | CastBool a -> CastBool (instantiate s a)
  | Distinct args -> Distinct (List.map (instantiate s) args)
  | IsThreadUnif a -> IsThreadUnif (instantiate s a)
  | AtomicResult _ -> template

type rule = {
  lhs : nexp;
  fire : subst -> subst option;
  rhs : nexp;
  emits : bexp list;
}

let apply_rule (r : rule) (subject : nexp) : (nexp * bexp list) option =
  matches r.lhs subject
  |> Seq.filter_map r.fire
  |> Seq.uncons
  |> Option.map (fun (s, _) ->
         (instantiate s r.rhs, List.map (instantiate_b s) r.emits))

let apply_rules (rules : rule list) (subject : nexp) :
    (nexp * bexp list) option =
  List.find_map (fun r -> apply_rule r subject) rules
