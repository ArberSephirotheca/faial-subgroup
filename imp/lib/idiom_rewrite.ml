open Protocols
open Exp
module EA = Encode_assigns

let rec rewrite_nexp (rules : Exp_match.rule list) (e : nexp) : nexp * bexp list
    =
  let e', es = rewrite_children rules e in
  match Exp_match.apply_rules rules e' with
  | Some (e2, es2) -> (e2, es @ es2)
  | None -> (e', es)

and rewrite_children (rules : Exp_match.rule list) (e : nexp) : nexp * bexp list
    =
  match e with
  | Var _ | Num _ -> (e, [])
  | Binary (op, a, b) ->
      let a', esa = rewrite_nexp rules a in
      let b', esb = rewrite_nexp rules b in
      ((if a' == a && b' == b then e else Binary (op, a', b')), esa @ esb)
  | Unary (op, a) ->
      let a', es = rewrite_nexp rules a in
      ((if a' == a then e else Unary (op, a')), es)
  | NCall (f, args) ->
      let args', es = rewrite_nexp_list rules args in
      ((if args' == args then e else NCall (f, args')), es)
  | NIf (b, a1, a2) ->
      let b', esb = rewrite_bexp rules b in
      let a1', es1 = rewrite_nexp rules a1 in
      let a2', es2 = rewrite_nexp rules a2 in
      ( (if b' == b && a1' == a1 && a2' == a2 then e else NIf (b', a1', a2')),
        esb @ es1 @ es2 )
  | CastInt b ->
      let b', es = rewrite_bexp rules b in
      ((if b' == b then e else CastInt b'), es)

and rewrite_bexp (rules : Exp_match.rule list) (b : bexp) : bexp * bexp list =
  match b with
  | Bool _ -> (b, [])
  | NRel (op, a1, a2) ->
      let a1', es1 = rewrite_nexp rules a1 in
      let a2', es2 = rewrite_nexp rules a2 in
      ((if a1' == a1 && a2' == a2 then b else NRel (op, a1', a2')), es1 @ es2)
  | BRel (op, b1, b2) ->
      let b1', es1 = rewrite_bexp rules b1 in
      let b2', es2 = rewrite_bexp rules b2 in
      ((if b1' == b1 && b2' == b2 then b else BRel (op, b1', b2')), es1 @ es2)
  | BNot b1 ->
      let b1', es = rewrite_bexp rules b1 in
      ((if b1' == b1 then b else BNot b1'), es)
  | Pred (f, args) ->
      let args', es = rewrite_nexp_list rules args in
      ((if args' == args then b else Pred (f, args')), es)
  | CastBool a ->
      let a', es = rewrite_nexp rules a in
      ((if a' == a then b else CastBool a'), es)
  | Distinct args ->
      let args', es = rewrite_nexp_list rules args in
      ((if args' == args then b else Distinct args'), es)
  | ThreadUnif a ->
      let a', es = rewrite_nexp rules a in
      ((if a' == a then b else ThreadUnif a'), es)
  | AtomicResult _ -> (b, [])

and rewrite_nexp_list (rules : Exp_match.rule list) (xs : nexp list) :
    nexp list * bexp list =
  match xs with
  | [] -> ([], [])
  | x :: rest ->
      let x', esx = rewrite_nexp rules x in
      let rest', esr = rewrite_nexp_list rules rest in
      ((if x' == x && rest' == rest then xs else x' :: rest'), esx @ esr)

let dedup (es : bexp list) : bexp list =
  List.fold_left
    (fun acc e ->
      if List.exists (Exp_match.b_equal e) acc then acc else e :: acc)
    [] es
  |> List.rev

let asserts (es : bexp list) : EA.t =
  dedup es
  |> List.map (fun e -> EA.Assert (Assert.make e Assert.Visibility.Global))
  |> List.fold_left EA.seq EA.Skip

let prepend (es : bexp list) (s : EA.t) : EA.t = EA.seq (asserts es) s

let rewrite_sync (rules : Exp_match.rule list) (sy : Sync.t) :
    Sync.t * bexp list =
  let id', es1 = rewrite_nexp rules sy.Sync.id in
  let participants', es2 =
    match sy.Sync.participants with
    | None -> (None, [])
    | Some p ->
        let p', es = rewrite_nexp rules p in
        ((if p' == p then sy.Sync.participants else Some p'), es)
  in
  let sy' =
    if id' == sy.Sync.id && participants' == sy.Sync.participants then sy
    else { sy with Sync.id = id'; participants = participants' }
  in
  (sy', es1 @ es2)

let rewrite_range (rules : Exp_match.rule list) (r : Range.t) :
    Range.t * bexp list =
  let lb', es1 = rewrite_nexp rules r.Range.lower_bound in
  let ub', es2 = rewrite_nexp rules r.Range.upper_bound in
  let r' =
    if lb' == r.Range.lower_bound && ub' == r.Range.upper_bound then r
    else { r with Range.lower_bound = lb'; upper_bound = ub' }
  in
  (r', es1 @ es2)

let rec rewrite_code (rules : Exp_match.rule list) (s : EA.t) : EA.t =
  match s with
  | EA.Skip -> s
  | EA.Access a ->
      let index', es = rewrite_nexp_list rules a.Access.index in
      let s' =
        if index' == a.Access.index then s
        else EA.Access { a with Access.index = index' }
      in
      prepend es s'
  | EA.Assert asrt ->
      let cond', es = rewrite_bexp rules asrt.Assert.cond in
      let s' =
        if cond' == asrt.Assert.cond then s
        else EA.Assert (Assert.map (fun _ -> cond') asrt)
      in
      prepend es s'
  | EA.Sync sy ->
      let sy', es = rewrite_sync rules sy in
      let s' = if sy' == sy then s else EA.Sync sy' in
      prepend es s'
  | EA.If (b, p, q) ->
      let b', es = rewrite_bexp rules b in
      let p' = rewrite_code rules p in
      let q' = rewrite_code rules q in
      let s' =
        if b' == b && p' == p && q' == q then s else EA.If (b', p', q')
      in
      prepend es s'
  | EA.For (r, body) ->
      let r', es = rewrite_range rules r in
      let body' = rewrite_code rules body in
      let s' = if r' == r && body' == body then s else EA.For (r', body') in
      prepend es s'
  | EA.Seq (p, q) ->
      let p' = rewrite_code rules p in
      let q' = rewrite_code rules q in
      if p' == p && q' == q then s else EA.seq p' q'
  | EA.Decl { var; ty; body } ->
      let body' = rewrite_code rules body in
      if body' == body then s else EA.Decl { var; ty; body = body' }

let rewrite (rules : Exp_match.rule list) (s : EA.t) : EA.t =
  match rules with [] -> s | _ -> rewrite_code rules s

module SA = Subst.Make (Subst.SubstAssoc)
module P = Protocols_parsing.Parsers

let ( let* ) = Result.bind

let un_dollar (name : string) : string option =
  if String.length name > 0 && name.[0] = '$' then
    Some ("?" ^ String.sub name 1 (String.length name - 1))
  else None

let rename_subst (vars : Variable.Set.t) : Subst.SubstAssoc.t =
  vars |> Variable.Set.elements
  |> List.filter_map (fun v ->
      match un_dollar (Variable.name v) with
      | Some q -> Some (Variable.name v, Exp.Var (Variable.from_name q))
      | None -> None)
  |> Subst.SubstAssoc.make

let rename_n (n : Exp.nexp) : Exp.nexp =
  SA.n_subst (rename_subst (Exp.n_free_names n Variable.Set.empty)) n

let rename_b (b : Exp.bexp) : Exp.bexp =
  SA.b_subst (rename_subst (Exp.b_free_names b Variable.Set.empty)) b

let hole_keys (vars : Variable.Set.t) : Variable.Set.t =
  vars |> Variable.Set.elements
  |> List.filter_map (fun v ->
      match Exp_match.classify v with
      | Exp_match.Plain k | Exp_match.Field (k, _) -> Some k
      | Exp_match.Literal -> None)
  |> Variable.Set.of_list

let split2 (sep : string) (s : string) : (string * string) option =
  let sl = String.length sep and n = String.length s in
  let rec find i =
    if i + sl > n then None
    else if String.sub s i sl = sep then
      Some (String.sub s 0 i, String.sub s (i + sl) (n - i - sl))
    else find (i + 1)
  in
  find 0

let parse_nexp (label : string) (s : string) : (Exp.nexp, string) result =
  match P.NExpParser.of_string (String.trim s) with
  | Ok n -> Ok (rename_n n)
  | Error e -> Error (label ^ ": " ^ e)

let parse_bexp (label : string) (s : string) : (Exp.bexp, string) result =
  match P.BExpParser.of_string (String.trim s) with
  | Ok b -> Ok (rename_b b)
  | Error e -> Error (label ^ ": " ^ e)

let parse_emits (s : string) : (Exp.bexp list, string) result =
  match String.trim s with
  | "" -> Ok []
  | s ->
      String.split_on_char ',' s
      |> List.fold_left
           (fun acc e ->
             let* es = acc in
             let* b = parse_bexp "assumption" e in
             Ok (es @ [ b ]))
           (Ok [])

let parse_rule (line : string) : (Exp_match.rule, string) result =
  match split2 "=>" line with
  | None -> Error ("rule missing '=>': " ^ line)
  | Some (lhs_s, rest) ->
      let rhs_s, emits_s =
        match split2 ";" rest with Some (r, e) -> (r, e) | None -> (rest, "")
      in
      let* lhs = parse_nexp "lhs" lhs_s in
      let* rhs = parse_nexp "rhs" rhs_s in
      let* emits = parse_emits emits_s in
      let bound = hole_keys (Exp.n_free_names lhs Variable.Set.empty) in
      let used =
        List.fold_left
          (fun acc b ->
            Variable.Set.union acc
              (hole_keys (Exp.b_free_names b Variable.Set.empty)))
          (hole_keys (Exp.n_free_names rhs Variable.Set.empty))
          emits
      in
      let unbound = Variable.Set.diff used bound in
      if Variable.Set.is_empty unbound then
        Ok Exp_match.{ lhs; fire = (fun s -> Some s); rhs; emits }
      else
        Error
          ("rule references holes not bound by the pattern: "
          ^ (Variable.Set.elements unbound
            |> List.map Variable.name |> String.concat ", "))

let is_comment (l : string) : bool =
  String.length l = 0
  || String.starts_with ~prefix:"#" l
  || String.starts_with ~prefix:"//" l

let parse (text : string) : (Exp_match.rule list, string) result =
  text |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun l -> not (is_comment l))
  |> List.fold_left
       (fun acc line ->
         let* rules = acc in
         let* r = parse_rule line in
         Ok (rules @ [ r ]))
       (Ok [])

let fastdiv_text =
  "(__umulhi($n, $fdv.x) +u $n) >>u $fdv.y => $n /u $fdv.z ; $fdv.z >=u 1"

let all : Exp_match.rule list =
  match parse fastdiv_text with Ok rules -> rules | Error _ -> []
