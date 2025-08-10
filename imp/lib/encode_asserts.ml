open Protocols

let if_ (b : Exp.bexp) (s : Code.t) : Code.t =
  match b with Bool true -> s | Bool false -> Skip | _ -> If (b, s, Skip)

module AssertionTree = struct
  open Exp

  type t =
    | True
    | Cond of bexp
    | Append of t * t
    | And of Exp.bexp * t
    | Implies of Exp.bexp * t

  let true_ : t = True

  let rec from_bexp (e : bexp) : t =
    let open Exp in
    let e =
      match e with
      | BNot e -> b_not e
      | BRel (BAnd, e1, e2) -> b_and e1 e2
      | BRel (BOr, e1, e2) -> b_or e1 e2
      | NRel (o, e1, e2) -> n_rel o e1 e2
      | _ -> e
    in
    match e with
    | Bool true -> True
    | BRel (BAnd, e1, e2) -> and_ e1 (from_bexp e2)
    | e -> Cond e

  and and_ (e1 : bexp) (e2 : t) : t =
    match (e1, e2) with
    | Bool true, e -> e
    | e, True -> from_bexp e
    | Bool false, _ -> Cond (Bool false)
    | _, _ -> And (e1, e2)

  let append (e1 : t) (e2 : t) : t =
    match (e1, e2) with True, e | e, True -> e | _, _ -> Append (e1, e2)

  let implies (e1 : bexp) (e2 : t) =
    match (e1, e2) with
    | Bool true, e -> e
    | Bool false, _ -> True
    | _, _ -> Implies (e1, e2)

  let rec to_bexp : t -> Exp.bexp =
    let open Exp in
    function
    | True -> Bool true
    | Cond b -> b
    | Append (a, b) -> b_and (to_bexp a) (to_bexp b)
    | And (b, p) -> b_and b (to_bexp p)
    | Implies (b, p) -> b_impl b (to_bexp p)

  let retain (x : Variable.t) : t -> bexp =
    let b_retain (b : bexp) : bexp = if Exp.b_mem x b then b else Bool true in
    let rec retain : t -> t = function
      | True -> True
      | Cond b -> if Exp.b_mem x b then Cond b else True
      | Append (a, b) -> append (retain a) (retain b)
      | And (a, b) -> and_ (b_retain a) (retain b)
      | Implies (a, b) -> implies (b_retain a) (retain b)
    in
    fun e -> retain e |> to_bexp

  let remove (x : Variable.t) : t -> t =
    let open Exp in
    let b_remove (e : bexp) : bexp = if Exp.b_mem x e then Bool true else e in
    let rec remove : t -> t = function
      | True -> True
      | Cond b -> if Exp.b_mem x b then True else Cond b
      | And (a, b) -> and_ (b_remove a) (remove b)
      | Implies (a, b) -> implies (b_remove a) (remove b)
      | Append (a, b) -> append (remove a) (remove b)
    in
    remove
end

let from_encode_assigns : Encode_assigns.t -> Code.t =
  let open Exp in
  let rec from : Encode_assigns.t -> Code.t * AssertionTree.t = function
    | Skip -> (Skip, AssertionTree.true_)
    | Access a -> (Access a, AssertionTree.true_)
    | Seq (Assert { cond = e; visibility = Local }, p) ->
        let p, a = from p in
        let a = AssertionTree.implies e a in
        (If (e, p, Skip), a)
    | Seq (p, q) ->
        let p, a1 = from p in
        let q, a2 = from q in
        (Seq (p, q), AssertionTree.append a1 a2)
    | Assert { cond = e; visibility = Global } ->
        (Skip, AssertionTree.from_bexp e)
    | Assert { visibility = Local; _ } -> (Skip, AssertionTree.true_)
    | If (b, then_s, else_s) ->
        let then_s, a1 = from then_s in
        let else_s, a2 = from else_s in
        let a = AssertionTree.(append (implies b a1) (implies (b_not b) a2)) in
        (If (b, then_s, else_s), a)
    | For (r, p) ->
        let p, a = from p in
        let guard = AssertionTree.retain (Range.var r) a in
        let a = AssertionTree.remove (Range.var r) a in
        ( Loop { range = r; body = if_ guard p },
          AssertionTree.implies (Range.has_next r) a )
    | Sync e -> (Sync e, AssertionTree.true_)
    | Decl { var; ty; body = p } ->
        let p, a = from p in
        let guard = AssertionTree.retain var a in
        let a = AssertionTree.remove var a in
        (Decl { var; ty; body = if_ guard p }, a)
  in
  fun e ->
    let p, a = from e in
    if_ (AssertionTree.to_bexp a) p
