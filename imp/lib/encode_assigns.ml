open Protocols
open Stage0

type t =
  | Access of Access.t
  | Assert of Assert.t
  | Sync of Sync.t
  | If of Exp.bexp * t * t
  | For of Range.t * t
  | Seq of t * t
  | Skip
  | Decl of {
      var : Variable.t;
      ty : C_type.t;
      body : t;
    }

let decl ?(ty = C_type.int) (var : Variable.t) (body : t) : t =
  Decl { var; ty; body }

let seq (p : t) (q : t) : t =
  match (p, q) with Skip, s | s, Skip -> s | _, _ -> Seq (p, q)

let to_string : t -> string =
  let rec to_s : t -> Indent.t list = function
    | Skip -> [ Line "skip;" ]
    | Sync s -> [ Line (Sync.to_string s ^ ";") ]
    | Assert b -> [ Line (Assert.to_string b ^ ";") ]
    | Access e -> [ Line (Access.to_string e) ]
    | Decl d ->
        [
          Line (C_type.to_string d.ty ^ " " ^ Variable.name d.var ^ " {");
          Block (to_s d.body);
          Line "}";
        ]
    | If (b, s1, s2) ->
        [
          Line ("if (" ^ Exp.b_to_string b ^ ") {");
          Block (to_s s1);
          Line "} else {";
          Block (to_s s2);
          Line "}";
        ]
    | For (r, s) ->
        [
          Line ("foreach (" ^ Range.to_string r ^ ") {");
          Block (to_s s);
          Line "}";
        ]
    | Seq (p, q) -> to_s p @ to_s q
  in
  fun p -> to_s p |> Indent.to_string

module SubstMake (S : Subst.SUBST) = struct
  module M = Subst.Make (S)

  let o_subst (st : S.t) : Exp.nexp option -> Exp.nexp option = function
    | Some n -> Some (M.n_subst st n)
    | None -> None

  let rec subst (st : S.t) : t -> t = function
    | Sync l -> Sync l
    | Skip -> Skip
    | Access a -> Access (M.a_subst st a)
    | Assert b -> Assert (Assert.map (M.b_subst st) b)
    | Decl d ->
        Decl
          {
            d with
            body =
              M.add st d.var (function
                | Some st' -> subst st' d.body
                | None -> d.body);
          }
    | If (b, p1, p2) -> If (M.b_subst st b, subst st p1, subst st p2)
    | For (r, p) ->
        For
          ( M.r_subst st r,
            M.add st r.var (function Some st -> subst st p | None -> p) )
    | Seq (p, q) -> Seq (subst st p, subst st q)
end

module ReplacePair = SubstMake (Subst.SubstPair)

let subst = ReplacePair.subst

let rec has_nif : Exp.nexp -> bool = function
  | NIf _ -> true
  | Var _ | Num _ -> false
  | Unary (_, e) -> has_nif e
  | Binary (_, e1, e2) -> has_nif e1 || has_nif e2
  | NCall (_, es) -> List.exists has_nif es
  | CastInt b -> has_nif_b b

and has_nif_b : Exp.bexp -> bool = function
  | Bool _ -> false
  | CastBool e -> has_nif e
  | NRel (_, e1, e2) -> has_nif e1 || has_nif e2
  | BRel (_, b1, b2) -> has_nif_b b1 || has_nif_b b2
  | BNot b -> has_nif_b b
  | Pred (_, es) -> List.exists has_nif es
  | Distinct es -> List.exists has_nif es
  | ThreadUnif e -> has_nif e
  | AtomicResult _ -> false

let from_scoped (known : Variable.Set.t) : Scoped.Code.t -> t =
  let n_subst (st : Subst.Vars.t) (n : Exp.nexp) : Exp.nexp =
    if Subst.Vars.is_empty st then n else Subst.ReplaceVars.n_subst st n
  in
  let b_subst (st : Subst.Vars.t) (b : Exp.bexp) : Exp.bexp =
    if Subst.Vars.is_empty st then b else Subst.ReplaceVars.b_subst st b
  in
  let a_subst (st : Subst.Vars.t) (a : Access.t) : Access.t =
    if Subst.Vars.is_empty st then a else Subst.ReplaceVars.a_subst st a
  in
  let r_subst (st : Subst.Vars.t) (r : Range.t) : Range.t =
    if Subst.Vars.is_empty st then r else Subst.ReplaceVars.r_subst st r
  in
  let rec inline (known : Variable.Set.t) (st : Subst.Vars.t)
      (i : Scoped.Code.t) : t =
    let add_var (x : Variable.t) :
        Variable.t * Variable.Set.t * Subst.Vars.t =
      let x, st =
        if Variable.Set.mem x known then
          let new_x = Variable.fresh known x in
          (new_x, Subst.Vars.put st x (Var new_x))
        else (x, st)
      in
      let known = Variable.Set.add x known in
      (x, known, st)
    in
    let name_or_inline (x : Variable.t) (ty : C_type.t) (data : Exp.nexp)
        (p : Scoped.Code.t) : t =
      let n = n_subst st data in
      if has_nif data then
        let x' = Variable.fresh known x in
        let known = Variable.Set.add x' known in
        let st = Subst.Vars.put st x (Var x') in
        Decl
          {
            var = x';
            ty;
            body =
              seq
                (Assert
                   (Assert.make (Exp.n_eq (Var x') n) Assert.Visibility.Local))
                (inline known st p);
          }
      else
        let st = Subst.Vars.put st x n in
        inline known st p
    in
    match i with
    | Sync l ->
        Sync
          {
            l with
            id = n_subst st l.id;
            participants = Option.map (n_subst st) l.participants;
          }
    | Assert b -> Assert (Assert.map (b_subst st) b)
    | Access e -> Access (a_subst st e)
    | Skip -> Skip
    | Call (_, p) -> inline known st p
    | If (b, p1, p2) ->
        let b = b_subst st b in
        If (b, inline known st p1, inline known st p2)
    | Decl ({ var = x; init = Some n; ty }, p) -> name_or_inline x ty n p
    | Assign { var = x; data = n; ty; body = p } -> name_or_inline x ty n p
    | Decl ({ var; init = None; ty }, p) ->
        Decl { var; ty; body = inline known st p }
    | For (r, p) ->
        let r = r_subst st r in
        let x, known, st = add_var r.var in
        For ({ r with var = x }, inline known st p)
    | Seq (p1, p2) -> Seq (inline known st p1, inline known st p2)
  in
  fun p -> p |> Scoped.Code.vars_distinct |> inline known (Subst.Vars.make [])
