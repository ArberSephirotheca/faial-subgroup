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
      ty : Ty.t;
      body : t;
    }

let decl ?(ty = Ty.int) (var : Variable.t) (body : t) : t =
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
          Line (Ty.to_string d.ty ^ " " ^ Variable.name d.var ^ " {");
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

(* Default for [from_scoped ~infer_cond_bound] (the [--infer-cond-bound] flag): an
   upper bound on the inlined (tree) size of a scalar value, in expression nodes.
   A value whose inlined size would exceed it is replaced by an unconstrained
   local rather than inlined, capping the exponential term growth that chained
   self-referential assignments (conditional [x = c ? f x : x] or multiplicative
   [s = s * s * v]) otherwise produce. Real index expressions are far smaller;
   only such chains, which are non-affine and not analyzable anyway, reach it. *)
let default_infer_cond_bound = 512

(* Inlined size of [n], computed from the pre-substitution expression and the
   recorded sizes of the variables it mentions, so it never walks the shared,
   possibly-huge substituted term. *)
let rec esize (sizes : int Variable.Map.t) (n : Exp.nexp) : int =
  match n with
  | Var x -> ( match Variable.Map.find_opt x sizes with Some s -> s | None -> 1)
  | Num _ -> 1
  | Unary (_, e) -> 1 + esize sizes e
  | Binary (_, e1, e2) -> 1 + esize sizes e1 + esize sizes e2
  | NCall (_, es) -> List.fold_left (fun a e -> a + esize sizes e) 1 es
  | ReadResult r -> List.fold_left (fun a e -> a + esize sizes e) 1 r.args
  | Convert c -> esize sizes c.arg
  | NIf (b, e1, e2) -> 1 + besize sizes b + esize sizes e1 + esize sizes e2
  | CastInt b -> 1 + besize sizes b

and besize (sizes : int Variable.Map.t) (b : Exp.bexp) : int =
  match b with
  | Bool _ -> 1
  | CastBool e -> 1 + esize sizes e
  | NRel (_, e1, e2) -> 1 + esize sizes e1 + esize sizes e2
  | BRel (_, b1, b2) -> 1 + besize sizes b1 + besize sizes b2
  | BNot b -> 1 + besize sizes b
  | Pred (_, es) -> List.fold_left (fun a e -> a + esize sizes e) 1 es
  | Distinct es -> List.fold_left (fun a e -> a + esize sizes e) 1 es
  | IsThreadUnif e -> 1 + esize sizes e
  | AtomicResult _ -> 1

let from_scoped ?(infer_cond_bound = default_infer_cond_bound)
    (known : Variable.Set.t) : Scoped.Code.t -> t =
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
      (sizes : int Variable.Map.t) (i : Scoped.Code.t) : t =
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
    let inline_or_havoc (x : Variable.t) (ty : Ty.t) (data : Exp.nexp)
        (p : Scoped.Code.t) : t =
      let sz = esize sizes data in
      if sz <= infer_cond_bound then
        let st = Subst.Vars.put st x (n_subst st data) in
        let sizes = Variable.Map.add x sz sizes in
        inline known st sizes p
      else
        (* The inlined value would exceed the budget; leave [x] as an
           unconstrained local so downstream reads a havoc value instead of an
           exploding term. *)
        let x' = Variable.fresh known x in
        let known = Variable.Set.add x' known in
        let st = Subst.Vars.put st x (Var x') in
        let sizes = Variable.Map.add x 1 sizes in
        Decl { var = x'; ty; body = inline known st sizes p }
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
    | Access e -> Access (a_subst st (Mem_access.to_access e))
    | Skip -> Skip
    | Call (_, p) -> inline known st sizes p
    | If (b, p1, p2) ->
        let b = b_subst st b in
        If (b, inline known st sizes p1, inline known st sizes p2)
    | Decl ({ var = x; init = Some n; ty }, p) -> inline_or_havoc x ty n p
    | Assign { var = x; data = n; ty; body = p } -> inline_or_havoc x ty n p
    | Decl ({ var; init = None; ty }, p) ->
        Decl { var; ty; body = inline known st sizes p }
    | PointerBind { body = p; _ } -> inline known st sizes p
    | For (r, p) ->
        let r = r_subst st r in
        let x, known, st = add_var r.var in
        For ({ r with var = x }, inline known st sizes p)
    | Seq (p1, p2) ->
        Seq (inline known st sizes p1, inline known st sizes p2)
  in
  fun p ->
    p |> Scoped.Code.vars_distinct
    |> inline known (Subst.Vars.make []) Variable.Map.empty
