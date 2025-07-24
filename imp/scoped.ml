open Stage0
open Protocols

type t =
  | Skip
  | Sync of Location.t option
  | Assert of Assert.t
  | Access of Access.t
  | If of (Exp.bexp * t * t)
  | For of (Range.t * t)
  | Assign of { var : Variable.t; ty : C_type.t; data : Exp.nexp; body : t }
  | Decl of (Decl.t * t)
  | Seq of t * t

let to_string : t -> string =
  let rec to_s : t -> Indent.t list = function
    | Skip -> [ Line "skip;" ]
    | Sync _ -> [ Line "sync;" ]
    | Assert b -> [ Line (Assert.to_string b ^ ";") ]
    | Access e -> [ Line (Access.to_string e) ]
    | Assign a ->
        [
          Line (Variable.name a.var ^ " = " ^ Exp.n_to_string a.data ^ " {");
          Block (to_s a.body);
          Line "}";
        ]
    | Decl (d, p) -> Line ("decl " ^ Decl.to_string d ^ ";") :: to_s p
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

let loc_subst (alias : Alias.t) : t -> t =
  let rec loc_subst : t -> t = function
    | Access a as i ->
        if Variable.equal a.array alias.target then
          (* Update the name of the resolved array,
           but keep the original location *)
          let new_x = { alias.source with location = a.array.location } in
          let new_a =
            if alias.offset = Num 0 then
              (* No offset, so same index *)
              a
            else
              match a.index with
              | n :: l ->
                  (* use the inlined variable but with the location of the alias,
                so that the error message appears in the right place. *)
                  { a with index = Exp.n_plus alias.offset n :: l }
              | [] -> failwith "Impossible to have 0 elements."
          in
          Access { new_a with array = new_x }
        else i
    | Assert _ as i -> i
    | Decl (d, l) -> Decl (d, loc_subst l)
    | Assign a -> Assign { a with body = loc_subst a.body }
    | If (b, s1, s2) -> If (b, loc_subst s1, loc_subst s2)
    | For (r, s) -> For (r, loc_subst s)
    | Sync l -> Sync l
    | Skip -> Skip
    | Seq (p, q) -> Seq (loc_subst p, loc_subst q)
  in
  fun s -> if Alias.is_trivial alias then s else loc_subst s

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
    | Decl (d, p) ->
        let d = Decl.map (M.n_subst st) d in
        Decl (d, M.add st d.var (function Some st' -> subst st' p | None -> p))
    | Assign a ->
        Assign
          {
            a with
            data = M.n_subst st a.data;
            body =
              M.add st a.var (function
                | Some st' -> subst st' a.body
                | None -> a.body);
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

let filter_locs (locs : Memory.t Variable.Map.t) : t -> t =
  let rec filter : t -> t = function
    | Access { array = x; _ } as i ->
        if Variable.Map.mem x locs then i else Skip
    | Assert _ as i -> i
    | Skip -> Skip
    | Sync l -> Sync l
    | If (b, p1, p2) -> If (b, filter p1, filter p2)
    | For (r, p) -> For (r, filter p)
    | Decl (d, p) -> Decl (d, filter p)
    | Assign a -> Assign { a with body = filter a.body }
    | Seq (p1, p2) -> Seq (filter p1, filter p2)
  in
  filter

let vars_distinct : t -> t =
  let rec distinct (vars : Variable.Set.t) (p : t) : Variable.Set.t * t =
    match p with
    | Access _ | Skip | Sync _ | Assert _ -> (vars, p)
    | Seq (p, q) ->
        let vars, p = distinct vars p in
        let vars, q = distinct vars q in
        (vars, Seq (p, q))
    | If (b, p, q) ->
        let vars, p = distinct vars p in
        let vars, q = distinct vars q in
        (vars, If (b, p, q))
    | Assign a ->
        let vars, body = distinct vars a.body in
        (vars, Assign { a with body })
    | Decl (d, p) ->
        let x = d.var in
        if Variable.Set.mem x vars then
          let new_x : Variable.t = Variable.fresh vars x in
          let vars = Variable.Set.add new_x vars in
          let p = subst (x, Var new_x) p in
          let vars, p = distinct vars p in
          (vars, Decl ({ d with var = new_x }, p))
        else
          let vars, p = distinct (Variable.Set.add x vars) p in
          (vars, Decl (d, p))
    | For (r, p) ->
        let x = Range.var r in
        if Variable.Set.mem x vars then
          let new_x : Variable.t = Variable.fresh vars x in
          let vars = Variable.Set.add new_x vars in
          let p = subst (x, Var new_x) p in
          let vars, p = distinct vars p in
          (vars, For ({ r with var = new_x }, p))
        else
          let vars, p = distinct (Variable.Set.add x vars) p in
          (vars, For (r, p))
  in
  fun p -> distinct Variable.Set.empty p |> snd

(* Rewrite assigns that cannot be represented as lets *)
let fix_assigns : t -> t =
  let decl (assigns : Params.t) (p : t) : t =
    Params.to_list assigns
    |> List.fold_left (fun p (x, ty) -> Decl (Decl.unset x ~ty, p)) p
  in
  let rec fix_assigns (defined : Params.t) (i : t) : Params.t * t =
    match i with
    | Skip | Sync _ | Access _ | Assert _ -> (Params.empty, i)
    | If (b, p, q) ->
        let assigns_1, p = fix_assigns Params.empty p in
        let assigns_2, q = fix_assigns Params.empty q in
        (Params.union_left assigns_1 assigns_2, If (b, p, q))
    | Assign a ->
        let assigns, body = fix_assigns defined a.body in
        let assigns =
          if Params.mem a.var defined then
            (* already defined, so no need to record outstanding
              assignment *)
            assigns
          else Params.add a.var a.ty assigns
        in
        (assigns, Assign { a with body })
    | Decl (d, p) ->
        let defined = Params.add d.var d.ty defined in
        let assigns, p = fix_assigns defined p in
        (assigns, Decl (d, p))
    | Seq (p, q) ->
        let assigns_1, p = fix_assigns defined p in
        let assigns_2, q = fix_assigns defined q in
        (Params.union_left assigns_1 assigns_2, Seq (p, decl assigns_1 q))
    | For (r, p) ->
        let assigns, p = fix_assigns Params.empty p in
        (* convert assigns to decls *)
        (assigns, For (r, decl assigns p))
  in
  fun s -> s |> fix_assigns Params.empty |> snd

type 'a state = (int * Params.t, 'a) State.t

let unknown_range (x : Variable.t) : Range.t =
  Range.
    {
      var = Variable.from_name "?";
      dir = Increase;
      lower_bound = Num 1;
      upper_bound = Var x;
      step = Step.plus (Num 1);
      ty = C_type.int;
    }

let from_stmt : Params.t * Stmt.t -> Params.t * t =
  let open State.Syntax in
  let unknown curr_id : Variable.t =
    Variable.from_name ("@loop_" ^ string_of_int curr_id)
  in
  let add_global (x : Variable.t) ?(ty = C_type.char) () : unit state =
    State.update (fun (curr_id, params) ->
        (curr_id + 1, Params.add x ty params))
  in
  let curr_unknown : Variable.t state =
    let* curr_id, _ = State.get in
    return (unknown curr_id)
  in
  let rec imp_to_scoped : Stmt.t -> t state = function
    | Skip ->
        return Skip
        (* normalize sequences so that they are sorted to the right-most *)
    | Seq (Seq (s1, s2), s3) -> imp_to_scoped (Seq (s1, Seq (s2, s3)))
    | Seq (LocationAlias e, s) ->
        let* s = imp_to_scoped s in
        return (loc_subst e s)
    | Seq (Decl d, p) ->
        let* s = imp_to_scoped p in
        return (Decl (d, s))
    | Seq (Assign { var; data; ty }, p) ->
        let* body = imp_to_scoped p in
        return (Assign { var; data; ty; body })
    | Seq (Read e, s) ->
        let* s = imp_to_scoped s in
        let rd = Access { array = e.array; index = e.index; mode = Read } in
        return
          (match e.target with
          | Some (ty, x) -> Seq (rd, Decl (Decl.unset ~ty x, s))
          | None -> Seq (rd, s))
    | Seq (Atomic e, s) ->
        let* s = imp_to_scoped s in
        let a =
          Access { array = e.array; index = e.index; mode = Atomic e.atomic }
        in
        return (Seq (a, Decl (Decl.unset ~ty:e.ty e.target, s)))
    | Seq (s1, s2) ->
        let* s1 = imp_to_scoped s1 in
        let* s2 = imp_to_scoped s2 in
        return (Seq (s1, s2))
    | Sync l -> return (Sync l)
    | Write e ->
        return
          (Access { array = e.array; index = e.index; mode = Write e.payload })
    | Assert b -> return (Assert b)
    | Call _ -> return Skip
    | If (b, s1, s2) ->
        let* s1 = imp_to_scoped s1 in
        let* s2 = imp_to_scoped s2 in
        return (If (b, s1, s2))
    | For (r, s) ->
        let* s = imp_to_scoped s in
        return (For (r, s))
    | Star s ->
        let synchronized = Stmt.has_sync s in
        let* s = imp_to_scoped s in
        let* x = curr_unknown in
        let r = unknown_range x in
        let s : t = For (r, s) in
        if synchronized then
          let* () = add_global x () in
          return s
        else return (Decl (Decl.unset x, s))
    (* Handled in the context of a prog *)
    | (LocationAlias _ | Decl _ | Assign _ | Read _ | Atomic _) as s ->
        imp_to_scoped (Seq (s, Skip))
  in
  fun (globals, s) ->
    let (_, globals), p = State.run (1, globals) (imp_to_scoped s) in
    (globals, p)
