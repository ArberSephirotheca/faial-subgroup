open Stage0
module StringSet = Common.StringSet

module Code = struct
  open Protocols

  type t =
    | Skip
    | Sync of Sync.t
    | Assert of Assert.t
    | Access of Access.t
    | Call of (Call.t * t)
    | If of (Exp.bexp * t * t)
    | For of (Range.t * t)
    | Assign of { var : Variable.t; ty : C_type.t; data : Exp.nexp; body : t }
    | Decl of (Decl.t * t)
    | Seq of t * t

  let add_inside ~(child : t) : t -> t =
    let rec add : t -> t = function
      (* leaves: *)
      | Skip -> child
      | (Sync _ | Assert _ | Access _ | If _ | For _) as i -> Seq (i, child)
      (* nested cases: *)
      | Call (c, s) -> Call (c, add s)
      | Assign a -> Assign { a with body = add a.body }
      | Decl (d, s) -> Decl (d, add s)
      | Seq (s1, s2) -> Seq (s1, add s2)
    in
    add

  let decl_set ?(ty = C_type.int) (x : Variable.t) (e : Exp.nexp) (s : t) : t =
    Decl (Decl.set ~ty x e, s)

  let decl_unset ?(ty = C_type.int) (x : Variable.t) (s : t) : t =
    Decl (Decl.unset ~ty x, s)

  let calls : t -> StringSet.t =
    let rec calls (cs : StringSet.t) : t -> StringSet.t = function
      | Skip | Sync _ | Assert _ | Access _ -> cs
      | Call (c, s) ->
          let cs = StringSet.add (Call.unique_id c) cs in
          calls cs s
      | If (_, s1, s2) | Seq (s1, s2) -> calls (calls cs s1) s2
      | For (_, s) | Decl (_, s) | Assign { body = s; _ } -> calls cs s
    in
    calls StringSet.empty

  let to_string : t -> string =
    let rec to_s : t -> Indent.t list = function
      | Skip -> [ Line "skip;" ]
      | Sync s -> [ Line (Sync.to_string s ^ ";") ]
      | Assert b -> [ Line (Assert.to_string b ^ ";") ]
      | Access e -> [ Line (Access.to_string e) ]
      | Call (c, s) ->
          [ Line (Call.to_string c ^ " {"); Block (to_s s); Line "}" ]
      | Assign a ->
          [
            Line (Variable.name a.var ^ " = " ^ Exp.n_to_string a.data ^ " {");
            Block (to_s a.body);
            Line "}";
          ]
      | Decl (d, p) ->
          [
            Line ("decl " ^ Decl.to_string d ^ " in {");
            Block (to_s p);
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
      | Decl (d, l) -> Decl (d, loc_subst l)
      | Assign a -> Assign { a with body = loc_subst a.body }
      | If (b, s1, s2) -> If (b, loc_subst s1, loc_subst s2)
      | For (r, s) -> For (r, loc_subst s)
      | Seq (p, q) -> Seq (loc_subst p, loc_subst q)
      | Call (c, s) -> Call (Call.loc_subst alias c, loc_subst s)
      | (Assert _ | Sync _ | Skip) as i -> i
    in
    fun s -> if Alias.is_trivial alias then s else loc_subst s

  module SubstMake (S : Subst.SUBST) = struct
    module M = Subst.Make (S)

    let o_subst (st : S.t) : Exp.nexp option -> Exp.nexp option = function
      | Some n -> Some (M.n_subst st n)
      | None -> None

    let rec subst (st : S.t) : t -> t = function
      | Access a -> Access (M.a_subst st a)
      | Assert b -> Assert (Assert.map (M.b_subst st) b)
      | Decl (d, p) ->
          let d = Decl.map (M.n_subst st) d in
          Decl
            (d, M.add st d.var (function Some st' -> subst st' p | None -> p))
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
      | Call (c, s) ->
          let c = Call.map (M.n_subst st) c in
          let s =
            match c.result with
            | Some (x, _) ->
                M.add st x (function Some st -> subst st s | None -> s)
            | None -> subst st s
          in
          Call (c, s)
      | Sync s ->
          Sync
            {
              s with
              id = M.n_subst st s.id;
              participants = Option.map (M.n_subst st) s.participants;
            }
      | Skip -> Skip
  end

  module ReplacePair = SubstMake (Subst.SubstPair)

  let subst = ReplacePair.subst

  (* Only keep accesses that mention an array in the set *)
  let filter_locs (locs : Variable.Set.t) : t -> t =
    let rec filter : t -> t = function
      | Access { array = x; _ } as i ->
          if Variable.Set.mem x locs then i else Skip
      | Call (c, s) ->
          let arrays = Call.arrays c |> Variable.Set.of_list in
          let s = filter s in
          if Variable.Set.is_empty arrays then
            (* Does not have arrays, then we keep the function call *)
            Call (c, s)
          else
            (* Has arrays in args, they must mention locs *)
            let arrays = Variable.Set.inter arrays locs in
            if not (Variable.Set.is_empty arrays) then Call (c, s) else s
      | If (b, p1, p2) -> If (b, filter p1, filter p2)
      | For (r, p) -> For (r, filter p)
      | Decl (d, p) -> Decl (d, filter p)
      | Assign a -> Assign { a with body = filter a.body }
      | Seq (p1, p2) -> Seq (filter p1, filter p2)
      | (Assert _ | Skip | Sync _) as i -> i
    in
    filter

  module Distinct = struct
    open State.Syntax

    type 'a state = (Variable.Set.t, 'a) State.t

    (* Check if variable is already used *)
    let is_used (x : Variable.t) : bool state =
      let* vars = State.get in
      return (Variable.Set.mem x vars)

    (* Add variable to used set *)
    let add_var (x : Variable.t) : unit state =
      State.update (Variable.Set.add x)

    (* Generate fresh variable and add it to state *)
    let fresh_var (x : Variable.t) : Variable.t state =
      let* vars = State.get in
      let new_x = Variable.fresh vars x in
      let* () = add_var new_x in
      return new_x

    let rec distinct : t -> t state = function
      | (Access _ | Skip | Sync _ | Assert _) as p -> return p
      | Call (c, p) ->
          let* c, p =
            match c.result with
            | Some (x, ty) ->
                let* used = is_used x in
                if used then
                  let* new_x = fresh_var x in
                  let p = subst (x, Var new_x) p in
                  return ({ c with result = Some (new_x, ty) }, p)
                else
                  let* () = add_var x in
                  return (c, p)
            | None -> return (c, p)
          in
          let* p = distinct p in
          return (Call (c, p))
      | Seq (p, q) ->
          let* p = distinct p in
          let* q = distinct q in
          return (Seq (p, q))
      | If (b, p, q) ->
          let* p = distinct p in
          let* q = distinct q in
          return (If (b, p, q))
      | Assign a ->
          let* body = distinct a.body in
          return (Assign { a with body })
      | Decl (d, p) ->
          let x = d.var in
          let* used = is_used x in
          if used then
            let* new_x = fresh_var x in
            let p = subst (x, Var new_x) p in
            let* p = distinct p in
            return (Decl ({ d with var = new_x }, p))
          else
            let* () = add_var x in
            let* p = distinct p in
            return (Decl (d, p))
      | For (r, p) ->
          let x = Range.var r in
          let* used = is_used x in
          if used then
            let* new_x = fresh_var x in
            let p = subst (x, Var new_x) p in
            let* p = distinct p in
            return (For ({ r with var = new_x }, p))
          else
            let* () = add_var x in
            let* p = distinct p in
            return (For (r, p))
  end

  (* Helper functions for variable distinctness state monad *)
  let vars_distinct ?(vars = Variable.Set.empty) : t -> t =
   fun p -> State.run_result (Distinct.distinct p) vars

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
      | Call (c, p) -> (
          match c.result with
          | Some (var, ty) ->
              let defined = Params.add var ty defined in
              let assigns, p = fix_assigns defined p in
              (assigns, Call (c, p))
          | None ->
              let assigns, p = fix_assigns defined p in
              (assigns, Call (c, p)))
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
      | Seq (Call c, s) ->
          let* s = imp_to_scoped s in
          return (Call (c, s))
      | Seq (s1, s2) ->
          let* s1 = imp_to_scoped s1 in
          let* s2 = imp_to_scoped s2 in
          return (Seq (s1, s2))
      | Sync s -> return (Sync s)
      | Write e ->
          return
            (Access { array = e.array; index = e.index; mode = Write e.payload })
      | Assert b -> return (Assert b)
      | Call c -> return (Call (c, Skip))
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
      let (_, globals), p = State.run (imp_to_scoped s) (1, globals) in
      (globals, p)
end

(* Kernel module for representing kernels in scoped form *)
module Kernel = struct
  module C = Code
  module K = Kernel
  open Protocols
  open K
  module Kernel = K
  module Code = C

  type t = {
    name : string;
    ty : string;
    parameters : ParameterList.t;
    global_arrays : Memory.t Variable.Map.t;
    global_variables : Params.t;
    code : Code.t;
    return : Exp.nexp option;
    visibility : Visibility.t;
    grid_dim : Dim3.t option;
    block_dim : Dim3.t option;
  }

  let local_set (k : t) : Variable.Set.t = ParameterList.to_set k.parameters
  let global_set (k : t) : Variable.Set.t = Params.to_set k.global_variables

  let variable_set (k : t) : Variable.Set.t =
    Variable.Set.union (local_set k) (global_set k)

  (* Generate a unique id that pairs the name and type. *)
  let unique_id (k : t) : string = Call.kernel_id ~kernel:k.name ~ty:k.ty
  let calls (k : t) : StringSet.t = Code.calls k.code

  let from_imp (k : Kernel.t) : t =
    let globals =
      (* Take the global variables and the scalars defined in the paramter list *)
      k.global_variables
      |> Params.union_left (ParameterList.to_params k.parameters)
    in
    (* Add any globals defined from scoped *)
    let globals, p = Code.from_stmt (globals, k.code) in
    {
      name = k.name;
      ty = k.ty;
      parameters = k.parameters;
      global_arrays = k.global_arrays;
      global_variables = globals;
      code = p;
      return = k.return;
      visibility = k.visibility;
      grid_dim = k.grid_dim;
      block_dim = k.block_dim;
    }
end
