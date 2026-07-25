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

  let rec written_arrays (acc : Variable.Set.t) : t -> Variable.Set.t = function
    | Access { array; mode = Write _ | Atomic _; _ } ->
        Variable.Set.add array acc
    | Call (c, p) ->
        Call.arrays c |> Variable.Set.of_list |> Variable.Set.union acc
        |> fun acc -> written_arrays acc p
    | If (_, p, q) | Seq (p, q) -> written_arrays (written_arrays acc p) q
    | For (_, p) | Decl (_, p) | Assign { body = p; _ } -> written_arrays acc p
    | Access _ | Assert _ | Sync _ | Skip -> acc

  (* Counts the stores to each array sequenced before a program point.
     Two loads agree only while nothing has stored in between, so the
     count is passed as the read call's leading argument and separates
     loads taken on either side of a store. It must stay a numeral: a
     variable in that position would let the solver equate two counts
     and merge versions that a store separates. *)
  module Version = struct
    type t = int Variable.Map.t

    let empty : t = Variable.Map.empty

    let get (x : Variable.t) (v : t) : int =
      Variable.Map.find_opt x v |> Option.value ~default:0

    let bump (x : Variable.t) (v : t) : t = Variable.Map.add x (get x v + 1) v

    let join : t -> t -> t =
      Variable.Map.union (fun _ a b -> Some (max a b))
  end

  (* A load binds to the read symbol at the array's current version. A
     loop body that stores to the array is the one shape a version
     cannot express, since the count would have to advance per
     iteration, so its loads are left unbound. A cross-thread store with
     no barrier in between needs no version bump, since it races with
     the read on its own. *)
  let bind_uniform_reads : t -> t =
    let read_call (v : Version.t) (ty : C_type.t) (array : Variable.t)
        (index : Exp.nexp list) : Exp.nexp =
      Exp.ReadResult
        { array; version = Version.get array v; ty; args = index }
    in
    let rec rewrite (looped : Variable.Set.t) (v : Version.t) :
        t -> t * Version.t = function
      | Seq
          ( (Access { array; index; mode = Read; _ } as acc),
            Decl ((({ init = None; _ } : Decl.t) as d), rest) )
        when not (Variable.Set.mem array looped) ->
          let call = read_call v d.ty array index in
          let rest, v = rewrite looped v rest in
          (Seq (acc, Decl ({ d with init = Some call }, rest)), v)
      | Seq
          ( If (b, (Access { array; index; mode = Read; _ } as acc), Skip),
            Decl ((({ init = None; _ } : Decl.t) as d), rest) )
        when not (Variable.Set.mem array looped) ->
          let call = read_call v d.ty array index in
          let rest, v = rewrite looped v rest in
          (Seq (If (b, acc, Skip), Decl ({ d with init = Some call }, rest)), v)
      | Seq (p, q) ->
          let p, v = rewrite looped v p in
          let q, v = rewrite looped v q in
          (Seq (p, q), v)
      | If (b, p, q) ->
          let p, v1 = rewrite looped v p in
          let q, v2 = rewrite looped v q in
          (If (b, p, q), Version.join v1 v2)
      | For (r, p) ->
          let p, v = rewrite (written_arrays looped p) v p in
          (For (r, p), v)
      | Decl (d, p) ->
          let p, v = rewrite looped v p in
          (Decl (d, p), v)
      | Assign a ->
          let body, v = rewrite looped v a.body in
          (Assign { a with body }, v)
      | Call (c, p) ->
          let v = Call.arrays c |> List.fold_left (Fun.flip Version.bump) v in
          let p, v = rewrite looped v p in
          (Call (c, p), v)
      | Access { array; mode = Write _ | Atomic _; _ } as p ->
          (p, Version.bump array v)
      | (Access _ | Assert _ | Sync _ | Skip) as p -> (p, v)
    in
    fun p -> rewrite Variable.Set.empty Version.empty p |> fst

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

  (* Collect every variable name mentioned anywhere in [p] (binders
     and free references). [vars_distinct] seeds its fresh-name pool
     with this so a renamed binder cannot collide with any other name
     in the term. *)
  let mentioned : t -> Variable.Set.t =
    let n = Exp.n_free_names in
    let b = Exp.b_free_names in
    let collect_arg (acc : Variable.Set.t) (a : Arg.t) : Variable.Set.t =
      match a with
      | Arg.Scalar e -> n e acc
      | Arg.Array u -> Variable.Set.add u.array (n u.offset acc)
      | Arg.Unsupported _ -> acc
    in
    let rec go (acc : Variable.Set.t) : t -> Variable.Set.t = function
      | Skip -> acc
      | Sync s ->
          let acc = n s.id acc in
          (match s.participants with Some e -> n e acc | None -> acc)
      | Assert a -> b a.cond acc
      | Access a ->
          let acc = Variable.Set.add a.array acc in
          List.fold_left (fun acc e -> n e acc) acc a.index
      | Decl (d, p) ->
          let acc = Variable.Set.add d.var acc in
          let acc = match d.init with Some e -> n e acc | None -> acc in
          go acc p
      | Assign { var; data; body; _ } ->
          let acc = Variable.Set.add var acc in
          go (n data acc) body
      | If (cond, p, q) -> go (go (b cond acc) p) q
      | Seq (p, q) -> go (go acc p) q
      | For (r, p) ->
          let acc = Variable.Set.add r.var acc in
          let acc = Range.free_names r acc in
          let acc = match r.step with Plus e | Mult e -> n e acc in
          go acc p
      | Call (c, p) ->
          let acc =
            match c.result with
            | Some (x, _) -> Variable.Set.add x acc
            | None -> acc
          in
          go (List.fold_left collect_arg acc c.args) p
    in
    go Variable.Set.empty

  let vars_distinct ?(vars = Variable.Set.empty) (root : t) : t =
    let module R = Subst.ReplaceVars in
    let empty (env : Subst.Vars.t) : bool = Variable.Map.is_empty env in
    let ns env e = if empty env then e else R.n_subst env e in
    let bs env b = if empty env then b else R.b_subst env b in
    let as_ env a = if empty env then a else R.a_subst env a in
    let rs env r = if empty env then r else R.r_subst env r in
    let enter (env : Subst.Vars.t) (bound : Variable.Set.t)
        (taken : Variable.Set.t) (x : Variable.t) :
        Variable.t * Subst.Vars.t * Variable.Set.t * Variable.Set.t =
      if Variable.Set.mem x bound then
        let x' = Variable.fresh taken x in
        ( x',
          Variable.Map.add x (Exp.Var x') env,
          Variable.Set.add x' bound,
          Variable.Set.add x' taken )
      else (x, env, Variable.Set.add x bound, taken)
    in
    let rec go (env : Subst.Vars.t) (bound : Variable.Set.t)
        (taken : Variable.Set.t) :
        t -> t * Variable.Set.t * Variable.Set.t = function
      | Skip -> (Skip, bound, taken)
      | Access a -> (Access (as_ env a), bound, taken)
      | Assert b -> (Assert (Assert.map (bs env) b), bound, taken)
      | Sync s ->
          ( Sync
              {
                s with
                id = ns env s.id;
                participants = Option.map (ns env) s.participants;
              },
            bound,
            taken )
      | If (b, p, q) ->
          let b = bs env b in
          let p, bound, taken = go env bound taken p in
          let q, bound, taken = go env bound taken q in
          (If (b, p, q), bound, taken)
      | Seq (p, q) ->
          let p, bound, taken = go env bound taken p in
          let q, bound, taken = go env bound taken q in
          (Seq (p, q), bound, taken)
      | Assign a ->
          let data = ns env a.data in
          (* [a.var] binds over the body but is never renamed by this
             pass; drop any inherited rename of that name so the body's
             references resolve to this binder. *)
          let body, bound, taken =
            go (Variable.Map.remove a.var env) bound taken a.body
          in
          (Assign { a with data; body }, bound, taken)
      | Decl (d, p) ->
          let d = Decl.map (ns env) d in
          let x, env, bound, taken = enter env bound taken d.var in
          let p, bound, taken = go env bound taken p in
          (Decl ({ d with var = x }, p), bound, taken)
      | For (r, p) ->
          let r = rs env r in
          let x, env, bound, taken = enter env bound taken (Range.var r) in
          let p, bound, taken = go env bound taken p in
          (For ({ r with var = x }, p), bound, taken)
      | Call (c, p) ->
          let c = Call.map (ns env) c in
          let result, env, bound, taken =
            match c.result with
            | Some (x, ty) ->
                let x, env, bound, taken = enter env bound taken x in
                (Some (x, ty), env, bound, taken)
            | None -> (None, env, bound, taken)
          in
          let p, bound, taken = go env bound taken p in
          (Call ({ c with result }, p), bound, taken)
    in
    let taken0 = Variable.Set.union vars (mentioned root) in
    let root, _, _ = go Variable.Map.empty vars taken0 root in
    root

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
          (* Inherit the enclosing [defined] set so that an [Assign]
             inside a branch to an outer-scope variable isn't wrongly
             reported as outstanding. Mirrors the [For] case fix. *)
          let assigns_1, p = fix_assigns defined p in
          let assigns_2, q = fix_assigns defined q in
          (Params.union_left assigns_1 assigns_2, If (b, p, q))
      | Assign a ->
          let assigns, body = fix_assigns defined a.body in
          (* Always bubble [a.var]. The enclosing [Decl (d, p)] case
             removes [d.var] from the bubbled set when [d.var = a.var],
             so a sequential [decl x = e in Assign x = e' in ...] still
             collapses cleanly. The bubble matters when the [Assign]
             sits inside a branch of an [If] (or a [For] body): the
             enclosing [Seq] wraps the continuation with a [Decl.unset
             a.var] so the post-conditional code sees a fresh free
             variable rather than the prior [Decl]'s [init] (or any
             prior [Assign]'s value), which is the correct semantics
             for a mutation that only fires on some control-flow paths. *)
          let assigns = Params.add a.var a.ty assigns in
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
          (* [d] already declares [d.var] at this scope, so any
             outstanding Assign of [d.var] within [p] is satisfied
             here. Drop it from the bubbled set so the enclosing
             [For]/[Seq] doesn't wrap an outer [Decl.unset] for it. *)
          (Params.remove_all (Variable.Set.singleton d.var) assigns, Decl (d, p))
      | Seq (p, q) ->
          let assigns_1, p = fix_assigns defined p in
          let assigns_2, q = fix_assigns defined q in
          (Params.union_left assigns_1 assigns_2, Seq (p, decl assigns_1 q))
      | For (r, p) ->
          (* Inherit the enclosing [defined] set so that outer-scope
             variables aren't wrongly reported as outstanding for
             [Assign]s inside the loop body. The For's range variable
             [r.var] is added too, since it's bound here. *)
          let defined = Params.add r.var C_type.int defined in
          let assigns, p = fix_assigns defined p in
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

  let atomic_result_marker (aw : Atomic_write.t) : Assert.t =
    let marker =
      Exp.AtomicResult
        {
          target = aw.target;
          array = aw.array;
          index = aw.index;
          operation = aw.atomic.operation;
        }
    in
    Assert.make marker Global

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
          let rd = Access (Access.read e.array e.index) in
          let rd = match e.guard with Some g -> If (g, rd, Skip) | None -> rd in
          return
            (match e.target with
            | Some (ty, x) -> Seq (rd, Decl (Decl.unset ~ty x, s))
            | None -> Seq (rd, s))
      | Seq (Atomic e, s) ->
          let* s = imp_to_scoped s in
          let a =
            Access
              (Access.make ~array:e.array ~index:e.index
                 ~mode:(Atomic e.atomic))
          in
          let a = match e.guard with Some g -> If (g, a, Skip) | None -> a in
          let s = Seq (Assert (atomic_result_marker e), s) in
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
          let a =
            Access (Access.write e.array e.index e.payload)
          in
          return (match e.guard with Some g -> If (g, a, Skip) | None -> a)
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

  let is_global (k : t) : bool = k.visibility = Visibility.Global

  let to_string (k : t) : string =
    Printf.sprintf "%s %s (%s)\nglobal {arrays: %s} {scalars: %s}\n{\n%s}\n"
      (Visibility.to_string k.visibility)
      k.name
      (ParameterList.to_string k.parameters)
      (Memory.map_to_string k.global_arrays)
      (Params.to_string k.global_variables)
      (Code.to_string k.code)

  let print (k : t) : unit = print_string (to_string k)
end
