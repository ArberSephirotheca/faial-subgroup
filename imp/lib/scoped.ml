open Stage0
module StringSet = Common.StringSet

module Code = struct
  open Protocols

  type t =
    | Skip
    | Sync of Sync.t
    | Assert of Assert.t
    | Access of Mem_access.t
    | Call of (Call.t * t)
    | If of (Exp.bexp * t * t)
    | For of (Range.t * t)
    | Assign of { var : Variable.t; ty : Ty.t; data : Exp.nexp; body : t }
    | Decl of (Decl.t * t)
    | PointerBind of { var : Variable.t; pointer : Pointer.t; body : t }
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
      | PointerBind p -> PointerBind { p with body = add p.body }
      | Seq (s1, s2) -> Seq (s1, add s2)
    in
    add

  let decl_set ?(ty = Ty.int) (x : Variable.t) (e : Exp.nexp) (s : t) : t =
    Decl (Decl.set ~ty x e, s)

  let decl_unset ?(ty = Ty.int) (x : Variable.t) (s : t) : t =
    Decl (Decl.unset ~ty x, s)

  let calls : t -> Function_id.Set.t =
    let rec calls (cs : Function_id.Set.t) : t -> Function_id.Set.t = function
      | Skip | Sync _ | Assert _ | Access _ -> cs
      | Call (c, s) ->
          let cs = Function_id.Set.add (Call.unique_id c) cs in
          calls cs s
      | If (_, s1, s2) | Seq (s1, s2) -> calls (calls cs s1) s2
      | For (_, s) | Decl (_, s) | Assign { body = s; _ }
      | PointerBind { body = s; _ } ->
          calls cs s
    in
    calls Function_id.Set.empty

  let to_string : t -> string =
    let rec to_s : t -> Indent.t list = function
      | Skip -> [ Line "skip;" ]
      | Sync s -> [ Line (Sync.to_string s ^ ";") ]
      | Assert b -> [ Line (Assert.to_string b ^ ";") ]
      | Access e -> [ Line (Mem_access.to_string e) ]
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
      | PointerBind p ->
          [
            Line
              ("alias " ^ Variable.name p.var ^ " = "
              ^ Pointer.to_string p.pointer ^ " in {");
            Block (to_s p.body);
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

  (* Replace every access through [target] by the memory the pointer names.
     An index that came out as a range of cells, which is what a pointer view
     wider than the element it lands on produces, is bound by a loop over the
     cells it spans. *)
  let resolve ~(arrays : Memory.t Variable.Map.t) ~(target : Variable.t)
      (pointer : Pointer.t) : t -> t =
    let materialise (a : Mem_access.t) (addr : Pointer.Address.t) : t =
      let taken =
        List.fold_left
          (fun acc (i : Pointer.Index.t) ->
            acc
            |> Exp.n_free_names (Pointer.Index.first i)
            |> Exp.n_free_names (Pointer.Index.last i))
          Variable.Set.empty addr.index
      in
      let ranges, index, _ =
        List.fold_left
          (fun (ranges, index, taken) (i : Pointer.Index.t) ->
            match i with
            | Pointer.Index.Exact { value } -> (ranges, value :: index, taken)
            | Pointer.Index.Span { first; last } ->
                let e = Variable.fresh taken (Variable.from_name "@view") in
                ( Range.make ~lower_bound:first e last :: ranges,
                  Exp.Var e :: index,
                  Variable.Set.add e taken ))
          ([], [], taken) addr.index
      in
      (* Keep the access's own location, so a diagnostic points at the use
         rather than at the pointer's declaration. *)
      let root =
        { addr.array with location = (Field_path.base a.path).location }
      in
      let path = Field_path.graft ~prefix:(Field_path.parse root) a.path in
      let index = List.rev index in
      let index =
        let dims =
          Variable.Map.find_opt (Field_path.to_variable path) arrays
          |> Option.map (fun (m : Memory.t) -> m.size)
          |> Option.value ~default:[]
        in
        Pointer.split_index ~dims index |> Option.value ~default:index
      in
      let body = Access { a with path; index } in
      let body = List.fold_left (fun s r -> For (r, s)) body ranges in
      (* A choice reaches one of its arms, so the access is emitted once per
         arm under the condition that selects it. The guard nests outside any
         loop a span introduced, since the span is inside the arm. *)
      match addr.guard with Some g -> If (g, body, Skip) | None -> body
    in
    let rewrite (a : Mem_access.t) : t =
      let a =
        match a.mode with
        | Access.Mode.Write (Some _) when not (Pointer.keeps_payload pointer)
          ->
            { a with mode = Access.Mode.Write None }
        | _ -> a
      in
      let rank (x : Variable.t) : int option =
        Variable.Map.find_opt x arrays
        |> Option.map (fun (m : Memory.t) -> List.length m.size)
      in
      match
        Pointer.addresses ~rank ~index:a.index pointer |> List.map (materialise a)
      with
      | [] -> Skip
      | i :: l -> List.fold_left (fun s i -> Seq (s, i)) i l
    in
    (* A binder for [target] ends the pointer's reach, so the substitution
       stops there. A pointer taken from this one is discharged by grafting
       into its base: that expression is read in the enclosing scope, so it
       is rewritten even when the binder it belongs to shadows [target]. *)
    let graft (p : Pointer.t) : Pointer.t =
      Pointer.subst_base ~target ~source:pointer p
    in
    (* A binder below [target] whose name descends from it, which is what a
       view of one field of a decomposed object is, owns every access on that
       field. Its own pointer already reaches [target], so letting the outer
       substitution take the access as well would apply the outer pointer
       once here and once again through the binder. *)
    let owned (bound : Variable.Set.t) (p : Exp.nexp Field_path.t) : bool =
      Variable.Set.exists
        (fun x -> Option.is_some (Field_path.under ~root:x p))
        bound
    in
    let rec resolve (bound : Variable.Set.t) : t -> t = function
      | Access a as i -> (
          if owned bound a.path then i
          else
            match Field_path.under ~root:target a.path with
            | Some path -> rewrite { a with path }
            | None -> i)
      | Decl (d, l) as i ->
          if Variable.equal d.var target then i else Decl (d, resolve bound l)
      | Assign a as i ->
          if Variable.equal a.var target then i
          else Assign { a with body = resolve bound a.body }
      | For (r, s) as i ->
          if Variable.equal r.var target then i else For (r, resolve bound s)
      | PointerBind p ->
          let pointer = graft p.pointer in
          if Variable.equal p.var target then PointerBind { p with pointer }
          else
            PointerBind
              {
                p with
                pointer;
                body = resolve (Variable.Set.add p.var bound) p.body;
              }
      | If (b, s1, s2) -> If (b, resolve bound s1, resolve bound s2)
      | Seq (p, q) -> Seq (resolve bound p, resolve bound q)
      | Call (c, s) -> Call (Call.resolve ~target pointer c, resolve bound s)
      | (Assert _ | Sync _ | Skip) as i -> i
    in
    fun s ->
      match Pointer.to_array pointer with
      | Some x when Variable.equal x target -> s
      | _ -> resolve Variable.Set.empty s

  let read_addresses (arrays : Memory.t Variable.Map.t) : t -> t =
    let open State.Syntax in
    let element (p : Exp.nexp Field_path.t) : Ty.t =
      Variable.Map.find_opt (Field_path.to_variable p) arrays
      |> Option.map (fun (m : Memory.t) ->
             Ty.of_c_string (String.concat " " m.data_type))
      |> Option.value ~default:Ty.unknown
    in
    let fresh : (int, Variable.t) State.t =
      State.update_return (fun n ->
          (n + 1, Variable.from_name ("@addr" ^ string_of_int n)))
    in
    let rec reads (index : Exp.nexp list)
        (l : (Exp.nexp Field_path.t * Exp.nexp list) list) :
        (int, (Mem_access.t * Variable.t * Ty.t) list * Exp.nexp list) State.t =
      match l with
      | [] -> State.return ([], index)
      | (storage, cell) :: l ->
          let* x = fresh in
          let rd =
            Mem_access.make ~path:storage ~index:(index @ cell) ~mode:Read
          in
          let* below, index = reads [ Exp.Var x ] l in
          State.return ((rd, x, element storage) :: below, index)
    in
    let stored (l : (Exp.nexp Field_path.t * Exp.nexp list) list) : bool =
      List.for_all
        (fun (storage, _) ->
          Variable.Map.mem (Field_path.to_variable storage) arrays)
        l
    in
    let rewrite (a : Mem_access.t) : (int, t) State.t =
      match Field_path.crossings a.path with
      | [], _ -> State.return (Access a)
      | l, _ when not (stored l) -> State.return (Access a)
      | l, offset ->
          let* l, address = reads [] l in
          let body =
            Access
              {
                a with
                path = Field_path.without_subscripts a.path;
                index = address @ offset @ a.index;
              }
          in
          State.return
            (List.fold_right
               (fun (rd, x, ty) (s : t) ->
                 Seq (Access rd, Decl (Decl.unset ~ty x, s)))
               l body)
    in
    let rec walk : t -> (int, t) State.t = function
      | Access a -> rewrite a
      | Seq (p, q) ->
          let* p = walk p in
          let* q = walk q in
          State.return (Seq (p, q))
      | If (b, p, q) ->
          let* p = walk p in
          let* q = walk q in
          State.return (If (b, p, q))
      | For (r, p) ->
          let* p = walk p in
          State.return (For (r, p))
      | Decl (d, p) ->
          let* p = walk p in
          State.return (Decl (d, p))
      | Call (c, p) ->
          let* p = walk p in
          State.return (Call (c, p))
      | Assign a ->
          let* body = walk a.body in
          State.return (Assign { a with body })
      | PointerBind p ->
          let* body = walk p.body in
          State.return (PointerBind { p with body })
      | (Assert _ | Sync _ | Skip) as i -> State.return i
    in
    fun s -> State.run (walk s) 0 |> snd

  let unnamed_access (locs : Variable.Set.t) (s : t) :
      (Stage0.Location.t * Variable.t) option =
    let roots =
      locs |> Variable.Set.elements
      |> List.map (fun x -> Field_path.base (Field_path.parse x))
      |> Variable.Set.of_list
    in
    let severed (d : Decl.t) : bool =
      Option.is_none d.init && Ty.is_array_or_pointer d.ty
    in
    let rooted (roots : Variable.Set.t) (p : Exp.nexp Field_path.t) : bool =
      Variable.Set.mem (Field_path.base p) roots
    in
    let rec walk (roots : Variable.Set.t) :
        t -> (Stage0.Location.t * Variable.t) option = function
      | Access a ->
          if
            Variable.Set.mem (Mem_access.array a) locs
            || not (rooted roots a.path)
          then None
          else Some (Mem_access.location a, Mem_access.array a)
      | Seq (p, q) | If (_, p, q) -> (
          match walk roots p with Some _ as r -> r | None -> walk roots q)
      | Decl (d, p) ->
          walk (if severed d then Variable.Set.add d.var roots else roots) p
      | For (_, p) | Call (_, p) -> walk roots p
      | Assign a -> walk roots a.body
      | PointerBind p -> walk roots p.body
      | Assert _ | Sync _ | Skip -> None
    in
    walk roots s

  (* Discharge every pointer binding, which is the erasure that leaves a
     protocol naming only arrays. Inside out: a pointer taken from another
     is resolved first, so by the time the outer binding runs the inner one
     already speaks in the outer's terms. [resolve] substitutes an open
     expression into a scope it was not written in, so this must run on a
     term whose binders are distinct. *)
  let resolve_pointers ~(arrays : Memory.t Variable.Map.t) : t -> t =
    let rec walk : t -> t = function
      | PointerBind { var; pointer; body } ->
          resolve ~arrays ~target:var pointer (walk body)
      | Decl (d, p) -> Decl (d, walk p)
      | Assign a -> Assign { a with body = walk a.body }
      | If (b, p, q) -> If (b, walk p, walk q)
      | For (r, p) -> For (r, walk p)
      | Seq (p, q) -> Seq (walk p, walk q)
      | Call (c, p) -> Call (c, walk p)
      | (Access _ | Assert _ | Sync _ | Skip) as i -> i
    in
    walk

  module SubstMake (S : Subst.SUBST) = struct
    module M = Subst.Make (S)

    let o_subst (st : S.t) : Exp.nexp option -> Exp.nexp option = function
      | Some n -> Some (M.n_subst st n)
      | None -> None

    let rec subst (st : S.t) : t -> t = function
      | Access a -> Access (Mem_access.map (M.n_subst st) a)
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
      | PointerBind p ->
          PointerBind
            {
              p with
              pointer =
                Pointer.map ~n:(M.n_subst st) ~b:(M.b_subst st) p.pointer;
              body =
                M.add st p.var (function
                  | Some st' -> subst st' p.body
                  | None -> p.body);
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
    | Access ({ mode = Write _ | Atomic _; _ } as a) ->
        Variable.Set.add (Mem_access.array a) acc
    | Call (c, p) ->
        Call.arrays c |> Variable.Set.of_list |> Variable.Set.union acc
        |> fun acc -> written_arrays acc p
    | If (_, p, q) | Seq (p, q) -> written_arrays (written_arrays acc p) q
    | For (_, p) | Decl (_, p) | Assign { body = p; _ }
    | PointerBind { body = p; _ } ->
        written_arrays acc p
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
    let read_call (v : Version.t) (ty : Ty.t) (array : Variable.t)
        (index : Exp.nexp list) : Exp.nexp =
      Exp.ReadResult
        {
          array;
          version = Version.get array v;
          ty = Ty.to_scalar ty;
          address = Ty.is_pointer ty;
          args = index;
        }
    in
    let rec rewrite (looped : Variable.Set.t) (v : Version.t) :
        t -> t * Version.t = function
      | Seq
          ( (Access ({ index; mode = Read; _ } as a) as acc),
            Decl ((({ init = None; _ } : Decl.t) as d), rest) )
        when not (Variable.Set.mem (Mem_access.array a) looped) ->
          let call = read_call v d.ty (Mem_access.array a) index in
          let rest, v = rewrite looped v rest in
          (Seq (acc, Decl ({ d with init = Some call }, rest)), v)
      | Seq
          ( If (b, (Access ({ index; mode = Read; _ } as a) as acc), Skip),
            Decl ((({ init = None; _ } : Decl.t) as d), rest) )
        when not (Variable.Set.mem (Mem_access.array a) looped) ->
          let call = read_call v d.ty (Mem_access.array a) index in
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
      | PointerBind p ->
          let body, v = rewrite looped v p.body in
          (PointerBind { p with body }, v)
      | Call (c, p) ->
          let v = Call.arrays c |> List.fold_left (Fun.flip Version.bump) v in
          let p, v = rewrite looped v p in
          (Call (c, p), v)
      | Access ({ mode = Write _ | Atomic _; _ } as a) as p ->
          (p, Version.bump (Mem_access.array a) v)
      | (Access _ | Assert _ | Sync _ | Skip) as p -> (p, v)
    in
    fun p -> rewrite Variable.Set.empty Version.empty p |> fst

  (* Only keep accesses that mention an array in the set *)
  let filter_locs (locs : Variable.Set.t) : t -> t =
    let rec filter : t -> t = function
      | Access a as i ->
          if Variable.Set.mem (Mem_access.array a) locs then i else Skip
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
      | PointerBind p -> PointerBind { p with body = filter p.body }
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
    let collect_arg (acc : Variable.Set.t) (a : Exp.nexp) : Variable.Set.t =
      n a acc
    in
    let rec go (acc : Variable.Set.t) : t -> Variable.Set.t = function
      | Skip -> acc
      | Sync s ->
          let acc = n s.id acc in
          (match s.participants with Some e -> n e acc | None -> acc)
      | Assert a -> b a.cond acc
      | Access a ->
          let acc = Variable.Set.add (Mem_access.array a) acc in
          List.fold_left (fun acc e -> n e acc) acc a.index
      | Decl (d, p) ->
          let acc = Variable.Set.add d.var acc in
          let acc = match d.init with Some e -> n e acc | None -> acc in
          go acc p
      | Assign { var; data; body; _ } ->
          let acc = Variable.Set.add var acc in
          go (n data acc) body
      | PointerBind { var; pointer; body } ->
          let acc = Variable.Set.add var acc in
          let acc = Variable.Set.union (Pointer.arrays pointer) acc in
          go (Pointer.free_names pointer acc) body
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
      | Access a -> (Access (Mem_access.map (ns env) a), bound, taken)
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
          (* [a.var] binds over the body exactly as a [Decl] does, so it
             is freshened on the same path. Without that this pass leaves
             the convention it is named for false for assignments, and a
             substitution into the body captures. *)
          let data = ns env a.data in
          let x, env, bound, taken = enter env bound taken a.var in
          let body, bound, taken = go env bound taken a.body in
          (Assign { a with var = x; data; body }, bound, taken)
      (* The pointer's expressions are read here, so they take the
         renaming in force at this point. The bound name itself is not
         renamed: a pointer is used as an [Access]'s array, and expression
         substitution reaches only an access's indices, so a fresh name
         here would part the binder from its uses. What a rebind of the
         same name needs instead is [resolve] stopping at it, which is the
         shadowing clause, and elimination running inside out. *)
      | PointerBind p ->
          let pointer = Pointer.map ~n:(ns env) ~b:(bs env) p.pointer in
          let bound = Variable.Set.add p.var bound in
          let body, bound, taken = go env bound taken p.body in
          (PointerBind { p with pointer; body }, bound, taken)
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
      | PointerBind p ->
          let assigns, body = fix_assigns defined p.body in
          (assigns, PointerBind { p with body })
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
          let defined = Params.add r.var Ty.int defined in
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
        ty = Scalar.int;
      }

  let atomic_result_marker (aw : Atomic_write.t) : Assert.t =
    let marker =
      Exp.AtomicResult
        {
          target = aw.target;
          array = Atomic_write.array aw;
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
    let add_global (x : Variable.t) ?(ty = Ty.char) () : unit state =
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
          return (PointerBind { var = e.target; pointer = e.pointer; body = s })
      | Seq (Decl d, p) ->
          let* s = imp_to_scoped p in
          return (Decl (d, s))
      | Seq (Assign { var; data; ty }, p) ->
          let* body = imp_to_scoped p in
          return (Assign { var; data; ty; body })
      | Seq (Read e, s) ->
          let* s = imp_to_scoped s in
          let rd =
            Access (Mem_access.make ~path:e.path ~index:e.index ~mode:Read)
          in
          let rd = match e.guard with Some g -> If (g, rd, Skip) | None -> rd in
          return
            (match e.target with
            | Some (ty, x) -> Seq (rd, Decl (Decl.unset ~ty x, s))
            | None -> Seq (rd, s))
      | Seq (Atomic e, s) ->
          let* s = imp_to_scoped s in
          let a =
            Access
              (Mem_access.make ~path:e.path ~index:e.index
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
            Access
              (Mem_access.make ~path:e.path ~index:e.index
                 ~mode:(Write e.payload))
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
      let s = Join_pointers.from_stmt s in
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
    id : Function_id.t;
    parameters : ParameterList.t;
    global_arrays : Memory.t Variable.Map.t;
    global_variables : Params.t;
    code : Code.t;
    return : Exp.nexp option;
    visibility : Visibility.t;
    grid_dim : Dim3.t option;
    block_dim : Dim3.t option;
    unsupported : Rejected_kernel.Reason.t option;
  }

  let local_set (k : t) : Variable.Set.t = ParameterList.to_set k.parameters
  let global_set (k : t) : Variable.Set.t = Params.to_set k.global_variables

  (* Merge globally-defined arrays and arrays defined in parameters. *)
  let array_map (k : t) : Memory.t Variable.Map.t =
    k.global_arrays
    |> Variable.MapUtil.union_left (ParameterList.to_arrays k.parameters)

  let arrays (k : t) : Variable.Set.t =
    k |> array_map |> Variable.MapSetUtil.map_to_set

  let variable_set (k : t) : Variable.Set.t =
    Variable.Set.union (local_set k) (global_set k)

  let unique_id (k : t) : Function_id.t = k.id
  let name (k : t) : string = Function_id.label k.id
  let calls (k : t) : Function_id.Set.t = Code.calls k.code

  let from_imp (k : Kernel.t) : t =
    let globals =
      (* Take the global variables and the scalars defined in the paramter list *)
      k.global_variables
      |> Params.union_left (ParameterList.to_params k.parameters)
    in
    (* Add any globals defined from scoped *)
    let globals, p = Code.from_stmt (globals, k.code) in
    {
      id = k.id;
      parameters = k.parameters;
      global_arrays = k.global_arrays;
      global_variables = globals;
      code = p;
      return = k.return;
      visibility = k.visibility;
      grid_dim = k.grid_dim;
      block_dim = k.block_dim;
      unsupported = k.unsupported;
    }

  let is_global (k : t) : bool = k.visibility = Visibility.Global

  let to_string (k : t) : string =
    Printf.sprintf "%s %s (%s)\nglobal {arrays: %s} {scalars: %s}\n{\n%s}\n"
      (Visibility.to_string k.visibility)
      (name k)
      (ParameterList.to_string k.parameters)
      (Memory.map_to_string k.global_arrays)
      (Params.to_string k.global_variables)
      (Code.to_string k.code)

  let print (k : t) : unit = print_string (to_string k)
end
