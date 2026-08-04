open Stage0
module IdMap = Function_id.Map
module IdSet = Function_id.Set
module K = Kernel

type t = {
  kernels : Scoped.Kernel.t IdMap.t;
  targets : IdSet.t IdMap.t;
      (* For each kernel which other kernels it is calling *)
  visited : IdSet.t;
}

let key_set (s : 'a IdMap.t) : IdSet.t =
  s |> IdMap.bindings |> List.map fst |> IdSet.of_list

let to_string (s : t) : string =
  let id_set (s : IdSet.t) =
    "["
    ^ (IdSet.elements s |> List.map Function_id.to_string |> String.concat ", ")
    ^ "]"
  in
  "{\n" ^ "\tkernels = "
  ^ (key_set s.kernels |> id_set)
  ^ "\n" ^ "\ttargets = "
  ^ String.concat ", "
      (s.targets |> IdMap.bindings
      |> List.map (fun (k, v) -> Function_id.to_string k ^ "=" ^ id_set v))
  ^ "\n" ^ "\tvisited = " ^ id_set s.visited ^ "\n" ^ "}"

module Inline = struct
  module Variable = Protocols.Variable
  module Ty = Protocols.Ty

  let apply ~(arrays : Protocols.Memory.t Variable.Map.t)
      (vars : Variable.Set.t) (result : (Variable.t * Ty.t) option)
      (args : Protocols.Exp.nexp list) (k : Scoped.Kernel.t)
      (s : Scoped.Code.t) : Scoped.Code.t =
    let open Scoped.Code in
    let array_set = Variable.MapSetUtil.map_to_set arrays in
    let bind_result (tail : Scoped.Code.t) : Scoped.Code.t =
      match (result, k.return) with
      | Some (var, ty), Some data ->
          (*
            var x in {
              k.body;
              x := k.return;
            }
            When the callee returns a bare struct variable [r] (e.g. a
            vector-typed return), its fields are mangled [r.field]
            locals; the scalar copy [x := r] does not connect them to
            the caller's [x.field] reads, so bind each field
            [x.field := r.field] instead.
          *)
          let fields =
            match data with
            | Protocols.Exp.Var r ->
                let base = Variable.name r in
                let prefix = base ^ "." in
                Scoped.Code.mentioned k.code
                |> Variable.Set.elements
                |> List.filter_map (fun m ->
                     let name = Variable.name m in
                     if String.starts_with ~prefix name then
                       Some
                         ( m,
                           String.sub name (String.length base)
                             (String.length name - String.length base) )
                     else None)
            | _ -> []
          in
          (* A callee handing back a pointer hands back a location, and a
             value copy of it names nothing the caller can index. The
             expression is in the callee's terms here, so the base is its
             own parameter; the argument binding grafts the caller's
             memory onto it afterwards, which a [Decl]'s initialiser
             would not receive. *)
          let pointer =
            if Ty.is_array_or_pointer ty then
              Array_use.from_nexp ~arrays:array_set data
              |> Option.map (fun (u : Array_use.t) ->
                     Pointer.from_array u.array
                     |> Pointer.shift
                          ~offset:(Pointer.Offset.elements u.offset))
            else None
          in
          (match (pointer, fields) with
           | Some pointer, _ -> PointerBind { var; pointer; body = tail }
           | None, [] -> decl_set ~ty var data tail
           | None, _ ->
               List.fold_left
                 (fun tail (m, suffix) ->
                   let dst = Variable.update_name (fun n -> n ^ suffix) var in
                   decl_set dst (Protocols.Exp.Var m) tail)
                 tail fields)
      (* TODO: | Some (var, ty), None -> *)
      | _, _ -> tail
    in
    (* Alpha-rename callee internal binders that clash with the
       caller's variable set BEFORE substituting parameters.
       Running [vars_distinct] after parameter substitution would
       let its capture-blind [subst] rewrite caller-side free
       variables baked into the body by [loc_subst]/[decl_set]
       (e.g. an [arr + i] argument carrying caller's [i] into
       accesses).

       Parameter [Decl]s introduced by substitution must also avoid
       shadowing caller-scope names: when a parameter name [x]
       collides with [vars], rename it to a fresh [x'] and substitute
       [x ↦ x'] in the body in the same step. The substitution is
       scope-aware (handled by [Scoped.Code.subst]'s [M.add] on inner
       rebinders), so only free [x] references in the body — the
       parameter references themselves — are rewritten. *)
    let rename_param (vars : Variable.Set.t) (x : Variable.t)
        (s : Scoped.Code.t) : Variable.t * Scoped.Code.t =
      if Variable.Set.mem x vars then
        let x' = Variable.fresh vars x in
        (x', Scoped.Code.subst (x, Var x') s)
      else (x, s)
    in
    k.code
    |> Scoped.Code.vars_distinct ~vars
    (* The result names what the callee returned, which is written in the
       callee's own terms, so it has to sit where the parameter bindings
       below can reach it. The caller's continuation goes in afterwards,
       where those bindings cannot rewrite the names it brought with it. *)
    |> Scoped.Code.add_inside ~child:(bind_result Skip)
    (* prepend the assignments of arguments to parameters *)
    |> List.fold_right
         (fun ((x, p_ty), a) s ->
           let open Scoped.Code in
           let ty = K.Parameter.Type.to_c_type p_ty in
           match Classify_arg.classify ~arrays:array_set p_ty a with
           (* A vector argument [v] passed to a vector parameter [x]:
              bind each lane [x.axis := v.axis] so the callee's
              per-lane reads resolve to the caller's value. *)
           | Arg.Scalar (Protocols.Exp.Var v)
             when Ty.vector_lanes ty <> None ->
               let axes = Option.get (Ty.vector_lanes ty) in
               List.fold_left
                 (fun s axis ->
                   let lane = Variable.update_name (fun n -> n ^ "." ^ axis) in
                   decl_set (lane x) (Protocols.Exp.Var (lane v)) s)
                 s axes
           | Arg.Scalar e ->
               let x, s = rename_param vars x s in
               decl_set ~ty x e s
           | Arg.Unsupported _ ->
               let x, s = rename_param vars x s in
               decl_unset ~ty x s
           | Arg.Array u ->
               (* The callee indexes in its parameter's step and the caller
                  supplied an array in its own, which is where a [void]
                  pointer parameter over an [int] array gets its 1 against
                  4. *)
               let param =
                 match p_ty with
                 | K.Parameter.Type.Array m -> Some m
                 | _ -> None
               in
               let caller = Variable.Map.find_opt u.array arrays in
               let view = param |> Option.map Protocols.Memory.step |> Option.join in
               let elem =
                 caller |> Option.map Protocols.Memory.step |> Option.join
               in
               (* The parameter reads the memory as an object the caller's
                  array is not shaped like, so the axes it indexes by are
                  the object's rather than the array's, and they collapse
                  into the one index the array takes. Divisibility is what
                  says the two agree on where a cell starts; without it the
                  binding is left alone and the access goes unnamed. *)
               let flatten (p : Pointer.t) : Pointer.t option =
                 let open Protocols in
                 match (param, caller, elem) with
                 | Some param, Some caller, Some w
                   when Option.is_none (Memory.layout caller) ->
                     Memory.layout param
                     |> Fun.flip Option.bind (fun (l : Memory.Layout.t) ->
                            let exact (n : int) : int option =
                              if w > 0 && n mod w = 0 then Some (n / w) else None
                            in
                            let scale = List.map exact l.strides in
                            if List.for_all Option.is_some scale then
                              exact l.offset
                              |> Option.map (fun shift ->
                                     Pointer.linear
                                       ~scale:(List.filter_map Fun.id scale)
                                       ~shift:(Exp.Num shift) p)
                            else None)
                 | _ -> None
               in
               let offset =
                 match (view, elem) with
                 | Some view, Some elem ->
                     Pointer.Offset.bytes ~amount:u.offset
                       ~step:(Pointer.Step.make ~view ~elem)
                 | _ -> Pointer.Offset.elements u.offset
               in
               let base = Pointer.from_array u.array in
               let in_cells =
                 match elem with
                 | Some w ->
                     Pointer.Offset.bytes ~amount:u.offset
                       ~step:(Pointer.Step.make ~view:w ~elem:w)
                 | None -> Pointer.Offset.elements u.offset
               in
               let pointer =
                 match flatten (Pointer.shift ~offset:in_cells base) with
                 | Some pointer -> pointer
                 | None -> Pointer.shift ~offset base
               in
               Scoped.Code.resolve ~arrays ~target:x pointer s)
         (Common.zip k.parameters args)
    (* then add inside the child, meaning that the free-variables of the
       outer-context are preserved  *)
    |> Scoped.Code.add_inside ~child:s

  let inline_stmt ~(arrays : Protocols.Memory.t Variable.Map.t)
      (funcs : Scoped.Kernel.t IdMap.t) :
      Variable.Set.t -> Scoped.Code.t -> Scoped.Code.t =
    let rec inline (vars : Variable.Set.t) : Scoped.Code.t -> Scoped.Code.t =
      function
      | Call (c, s) -> (
          let vars =
            match c.result with
            | Some (x, _) -> Variable.Set.add x vars
            | None -> vars
          in
          (* Inline the continuation first, then this call. The
             continuation is the rest of the call chain in [Scoped.Code]
             (e.g. [Call (c1, Call (c2, Call (c3, ...)))]); without
             recursing into [s] here, only the head call ever gets
             inlined and the tail remains as opaque [Call] nodes. *)
          let s = inline vars s in
          match IdMap.find_opt (Call.unique_id c) funcs with
          | Some (k : Scoped.Kernel.t)
            when List.length k.parameters = List.length c.args ->
              apply ~arrays vars c.result c.args k s
          | Some k ->
              (* The two lists are built from one description of the
                 callee's type, so a difference is a bug here rather than
                 a program faial cannot analyze. *)
              failwith
                (Printf.sprintf
                   "Inline_calls: %s takes %d parameters and the call passes \
                    %d arguments."
                   (Scoped.Kernel.name k)
                   (List.length k.parameters)
                   (List.length c.args))
          | None -> Call (c, s))
      | Seq (p, q) -> Seq (inline vars p, inline vars q)
      | If (b, s1, s2) -> If (b, inline vars s1, inline vars s2)
      | For (r, s) -> For (r, inline (Variable.Set.add r.var vars) s)
      | Decl (d, s) -> Decl (d, inline (Variable.Set.add d.var vars) s)
      | PointerBind p ->
          PointerBind
            { p with body = inline (Variable.Set.add p.var vars) p.body }
      | Assign a -> Assign { a with body = inline vars a.body }
      | (Sync _ | Assert _ | Access _ | Skip) as s -> s
    in
    inline
end

let inline (funcs : Scoped.Kernel.t IdMap.t) (k : Scoped.Kernel.t) :
    Scoped.Kernel.t =
  {
    k with
    code =
      Inline.inline_stmt ~arrays:(Scoped.Kernel.array_map k) funcs
        (Scoped.Kernel.variable_set k) k.code;
  }

let inline_kernels (kernels : IdSet.t) (s : t) : t =
  (* Get the code of the kernels to call *)
  let leaves =
    IdMap.filter (fun k _ -> IdSet.mem k kernels) s.kernels
  in
  let leaf_set = key_set leaves in
  (* Get the set of all kernels that call `kernel` *)
  let to_inline : IdSet.t =
    s.targets
    |> IdMap.filter (fun _ x ->
        (* any kernel that depends on a leaf *)
        not (IdSet.is_empty (IdSet.inter leaf_set x)))
    |> key_set
  in
  {
    (* inline each call to a leaf *)
    kernels =
      IdMap.mapi
        (fun name k ->
          (* if this kernel calls any of the leaves *)
          if IdSet.mem name to_inline then
            (* Inline leaves in k *)
            inline leaves k
          else
            (* nothing to do, leave kernel as is *)
            k)
        s.kernels;
    (* remove the leaves from all dependencies *)
    targets = IdMap.map (fun s -> IdSet.diff s leaf_set) s.targets;
    (* add leaves to the set of all visited *)
    visited = IdSet.union leaf_set s.visited;
  }

(* Calculate the set of next possible kernels to inline *)
let next (s : t) : IdSet.t =
  let possible =
    s.targets |> IdMap.filter (fun _ ts -> IdSet.is_empty ts) |> key_set
  in
  IdSet.diff possible s.visited

let unresolved (s : t) : IdSet.t =
  s.targets
  |> IdMap.filter (fun _ ts -> not (IdSet.is_empty ts))
  |> key_set

(* The path runs from [start] to the kernel that closes the cycle, which is
   repeated as the last element. *)
let path_to_cycle (s : t) (start : Function_id.t) : Function_id.t list option =
  let rec walk (seen : Function_id.t list) (node : Function_id.t) :
      Function_id.t list option =
    if List.exists (Function_id.equal node) seen then
      Some (List.rev (node :: seen))
    else
      IdMap.find_opt node s.targets
      |> Option.value ~default:IdSet.empty
      |> IdSet.elements
      |> List.find_map (walk (node :: seen))
  in
  walk [] start

(* The path runs from [start] to a callee that is not a kernel we hold,
   which is repeated as the last element. A call node whose callee has no
   entry in [kernels] is a call to a function with no visible body: the
   front end recorded the call precisely so that it would be missing
   here. *)
let path_to_undefined (s : t) (start : Function_id.t) :
    Function_id.t list option =
  let rec walk (seen : Function_id.t list) (node : Function_id.t) :
      Function_id.t list option =
    if not (IdMap.mem node s.kernels) then Some (List.rev (node :: seen))
    else if List.exists (Function_id.equal node) seen then None
    else
      IdMap.find_opt node s.targets
      |> Option.value ~default:IdSet.empty
      |> IdSet.elements
      |> List.find_map (walk (node :: seen))
  in
  walk [] start

(* A kernel that inlines an unsupported one takes on its construct, so the
   reason travels the call edges the way an undefined callee's does. *)
let path_to_unsupported (s : t) (start : Function_id.t) :
    Rejected_kernel.Reason.t option =
  let rec walk (seen : Function_id.t list) (node : Function_id.t) :
      Rejected_kernel.Reason.t option =
    match IdMap.find_opt node s.kernels with
    | None -> None
    | Some k -> (
        match k.Scoped.Kernel.unsupported with
        | Some r -> Some r
        | None ->
            if List.exists (Function_id.equal node) seen then None
            else
              IdMap.find_opt node s.targets
              |> Option.value ~default:IdSet.empty
              |> IdSet.elements
              |> List.find_map (walk (node :: seen)))
  in
  walk [] start

let from_list (ks : Scoped.Kernel.t list) : t =
  {
    targets =
      ks
      |> List.map (fun k -> (Scoped.Kernel.unique_id k, Scoped.Kernel.calls k))
      |> IdMap.of_list;
    kernels =
      ks
      |> List.map (fun k -> (Scoped.Kernel.unique_id k, k))
      |> IdMap.of_list;
    visited = IdSet.empty;
  }

let kernel_list (s : t) : Scoped.Kernel.t list =
  s.kernels |> IdMap.bindings |> List.map snd

let rec inline_all (s : t) : t =
  let n = next s in
  if IdSet.is_empty n then
    (* we are done *)
    s
  else
    (* inline more *)
    inline_all (inline_kernels n s)

(* Every kernel whose calls did not all resolve is discarded, not just the
   recursive ones. Keeping one would leave a [Call] node in its body, and
   [Encode_assigns] drops such a node without trace, so the kernel would be
   analyzed as a strict subset of what was written. *)
let inline_calls (l : Scoped.Kernel.t list) :
    Scoped.Kernel.t list * Rejected_kernel.t list =
  let calls = from_list l in
  let s = inline_all calls in
  let discarded = unresolved s in
  let reason (id : Function_id.t) : Rejected_kernel.Reason.t =
    let names = List.map Function_id.label in
    match path_to_cycle s id with
    | Some path -> RecursiveCall { path = names path }
    | None -> (
        match path_to_undefined s id with
        | Some path -> UndefinedKernel { path = names path }
        | None -> UndefinedKernel { path = [ Function_id.label id ] })
  in
  let unsupported =
    kernel_list s
    |> List.filter_map (fun k ->
        let id = Scoped.Kernel.unique_id k in
        if IdSet.mem id discarded then None
        else path_to_unsupported calls id |> Option.map (fun r -> (id, r)))
  in
  let dropped =
    IdSet.union discarded (unsupported |> List.map fst |> IdSet.of_list)
  in
  let kernels =
    kernel_list s
    |> List.filter (fun k ->
        not (IdSet.mem (Scoped.Kernel.unique_id k) dropped))
  in
  let rejected =
    (discarded |> IdSet.elements
     |> List.map (fun id ->
          Rejected_kernel.make ~kernel:(Function_id.label id)
            ~reason:(reason id)))
    @ (unsupported
       |> List.map (fun (id, reason) ->
            Rejected_kernel.make ~kernel:(Function_id.label id) ~reason))
  in
  (kernels, rejected)
