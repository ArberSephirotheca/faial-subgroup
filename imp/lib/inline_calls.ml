open Stage0
module StringMap = Common.StringMap
module StringSet = Common.StringSet
module K = Kernel

type t = {
  kernels : Scoped.Kernel.t StringMap.t; (* Kernel name to kernel *)
  targets : StringSet.t StringMap.t;
      (* For each kernel which other kernels it is calling *)
  visited : StringSet.t;
}

let key_set (s : 'a StringMap.t) : StringSet.t =
  s |> StringMap.bindings |> List.map fst |> StringSet.of_list

let to_string (s : t) : string =
  let string_set (s : StringSet.t) =
    "[" ^ (StringSet.elements s |> String.concat ", ") ^ "]"
  in
  "{\n" ^ "\tkernels = "
  ^ (key_set s.kernels |> string_set)
  ^ "\n" ^ "\ttargets = "
  ^ String.concat ", "
      (s.targets |> StringMap.bindings
      |> List.map (fun (k, v) -> k ^ "=" ^ string_set v))
  ^ "\n" ^ "\tvisited = " ^ string_set s.visited ^ "\n" ^ "}"

module Inline = struct
  module Variable = Protocols.Variable
  module Ty = Protocols.Ty

  let apply ~(arrays : Variable.Set.t) (vars : Variable.Set.t)
      (result : (Variable.t * Ty.t) option) (args : Protocols.Exp.nexp list)
      (k : Scoped.Kernel.t) (s : Scoped.Code.t) : Scoped.Code.t =
    let open Scoped.Code in
    let s =
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
          (match fields with
           | [] -> decl_set ~ty var data s
           | _ ->
               List.fold_left
                 (fun s (m, suffix) ->
                   let dst = Variable.update_name (fun n -> n ^ suffix) var in
                   decl_set dst (Protocols.Exp.Var m) s)
                 s fields)
      (* TODO: | Some (var, ty), None -> *)
      | _, _ -> s
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
    (* prepend the assignments of arguments to parameters *)
    |> List.fold_right
         (fun ((x, p_ty), a) s ->
           let open Scoped.Code in
           let ty = K.Parameter.Type.to_c_type p_ty in
           match Classify_arg.classify ~arrays p_ty a with
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
               Scoped.Code.loc_subst
                 { target = x; source = u.array; offset = u.offset }
                 s)
         (Common.zip k.parameters args)
    (* then add inside the child, meaning that the free-variables of the
       outer-context are preserved  *)
    |> Scoped.Code.add_inside ~child:s

  let inline_stmt ~(arrays : Variable.Set.t)
      (funcs : Scoped.Kernel.t StringMap.t) :
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
          match StringMap.find_opt (Call.unique_id c) funcs with
          | Some (k : Scoped.Kernel.t) ->
              apply ~arrays vars c.result c.args k s
          | None -> Call (c, s))
      | Seq (p, q) -> Seq (inline vars p, inline vars q)
      | If (b, s1, s2) -> If (b, inline vars s1, inline vars s2)
      | For (r, s) -> For (r, inline (Variable.Set.add r.var vars) s)
      | Decl (d, s) -> Decl (d, inline (Variable.Set.add d.var vars) s)
      | Assign a -> Assign { a with body = inline vars a.body }
      | (Sync _ | Assert _ | Access _ | Skip) as s -> s
    in
    inline
end

let inline (funcs : Scoped.Kernel.t StringMap.t) (k : Scoped.Kernel.t) :
    Scoped.Kernel.t =
  {
    k with
    code =
      Inline.inline_stmt ~arrays:(Scoped.Kernel.arrays k) funcs
        (Scoped.Kernel.variable_set k) k.code;
  }

let inline_kernels (kernels : StringSet.t) (s : t) : t =
  (* Get the code of the kernels to call *)
  let leaves =
    StringMap.filter (fun k _ -> StringSet.mem k kernels) s.kernels
  in
  let leaf_set = key_set leaves in
  (* Get the set of all kernels that call `kernel` *)
  let to_inline : StringSet.t =
    s.targets
    |> StringMap.filter (fun _ x ->
        (* any kernel that depends on a leaf *)
        not (StringSet.is_empty (StringSet.inter leaf_set x)))
    |> key_set
  in
  {
    (* inline each call to a leaf *)
    kernels =
      StringMap.mapi
        (fun name k ->
          (* if this kernel calls any of the leaves *)
          if StringSet.mem name to_inline then
            (* Inline leaves in k *)
            inline leaves k
          else
            (* nothing to do, leave kernel as is *)
            k)
        s.kernels;
    (* remove the leaves from all dependencies *)
    targets = StringMap.map (fun s -> StringSet.diff s leaf_set) s.targets;
    (* add leaves to the set of all visited *)
    visited = StringSet.union leaf_set s.visited;
  }

(* Calculate the set of next possible kernels to inline *)
let next (s : t) : StringSet.t =
  let possible =
    s.targets |> StringMap.filter (fun _ ts -> StringSet.is_empty ts) |> key_set
  in
  StringSet.diff possible s.visited

let unresolved (s : t) : StringSet.t =
  s.targets
  |> StringMap.filter (fun _ ts -> not (StringSet.is_empty ts))
  |> key_set

(* The path runs from [start] to the kernel that closes the cycle, which is
   repeated as the last element. *)
let path_to_cycle (s : t) (start : string) : string list option =
  let rec walk (seen : string list) (node : string) : string list option =
    if List.mem node seen then Some (List.rev (node :: seen))
    else
      StringMap.find_opt node s.targets
      |> Option.value ~default:StringSet.empty
      |> StringSet.elements
      |> List.find_map (walk (node :: seen))
  in
  walk [] start

let kernel_name (s : t) (id : string) : string =
  match StringMap.find_opt id s.kernels with
  | Some k -> k.Scoped.Kernel.name
  | None -> id

let recursive (s : t) : (string * string list) list =
  unresolved s |> StringSet.elements
  |> List.filter_map (fun id ->
       path_to_cycle s id |> Option.map (fun path -> (id, path)))

let from_list (ks : Scoped.Kernel.t list) : t =
  {
    targets =
      ks
      |> List.map (fun k -> (Scoped.Kernel.unique_id k, Scoped.Kernel.calls k))
      |> StringMap.of_list;
    kernels =
      ks
      |> List.map (fun k -> (Scoped.Kernel.unique_id k, k))
      |> StringMap.of_list;
    visited = StringSet.empty;
  }

let kernel_list (s : t) : Scoped.Kernel.t list =
  s.kernels |> StringMap.bindings |> List.map snd

let rec inline_all (s : t) : t =
  let n = next s in
  if StringSet.is_empty n then
    (* we are done *)
    s
  else
    (* inline more *)
    inline_all (inline_kernels n s)

let inline_calls (l : Scoped.Kernel.t list) :
    Scoped.Kernel.t list * Rejected_kernel.t list =
  let s = l |> from_list |> inline_all in
  let found = recursive s in
  let discarded = found |> List.map fst |> StringSet.of_list in
  let kernels =
    kernel_list s
    |> List.filter (fun k ->
        not (StringSet.mem (Scoped.Kernel.unique_id k) discarded))
  in
  let rejected =
    found
    |> List.map (fun (id, path) ->
         Rejected_kernel.make ~kernel:(kernel_name s id)
           ~reason:(Rejected_kernel.Reason.RecursiveCall
                      { path = List.map (kernel_name s) path }))
  in
  (kernels, rejected)
