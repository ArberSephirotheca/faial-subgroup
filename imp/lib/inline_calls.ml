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
  module C_type = Protocols.C_type

  let apply (vars : Variable.Set.t) (result : (Variable.t * C_type.t) option)
      (args : Arg.t list) (k : Scoped.Kernel.t) (s : Scoped.Code.t) :
      Scoped.Code.t =
    let open Scoped.Code in
    let s =
      match (result, k.return) with
      | Some (var, ty), Some data ->
          (*
            var x in {
              k.body;
              x := k.return;
            }
            *)
          decl_set ~ty var data s
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
         (fun ((x, ty), a) s ->
           let open Scoped.Code in
           let open Arg in
           match a with
           | Scalar e ->
               let x, s = rename_param vars x s in
               decl_set ~ty x e s
           | Unsupported ->
               let x, s = rename_param vars x s in
               decl_unset ~ty x s
           | Array u ->
               Scoped.Code.loc_subst
                 { target = x; source = u.array; offset = u.offset }
                 s)
         (Common.zip (K.ParameterList.to_c_type k.parameters) args)
    (* then add inside the child, meaning that the free-variables of the
       outer-context are preserved  *)
    |> Scoped.Code.add_inside ~child:s

  let inline_stmt (funcs : Scoped.Kernel.t StringMap.t) :
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
          | Some (k : Scoped.Kernel.t) -> apply vars c.result c.args k s
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
    code = Inline.inline_stmt funcs (Scoped.Kernel.variable_set k) k.code;
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

let inline_calls (l : Scoped.Kernel.t list) : Scoped.Kernel.t list =
  l |> from_list |> inline_all |> kernel_list
