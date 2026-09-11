module K = Kernel
open Protocols
open K
open Exp
open Rejected_kernel.Reason

let compile ?(rules = Idiom_rewrite.all) ?infer_cond_bound
    (k : Scoped.Kernel.t) : (Kernel.t, Rejected_kernel.t) Result.t =
  let reject (reason : Rejected_kernel.Reason.t) :
      (Kernel.t, Rejected_kernel.t) Result.t =
    Error (Rejected_kernel.make ~kernel:(Scoped.Kernel.name k) ~reason)
  in
  let arrays = Scoped.Kernel.array_map k in
  let resolved =
    k.code
    (* Pointer resolution substitutes an open expression into an already
       built scope, so the binders have to be distinct before it runs, and
       it has to run before [filter_locs] deletes the accesses whose array
       is still a pointer's name. *)
    |> Scoped.Code.vars_distinct ~vars:(ParameterList.to_set k.parameters)
    |> Scoped.Code.resolve_pointers ~arrays
  in
  let resolved = Scoped.Code.read_addresses arrays resolved in
  let array_set = Variable.MapSetUtil.map_to_set arrays in
  match Scoped.Code.unnamed_access array_set resolved with
  | Some (location, region) ->
      reject (UnnamedRegion { location; region = Variable.name region })
  | None ->
  let p =
    resolved
    |> Scoped.Code.filter_locs array_set
       (* Remove unknown arrays *)
    |> Scoped.Code.bind_uniform_reads
    |> Scoped.Code.fix_assigns
    (* Inline local variable assignment and ensure variables are distinct*)
    |> Encode_assigns.from_scoped ?infer_cond_bound
         (ParameterList.to_set k.parameters)
    |> Idiom_rewrite.rewrite rules
    |> Encode_asserts.from_encode_assigns
  in
  let p, locals, pre =
    let rec inline_header :
        Code.t * Params.t * bexp -> Code.t * Params.t * bexp
        =
     fun (p, locals, pre) ->
      match p with
      | If (b, p, Skip) -> inline_header (p, locals, b_and b pre)
      | Decl { var = x; body = p; ty; _ } ->
          inline_header (p, Params.add x ty locals, pre)
      | _ -> (p, locals, pre)
    in
    inline_header (p, Params.empty, Bool true)
  in
  (*
    1. We rename all variables so that they are all different
    2. We break down for-loops and variable declarations
    *)
  Ok
    (Kernel.reset_variable_kind
    {
      name = Scoped.Kernel.name k;
      pre;
      arrays;
      local_variables = locals;
      global_variables = k.global_variables;
      code = p;
      visibility = k.visibility;
      block_dim = k.block_dim;
      grid_dim = k.grid_dim;
    })

let compile_all ?(rules = Idiom_rewrite.all) ?infer_cond_bound
    ?only_kernel
    (l : K.t list) : Kernel.t list * Rejected_kernel.t list =
  let l =
    match only_kernel with
    | None -> l
    | Some name ->
        let module M = Function_id.Map in
        let module S = Function_id.Set in
        let by_id = List.map (fun k -> (K.unique_id k, k)) l |> M.of_list in
        let rec visit seen id =
          if S.mem id seen then seen
          else
            let seen = S.add id seen in
            match M.find_opt id by_id with
            | None -> seen
            | Some k -> S.fold (fun id seen -> visit seen id) (K.calls k) seen
        in
        let reachable =
          l |> List.filter (fun k -> K.name k = name)
          |> List.fold_left (fun seen k -> visit seen (K.unique_id k)) S.empty
        in
        List.filter (fun k -> S.mem (K.unique_id k) reachable) l
  in
  let l, rejected =
    l |> List.map Scoped.Kernel.from_imp |> Inline_calls.inline_calls
  in
  let compiled, declined =
    l
    |> List.map (compile ~rules ?infer_cond_bound)
    |> List.partition_map (function Ok k -> Left k | Error r -> Right r)
  in
  (compiled, rejected @ declined)
