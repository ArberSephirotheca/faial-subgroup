open Kernel
module Variable = Protocols.Variable
module Params = Protocols.Params
module Exp = Protocols.Exp
open Exp

let compile ?(rules = Idiom_rewrite.all) ?infer_cond_bound
    (k : Scoped.Kernel.t) : Protocols.Kernel.t =
  (* Merge globally-defined arrays and arrays defined in parameters. *)
  let arrays =
    k.global_arrays
    |> Variable.MapUtil.union_left (ParameterList.to_arrays k.parameters)
  in
  let array_set = arrays |> Variable.MapSetUtil.map_to_set in
  let p =
    k.code
    |> Scoped.Code.filter_locs array_set
       (* Remove unknown arrays *)
    |> (fun c ->
         let read_only = Variable.Set.diff array_set (Scoped.Code.rw_arrays c) in
         Scoped.Code.bind_uniform_reads read_only c)
    |> Scoped.Code.fix_assigns
    (* Inline local variable assignment and ensure variables are distinct*)
    |> Encode_assigns.from_scoped ?infer_cond_bound
         (ParameterList.to_set k.parameters)
    |> Idiom_rewrite.rewrite rules
    |> Encode_asserts.from_encode_assigns
  in
  let p, locals, pre =
    let rec inline_header :
        Protocols.Code.t * Params.t * bexp -> Protocols.Code.t * Params.t * bexp
        =
     fun (p, locals, pre) ->
      match p with
      | If (b, p, Skip) -> inline_header (p, locals, b_and b pre)
      | Decl { var = x; body = p; ty } ->
          inline_header (p, Params.add x ty locals, pre)
      | _ -> (p, locals, pre)
    in
    inline_header (p, Params.empty, Bool true)
  in
  (*
    1. We rename all variables so that they are all different
    2. We break down for-loops and variable declarations
    *)
  {
    name = k.name;
    pre;
    arrays;
    local_variables = locals;
    global_variables = k.global_variables;
    code = p;
    visibility = k.visibility;
    block_dim = k.block_dim;
    grid_dim = k.grid_dim;
  }

let compile_all ?(rules = Idiom_rewrite.all) ?(inline_calls = true)
    ?infer_cond_bound (l : Kernel.t list) : Protocols.Kernel.t list =
  let l = List.map Scoped.Kernel.from_imp l in
  let l = if inline_calls then Inline_calls.inline_calls l else l in
  List.map (compile ~rules ?infer_cond_bound) l
