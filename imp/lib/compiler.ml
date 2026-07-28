open Kernel
module Variable = Protocols.Variable
module Params = Protocols.Params
module Exp = Protocols.Exp
open Exp

let compile ?(rules = Idiom_rewrite.all) ?infer_cond_bound
    (k : Scoped.Kernel.t) : Protocols.Kernel.t =
  let arrays = Scoped.Kernel.array_map k in
  let array_set = Scoped.Kernel.arrays k in
  let p =
    k.code
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
        Protocols.Code.t * Params.t * bexp -> Protocols.Code.t * Params.t * bexp
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
  Protocols.Kernel.reset_variable_kind
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

let compile_all ?(rules = Idiom_rewrite.all) ?infer_cond_bound
    (l : Kernel.t list) : Protocols.Kernel.t list * Rejected_kernel.t list =
  let l, rejected =
    l |> List.map Scoped.Kernel.from_imp |> Inline_calls.inline_calls
  in
  (List.map (compile ~rules ?infer_cond_bound) l, rejected)
