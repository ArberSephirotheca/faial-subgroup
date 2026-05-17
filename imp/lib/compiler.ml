open Kernel
module Variable = Protocols.Variable
module Params = Protocols.Params
module Exp = Protocols.Exp
open Exp

let compile (k : Scoped.Kernel.t) : Protocols.Kernel.t =
  (* Merge globally-defined arrays and arrays defined in parameters. *)
  let arrays =
    k.global_arrays
    |> Variable.MapUtil.union_left (ParameterList.to_arrays k.parameters)
  in
  let p =
    k.code
    |> Scoped.Code.filter_locs (arrays |> Variable.MapSetUtil.map_to_set)
       (* Remove unknown arrays *)
    |> Scoped.Code.fix_assigns
    (* Inline local variable assignment and ensure variables are distinct*)
    |> Encode_assigns.from_scoped (ParameterList.to_set k.parameters)
    |> Encode_asserts.from_encode_assigns
  in
  let p, locals, pre =
    let rec inline_header :
        Protocols.Code.t * Params.t * bexp -> Protocols.Code.t * Params.t * bexp
        =
     fun (p, locals, pre) ->
      match p with
      | If (b, p, Skip) -> inline_header (p, locals, b_and b pre)
      | Decl { var = x; body = p; ty; pre = decl_pre } ->
          let pre =
            match decl_pre with Some b -> b_and pre b | None -> pre
          in
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

let compile_all ?(inline_calls = true) (l : Kernel.t list) :
    Protocols.Kernel.t list =
  let l = List.map Scoped.Kernel.from_imp l in
  let l = if inline_calls then Inline_calls.inline_calls l else l in
  List.map compile l
