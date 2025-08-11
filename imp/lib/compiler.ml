
open Kernel
module Variable = Protocols.Variable
module Params = Protocols.Params
module Exp = Protocols.Exp
open Exp

let compile (k : Kernel.t) : Protocols.Kernel.t =
  let globals =
    (* Take the global variables and the scalars defined in the paramter list *)
    k.global_variables
    |> Protocols.Params.union_left (ParameterList.to_params k.parameters)
  in
  (* Add any globals defined from scoped *)
  let globals, p = Scoped.from_stmt (globals, k.code) in
  (* Merge globally-defined arrays and arrays defined in parameters. *)
  let arrays =
    k.global_arrays
    |> Variable.MapUtil.union_left (ParameterList.to_arrays k.parameters)
  in
  let p =
    p
    |> Scoped.filter_locs arrays (* Remove unknown arrays *)
    |> Scoped.fix_assigns
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
    global_variables = globals;
    code = p;
    visibility = k.visibility;
    block_dim = k.block_dim;
    grid_dim = k.grid_dim;
  }

