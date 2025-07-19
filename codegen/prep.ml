open Protocols
open Bank_conflicts
open Exp

module VarSet = Variable.Set
module VarMap = Variable.Map

(* Make blockDim parameters RaCUDA-friendly *)
let prepare_params (g : Generator.t) (gv : Gv_parser.t)
  : Gv_parser.t * Config.t =
  let f x = if not g.mod_gv_args then x else if x > 1 then max x 32 else 1 in
  let x, y, z = gv.block_dim.x, gv.block_dim.y, gv.block_dim.z in
  let block_dim = Dim3.{x = f x; y = f y; z = f z} in
  let grid_dim = gv.grid_dim in
  { gv with block_dim }, Config.make ~block_dim ~grid_dim ()

(* Convert source instruction to the list of variables used in it *)
let rec inst_to_vars : Code.t -> Variable.t list =
  function
  | Skip -> []
  | Decl {body=p;_} -> inst_to_vars p
  | Access e -> List.map n_to_vars e.index |> List.flatten
  | Sync _ -> []
  | Seq (p, q) ->
    inst_to_vars p @ inst_to_vars q
  | If (b, p, q) ->
    b_to_vars b @ inst_to_vars p @ inst_to_vars q
  | Loop {range=r; body=p} -> r_to_vars r @ inst_to_vars p
and n_to_vars : nexp -> Variable.t list =
  function
  | Var x -> [x]
  | Num _ -> []
  | Binary (_, e1, e2) -> n_to_vars e1 @ n_to_vars e2
  | Unary (_, e) | Other e -> n_to_vars e
  | NCall (_, e) -> n_to_vars e
  | NIf (b, e1, e2) -> b_to_vars b @ n_to_vars e1 @ n_to_vars e2
  | CastInt e -> b_to_vars e
and b_to_vars : bexp -> Variable.t list =
  function
  | CastBool e -> n_to_vars e
  | Bool _ -> []
  | NRel (_, e1, e2) -> n_to_vars e1 @ n_to_vars e2
  | BRel (_, b1, b2) -> b_to_vars b1 @ b_to_vars b2
  | BNot b -> b_to_vars b
  | Pred (_, e) -> n_to_vars e
  | Distinct exprs -> List.concat_map n_to_vars exprs
and r_to_vars (r : Range.t) : Variable.t list =
  let step_variables = match r.step with
    | Plus e -> n_to_vars e
    | Mult e -> n_to_vars e
  in
  [[r.var]; n_to_vars r.lower_bound; n_to_vars r.upper_bound; step_variables]
  |> List.flatten

(* Get the set of variables used in the code *)
let variables_used (p : Code.t) : VarSet.t =
  inst_to_vars p |> VarSet.of_list

(* Remove unused local variables from the kernel *)
let remove_unused_variables (k : Kernel.t) : Kernel.t =
  let used_variables = variables_used k.code in
  let local_variables =
    VarSet.inter used_variables (Params.to_set k.local_variables)
  in
  { k with
    local_variables =
      Params.filter
        (fun x -> Variable.Set.mem x local_variables)
        k.local_variables
  }

(* Remove template parameters from a CUDA type *)
let remove_template (s : string) : string =
  match String.index_opt s '<' with
  | None -> s
  | Some t_index -> String.sub s 0 t_index

(* Convert a dependent/templated type to a generic CUDA type *)
let rec convert_type : string list -> string list =
  function
  | [] -> []
  | [last_type] -> [remove_template last_type]
  | modifier :: types ->
    if modifier = "typename" then convert_type types
    else modifier :: convert_type types

(* Replace the type of each array with a compatible alternative *)
let mk_types_compatible (k : Kernel.t) : Kernel.t =
  let mk_array_compatible (arr : Memory.t) : Memory.t =
    {arr with data_type = convert_type arr.data_type}
  in
  let arrays = VarMap.map mk_array_compatible k.arrays in
  {k with arrays}

(* Prepare the kernel for serialization *)
let prepare_kernel
  (g : Generator.t)
  (params : Config.t)
  (k : Kernel.t)
:
  Kernel.t
=
  k
  |> (if g.const_fold then Kernel.opt else Fun.id)
  |> remove_unused_variables
  |> mk_types_compatible
  |> (if g.distinct_vars then Kernel.vars_distinct else Fun.id)
  |> (if g.simplify_kernel then Protocol_analysis.Silent.simplify_kernel params
      else Fun.id)
