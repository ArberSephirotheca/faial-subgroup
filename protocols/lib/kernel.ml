open Stage0

let ( @ ) = Common.append_tr

open Exp

type t = {
  (* The kernel name *)
  name : string;
  (* The internal variables are used in the code of the kernel.  *)
  global_variables : Params.t;
  (* The internal variables are used in the code of the kernel.  *)
  local_variables : Params.t;
  (* The modifiers of each array *)
  arrays : Memory.t Variable.Map.t;
  (* A thread-local pre-condition that is true on all phases. *)
  pre : bexp;
  (* The code of a kernel performs the actual memory accesses. *)
  code : Code.t;
  (* The kernel's visibility *)
  visibility : Visibility.t;
  (* Number of blocks *)
  grid_dim : Dim3.t option;
  (* Number of blocks *)
  block_dim : Dim3.t option;
}

let name (k : t) : string = k.name

let apply_arch (a : Architecture.t) (k : t) : t =
  let d = Architecture.to_defaults a in
  let arrays = Variable.Map.filter (fun _ a -> Memory.is_global a) k.arrays in
  {
    k with
    arrays = (match a with Grid -> arrays | Block -> k.arrays);
    code = Code.apply_arch (Variable.MapSetUtil.map_to_set arrays) a k.code;
    global_variables = Params.union_right k.global_variables d.globals;
    local_variables = Params.union_right k.local_variables d.locals;
    pre = b_and (Architecture.Defaults.to_bexp d) k.pre;
  }

let is_global (k : t) : bool = k.visibility = Global
let is_device (k : t) : bool = k.visibility = Device

let has_shared_arrays (k : t) : bool =
  Variable.Map.exists (fun _ m -> Memory.is_shared m) k.arrays

let shared_arrays (k : t) : Variable.Set.t =
  k.arrays
  |> Variable.Map.filter (fun _ v -> Memory.is_shared v)
  |> Variable.MapSetUtil.map_to_set

let global_arrays (k : t) : Variable.Set.t =
  k.arrays
  |> Variable.Map.filter (fun _ v -> Memory.is_global v)
  |> Variable.MapSetUtil.map_to_set

let used_arrays (k : t) : Variable.Set.t =
  Code.used_arrays k.code Variable.Set.empty

let constants (k : t) =
  let rec constants (b : bexp) (kvs : (string * int) list) : (string * int) list
      =
    match b with
    | CastBool (CastInt b) -> constants b kvs
    | NRel (Eq, Var x, Num n) | NRel (Eq, Num n, Var x) ->
        (Variable.name x, n) :: kvs
    | BRel (BAnd, b1, b2) -> constants b1 kvs |> constants b2
    | Bool _ | CastBool _ | BNot _ | Pred _ | NRel _ | BRel _ | Distinct _ ->
        kvs
  in
  constants k.pre []

let filter_access (f : Access.t -> bool) (k : t) : t =
  { k with code = Code.filter (function Access a -> f a | _ -> true) k.code }

let filter_array (to_keep : Variable.t -> bool) (k : t) : t =
  {
    k with
    (* update the set of arrays *)
    arrays = Variable.Map.filter (fun v _ -> to_keep v) k.arrays;
    code =
      Code.filter
        (function Access { array = v; _ } -> to_keep v | _ -> true)
        k.code;
  }

(* Create a new kernel with same name, but no code to check *)
let clear (k : t) : t =
  {
    name = k.name;
    arrays = Variable.Map.empty;
    pre = Bool true;
    code = Skip;
    global_variables = Params.empty;
    local_variables = Params.empty;
    visibility = k.visibility;
    block_dim = None;
    grid_dim = None;
  }

let opt (k : t) : t =
  { k with pre = Constfold.b_opt k.pre; code = Code.opt k.code }

let global_set (k : t) : Variable.Set.t = Params.to_set k.local_variables
let local_set (k : t) : Variable.Set.t = Params.to_set k.local_variables

let parameter_set (k : t) : Variable.Set.t =
  Variable.Set.union (local_set k) (global_set k)

let assign_globals (kvs : (string * int) list) (k : t) : t =
  if Common.list_is_empty kvs then k
  else
    (* retrieve a set of key-values *)
    let keys =
      kvs |> List.split |> fst
      |> List.map Variable.from_name
      |> Variable.Set.of_list
    in
    let non_global_keys = Variable.Set.diff keys (global_set k) in
    if Variable.Set.is_empty non_global_keys then
      raise
        (invalid_arg
           ("The following keys are not thread-global parameters: "
           ^ Variable.set_to_string non_global_keys))
    else ();
    let kvs =
      kvs |> List.map (fun (x, n) -> (x, Num n)) |> Subst.SubstAssoc.make
    in
    {
      k with
      pre = Code.PSubstAssoc.M.b_subst kvs k.pre;
      code = Code.PSubstAssoc.subst kvs k.code;
      global_variables = Params.remove_all keys k.global_variables;
      local_variables = Params.remove_all keys k.local_variables;
    }

let vars_distinct (k : t) : t =
  { k with code = Code.vars_distinct k.code (parameter_set k) }

(*
  Makes all variables distinct and hoists declarations as
  thread-locals.
*)
let hoist_decls : t -> t =
  let rec inline (vars : Params.t) (p : Code.t) : Params.t * Code.t =
    match p with
    | Decl { var = x; body = p; ty } -> inline (Params.add x ty vars) p
    | Access _ | Skip | Sync _ -> (vars, p)
    | If (b, p, q) ->
        let vars, p = inline vars p in
        let vars, q = inline vars q in
        (vars, If (b, p, q))
    | Loop { range = r; body = p } ->
        let vars, p = inline vars p in
        (vars, Loop { range = r; body = p })
    | Seq (p, q) ->
        let vars, p = inline vars p in
        let vars, q = inline vars q in
        (vars, Seq (p, q))
  in
  fun k ->
    let k = vars_distinct k in
    let locals, p = inline Params.empty k.code in
    {
      k with
      code = p;
      local_variables = Params.union_left locals k.local_variables;
    }

let inline_dims (dims : (string * Dim3.t) list) (k : t) : t =
  let key_vals =
    List.concat_map (fun (name, d) -> Dim3.to_assoc ~prefix:(name ^ ".") d) dims
  in
  assign_globals key_vals k

let inline_inferred (k : t) : t =
  let key_vals =
    constants k
    |> List.filter (fun (x, _) ->
        (* Make sure we only replace thread-global variables *)
        Params.mem (Variable.from_name x) k.global_variables)
  in
  assign_globals key_vals k

let inline_all ~globals ~block_dim ~grid_dim (k : t) : t =
  let or_ o1 o2 = if Option.is_some o1 then o1 else o2 in
  let block_dim = or_ k.block_dim block_dim in
  let grid_dim = or_ k.grid_dim grid_dim in
  let to_dim k d =
    d |> Option.map (fun x -> [ (k, x) ]) |> Option.value ~default:[]
  in
  let extra =
    [ (Variable.tid_list, block_dim); (Variable.bid_list, grid_dim) ]
    |> List.concat_map (function
      | [ x; y; z ], Some Dim3.{ x = v1; y = v2; z = v3 } ->
          [ (x, v1); (y, v2); (z, v3) ]
      | _, _ -> [])
    |> List.filter_map (fun (k, v) ->
        if v = 1 then Some (Variable.name k, 0) else None)
  in
  k
  |> assign_globals (globals @ extra)
  |> inline_dims (to_dim "blockDim" block_dim @ to_dim "gridDim" grid_dim)
  |> inline_inferred

let used_variables (k : t) : Variable.Set.t =
  Code.free_names k.code Variable.Set.empty |> Exp.b_free_names k.pre

let trim_binders (k : t) : t =
  let fns = used_variables k in
  {
    k with
    global_variables = Params.retain_all fns k.global_variables;
    local_variables = Params.retain_all fns k.local_variables;
  }

let free_names (k : t) : Variable.Set.t =
  let fns = used_variables k in
  let fns = Variable.Set.diff fns (Params.to_set k.local_variables) in
  let fns = Variable.Set.diff fns (Params.to_set k.global_variables) in
  fns

(*
Given a protocol with free names, add those as thread-locals.
  *)
let add_missing_binders (k : t) : t =
  let locals = Params.from_set C_type.int (free_names k) in
  { k with local_variables = Params.union_left k.local_variables locals }

let to_ci_di (k : t) : t =
  let approx = k.local_variables |> Params.to_set in
  let approx = Variable.Set.diff approx Variable.tid_set in
  { k with code = Code.to_ci_di approx k.code }

let to_s (k : t) : Indent.t list =
  [
    Line ("name: " ^ k.name ^ ";");
    Line ("arrays: " ^ Memory.map_to_string k.arrays ^ ";");
    Line ("globals: " ^ Params.to_string k.global_variables ^ ";");
    Line ("locals: " ^ Params.to_string k.local_variables ^ ";");
    Line "invariant:";
    Block (b_to_s k.pre);
    Line ";";
    Line "code:";
    Block (Code.to_s k.code);
    Line "; end of code";
  ]

let to_string (p : t) : string = Indent.to_string (to_s p)
let print (k : t) : unit = Indent.print (to_s k)
