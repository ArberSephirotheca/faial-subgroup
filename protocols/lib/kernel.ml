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
let global_set (k : t) : Variable.Set.t = Params.to_set k.global_variables
let local_set (k : t) : Variable.Set.t = Params.to_set k.local_variables

let parameter_set (k : t) : Variable.Set.t =
  Variable.Set.union (local_set k) (global_set k)

let kvs_to_string (kvs : (string * int) list) : string =
  let kvs =
    kvs
    |> List.map (fun (k, v) -> k ^ ":" ^ string_of_int v)
    |> String.concat ", "
  in
  "{" ^ kvs ^ "}"

let subst_vars (kvs : (Variable.t * Exp.nexp) list) (k : t) : t =
  let keys = List.map fst kvs |> Variable.Set.of_list in
  let kvs =
    kvs
    |> List.map (fun (k, n) -> (Variable.name k, n))
    |> Subst.SubstAssoc.make
  in
  {
    k with
    pre = Code.PSubstAssoc.M.b_subst kvs k.pre;
    code = Code.PSubstAssoc.subst kvs k.code;
    global_variables = Params.remove_all keys k.global_variables;
    local_variables = Params.remove_all keys k.local_variables;
  }

let assign_globals (kvs : (string * int) list) (k : t) : t =
  let keys =
    List.map fst kvs |> List.map Variable.from_name |> Variable.Set.of_list
  in
  if Common.list_is_empty kvs then k
  else
    let global_set = global_set k in
    let non_global_keys = Variable.Set.diff keys global_set in
    if not (Variable.Set.is_empty non_global_keys) then
      let local_kvs =
        List.filter
          (fun (k, _) ->
            Variable.Set.mem (Variable.from_name k) non_global_keys)
          kvs
        |> kvs_to_string
      in
      let global_set = Variable.set_to_string global_set in
      raise
        (invalid_arg
           ("The following keys are not thread-global parameters: locals="
          ^ local_kvs ^ " globals={" ^ global_set ^ "}"))
    else ();
    let kvs = List.map (fun (x, n) -> (Variable.from_name x, Num n)) kvs in
    subst_vars kvs k

let inline_unit_var (x : Variable.t) (dim : int) (k : t) : t =
  if dim = 1 then subst_vars [ (x, Num 0) ] k else k

let inline_unit_dim ~x ~y ~z (d : Dim3.t) (k : t) : t =
  k |> inline_unit_var x d.x |> inline_unit_var y d.y |> inline_unit_var z d.z

let set_block_dim (d : Dim3.t) (k : t) : t =
  { k with block_dim = Some d }
  |> inline_unit_dim ~x:Variable.tid_x ~y:Variable.tid_y ~z:Variable.tid_z d

let set_grid_dim (d : Dim3.t) (k : t) : t =
  { k with grid_dim = Some d }
  |> inline_unit_dim ~x:Variable.bid_x ~y:Variable.bid_y ~z:Variable.bid_z d

let try_set_block_dim (d : Dim3.t option) (k : t) : t =
  match d with Some d -> set_block_dim d k | None -> k

let try_set_grid_dim (d : Dim3.t option) (k : t) : t =
  match d with Some d -> set_grid_dim d k | None -> k

let add_pre (b : bexp) (k : t) : t =
  { k with pre = b_and b k.pre }

let apply_arch_binders (d : Architecture.Defaults.t) (k : t) : t =
  {
    k with
    global_variables = Params.union_right k.global_variables d.globals;
    local_variables = Params.union_right k.local_variables d.locals;
  }

let apply_arch (a : Architecture.t) (k : t) : t =
  let d = Architecture.to_defaults a in
  let arrays = Variable.Map.filter (fun _ a -> Memory.is_global a) k.arrays in
  {
    k with
    arrays = (match a with Grid -> arrays | Block -> k.arrays);
    code = Code.apply_arch (Variable.MapSetUtil.map_to_set arrays) a k.code;
    pre = b_and (Architecture.Defaults.to_bexp d) k.pre;
  }
  |> apply_arch_binders d

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
    | Bool _ | CastBool _ | BNot _ | Pred _ | NRel _ | BRel _ | Distinct _
    | AtomicResult _ | IsThreadUnif _ ->
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

let vars_distinct (k : t) : t =
  { k with code = Code.vars_distinct k.code (parameter_set k) }

(* When a kernel list has multiple kernels sharing a name (for example
   synth kernels emitted by [Synthesise_launches] from the same source
   line, or repeated template instantiations whose names collide after
   inference), give every duplicate a fresh [_N] suffix so each kernel
   has a unique identifier in user-facing output and per-kernel data
   structures. The first occurrence keeps its original name; subsequent
   duplicates skip suffixes that would collide with another kernel's
   existing name. *)
let uniquify_names (ks : t list) : t list =
  let module SS = Stage0.Common.StringSet in
  let initial =
    List.fold_left (fun acc (k : t) -> SS.add k.name acc) SS.empty ks
  in
  let used = ref SS.empty in
  List.map (fun (k : t) ->
    if not (SS.mem k.name !used) then begin
      used := SS.add k.name !used;
      k
    end else
      let rec fresh n =
        let candidate = Printf.sprintf "%s_%d" k.name n in
        if SS.mem candidate !used || SS.mem candidate initial
        then fresh (n + 1)
        else candidate
      in
      let new_name = fresh 2 in
      used := SS.add new_name !used;
      { k with name = new_name })
    ks

(* One-line-per-param signature; useful for the [--list-kernels
   --show-signature] CLI path. Signed types render bare (the C
   default); unsigned types render with [unsigned] preceding the
   type as in a C declaration. Typedefs like [size_t] or [uint32_t]
   that already encode unsignedness in the typedef name are shown
   as-is — [unsigned] is only prepended when the C type string does
   not already contain the [unsigned] keyword. *)
let signature_string (k : t) : string =
  let format_param (v, ty) =
    let s = C_type.to_string ty in
    let has_unsigned_keyword =
      Stage0.Common.contains ~substring:"unsigned" s
    in
    let display =
      if C_type.is_unsigned ty && not has_unsigned_keyword
      then "unsigned " ^ s
      else s
    in
    Printf.sprintf "    %s %s" display (Variable.name v)
  in
  let section title params =
    if params = [] then []
    else
      ("  " ^ title ^ ":") :: List.map format_param params
  in
  let globals = Params.to_list k.global_variables in
  let locals = Params.to_list k.local_variables in
  String.concat "\n"
    (k.name :: section "globals" globals @ section "locals" locals)

(*
  Makes all variables distinct, then routes binder constraints and hoists
  declarations as thread-locals. Each declaration's [cond] and each loop's
  [cond] is split over conjunction, and every conjunct floats up to the
  innermost enclosing loop whose counter it references; a conjunct that
  mentions no loop counter is conjoined into the kernel-level [pre]. A
  declaration is eliminated (its variable becomes a thread-local), so it
  never stops a conjunct. This is loop-invariant code motion applied to
  constraints (see [../claude-docs/faial/binder-constraints.md]).
*)
let hoist_decls : t -> t =
  let references (x : Variable.t) (c : Exp.bexp) : bool =
    Variable.Set.mem x (Exp.b_free_names c Variable.Set.empty)
  in
  let rec inline (p : Code.t) : Params.t * Code.t * Exp.bexp list =
    match p with
    | Decl { var = x; body = p; ty; cond } ->
        let vars, p, rising = inline p in
        (Params.add x ty vars, p, Exp.b_and_split cond @ rising)
    | Access _ | Skip | Sync _ -> (Params.empty, p, [])
    | If (b, p, q) ->
        let vars_p, p, rising_p = inline p in
        let vars_q, q, rising_q = inline q in
        (Params.union_left vars_p vars_q, If (b, p, q), rising_p @ rising_q)
    | Loop { cond_range; body = p } ->
        let vars, p, rising = inline p in
        let stay, rise =
          List.partition
            (references (Cond_range.var cond_range))
            (Exp.b_and_split cond_range.cond @ rising)
        in
        let cond_range =
          Cond_range.make cond_range.range (Exp.b_and_ex stay)
        in
        (vars, Loop { cond_range; body = p }, rise)
    | Seq (p, q) ->
        let vars_p, p, rising_p = inline p in
        let vars_q, q, rising_q = inline q in
        (Params.union_left vars_p vars_q, Seq (p, q), rising_p @ rising_q)
  in
  fun k ->
    let k = vars_distinct k in
    let locals, p, rising = inline k.code in
    {
      k with
      code = p;
      local_variables = Params.union_left locals k.local_variables;
      pre = Exp.b_and k.pre (Exp.b_and_ex rising);
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

let inline_globals (globals : (string * int) list) (k : t) : t =
  let to_dim k d =
    d |> Option.map (fun x -> [ (k, x) ]) |> Option.value ~default:[]
  in
  k |> assign_globals globals
  |> inline_dims (to_dim "blockDim" k.block_dim @ to_dim "gridDim" k.grid_dim)
  |> inline_inferred

let used_variables (k : t) : Variable.Set.t =
  Code.free_names k.code Variable.Set.empty |> Exp.b_free_names k.pre

(*
  For each thread-index / block-index axis whose variable does not
  appear in [code], assert that the matching launch dimension has
  extent 1 by adding the equality to [pre]. The pairings are
  [threadIdx.{x,y,z}] with [blockDim.{x,y,z}] and [blockIdx.{x,y,z}]
  with [gridDim.{x,y,z}].

  Note: only [code] free names are consulted, not [pre]. After
  [apply_arch] the precondition contains arch-default references
  like [tid.y < blockDim.y] for every axis, so consulting [pre]
  would always mark every axis as used and the function would
  never fire.

  This is a user-asserted assumption, not a sound inference. A
  kernel that writes to memory but does not reference [threadIdx.y]
  still races between any two threads that differ only in
  y-coordinate, and pinning [blockDim.y == 1] hides those races.
  The caller (e.g. the [--assume-dims] CLI flag) is asserting that
  each unreferenced axis was intended to be launched with extent 1.

  Run after [apply_arch] (so [blockDim.*] / [gridDim.*] are
  registered as globals) and before [inline_globals] (so
  [inline_inferred] picks up the new equality and propagates the
  constant through both [code] and [pre]).
*)
let add_dim_assumptions (k : t) : t =
  let used = Code.free_names k.code Variable.Set.empty in
  let pairs =
    [
      (Variable.tid_x, Variable.bdim_x);
      (Variable.tid_y, Variable.bdim_y);
      (Variable.tid_z, Variable.bdim_z);
      (Variable.bid_x, Variable.gdim_x);
      (Variable.bid_y, Variable.gdim_y);
      (Variable.bid_z, Variable.gdim_z);
    ]
  in
  List.fold_left
    (fun k (idx, dim) ->
      if Variable.Set.mem idx used then k
      else add_pre (n_eq (Var dim) (Num 1)) k)
    k pairs

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

let reset_variable_kind (k : t) : t =
  let params = parameter_set k in
  let reset_binders = Params.reset_kind ~kernel_parameters:params in
  {
    k with
    global_variables = reset_binders k.global_variables;
    local_variables = reset_binders k.local_variables;
    pre =
      Exp.reset_variable_kind_b ~kernel_parameters:params
        ~loop_variables:Variable.Set.empty k.pre;
    code = Code.reset_variable_kind params k.code;
  }

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
