open Stage0
open Protocols
module StringMap = Common.StringMap
module StringMapUtil = Common.StringMapUtil
module StringSet = Common.StringSet

let ( @ ) = Common.append_tr

open Exp

type var_type = Location | Index
type access_expr = { access_index : nexp list; access_mode : Access.Mode.t }

(*
  Translation goals:
  1. lexical scoping is contained in the AST term (simplifies substitution)
  2. inline assignments given by Imp.Decl
  3. inline array alias
  4. inline asserts

  1. In Imp, the lexical scoping of a variable binding is the sequence
  of statements that succeed that statement. In Scoped, the lexical scoping is
  always  _contained_ in the variable binding operator.
  
  For instance a variable declaration in Imp:
    var x; s1; ...; sn
  Becomes
    var x { s1; ...; sn }
  
  2. In Imp we can have local variable assignments. We inline such assignments
  in Scoped. However, variable declaration still remains in Scoped.
  
  In Imp:
    local x = 1; s1; ...; sn
  becomes in Scoped:
    local x {s1[x=1] ; ... sn[x=1]}
*)
module Parameter = struct
  module Type = struct
    type t =
      | Scalar of C_type.t
      | Array of Memory.t
      | Enum of Enum.t
      | Unsupported

    let to_string : t -> string = function
      | Scalar s -> C_type.to_string s
      | Array m -> Memory.to_string m
      | Enum e -> Enum.name e
      | Unsupported -> "?"
  end

  type t = Variable.t * Type.t

  let enum (name : Variable.t) (e : Enum.t) : t = (name, Enum e)
  let array (name : Variable.t) (m : Memory.t) : t = (name, Array m)
  let scalar (name : Variable.t) (ty : C_type.t) : t = (name, Scalar ty)
  let unsupported (name : Variable.t) : t = (name, Unsupported)

  let to_array ((name, ty) : t) : (Variable.t * Memory.t) option =
    match ty with Type.Array m -> Some (name, m) | _ -> None

  let to_string ((p, ty) : t) : string =
    Printf.sprintf "%s %s" (Type.to_string ty) (Variable.name p)
end

module ParameterList = struct
  type t = Parameter.t list

  let empty : t = []

  let to_string (l : t) : string =
    l |> List.map Parameter.to_string |> String.concat ", "

  let to_arrays (x : t) : Memory.t Variable.Map.t =
    x |> List.filter_map Parameter.to_array |> Variable.Map.of_list

  let to_params (l : t) : Params.t =
    List.fold_left
      (fun ps (x, ty) ->
        match ty with
        | Parameter.Type.Enum e ->
            Params.add ~bound:(Some (Enum.to_bexp x e)) x (Enum.to_c_type e) ps
        | Scalar ty -> Params.add x ty ps
        | Unsupported | Array _ -> ps)
      Params.empty l

  let to_list (x : t) : Variable.t list = x |> List.map fst
  let to_set (x : t) : Variable.Set.t = x |> to_list |> Variable.Set.of_list
end

type t = {
  (* The kernel name *)
  name : string;
  (* The type signature of the kernel *)
  ty : string;
  (* Kernel parameters *)
  parameters : ParameterList.t;
  (* Globally-defined arrays that can be accessed by the kernel. *)
  global_arrays : Memory.t Variable.Map.t;
  (* Global variables of the kernels (scalars).  *)
  global_variables : Params.t;
  (* The code of a kernel performs the actual memory accesses. *)
  code : Stmt.t;
  (* A kernel may return a value *)
  return : Exp.nexp option;
  (* Visibility *)
  visibility : Visibility.t;
  (* Number of blocks *)
  grid_dim : Dim3.t option;
  (* Number of blocks *)
  block_dim : Dim3.t option;
}

(* Generate a unique id that pairs the name and type. *)
let unique_id (k : t) : string = Call.kernel_id ~kernel:k.name ~ty:k.ty

let to_s (k : t) : Indent.t list =
  [
    Indent.Line "";
    Line
      (Printf.sprintf "%s %s (%s)"
         (Visibility.to_string k.visibility)
         k.name
         (ParameterList.to_string k.parameters));
    Line
      (Printf.sprintf "global {arrays: %s} {scalars: %s}"
         (Memory.map_to_string k.global_arrays)
         (Params.to_string k.global_variables));
    Line "{";
    Block
      ((if k.code = Skip then [] else Stmt.to_s k.code)
      @
      match k.return with
      | Some e -> [ Indent.Line ("return " ^ Exp.n_to_string e ^ ";") ]
      | None -> []);
    Line "}";
  ]

let to_string (k: t) : string = Indent.to_string (to_s k)

let print (k : t) : unit = Indent.print (to_s k)
let is_global (k : t) : bool = k.visibility = Visibility.Global

let remove_global_asserts (k : t) : t =
  { k with code = Stmt.filter_asserts Assert.is_local k.code }

let compile (k : t) : Protocols.Kernel.t =
  let globals =
    (* Take the global variables and the scalars defined in the paramter list *)
    k.global_variables
    |> Params.union_left (ParameterList.to_params k.parameters)
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

let calls (k : t) : StringSet.t = Stmt.calls k.code

let apply (result : (Variable.t * C_type.t) option) (args : Arg.t list) (k : t)
    : Stmt.t =
  let code =
    match result with
    | Some (var, ty) ->
        let d : Decl.t = { var; init = k.return; ty } in
        Stmt.Seq (Stmt.decl d, k.code)
    | None -> k.code
  in
  List.fold_right
    (fun (x, a) s ->
      let i =
        let open Arg in
        match a with
        | Scalar e -> Stmt.decl_set x e
        | Unsupported -> Stmt.decl_unset x
        | Array u ->
            Stmt.LocationAlias
              { target = x; source = u.array; offset = u.offset }
      in
      Stmt.Seq (i, s))
    (Common.zip (ParameterList.to_list k.parameters) args)
    code

let inline (funcs : t StringMap.t) (k : t) : t =
  let rec inline (s : Stmt.t) : Stmt.t =
    match s with
    | Call c -> (
        match StringMap.find_opt (Call.unique_id c) funcs with
        | Some k -> apply c.result c.args k
        | None -> s)
    | Sync _ | Assert _ | Read _ | Write _ | Atomic _ | Decl _ | LocationAlias _
    | Assign _ ->
        s
    | Skip -> Skip
    | Seq (p, q) -> Seq (inline p, inline q)
    | If (b, s1, s2) -> If (b, inline s1, inline s2)
    | For (r, s) -> For (r, inline s)
    | Star s -> Star (inline s)
  in
  { k with code = inline k.code }
