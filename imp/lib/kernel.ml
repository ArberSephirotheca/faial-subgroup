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
    (* [Unsupported] retains the source [Ty.t] so the IR keeps
       the parameter's declared type even when the C-to-Imp lifting
       has no specialised handling for it (pointer-to-pointer, opaque
       structs, function pointers, etc.). Downstream analyses can
       inspect the type without having to re-read the source. *)
    type t =
      | Scalar of Ty.t
      | Array of Memory.t
      | Enum of Enum.t
      | Unsupported of Ty.t

    let to_string : t -> string = function
      | Scalar s -> Ty.to_string s
      | Array m -> Memory.to_string m
      | Enum e -> Enum.name e
      | Unsupported ty -> Ty.to_string ty

    let to_c_type : t -> Ty.t = function
      | Enum e -> Enum.to_c_type e
      | Array _ -> Ty.unknown
      | Unsupported ty -> ty
      | Scalar ty -> ty
  end

  type t = Variable.t * Type.t

  let to_c_type : Variable.t * Type.t -> Variable.t * Ty.t =
   fun (a, ty) -> (a, Type.to_c_type ty)

  let enum (name : Variable.t) (e : Enum.t) : t = (name, Enum e)
  let array (name : Variable.t) (m : Memory.t) : t = (name, Array m)
  let scalar (name : Variable.t) (ty : Ty.t) : t = (name, Scalar ty)
  let unsupported (name : Variable.t) (ty : Ty.t) : t =
    (name, Unsupported ty)

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
        | Unsupported _ | Array _ -> ps)
      Params.empty l

  let to_c_type (x : t) : (Variable.t * Ty.t) list =
    x |> List.map Parameter.to_c_type

  let to_list (x : t) : Variable.t list = x |> List.map fst
  let to_set (x : t) : Variable.Set.t = x |> to_list |> Variable.Set.of_list
end

type t = {
  (* What makes this function distinct from every other one. *)
  id : Function_id.t;
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

let unique_id (k : t) : Function_id.t = k.id
let name (k : t) : string = Function_id.label k.id

let to_s (k : t) : Indent.t list =
  [
    Indent.Line "";
    Line
      (Printf.sprintf "%s %s (%s)"
         (Visibility.to_string k.visibility)
         (name k)
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

let to_string (k : t) : string = Indent.to_string (to_s k)
let print (k : t) : unit = Indent.print (to_s k)
let is_global (k : t) : bool = k.visibility = Visibility.Global

let remove_global_asserts (k : t) : t =
  { k with code = Stmt.filter_asserts Assert.is_local k.code }

let calls (k : t) : Function_id.Set.t = Stmt.calls k.code
