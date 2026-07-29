open Protocols

type json = Yojson.Basic.t

module Kind = struct
  type t =
    | CXXMethod
    | Function
    | NonTypeTemplateParm
    | EnumConstant
    | Var
    | ParmVar

  let to_string : t -> string = function
    | CXXMethod -> "meth"
    | Function -> "func"
    | NonTypeTemplateParm -> "tmpl"
    | EnumConstant -> "enum"
    | Var -> "var"
    | ParmVar -> "parm"

  (* Only [Var] / [ParmVar] references carry runtime values; function /
     method / enum / template-parm references resolve at compile time. *)
  let is_runtime_value : t -> bool = function
    | Var | ParmVar -> true
    | Function | CXXMethod | NonTypeTemplateParm | EnumConstant -> false
end

type t = {
  name : Variable.t;
  ty : Ty.t;
  kind : Kind.t;
  decl_id : string option;
}

let from_name ?(ty = J_type.int) ?(kind = Kind.Var) (name : Variable.t) : t =
  { name; ty; kind; decl_id = None }

let equal (e1 : t) (e2 : t) : bool = Variable.equal e1.name e2.name

let from_ty_var ?(kind = Kind.Var) (ty_var : Ty_variable.t) : t =
  { name = ty_var.name; ty = ty_var.ty; kind; decl_id = None }

let name (e : t) : Variable.t = e.name
let ty (e : t) : Ty.t = e.ty

let to_string ?(modifier : bool = false) (e : t) : string =
  let attr : string =
    if modifier then "@" ^ Kind.to_string e.kind ^ " " else ""
  in
  let name = e.name |> Variable.name in
  attr ^ name

let attr (e : t) : string = e.kind |> Kind.to_string
let is_runtime_value (e : t) : bool = Kind.is_runtime_value e.kind

let update_name (f : string -> string) (e : t) : t =
  { e with name = Variable.update_name f e.name }

let compare (x : t) (y : t) : int = Variable.compare x.name y.name

module OT = struct
  type t' = t
  type t = t'

  let compare = compare
end

module Set = Set.Make (OT)
module Map = Map.Make (OT)
