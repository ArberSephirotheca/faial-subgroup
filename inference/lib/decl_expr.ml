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
end

(* A program variable *)
type t = { name : Variable.t; ty : J_type.t; kind : Kind.t }

let from_name ?(ty = J_type.int) ?(kind = Kind.Var) (name : Variable.t) : t =
  { name; ty; kind }

let equal (e1 : t) (e2 : t) : bool = Variable.equal e1.name e2.name

let from_ty_var ?(kind = Kind.Var) (ty_var : Ty_variable.t) : t =
  { name = ty_var.name; ty = ty_var.ty; kind }

let name (e : t) : Variable.t = e.name
let ty (e : t) : J_type.t = e.ty

let to_string ?(modifier : bool = false) (e : t) : string =
  let attr : string =
    if modifier then "@" ^ Kind.to_string e.kind ^ " " else ""
  in
  let name = e.name |> Variable.name in
  attr ^ name

let attr (e : t) : string = e.kind |> Kind.to_string

let update_name (f : string -> string) (e : t) : t =
  { e with name = Variable.update_name f e.name }
