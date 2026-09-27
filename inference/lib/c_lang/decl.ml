open Stage0
open Protocols
open Ast

type t = c_decl = {
  var : Variable.t;
  ty : Ty.t;
  init : Init.t option;
  attrs : string list;
}

let make ~ty_var ~init ~attrs : t =
  { ty = ty_var.Ty_variable.ty; var = Ty_variable.name ty_var; init; attrs }

let init (x : t) : Init.t option = x.init
let attrs (x : t) : string list = x.attrs
let var (x : t) : Variable.t = x.var
let ty (x : t) : Ty.t = x.ty
let location (x : t) : Location.t = Variable.location x.var
let matches pred (x : t) = pred x.ty
let is_shared (x : t) : bool = List.mem c_attr_shared x.attrs

let to_expr_seq (x : t) : Expr.t Seq.t =
  match x.init with Some i -> Init.to_expr_seq i | None -> Seq.empty

let map_expr (f : Expr.t -> Expr.t) (x : t) : t =
  { x with init = x.init |> Option.map (Init.map_expr f) }

let to_string (d : t) : string =
  let i =
    match d.init with Some e -> " = " ^ Init.to_string e | None -> ""
  in
  let attr =
    if d.attrs = [] then ""
    else
      let attrs = String.concat " " d.attrs |> String.trim in
      attrs ^ " "
  in
  attr ^ Ty.to_string d.ty ^ " " ^ Variable.name d.var ^ i

let to_s (d : t) : Indent.t list = [ Indent.Line (to_string d ^ ";") ]

let parse : Parse_util.json -> t option Parse_util.j_result = Parsers.parse_decl
