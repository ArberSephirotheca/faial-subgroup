open Protocols
open Ast

type t = c_template_argument =
  | TArgType of Ty.t
  | TArgIntegral of int
  | TArgNullArg
  | TArgNullPtr
  | TArgDecl of string
  | TArgExpr of c_expr
  | TArgPack of t list
  | TArgTemplate of string
  | TArgTemplateExpansion of string

let parse = Parsers.parse_c_template_argument

let rec to_string : t -> string = function
  | TArgType ty -> Ty.to_string ty
  | TArgIntegral n -> string_of_int n
  | TArgNullArg -> "<null>"
  | TArgNullPtr -> "nullptr"
  | TArgDecl n -> n
  | TArgExpr _ -> "<expr>"
  | TArgPack xs -> "{" ^ list_to_s to_string xs ^ "}"
  | TArgTemplate n -> n
  | TArgTemplateExpansion n -> n ^ "..."
