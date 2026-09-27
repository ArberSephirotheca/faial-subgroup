open Protocols
open Ast

type t = c_init =
  | InitListExpr of { ty : Ty.t; args : Expr.t list }
  | IExpr of Expr.t

let map_expr (f : Expr.t -> Expr.t) : t -> t = function
  | InitListExpr { ty; args = l } -> InitListExpr { ty; args = List.map f l }
  | IExpr e -> IExpr (f e)

let to_expr_seq : t -> Expr.t Seq.t = function
  | InitListExpr l -> List.to_seq l.args
  | IExpr e -> Seq.return e

let to_string : t -> string = function
  | InitListExpr i -> list_to_s Expr.to_string i.args
  | IExpr i -> Expr.to_string i

let parse : Parse_util.json -> t Parse_util.j_result = Parsers.parse_init
