open Stage0
open Protocols
open Ast

type t = c_for_init = Decls of Decl.t list | Expr of Expr.t

let to_expr_seq : t -> Expr.t Seq.t = function
  | Decls l -> List.to_seq l |> Seq.concat_map Decl.to_expr_seq
  | Expr e -> Seq.return e

let loop_vars : t -> Variable.t list =
  let rec exp_var (e : Expr.t) : Variable.t list =
    match e with
    | BinaryOperator { lhs = l; opcode = ","; rhs = r; _ } ->
        exp_var l |> Common.append_rev1 (exp_var r)
    | BinaryOperator { lhs = Ident l; opcode = "="; _ } -> [ l.name ]
    | _ -> []
  in
  function Decls l -> List.map Decl.var l | Expr e -> exp_var e

let to_string : t -> string = function
  | Decls d -> list_to_s Decl.to_string d
  | Expr e -> Expr.to_string e

let opt_to_string : t option -> string = function
  | Some o -> to_string o
  | None -> ""

let parse : Parse_util.json -> t Parse_util.j_result = Parsers.parse_for_init
