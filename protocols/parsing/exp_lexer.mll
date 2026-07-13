{
open Exp_parser
}

rule read = parse
  | [' ' '\t' '\r']       { read lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; read lexbuf }

  (* Comments *)
  | "//" [^ '\n' '\r']* '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "//" [^ '\n' '\r']* ('\r' | "\r\n") { read lexbuf }
  | "/*"                  { read_block_comment lexbuf }

  (* Literals *)
  | "true"                { BOOL(true) }
  | "false"               { BOOL(false) }
  | ['0'-'9']+ as i       { INT(int_of_string i) }

  (* Arithmetic operators (C precedence). The [u] suffix marks the
     unsigned variant emitted by [N_binary.to_string] /
     [N_rel.to_string]; greedy lexer matching picks the longer
     [+u] / [<=u] / etc. forms before falling back to the signed
     [+] / [<=]. An identifier starting with [u] immediately after
     a bare signed operator with no separating space is the one
     pathological case where this misparses; in practice the
     emitted form has whitespace between tokens. *)
  | "+u"                  { PLUS_U }
  | "-u"                  { MINUS_U }
  | "*u"                  { MULT_U }
  | "/u"                  { DIV_U }
  | "%u"                  { MOD_U }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { MULT }
  | "/"                   { DIV }
  | "%"                   { MOD }

  (* Bitwise operators *)
  | "&"                   { BIT_AND }
  | "|"                   { BIT_OR }
  | "^"                   { BIT_XOR }
  | "<<"                  { LEFT_SHIFT }
  | ">>u"                 { URIGHT_SHIFT }
  | ">>"                  { RIGHT_SHIFT }
  | "~"                   { BIT_NOT }

  (* Comparison operators *)
  | "=="                  { EQ }
  | "!="                  { NEQ }
  | "<=u"                 { LE_U }
  | ">=u"                 { GE_U }
  | "<u"                  { LT_U }
  | ">u"                  { GT_U }
  | "<="                  { LE }
  | ">="                  { GE }
  | "<"                   { LT }
  | ">"                   { GT }

  (* Argument separator (used in n-ary predicate calls
     [bvumul_noovfl(a, b)] and friends). *)
  | ","                   { COMMA }

  (* Logical operators *)
  | "&&"                  { L_AND }
  | "||"                  { L_OR }
  | "!"                   { L_NOT }

  (* Ternary conditional *)
  | "?"                   { QUESTION }
  | ":"                   { COLON }

  (* Type casts *)
  | "int"                 { CAST_INT }
  | "bool"                { CAST_BOOL }

  | "bvumul_noovfl"       { BVUMUL }

  (* Identifiers (C-style + dots + dollar signs) *)
  | ['a'-'z' 'A'-'Z' '_' '$']['a'-'z' 'A'-'Z' '_' '.' '$' '0'-'9']* as id { IDENT(id) }

  (* Punctuation *)
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | eof                   { EOF }

  (* Error *)
  | _                     { failwith (Printf.sprintf "Unknown character: %c" (Lexing.lexeme_char lexbuf 0)) }

and read_block_comment = parse
  | "*/"                  { read lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; read_block_comment lexbuf }
  | _                     { read_block_comment lexbuf }
  | eof                   { failwith "Unterminated block comment" }
