{
open Theorem_parser
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
  
  (* Arithmetic operators (C precedence) *)
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
  | ">>"                  { RIGHT_SHIFT }
  | "~"                   { BIT_NOT }
  
  (* Comparison operators *)
  | "=="                  { EQ }
  | "!="                  { NEQ }
  | "<="                  { LE }
  | ">="                  { GE }
  | "<"                   { LT }
  | ">"                   { GT }
  
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
  
  (* Keywords (must come before identifiers) *)
  | "threads_per_warp"    { THREADS_PER_WARP }
  | "block_dim"           { BLOCK_DIM }
  | "locals"              { LOCALS }
  | "globals"             { GLOBALS }
  | "active_threads"      { ACTIVE_THREADS }
  | "assumptions"         { ASSUMPTIONS }
  | "x"                   { X }
  | "y"                   { Y }
  | "z"                   { Z }
  | "prove"               { PROVE }
  | "max"                 { MAX }
  | "min"                 { MIN }
  
  (* Identifiers (C-style + dots + dollar signs) *)
  | ['a'-'z' 'A'-'Z' '_' '$']['a'-'z' 'A'-'Z' '_' '.' '$' '0'-'9']* as id { IDENT(id) }
  
  (* Punctuation *)
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | '['                   { LBRACKET }
  | ']'                   { RBRACKET }
  | ';'                   { SEMICOLON }
  | ','                   { COMMA }
  | eof                   { EOF }
  
  (* Error *)
  | _                     { failwith (Printf.sprintf "Unknown character: %c" (Lexing.lexeme_char lexbuf 0)) }

and read_block_comment = parse
  | "*/"                  { read lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; read_block_comment lexbuf }
  | _                     { read_block_comment lexbuf }
  | eof                   { failwith "Unterminated block comment" }
