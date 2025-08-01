{
open Tactics_parser
}

rule read = parse
  | [' ' '\t' '\r' '\n']  { read lexbuf }
  | "true"                { BOOL(true) }
  | "false"               { BOOL(false) }
  | ['0'-'9']+ as i       { INT(int_of_string i) }
  | ['0'-'9']+ "." ['0'-'9']* as f { FLOAT(float_of_string f) }
  | '"' ([^'"']* as s) '"' { STRING(s) }
  | ";"                   { SEMI }
  | ":"                   { COLON }
  | "||"                  { L_OR }
  | "&&"                  { L_AND }
  | "!"                   { NOT }
  | "if"                  { IF }
  | "par"                 { PAR }
  | "seq"                 { SEQ }
  | "else"                { ELSE }
  | "fail_if_not_decided" { FAIL_IF_NOT_DECIDED }
  | "timeout"             { TIMEOUT }
  | "repeat"              { REPEAT }
  | "with"                { WITH }
  | "skip"                { SKIP }
  | "fail"                { FAIL }
  | "s"  { SEC }
  | "ms" { MILLI_SEC}
  | ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '_' '.' '-' '0'-'9']* as id { IDENT(id) }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | ','                   { COMMA }
  | eof                   { EOF }
  | _                     { failwith (Printf.sprintf "Unknown character: %c" (Lexing.lexeme_char lexbuf 0))}
