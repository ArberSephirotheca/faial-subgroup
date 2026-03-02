{
open Tactics_parser
}

rule read = parse
  | [' ' '\t' '\r' '\n']  { read lexbuf }
  | "true"                { BOOL(true) }
  | "false"               { BOOL(false) }
  | ['0'-'9']+ as i       { INT(int_of_string i) }
  | ['0'-'9']+ "." ['0'-'9']* as f { FLOAT(float_of_string f) }
  | '"'                   { read_string (Buffer.create 16) lexbuf }
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
  | "print"               { PRINT }
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

and read_string buf = parse
  | '"'                   { STRING (Buffer.contents buf) }
  | '\\' '"'              { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\'             { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'              { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'              { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' 'r'              { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 'b'              { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'              { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' (['0'-'9'] as c1) (['0'-'9'] as c2) (['0'-'9'] as c3)
                          { let code = 100 * (Char.code c1 - Char.code '0') +
                                       10 * (Char.code c2 - Char.code '0') +
                                       (Char.code c3 - Char.code '0') in
                            if code > 255 then
                              failwith (Printf.sprintf "Invalid escape sequence: \\%c%c%c" c1 c2 c3)
                            else
                              Buffer.add_char buf (Char.chr code);
                            read_string buf lexbuf }
  | '\\' _                { failwith (Printf.sprintf "Invalid escape sequence: %s" (Lexing.lexeme lexbuf)) }
  | eof                   { failwith "Unterminated string literal" }
  | _ as c                { Buffer.add_char buf c; read_string buf lexbuf }
