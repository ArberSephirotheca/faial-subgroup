open Protocols.Gen_z3

let parse_string (input : string) : (Tactic.t, string) result =
  let lexbuf = Lexing.from_string input in
  try Ok (Tactics_parser.main Tactics_lexer.read lexbuf) with
  | Tactics_parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in
      let token = Lexing.lexeme lexbuf in
      Error
        (Printf.sprintf "Parse error at line %d, column %d, near token '%s'"
           line col token)
  | Failure msg -> Error ("Lexer error: " ^ msg)
  | exn -> Error (Printexc.to_string exn)

let parse_channel ?(filename = None) (ic : in_channel) :
    (Tactic.t, string) result =
  let lexbuf = Lexing.from_channel ic in
  (* Set filename in lexbuf position info for better error messages *)
  (match filename with
  | Some fname ->
      let open Lexing in
      let pos = lexbuf.lex_curr_p in
      let new_pos = { pos with pos_fname = fname } in
      lexbuf.lex_curr_p <- new_pos;
      lexbuf.lex_start_p <- new_pos
  | None -> ());
  try Ok (Tactics_parser.main Tactics_lexer.read lexbuf) with
  | Tactics_parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in
      let token = Lexing.lexeme lexbuf in
      let location =
        match filename with
        | Some fname ->
            Printf.sprintf "in file '%s' at line %d, column %d" fname line col
        | None -> Printf.sprintf "at line %d, column %d" line col
      in
      Error (Printf.sprintf "Parse error %s, near token '%s'" location token)
  | Failure msg ->
      let prefix =
        match filename with
        | Some fname -> "Lexer error in file '" ^ fname ^ "': "
        | None -> "Lexer error: "
      in
      Error (prefix ^ msg)

let parse_file (filename : string) : (Tactic.t, string) result =
  try
    let ic = open_in filename in
    try
      let result = parse_channel ~filename:(Some filename) ic in
      close_in ic;
      result
    with exn ->
      close_in ic;
      raise exn
  with Sys_error msg -> Error ("File error: " ^ msg)
