open Protocols.Gen_z3

module type PARSER = sig
  type t

  exception Parsing_error

  val parse : Lexing.lexbuf -> t
end

module Make (P : PARSER) = struct
  let parse ?(filename = None) (lexbuf : Lexing.lexbuf) : (P.t, string) Result.t
      =
    try Ok (P.parse lexbuf) with
    | P.Parsing_error ->
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

  let of_string (input : string) : (P.t, string) Result.t =
    input |> Lexing.from_string |> parse

  let of_channel ?(filename = None) (ic : in_channel) : (P.t, string) result =
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
    parse ~filename lexbuf

  let of_filename (filename : string) : (P.t, string) result =
    try
      let ic = open_in filename in
      try
        let result = of_channel ~filename:(Some filename) ic in
        close_in ic;
        result
      with exn ->
        close_in ic;
        raise exn
    with Sys_error msg -> Error ("File error: " ^ msg)
end

module TacticParser = Make (struct
  type t = Tactic.t

  exception Parsing_error = Tactics_parser.Error

  let parse = Tactics_parser.main Tactics_lexer.read
end)

module NExpParser = Make (struct
  type t = Protocols.Exp.nexp

  exception Parsing_error = Exp_parser.Error

  let parse = Exp_parser.nexp_main Exp_lexer.read
end)

module BExpParser = Make (struct
  type t = Protocols.Exp.bexp

  exception Parsing_error = Exp_parser.Error

  let parse = Exp_parser.bexp_main Exp_lexer.read
end)
