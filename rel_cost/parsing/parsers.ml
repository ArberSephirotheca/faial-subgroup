open Stage0

module NExpParser = Parser.Make (struct
  type t = Protocols.Exp.nexp

  exception Parsing_error = Theorem_parser.Error

  let parse = Theorem_parser.nexp_main Theorem_lexer.read
end)

module BExpParser = Parser.Make (struct
  type t = Protocols.Exp.bexp

  exception Parsing_error = Theorem_parser.Error

  let parse = Theorem_parser.bexp_main Theorem_lexer.read
end)

module TheoremFileParser = Parser.Make (struct
  type t = Theorem_file.t

  exception Parsing_error = Theorem_parser.Error

  let parse = Theorem_parser.file_main Theorem_lexer.read
end)
