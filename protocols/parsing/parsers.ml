open Protocols.Gen_z3
open Stage0

module TacticParser = Parser.Make (struct
  type t = Tactic.t

  exception Parsing_error = Tactics_parser.Error

  let parse = Tactics_parser.main Tactics_lexer.read
end)

module NExpParser = Parser.Make (struct
  type t = Protocols.Exp.nexp

  exception Parsing_error = Exp_parser.Error

  let parse = Exp_parser.nexp_main Exp_lexer.read
end)

module BExpParser = Parser.Make (struct
  type t = Protocols.Exp.bexp

  exception Parsing_error = Exp_parser.Error

  let parse = Exp_parser.bexp_main Exp_lexer.read
end)
