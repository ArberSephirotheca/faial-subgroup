open Protocols.Gen_z3
open Stage0

module TacticParser = Parser.Make (struct
  type t = Tactic.t

  exception Parsing_error = Tactics_parser.Error

  let parse = Tactics_parser.main Tactics_lexer.read
end)
