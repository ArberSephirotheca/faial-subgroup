open Stage0
open Protocols

(* V1 scope: accept `bar.{sync,arrive}[.aligned]` and
   `barrier.{sync,arrive}[.aligned]` with literal-integer operands only.
   Anything else returns None, and the caller drops the node with a warning.

   Supported forms (whitespace-tolerant, trailing `;` optional):
     bar.sync N
     bar.sync N, M
     bar.arrive N, M
     barrier.sync N[, M]
     barrier.arrive N[, M]
     bar.sync.aligned N[, M]   (* .aligned is a codegen hint; stripped *)
*)
let re =
  Str.regexp
    "^[ \t]*\\(bar\\|barrier\\)\\.\\(sync\\|arrive\\)\\(\\.aligned\\)?[ \t]+\
     \\([0-9]+\\)\\([ \t]*,[ \t]*\\([0-9]+\\)\\)?[ \t]*;?[ \t]*$"

let parse ?(loc : Location.t option) (asm_string : string) : Sync.t option =
  if Str.string_match re asm_string 0 then
    let mnemonic = Str.matched_group 2 asm_string in
    let id = int_of_string (Str.matched_group 4 asm_string) in
    let count =
      try Some (int_of_string (Str.matched_group 6 asm_string))
      with Not_found -> None
    in
    let mode =
      match mnemonic with
      | "sync" -> Sync.Mode.Sync
      | "arrive" -> Sync.Mode.Arrive
      | _ -> assert false
    in
    Some Sync.{ mode; id; count; loc }
  else None
