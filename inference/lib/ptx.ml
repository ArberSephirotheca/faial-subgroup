open Stage0
open Protocols

(* V2 scope: accept `bar.{sync,arrive}[.aligned]` and
   `barrier.{sync,arrive}[.aligned]`. Each numeric operand may be either
   a literal integer or a `%N` placeholder; placeholders are resolved by
   indexing into the [operands] list (positions count outputs first, then
   inputs, matching the GCC inline-asm convention).

   Unrecognized templates return None, and the caller drops the node with
   a warning. A `%N` whose corresponding operand could not be lifted to
   an [Exp.nexp] (passed as [None] in the operand list) also makes the
   parser return None.

   Supported forms (whitespace-tolerant, trailing `;` optional):
     bar.sync N
     bar.sync N, M
     bar.sync %0
     bar.sync %0, %1
     bar.arrive N, M
     barrier.sync N[, M]
     bar.sync.aligned N[, M]   (* .aligned is a codegen hint; stripped *)
*)
let re =
  Str.regexp
    "^[ \t]*\\(bar\\|barrier\\)\\.\\(sync\\|arrive\\)\\(\\.aligned\\)?[ \t]+\
     \\(%[0-9]+\\|[0-9]+\\)\\([ \t]*,[ \t]*\\(%[0-9]+\\|[0-9]+\\)\\)?\
     [ \t]*;?[ \t]*$"

let bar_array : Variable.t = Variable.from_name "bar"

(* Resolve a single operand token. Literal integers always succeed;
   a `%N` placeholder is looked up in [operands] (and may itself be [None]
   if the C-side expression couldn't be lifted to an nexp). *)
let resolve_token (operands : Exp.nexp option list) (tok : string)
    : Exp.nexp option =
  if String.length tok > 0 && tok.[0] = '%' then
    let i = int_of_string (String.sub tok 1 (String.length tok - 1)) in
    Option.bind (List.nth_opt operands i) (fun x -> x)
  else
    Some (Exp.Num (int_of_string tok))

let parse
    ?(loc : Location.t option)
    ?(operands : Exp.nexp option list = [])
    (asm_string : string) : Sync.t option =
  let ( let* ) = Option.bind in
  if Str.string_match re asm_string 0 then
    let mnemonic = Str.matched_group 2 asm_string in
    let mode : Sync.Mode.t =
      match mnemonic with
      | "sync" -> Sync.Mode.Sync
      | "arrive" -> Sync.Mode.Arrive
      | _ -> assert false
    in
    let* id = resolve_token operands (Str.matched_group 4 asm_string) in
    let count_tok =
      try Some (Str.matched_group 6 asm_string) with Not_found -> None
    in
    let* count =
      match count_tok with
      | None -> Some None
      | Some tok -> Option.map Option.some (resolve_token operands tok)
    in
    Some Sync.{ mode; array = bar_array; index = [ id ]; count; loc }
  else None
