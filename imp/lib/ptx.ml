open Stage0

module BarrierMode = struct
  type t = Sync | Arrive

  let to_string : t -> string = function Sync -> "sync" | Arrive -> "arrive"
end

type t = {
  mode : BarrierMode.t;
  id : int;
  count : int option;
  loc : Location.t option;
}

(* V1 scope: accept `bar.{sync,arrive}[.aligned]` and `barrier.{sync,arrive}[.aligned]`
   with literal-integer operands only. Anything else returns None, and the caller
   drops the node with a warning.

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

let parse ?(loc : Location.t option) (asm_string : string) : t option =
  if Str.string_match re asm_string 0 then
    let mnemonic = Str.matched_group 2 asm_string in
    let id = int_of_string (Str.matched_group 4 asm_string) in
    let count =
      try Some (int_of_string (Str.matched_group 6 asm_string))
      with Not_found -> None
    in
    let mode =
      match mnemonic with
      | "sync" -> BarrierMode.Sync
      | "arrive" -> BarrierMode.Arrive
      | _ -> assert false
    in
    Some { mode; id; count; loc }
  else None

let to_string (p : t) : string =
  let args =
    match p.count with
    | Some c -> Printf.sprintf "%d, %d" p.id c
    | None -> string_of_int p.id
  in
  Printf.sprintf "bar.%s(%s)" (BarrierMode.to_string p.mode) args
