open Protocols

(* How to treat a call whose callee is declared but never defined in the
   translation unit. Ordered from loosest to strictest by how much is
   silently skipped. *)
type t =
  (* Skip every such call, which is what faial did before the policy
     existed: the callee's effects vanish and the kernel is analysed as
     though the call were never written. *)
  | Skip_all
  (* Skip only a callee that cannot write through any parameter. *)
  | Skip_without_arrays
  (* Skip nothing: any callee without a visible body is opaque. *)
  | Skip_none

let default : t = Skip_without_arrays
let all : t list = [ Skip_all; Skip_without_arrays; Skip_none ]

let to_string : t -> string = function
  | Skip_all -> "skip-all"
  | Skip_without_arrays -> "skip-without-arrays"
  | Skip_none -> "skip-none"

let parse (x : string) : t option =
  List.find_opt (fun p -> to_string p = x) all

let enum : (string * t) list = List.map (fun p -> (to_string p, p)) all

(* Whether a body-less declaration denotes effects faial cannot see, so
   that a kernel reaching it must be discarded rather than analysed.

   A function the [Functions] registry models is never opaque, whatever
   the policy. Its applications lower to an [NCall] carrying the
   registry's postcondition, and that lowering is selected by the
   signature lookup missing, so recording the declaration here would
   turn [log2(x)] into a call to a body that does not exist. *)
let is_opaque (policy : t) ~(name : string) ~(params : C_lang.Param.t list) :
    bool =
  if Functions.supported name then false
  else
    match policy with
    | Skip_all -> false
    | Skip_none -> true
    | Skip_without_arrays ->
        List.exists (C_lang.Param.matches Ty.writes_through) params
