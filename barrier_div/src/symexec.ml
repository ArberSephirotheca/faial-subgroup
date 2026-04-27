open Protocols

(* Top-level entry for the participant analysis. Takes a kernel,
   preprocesses it, runs the symbolic execution to a normal form, and
   classifies any stuck residual into Diagnostic.t entries.

   The preprocessing pipeline mirrors check.ml: inline integer
   parameters into globals, add missing binders for free names, run
   constant folding, then alpha-rename loop and decl variables for
   uniqueness so Rule L's iteration-variable extraction is sound. *)

let setup ~(params : (string * int) list) (k : Kernel.t) : Kernel.t =
  let k =
    k
    |> Kernel.inline_globals params
    |> Kernel.add_missing_binders
    |> Kernel.opt
  in
  let used = Kernel.parameter_set k in
  { k with code = Code.vars_distinct k.code used }

let check ?(params : (string * int) list = []) ?(timeout = 0)
    (cfg : Rel_cost.Config.t) (k : Kernel.t) : Diagnostic.t list =
  let k = setup ~params k in
  let initial : Thread.t = { path_cond = k.pre; proto = k.code } in
  let locals = Kernel.local_set k in
  let final = State.reduce ~timeout cfg locals (State.initial initial) in
  Diagnostic.of_state ~timeout cfg locals final

let is_safe ?params ?timeout (cfg : Rel_cost.Config.t) (k : Kernel.t) : bool =
  check ?params ?timeout cfg k = []
