open Exp

type t = { facts : bexp; goal : bexp }

let make (goal : bexp) : t = { facts = Bool true; goal }
let assume (b : bexp) (f : t) : t = { f with facts = b_and b f.facts }
let facts (f : t) : bexp = f.facts
let goal (f : t) : bexp = f.goal
let map_goal (g : bexp -> bexp) (f : t) : t = { f with goal = g f.goal }
let negate_goal (f : t) : t = { f with goal = b_not f.goal }

let read_postcondition (n : nexp) : bexp option =
  match n with
  | ReadResult r -> r.ty |> Option.map (scalar_bound n)
  | _ -> None

let add_declarations (b : bexp) : bexp =
  let calls = b_calls b in
  let from_calls = List.filter_map Functions.postcondition calls in
  let from_reads = List.filter_map read_postcondition calls in
  List.fold_left b_and b (from_calls @ from_reads)

let to_bexp (f : t) : bexp =
  b_and f.facts f.goal
  |> Predicates.b_inline
  |> Predicates.strip_cross_thread
  |> add_declarations

let free_names (f : t) (init : Variable.Set.t) : Variable.Set.t =
  init |> b_free_names f.facts |> b_free_names f.goal
