open Protocols

let n_is_uniform (locals:Variable.Set.t) (e:Exp.nexp) : bool =
  locals
  |> Variable.Set.inter (Exp.n_free_names e Variable.Set.empty)
  |> Variable.Set.is_empty

let r_is_uniform (locals:Variable.Set.t) (r:Range.t) : bool =
  let open Range in
  n_is_uniform locals r.lower_bound && n_is_uniform locals r.upper_bound

let b_is_uniform (locals:Variable.Set.t) (e:Exp.bexp) : bool =
  locals
  |> Variable.Set.inter (Exp.b_free_names e Variable.Set.empty)
  |> Variable.Set.is_empty

let rec c_is_uniform (locals:Variable.Set.t) : Bank.Code.t -> bool =
  function
  | Index _ -> true
  | Loop {range=r; body=p} ->
    r_is_uniform locals r && c_is_uniform locals p
  | Cond (e, p) ->
    b_is_uniform locals e && c_is_uniform locals p
  | Decl (x, p) ->
    c_is_uniform (Variable.Set.add x locals) p

let is_uniform (k:Bank.t) : bool =
  c_is_uniform
    (Variable.Set.union k.local_variables Variable.tid_set)
    k.code

(* ============================== *)

let n_is_divergent : Exp.nexp -> bool =
  Exp.n_exists Variable.is_tid

let r_is_divergent : Range.t -> bool =
  Range.exists Variable.is_tid

let b_is_divergent : Exp.bexp -> bool =
  Exp.b_exists Variable.is_tid

let rec c_is_divergent : Bank.Code.t -> bool =
  function
  | Index _ -> false
  | Loop {range=r; body=p} ->
    r_is_divergent r || c_is_divergent p
  | Cond (e, p) ->
    b_is_divergent e || c_is_divergent p
  | Decl (_, p) ->
    c_is_divergent p

let is_divergent (k:Bank.t) : bool =
  c_is_divergent k.code

type t =
  | ThreadUniform
  | ThreadDivergent
  | PossiblyThreadDivergent

let is_thread_uniform (x:t) : bool =
  x = ThreadUniform

let is_known : t -> bool =
  function
  | ThreadUniform | ThreadDivergent -> true
  | PossiblyThreadDivergent -> false

let from_bank (b:Bank.t) : t =
  if is_uniform b then
    ThreadUniform
  else if is_divergent b then
    ThreadDivergent
  else
    PossiblyThreadDivergent

let to_string : t -> string =
  function
  | ThreadUniform -> "thread-uniform"
  | ThreadDivergent -> "thread-divergent"
  | PossiblyThreadDivergent -> "potentially thread-divergent"
