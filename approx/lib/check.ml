module Variable = Protocols.Variable
module Params = Protocols.Params

let kernel_env (k : Protocols.Kernel.t) : Variable.Set.t =
  let globals = Params.to_set k.global_variables in
  Variable.Set.union globals Variable.tid_set

type t = { data_independent : bool; control_independent : bool }

let from_code (env : Variable.Set.t) (c : Code.t) : t =
  {
    data_independent = Code.is_data_independent env c;
    control_independent = Code.is_control_independent env c;
  }

let add (env : Variable.Set.t) (r : t) (c : Code.t) : t =
  let data_independent = r.data_independent && Code.is_data_independent env c in
  let control_independent =
    r.control_independent && Code.is_control_independent env c
  in
  { data_independent; control_independent }

let ci_di : t = { control_independent = true; data_independent = true }

let to_string (x : t) =
  let ci = if x.control_independent then "CI" else "CD" in
  let di = if x.data_independent then "DI" else "DD" in
  ci ^ di

let per_kernel (k : Protocols.Kernel.t) : t =
  let env = kernel_env k in
  k |> Code.from_kernel |> Seq.fold_left (add env) ci_di

let per_access (k : Protocols.Kernel.t) : (Code.t * t) list =
  let env = kernel_env k in
  k |> Code.from_kernel
  |> Seq.map (fun (c : Code.t) -> (c, from_code env c))
  |> List.of_seq
