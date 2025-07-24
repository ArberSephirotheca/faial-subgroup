open Protocols
open Stage0

module Fvs = struct
  type t = int Variable.Map.t

  let empty : t = Variable.Map.empty
  let get (x : Variable.t) (fvs : t) : int = Variable.Map.find x fvs
  let size (env : t) = Variable.Map.cardinal env

  let to_array (fvs : t) : Variable.t array =
    let vars : Variable.t option array = Array.make (size fvs) None in
    Variable.Map.to_seq fvs
    |> Seq.iter (fun (v, idx) -> Array.set vars idx (Some v));
    vars |> Array.map Option.get

  let add_var (x : Variable.t) (env : t) : t =
    if Variable.Map.mem x env then env
    else
      let next_idx = Variable.Map.cardinal env in
      Variable.Map.add x next_idx env

  let add_n (e : Exp.nexp) (env : t) : t =
    let fvs = Exp.n_free_names e Variable.Set.empty in
    Variable.Set.fold add_var fvs env

  let add_b (e : Exp.bexp) : t -> t = add_n (CastInt e)

  let add_r (r : Range.t) (env : t) : t =
    env |> add_var r.var |> add_n r.lower_bound |> add_n r.upper_bound
    |> add_n (Range.stride r)
end

type t = { ctx : Fvs.t; data : string array }

let var (idx : int) : string = "Arg_" ^ string_of_int idx

let from_fvs (ctx : Fvs.t) : t =
  let data = Common.range (Fvs.size ctx) |> List.map var |> Array.of_list in
  let data =
    if Array.length data = 0 then
      (* CoFloCo does support empty environments. *)
      Array.of_list [ var 0 ]
    else data
  in
  { ctx; data }

let get (x : Variable.t) (env : t) : string = Fvs.get x env.ctx |> var

let n_normalize (e : Exp.nexp) (env : t) : Exp.nexp =
  let fvs = Exp.n_free_names e Variable.Set.empty in
  Variable.Set.fold
    (fun x e ->
      let new_x = Exp.Var (Variable.from_name (get x env)) in
      Subst.ReplacePair.n_subst (x, new_x) e)
    fvs e

let b_normalize (e : Exp.bexp) (env : t) : Exp.bexp =
  let fvs = Exp.b_free_names e Variable.Set.empty in
  Variable.Set.fold
    (fun x e ->
      let new_x = Exp.Var (Variable.from_name (get x env)) in
      Subst.ReplacePair.b_subst (x, new_x) e)
    fvs e

let n_to_string (env : t) (e : Exp.nexp) : string =
  n_normalize e env |> Exp.n_to_string

let b_to_string (env : t) (e : Exp.bexp) : string =
  b_normalize e env |> Exp.b_to_string

let decode (data : string) (env : t) : string =
  Fvs.to_array env.ctx |> Array.to_seqi
  |> Seq.fold_left
       (fun data (idx, x) ->
         let substring = var idx in
         Common.replace ~substring ~by:(Variable.name x) data)
       data

let put (x : Variable.t) (e : Exp.nexp) (env : t) : t =
  let d = Array.copy env.data in
  let idx = Fvs.get x env.ctx in
  let e = n_to_string env e in
  Array.set d idx e;
  { env with data = d }

let to_list (x : t) : string list = x.data |> Array.to_list

type call = { id : int; args : (Variable.t * Exp.nexp) list }

let instantiate (env : t) (c : call) : t =
  List.fold_left (fun env (k, v) -> put k v env) env c.args

let c_to_string (env : t) (x : call) : string =
  let env = instantiate env x |> to_list |> String.concat ", " in
  "inst_" ^ string_of_int x.id ^ "(" ^ env ^ ")"

type rule = { src : int; cost : int; dst : call list; cnd : Exp.bexp list }
type inst = Rule of rule | Comment of string
