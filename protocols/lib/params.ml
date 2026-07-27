type t = (Exp.bexp * Ty.t) Variable.Map.t

let empty = Variable.Map.empty
let union_left = Variable.MapUtil.union_left
let union_right = Variable.MapUtil.union_right

(* A parameter keeps its declared type for printing; the bound is the
   constraint that type imposes on the parameter. The bound is a hypothesis
   about an input, so a type the analysis cannot classify contributes none:
   assuming one would rule out an argument the kernel accepts, and a race
   that only that argument reaches would go unreported. *)
let default_bound (x : Variable.t) (ty : Ty.t) : Exp.bexp * Ty.t =
  (Exp.ty_bound (Exp.Var x) ty, ty)

let filter (to_keep : Variable.t -> bool) : t -> t =
  Variable.Map.filter (fun x _ -> to_keep x)

let add ?(bound = None) x ty =
  let p = match bound with Some b -> (b, ty) | None -> default_bound x ty in
  Variable.Map.add x p

let remove_all (s : Variable.Set.t) : t -> t =
  Variable.Set.fold Variable.Map.remove s

let retain_all (s : Variable.Set.t) : t -> t =
  Variable.Map.filter (fun x _ -> Variable.Set.mem x s)

let reset_kind ~(kernel_parameters : Variable.Set.t) (m : t) : t =
  let loop_variables = Variable.Set.empty in
  let reset_v = Variable.reset_kind ~kernel_parameters ~loop_variables in
  let reset_b = Exp.reset_variable_kind_b ~kernel_parameters ~loop_variables in
  Variable.Map.fold
    (fun x (b, ty) acc -> Variable.Map.add (reset_v x) (reset_b b, ty) acc)
    m empty

let from_set (ty : Ty.t) (s : Variable.Set.t) : t =
  s |> Variable.Set.to_seq
  |> Seq.map (fun x -> (x, default_bound x ty))
  |> Variable.Map.of_seq

let mem = Variable.Map.mem
let find_opt = Variable.Map.find_opt

let from_list (l : (Variable.t * Ty.t) list) : t =
  l |> List.map (fun (x, ty) -> (x, default_bound x ty)) |> Variable.Map.of_list

let to_list (m : t) : (Variable.t * Ty.t) list =
  m |> Variable.Map.to_list |> List.map (fun (x, (_, ty)) -> (x, ty))

let to_set (m : t) : Variable.Set.t = Variable.MapSetUtil.map_to_set m

let to_bexp (m : t) : Exp.bexp =
  Variable.Map.fold (fun _ (b1, _) b2 -> Exp.b_and b1 b2) m (Bool true)

let to_string (m : t) : string =
  m |> Variable.Map.bindings
  |> List.sort (fun k1 k2 -> Variable.compare (fst k1) (fst k2))
  |> List.map (fun (k, (_, ty)) -> Ty.to_string ty ^ " " ^ Variable.name k)
  |> String.concat ", "
  |> fun l -> "[" ^ l ^ "]"
