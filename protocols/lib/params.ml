type t = (Exp.bexp * C_type.t) Variable.Map.t

let empty = Variable.Map.empty
let union_left = Variable.MapUtil.union_left
let union_right = Variable.MapUtil.union_right

let default_bound (x : Variable.t) (ty : C_type.t) : Exp.bexp * C_type.t =
  let b =
    ty |> C_type.to_int_dom
    |> Option.value ~default:Int_dom.signed_int
    |> Int_dom.to_bexp x
  in
  (b, ty)

let filter (to_keep : Variable.t -> bool) : t -> t =
  Variable.Map.filter (fun x _ -> to_keep x)

let add ?(bound = None) x ty =
  let p = match bound with Some b -> (b, ty) | None -> default_bound x ty in
  Variable.Map.add x p

let remove_all (s : Variable.Set.t) : t -> t =
  Variable.Set.fold Variable.Map.remove s

let retain_all (s : Variable.Set.t) : t -> t =
  Variable.Map.filter (fun x _ -> Variable.Set.mem x s)

let from_set (ty : C_type.t) (s : Variable.Set.t) : t =
  s |> Variable.Set.to_seq
  |> Seq.map (fun x -> (x, default_bound x ty))
  |> Variable.Map.of_seq

let mem = Variable.Map.mem

let from_list (l : (Variable.t * C_type.t) list) : t =
  l |> List.map (fun (x, ty) -> (x, default_bound x ty)) |> Variable.Map.of_list

let to_list (m : t) : (Variable.t * C_type.t) list =
  m |> Variable.Map.to_list |> List.map (fun (x, (_, ty)) -> (x, ty))

let to_set (m : t) : Variable.Set.t = Variable.MapSetUtil.map_to_set m

let to_bexp (m : t) : Exp.bexp =
  Variable.Map.fold (fun _ (b1, _) b2 -> Exp.b_and b1 b2) m (Bool true)

let to_string (m : t) : string =
  m |> Variable.Map.bindings
  |> List.sort (fun k1 k2 -> Variable.compare (fst k1) (fst k2))
  |> List.map (fun (k, (_, ty)) -> C_type.to_string ty ^ " " ^ Variable.name k)
  |> String.concat ", "
  |> fun l -> "[" ^ l ^ "]"
