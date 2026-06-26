type t = int Indet.Map.t

let compare = Indet.Map.compare Int.compare

let normalize = Indet.Map.filter (fun _ v -> v != 0)
let ( ||> ) (x, y) f = f x y
let ( * ) (t1: t) (t2: t): t = (t1, t2)
  ||> Indet.Map.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 -> Some (v1 + v2)
    | Some v, None | None, Some v -> Some v
    | None, None -> None
  )
  |> normalize
let fold f acc t = Indet.Map.fold f t acc
let to_list = Indet.Map.bindings
let nfactors t = t
  |> Indet.Map.to_list
  |> List.length
let is_const t = nfactors t = 0

module OT = struct
  type nonrec t = t
  let compare = compare
end

module Map = Map.Make (OT)
