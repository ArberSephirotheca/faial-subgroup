type t = int Atom.Map.t

let compare = Atom.Map.compare Int.compare

let normalize = Atom.Map.filter (fun _ v -> v != 0)
let ( ||> ) (x, y) f = f x y
let ( * ) (t1: t) (t2: t): t = (t1, t2)
  ||> Atom.Map.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 -> Some (v1 + v2)
    | Some v, None | None, Some v -> Some v
    | None, None -> None
  )
  |> normalize
let fold f acc t = Atom.Map.fold f t acc
let to_list = Atom.Map.bindings
let nfactors t = t
  |> Atom.Map.to_list
  |> List.length
let is_const t = nfactors t = 0

module OT = struct
  type nonrec t = t
  let compare = compare
end

module Map = Map.Make (OT)
