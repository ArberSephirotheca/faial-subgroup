(* Array-shape recovery helpers shared by the delinearization
   decomposers: extract the candidate size parameters from an access,
   divide them into per-dimension extents, and peel a flat address
   apart by those extents. *)

let size_params (t : Poly.t) : Mono.t list = t
  |> Poly.to_list
  |> List.filter (fun t -> Mono.has_induction t && Mono.has_parameter t)
  |> List.map (Mono.filter (fun v _ -> Indet.is_parameter v))
  |> List.sort_uniq Mono.compare
  |> List.sort (fun a b -> -compare (Mono.nfactors a) (Mono.nfactors b))

let size_params_all (ts : Poly.t list) : Mono.t list = ts
  |> List.concat_map size_params
  |> List.sort_uniq Mono.compare
  |> List.sort (fun a b -> -compare (Mono.nfactors a) (Mono.nfactors b))

(* Divides out size params *)
let rec dims: Mono.t list -> Mono.t list option = function
  | [] -> Some []
  | [x] -> Some [x]
  | x :: y :: ys ->
    let ( let* ) = Option.bind in
    let* dim = Mono.try_div x y in
    let* r = dims (y :: ys) in
    Some (dim :: r)

let accesses (dims : Mono.t list) (t : Poly.t): Poly.t list =
  let rec loop rdims t = match rdims with
  | [] -> [t]
  | d :: ds -> let q, r = Poly.div_mod t d in
    r :: loop ds q
  in
  t |> loop (List.rev dims)
  |> List.rev
