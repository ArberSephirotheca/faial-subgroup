let monic_degree (s : Monic.t) : int =
  Monic.to_list s |> List.fold_left (fun acc (_, e) -> acc + e) 0

let graded_compare (a : Monic.t) (b : Monic.t) : int =
  match Int.compare (monic_degree a) (monic_degree b) with
  | 0 -> Monic.compare a b
  | c -> c

let mono_degree (m : Mono.t) : int =
  Mono.factors m |> List.fold_left (fun acc (_, e) -> acc + e) 0

let degree (p : Poly.t) : int =
  Poly.fold (fun m acc -> max acc (mono_degree m)) 0 p

let rec gcd (a : int) (b : int) : int = if b = 0 then abs a else gcd b (a mod b)

(* Divide an integer vector by the gcd of its entries and fix the sign so the
   first nonzero entry is positive. *)
let primitive (v : int array) : int array =
  let g = Array.fold_left (fun g x -> gcd g x) 0 v in
  if g = 0 then v
  else
    let v = Array.map (fun x -> x / g) v in
    let lead = Array.fold_left (fun acc x -> if acc <> 0 then acc else x) 0 v in
    if lead < 0 then Array.map ( ~- ) v else v

(* Fraction-free Gaussian elimination over a fixed coordinate basis: returns a
   basis of the row space (the pivots), each made primitive. Columns are
   processed in order; rows reaching zero are dropped. *)
let row_reduce (rows : int array list) : int array list =
  let ncols = match rows with [] -> 0 | r :: _ -> Array.length r in
  let rec go col rows acc =
    if col >= ncols then List.rev acc
    else
      match List.partition (fun r -> r.(col) <> 0) rows with
      | [], _ -> go (col + 1) rows acc
      | pivot :: rest_nz, zeros ->
        let elim r =
          let a = pivot.(col) and b = r.(col) in
          primitive (Array.mapi (fun k rk -> (a * rk) - (b * pivot.(k))) r)
        in
        go (col + 1) (List.map elim rest_nz @ zeros) (primitive pivot :: acc)
  in
  go 0 (List.map Array.copy rows) []

let column_space (polys : Poly.t list) : Poly.t list =
  let polys = List.filter (fun p -> Poly.compare p Poly.zero <> 0) polys in
  let monos =
    polys
    |> List.concat_map (fun p -> List.map snd (Poly.to_list p))
    |> List.sort_uniq Monic.compare
    |> List.sort (fun a b -> -graded_compare a b)
  in
  let index = Array.of_list monos in
  let to_vec p = Array.map (fun s -> Poly.coeff_of s p) index in
  let of_vec v =
    Array.to_list v
    |> List.mapi (fun i c -> (c, index.(i)))
    |> List.filter (fun (c, _) -> c <> 0)
    |> Poly.of_list
  in
  List.map to_vec polys
  |> row_reduce
  |> List.map of_vec
  |> List.sort (fun a b -> Int.compare (degree a) (degree b))

let leading (p : Poly.t) : Mono.t option =
  match Poly.to_list p with
  | [] -> None
  | m :: ms ->
    Some
      (List.fold_left
         (fun best m -> if graded_compare (snd m) (snd best) > 0 then m else best)
         m ms)

let rec divide_exact (num : Poly.t) (den : Poly.t) : Poly.t option =
  if Poly.compare num Poly.zero = 0 then Some Poly.zero
  else
    let ( let* ) = Option.bind in
    let* ld = leading den in
    let* ln = leading num in
    let* q = Mono.try_div ln ld in
    let qp = Poly.of_list [ q ] in
    let num' = Poly.( - ) num (Poly.( * ) qp den) in
    let* rest = divide_exact num' den in
    Some (Poly.( + ) qp rest)
