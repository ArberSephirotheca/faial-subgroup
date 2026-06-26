let radix_indets (radix : Poly.t list) : Indet.t list =
  List.concat_map Poly.indets radix |> List.sort_uniq Indet.compare

let recover (radix : Poly.t list) (p : Poly.t) : Poly.t list =
  let ( let* ) = Option.bind in
  let indets = radix_indets radix in
  let is_param v = List.exists (fun a -> Indet.compare a v = 0) indets in
  let pv = Index.place_values radix in
  let n = List.length radix in
  let param_monics =
    pv
    |> List.concat_map (fun w ->
         Poly.to_list w |> List.map (fun m -> fst (Mono.split is_param m)))
    |> List.sort_uniq Monic.compare
  in
  let w_matrix =
    List.map
      (fun param_monic -> List.map (Poly.coeff_of param_monic) pv)
      param_monics
  in
  let induction_monics =
    Poly.to_list p
    |> List.map (fun m -> snd (Mono.split is_param m))
    |> List.sort_uniq Monic.compare
  in
  let solutions =
    induction_monics
    |> List.map (fun induction_monic ->
         ( induction_monic,
           param_monics
           |> List.map (fun param_monic ->
                Poly.coeff_of (Monic.( * ) param_monic induction_monic) p)
           |> Int_linear.int_solve w_matrix ))
  in
  let monic_polys =
    List.map (fun (induction_monic, _) -> Poly.of_monic induction_monic) solutions
  in
  List.init (n + 1) (fun k ->
      solutions
      |> List.map (fun (_, sol) ->
           (let* c = sol in
            List.nth_opt c k)
           |> Option.value ~default:0)
      |> Poly.linear_combination monic_polys)

let delin ~(radix : Poly.t list) (p : Poly.t) : Index.t option =
  let fs = recover radix p in
  let idx : Index.t = { indices = fs; dims = radix; conditions = [] } in
  if Poly.compare (Index.reconstruct idx) p = 0 then Some idx else None

let candidates ~globals:_ ~size_params expr =
  Shape.dims size_params
  |> Option.fold ~none:Seq.empty ~some:(fun dims ->
       let radix = List.map (fun m -> Poly.of_list [ m ]) dims in
       delin ~radix expr |> Option.to_seq)
