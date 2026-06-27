module Decompose : Algorithm.Decompose = struct
  let radix_indets (radix : Poly.t list) : Indet.t list =
    List.concat_map Poly.indets radix |> List.sort_uniq Indet.compare

  let recover (radix : Poly.t list) (p : Poly.t) : Poly.t list =
    let ( let* ) = Option.bind in
    let indets = radix_indets radix in
    let is_param v = List.exists (fun a -> Indet.compare a v = 0) indets in
    let pv = Subscript.place_values radix in
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
      List.map
        (fun (induction_monic, _) -> Poly.of_monic induction_monic)
        solutions
    in
    List.init (n + 1) (fun k ->
        solutions
        |> List.map (fun (_, sol) ->
             (let* c = sol in
              List.nth_opt c k)
             |> Option.value ~default:0)
        |> Poly.linear_combination monic_polys)

  let delinearize ~(radix : Poly.t list) (p : Poly.t) : Poly.t list option =
    let fs = recover radix p in
    let s : Subscript.t = { numeral = fs; radix } in
    if Poly.compare (Subscript.flatten s) p = 0 then Some fs else None
end

include
  Algorithm.Driver
    (Algorithm.Chain
       (Flag.Infer)
       (Algorithm.Chain (Monomial_infer) (Ics15_opt.Infer)))
    (Decompose)

let delin ~(radix : Poly.t list) (p : Poly.t) : Subscript.t option =
  Decompose.delinearize ~radix p
  |> Option.map (fun numeral ->
       { Subscript.numeral; radix })
