module Decompose : Algorithm.Decompose = struct
  let radix_indets (radix : Poly.t list) : Indet.Set.t =
    List.concat_map Poly.indets radix |> Indet.Set.of_list

  let leading_radix_monic (is_radix : Indet.t -> bool) (w : Poly.t) : Monic.t =
    match Poly.to_monic_list w |> List.map (Monic.filter_factor is_radix) with
    | [] -> Monic.empty
    | m :: ms ->
      List.fold_left
        (fun best m ->
          let c = compare (Monic.degree m) (Monic.degree best) in
          if c > 0 || (c = 0 && Monic.compare m best < 0) then m else best)
        m ms

  let recover (radix : Poly.t list) (p : Poly.t) : Poly.t list =
    let ( let* ) = Option.bind in
    let indets = radix_indets radix in
    let is_radix v = Indet.Set.mem v indets in
    let pv = Subscript.place_values radix in
    let lead_monics = List.map (leading_radix_monic is_radix) pv in
    let l_matrix =
      lead_monics
      |> List.map (fun r ->
           pv |> List.map (Poly.coeff_of r) |> Int_linear.Vector.of_list)
      |> Int_linear.Matrix.of_rows
    in
    let numeral_monics =
      Poly.to_monic_list p
      |> List.map (Monic.filter_factor (fun a -> not (is_radix a)))
      |> List.sort_uniq Monic.compare
    in
    let coeffs : Int_linear.Vector.t option list =
      numeral_monics
      |> List.map (fun d ->
           lead_monics
           |> List.map (fun r -> Poly.coeff_of Monic.(r * d) p)
           |> Int_linear.Vector.of_list
           |> Int_linear.tri_solve l_matrix)
    in
    let monics = List.map Poly.of_monic numeral_monics in
    List.init (List.length radix + 1) (fun k ->
        coeffs
        |> List.map (fun c ->
             (let* c = c in Int_linear.Vector.nth_opt c k)
             |> Option.value ~default:0)
        |> Poly.linear_combination monics)

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
  |> Option.map (fun numeral -> { Subscript.numeral; radix })
