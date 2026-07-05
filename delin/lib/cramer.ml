module Decompose : Algorithm.Decompose = struct
  (** Returns the indeterminates of a radix *)
  let radix_indets (radix : Poly.t list) : Indet.Set.t =
    List.concat_map Poly.indets radix |> Indet.Set.of_list

  let recover (radix : Poly.t list) (p : Poly.t) : Poly.t list =
    let ( let* ) = Option.bind in
    let indets = radix_indets radix in
    let is_radix v = Indet.Set.mem v indets in
    (* compute the place-value vector *)
    let pv = Subscript.place_values radix in
    (* generate the row-labels of W(r) *)
    let radix_monics =
      pv
      |> List.concat_map (fun w ->
           Poly.to_monic_list w
           (* only keep monics whose factor is a radix, so [x.N] becomes [N] *)
           |> List.map (Monic.filter_factor is_radix)
        )
      |> List.sort_uniq Monic.compare
    in
    (* Build W *)
    let w_matrix =
      (* For each radix monic r extract the coefficient: [r] p *)
      List.map
        (fun r -> List.map (Poly.extract_coeff r) pv)
        radix_monics
    in
    let numeral_monics =
      Poly.to_monic_list p
      |> List.map (Monic.filter_factor (fun a -> not (is_radix a)))
      |> List.sort_uniq Monic.compare
    in
    (* Get the coefficients of each numeral *)
    let coeffs =
      numeral_monics
      |> List.map (fun d ->
           (* Solves system `W ([d] n) = [r d] P` for `[d] n` *)
           radix_monics
           |> List.map (fun r ->
                Poly.extract_coeff (Monic.(r  * d)) p)
           |> Int_linear.int_solve w_matrix)
    in
    let monics = List.map Poly.of_monic numeral_monics in
    (* Reconstruct the coefficients *)
    List.init (List.length radix + 1) (fun k ->
        coeffs
        |> List.map (fun c ->
            (let* c = c in List.nth_opt c k)
            |> Option.value ~default:0
          )
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
  |> Option.map (fun numeral ->
       { Subscript.numeral; radix })
