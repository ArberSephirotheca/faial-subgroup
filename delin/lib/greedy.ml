module Decompose : Algorithm.Decompose = struct
  let as_mono (p : Poly.t) : Mono.t option =
    match Poly.to_list p with [ m ] -> Some m | _ -> None

  let delinearize ~(radix : Poly.t list) (expr : Poly.t) : Poly.t list option =
    let ( let* ) = Option.bind in
    let* monos =
      List.fold_right
        (fun d acc ->
          let* rest = acc in
          let* m = as_mono d in
          Some (m :: rest))
        radix (Some [])
    in
    let numeral = Shape.accesses monos expr in
    let s : Subscript.t = { numeral; radix } in
    if Poly.compare (Subscript.flatten s) expr = 0 then Some numeral else None
end

include Algorithm.Driver (Monomial_infer) (Decompose)
