module Decompose : Algorithm.Decompose = struct
  let delinearize ~(radix : Poly.t list) (expr : Poly.t) : Poly.t list option =
    let ( let* ) = Option.bind in
    let* monos =
      List.fold_right
        (fun d acc ->
          let* rest = acc in
          let* m = Poly.to_mono d in
          Some (m :: rest))
        radix (Some [])
    in
    let numeral = Shape.accesses monos expr in
    let s : Subscript.t = { numeral; radix } in
    if Poly.compare (Subscript.flatten s) expr = 0 then Some numeral else None
end

include Algorithm.Driver (Monomial_infer) (Decompose)
