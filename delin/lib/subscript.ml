type t = {
  numeral : Poly.t list;
  radix : Poly.t list;
}

let rec place_values = function
  | [] -> [ Poly.of_int 1 ]
  | d :: ds ->
    (match place_values ds with
     | p :: _ as tail -> Poly.( * ) d p :: tail
     | [] -> assert false)

(* Evaluate a subscript into a value *)
let flatten (s : t) : Poly.t =
  Poly.dot s.numeral (place_values s.radix)
