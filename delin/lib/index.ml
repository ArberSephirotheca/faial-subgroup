open Protocols

type t = {
  indices : Poly.t list;
  dims : Poly.t list;
  conditions : Exp.bexp list;
}

(* The positional weight of each subscript: [place_values dims] is
   [prod dims[0..]; prod dims[1..]; ..; prod dims[nd-1..]; 1], the
   product of all dims from each position onward (the empty product 1
   once a position runs past the dims list). Computed right-to-left so
   each suffix product reuses the next, keeping it linear in the
   dimension count. *)
let rec place_values = function
  | [] -> [ Poly.of_int 1 ]
  | d :: ds ->
    (match place_values ds with
     | p :: _ as tail -> Poly.( * ) d p :: tail
     | [] -> assert false)

let reconstruct (idx : t) : Poly.t =
  Poly.dot idx.indices (place_values idx.dims)
