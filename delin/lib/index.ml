open Protocols

type t = {
  indices : Expr.t list;
  dims : Expr.t list;
  conditions : Exp.bexp list;
}

let reconstruct (idx : t) : Expr.t =
  (* The index at position [j] is scaled by the product of all dims
     from [j] onward (the empty product 1 once [j] runs past the dims
     list). Computing those suffix products right-to-left and reusing
     them keeps reconstruction linear in the dimension count rather
     than recomputing each suffix product from scratch. [suffix] is
     [prod dims[0..]; prod dims[1..]; ..; prod dims[nd-1..]; 1], so its
     head aligns with the first index and is consumed in lock-step. *)
  let rec suffix_products = function
    | [] -> [ Expr.of_int 1 ]
    | d :: ds ->
      (match suffix_products ds with
       | p :: _ as tail -> Expr.( * ) d p :: tail
       | [] -> assert false)
  in
  let rec go indices suffix acc =
    match indices with
    | [] -> acc
    | i :: rest ->
      let mult, suffix' =
        match suffix with
        | m :: ms -> (m, ms)
        | [] -> (Expr.of_int 1, [])
      in
      go rest suffix' (Expr.( + ) acc (Expr.( * ) i mult))
  in
  go idx.indices (suffix_products idx.dims) Expr.zero
