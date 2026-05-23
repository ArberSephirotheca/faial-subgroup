open Protocols

type t = {
  indices : Expr.t list;
  dims : Expr.t list;
  conditions : Exp.bexp list;
}

let reconstruct (idx : t) : Expr.t =
  let rec go indices dims acc =
    match indices with
    | [] -> acc
    | i :: rest ->
      let mult =
        List.fold_left Expr.( * ) (Expr.of_int 1) dims
      in
      let term = Expr.( * ) i mult in
      let dims' = match dims with [] -> [] | _ :: t -> t in
      go rest dims' (Expr.( + ) acc term)
  in
  go idx.indices idx.dims Expr.zero
