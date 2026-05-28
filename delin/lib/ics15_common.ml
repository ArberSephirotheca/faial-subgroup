(* Helpers shared verbatim by the reference [Ics15] driver and the
   optimized [Ics15_opt] driver. They are algorithm-agnostic: the
   bucket signature and lookup, scalar scaling, dimension construction,
   and candidate-atom extraction do not depend on how the permutation
   search is organized, so both drivers reuse them rather than forking. *)

(* Multiset key for a list of atoms: each atom paired with exponent 1.
   Permutation-invariant, since [Atom.Map.of_list] erases order. *)
let bucket_sig (atoms : Atom.t list) : Term_inner.t =
  atoms |> List.map (fun a -> (a, 1)) |> Atom.Map.of_list

let bucket_lookup
    (buckets : Expr.t Term_inner.Map.t) (atoms : Atom.t list) : Expr.t =
  Term_inner.Map.find_opt (bucket_sig atoms) buckets
  |> Option.value ~default:Expr.zero

let scale (n : int) (e : Expr.t) : Expr.t =
  if n = 0 then Expr.zero
  else if n = 1 then e
  else Expr.( * ) (Expr.of_int n) e

let build_dims (perm : Atom.t list) (alphas : int list) : Expr.t list =
  List.mapi (fun i p ->
    let a = List.nth alphas i in
    let p_expr = Expr.of_atom p in
    if a = 0 then p_expr
    else Expr.( + ) p_expr (Expr.of_int a))
    perm

(* Candidate parameters must be drawn from the array-shared
   [size_params], not from the per-access expression. Otherwise two
   accesses to one array can pick different shapes, breaking the
   downstream invariant that all accesses agree on dimensionality. *)
let params_in_size_params (sp : Term.t list) : Atom.t list =
  sp |> List.concat_map (fun t ->
    Term.factors t |> List.filter_map (fun (a, _) ->
      if Atom.is_parameter a then Some a else None))
  |> List.sort_uniq Atom.compare
