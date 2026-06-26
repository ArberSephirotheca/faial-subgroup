(* Helpers shared verbatim by the reference [Ics15] driver and the
   optimized [Ics15_opt] driver. They are algorithm-agnostic: the
   bucket signature and lookup, scalar scaling, dimension construction,
   and candidate-indeterminate extraction do not depend on how the permutation
   search is organized, so both drivers reuse them rather than forking. *)

(* Multiset key for a list of indeterminates: each indeterminate paired with exponent 1.
   Permutation-invariant, since [Indet.Map.of_list] erases order. *)
let bucket_sig (indets : Indet.t list) : Monic.t =
  indets |> List.map (fun a -> (a, 1)) |> Indet.Map.of_list

let bucket_lookup
    (buckets : Poly.t Monic.Map.t) (indets : Indet.t list) : Poly.t =
  Monic.Map.find_opt (bucket_sig indets) buckets
  |> Option.value ~default:Poly.zero


let build_dims (perm : Indet.t list) (alphas : int list) : Poly.t list =
  List.mapi (fun i p ->
    let a = List.nth alphas i in
    let p_expr = Poly.of_indet p in
    if a = 0 then p_expr
    else Poly.( + ) p_expr (Poly.of_int a))
    perm

(* Candidate parameters must be drawn from the array-shared
   [size_params], not from the per-access expression. Otherwise two
   accesses to one array can pick different shapes, breaking the
   downstream invariant that all accesses agree on dimensionality. *)
let params_in_size_params (sp : Mono.t list) : Indet.t list =
  sp |> List.concat_map (fun t ->
    Mono.factors t |> List.filter_map (fun (a, _) ->
      if Indet.is_parameter a then Some a else None))
  |> List.sort_uniq Indet.compare

(* All orderings of a list, as a lazy sequence. The permutation search
   over candidate parameters drives both ics15 drivers. *)
let rec permutations : 'a list -> 'a list Seq.t = function
  | [] -> Seq.return []
  | xs ->
    List.mapi (fun i x -> (i, x)) xs
    |> List.to_seq
    |> Seq.concat_map (fun (i, x) ->
        let rest = List.filteri (fun j _ -> j <> i) xs in
        Seq.map (fun p -> x :: p) (permutations rest))
