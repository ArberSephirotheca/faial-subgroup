(* Performance-tuned variant of the reference [Ics15] driver. Same
   sound fragment, same results: the differential tests assert this
   module yields the identical [Index.t] stream (same order, same
   Some/None) as [Ics15] on every fixture. Two ideas drive the speedup,
   both resting on the fact that the bucket map keys on multisets of
   candidate indeterminates and so is permutation-invariant:

   (B) Hoist the permutation-invariant work. [group_by_parameters] and
   [f0 = bucket(all candidates)] are identical for every permutation,
   so they are computed once in [candidates] rather than per
   permutation. When [f0 = 0] (and there is at least one candidate)
   no permutation can succeed, so the search returns immediately
   without enumerating a single permutation.

   (C) Derive alphas from a per-indeterminate table. In the reference,
   [alpha_k] for a permutation is the integer scalar satisfying
   [bucket(perm \ {p_k}) = alpha_k * f0]; since [bucket(perm \ {p_k})]
   keys on the multiset "all candidates except [p_k]", that scalar
   depends only on which indeterminate is dropped, not on its position. So we
   build a table [quot a] once, with one entry per candidate indeterminate, and
   read alphas off it. Position 0 of a permutation is never consulted
   by alpha-derivation (alpha_1 is pinned to 0), so a permutation
   survives alpha-derivation exactly when every indeterminate after the first
   has a defined quotient. Two consequences for pruning:
   - two or more indeterminates with an undefined quotient: no permutation can
     place both at position 0, so the search returns nothing;
   - otherwise we keep only the permutations whose tail avoids the
     undefined-quotient indeterminate, skipping the expensive subscript-recovery
     and reconstruction on the rest.
   The surviving permutations are filtered out of the same lazy stream
   the reference enumerates, so their relative order, and therefore the
   first accepted candidate, matches the reference exactly. *)

open Ics15_common

(* Subscript recovery (Algorithm 3), identical to the reference except
   alphas is an [int array] for O(1) indexing instead of [List.nth]. *)
let derive_fs
    ~(perm : Indet.t list)
    ~(buckets : Poly.t Monic.Map.t)
    ~(alphas : int array)
    ~(f0 : Poly.t)
    : Poly.t list =
  let d = List.length perm + 1 in
  let alpha k = alphas.(k - 1) in
  let alpha_prod ~from ~upto =
    let rec go m acc =
      if m > upto then acc else go (m + 1) (acc * alpha m)
    in
    go from 1
  in
  let contribution (prev_fs : Poly.t list) (j : int) : Poly.t =
    prev_fs
    |> List.mapi (fun i _ ->
         if i = 0 then 0 else alpha_prod ~from:(i + 1) ~upto:j)
    |> Poly.linear_combination prev_fs
  in
  let rec go j prev_fs_rev =
    if j > d - 1 then List.rev prev_fs_rev
    else
      let tail = List.filteri (fun idx _ -> idx + 1 > j) perm in
      let bucket = bucket_lookup buckets tail in
      let prev_fs = List.rev prev_fs_rev in
      let f_j = Poly.( - ) bucket (contribution prev_fs j) in
      go (j + 1) (f_j :: prev_fs_rev)
  in
  go 1 [f0]

(* [lookup a] is the precomputed quotient table; the [survives] filter
   guarantees every tail indeterminate has [Some], so [Option.get] is safe. *)
let try_permutation
    ~(buckets : Poly.t Monic.Map.t)
    ~(f0 : Poly.t)
    ~(lookup : Indet.t -> int option)
    ~(expr : Poly.t)
    (perm : Indet.t list)
    : Index.t option =
  let tail = match perm with [] -> [] | _ :: tl -> tl in
  let alphas_list = 0 :: List.map (fun a -> Option.get (lookup a)) tail in
  let alphas = Array.of_list alphas_list in
  let fs = derive_fs ~perm ~buckets ~alphas ~f0 in
  let dims = build_dims perm alphas_list in
  let idx : Index.t = { indices = fs; dims; conditions = [] } in
  if Poly.compare (Index.reconstruct idx) expr = 0 then Some idx else None

let candidates ~globals:_ ~size_params expr =
  let indets = params_in_size_params size_params in
  let buckets = Poly.group_by_parameters ~candidates:indets expr in
  let f0 = bucket_lookup buckets indets in
  let d = List.length indets + 1 in
  (* (B) f0 is permutation-invariant; if it vanishes no permutation
     reconstructs (the [d > 1] guard mirrors the reference's per-perm
     check, which only fires once there is a candidate to permute). *)
  if d > 1 && Poly.compare f0 Poly.zero = 0 then Seq.empty
  else
    (* (C) one quotient per candidate indeterminate, keyed on "drop this indeterminate". *)
    let quot_tbl =
      List.fold_left
        (fun m a ->
          let others =
            List.filter (fun b -> Indet.compare a b <> 0) indets
          in
          let q =
            Poly.try_scalar_quotient (bucket_lookup buckets others) f0
          in
          Indet.Map.add a q m)
        Indet.Map.empty indets
    in
    let lookup a = Indet.Map.find a quot_tbl in
    let bad_count =
      List.fold_left
        (fun n a -> if Option.is_none (lookup a) then n + 1 else n)
        0 indets
    in
    (* Two undefined quotients can never both sit at position 0. *)
    if d > 1 && bad_count >= 2 then Seq.empty
    else
      let survives = function
        | [] -> true
        | _ :: tl -> List.for_all (fun a -> Option.is_some (lookup a)) tl
      in
      indets
      |> permutations
      |> Seq.filter survives
      |> Seq.filter_map (try_permutation ~buckets ~f0 ~lookup ~expr)
