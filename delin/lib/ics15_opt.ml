(* Performance-tuned [Ics15]: a pruned shape inference paired with the
   shared recovery [Ics15.Decompose]. Yields the identical candidate
   stream as [Ics15] (asserted by the differential tests and the
   [bench_ics15] benchmark). Two ideas drive the speedup, both resting
   on the coefficient map keying on multisets of candidate
   indeterminates (so it is permutation-invariant):

   (B) Hoist the permutation-invariant work. [Coefficient.of_poly] and
   [f0 = coef(all candidates)] are identical for every permutation, so
   they are computed once. When [f0 = 0] (and there is at least one
   candidate) no permutation can succeed, so the search returns
   immediately.

   (C) Derive alphas from a per-indeterminate table. [alpha_k] depends
   only on which indeterminate is dropped, not on its position, so we
   build a table once and read alphas off it. Position 0 is never
   consulted (alpha_1 is pinned to 0), so a permutation survives exactly
   when every indeterminate after the first has a defined quotient: two
   or more undefined quotients ⇒ nothing survives; otherwise keep only
   the permutations whose tail avoids the undefined-quotient one.
   Survivors are filtered out of the same lazy permutation stream the
   reference enumerates, so their order matches exactly. *)

open Ics15

module Infer : Algorithm.Infer = struct
  let per_access ~size_params expr =
    let indets = params_in_size_params size_params in
    let coefs = Coefficient.of_poly ~params:indets expr in
    let f0 = Coefficient.find coefs indets in
    let d = List.length indets + 1 in
    if d > 1 && Poly.compare f0 Poly.zero = 0 then Seq.empty
    else
      let quot_tbl =
        List.fold_left
          (fun m a ->
            let others =
              List.filter (fun b -> Indet.compare a b <> 0) indets
            in
            let q =
              Poly.try_scalar_quotient (Coefficient.find coefs others) f0
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
      if d > 1 && bad_count >= 2 then Seq.empty
      else
        let survives = function
          | [] -> true
          | _ :: tl -> List.for_all (fun a -> Option.is_some (lookup a)) tl
        in
        indets
        |> Stage0.Common.permutations_seq
        |> Seq.filter survives
        |> Seq.map (fun perm ->
             let tail = match perm with [] -> [] | _ :: tl -> tl in
             let alphas = 0 :: List.map (fun a -> Option.get (lookup a)) tail in
             build_dims perm alphas)

  let infer_dimensions ~globals:_ ~size_params accesses =
    List.to_seq accesses |> Seq.concat_map (per_access ~size_params)
end

include Algorithm.Driver (Infer) (Decompose)
