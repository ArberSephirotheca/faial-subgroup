open Protocols

(* The parameter-pure coefficient columns of the accesses: for each access,
   group its monomials by numeral signature [nu]; the column for [nu] is the
   parameter-pure polynomial [sum_mu C[mu] * mu]. Concatenating over accesses
   is the joint sum of the per-access column spaces. *)
let columns (accesses : Poly.t list) : Poly.t list =
  accesses
  |> List.concat_map (fun p ->
       let terms =
         Poly.to_mono_list p
         |> List.map (fun m ->
              let mu, nu = Monic.partition Indet.is_parameter (Mono.monic m) in
              (nu, Mono.of_monic ~coeff:(Mono.coeff m) mu))
       in
       terms
       |> List.map fst
       |> List.sort_uniq Monic.compare
       (* Drop the empty-signature column: a monomial with no induction factor
          is an additive offset (e.g. blockDim.x*blockIdx.x), not a stride, so it
          must never become a place value. *)
       |> List.filter (fun nu -> not (Monic.is_const nu))
       |> List.map (fun nu ->
            terms
            |> List.filter_map (fun (nu', cm) ->
                 if Monic.compare nu nu' = 0 then Some cm else None)
            |> Poly.of_list))

(* All combinations [sum c_i * g_i] with each [c_i] in {-1, 0, 1}. *)
let rec combos (gens : Poly.t list) : Poly.t list =
  match gens with
  | [] -> [ Poly.zero ]
  | g :: rest ->
    combos rest
    |> List.concat_map (fun t -> [ t; Poly.( + ) t g; Poly.( - ) t g ])

(* All maximal divisibility chains from place value [pv] down to the constant,
   over the place-value span generators [gens] (ascending degree). Each chain is
   returned as the list of dimensions (consecutive quotients), outermost first.
   The next place value [pv'] is enumerated as a leading generator plus small
   corrections; it is kept when it divides [pv] exactly with a non-constant
   quotient (the dimension). *)
let rec flags (pv : Poly.t) (gens : Poly.t list) : Poly.t list list =
  if Vspace.degree pv = 0 then [ [] ]
  else
    let lower = List.filter (fun g -> Vspace.degree g < Vspace.degree pv) gens in
    lower
    |> List.concat_map (fun gj ->
         let corr =
           List.filter (fun g -> Vspace.degree g < Vspace.degree gj) lower
         in
         combos corr
         |> List.filter_map (fun c ->
              let pv' = Poly.( + ) gj c in
              match Vspace.divide_exact pv pv' with
              | Some d when Vspace.degree d > 0 -> Some (d, pv')
              | _ -> None))
    |> List.concat_map (fun (d, pv') ->
         flags pv' gens |> List.map (fun rest -> d :: rest))

let infer ~globals:(_ : Variable.Set.t) (accesses : Poly.t list) :
    Poly.t list Seq.t =
  let gens = Vspace.column_space (columns accesses) in
  match List.rev gens with
  | [] -> Seq.empty
  | pv0 :: _ ->
    flags pv0 gens
    |> List.sort_uniq (fun a b ->
         match Int.compare (List.length b) (List.length a) with
         | 0 -> List.compare Poly.compare a b
         | c -> c)
    |> List.to_seq

module Infer : Algorithm.Infer = struct
  let infer_dimensions ~globals ~size_params:_ accesses = infer ~globals accesses
end
