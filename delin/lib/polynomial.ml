let size_params (t : Expr.t) : Term.t list = t
  |> Expr.to_list
  |> List.filter (fun t -> Term.has_induction t && Term.has_parameter t)
  |> List.map (Term.filter (fun v _ -> Atom.is_parameter v))
  |> List.sort_uniq Term.compare
  |> List.sort (fun a b -> -compare (Term.nfactors a) (Term.nfactors b))

let size_params_all (ts : Expr.t list) : Term.t list = ts
  |> List.concat_map size_params
  |> List.sort_uniq Term.compare
  |> List.sort (fun a b -> -compare (Term.nfactors a) (Term.nfactors b))

(* Divides out size params *)
let rec dims: Term.t list -> Term.t list option = function
  | [] -> Some []
  | [x] -> Some [x]
  | x :: y :: ys ->
    let ( let* ) = Option.bind in
    let* dim = Term.try_div x y in
    let* r = dims (y :: ys) in
    Some (dim :: r)

let accesses (dims : Term.t list) (t : Expr.t): Expr.t list =
  let rec loop rdims t = match rdims with
  | [] -> [t]
  | d :: ds -> let q, r = Expr.div_mod t d in
    r :: loop ds q
  in
  t |> loop (List.rev dims)
  |> List.rev

let parameter_atoms (e : Expr.t) : Atom.t list =
  Expr.fold (fun term acc ->
    Term.fold (fun a _ acc ->
      if Atom.is_parameter a then a :: acc else acc
    ) acc term
  ) [] e
  |> List.sort_uniq Atom.compare

(* Group polynomial terms by their factor signature restricted to a
   given candidate parameter set. Key: multiset of [candidates] atoms
   appearing in the term. Value: induction-only polynomial summed
   from the term parts excluding those [candidates] factors. Atoms
   outside [candidates] (including other parameter atoms) flow to the
   induction side, so they end up inside the bucket's polynomial
   value rather than partitioning the key space. *)
let group_by_parameters ~(candidates : Atom.t list) (e : Expr.t)
    : Expr.t Term_inner.Map.t =
  let is_candidate a =
    List.exists (fun c -> Atom.compare a c = 0) candidates
  in
  Expr.fold (fun term acc ->
    let coeff = Term.coeff term in
    let (param_sig, induct_factors) =
      Term.fold (fun a n (p, i) ->
        if is_candidate a then (Atom.Map.add a n p, i)
        else (p, Atom.Map.add a n i))
        (Atom.Map.empty, Atom.Map.empty)
        term
    in
    let induct_expr = Expr.of_list [(coeff, induct_factors)] in
    let existing =
      Term_inner.Map.find_opt param_sig acc
      |> Option.value ~default:Expr.zero
    in
    Term_inner.Map.add param_sig (Expr.( + ) existing induct_expr) acc
  ) Term_inner.Map.empty e

(* Try to express [a] as [k * b] for an integer scalar [k]. Pick any
   non-zero term of [b], read off the matching term of [a], compute
   the candidate scalar, then verify the whole [a == k * b]. *)
let try_scalar_quotient (a : Expr.t) (b : Expr.t) : int option =
  match Expr.to_list b with
  | [] -> None
  | (c1, fm1) :: _ ->
    let matching =
      Expr.to_list a
      |> List.find_opt (fun (_, fm) -> Term_inner.compare fm fm1 = 0)
    in
    (match matching with
     | None ->
       if Expr.to_list a = [] then Some 0 else None
     | Some (c2, _) ->
       if c1 = 0 || c2 mod c1 <> 0 then None
       else
         let k = c2 / c1 in
         let scaled =
           Expr.to_list b
           |> List.map (fun (c, fm) -> (k * c, fm))
           |> Expr.of_list
         in
         if Expr.compare a scaled = 0 then Some k else None)

let rec permutations : 'a list -> 'a list Seq.t = function
  | [] -> Seq.return []
  | xs ->
    List.mapi (fun i x -> (i, x)) xs
    |> List.to_seq
    |> Seq.concat_map (fun (i, x) ->
        let rest = List.filteri (fun j _ -> j <> i) xs in
        Seq.map (fun p -> x :: p) (permutations rest))
