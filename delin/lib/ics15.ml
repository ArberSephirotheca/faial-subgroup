(* "Optimistic Delinearization of Parametrically Sized Arrays"
   (Grosser et al., ICS'15), section 4, sound fragment: permutation
   search + Algorithm 2 alpha-derivation + Algorithm 3 subscript
   recovery, restricted to candidates whose derivations are exact at
   the polynomial-ring level. The redundancy-based consistency check
   is skipped; final soundness is verified by reconstructing the
   linearised form via [Index.reconstruct] and comparing against the
   input. Assumes alpha_1 = 0 (Algorithm 3's documented constraint). *)

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

(* For each k in 2..d-1, the integer scalar that satisfies
   [bucket(perm \ {p_k}) = alpha_k * f0]; bails out on non-integer
   quotients. Returned list is [alpha_1; ..; alpha_{d-1}] with
   [alpha_1] pinned to 0 (the documented constraint for the
   subscript-recovery step). *)
let derive_alphas
    ~(perm : Atom.t list)
    ~(buckets : Expr.t Term_inner.Map.t)
    ~(f0 : Expr.t)
    : int list option =
  let ( let* ) = Option.bind in
  let d = List.length perm + 1 in
  let rec go k acc =
    if k > d - 1 then Some (List.rev acc)
    else
      let perm_without_pk =
        List.filteri (fun i _ -> i + 1 <> k) perm
      in
      let* a =
        Polynomial.try_scalar_quotient
          (bucket_lookup buckets perm_without_pk) f0
      in
      go (k + 1) (a :: acc)
  in
  let* tail = go 2 [] in
  Some (0 :: tail)

(* Returns d subscripts [f_1; ..; f_d] with [f_1 = f0 = bucket(perm)]
   as the outermost (the coefficient of the highest-degree parameter
   monomial). Each later subscript is recovered as
   [bucket(perm with the first j elements dropped) - contribution],
   where [contribution] sums each prior [f_i] scaled by
   [alpha_{i+1} * .. * alpha_j]. *)
let derive_fs
    ~(perm : Atom.t list)
    ~(buckets : Expr.t Term_inner.Map.t)
    ~(alphas : int list)
    ~(f0 : Expr.t)
    : Expr.t list =
  let d = List.length perm + 1 in
  let alpha k = List.nth alphas (k - 1) in
  let alpha_prod ~from ~upto =
    let rec go m acc =
      if m > upto then acc else go (m + 1) (acc * alpha m)
    in
    go from 1
  in
  let contribution (prev_fs : Expr.t list) (j : int) : Expr.t =
    List.fold_left
      (fun (sum, i) f_i ->
        let next =
          if i = 0 then sum
          else
            Expr.( + ) sum
              (scale (alpha_prod ~from:(i + 1) ~upto:j) f_i)
        in
        (next, i + 1))
      (Expr.zero, 0)
      prev_fs
    |> fst
  in
  let rec go j prev_fs_rev =
    if j > d - 1 then List.rev prev_fs_rev
    else
      let tail = List.filteri (fun idx _ -> idx + 1 > j) perm in
      let bucket = bucket_lookup buckets tail in
      let prev_fs = List.rev prev_fs_rev in
      let f_j = Expr.( - ) bucket (contribution prev_fs j) in
      go (j + 1) (f_j :: prev_fs_rev)
  in
  go 1 [f0]

let build_dims (perm : Atom.t list) (alphas : int list) : Expr.t list =
  List.mapi (fun i p ->
    let a = List.nth alphas i in
    let p_expr = Expr.of_atom p in
    if a = 0 then p_expr
    else Expr.( + ) p_expr (Expr.of_int a))
    perm

let try_permutation (perm : Atom.t list) (expr : Expr.t) : Index.t option =
  let buckets = Polynomial.group_by_parameters ~candidates:perm expr in
  let f0 = bucket_lookup buckets perm in
  let d = List.length perm + 1 in
  if d > 1 && Expr.compare f0 Expr.zero = 0 then None
  else
    let ( let* ) = Option.bind in
    let* alphas = derive_alphas ~perm ~buckets ~f0 in
    let fs = derive_fs ~perm ~buckets ~alphas ~f0 in
    let dims = build_dims perm alphas in
    let idx : Index.t = { indices = fs; dims; conditions = [] } in
    if Expr.compare (Index.reconstruct idx) expr = 0 then Some idx
    else None

(* Candidate parameters must be drawn from the array-shared
   [size_params], not from the per-access expression. Otherwise two
   accesses to one array can pick different shapes, breaking the
   downstream invariant that all accesses agree on dimensionality. *)
let params_in_size_params (sp : Term.t list) : Atom.t list =
  sp |> List.concat_map (fun t ->
    Term.factors t |> List.filter_map (fun (a, _) ->
      if Atom.is_parameter a then Some a else None))
  |> List.sort_uniq Atom.compare

let candidates ~globals:_ ~size_params expr =
  params_in_size_params size_params
  |> Polynomial.permutations
  |> Seq.filter_map (fun perm -> try_permutation perm expr)
