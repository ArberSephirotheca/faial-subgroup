
(* The coefficient map of a polynomial with respect to the parameter
   set: it sends each parameter-monomial mu to the coefficient [mu] p
   (the sum of the numeral parts of every monomial whose parameter
   signature is mu). [find] keys on the squarefree product of a list of
   parameters, which is order-independent, so the whole search is
   permutation-invariant. *)
module Coefficient : sig
  type t
  val of_poly : params:Indet.t list -> Poly.t -> t
  val find : t -> Indet.t list -> Poly.t
end = struct
  type t = Poly.t Monic.Map.t

  let of_poly ~(params : Indet.t list) (p : Poly.t) : t =
    let is_param a = List.exists (fun c -> Indet.compare a c = 0) params in
    Poly.fold (fun term acc ->
      let coeff = Mono.coeff term in
      let (param_sig, numeral_factors) = Monic.partition is_param (Mono.monic term) in
      let numeral = Poly.of_list [ Mono.of_monic ~coeff numeral_factors ] in
      let existing =
        Monic.Map.find_opt param_sig acc |> Option.value ~default:Poly.zero
      in
      Monic.Map.add param_sig (Poly.( + ) existing numeral) acc)
      Monic.Map.empty p

  let find (c : t) (params : Indet.t list) : Poly.t =
    let key = params |> List.map (fun a -> (a, 1)) |> Indet.Map.of_list in
    Monic.Map.find_opt key c |> Option.value ~default:Poly.zero
end

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

(* Reference shape inference: for each ordering of the candidate
   parameters, derive the affine offsets (Algorithm 2) and propose the
   shape [build_dims perm alphas]. *)
module Infer : Algorithm.Infer = struct
  (* For each k in 2..d-1, the integer scalar that satisfies
     [coef(perm \ {p_k}) = alpha_k * f0]; bails out on non-integer
     quotients. Returned list is [alpha_1; ..; alpha_{d-1}] with
     [alpha_1] pinned to 0 (the documented constraint for recovery). *)
  let derive_alphas
      ~(perm : Indet.t list)
      ~(coefs : Coefficient.t)
      ~(f0 : Poly.t)
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
          Poly.try_scalar_quotient
            (Coefficient.find coefs perm_without_pk) f0
        in
        go (k + 1) (a :: acc)
    in
    let* tail = go 2 [] in
    Some (0 :: tail)

  let infer_dimensions ~globals:_ ~size_params accesses =
    List.to_seq accesses
    |> Seq.concat_map (fun expr ->
         params_in_size_params size_params
         |> Stage0.Common.permutations_seq
         |> Seq.filter_map (fun perm ->
              let coefs = Coefficient.of_poly ~params:perm expr in
              let f0 = Coefficient.find coefs perm in
              let d = List.length perm + 1 in
              if d > 1 && Poly.compare f0 Poly.zero = 0 then None
              else derive_alphas ~perm ~coefs ~f0 |> Option.map (build_dims perm)))
end

(* Subscript recovery (Algorithm 3), reformulated to take the shape
   [radix] as input. Each dimension is an affine base [perm_i + alpha_i]
   produced by [build_dims]; we read [(perm_i, alpha_i)] back off it,
   rebuild the coefficient map, and run the recovery. *)
module Decompose : Algorithm.Decompose = struct
  (* Returns d subscripts [f_1; ..; f_d] with [f_1 = f0 = coef(perm)]
     as the outermost. Each later subscript is
     [coef(perm with the first j elements dropped) - contribution],
     where [contribution] sums each prior [f_i] scaled by
     [alpha_{i+1} * .. * alpha_j]. *)
  let derive_fs
      ~(perm : Indet.t list)
      ~(coefs : Coefficient.t)
      ~(alphas : int list)
      ~(f0 : Poly.t)
      : Poly.t list =
    let d = List.length perm + 1 in
    let alpha k = List.nth alphas (k - 1) in
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
        let coef = Coefficient.find coefs tail in
        let prev_fs = List.rev prev_fs_rev in
        let f_j = Poly.( - ) coef (contribution prev_fs j) in
        go (j + 1) (f_j :: prev_fs_rev)
    in
    go 1 [f0]

  (* Read an affine base [a + c] (single bare variable plus a constant)
     back off a dimension polynomial; [None] if not of that form. *)
  let parse_affine (d : Poly.t) : (Indet.t * int) option =
    let ( let* ) = Option.bind in
    let c = Poly.coeff_of Indet.Map.empty d in
    let* a =
      match Poly.to_mono_list (Poly.( - ) d (Poly.of_int c)) with
      | [ m ] when Mono.coeff m = 1 ->
        (match Monic.to_list (Mono.monic m) with [ (v, 1) ] -> Some v | _ -> None)
      | _ -> None
    in
    Some (a, c)

  let delinearize ~(radix : Poly.t list) (expr : Poly.t) : Poly.t list option =
    let ( let* ) = Option.bind in
    let* parsed =
      List.fold_right
        (fun d acc ->
          let* rest = acc in
          let* base = parse_affine d in
          Some (base :: rest))
        radix (Some [])
    in
    let perm = List.map fst parsed in
    let alphas = List.map snd parsed in
    let coefs = Coefficient.of_poly ~params:perm expr in
    let f0 = Coefficient.find coefs perm in
    let fs = derive_fs ~perm ~coefs ~alphas ~f0 in
    let s : Subscript.t = { numeral = fs; radix } in
    if Poly.compare (Subscript.flatten s) expr = 0 then Some fs else None
end

include Algorithm.Driver (Infer) (Decompose)
