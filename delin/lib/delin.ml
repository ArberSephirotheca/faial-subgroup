open Protocols

module Atom : sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
  val to_nexp : t -> Exp.nexp
  val from_nexp : globals:Variable.Set.t -> Exp.nexp -> t

  val induction : string -> t
  val parameter : string -> t

  val is_induction : t -> bool
  val is_parameter : t -> bool

  (* When the atom is a bare induction variable [Var v], return [Some v].
     Returns [None] for parameters or compound induction expressions. *)
  val as_induction_var : t -> Variable.t option

  module Map : Map.S with type key = t
end = struct
  type t =
    | Induction of Exp.nexp
    | Parameter of Exp.nexp

  let induction s = Induction (Exp.Var (Variable.from_name s))
  let parameter s = Parameter (Exp.Var (Variable.from_name s))

  let to_nexp : t -> Exp.nexp = function
    | Induction n | Parameter n -> n

  let is_induction = function Induction _ -> true | Parameter _ -> false
  let is_parameter = function Parameter _ -> true | Induction _ -> false

  let as_induction_var = function
    | Induction (Exp.Var v) -> Some v
    | _ -> None

  let compare x y = match Exp.n_compare (to_nexp x) (to_nexp y) with
    | 0 -> compare (is_parameter x) (is_parameter y)
    | n -> n

  let to_string = function
    | Parameter n -> Exp.n_to_string n ^ " (global)"
    | Induction n -> Exp.n_to_string n

  let from_nexp ~globals (value : Exp.nexp) : t =
    let thread_global =
      let free = Exp.n_free_names value Variable.Set.empty in
      Variable.Set.diff free globals |> Variable.Set.is_empty
    in
    if thread_global then Parameter value else Induction value


  module OT = struct
    type nonrec t = t
    let compare = compare
  end

  module Map = Map.Make (OT)
end

module TermInner = struct
  type t = int Atom.Map.t

  let compare = Atom.Map.compare Int.compare

  let normalize = Atom.Map.filter (fun _ v -> v != 0)
  let ( ||> ) (x, y) f = f x y
  let ( * ) (t1: t) (t2: t): t = (t1, t2)
    ||> Atom.Map.merge (fun _ v1 v2 -> match v1, v2 with
      | Some v1, Some v2 -> Some (v1 + v2)
      | Some v, None | None, Some v -> Some v
      | None, None -> None
    )
    |> normalize
  let fold f acc t = Atom.Map.fold f t acc
  let to_list = Atom.Map.bindings
  let nfactors t = t
    |> Atom.Map.to_list
    |> List.length
    let is_const t = nfactors t = 0

  module OT = struct
    type nonrec t = t
    let compare = compare
  end

  module Map = Map.Make (OT)
end

module Term : sig
  type t = int * TermInner.t
  val compare : t -> t -> int
  val to_string : t -> string
  val parameter : string -> t
  val induction : string -> t
  val ( * ) : t -> t -> t

  val try_div : t -> t -> t option

  val coeff : t -> int
  val factors : t -> (Atom.t * int) list
  val of_factors : ?coeff:int -> (Atom.t * int) list -> t

  val fold : (Atom.t -> int -> 'a -> 'a) -> 'a -> t -> 'a
  val filter : (Atom.t -> int -> bool) -> t -> t

  val has_induction : t -> bool
  val has_parameter : t -> bool
  val is_const : t -> bool
  val nfactors: t -> int
  val to_nexp : t -> Exp.nexp
end = struct
  type t = int * TermInner.t
  let compare (c1, t1) (c2, t2) = match c1 - c2 with
    | 0 -> TermInner.compare t1 t2
    | x -> x
  let to_string (c, t) =
    let ctor = match c with
      | 1 -> "Term.of_factors"
      | n -> Printf.sprintf "Term.of_factors ~coeff:%d" n
    in
    if Atom.Map.is_empty t
    then Printf.sprintf "%s []" ctor
    else
      Atom.Map.bindings t
      |> List.map (fun (a, i) -> (Atom.to_string a, i))
      |> List.map (function
      | k, v -> Printf.sprintf "%s, %d" k v)
      |> String.concat "; "
      |> Printf.sprintf "%s [%s]" ctor
  let parameter s = (1, Atom.Map.singleton (Atom.parameter s) 1)
  let induction s = (1, Atom.Map.singleton (Atom.induction s) 1)

  let ( * ) (c1, t1) (c2, t2) = (c1 * c2, TermInner.( * ) t1 t2)
  let coeff (c, _) = c
  let fold f acc (_, t) = TermInner.fold f acc t
  let filter f (c, t) = c, Atom.Map.filter f t
  let factors (_, t): (Atom.t * int) list = TermInner.to_list t
  let of_factors ?(coeff = 1) factors = coeff, Atom.Map.of_list factors

  let is_const (_, t) = TermInner.is_const t
  let nfactors (_, t) = TermInner.nfactors t
  let has_induction t = t
    |> factors
    |> List.exists (fun (v, _) -> Atom.is_induction v)
  let has_parameter t = t
    |> factors
    |> List.exists (fun (v, _) -> Atom.is_parameter v)

  let rec factor_to_nexp ((factor, exp): Atom.t * int): Exp.nexp = match exp with
    | 0 -> failwith "exponent shouldn't be 0"
    | 1 -> Atom.to_nexp factor
    | n ->
      Binary
        ( N_binary.Mult Signedness.Signed,
          factor_to_nexp (factor, n-1),
          Atom.to_nexp factor )

  let to_nexp ((coeff, factors): t): Exp.nexp = match coeff, TermInner.to_list factors with
    | 0, _ -> failwith "coefficient shouldn't be 0"
    | _, [] -> Num coeff
    | 1, x :: xs ->
      xs
      |> List.fold_left
           (fun r x ->
             Exp.Binary (N_binary.Mult Signedness.Signed, r, factor_to_nexp x))
           (factor_to_nexp x)
    | n, xs ->
      xs
      |> List.fold_left
           (fun r x ->
             Exp.Binary (N_binary.Mult Signedness.Signed, r, factor_to_nexp x))
           (Exp.Num n)


  let try_div ((c1, f1) : t) ((c2, f2) : t) : t option =
  match c1 mod c2 with
  | 0 -> let d = TermInner.(f1 * (Atom.Map.map (~-) f2))
    in if (Atom.Map.exists (fun _ e -> e < 0) d)
      then None
      else Some (c1 / c2, d)
  | _ -> None
end

module Expr : sig
  type t
  val to_string : t -> string
  val parameter : string -> t
  val induction : string -> t
  val of_int : int -> t
  val of_atom : Atom.t -> t
  val zero : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t

  val div_mod : t -> Term.t -> t * t

  val fold : (Term.t -> 'a -> 'a) -> 'a -> t -> 'a
  val to_list : t -> Term.t list
  val of_list : Term.t list -> t
  val compare : t -> t -> int
  val from_nexp : globals:Variable.Set.t -> Exp.nexp -> t
  val to_nexp : t -> Exp.nexp
end = struct
  type t = int TermInner.Map.t

  let compare = TermInner.Map.compare Int.compare

  let of_int i = match i with
    | 0 -> TermInner.Map.empty
    | _ -> TermInner.Map.singleton (Atom.Map.empty) i

  let of_atom (v : Atom.t): t = TermInner.Map.singleton (Atom.Map.singleton v 1) 1

  let to_list t: Term.t list = t
    |> TermInner.Map.bindings
    |> List.map (fun (t, c) -> (c, t))
  let of_list (t : Term.t list): t = t
    |>  List.map (fun (c, t) -> (t, c))
    |> TermInner.Map.of_list
  let fold f acc t = TermInner.Map.fold (fun t c acc -> f (c, t) acc) t acc

  let parameter v = TermInner.Map.singleton (Atom.Map.singleton (Atom.parameter v) 1) 1
  let induction v = TermInner.Map.singleton (Atom.Map.singleton (Atom.induction v) 1) 1
  let zero = TermInner.Map.empty

  let to_string (t : t) : string =
    if TermInner.Map.is_empty t then "Expr.of_list []"
    else t
      |> to_list
      |> List.map (function
        | t -> Printf.sprintf "  %s;\n" (Term.to_string t))
      |> String.concat ""
      |> Printf.sprintf "Expr.of_list [\n%s]"

  let ( + ) t1 t2 =
    TermInner.Map.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 ->
        (let sum = v1 + v2
        in match v1 + v2 with
        | 0 -> None
        | _ -> Some sum)
    | Some v, None | None, Some v -> Some v
    | None, None -> None) t1 t2
  let ( - ) t1 t2 =
    TermInner.Map.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 ->
        (let diff = v1 - v2
        in match v1 - v2 with
        | 0 -> None
        | _ -> Some diff)
    | Some v, None -> Some v
    | None, Some v -> Some (-v)
    | None, None -> None) t1 t2
  let ( * ) (t1: t) (t2: t): t =
    TermInner.Map.fold (fun k1 v1 acc ->
      TermInner.Map.fold (fun k2 v2 acc ->
        let product = TermInner.(k1 * k2)
        in let coeff = v1 * v2
        in TermInner.Map.singleton product coeff + acc) t2 acc) t1 zero

  let div_mod (n : t) (d : Term.t) : t * t =
    let q, r = n
    |> to_list
    |> List.partition_map (fun t -> match Term.try_div t d with
      | Some q -> Left q
      | None -> Right t
    ) in
    of_list q, of_list r

  let rec from_nexp ~(globals) (e: Exp.nexp): t =
    match e with
    | Exp.Num n -> of_int n
    | Exp.Binary (N_binary.Plus _, a, b) -> from_nexp ~globals a + from_nexp ~globals b
    | Exp.Binary (N_binary.Mult _, a, b) -> from_nexp ~globals a * from_nexp ~globals b
    | Exp.Binary (N_binary.Minus _, a, b) -> from_nexp ~globals a - from_nexp ~globals b
    | v -> of_atom (Atom.from_nexp ~globals v)

  let to_nexp (e: t): Exp.nexp =
    match to_list e with
    | [] -> Num 0
    | x :: xs -> xs |> List.fold_left (fun r x ->
        Exp.Binary(N_binary.Plus Signedness.Signed, r, Term.to_nexp x)
      ) (Term.to_nexp x)
end

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

module Index = struct
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
end

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
    : Expr.t TermInner.Map.t =
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
      TermInner.Map.find_opt param_sig acc
      |> Option.value ~default:Expr.zero
    in
    TermInner.Map.add param_sig (Expr.( + ) existing induct_expr) acc
  ) TermInner.Map.empty e

(* Try to express [a] as [k * b] for an integer scalar [k]. Pick any
   non-zero term of [b], read off the matching term of [a], compute
   the candidate scalar, then verify the whole [a == k * b]. *)
let try_scalar_quotient (a : Expr.t) (b : Expr.t) : int option =
  match Expr.to_list b with
  | [] -> None
  | (c1, fm1) :: _ ->
    let matching =
      Expr.to_list a
      |> List.find_opt (fun (_, fm) -> TermInner.compare fm fm1 = 0)
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

module type DelinAlgorithm = sig
  val candidates :
    globals:Variable.Set.t ->
    size_params:Term.t list ->
    Expr.t ->
    Index.t Seq.t
end

module Greedy : DelinAlgorithm = struct
  let candidates ~globals:_ ~size_params expr =
    match dims size_params with
    | None -> Seq.empty
    | Some ds ->
      let is = accesses ds expr in
      let dims_e = List.map (fun t -> Expr.of_list [t]) ds in
      Seq.return
        { Index.indices = is;
          dims = dims_e;
          conditions = [];
        }
end

(* "Optimistic Delinearization of Parametrically Sized Arrays"
   (Grosser et al., ICS'15), section 4, sound fragment: permutation
   search + Algorithm 2 alpha-derivation + Algorithm 3 subscript
   recovery, restricted to candidates whose derivations are exact at
   the polynomial-ring level. The redundancy-based consistency check
   is skipped; final soundness is verified by reconstructing the
   linearised form via [Index.reconstruct] and comparing against the
   input. Assumes alpha_1 = 0 (Algorithm 3's documented constraint). *)
module ICS15 : DelinAlgorithm = struct
  let bucket_sig (atoms : Atom.t list) : TermInner.t =
    atoms |> List.map (fun a -> (a, 1)) |> Atom.Map.of_list

  let bucket_lookup
      (buckets : Expr.t TermInner.Map.t) (atoms : Atom.t list) : Expr.t =
    TermInner.Map.find_opt (bucket_sig atoms) buckets
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
      ~(buckets : Expr.t TermInner.Map.t)
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
          try_scalar_quotient (bucket_lookup buckets perm_without_pk) f0
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
      ~(buckets : Expr.t TermInner.Map.t)
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
    let buckets = group_by_parameters ~candidates:perm expr in
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
    |> permutations
    |> Seq.filter_map (fun perm -> try_permutation perm expr)
end
