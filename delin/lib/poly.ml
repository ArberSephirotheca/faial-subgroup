open Protocols

type t = int Monic.Map.t

let compare = Monic.Map.compare Int.compare

let of_int i = match i with
  | 0 -> Monic.Map.empty
  | _ -> Monic.Map.singleton (Indet.Map.empty) i

let of_indet (v : Indet.t): t = Monic.Map.singleton (Indet.Map.singleton v 1) 1

let of_monic (s : Monic.t): t = Monic.Map.singleton s 1

let to_list t: Mono.t list = t
  |> Monic.Map.bindings
  |> List.map (fun (t, c) -> (c, t))
let of_list (t : Mono.t list): t = t
  |>  List.map (fun (c, t) -> (t, c))
  |> Monic.Map.of_list
let fold f acc t = Monic.Map.fold (fun t c acc -> f (c, t) acc) t acc

let parameter v = Monic.Map.singleton (Indet.Map.singleton (Indet.parameter v) 1) 1
let induction v = Monic.Map.singleton (Indet.Map.singleton (Indet.induction v) 1) 1
let zero = Monic.Map.empty

let to_string (t : t) : string =
  if Monic.Map.is_empty t then "Poly.of_list []"
  else t
    |> to_list
    |> List.map (function
      | t -> Printf.sprintf "  %s;\n" (Mono.to_string t))
    |> String.concat ""
    |> Printf.sprintf "Poly.of_list [\n%s]"

let ( + ) t1 t2 =
  Monic.Map.merge (fun _ v1 v2 -> match v1, v2 with
  | Some v1, Some v2 ->
      (let sum = v1 + v2
      in match v1 + v2 with
      | 0 -> None
      | _ -> Some sum)
  | Some v, None | None, Some v -> Some v
  | None, None -> None) t1 t2
let ( - ) t1 t2 =
  Monic.Map.merge (fun _ v1 v2 -> match v1, v2 with
  | Some v1, Some v2 ->
      (let diff = v1 - v2
      in match v1 - v2 with
      | 0 -> None
      | _ -> Some diff)
  | Some v, None -> Some v
  | None, Some v -> Some (-v)
  | None, None -> None) t1 t2
let ( * ) (t1: t) (t2: t): t =
  Monic.Map.fold (fun k1 v1 acc ->
    Monic.Map.fold (fun k2 v2 acc ->
      let product = Monic.(k1 * k2)
      in let coeff = v1 * v2
      in Monic.Map.singleton product coeff + acc) t2 acc) t1 zero

let div_mod (n : t) (d : Mono.t) : t * t =
  let q, r = n
  |> to_list
  |> List.partition_map (fun t -> match Mono.try_div t d with
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
  | v -> of_indet (Indet.from_nexp ~globals v)

let to_nexp (e: t): Exp.nexp =
  match to_list e with
  | [] -> Num 0
  | x :: xs -> xs |> List.fold_left (fun r x ->
      Exp.Binary(N_binary.Plus Signedness.Signed, r, Mono.to_nexp x)
    ) (Mono.to_nexp x)

(* Group monomials by their factor signature restricted to a given
   candidate indeterminate set. Key: multiset of [candidates] indeterminates appearing
   in the monomial. Value: induction-only polynomial summed from the
   monomial parts excluding those [candidates] factors. Indeterminates outside
   [candidates] (including other parameter indeterminates) flow to the
   induction side, so they end up inside the bucket's polynomial value
   rather than partitioning the key space. *)
let group_by_parameters ~(candidates : Indet.t list) (e : t)
    : t Monic.Map.t =
  let is_candidate a =
    List.exists (fun c -> Indet.compare a c = 0) candidates
  in
  fold (fun term acc ->
    let coeff = Mono.coeff term in
    let (param_sig, induct_factors) =
      Mono.fold (fun a n (p, i) ->
        if is_candidate a then (Indet.Map.add a n p, i)
        else (p, Indet.Map.add a n i))
        (Indet.Map.empty, Indet.Map.empty)
        term
    in
    let induct_expr = of_list [(coeff, induct_factors)] in
    let existing =
      Monic.Map.find_opt param_sig acc
      |> Option.value ~default:zero
    in
    Monic.Map.add param_sig (existing + induct_expr) acc
  ) Monic.Map.empty e

(* Try to express [a] as [k * b] for an integer scalar [k]. Pick any
   non-zero term of [b], read off the matching term of [a], compute
   the candidate scalar, then verify the whole [a == k * b]. *)
let try_scalar_quotient (a : t) (b : t) : int option =
  match to_list b with
  | [] -> None
  | (c1, fm1) :: _ ->
    let matching =
      to_list a
      |> List.find_opt (fun (_, fm) -> Monic.compare fm fm1 = 0)
    in
    (match matching with
     | None ->
       if to_list a = [] then Some 0 else None
     | Some (c2, _) ->
       if c1 = 0 || c2 mod c1 <> 0 then None
       else
         let k = c2 / c1 in
         let scaled =
           to_list b
           |> List.map (fun (c, fm) -> (Stdlib.( * ) k c, fm))
           |> of_list
         in
         if compare a scaled = 0 then Some k else None)

let coeff_of (s : Monic.t) (p : t) : int =
  Monic.Map.find_opt s p |> Option.value ~default:0

let scale (n : int) (e : t) : t =
  if n = 0 then zero
  else if n = 1 then e
  else of_int n * e

let indets (p : t) : Indet.t list =
  fold (fun m acc -> Mono.fold (fun a _ acc -> a :: acc) acc m) [] p
  |> List.sort_uniq Indet.compare

let dot (a : t list) (b : t list) : t =
  List.fold_left2 (fun acc x y -> acc + (x * y)) zero a b

let linear_combination (ps : t list) (weights : int list) : t =
  List.fold_left2 (fun acc p n -> acc + scale n p) zero ps weights
