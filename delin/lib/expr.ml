open Protocols

type t = int Term_inner.Map.t

let compare = Term_inner.Map.compare Int.compare

let of_int i = match i with
  | 0 -> Term_inner.Map.empty
  | _ -> Term_inner.Map.singleton (Atom.Map.empty) i

let of_atom (v : Atom.t): t = Term_inner.Map.singleton (Atom.Map.singleton v 1) 1

let to_list t: Term.t list = t
  |> Term_inner.Map.bindings
  |> List.map (fun (t, c) -> (c, t))
let of_list (t : Term.t list): t = t
  |>  List.map (fun (c, t) -> (t, c))
  |> Term_inner.Map.of_list
let fold f acc t = Term_inner.Map.fold (fun t c acc -> f (c, t) acc) t acc

let parameter v = Term_inner.Map.singleton (Atom.Map.singleton (Atom.parameter v) 1) 1
let induction v = Term_inner.Map.singleton (Atom.Map.singleton (Atom.induction v) 1) 1
let zero = Term_inner.Map.empty

let to_string (t : t) : string =
  if Term_inner.Map.is_empty t then "Expr.of_list []"
  else t
    |> to_list
    |> List.map (function
      | t -> Printf.sprintf "  %s;\n" (Term.to_string t))
    |> String.concat ""
    |> Printf.sprintf "Expr.of_list [\n%s]"

let ( + ) t1 t2 =
  Term_inner.Map.merge (fun _ v1 v2 -> match v1, v2 with
  | Some v1, Some v2 ->
      (let sum = v1 + v2
      in match v1 + v2 with
      | 0 -> None
      | _ -> Some sum)
  | Some v, None | None, Some v -> Some v
  | None, None -> None) t1 t2
let ( - ) t1 t2 =
  Term_inner.Map.merge (fun _ v1 v2 -> match v1, v2 with
  | Some v1, Some v2 ->
      (let diff = v1 - v2
      in match v1 - v2 with
      | 0 -> None
      | _ -> Some diff)
  | Some v, None -> Some v
  | None, Some v -> Some (-v)
  | None, None -> None) t1 t2
let ( * ) (t1: t) (t2: t): t =
  Term_inner.Map.fold (fun k1 v1 acc ->
    Term_inner.Map.fold (fun k2 v2 acc ->
      let product = Term_inner.(k1 * k2)
      in let coeff = v1 * v2
      in Term_inner.Map.singleton product coeff + acc) t2 acc) t1 zero

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
