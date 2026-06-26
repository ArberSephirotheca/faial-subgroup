open Protocols

type t = int * Term_inner.t
let compare (c1, t1) (c2, t2) = match c1 - c2 with
  | 0 -> Term_inner.compare t1 t2
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

let ( * ) (c1, t1) (c2, t2) = (c1 * c2, Term_inner.( * ) t1 t2)
let coeff (c, _) = c
let fold f acc (_, t) = Term_inner.fold f acc t
let filter f (c, t) = c, Atom.Map.filter f t
let factors (_, t): (Atom.t * int) list = Term_inner.to_list t
let of_factors ?(coeff = 1) factors = coeff, Atom.Map.of_list factors

let is_const (_, t) = Term_inner.is_const t
let nfactors (_, t) = Term_inner.nfactors t
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

let to_nexp ((coeff, factors): t): Exp.nexp = match coeff, Term_inner.to_list factors with
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
| 0 -> let d = Term_inner.(f1 * (Atom.Map.map (~-) f2))
  in if (Atom.Map.exists (fun _ e -> e < 0) d)
    then None
    else Some (c1 / c2, d)
| _ -> None
