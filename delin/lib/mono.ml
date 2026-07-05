open Protocols

type t = int * Monic.t
let compare (c1, t1) (c2, t2) = match c1 - c2 with
  | 0 -> Monic.compare t1 t2
  | x -> x
let to_string (c, t) =
  let ctor = match c with
    | 1 -> "Mono.of_factors"
    | n -> Printf.sprintf "Mono.of_factors ~coeff:%d" n
  in
  if Indet.Map.is_empty t
  then Printf.sprintf "%s []" ctor
  else
    Indet.Map.bindings t
    |> List.map (fun (a, i) -> (Indet.to_string a, i))
    |> List.map (function
    | k, v -> Printf.sprintf "%s, %d" k v)
    |> String.concat "; "
    |> Printf.sprintf "%s [%s]" ctor
let parameter s = (1, Indet.Map.singleton (Indet.parameter s) 1)
let induction s = (1, Indet.Map.singleton (Indet.induction s) 1)

let ( * ) (c1, t1) (c2, t2) = (c1 * c2, Monic.( * ) t1 t2)
let coeff (c, _) = c
let monic (_, t) = t
let fold f acc (_, t) = Monic.fold f acc t
let filter f (c, t) = c, Indet.Map.filter f t
let factors (_, t): (Indet.t * int) list = Monic.to_list t
let of_factors ?(coeff = 1) factors = coeff, Indet.Map.of_list factors
let of_monic ?(coeff = 1) (t : Monic.t) : t = (coeff, t)

let is_const (_, t) = Monic.is_const t
let nfactors (_, t) = Monic.nfactors t
let has_induction t = t
  |> factors
  |> List.exists (fun (v, _) -> Indet.is_induction v)
let has_parameter t = t
  |> factors
  |> List.exists (fun (v, _) -> Indet.is_parameter v)

let rec factor_to_nexp ((factor, exp): Indet.t * int): Exp.nexp = match exp with
  | 0 -> failwith "exponent shouldn't be 0"
  | 1 -> Indet.to_nexp factor
  | n ->
    Binary
      ( N_binary.Mult Signedness.Signed,
        factor_to_nexp (factor, n-1),
        Indet.to_nexp factor )

let to_nexp ((coeff, factors): t): Exp.nexp = match coeff, Monic.to_list factors with
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
| 0 -> let d = Monic.(f1 * (Indet.Map.map (~-) f2))
  in if (Indet.Map.exists (fun _ e -> e < 0) d)
    then None
    else Some (c1 / c2, d)
| _ -> None
