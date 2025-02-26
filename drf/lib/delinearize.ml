open Protocols

[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-type-declaration"]


module Expr : sig
  type t
  module Var : sig
    type t = Param of string | Induction of string
    val compare : t -> t -> int
    val to_string : t -> string
    val to_nexp : t -> Exp.nexp
  end
  module Term : sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
    (* val of_int : int -> t *)
    val param : string -> t
    val ind : string -> t
    (* val one : t *)
    val ( * ) : t -> t -> t

    val try_div : t -> t -> t option

    val coeff : t -> int
    val factors : t -> (Var.t * int) list
    val fold : (Var.t -> int -> 'a -> 'a) -> 'a -> t -> 'a
    val filter : (Var.t -> int -> bool) -> t -> t
    val is_const : t -> bool
    val nfactors: t -> int
    val to_nexp : t -> Exp.nexp
  end
  val to_string : t -> string
  val param : string -> t
  val ind : string -> t
  val of_int : int -> t
  val zero : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val fold : (Term.t -> 'a -> 'a) -> 'a -> t -> 'a
  val to_list : t -> Term.t list
  val compare : t -> t -> int
  val from_nexp : Exp.nexp -> t
  val to_nexp : t -> Exp.nexp
end = struct
  module Var = struct
    type t = Param of string | Induction of string
    let compare x y = match x, y with
      | Param s1, Param s2 -> String.compare s1 s2
      | Induction s1, Induction s2 -> String.compare s1 s2
      | Param _, Induction _ -> -1
      | Induction _, Param _ -> 1
    let to_string = function
      | Param s -> s
      | Induction s -> Printf.sprintf "i_%s" s

    let to_nexp : t -> Exp.nexp = function
      | Param s | Induction s -> Var (Variable.from_name s)
  end
  module VarMap = Map.Make(Var)
  module TermInner = struct
    type t = int VarMap.t

    let compare = VarMap.compare Int.compare
    let to_string t = 
      if VarMap.is_empty t
      then "1"
      else
        VarMap.bindings t
        |> List.map (fun (a, i) -> (Var.to_string a, i))
        |> List.map (function
        | k, 1 -> k
        | k, v -> Printf.sprintf "%s^%d" k v)
        |> String.concat " * "

    let normalize = VarMap.filter (fun _ v -> v != 0)
    let (||>) (x, y) f = f x y
    let ( * ) (t1: t) (t2: t): t = (t1, t2)
      ||> VarMap.merge (fun _ v1 v2 -> match v1, v2 with
        | Some v1, Some v2 -> Some (v1 + v2)
        | Some v, None | None, Some v -> Some v
        | None, None -> None
      )
      |> normalize
    let fold f acc t = VarMap.fold f t acc
    let to_list t = VarMap.bindings t
    let nfactors t = t
      |> VarMap.to_list
      |> List.length
      let is_const t = nfactors t = 0

  end

  module Term = struct
    type t = int * TermInner.t
    let compare (c1, t1) (c2, t2) = match c1 - c2 with
      | 0 -> TermInner.compare t1 t2
      | x -> x
    let to_string (c, t) = match c with
      | 0 -> "0"
      | 1 -> TermInner.to_string t
      | _ -> Printf.sprintf "%d * %s" c (TermInner.to_string t)
    let param s = (1, VarMap.singleton (Var.Param s) 1)
    let ind s = (1, VarMap.singleton (Var.Induction s) 1)
    let of_int i = (i, VarMap.empty)
    let one = of_int 1

    let ( * ) (c1, t1) (c2, t2) = (c1 * c2, TermInner.( * ) t1 t2)
    let coeff (c, _) = c
    let fold f acc (_, t) = TermInner.fold f acc t
    let filter f (c, t) = c, VarMap.filter f t
    let factors (_, t): (Var.t * int) list = TermInner.to_list t
    let is_const (_, t) = TermInner.is_const t
    let nfactors (_, t) = TermInner.nfactors t

    let rec factor_to_nexp ((factor, exp): Var.t * int): Exp.nexp = match exp with
      | 0 -> failwith "exponent shouldn't be 0"
      | 1 -> Var.to_nexp factor
      | n -> Binary (N_binary.Mult, factor_to_nexp (factor, n-1), Var.to_nexp factor)

    let to_nexp ((coeff, factors): t): Exp.nexp = match coeff, TermInner.to_list factors with
      | 0, _ -> failwith "coefficient shouldn't be 0"
      | _, [] -> Num coeff
      | 1, x :: xs -> xs |> List.fold_left (fun r x -> Exp.Binary (N_binary.Mult, r, factor_to_nexp x)) (factor_to_nexp x)
      | n, xs -> xs |> List.fold_left (fun r x -> Exp.Binary (N_binary.Mult, r, factor_to_nexp x)) (Exp.Num n)


    let try_div ((c1, f1) : t) ((c2, f2) : t) : t option = match c1 mod c2 with
    | 0 -> let d = TermInner.(f1 * (VarMap.map (~-) f2))
      in if (VarMap.exists (fun _ e -> e < 0) d)
        then None
        else Some (c1 / c2, d)
    | _ -> None
  end

  module TermMap = Map.Make(TermInner)
  type t = int TermMap.t

  let to_string t = 
    if TermMap.is_empty t then "0"
    else TermMap.bindings t
      |> List.map (function
        | k, v when VarMap.is_empty k -> string_of_int v
        | k, 1 -> TermInner.to_string k
        | k, v -> Printf.sprintf "%d * %s" v (TermInner.to_string k))
      |> String.concat " + "

  let compare = TermMap.compare Int.compare

  let of_int i = match i with
    | 0 -> TermMap.empty
    | _ -> TermMap.singleton (VarMap.empty) i

  let param v = TermMap.singleton (VarMap.singleton (Var.Param v) 1) 1
  let ind v = TermMap.singleton (VarMap.singleton (Var.Induction v) 1) 1
  let zero = TermMap.empty

  let ( + ) t1 t2 = 
    TermMap.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 ->
        (let sum = v1 + v2
        in match v1 + v2 with
        | 0 -> None
        | _ -> Some sum)
    | Some v, None | None, Some v -> Some v
    | None, None -> None) t1 t2
  let ( - ) t1 t2 = 
    TermMap.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 ->
        (let diff = v1 - v2
        in match v1 - v2 with
        | 0 -> None
        | _ -> Some diff)
    | Some v, None | None, Some v -> Some v
    | None, None -> None) t1 t2
  let ( * ) (t1: t) (t2: t): t =
    TermMap.fold (fun k1 v1 acc ->
      TermMap.fold (fun k2 v2 acc ->
        let product = TermInner.(k1 * k2)
        in let coeff = v1 * v2
        in TermMap.singleton product coeff + acc) t2 acc) t1 zero
  let fold f acc t = TermMap.fold (fun t c acc -> f (c, t) acc) t acc
  let to_list t: Term.t list = TermMap.bindings t
        |> List.map (fun (t, c) -> (c, t))

  let rec from_nexp (e: Exp.nexp): t =
    match e with
    | Exp.Var v -> 
      let first = String.get v.name 0
      in if first == Char.uppercase_ascii first
        then param v.name
        else ind v.name
    | Exp.Num n -> of_int n
    | Exp.Binary (N_binary.Plus, a, b) -> from_nexp a + from_nexp b
    | Exp.Binary (N_binary.Mult, a, b) -> from_nexp a * from_nexp b
    | Exp.Binary (N_binary.Minus, a, b) -> from_nexp a - from_nexp b
    | _ -> failwith "unsupported expression"

  let to_nexp (e: t): Exp.nexp = 
    match to_list e with
    | [] -> Num 0
    | x :: xs -> xs |> List.fold_left (fun r x -> 
        Exp.Binary(N_binary.Plus, r, Term.to_nexp x)
      ) (Term.to_nexp x) 
end

(* module StringSet = Set.Make(String) *)
module Term = Expr.Term
module Var = Expr.Var

let group (l : 'a list) (f: 'a -> 'a -> bool) : 'a list list =
  let rec group' (l: 'a list) (acc: 'a list list) : 'a list list = match l with
    | [] -> acc
    | x :: xs -> match acc with
      | [] -> group' xs [[x]]
      | y :: ys -> if f x (List.hd y) then group' xs ((x :: y) :: ys) else group' xs ([x] :: acc)
  in group' l []

(* let () = group [1; 1; 2; 3; 1; 1; 4; 4; 5] (fun x y -> x = y)
  |> List.map (fun l -> l |> List.map string_of_int |> String.concat ", ")
  |> String.concat "\n"
  |> print_endline *)

let size_params (t: Expr.t): Term.t list =
  t
  |> Expr.to_list
  |> List.filter (fun t ->
    t
    |> Term.factors
    |> (fun f -> f |> List.exists (function
      | (_, 0) -> failwith "coefficient shouldn't be 0"
      | (Var.Induction _, _) -> true
      | _ -> false
    ) && f |> List.exists (function
      | (_, 0) -> failwith "coefficient shouldn't be 0"
      | (Var.Param _, _) -> true
      | _ -> false
    ))
  )

let dims (t: Term.t list): Term.t list option = 
  let rec loop: Term.t list -> Term.t list option = function
  | [] -> Some []
  | [x] -> Some [x]
  | x :: y :: xs -> 
    let ( let* ) x f = Option.bind x f in
    let* dim = Term.try_div x y in
    let* r = loop (y :: xs) in
    Some (dim :: r)
  in
  t 
  |> List.sort (fun a b -> -compare (Term.nfactors a) (Term.nfactors b))
  |> List.map (Term.filter (fun v _ -> match v with
    | Induction _ -> false
    | _ -> true
  ))
  |> loop

let n1 = Expr.param "n1"
let n2 = Expr.param "n2"
let o0 = Expr.param "o0"
let o1 = Expr.param "o1"
let o2 = Expr.param "o2"
let c = Expr.param "c"
let i = Expr.ind "i"
let j = Expr.ind "j"
let k = Expr.ind "k"

let expr = Expr.(
  n2 * (n1 * o0 + o1) + o2 + n1 * n2 * i + n2 * j + k
)

(* let () = expr
  |> size_params
  |> List.map Expr.Term.to_string
  |> String.concat ", "
  |> print_endline *)

(* let () = Expr.(derive (
    a * i + j
)) |> result_to_string |> print_endline *)


type t = {
  indices: Exp.nexp list;
  dims: Exp.nexp list;
  (* conditions *)
}

let to_string: t -> string = function
  | { indices; dims; } -> 
    Printf.sprintf "{indices = [%s]; dims = [%s]}"
      (indices |> List.map Exp.n_to_string |> String.concat "; ")
      (dims |> List.map Exp.n_to_string |> String.concat "; ")

let from_nexp (expr: Exp.nexp): t option = 
  let res = expr
    |> Expr.from_nexp
    |> size_params
    |> List.map Expr.Term.to_nexp
  (* |> List.map Expr.Term.to_string
  |> String.concat ", "
  |> print_endline *)
  in
    Some {
      indices = res;
      dims = [];
    }


(* debug code *)
module Build = struct
  let var x = Exp.Var (Variable.from_name x)
  let ( + ) a b = Exp.Binary (Plus, a, b)
  let ( * ) a b = Exp.Binary (Mult, a, b) 
end

(* let () =
  match from_nexp Build.( var "x" ) with
  | Some r -> r |> to_string |> print_endline
  | None -> print_endline "error" *)