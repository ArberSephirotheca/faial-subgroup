open Protocols

[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-type-declaration"]


module Expr : sig
  type t
  module Var : sig (* make wrapper around nexp, handle arbitrary expressions. rename to atom *)
    type t
    val compare : t -> t -> int
    val to_string : t -> string
    val to_nexp : t -> Exp.nexp
    val from_nexp : Exp.nexp -> t

    val induction : string -> t (* should take nexp *)
    val parameter : string -> t

    val is_induction : t -> bool
    val is_parameter : t -> bool
  end
  module Term : sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
    (* val of_int : int -> t *)
    val parameter : string -> t
    val induction : string -> t
    (* val one : t *)
    val ( * ) : t -> t -> t

    val try_div : t -> t -> t option

    val coeff : t -> int
    val factors : t -> (Var.t * int) list
    (* val from_factors : (Var.t * int) list *)

    val fold : (Var.t -> int -> 'a -> 'a) -> 'a -> t -> 'a
    val filter : (Var.t -> int -> bool) -> t -> t

    val has_induction : t -> bool
    val has_parameter : t -> bool
    val is_const : t -> bool
    val nfactors: t -> int
    val to_nexp : t -> Exp.nexp
  end
  val to_string : t -> string
  val param : string -> t
  val ind : string -> t
  val of_int : int -> t
  val of_var : Var.t -> t
  val zero : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t

  val div_mod : t -> Term.t -> t * t

  val fold : (Term.t -> 'a -> 'a) -> 'a -> t -> 'a
  val to_list : t -> Term.t list
  val of_list : Term.t list -> t
  val compare : t -> t -> int
  val from_nexp : Exp.nexp -> t
  val to_nexp : t -> Exp.nexp
end = struct
  module Var = struct
    type t = Param of string | Induction of string

    let induction s = Induction s
    let parameter s = Param s

    let compare x y = match x, y with
      | Param s1, Param s2 -> String.compare s1 s2
      | Induction s1, Induction s2 -> String.compare s1 s2
      | Param _, Induction _ -> -1
      | Induction _, Param _ -> 1
    let to_string = function
      | Param s -> s
      | Induction s -> Printf.sprintf "i_%s" s

    let from_nexp = function
      | Exp.Var v -> let first = String.get v.name 0
          in if first == Char.uppercase_ascii first
            then parameter v.name (* ??? *)
            else induction v.name
      | _ -> failwith "Unsupported operation"


    let to_nexp : t -> Exp.nexp = function
      | Param s | Induction s -> Var (Variable.from_name s)

    let is_induction = function
      | Induction _ -> true
      | _ -> false
    let is_parameter = function
      | Param _ -> true
      | _ -> false
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
    let to_list = VarMap.bindings
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
    let parameter s = (1, VarMap.singleton (Var.Param s) 1)
    let induction s = (1, VarMap.singleton (Var.Induction s) 1)
    let of_int i = (i, VarMap.empty)
    let one = of_int 1

    let ( * ) (c1, t1) (c2, t2) = (c1 * c2, TermInner.( * ) t1 t2)
    let coeff (c, _) = c
    let fold f acc (_, t) = TermInner.fold f acc t
    let filter f (c, t) = c, VarMap.filter f t
    let factors (_, t): (Var.t * int) list = TermInner.to_list t

    let is_const (_, t) = TermInner.is_const t
    let nfactors (_, t) = TermInner.nfactors t
    let has_induction t = t
      |> factors
      |> List.exists (fun (v, _) -> Var.is_induction v)
    let has_parameter t = t
      |> factors
      |> List.exists (fun (v, _) -> Var.is_parameter v)

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

  let of_var (v : Var.t): t = TermMap.singleton (VarMap.singleton v 1) 1


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
  let to_list t: Term.t list = t
        |> TermMap.bindings
        |> List.map (fun (t, c) -> (c, t))
  let of_list (t : Term.t list): t = t
    |>  List.map (fun (c, t) -> (t, c))
    |> TermMap.of_list


  let div_mod (n : t) (d : Term.t) : t * t =
    let q, r = n
    |> to_list
    |> List.partition_map (fun t -> match Term.try_div t d with
      | Some q -> Left q
      | None -> Right t
    ) in
    of_list q, of_list r

  let rec from_nexp (e: Exp.nexp): t =
    match e with
    | Exp.Num n -> of_int n
    | Exp.Binary (N_binary.Plus, a, b) -> from_nexp a + from_nexp b
    | Exp.Binary (N_binary.Mult, a, b) -> from_nexp a * from_nexp b
    | Exp.Binary (N_binary.Minus, a, b) -> from_nexp a - from_nexp b
    | v -> of_var (Var.from_nexp v)

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

(* Extracts all size parameters *)
let size_params (t: Expr.t): Term.t list =
  t
  |> Expr.to_list
  |> List.filter (fun t ->
    Term.has_induction t && Term.has_parameter t
  )
  |> List.map (Term.filter (fun v _ -> Var.is_parameter v))
  |> List.sort_uniq (fun a b -> -compare (Term.nfactors a) (Term.nfactors b))

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
  (*turn this into a fold later*)

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
  let ( let* ) = Option.bind in
  let expr' = Expr.from_nexp expr in
  let* ds = expr'
    |> size_params
    |> dims in
  let is = accesses ds expr' in
  Some {
    indices = List.map Expr.to_nexp is;
    dims = List.map Expr.Term.to_nexp ds;
  }

(* let list_bind (x : 'a list) (f : 'a -> 'b list) : 'b list =
  x |> List.map f |> List.flatten

let loption_bind (x : 'a list option) (f : 'a -> 'b list option) : 'b list option =
  match x with  *)


(* Throwing out conditions for now. eventually will use t as a sort of rewrite template *)
let rewrite_access (acc : Access.t) : Access.t =
  let (let*) = Option.bind in
  (match acc with
  | { index = [a]; _ } ->
    let* result = from_nexp a in
    Some { acc with index = result.indices }
  | _ -> None
  ) |>
  Option.value ~default:acc
(* TODO: this should simply not rewrite if there is a failure *)


(* Global analysis not there yet. will need to collect all accesses in a block *)
let rewrite_unsync (_globals : Variable.Set.t) : Unsync.t -> Unsync.t =
  let open Unsync in
  let rec rewrite_unsync : Unsync.t -> Unsync.t =
  function
  | Access a -> Access (rewrite_access a)
  | Cond (p, b) -> Cond (p, rewrite_unsync b)
  | Loop (r, b) -> Loop (r, rewrite_unsync b)
  | Seq (a, b) -> Seq (rewrite_unsync a, rewrite_unsync b)
  | code -> code
  in
  rewrite_unsync

let rec rewrite_aligned (globals : Variable.Set.t) : Aligned.Code.t -> Aligned.Code.t =
  let open Aligned.Code in
  function
  | Sync c -> Sync (rewrite_unsync globals c)
  | Loop ({ range = {var=x; _}; body; _ } as loop) ->
    Loop { loop with body = rewrite_aligned (Variable.Set.add x globals) body }
  | Seq (a, b) -> Seq (rewrite_aligned globals a, rewrite_aligned globals b)

let rewrite_kernel (kernel : Aligned.Kernel.t) : Aligned.Kernel.t =
  let globals = Params.to_set kernel.global_variables in
  { kernel with code = rewrite_aligned globals kernel.code }
  
(* add function mapping aligned.code to proto *)
(* analysis on each unsync block - some sort of set/map of variable status *)
(* loop bounds are uniform in aligned *)