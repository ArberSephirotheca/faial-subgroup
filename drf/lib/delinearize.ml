open Stage0
open Protocols

(* [@@@warning "-unused-value-declaration"]
[@@@warning "-unused-type-declaration"] *)


let list_to_string (f : 'a -> string) (l : 'a list): string =
  "[" ^ (l |> List.map f |> String.concat "; ") ^ "]"
let option_to_string (f : 'a -> string): 'a option -> string = function
| None -> "none"
| Some x -> f x 

module Expr : sig
  type t
  module Atom : sig (* make wrapper around nexp, handle arbitrary expressions. rename to atom *)
    type t
    val compare : t -> t -> int
    val to_string : t -> string
    val to_nexp : t -> Exp.nexp
    val from_nexp : globals:Variable.Set.t -> Exp.nexp -> t

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
    val factors : t -> (Atom.t * int) list
    val of_factors : ?coeff:int -> (Atom.t * int) list -> t

    val fold : (Atom.t -> int -> 'a -> 'a) -> 'a -> t -> 'a
    val filter : (Atom.t -> int -> bool) -> t -> t

    val has_induction : t -> bool
    val has_parameter : t -> bool
    val is_const : t -> bool
    val nfactors: t -> int
    val to_nexp : t -> Exp.nexp
  end
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
  module Atom = struct
    type t = {
      value: Exp.nexp;
      thread_global: bool;
    }

    let induction s = {
      value = Exp.Var (Variable.from_name s);
      thread_global = false;
    }
    let parameter s = {
      value = Exp.Var (Variable.from_name s);
      thread_global = true;
    }

    let compare x y = match Exp.n_compare x.value y.value with
      | 0 -> compare x.thread_global y.thread_global
      | n -> n
      
    let to_string = function
      | { value; thread_global = true } -> Exp.n_to_string value ^ " (global)"
      | { value; thread_global = false} -> Exp.n_to_string value

    let from_nexp ~globals (value: Exp.nexp) : t =
      let thread_global = 
        let free = Exp.n_free_names value Variable.Set.empty in
        Variable.Set.diff free globals |> Variable.Set.is_empty
      in
      {
        value; thread_global
      }


    let to_nexp : t -> Exp.nexp = function
      | { value; _ } -> value

    let is_induction v = not v.thread_global
    let is_parameter v = v.thread_global
  end
  module VarMap = Map.Make(Atom)
  module TermInner = struct
    type t = int VarMap.t

    let compare = VarMap.compare Int.compare
    (* let to_string t =
      if VarMap.is_empty t
      then "TermInner.of_factors []"
      else
        VarMap.bindings t
        |> List.map (fun (a, i) -> (Atom.to_string a, i))
        |> List.map (function
        | k, v -> Printf.sprintf "%s, %d" k v)
        |> String.concat "; "
        |> Printf.sprintf "TermInner.of_factors [%s]" *)

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
    (* let to_string (c, t) = match c with
      | 0 -> "0"
      | 1 -> Printf.sprintf "(%s)" (TermInner.to_string t)
      | _ -> Printf.sprintf "(%d * %s)" c (TermInner.to_string t) *)
    let to_string (c, t) =
      let ctor = match c with
        | 1 -> "Term.of_factors"
        | n -> Printf.sprintf "Term.of_factors ~coeff:%d" n
      in
      if VarMap.is_empty t
      then Printf.sprintf "%s []" ctor
      else
        VarMap.bindings t
        |> List.map (fun (a, i) -> (Atom.to_string a, i))
        |> List.map (function
        | k, v -> Printf.sprintf "%s, %d" k v)
        |> String.concat "; "
        |> Printf.sprintf "%s [%s]" ctor
    let parameter s = (1, VarMap.singleton (Atom.parameter s) 1)
    let induction s = (1, VarMap.singleton (Atom.induction s) 1)
    (* let of_int i = (i, VarMap.empty) *)
    (* let one = of_int 1 *)

    let ( * ) (c1, t1) (c2, t2) = (c1 * c2, TermInner.( * ) t1 t2)
    let coeff (c, _) = c
    let fold f acc (_, t) = TermInner.fold f acc t
    let filter f (c, t) = c, VarMap.filter f t
    let factors (_, t): (Atom.t * int) list = TermInner.to_list t
    let of_factors ?(coeff = 1) factors = coeff, VarMap.of_list factors

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
    | 0 -> let d = TermInner.(f1 * (VarMap.map (~-) f2))
      in if (VarMap.exists (fun _ e -> e < 0) d)
        then None
        else Some (c1 / c2, d)
    | _ -> None
  end

  module TermMap = Map.Make(TermInner)
  type t = int TermMap.t

  let compare = TermMap.compare Int.compare

  let of_int i = match i with
    | 0 -> TermMap.empty
    | _ -> TermMap.singleton (VarMap.empty) i

  let of_atom (v : Atom.t): t = TermMap.singleton (VarMap.singleton v 1) 1

  let to_list t: Term.t list = t
    |> TermMap.bindings
    |> List.map (fun (t, c) -> (c, t))
  let of_list (t : Term.t list): t = t
    |>  List.map (fun (c, t) -> (t, c))
    |> TermMap.of_list
  let fold f acc t = TermMap.fold (fun t c acc -> f (c, t) acc) t acc

  let parameter v = TermMap.singleton (VarMap.singleton (Atom.parameter v) 1) 1
  let induction v = TermMap.singleton (VarMap.singleton (Atom.induction v) 1) 1
  let zero = TermMap.empty

  let to_string (t : t) : string =
    if TermMap.is_empty t then "Expr.of_list []"
    else t
      |> to_list
      |> List.map (function
        | t -> Printf.sprintf "  %s;\n" (Term.to_string t))
      |> String.concat ""
      |> Printf.sprintf "Expr.of_list [\n%s]"
      (* |> List.map (function
        | k, v -> Printf.sprintf "(%s, %d)"
          (TermInner.to_string v)
          v
      )
      |> String.concat ";\n"
      |> Printf.sprintf "TermMap.of_list [\n  %s]" *)

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
    | Some v, None -> Some v
    | None, Some v -> Some (-v)
    | None, None -> None) t1 t2
  let ( * ) (t1: t) (t2: t): t =
    TermMap.fold (fun k1 v1 acc ->
      TermMap.fold (fun k2 v2 acc ->
        let product = TermInner.(k1 * k2)
        in let coeff = v1 * v2
        in TermMap.singleton product coeff + acc) t2 acc) t1 zero

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

(* module StringSet = Set.Make(String) *)
module Term = Expr.Term
module Atom = Expr.Atom

type t = {
  indices: Exp.nexp list;
  dims: Exp.nexp list;
  conditions: Exp.bexp list;
}



let to_string: t -> string = function
| { indices; dims; conditions } ->
  Printf.sprintf "{ indices = %s; dims = %s; conditions = %s }"
    (list_to_string Exp.n_to_string indices)
    (list_to_string Exp.n_to_string dims)
    (list_to_string Exp.b_to_string conditions)

module Make(L:Logger.Logger) : sig
  val rewrite_kernel : Aligned.Kernel.t -> Aligned.Kernel.t
end = struct

  let size_params_all (ts : Expr.t list) : Term.t list = ts
      |> List.map (fun t -> (*print_endline (Expr.to_string t);*) t
        |> Expr.to_list
        |> List.filter (fun t ->
          Term.has_induction t && Term.has_parameter t
        )
        |> List.map (Term.filter (fun v _ -> Atom.is_parameter v))
      )
      |> List.flatten
      (* TODO?: handle some kind of dimension selection. i need a deduplication then a size sort *)
      (* deduplicate *)
      |> List.sort_uniq Term.compare
      |> List.sort (fun a b -> -compare (Term.nfactors a) (Term.nfactors b))

  (* Divides out size params *)
  let rec dims: Term.t list -> Term.t list option = function
    | [] -> (*print_endline "no dim";*) Some []
    | [x] -> Some [x]
    | x :: y :: ys ->
      let ( let* ) = Option.bind in
      let* dim = Term.try_div x y in
      let* r = dims (y :: ys) in
      Some (dim :: r)
    (* don't consider numbers params? or do *)

  let accesses (dims : Term.t list) (t : Expr.t): Expr.t list =
    let rec loop rdims t = match rdims with
    | [] -> [t]
    | d :: ds -> let q, r = Expr.div_mod t d in
      r :: loop ds q
    in
    t |> loop (List.rev dims)
    |> List.rev
    (*turn this into a fold later*)

  let from_exp (ds : Term.t list) (expr : Expr.t) : t option =
    L.info (fun () -> "Dims = \n" ^ list_to_string Term.to_string ds);
    L.info (fun () -> "Expr = \n" ^ Expr.to_string expr);
    let is = accesses ds expr in
    L.info (fun () -> "Indices = \n" ^ list_to_string Expr.to_string is);
    let conditions ds is = match ds, is with
    | ds, _ :: is -> List.map2 (fun d i ->
        let open Exp in
        b_and (n_le (Num 0) (Expr.to_nexp i)) (n_lt (Expr.to_nexp i) (Expr.Term.to_nexp d))
      ) ds is
    | _ -> failwith "unreachable?"
    in
    L.info (fun () ->
      let conds = conditions ds is |> list_to_string Exp.b_to_string in
      if conds <> "true" then "Unchecked conditions = \n" ^ conds
      else "Conditions = true");
    Some {
      indices = List.map Expr.to_nexp is;
      dims = List.map Expr.Term.to_nexp ds;
      conditions = conditions ds is
    }
  

  (* let list_bind (x : 'a list) (f : 'a -> 'b list) : 'b list =
    x |> List.map f |> List.flatten

  let loption_bind (x : 'a list option) (f : 'a -> 'b list option) : 'b list option =
    match x with  *)

  (* let rewrite_access_with (dims : Term.t list) (acc : Access.t) : Acccess.t = *)
    

  (* Throwing out conditions for now. eventually will use t as a sort of rewrite template *)

  (* TODO: this should simply not rewrite if there is a failure *)

  let get_accesses (unsync : Unsynced.t) : Exp.nexp list list Variable.Map.t =
    let open Unsynced in
    let rec get_accesses_unsync = function
      | Skip | Assert _ -> Fun.id
      | Access {array; index; _} -> 
        L.info (fun () -> Printf.sprintf "array %s has %d dimensions" array.name (List.length index));
        Variable.Map.add_to_list array index
      | Cond (_, u) -> get_accesses_unsync u
      | Loop (_, u) -> get_accesses_unsync u
      | Seq (u, v) -> Fun.compose (get_accesses_unsync u) (get_accesses_unsync v)
    in get_accesses_unsync unsync Variable.Map.empty

  let rewrite_unsync ~(globals : Variable.Set.t) (unsync : Unsynced.t) : Unsynced.t =
    let (let*) = Option.bind in
    let open Unsynced in
    (* Three sub-phases of [rewrite_unsync], measured separately so the
       JSON phase_times shows where delin time actually goes. They sum
       to ~all of [rewrite_unsync] (modulo glue), which itself sums
       across Sync blocks into the top-level "delin" boundary. *)
    let accs =
      Phase_timer.measure "delin/get-accesses" (fun () -> get_accesses unsync)
    in
    let dims = Phase_timer.measure "delin/dims" (fun () ->
      accs
      |> Variable.Map.filter_map (fun _ accesses ->
        (* Skip arrays that already have multi-index accesses — there is
           nothing to delinearize for those. Returning [None] drops the
           entry from the dims map, so the rewriter's [find_opt] misses
           and leaves the access unchanged. *)
        let* singletons = accesses
          |> List.fold_left (fun acc -> function
            | [a] -> Option.map (fun xs -> Expr.from_nexp ~globals a :: xs) acc
            | _ -> None
          ) (Some [])
        in
        singletons |> size_params_all |> dims))
    in
    let rec rewrite_unsync : Unsynced.t -> Unsynced.t = function
      | Access ({ array; index = [a]; _ } as acc) ->
        (match (
          let a = Expr.from_nexp ~globals a in
          let* dim = Variable.Map.find_opt array dims in
          let* rewritten =
            Phase_timer.measure "delin/from-exp" (fun () -> from_exp dim a)
          in
          Some rewritten.indices
        ) with
        | Some indices -> Access { acc with index = indices }
        | None -> Access acc)
      | Access _ as code -> code
      | Cond (p, b) -> Cond (p, rewrite_unsync b)
      | Loop (r, b) -> Loop (r, rewrite_unsync b)
      | Seq (a, b) -> Seq (rewrite_unsync a, rewrite_unsync b)
      | code -> code
    in
    Phase_timer.measure "delin/rewrite" (fun () -> rewrite_unsync unsync)

  let rec rewrite_aligned ~(globals : Variable.Set.t): Aligned.Code.t -> Aligned.Code.t =
    let open Aligned.Code in
    (* TODO rewrite: get dimensionality *)
    function
    | Sync c -> Sync (rewrite_unsync ~globals c)
    | Loop ({ range = { var = x; _ }; body; _ } as loop) ->
      Loop { loop with body = rewrite_aligned ~globals:(Variable.Set.add x globals) body }
    | Seq (a, b) -> Seq (rewrite_aligned ~globals a, rewrite_aligned ~globals b)

  let rewrite_kernel (kernel : Aligned.Kernel.t) : Aligned.Kernel.t =
    let globals = Params.to_set kernel.global_variables in
    { kernel with code = rewrite_aligned ~globals kernel.code }
  (* currently this thinks blockIdx is global *)
end

module Silent = Make(Logger.Silent)
module Warnings = Make(Logger.Warnings)
module Default = Make(Logger.Default)

(* add function mapping aligned.code to proto *)
