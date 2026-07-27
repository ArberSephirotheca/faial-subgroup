open Stage0
open Exp

module type SUBST = sig
  type t

  (* Given a substitution map and a variable perform the substitution if possible. *)
  val find : t -> Variable.t -> nexp option

  (* Removes a variable from the current substitution map; return None if the map became empty *)
  val remove : t -> Variable.t -> t option

  (* Renders as a string *)
  val to_string : t -> string

  (* Tests if the map is empty *)
  val is_empty : t -> bool
end

module Make (S : SUBST) = struct
  let shadows (s : S.t) (x : Variable.t) : bool =
    match S.find s x with Some _ -> true | None -> false

  let add (s : S.t) (x : Variable.t) (cont : S.t option -> 'a) : 'a =
    if shadows s x then
      match S.remove s x with Some s -> cont (Some s) | None -> cont None
    else cont (Some s)

  let rec n_subst (s : S.t) (n : nexp) : nexp =
    match n with
    | Var x -> ( match S.find s x with Some v -> v | None -> n)
    | CastInt b -> CastInt (b_subst s b)
    | Num _ -> n
    | Unary (o, e) -> Unary (o, n_subst s e)
    | Binary (o, n1, n2) -> Binary (o, n_subst s n1, n_subst s n2)
    | NIf (b, n1, n2) -> NIf (b_subst s b, n_subst s n1, n_subst s n2)
    | NCall (x, args) -> NCall (x, List.map (n_subst s) args)
    | ReadResult r -> ReadResult { r with args = List.map (n_subst s) r.args }
    | Convert c -> convert c.ty (n_subst s c.arg)

  and b_subst (s : S.t) (b : bexp) : bexp =
    match b with
    | CastBool e -> CastBool (n_subst s e)
    | Pred (n, vs) -> Pred (n, List.map (n_subst s) vs)
    | Bool _ -> b
    | NRel (o, n1, n2) -> NRel (o, n_subst s n1, n_subst s n2)
    | BRel (o, b1, b2) -> BRel (o, b_subst s b1, b_subst s b2)
    | BNot b -> BNot (b_subst s b)
    | Distinct exprs -> Distinct (List.map (n_subst s) exprs)
    | AtomicResult { target; array; index; operation } ->
        (* Substitute through index and operand expressions; leave
           [target] and [array] as-is (binding-name references, not
           free expressions). *)
        AtomicResult
          {
            target;
            array;
            index = List.map (n_subst s) index;
            operation = Atomic.Operation.map (n_subst s) operation;
          }
    | IsThreadUnif e -> IsThreadUnif (n_subst s e)

  let a_subst (s : S.t) (a : Access.t) : Access.t =
    { a with index = List.map (n_subst s) a.index }

  let r_subst (s : S.t) : Range.t -> Range.t = Range.map (n_subst s)
end

module SubstPair = struct
  type t = Variable.t * nexp

  let make (x, v) : t = (x, v)
  let find (x, v) y = if Variable.equal x y then Some v else None
  let remove (x, v) y = if Variable.equal x y then None else Some (x, v)
  let to_string (x, v) = "[" ^ Variable.name x ^ "=" ^ n_to_string v ^ "]"
  let is_empty (_, _) = false
end

module ReplacePair = Make (SubstPair)

(** Substitute using an association list. *)

module SubstAssoc = struct
  type t = (string, nexp) Hashtbl.t

  let make kvs = Common.hashtbl_from_list kvs
  let find ht k = Hashtbl.find_opt ht (Variable.name k)

  let put_mut (ht : t) (k : Variable.t) (n : nexp) : unit =
    Hashtbl.replace ht (Variable.name k) n

  let is_empty (ht : t) : bool = Hashtbl.length ht = 0

  let put (ht : t) (k : Variable.t) (n : nexp) : t =
    let ht = Hashtbl.copy ht in
    put_mut ht k n;
    ht

  let del ht k =
    let ht = Hashtbl.copy ht in
    Hashtbl.remove ht (Variable.name k);
    ht

  let remove ht k =
    let ht = del ht k in
    if Hashtbl.length ht = 0 then None else Some ht

  let to_string ht =
    Common.hashtbl_elements ht
    |> List.map (fun (k, v) -> k ^ "=" ^ n_to_string v)
    |> String.concat ", "
    |> fun x -> "[" ^ x ^ "]"
end

module Vars = struct
  type t = nexp Variable.Map.t

  let make = Variable.Map.of_list

  let find (m : t) (k : Variable.t) : nexp option =
    Variable.Map.find_opt k m

  let is_empty = Variable.Map.is_empty

  let put (m : t) (k : Variable.t) (n : nexp) : t =
    Variable.Map.add k n m

  let remove (m : t) (k : Variable.t) : t option =
    let m = Variable.Map.remove k m in
    if is_empty m then None else Some m

  let to_string = Variable.map_to_string n_to_string
end

module ReplaceAssoc = Make (SubstAssoc)
module ReplaceVars = Make (Vars)
