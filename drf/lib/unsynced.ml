open Stage0
open Protocols
open Exp
open Subst

type t =
  | Skip
  | Assert of bexp
  | Access of Access.t
  | Cond of bexp * t
  | Loop of Range.t * t
  | Seq of t * t

module Opt = struct
  let cond (b : bexp) (u : t) : t = if u = Skip then Skip else Cond (b, u)

  let seq (u1 : t) (u2 : t) : t =
    if u1 = Skip then u2 else if u2 = Skip then u1 else Seq (u1, u2)

  let loop (r : Range.t) (u : t) : t = if u = Skip then Skip else Loop (r, u)
end

let rec to_s : t -> Indent.t list = function
  | Skip -> [ Line "skip;" ]
  | Assert b -> [ Line ("assert " ^ Exp.b_to_string b ^ ";") ]
  | Access e -> [ Line (Access.to_string e) ]
  | Cond (b, p1) ->
      [ Line ("if (" ^ Exp.b_to_string b ^ ") {"); Block (to_s p1); Line "}" ]
  | Loop (r, p) ->
      [
        Line ("foreach (" ^ Range.to_string r ^ ") {"); Block (to_s p); Line "}";
      ]
  | Seq (p, q) -> to_s p @ to_s q

let to_string (p : t) : string = to_s p |> Indent.to_string

let cond (b : bexp) (p : t) : t =
  match b with Bool true -> p | Bool false -> Skip | _ -> Cond (b, p)

module Make (S : SUBST) = struct
  module M = Subst.Make (S)

  let rec subst (s : S.t) : t -> t = function
    | Skip -> Skip
    | Assert b -> Assert (M.b_subst s b)
    | Access e -> Access (M.a_subst s e)
    | Cond (b, p) -> Cond (M.b_subst s b, subst s p)
    | Loop (r, p) ->
        let p = M.add s r.var (function Some s -> subst s p | None -> p) in
        Loop (M.r_subst s r, p)
    | Seq (p, q) -> Seq (subst s p, subst s q)
end

module S1 = Make (SubstPair)

let subst = S1.subst

let seq (u1 : t) (u2 : t) =
  (* The order of appending doesn't matter for unsync insts *)
  Seq (u1, u2)

let rec write_locations (p : t) (known : Variable.Set.t) =
  match p with
  | Skip | Assert _ -> known
  | Seq (p, q) -> write_locations p known |> write_locations q
  | Access a ->
      if not (Access.is_read a) then Variable.Set.add a.array known else known
  | Loop (_, p) | Cond (_, p) -> write_locations p known

let rec free_names (p : t) (fns : Variable.Set.t) : Variable.Set.t =
  match p with
  | Skip -> fns
  | Assert b -> Exp.b_free_names b fns
  | Access e -> Access.free_names e fns
  | Loop (r, l) ->
      free_names l fns
      |> Variable.Set.remove (Range.var r)
      |> Range.free_names r
  | Cond (b, l) -> Exp.b_free_names b fns |> free_names l
  | Seq (p, q) -> free_names p fns |> free_names q

let rec unsafe_binders (i : t) (vars : Variable.Set.t) : Variable.Set.t =
  match i with
  | Skip | Assert _ | Access _ -> vars
  | Cond (_, p) -> unsafe_binders p vars
  | Loop (r, p) ->
      let vars =
        let r_vars = Range.free_names r Variable.Set.empty in
        if Variable.Set.is_empty (Variable.Set.inter r_vars vars) then vars
        else Variable.Set.add r.var vars
      in
      unsafe_binders p vars
  | Seq (p, q) -> unsafe_binders p vars |> unsafe_binders q

let rec binders (i : t) (vars : Variable.Set.t) : Variable.Set.t =
  match i with
  | Skip | Assert _ | Access _ -> vars
  | Cond (_, p) -> binders p vars
  | Loop (r, p) -> binders p (Variable.Set.add r.var vars)
  | Seq (p, q) -> binders p vars |> binders q

(* Pre-flatacc pass: every code-level [Loop] whose range has a
   literal stride [k > 1] is rewritten to step 1 over a fresh
   quotient variable, and the original loop variable is
   substituted by [lb + k * q] throughout the body. After this
   pass no [Loop] in [t] carries a literal-step [Plus] (k > 1)
   range, so [Range.to_cond]'s [Plus n] arm only sees
   genuinely-symbolic strides. *)
let rec normalize_loops : t -> t = function
  | Skip -> Skip
  | Assert b -> Assert b
  | Access a -> Access a
  | Cond (b, p) -> Cond (b, normalize_loops p)
  | Seq (p, q) -> Seq (normalize_loops p, normalize_loops q)
  | Loop (r, body) ->
      let body = normalize_loops body in
      (match Range.normalize r with
      | None -> Loop (r, body)
      | Some (r', sp) -> Loop (r', subst sp body))

let inline_asserts : t -> t =
  let rec has_asserts : t -> bool = function
    | Skip | Access _ -> false
    | Assert _ -> true
    | Cond (_, p) | Loop (_, p) -> has_asserts p
    | Seq (p, q) -> has_asserts p || has_asserts q
  in
  let rec conditions (p : t) (b : bexp) : bexp =
    match p with
    | Skip | Access _ | Cond (_, _) | Loop (_, _) -> b
    | Assert b' -> b_and b b'
    | Seq (p, q) -> conditions p b |> conditions q
  in
  let rec remove_asserts : t -> t = function
    | Skip -> Skip
    | Assert _ -> Skip
    | Access a -> Access a
    | Cond (b, p) -> cond (conditions p b) (remove_asserts p)
    | Loop (r, p) -> Loop (r, cond (conditions p (Bool true)) (remove_asserts p))
    | Seq (p, q) -> Seq (remove_asserts p, remove_asserts q)
  in
  fun p ->
    if has_asserts p then cond (conditions p (Bool true)) (remove_asserts p)
    else p

(* ------ filter by location ----- *)

module Perhaps = struct
  type 'a t = Has of 'a | Might of 'a

  let to_option = function Has p -> Some p | Might _ -> None

  let map (f : 'a -> 'a) : 'a t -> 'a t = function
    | Has p -> Has (f p)
    | Might p -> Might (f p)

  let merge (f : 'a -> 'a -> 'a) (p1 : 'a t) (p2 : 'a t) : 'a t =
    match (p1, p2) with
    | Has a1, Might a2 | Might a1, Has a2 | Has a1, Has a2 -> Has (f a1 a2)
    | Might a1, Might a2 -> Might (f a1 a2)
end

(* Given an input phase, returns a phase with only accesses x.
  Changes the accesses to not contain the location info. *)
let filter_by_location (x : Variable.t) : t -> t option =
  (* Filter programs *)
  let rec filter : t -> t Perhaps.t = function
    | Skip -> Might Skip
    | Assert b -> Might (Assert b)
    | Access { array = y; _ } as i ->
        if Variable.equal x y then Has i else Might Skip
    | Cond (b, p) -> filter p |> Perhaps.map (fun p -> Cond (b, p))
    | Loop (r, p) -> filter p |> Perhaps.map (fun p -> Loop (r, p))
    | Seq (p, q) -> Perhaps.merge (fun p q -> Seq (p, q)) (filter p) (filter q)
  in
  fun i -> filter i |> Perhaps.to_option
