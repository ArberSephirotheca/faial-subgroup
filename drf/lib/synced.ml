open Stage0
open Protocols
open Subst
open Exp

type t = Sync of Unsynced.t | SeqLoop of (Unsynced.t * loop) | Seq of t * t
and loop = { range : Range.t; body : t * Unsynced.t }

let skip : t = Sync Skip

let rec to_s : t -> Indent.t list = function
  | Sync e -> Unsynced.to_s e @ [ Line "sync;" ]
  | SeqLoop (c1, { range = r; body = p, c2 }) ->
      Unsynced.to_s c1
      @ [
          Line ("foreach* (" ^ Range.to_string r ^ ") {");
          Block (to_s p @ Unsynced.to_s c2);
          Line "}";
        ]
  | Seq (p1, p2) -> to_s p1 @ to_s p2

module Make (S : SUBST) = struct
  module M = Subst.Make (S)
  module U = Unsynced.Make (S)

  let rec subst (s : S.t) : t -> t = function
    | Sync c -> Sync (U.subst s c)
    | SeqLoop (c1, { range = r; body = p, c2 }) ->
        let p, c2 =
          M.add s r.var (function
            | Some s -> (subst s p, U.subst s c2)
            | None -> (p, c2))
        in
        SeqLoop (U.subst s c1, { range = M.r_subst s r; body = (p, c2) })
    | Seq (p, q) -> Seq (subst s p, subst s q)
end

module S1 = Make (SubstPair)

let subst = S1.subst

let inline_cond (b : bexp) (w : t) : t =
  let b = Constfold.b_opt b in
  let rec inline : t -> t = function
    | Sync c -> Sync (Seq (Assert b, c))
    | SeqLoop (c1, { range; body = w, c2 }) ->
        SeqLoop
          (Seq (Assert b, c1), { range; body = (inline w, Seq (Assert b, c2)) })
    | Seq (p, q) -> Seq (inline p, inline q)
  in
  match b with Bool true -> w | Bool false -> skip | _ -> inline w

(* Apply a function to the first unsync *)
let rec map_first (f : Unsynced.t -> Unsynced.t) : t -> t = function
  | Sync c -> Sync (f c)
  | SeqLoop (c1, l) -> SeqLoop (f c1, l)
  | Seq (p, q) -> Seq (map_first f p, q)

let add (u : Unsynced.t) : t -> t = map_first (fun u2 -> Seq (u, u2))

let rec free_names (i : t) (fns : Variable.Set.t) : Variable.Set.t =
  match i with
  | Sync c -> Unsynced.free_names c fns
  | SeqLoop (c1, { range = r; body = p, c2 }) ->
      Unsynced.free_names c2 fns |> free_names p
      |> Variable.Set.remove (Range.var r)
      |> Range.free_names r |> Unsynced.free_names c1
  | Seq (p, q) -> free_names p fns |> free_names q
