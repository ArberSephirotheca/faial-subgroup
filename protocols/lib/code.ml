open Stage0

let ( @ ) = Common.append_tr

open Exp

(* The source instruction uses the base defined above *)
type t =
  | Access of Access.t
  | Sync of Sync.t
  | If of bexp * t * t
  | Loop of { range : Range.t; body : t }
  | Seq of t * t
  | Skip
  | Decl of { var : Variable.t; ty : C_type.t; body : t }

let rec filter (f : t -> bool) (p : t) : t =
  if not (f p) then Skip
  else
    match p with
    | Sync _ | Skip | Access _ -> p
    | Seq (p, q) -> Seq (filter f p, filter f q)
    | If (b, p, q) -> If (b, filter f p, filter f q)
    | Decl d -> Decl { d with body = filter f d.body }
    | Loop { range; body = p } -> Loop { range; body = filter f p }

let rec exists (f : t -> bool) (i : t) : bool =
  f i
  ||
  match i with
  | Access _ | Sync _ | Skip -> false
  | Loop { body = p; _ } | Decl { body = p; _ } -> exists f p
  | If (_, p, q) | Seq (p, q) -> exists f p || exists f q

(** Replace variables by constants. *)

module Make (S : Subst.SUBST) = struct
  module M = Subst.Make (S)

  let rec subst (s : S.t) (i : t) : t =
    match i with
    | Skip -> Skip
    | Seq (p, q) -> Seq (subst s p, subst s q)
    | Access a -> Access (M.a_subst s a)
    | Sync l -> Sync l
    | If (b, p, q) -> If (M.b_subst s b, subst s p, subst s q)
    | Decl d ->
        M.add s d.var (function
          | Some s -> Decl { d with body = subst s d.body }
          | None -> Decl d)
    | Loop { range; body } ->
        let range = M.r_subst s range in
        M.add s range.var (function
          | Some s -> Loop { range; body = subst s body }
          | None -> Loop { range; body })
end

let apply_arch (arrays : Variable.Set.t) : Architecture.t -> t -> t = function
  | Grid ->
      filter (function
        | Sync _ -> false
        | Access { array; _ } -> Variable.Set.mem array arrays
        | _ -> true)
  | Block -> fun s -> s

module PSubstAssoc = Make (Subst.SubstAssoc)
module PSubstPair = Make (Subst.SubstPair)

let seq (p : t) (q : t) : t =
  match (p, q) with Skip, p | p, Skip -> p | _, _ -> Seq (p, q)

let if_ (b : bexp) (p : t) (q : t) : t =
  match (b, p, q) with
  | Bool b, _, _ -> if b then p else q
  | _, Skip, Skip -> Skip
  | _, Skip, _ -> If (b_not b, q, Skip)
  | _, _, _ -> If (b, p, q)

let loop (r : Range.t) (p : t) : t =
  if p = Skip then Skip
  else
    let is_empty =
      r |> Range.is_empty |> Exp.b_eval_res |> Result.value ~default:false
    in
    if is_empty then Skip else Loop { range = r; body = p }

let decl ?(ty = C_type.int) (var : Variable.t) : t -> t = function
  | Skip -> Skip
  | body -> Decl { var; ty; body }

let rec opt : t -> t = function
  | Skip -> Skip
  | Decl d -> Decl { d with body = opt d.body }
  | Seq (p, q) -> seq (opt p) (opt q)
  | Access a -> Access (Constfold.a_opt a)
  | Sync l -> Sync l
  | If (b, p, q) -> if_ (Constfold.b_opt b) (opt p) (opt q)
  | Loop { range = r; body = p } -> loop (Constfold.r_opt r) (opt p)

let subst_block_dim (block_dim : Dim3.t) (p : t) : t =
  let subst x n p = PSubstPair.subst (Variable.from_name x, Num n) p in
  p
  |> subst "blockDim.x" block_dim.x
  |> subst "blockDim.y" block_dim.y
  |> subst "blockDim.z" block_dim.z

let subst_grid_dim (grid_dim : Dim3.t) (p : t) : t =
  let subst x n p = PSubstPair.subst (Variable.from_name x, Num n) p in
  p
  |> subst "gridDim.x" grid_dim.x
  |> subst "gridDim.y" grid_dim.y
  |> subst "gridDim.z" grid_dim.z

let vars_distinct : t -> Variable.Set.t -> t =
  let rec uniq (i : t) (xs : Variable.Set.t) : t * Variable.Set.t =
    match i with
    | Skip | Access _ | Sync _ -> (i, xs)
    | If (b, p, q) ->
        let p, xs = uniq p xs in
        let q, xs = uniq q xs in
        (If (b, p, q), xs)
    | Decl d ->
        let x = d.var in
        let p = d.body in
        if Variable.Set.mem x xs then
          let new_x : Variable.t = Variable.fresh xs x in
          let new_xs = Variable.Set.add new_x xs in
          let s = Subst.SubstPair.make (x, Var new_x) in
          let new_p = PSubstPair.subst s p in
          let p, new_xs = uniq new_p new_xs in
          (Decl { var = new_x; body = p; ty = d.ty }, new_xs)
        else
          let p, new_xs = uniq p (Variable.Set.add x xs) in
          (Decl { var = x; body = p; ty = d.ty }, new_xs)
    | Loop { range = r; body = p } ->
        let x = r.var in
        if Variable.Set.mem x xs then
          let new_x : Variable.t = Variable.fresh xs x in
          let new_xs = Variable.Set.add new_x xs in
          let s = Subst.SubstPair.make (x, Var new_x) in
          let new_p = PSubstPair.subst s p in
          let p, new_xs = uniq new_p new_xs in
          (Loop { range = { r with var = new_x }; body = p }, new_xs)
        else
          let p, new_xs = uniq p (Variable.Set.add x xs) in
          (Loop { range = r; body = p }, new_xs)
    | Seq (i, p) ->
        let i, xs = uniq i xs in
        let p, xs = uniq p xs in
        (Seq (i, p), xs)
  in
  fun p known -> uniq p known |> fst

let rec free_names (i : t) (fns : Variable.Set.t) : Variable.Set.t =
  match i with
  | Skip -> fns
  | Sync s ->
      let fns =
        List.fold_left (fun fns e -> n_free_names e fns) fns s.index
      in
      (match s.count with Some c -> n_free_names c fns | None -> fns)
  | Access a -> Access.free_names a fns
  | If (b, p, q) -> b_free_names b fns |> free_names p |> free_names q
  | Decl { var = x; body = p; _ } -> free_names p fns |> Variable.Set.remove x
  | Loop { range = r; body = p } ->
      free_names p fns |> Variable.Set.remove r.var |> Range.free_names r
  | Seq (p, q) -> free_names p fns |> free_names q

(* Only retain CI-DI accesses *)
let rec to_ci_di (approx : Variable.Set.t) : t -> t = function
  | If (b, p, q) ->
      if Exp.b_intersects approx b then Skip
      else If (b, to_ci_di approx p, to_ci_di approx q)
  | Loop { range = r; body = p } ->
      if Range.intersects approx r then Skip
      else
        (* the loop variable is CIDI, hence remove any existing CIDI *)
        let approx = Variable.Set.remove (Range.var r) approx in
        Loop { range = r; body = to_ci_di approx p }
  | Access a -> if Access.index_intersects approx a then Skip else Access a
  | Decl { var = x; body = p; _ } ->
      (* In this scope x is approximate *)
      to_ci_di (Variable.Set.add x approx) p
  | Seq (p, q) -> Seq (to_ci_di approx p, to_ci_di approx q)
  | Skip -> Skip
  | Sync a -> Sync a

let rec used_arrays (i : t) (fns : Variable.Set.t) : Variable.Set.t =
  match i with
  | Skip | Sync _ -> fns
  | Access { array = x; _ } -> Variable.Set.add x fns
  | Decl { body = p; _ } | Loop { body = p; _ } -> used_arrays p fns
  | If (_, p, q) | Seq (p, q) -> used_arrays p fns |> used_arrays q

let rec to_s : t -> Indent.t list = function
  | Skip -> [ Line "skip;" ]
  | Sync s -> [ Line (Sync.to_string s ^ ";") ]
  | Access a -> [ Line (Access.to_string a) ]
  | If (b, p, Skip) ->
      [ Line ("if (" ^ b_to_string b ^ ") {"); Block (to_s p); Line "}" ]
  | If (b, p, q) ->
      [
        Line ("if (" ^ b_to_string b ^ ") {");
        Block (to_s p);
        Line "} else {";
        Block (to_s q);
        Line "}";
      ]
  | Decl d ->
      let var = Variable.name d.var in
      let ty = C_type.to_string d.ty in
      Line (ty ^ " " ^ var ^ ";") :: to_s d.body
  | Loop { range = r; body = p } ->
      [
        Line ("foreach (" ^ Range.to_string r ^ ") {"); Block (to_s p); Line "}";
      ]
  | Seq (p, q) -> to_s p @ to_s q

let to_string (p : t) : string = Indent.to_string (to_s p)
let print (p : t) : unit = Indent.print (to_s p)
