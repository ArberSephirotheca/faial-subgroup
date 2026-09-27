open Stage0

let ( @ ) = Common.append_tr

open Exp

(* The source instruction uses the base defined above *)
type t =
  | Access of Access.t
  | Sync of Sync.t
  | If of bexp * t * t
  | Loop of { cond_range : Cond_range.t; body : t }
  | Seq of t * t
  | Skip
  | Decl of { var : Variable.t; ty : Ty.t; cond : bexp; body : t }

let rec filter (f : t -> bool) (p : t) : t =
  if not (f p) then Skip
  else
    match p with
    | Sync _ | Skip | Access _ -> p
    | Seq (p, q) -> Seq (filter f p, filter f q)
    | If (b, p, q) -> If (b, filter f p, filter f q)
    | Decl d -> Decl { d with body = filter f d.body }
    | Loop { cond_range; body = p } -> Loop { cond_range; body = filter f p }

let rec exists (f : t -> bool) (i : t) : bool =
  f i
  ||
  match i with
  | Access _ | Sync _ | Skip -> false
  | Loop { body = p; _ } | Decl { body = p; _ } -> exists f p
  | If (_, p, q) | Seq (p, q) -> exists f p || exists f q

let reset_variable_kind (kernel_parameters : Variable.Set.t) : t -> t =
  let open State.Syntax in
  let reset_n ~loop_variables =
    reset_variable_kind_n ~kernel_parameters ~loop_variables
  in
  let fresh_id : (Access.Id.t, Access.Id.t) State.t =
    State.update_return (fun id -> (Access.Id.next id, id))
  in
  let rec loop (loop_variables : Variable.Set.t) :
      t -> (Access.Id.t, t) State.t = function
    | Skip -> return Skip
    | Access a ->
        let* id = fresh_id in
        return
          (Access
             {
               a with
               id;
               array = Variable.set_kind Array a.array;
               index = List.map (reset_n ~loop_variables) a.index;
             })
    | Sync s -> return (Sync (Sync.map (reset_n ~loop_variables) s))
    | If (b, p, q) ->
        let* p = loop loop_variables p in
        let* q = loop loop_variables q in
        return (If (Exp.b_map (reset_n ~loop_variables) b, p, q))
    | Seq (p, q) ->
        let* p = loop loop_variables p in
        let* q = loop loop_variables q in
        return (Seq (p, q))
    | Loop { cond_range; body } ->
        let loop_variables =
          Variable.Set.add (Cond_range.var cond_range) loop_variables
        in
        let cond_range =
          let cr = Cond_range.map (reset_n ~loop_variables) cond_range in
          {
            cr with
            range =
              {
                cr.range with
                var = Variable.set_kind LoopVariable (Cond_range.var cr);
              };
          }
        in
        let* body = loop loop_variables body in
        return (Loop { cond_range; body })
    | Decl d ->
        let* body = loop loop_variables d.body in
        return
          (Decl
             {
               d with
               var = Variable.set_kind Decl d.var;
               cond = Exp.b_map (reset_n ~loop_variables) d.cond;
               body;
             })
  in
  fun code -> State.run_result (loop Variable.Set.empty code) Access.Id.first


(** Replace variables by constants. *)

module Make (S : Subst.SUBST) = struct
  module M = Subst.Make (S)
  module CR = Cond_range.Make (S)

  let rec subst (s : S.t) (i : t) : t =
    match i with
    | Skip -> Skip
    | Seq (p, q) -> Seq (subst s p, subst s q)
    | Access a -> Access (M.a_subst s a)
    | Sync l -> Sync l
    | If (b, p, q) -> If (M.b_subst s b, subst s p, subst s q)
    | Decl d ->
        M.add s d.var (function
          | Some s -> Decl { d with cond = M.b_subst s d.cond; body = subst s d.body }
          | None -> Decl d)
    | Loop { cond_range; body } ->
        M.add s (Cond_range.var cond_range) (function
          | Some s -> Loop { cond_range = CR.subst s cond_range; body = subst s body }
          | None -> Loop { cond_range; body })
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

let loop ?(cond = Bool true) (r : Range.t) (p : t) : t =
  if p = Skip then Skip
  else
    let is_empty =
      r |> Range.is_empty |> Exp.b_eval_res |> Result.value ~default:false
    in
    if is_empty then Skip
    else Loop { cond_range = Cond_range.make r cond; body = p }

let decl ?(ty = Ty.int) ?(cond = Bool true) (var : Variable.t) : t -> t =
  function
  | Skip -> Skip
  | body -> Decl { var; ty; cond; body }

let rec opt : t -> t = function
  | Skip -> Skip
  | Decl d -> Decl { d with cond = Constfold.b_opt d.cond; body = opt d.body }
  | Seq (p, q) -> seq (opt p) (opt q)
  | Access a -> Access (Constfold.a_opt a)
  | Sync l -> Sync l
  | If (b, p, q) -> if_ (Constfold.b_opt b) (opt p) (opt q)
  | Loop { cond_range; body = p } ->
      loop
        ~cond:(Constfold.b_opt cond_range.cond)
        (Constfold.r_opt cond_range.range)
        (opt p)

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
          let cond = Subst.ReplacePair.b_subst s d.cond in
          let p, new_xs = uniq new_p new_xs in
          (Decl { var = new_x; body = p; ty = d.ty; cond }, new_xs)
        else
          let p, new_xs = uniq p (Variable.Set.add x xs) in
          (Decl { var = x; body = p; ty = d.ty; cond = d.cond }, new_xs)
    | Loop { cond_range; body = p } ->
        let x = Cond_range.var cond_range in
        if Variable.Set.mem x xs then
          let new_x : Variable.t = Variable.fresh xs x in
          let new_xs = Variable.Set.add new_x xs in
          let s = Subst.SubstPair.make (x, Var new_x) in
          let new_p = PSubstPair.subst s p in
          let cond_range =
            Cond_range.make
              { cond_range.range with var = new_x }
              (Subst.ReplacePair.b_subst s cond_range.cond)
          in
          let p, new_xs = uniq new_p new_xs in
          (Loop { cond_range; body = p }, new_xs)
        else
          let p, new_xs = uniq p (Variable.Set.add x xs) in
          (Loop { cond_range; body = p }, new_xs)
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
      let fns = n_free_names s.id fns in
      (match s.participants with Some c -> n_free_names c fns | None -> fns)
  | Access a -> Access.free_names a fns
  | If (b, p, q) -> b_free_names b fns |> free_names p |> free_names q
  | Decl { var = x; cond; body = p; _ } ->
      free_names p fns |> b_free_names cond |> Variable.Set.remove x
  | Loop { cond_range; body = p } ->
      free_names p fns |> Cond_range.free_names cond_range
  | Seq (p, q) -> free_names p fns |> free_names q

(* Only retain CI-DI accesses *)
let rec to_ci_di (approx : Variable.Set.t) : t -> t = function
  | If (b, p, q) ->
      if Exp.b_intersects approx b then Skip
      else If (b, to_ci_di approx p, to_ci_di approx q)
  | Loop { cond_range; body = p } ->
      if Range.intersects approx cond_range.range then Skip
      else
        (* the loop variable is CIDI, hence remove any existing CIDI *)
        let approx = Variable.Set.remove (Cond_range.var cond_range) approx in
        Loop { cond_range; body = to_ci_di approx p }
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
      let ty = Ty.to_string d.ty in
      let guard : Indent.t list =
        match d.cond with
        | Bool true -> []
        | _ -> [ Line ("assume " ^ b_to_string d.cond ^ ";") ]
      in
      (Indent.Line (ty ^ " " ^ var ^ ";") :: guard) @ to_s d.body
  | Loop { cond_range; body = p } ->
      [
        Line ("foreach (" ^ Cond_range.to_string cond_range ^ ") {");
        Block (to_s p);
        Line "}";
      ]
  | Seq (p, q) -> to_s p @ to_s q

let to_string (p : t) : string = Indent.to_string (to_s p)
let print (p : t) : unit = Indent.print (to_s p)
