(* Abductive generalisation for genie.

   A CEGAR proposal step that, given a fixed pool of candidate
   predicates and an accumulating sample of racy models, asks Z3
   (as a propositional MaxSAT instance) for the minimum-cardinality
   subset of candidates that excludes every model in the sample.

   The pool is built once per kernel as the Cartesian product of
   template families (sign, single-dim bound, multiplicative bound,
   divisibility, scaled bound, eq-to-dim, cross-parameter) against
   the kernel's int parameters and launch dimensions. Each candidate
   gets a stable Boolean selector that lives across all CEGAR rounds
   for that kernel.

   Following the recipe in Dillig, Dillig & Aiken's MSA-based
   abduction (POPL 2013), with the two-call structure required by
   Z3's [Optimize] (which cannot hard-constrain a sub-formula to be
   UNSAT): step 1 here is the propositional MaxSAT instance; step 2
   (run a race query on the selected subset) is done by the caller. *)

open Protocols
open Exp

(* ----- bexp evaluation under a (name -> int option) lookup ----- *)

let rec eval_n (lookup : string -> int option) : nexp -> int option = function
  | Num n -> Some n
  | Var v -> lookup (Variable.name v)
  | Binary (op, a, b) ->
    (match eval_n lookup a, eval_n lookup b with
     | Some va, Some vb ->
       (try Some (N_binary.eval op va vb) with Division_by_zero -> None)
     | _ -> None)
  | Unary (op, a) -> Option.map (N_unary.eval op) (eval_n lookup a)
  | NCall _ -> None
  | NIf (b, t, f) ->
    (match eval_b lookup b with
     | Some true -> eval_n lookup t
     | Some false -> eval_n lookup f
     | None -> None)
  | Other _ -> None
  | CastInt b ->
    (match eval_b lookup b with
     | Some v -> Some (if v then 1 else 0)
     | None -> None)

and eval_b (lookup : string -> int option) : bexp -> bool option = function
  | Bool b -> Some b
  | NRel (op, a, b) ->
    (match eval_n lookup a, eval_n lookup b with
     | Some va, Some vb -> Some (N_rel.eval op va vb)
     | _ -> None)
  | BRel (op, a, b) ->
    (match eval_b lookup a, eval_b lookup b with
     | Some va, Some vb -> Some (B_rel.eval op va vb)
     | _ -> None)
  | BNot b -> Option.map not (eval_b lookup b)
  | Pred _ -> None
  | CastBool n -> Option.map (fun v -> v <> 0) (eval_n lookup n)
  | Distinct xs ->
    let vs = List.map (eval_n lookup) xs in
    if List.exists Option.is_none vs then None
    else
      let ints = List.map Option.get vs in
      Some (List.length (List.sort_uniq Int.compare ints) = List.length ints)

(* ----- candidate-pool construction ----- *)

let launch_config_set : Variable.Set.t =
  let open Variable in
  Set.union (Set.union tid_set bid_set) (Set.union bdim_set gdim_set)

let int_params (k : Kernel.t) : Variable.t list =
  Params.to_list (Params.union_left k.global_variables k.local_variables)
  |> List.filter_map (fun (v, ty) ->
    if C_type.is_int ty && not (Variable.Set.mem v launch_config_set)
    then Some v else None)
  |> List.sort_uniq Variable.compare

let build_pool (k : Kernel.t) : bexp array =
  let params = int_params k in
  let bdims = [ Variable.bdim_x; Variable.bdim_y; Variable.bdim_z ] in
  let gdims = [ Variable.gdim_x; Variable.gdim_y; Variable.gdim_z ] in
  let all_dims = bdims @ gdims in
  let dim_pairs =
    let pairs = ref [] in
    List.iter (fun a ->
      List.iter (fun b ->
        if Variable.compare a b < 0 then pairs := (a, b) :: !pairs)
        all_dims)
      all_dims;
    !pairs
  in
  let buf = ref [] in
  let push c = buf := c :: !buf in
  List.iter (fun p ->
    (* sign *)
    push (n_gt (Var p) (Num 0));
    (* single-dim bound *)
    List.iter (fun d -> push (n_ge (Var p) (Var d))) all_dims;
    (* multiplicative bound *)
    List.iter (fun (a, b) -> push (n_ge (Var p) (n_mult (Var a) (Var b))))
      dim_pairs;
    (* divisibility *)
    List.iter (fun d -> push (n_eq (n_mod (Var p) (Var d)) (Num 0))) all_dims;
    (* scaled bound *)
    List.iter (fun d ->
      List.iter (fun k -> push (n_ge (Var p) (n_mult (Num k) (Var d))))
        [ 2; 4 ])
      all_dims;
    (* eq-to-dim *)
    List.iter (fun d -> push (n_eq (Var p) (Var d))) all_dims;
    List.iter (fun (a, b) -> push (n_eq (Var p) (n_mult (Var a) (Var b))))
      dim_pairs)
    params;
  (* cross-parameter *)
  let pp = List.mapi (fun i x -> (i, x)) params in
  List.iter (fun (i, p1) ->
    List.iter (fun (j, p2) ->
      if i <> j then push (n_ge (Var p1) (Var p2)))
      pp)
    pp;
  Array.of_list !buf

(* ----- Z3.Optimize session ----- *)

type session = {
  candidates : bexp array;
  ctx : Z3.context;
  opt : Z3.Optimize.optimize;
  selectors : Z3.Expr.expr array;
}

let create_from_pool (candidates : bexp array) : session =
  let n = Array.length candidates in
  let ctx = Z3.mk_context [] in
  let opt = Z3.Optimize.mk_opt ctx in
  let selectors =
    Array.init n (fun i ->
      Z3.Boolean.mk_const_s ctx ("b_" ^ string_of_int i))
  in
  let group = Z3.Symbol.mk_string ctx "minimize" in
  Array.iter (fun sel ->
    let not_sel = Z3.Boolean.mk_not ctx sel in
    let _ : Z3.Optimize.handle = Z3.Optimize.add_soft opt not_sel "1" group in
    ())
    selectors;
  { candidates; ctx; opt; selectors }

let create (k : Kernel.t) : session = create_from_pool (build_pool k)

let build_pool_union (ks : Kernel.t list) : bexp array =
  ks
  |> List.concat_map (fun k -> Array.to_list (build_pool k))
  |> List.sort_uniq Exp.b_compare
  |> Array.of_list

let create_for_kernels (ks : Kernel.t list) : session =
  create_from_pool (build_pool_union ks)

let candidate_count (s : session) : int = Array.length s.candidates

(* Look up an integer-parsed value for [name] in a witness's
   string-valued model. Caller supplies the raw (name, string-value)
   pairs from [Solve_drf.Witness.t.globals.variables]; we re-parse
   to int. *)
let witness_lookup (vars : (string * string) list) : string -> int option =
  let table = Hashtbl.create 32 in
  List.iter (fun (k, v) ->
    match int_of_string_opt (String.trim v) with
    | Some n -> Hashtbl.replace table k n
    | None -> ())
    vars;
  fun name -> Hashtbl.find_opt table name

(* Add a sample to the session: at least one candidate that is
   *false* under [vars] must be selected. Returns the number of
   candidates that disagree with the witness. *)
let add_sample (s : session) (vars : (string * string) list) : int =
  let lookup = witness_lookup vars in
  let bad = ref [] in
  Array.iteri (fun i c ->
    let c' = Predicates.b_inline c in
    match eval_b lookup c' with
    | Some false -> bad := s.selectors.(i) :: !bad
    | _ -> ())
    s.candidates;
  match !bad with
  | [] -> 0
  | xs ->
    Z3.Optimize.add s.opt [ Z3.Boolean.mk_or s.ctx xs ];
    List.length xs

(* Solve. Returns the selected candidate bexps, or [None] if the
   accumulated samples cannot be excluded by any subset of the
   pool (vocabulary exhausted). *)
let solve (s : session) : bexp list option =
  match Z3.Optimize.check s.opt with
  | Z3.Solver.SATISFIABLE ->
    (match Z3.Optimize.get_model s.opt with
     | None -> None
     | Some m ->
       let selected = ref [] in
       Array.iteri (fun i sel ->
         match Z3.Model.eval m sel false with
         | Some v ->
           (match Z3.Boolean.get_bool_value v with
            | Z3enums.L_TRUE -> selected := s.candidates.(i) :: !selected
            | _ -> ())
         | None -> ())
         s.selectors;
       Some !selected)
  | Z3.Solver.UNSATISFIABLE -> None
  | Z3.Solver.UNKNOWN -> None
