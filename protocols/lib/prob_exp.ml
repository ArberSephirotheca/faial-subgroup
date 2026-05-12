(* Evaluate expressions and return int32 *)
let eval_nbin : N_binary.t -> Int32.t -> Int32.t -> Int32.t = function
  | BitAnd -> Int32.logand
  | BitXOr -> Int32.logxor
  | BitOr -> Int32.logor
  | Plus -> Int32.add
  | UPlus -> Int32.add
  | Minus -> Int32.sub
  | Mult -> Int32.mul
  | UMult -> Int32.mul
  | Div _ -> Int32.div
  | Mod _ -> Int32.rem
  | LeftShift -> fun x y -> Int32.shift_left x (Int32.to_int y)
  | RightShift -> fun x y -> Int32.shift_right x (Int32.to_int y)
  | URightShift -> fun x y -> Int32.shift_right_logical x (Int32.to_int y)

let eval_nrel : N_rel.t -> Int32.t -> Int32.t -> bool = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Le -> ( <= )
  | ULe -> ( <= )
  | Ge -> ( >= )
  | UGe -> ( >= )
  | Lt -> ( < )
  | ULt -> ( < )
  | Gt -> ( > )
  | UGt -> ( > )

let default_env (x : Variable.t) : (Int32.t, string) Result.t =
  Error ("n_eval: variable " ^ Variable.name x)

let rec n_eval_res ?(env = default_env) (n : Exp.nexp) :
    (Int32.t, string) Result.t =
  let ( let* ) = Result.bind in
  match n with
  | CastInt b ->
      let* b = b_eval_res ~env b in
      Ok (Int32.of_int (if b then 1 else 0))
  | Var x -> env x
  | Unary (BitNot, e) ->
      let* n = n_eval_res ~env e in
      Ok (Int32.neg n)
  | Unary (Negate, e) ->
      let* n = n_eval_res ~env e in
      Ok (Int32.mul Int32.minus_one n)
  | Num n -> Ok (Int32.of_int n)
  | Binary (o, n1, n2) ->
      let* n1 = n_eval_res ~env n1 in
      let* n2 = n_eval_res ~env n2 in
      Ok (eval_nbin o n1 n2)
  | Other _ -> Error "n_eval: other"
  | NCall (x, _) -> Error ("n_eval: call " ^ x)
  | NIf (b, n1, n2) ->
      let* b = b_eval_res ~env b in
      if b then n_eval_res ~env n1 else n_eval_res ~env n2

and b_eval_res ?(env = default_env) (b : Exp.bexp) : (bool, string) Result.t =
  let ( let* ) = Result.bind in
  match b with
  | Bool b -> Ok b
  | CastBool e ->
      let* e = n_eval_res ~env e in
      Ok (not (Int32.equal e Int32.zero))
  | NRel (o, n1, n2) ->
      let* n1 = n_eval_res ~env n1 in
      let* n2 = n_eval_res ~env n2 in
      Ok (eval_nrel o n1 n2)
  | BRel (o, b1, b2) ->
      let* b1 = b_eval_res ~env b1 in
      let* b2 = b_eval_res ~env b2 in
      Ok (B_rel.eval o b1 b2)
  | BNot b ->
      let* b = b_eval_res ~env b in
      Ok (not b)
  | Pred (x, _) -> Error ("b_eval: pred " ^ x)
  | Distinct _ -> Error "Distinct evaluation not implemented yet"

(* Standard int32 bounds *)
let int32_range : Int32.t * Int32.t = (Int32.min_int, Int32.max_int)

(* Returns a range between lo and hi *)
let random_in_range (lo, hi) : Int32.t =
  if lo > hi then invalid_arg "random_in_range: min > max"
  else if lo = hi then lo
  else if lo = Int32.min_int && hi = Int32.max_int then Random.bits32 ()
  else
    let f_hi = Int32.to_float hi in
    let f_lo = Int32.to_float lo in
    let r = Float.(add (sub f_hi f_lo) one) in
    Int32.(add lo (Int32.of_float (Random.float 1.0 *. r)))

(* Given a variable return a randome int32 *)
let sample_int32 (_x : Variable.t) : Int32.t = random_in_range int32_range

let prob ?(sample_size = 5000) ?(sample_var = sample_int32)
    (fns : Variable.t list) (b : Exp.bexp) : float =
  let rec iter (accum : int) : int -> int = function
    | 0 -> accum
    | n ->
        (* vals maps a sample for each variable *)
        let vals : Int32.t Variable.Map.t =
          fns |> List.map (fun x -> (x, sample_var x)) |> Variable.Map.of_list
        in
        (* build an environment from variable to value *)
        let env (x : Variable.t) : (Int32.t, string) Result.t =
          (* if we get an exception, then we have a bug in Freenames *)
          Ok (Variable.Map.find x vals)
        in
        (* calculate the new accumulator by evaluating b again *)
        let accum =
          match b_eval_res ~env b |> Result.get_ok with
          | true -> accum + 1
          | false -> accum
        in
        (* loop again *)
        iter accum (n - 1)
  in
  Float.of_int (iter 0 sample_size) /. Float.of_int sample_size

(*
  Given a goal and a variable, obtain its maximum and minimum values.
  *)
let infer_dom ?(timeout = 100) (x : Variable.t) (goal : Exp.bexp) :
    (Int32.t * Int32.t) option =
  let open Exp in
  let open Z3 in
  let parse_num = Gen_z3.SignedBv32Gen.parse_num in
  let handler (m : Z3.Model.model) : Int32.t option =
    (* Go through all declarations of the model *)
    Model.get_const_decls m
    (* Search for the declaration of x *)
    |> List.find_map (fun d ->
        (* Convert the declaration to a variable *)
        let y : Variable.t =
          d |> FuncDecl.get_name |> Symbol.get_string |> Variable.from_name
        in
        if Variable.equal y x then
          (* Evaluate the value: Variables in the model are actually functions
           with 0 args, so we create a function call *)
          Model.eval m (FuncDecl.apply d []) true
          |> Option.map (fun v ->
              (* Try to cast value to an integer *)
              try
                Expr.to_string v |> parse_num |> Int32.of_string |> Option.some
              with Failure _ -> None)
          |> Option.join
        else None)
  in
  let solve (op : Z3.Optimize.optimize -> Expr.expr -> Z3.Optimize.handle) :
      Int32.t option =
    let open Z3 in
    let ctx = mk_context [ ("timeout", string_of_int timeout) ] in
    let b_to_expr = Gen_z3.SignedBv32Gen.b_to_expr ctx in
    let n_to_expr = Gen_z3.SignedBv32Gen.n_to_expr ctx in
    let opt = Optimize.mk_opt ctx in
    Optimize.add opt [ b_to_expr goal ];
    (* Optimize the given variable *)
    let _ = op opt (n_to_expr (Var x)) in
    match Optimize.check opt with
    | Solver.SATISFIABLE ->
        Optimize.get_model opt |> Option.map handler |> Option.join
    | _ -> None
  in
  match (solve Optimize.minimize, solve Optimize.maximize) with
  | Some lo, Some hi -> Some (lo, hi)
  | _, _ -> None

let calc_range : Int32.t * Int32.t -> int =
 fun (lo, hi) ->
  let x = Int64.(sub (of_int32 hi) (of_int32 lo)) in
  if x > Int64.of_int max_int then max_int else Int64.to_int x

let calc_sample_size ?(sample_size = 5000)
    (doms : (Int32.t * Int32.t) Variable.Map.t) : int =
  let rec iter (size : int) : (Int32.t * Int32.t) list -> int = function
    | [] -> size
    | r :: rs ->
        let r = calc_range r in
        let r = if r = 0 then 1 else r in
        if r >= sample_size then sample_size
        else
          let size = size * r in
          if size > sample_size then sample_size else iter size rs
  in
  match Variable.Map.bindings doms |> List.map snd with
  | [] -> 1 (* only one sample is enough *)
  | r :: rs ->
      let size = calc_range r in
      if size > sample_size then sample_size else iter size rs

let make_doms ?(timeout = 100) (fns : Variable.t list) (goal : Exp.bexp) :
    (Int32.t * Int32.t) Variable.Map.t =
  fns
  |> List.map (fun x ->
      let r =
        (* If there no value can be obtained, then let us
           that means that any value is possible, so let us
           pick the smallest set possible.
           *)
        infer_dom ~timeout x goal
        |> Option.value ~default:(Int32.zero, Int32.one)
      in
      (x, r))
  |> Variable.Map.of_list

let make_sample_var (doms : (Int32.t * Int32.t) Variable.Map.t) (x : Variable.t)
    : Int32.t =
  match Variable.Map.find_opt x doms with
  | Some r -> random_in_range r
  | None -> Random.bits32 ()

(* Calculate a probability where the variables are conditioned according to
   some pre-condition. *)
let cond_prob ?(sample_size = 5000) ?(timeout = 100) ?(pre = Exp.Bool true)
    (goal : Exp.bexp) : float =
  let fns =
    (* free names of both expressions *)
    Exp.b_free_names (Exp.b_and goal pre) Variable.Set.empty
    |> Variable.Set.elements
  in
  (* Calculate range of each variable *)
  let doms = make_doms ~timeout fns pre in
  (* Find a tighter sample size if possible *)
  let sample_size = calc_sample_size ~sample_size doms in
  (* Run the probabilities *)
  prob ~sample_size ~sample_var:(make_sample_var doms) fns (Exp.b_and pre goal)
