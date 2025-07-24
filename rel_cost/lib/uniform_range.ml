open Protocols

type t = Maximize | Minimize

(*
  Maximizes the given expression, and replaces tids by concrete values.
  *)
let optimize ?(timeout = 100) (strategy : t) (block_dim : Dim3.t)
    (pre : Exp.bexp) (n : Exp.nexp) : (Variable.t * Exp.nexp) list option =
  let open Exp in
  let solve (opt : Z3.Optimize.optimize) (lb : Z3.Expr.expr) :
      Z3.Model.model option =
    let open Z3 in
    let _ =
      match strategy with
      | Maximize -> Optimize.maximize opt lb
      | Minimize -> Optimize.minimize opt lb
    in
    if Optimize.check opt = Solver.SATISFIABLE then Optimize.get_model opt
    else None
  in
  let open Z3 in
  let ctx = mk_context [ ("timeout", string_of_int timeout) ] in
  let b_to_expr = Gen_z3.IntGen.b_to_expr ctx in
  let n_to_expr = Gen_z3.IntGen.n_to_expr ctx in
  let parse_num = Gen_z3.IntGen.parse_num in
  let x = Var (Variable.from_name "?max") in
  let restrict tid tid_count =
    let lhs = n_ge (Var tid) (Num 0) in
    let rhs = n_lt (Var tid) (Num tid_count) in
    b_to_expr (b_and lhs rhs)
  in
  let opt = Optimize.mk_opt ctx in
  Optimize.add opt
    [
      (*
        Bit-vector maximization has no notion of signedness.
        The following constrain guarantees that the goal being maximized
        is a signed-positive number.

        https://stackoverflow.com/questions/64484347/
      *)
      b_to_expr (n_ge x (Num 0));
      b_to_expr (n_eq x n);
      b_to_expr pre;
      restrict Variable.tid_x block_dim.x;
      restrict Variable.tid_y block_dim.y;
      restrict Variable.tid_z block_dim.z;
    ];
  solve opt (n_to_expr x)
  |> Option.map (fun m ->
         (* Go through all declarations of the model *)
         Model.get_const_decls m
         |> List.map (fun d ->
                (* Convert the declaration to a variable *)
                let tid : Variable.t =
                  d |> FuncDecl.get_name |> Symbol.get_string
                  |> Variable.from_name
                in
                (d, tid))
         (* Only keep tids *)
         |> List.filter (fun (_, tid) -> Variable.is_tid tid)
         (* Evaluate the value *)
         |> List.filter_map (fun (d, tid) ->
                (* Replace each tid by the value in the model *)
                (* Variables in the model are actually functions with
        0 args, so we create a function call *)
                (* We then evaluate the function call *)
                Model.eval m (FuncDecl.apply d []) true
                |> Option.map (fun tid_val -> (tid, tid_val)))
         |> List.filter_map (fun (tid, tid_val) ->
                (* Try to cast tid to an integer and then substitute *)
                (* Try to cast a value to a string, if we fail, return None *)
                try
                  let tid_val : int =
                    Expr.to_string tid_val |> parse_num |> int_of_string
                  in
                  Some (tid, Num tid_val)
                with Failure _ -> None))

(*
  Given a range, makes that range uniform according to tids.
  *)
let uniform (strategy : t) (params : Params.t) (block_dim : Dim3.t)
    (r : Range.t) : Range.t option =
  let open Exp in
  let fvs = Range.free_names r Variable.Set.empty in
  let pre = params |> Params.retain_all fvs |> Params.to_bexp in
  if Variable.contains_tids fvs then
    let r_subst (r : Range.t) : (Variable.t * Exp.nexp) list -> Range.t =
      List.fold_left (fun r (k, v) -> Subst.ReplacePair.r_subst (k, v) r) r
    in
    match
      optimize strategy block_dim pre (n_minus r.upper_bound r.lower_bound)
    with
    | None -> None
    | Some v ->
        let r' = r_subst r v in
        Some r'
  else None
