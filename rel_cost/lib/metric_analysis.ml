open Stage0
open Protocols

type t = {
  strategy : Analysis_strategy.t;
  locals : Variable.Set.t;
  index : Exp.nexp;
  divergence : Exp.bexp;
  config : Config.t;
}

let const_tid (tid : Variable.t) : Exp.nexp -> Exp.nexp option =
  let rec const_tid : Exp.nexp -> Exp.nexp option = function
    | Var x when Variable.(equal tid x) -> Some (Num 1)
    | Binary ((Mult _ as op), e1, e2) -> (
        match const_tid e1 with
        | Some e1 -> Some (Binary (op, e1, e2))
        | None -> (
            match const_tid e2 with
            | Some e2 -> Some (Binary (op, e1, e2))
            | None -> None))
    | _ -> None
  in
  const_tid

module UA = struct
  type t = Constant | Uniform | AnyAccurate | Inc

  let max (x1 : t) (x2 : t) : t =
    match (x1, x2) with
    | Constant, e | e, Constant -> e
    | Uniform, e | e, Uniform -> e
    | AnyAccurate, e | e, AnyAccurate -> e
    | Inc, Inc -> Inc

  let bin : N_binary.t -> Exp.nexp * t -> Exp.nexp * t -> Exp.nexp * t =
   fun o (e1, x1) (e2, x2) ->
    let both : Exp.nexp = Binary (o, e1, e2) in
    match (o, x1, x2) with
    | (Plus _ | Minus _), Uniform, (AnyAccurate | Inc) -> (e2, Inc)
    | (Plus _ | Minus _), (AnyAccurate | Inc), Uniform -> (e1, Inc)
    | _, _, _ -> (both, max x1 x2)

  let map (f : Exp.nexp -> Exp.nexp) ((e, x) : Exp.nexp * t) : Exp.nexp * t =
    (f e, x)

  let from_nexp (cfg : Config.t) (locals : Variable.Set.t) :
      Exp.nexp -> Exp.nexp * t =
    let locals = Variable.Set.union locals Variable.tid_set in
    let word : int = Config.memory_segments_bits cfg in
    let is_aligned (e : Exp.nexp) (ty : t) : bool =
      ty <> AnyAccurate
      && match e with Num n -> n > 0 && n mod word == 0 | _ -> false
    in
    let rec from_nexp : Exp.nexp -> Exp.nexp * t = function
      | Num n -> (Num n, Constant)
      | Var x ->
          let r =
            if Config.is_warp_uniform x cfg then Uniform
            else if Variable.Set.mem x locals then AnyAccurate
            else Uniform
          in
          (Var x, r)
      | Unary (o, e) -> map (fun e -> Unary (o, e)) (from_nexp e)
      | Binary (Mult _, Num n1, Num n2) -> (Num (n1 * n2), Constant)
      | Binary (Mult _, Binary (Mult _, e, Num n1), Num n2)
      | Binary (Mult _, Binary (Mult _, Num n1, e), Num n2)
      | Binary (Mult _, Num n1, Binary (Mult _, e, Num n2))
      | Binary (Mult _, Num n1, Binary (Mult _, Num n2, e)) ->
          from_nexp Exp.(n_mult (Num (n1 * n2)) e)
      | Binary (Mult _, Num n, Binary (Mult _, e1, e2)) ->
          from_nexp Exp.(n_mult (n_mult (Num n) e1) e2)
      | Binary (Mult _, Num n, Binary ((Plus _ as op), e1, e2))
      | Binary (Mult _, Binary ((Plus _ as op), e1, e2), Num n) ->
          from_nexp
            (Binary (op, Exp.n_mult (Num n) e1, Exp.n_mult (Num n) e2))
      | Binary ((Mult _ as op), e1, e2) ->
          let e1, ty1 = from_nexp e1 in
          let e2, ty2 = from_nexp e2 in
          (* Any divisor of 32 can be elided when it's being multiplies by
         a uniform/constant/inc *)
          if is_aligned e1 ty2 || is_aligned e2 ty1 then (Num word, Constant)
          else bin op (e1, ty1) (e2, ty2)
      | Binary (o, e1, e2) -> bin o (from_nexp e1) (from_nexp e2)
      | NCall (f, args) ->
          let args_t = List.map from_nexp args in
          let r =
            List.fold_left (fun acc (_, t) -> max acc t) Constant args_t
          in
          (NCall (f, List.map fst args_t), r)
      | CastInt e ->
          let r = if Exp.b_intersects locals e then AnyAccurate else Uniform in
          (CastInt e, r)
      | NIf (c, e1, e2) ->
          if Exp.b_intersects locals c then (NIf (c, e1, e2), Inc)
          else
            let e1, r1 = from_nexp e1 in
            let e2, r2 = from_nexp e2 in
            (NIf (c, e1, e2), max r1 r2)
    in
    from_nexp

  let to_string : Exp.nexp * t -> string =
   fun (e, x) ->
    let prefix =
      match x with
      | Constant -> "num"
      | AnyAccurate -> "accurate"
      | Inc -> "inc"
      | Uniform -> "unif"
    in
    Exp.n_to_string e ^ ": " ^ prefix
end

module BC = struct
  (*
    Given a numeric expression try to remove any offsets in the form of
    `expression + constant` or `expression - constant`.

    The way we do this is by first getting all the free-names that are
    **not** tids. Secondly, we rearrange the expression as a polynomial
    in terms of each free variable. Third, we only keep polynomials that
    mention a tid, otherwise we can safely discard such a polynomial.
  *)
  type t = Uniform | Any

  let bin : N_binary.t -> Exp.nexp * t -> Exp.nexp * t -> Exp.nexp * t =
   fun o (e1, x1) (e2, x2) ->
    let both : Exp.nexp = Binary (o, e1, e2) in
    match (o, x1, x2) with
    | (Plus _ | Minus _), Any, Uniform -> (e1, Any)
    | (Plus _ | Minus _), Uniform, Any -> (e2, Any)
    | _, Uniform, Uniform -> (both, Uniform)
    | _, _, _ -> (both, Any)

  let map (f : Exp.nexp -> Exp.nexp) ((e, x) : Exp.nexp * t) : Exp.nexp * t =
    (f e, x)

  let from_nexp (cfg : Config.t) (locals : Variable.Set.t) :
      Exp.nexp -> Exp.nexp * t =
    let locals = Variable.Set.union locals Variable.tid_set in
    let rec from_nexp : Exp.nexp -> Exp.nexp * t = function
      | Num n -> (Num n, Uniform)
      | Var x ->
          let r =
            if Config.is_warp_uniform x cfg then Uniform
            else if Variable.Set.mem x locals then Any
            else Uniform
          in
          (Var x, r)
      | Unary (o, e) -> map (fun e -> Unary (o, e)) (from_nexp e)
      | Binary (o, e1, e2) -> bin o (from_nexp e1) (from_nexp e2)
      | NCall (f, args) ->
          let args_t = List.map from_nexp args in
          let r =
            List.fold_left
              (fun acc (_, t) -> if acc = Any || t = Any then Any else Uniform)
              Uniform args_t
          in
          (NCall (f, List.map fst args_t), r)
      | CastInt e ->
          let r = if Exp.b_intersects locals e then Any else Uniform in
          (CastInt e, r)
      | NIf (c, e1, e2) ->
          if Exp.b_intersects locals c then (NIf (c, e1, e2), Any)
          else
            let e1, r1 = from_nexp e1 in
            let e2, r2 = from_nexp e2 in
            let r = if r1 = r2 then r1 else Any in
            (NIf (c, e1, e2), r)
    in
    from_nexp

  let to_string : Exp.nexp * t -> string =
   fun (e, x) ->
    let prefix = match x with Any -> "any" | Uniform -> "unif" in
    Exp.n_to_string e ^ ": " ^ prefix
end

module IndexCost = struct
  type t = { code : Ra.Stmt.t; exact : bool }

  let from_cost (c : Cost.t) : t =
    { code = Ra.Stmt.Tick (Cost.value c); exact = c.exact }

  let to_cost (e : t) : (Cost.t, string) Result.t =
    match e.code with
    | Ra.Stmt.Tick n -> Ok (Cost.from_int ~value:n ~exact:e.exact ())
    | _ -> Error ("to_cost: " ^ Ra.Stmt.to_string e.code)

  let to_string (e : t) : string = Ra.Stmt.to_string e.code
end

module Make (L : Logger.Logger) = struct
  open Exp

  let bc_remove_offset_aux (cfg : Config.t) (locals : Variable.Set.t)
      (index : Exp.nexp) : Exp.nexp =
    let after =
      match BC.from_nexp cfg locals index with
      | _, Uniform -> Num 0
      | e, Any -> e
    in
    if index <> after then
      L.info (fun () ->
        "BC: removed offset: " ^ Exp.n_to_string index ^ " 🡆 "
       ^ Exp.n_to_string after);
    after

  let bc_remove_offset (ctx : t) : Exp.nexp =
    bc_remove_offset_aux ctx.config ctx.locals ctx.index

  let to_vectorized (ctx : t) : Vectorized.t =
    let vec = Vectorized.from_config ctx.config in
    if Result.is_ok (Vectorized.b_eval_res ctx.divergence vec) then
      Vectorized.restrict ctx.divergence vec
    else (
      L.info (fun () ->
        "Index analysis: ignoring divergence: "
        ^ Exp.b_to_string ctx.divergence);
      vec)

  let bc_simulate (vec : Vectorized.t) (index : Exp.nexp) : Cost.t =
    match Vectorized.bank_conflicts index vec with
    | Ok cost -> cost
    | Error msg ->
      L.info (fun () ->
        "BC: could not simulate cost " ^ Exp.n_to_string index ^ ": " ^ msg);
      Vectorized.max_cost Metric.BankConflicts vec

  (* Delin-driven preprocessor. Falls through to [NeedsSimulation] until
     [Bc_axis.classify_index] is wired with real per-axis logic. *)
  let bc_preprocess (ctx : t) : Bc_axis.bc_outcome =
    let stripped = bc_remove_offset ctx in
    match stripped with
    | Num 0 -> Bc_axis.Exact (Cost.from_int ~value:0 ~exact:true ())
    | _ ->
      (* Mirror [BC.from_nexp]'s locals-with-tids union: a thread coord
         is induction-side in delin's classification (not a parameter),
         so it must be excluded from the [globals] passed to
         [Expr.from_nexp]. *)
      let local_scope =
        Variable.Set.union ctx.locals Variable.tid_set
      in
      let globals =
        Variable.Set.diff
          (Exp.n_free_names stripped Variable.Set.empty)
          local_scope
      in
      let expr = Delin.Expr.from_nexp ~globals stripped in
      let size_params = Delin.size_params expr in
      match
        Delin.Greedy.candidates ~globals ~size_params expr |> Seq.uncons
      with
      | None -> Bc_axis.NeedsSimulation stripped
      | Some (idx, _) ->
        let vec = to_vectorized ctx in
        let classes =
          Bc_axis.classify_index ~config:ctx.config ~locals:ctx.locals idx
        in
        Bc_axis.decide
          ~config:ctx.config
          ~tid_count:(Vectorized.tid_count vec)
          ~reduced:stripped classes

  let run_bc ~(delin_bc : bool) (ctx : t) : IndexCost.t =
    let vec = to_vectorized ctx in
    if delin_bc then
      match bc_preprocess ctx with
      | Bc_axis.Exact cost ->
        L.info (fun () ->
          "BC: delin Exact: " ^ Exp.n_to_string ctx.index ^ " 🡆 "
         ^ Cost.to_string cost);
        IndexCost.from_cost cost
      | Bc_axis.NeedsSimulation index ->
        L.info (fun () ->
          "BC: delin NeedsSimulation: " ^ Exp.n_to_string ctx.index ^ " 🡆 "
         ^ Exp.n_to_string index);
        bc_simulate vec index |> IndexCost.from_cost
    else
      let index = bc_remove_offset ctx in
      bc_simulate vec index |> IndexCost.from_cost

  let run_ua (ctx : t) : IndexCost.t =
    let vec = to_vectorized ctx in
    let index, ty = UA.from_nexp ctx.config ctx.locals ctx.index in
    if ctx.index <> index then
      L.info (fun () ->
        Printf.sprintf "UA: removed offset: %s 🡆 %s"
           (Exp.n_to_string ctx.index)
           (Exp.n_to_string index));
    if ty = UA.Uniform || ty = UA.Constant then (
      L.info (fun () ->
        "UA: found coalesced access (warp-uniform): "
       ^ Exp.n_to_string ctx.index);
      Cost.from_int ~value:Metric.UncoalescedAccesses.min_cost ~exact:true ()
      |> IndexCost.from_cost)
    else
      let to_cost index =
        (match Vectorized.uncoalesced index vec with
          | Ok cost ->
              if ty = UA.Inc then (
                L.info (fun () ->
                  "UA: incrementing approximated cost: " ^ Cost.to_string cost);
                let v =
                  min (cost.value + 1)
                    (Vectorized.max_cost UncoalescedAccesses vec |> Cost.value)
                in
                Cost.set_value v cost)
              else cost
          | Error msg ->
              L.info (fun () ->
                "UA: could not simulate cost " ^ Exp.n_to_string index ^ ": "
               ^ msg);
              Vectorized.max_cost UncoalescedAccesses vec)
        |> IndexCost.from_cost
      in
      let fns = Exp.n_free_names index Variable.Set.empty in
      let globals = Variable.Set.diff fns ctx.locals in
      let unknowns = Variable.Set.diff ctx.locals Variable.tid_set in
      let my_unknowns = Variable.Set.inter fns unknowns in
      (* When there are globals and no unknowns *)
      if
        (not (Variable.Set.is_empty globals))
        && Variable.Set.is_empty my_unknowns
      then
        (* Try to find tidx * uniform *)
        match const_tid Variable.tid_x index with
        | Some coef ->
            L.info (fun () ->
              Printf.sprintf
                 "UA: found uniform-times-tid, generating exact cost: %s 🡆 %s"
                 (Exp.n_to_string index) (Exp.n_to_string coef));
            let code =
              Ra.Stmt.Opt.clamp ~value:(Constfold.n_opt coef)
                ~upper_bound:(Vectorized.tid_count vec)
            in
            { code; exact = true }
        | _ -> to_cost index
      else index |> to_cost

  let run_ua_sat ~verbose (ctx : t) : IndexCost.t =
    let vec = to_vectorized ctx in
    (match
       Symbolic_metric_analysis.ua ~verbose ctx.config ctx.locals ctx.divergence
         ctx.index
     with
      | Some i -> Cost.from_int ~value:i ~exact:true ()
      | None -> Vectorized.max_cost Metric.UncoalescedAccesses vec)
    |> IndexCost.from_cost

  let run_count_active_threads ~verbose (ctx : t) : IndexCost.t =
    let vec = to_vectorized ctx in
    (match
       Symbolic_metric_analysis.count_active_threads ~verbose ctx.config
         ctx.locals ctx.divergence (Num 0)
     with
      | Some i -> Cost.from_int ~value:i ~exact:true ()
      | None -> Vectorized.max_cost Metric.ActiveThreads vec)
    |> IndexCost.from_cost

  let run_count (_ctx : t) : IndexCost.t =
    IndexCost.from_cost (Cost.from_int ~value:1 ~exact:true ())

  let run ?(delin_bc = false) (m : Metric.t) (config : Config.t) ~verbose
      ~strategy ~locals ~index ~divergence : IndexCost.t =
    let run =
      match m with
      | BankConflicts -> run_bc ~delin_bc
      | UncoalescedAccesses -> run_ua
      | UncoalescedAccessesSat -> run_ua_sat ~verbose
      | CountAccesses -> run_count
      | ActiveThreads -> run_count_active_threads ~verbose
    in
    run { config; divergence; strategy; locals; index }
end

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
