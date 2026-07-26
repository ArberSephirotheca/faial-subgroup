open Stage0
open Protocols

(* [UA.t]: per-access coalescing classification used by the UA
   preprocessing. [Constant] survives the [Mult] alignment-elision
   rule, [Uniform] folds away under additive offsets, [AnyAccurate]
   is warp-divergent with exact per-thread evaluation, [Inc] is
   warp-divergent under an approximating compound. *)
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
      | ReadResult rd ->
          let args_t = List.map from_nexp rd.args in
          let r =
            List.fold_left (fun acc (_, t) -> max acc t) Constant args_t
          in
          (ReadResult { rd with args = List.map fst args_t }, r)
      (* Erased rather than rebuilt, on the same terms as [Reals.from_nexp]:
         the cost analyses assume every conversion is the identity, and
         keeping the node here only blocks the algebraic rewrites that strip
         a warp-uniform offset. *)
      | Convert c -> from_nexp c.arg
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

(* Recognise [c * tid_x] for a uniform coefficient [c]: returns the
   coefficient when the expression reduces to that shape, otherwise
   [None]. Used by [run_ua]'s exact-cost shortcut on
   uniform-times-tid coalesced accesses. *)
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

module Make (L : Logger.Logger) = struct
  open Exp

  let to_vectorized (ctx : Analysis_ctx.t) : Vectorized.t =
    let vec = Vectorized.from_config ctx.config in
    if Result.is_ok (Vectorized.b_eval_res ctx.divergence vec) then
      Vectorized.restrict ctx.divergence vec
    else (
      L.info (fun () ->
        "Index analysis: ignoring divergence: "
        ^ Exp.b_to_string ctx.divergence);
      vec)

  let run_ua (ctx : Analysis_ctx.t) : Index_cost.t =
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
      |> Index_cost.from_cost)
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
        |> Index_cost.from_cost
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

  let run_ua_sat ~verbose (ctx : Analysis_ctx.t) : Index_cost.t =
    let vec = to_vectorized ctx in
    (match
       Symbolic_metric_analysis.ua ~verbose ctx.config ctx.locals ctx.divergence
         ctx.index
     with
      | Some i -> Cost.from_int ~value:i ~exact:true ()
      | None -> Vectorized.max_cost Metric.UncoalescedAccesses vec)
    |> Index_cost.from_cost

  let run_count_active_threads ~verbose (ctx : Analysis_ctx.t) : Index_cost.t =
    let vec = to_vectorized ctx in
    (match
       Symbolic_metric_analysis.count_active_threads ~verbose ctx.config
         ctx.locals ctx.divergence (Num 0)
     with
      | Some i -> Cost.from_int ~value:i ~exact:true ()
      | None -> Vectorized.max_cost Metric.ActiveThreads vec)
    |> Index_cost.from_cost
end
