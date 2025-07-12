open Stage0
open Protocols

type t = {
  strategy: Analysis_strategy.t;
  locals: Variable.Set.t;
  index: Exp.nexp;
  divergence: Exp.bexp;
  config: Config.t;
}

let const_tid (tid:Variable.t) : Exp.nexp -> Exp.nexp option =
  let rec const_tid : Exp.nexp -> Exp.nexp option =
    function
    | Var x when Variable.(equal tid x) -> Some (Num 1)
    | Binary (Mult, e1, e2) ->
      (match const_tid e1 with
      | Some e1 -> Some (Binary (Mult, e1, e2))
      | None ->
        (match const_tid e2 with
        | Some e2 -> Some (Binary (Mult, e1, e2))
        | None -> None)
      )
    | _ -> None
  in
  const_tid

module UA = struct
  type t =
    | Constant
    | Uniform
    | AnyAccurate
    | Inc

  let max (x1:t) (x2:t) : t =
    match x1, x2 with
    | Constant, e | e, Constant -> e
    | Uniform, e | e, Uniform -> e
    | AnyAccurate, e | e, AnyAccurate -> e
    | Inc, Inc -> Inc

  let bin : N_binary.t -> (Exp.nexp * t) -> (Exp.nexp * t) -> (Exp.nexp * t) =
    fun o (e1, x1) (e2, x2) ->
      let both : Exp.nexp = Binary (o, e1, e2) in
      match o, x1, x2 with
      | (Plus | Minus), Uniform, (AnyAccurate | Inc) -> e2, Inc
      | (Plus | Minus), (AnyAccurate | Inc), Uniform -> e1, Inc
      | _, _, _ -> both, max x1 x2

  let map (f:Exp.nexp -> Exp.nexp) ((e,x): Exp.nexp * t) : Exp.nexp * t =
    f e, x

  let from_nexp (cfg:Config.t) (locals:Variable.Set.t) : Exp.nexp -> Exp.nexp * t =
    let locals = Variable.Set.union locals Variable.tid_set in
    let word : int = 8 * cfg.bytes_per_word in
    let is_aligned (e:Exp.nexp) (ty:t) : bool =
      ty <> AnyAccurate &&
      match e with
      | Num n -> n > 0 && n mod word == 0
      | _ -> false
    in
    let rec from_nexp : Exp.nexp -> Exp.nexp * t =
      function
      | Num n -> Num n, Constant
      | Var x ->
        let r =
          if Config.is_warp_uniform x cfg then
            Uniform
          else if Variable.Set.mem x locals then
            AnyAccurate
          else
            Uniform
        in
        Var x, r
      | Unary (o, e) ->
        map (fun e -> Unary (o, e)) (from_nexp e)
      | Binary (Mult, Num n1, Num n2) ->
        Num (n1 * n2), Constant
      | Binary (Mult, Binary (Mult, e, Num n1), Num n2)
      | Binary (Mult, Binary (Mult, Num n1, e), Num n2)
      | Binary (Mult, Num n1, Binary (Mult, e, Num n2))
      | Binary (Mult, Num n1, Binary (Mult, Num n2, e)) ->
        from_nexp Exp.(n_mult (Num (n1 * n2)) e)
      | Binary (Mult, Num n, Binary (Mult, e1, e2)) ->
        from_nexp Exp.(n_mult (n_mult (Num n) e1) e2)
      | Binary (Mult, Num n, Binary (Plus, e1, e2))
      | Binary (Mult, Binary (Plus, e1, e2), Num n) ->
        from_nexp (Exp.(n_plus (n_mult (Num n) e1) (n_mult (Num n) e2)))
      | Binary (Mult, e1, e2) ->
        let (e1, ty1) = from_nexp e1 in
        let (e2, ty2) = from_nexp e2 in
      (* Any divisor of 32 can be elided when it's being multiplies by
         a uniform/constant/inc *)
        if is_aligned e1 ty2 || is_aligned e2 ty1 then
          (Num word, Constant)
        else
          bin Mult (e1, ty1) (e2, ty2)
      | Binary (o, e1, e2) ->
        bin o (from_nexp e1) (from_nexp e2)
      | NCall (f, e) ->
        map (fun e -> NCall (f, e)) (from_nexp e)
      | Other e ->
        map (fun e -> Other e) (from_nexp e)
      | CastInt e ->
        let r =
          if Exp.b_intersects locals e then
            AnyAccurate
          else
            Uniform
        in
        CastInt e, r
      | NIf (c, e1, e2) ->
        if Exp.b_intersects locals c then
          NIf (c, e1, e2), Inc
        else
          let (e1, r1) = from_nexp e1 in
          let (e2, r2) = from_nexp e2 in
          NIf (c, e1, e2), max r1 r2
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
  type t =
    | Uniform
    | Any

  let bin : N_binary.t -> (Exp.nexp * t) -> (Exp.nexp * t) -> (Exp.nexp * t) =
    fun o (e1, x1) (e2, x2) ->
      let both : Exp.nexp = Binary (o, e1, e2) in
      match o, x1, x2 with
      | (Plus | Minus), Any, Uniform -> e1, Any
      | (Plus | Minus), Uniform, Any -> e2, Any
      | _, Uniform, Uniform -> both, Uniform
      | _, _, _ -> both, Any

  let map (f:Exp.nexp -> Exp.nexp) ((e,x): Exp.nexp * t) : Exp.nexp * t =
    f e, x

  let from_nexp (cfg:Config.t) (locals:Variable.Set.t) : Exp.nexp -> Exp.nexp * t =
    let locals = Variable.Set.union locals Variable.tid_set in
    let rec from_nexp : Exp.nexp -> Exp.nexp * t =
      function
      | Num n -> Num n, Uniform
      | Var x ->
        let r =
          if Config.is_warp_uniform x cfg then
            Uniform
          else if Variable.Set.mem x locals then
            Any
          else
            Uniform
        in
        Var x, r
      | Unary (o, e) ->
        map (fun e -> Unary (o, e)) (from_nexp e)
      | Binary (o, e1, e2) ->
        bin o (from_nexp e1) (from_nexp e2)
      | NCall (f, e) ->
        map (fun e -> NCall (f, e)) (from_nexp e)
      | Other e ->
        map (fun e -> Other e) (from_nexp e)
      | CastInt e ->
        let r =
          if Exp.b_intersects locals e then
            Any
          else
            Uniform
        in
        CastInt e, r
      | NIf (c, e1, e2) ->
        if Exp.b_intersects locals c then
          NIf (c, e1, e2), Any
        else
          let (e1, r1) = from_nexp e1 in
          let (e2, r2) = from_nexp e2 in
          let r =
            if r1 = r2 then
              r1
            else
              Any
          in
          NIf (c, e1, e2), r
    in
    from_nexp

  let to_string : Exp.nexp * t -> string =
    fun (e, x) ->
      let prefix =
        match x with
        | Any -> "any"
        | Uniform -> "unif"
      in
      Exp.n_to_string e ^ ": " ^ prefix
end

module IndexCost = struct
  type t = {
    code: Ra.Stmt.t;
    exact: bool;
  }
  let from_cost (c:Cost.t) : t =
    { code = Ra.Stmt.Tick (Cost.value c); exact=c.exact }
end

module Make (L:Logger.Logger) = struct
  open Exp

  let bc_remove_offset_aux
    (cfg:Config.t)
    (locals:Variable.Set.t) (index:Exp.nexp)
  : Exp.nexp =
    let after =
      match BC.from_nexp cfg locals index with
      | _, Uniform -> Num 0
      | e, Any -> e
    in
    (if index <> after then
      L.info ("BC: removed offset: " ^ Exp.n_to_string index ^ " 🡆 " ^ Exp.n_to_string after)
    );
    after

  let bc_remove_offset (ctx:t) : Exp.nexp =
    bc_remove_offset_aux ctx.config ctx.locals ctx.index

  let to_vectorized (ctx:t) : Vectorized.t =
    let vec = Vectorized.from_config ctx.config in
    if Result.is_ok (Vectorized.b_eval_res ctx.divergence vec) then
      Vectorized.restrict ctx.divergence vec
    else (
      L.info (
        "Index analysis: ignoring divergence: " ^ Exp.b_to_string ctx.divergence
      );
      vec
    )

  let run_bc
    (ctx:t)
  :
    IndexCost.t
  =
    let vec = to_vectorized ctx in
    let index = bc_remove_offset ctx in
    (match Vectorized.to_cost Metric.BankConflicts index vec with
    | Ok cost -> cost
    | Error msg ->
      L.info ("BC: could not simulate cost " ^ Exp.n_to_string index ^ ": " ^ msg);
      Vectorized.max_cost Metric.BankConflicts vec)
    |> IndexCost.from_cost

  let run_ua (ctx:t) : IndexCost.t =
    let vec = to_vectorized ctx in
    let (index, ty) = UA.from_nexp ctx.config ctx.locals ctx.index in
    (if ctx.index <> index then
      L.info (Printf.sprintf
        "UA: removed offset: %s 🡆 %s"
        (Exp.n_to_string ctx.index)
        (Exp.n_to_string index)
      )
    );
    if ty = UA.Uniform || ty = UA.Constant then (
      L.info ("UA: found coalesced access (warp-uniform): " ^
        Exp.n_to_string ctx.index);
      Cost.from_int ~value:Metric.min_uncoalesced_accesses ~exact:true ()
      |> IndexCost.from_cost
    ) else
    let to_cost index =
      (match Vectorized.to_cost UncoalescedAccesses index vec with
      | Ok cost ->
        if ty = UA.Inc then (
          L.info ("UA: incrementing approximated cost: " ^ Cost.to_string cost);
          let v =
            min
              (cost.value + 1)
              (Vectorized.max_cost UncoalescedAccesses vec |> Cost.value)
          in
          Cost.set_value v cost
        ) else
          cost
      | Error msg ->
        L.info ("UA: could not simulate cost " ^ Exp.n_to_string index ^ ": " ^ msg);
        Vectorized.max_cost UncoalescedAccesses vec)
      |> IndexCost.from_cost
    in
    let fns = Exp.n_free_names index Variable.Set.empty in
    let globals = Variable.Set.diff fns ctx.locals in
    let unknowns = Variable.Set.diff ctx.locals Variable.tid_set in
    let my_unknowns = Variable.Set.inter fns unknowns in
    (* When there are globals and no unknowns *)
    if not (Variable.Set.is_empty globals) && Variable.Set.is_empty my_unknowns then (
      (* Try to find tidx * uniform *)
      match const_tid Variable.tid_x index with
      | Some coef ->
        L.info (
          Printf.sprintf
            "UA: found uniform-times-tid, generating exact cost: %s 🡆 %s"
            (Exp.n_to_string index)
            (Exp.n_to_string coef)
          );
        let code =
          Ra.Stmt.Clamp {value=coef; upper_bound=Vectorized.tid_count vec}
        in
        {code; exact=true}
      | _ -> to_cost index
    ) else
      index
      |> to_cost

  let run_count
    (_ctx:t)
  :
    IndexCost.t
  =
    IndexCost.from_cost (Cost.from_int ~value:1 ~exact:true ())

  let run (m:Metric.t) (config:Config.t)
    ~strategy
    ~locals
    ~index
    ~divergence
  :
    IndexCost.t
  =
    let run =
      match m with
      | BankConflicts -> run_bc
      | UncoalescedAccesses -> run_ua
      | CountAccesses -> run_count
    in
    run {config; divergence; strategy; locals; index}
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
