open Stage0
open Protocols
module UniformCond = struct
  type t = Exact | Approximate
end

module Metrics = struct
  type statistics = { approximate: int; exact : int ; total: int}
  type t = {index: statistics; loop: statistics; condition: statistics}
  let empty : t = {
    index = {approximate=0; exact=0; total=0};
    loop = {approximate=0; exact=0; total=0};
    condition={approximate=0; exact=0; total=0}
  }
  let count_as (exact: bool) =
    {
      approximate=(if exact then 0 else 1);
      exact=if exact then 1 else 0;
      total=1
    }

  let (>>) f g x = g(f(x))

  let isExact : Divergence.t -> bool = function
    | Uniform -> true
    | Divergent -> false
  let div_to_stat = isExact >> count_as

  let loop = div_to_stat >> fun stat -> { empty with loop=stat }
  let index = div_to_stat >> fun stat -> { empty with index=stat }
  let condition = div_to_stat >> fun stat -> { empty with condition=stat }

  let add (l:statistics) (r:statistics) : statistics =
    {
      approximate = l.approximate + r.approximate;
      exact = l.exact + r.exact;
      total= l.total + r.total
    }
  let add (l:t) (r:t) : t =
    {
      index = add l.index r.index;
      loop = add l.loop r.loop;
      condition=add l.condition r.condition
    }
  let to_string (a:t) : string =
    let inner (e:statistics) : string =
      Printf.sprintf
        "{exact=%d, approximate=%d, total=%d}"
        e.exact
        e.approximate
        e.total
    in
    Printf.sprintf
      "{index=%s, loop=%s, cond=%s}"
      (inner a.index)
      (inner a.loop)
      (inner a.condition)
end

module Approx = struct
  type t = {exact_index: bool; exact_loop: bool; exact_condition: bool}

  let exact : t =
    { exact_index = true; exact_loop = true; exact_condition = true}

  let to_string (e:t) : string =
    let f =
      function
      | true -> "exact"
      | false -> "inexact"
    in
    Printf.sprintf
      "{index=%s, loop=%s, cond=%s}"
      (f e.exact_index)
      (f e.exact_loop)
      (f e.exact_condition)

  let set_exact_index (e:bool) (a:t) : t =
    { a with exact_index = e }

  let add (lhs:t) (rhs:t) : t =
    {
      exact_index = lhs.exact_index && rhs.exact_index;
      exact_loop = lhs.exact_loop && rhs.exact_loop;
      exact_condition = lhs.exact_condition && rhs.exact_condition;
    }

  let is_thread_uniform (e:t) : bool =
    e.exact_loop && e.exact_condition

  let is_thread_divergent (e:t) : bool =
    not (is_thread_uniform e)

  let set_unexact_cond (e:t) : t =
    { e with exact_condition = false }

  let set_unexact_loop (e:t) : t =
    { e with exact_loop = false }
end

let to_optimize : Analysis_strategy.t -> Uniform_range.t =
  function
  | OverApproximation -> Uniform_range.Maximize
  | UnderApproximation -> Uniform_range.Minimize

module Make (L:Logger.Logger) = struct
  module R = Uniform_range.Make(L)
  module I = Index_analysis.Make(L)
  module L = Linearize_index.Make(L)

  let from_access_context
    (idx_analysis : Variable.Set.t -> Exp.nexp -> int)
  :
    Bank.t -> Ra.Stmt.t
  =
    let rec from (locals:Variable.Set.t) : Bank.Code.t -> Ra.Stmt.t =
      function
      | Index a -> Tick (idx_analysis locals a)
      | Cond (_, p) -> from locals p
      | Decl (x, p) -> from (Variable.Set.add x locals) p
      | Loop {range; body} -> Loop {range; body=from locals body}
    in
    fun k ->
      from k.local_variables k.code
  module Context = struct
    type t = {
      divergence: Exp.bexp;
      approx: Approx.t;
      locals: Variable.Set.t;
    }

    let make (locals:Variable.Set.t) : t =
      { divergence = Bool true; approx = Approx.exact; locals}

    let add_local (var:Variable.t) (ctx:t) : t =
      { ctx with locals = Variable.Set.add var ctx.locals }

    let set_unexact_cond (ctx:t) : t =
      { ctx with approx = Approx.set_unexact_cond ctx.approx }

    let set_unexact_loop (ctx:t) : t =
      { ctx with approx = Approx.set_unexact_loop ctx.approx }

    let rec add_condition (cond:Exp.bexp) (ctx:t) : t * Divergence.t * Metrics.t =
      (*
        When we find an and, we try to each sub-expression so that we can
        be as precise as possible and possibly miss some sub-conditions.
      *)
      match cond with
      | BRel (BAnd, cond1, cond2) ->
        let (ctx, div1, metrics1) = add_condition cond1 ctx in
        let (ctx, div2, metrics2) = add_condition cond2 ctx in
        let div = Divergence.add div1 div2 in
        (ctx, div, Metrics.add (Metrics.condition div) (Metrics.add metrics1 metrics2))
      | _ ->
        let fns =
          Variable.Set.inter
            (Exp.b_free_names cond Variable.Set.empty)
            ctx.locals
        in
        let only_tid_in_locals =
          Variable.Set.diff fns Variable.tid_set
          |> Variable.Set.is_empty
        in

        if Variable.Set.is_empty fns then
          (* warp-uniform  -> exact*)
          (ctx, Divergence.Uniform, Metrics.condition Divergence.Uniform)
        else
          (* warp-divergent -*)
          let (approxi, divergence): t * Divergence.t = (if only_tid_in_locals then
             (* exact *)
            ({ ctx with divergence = Exp.b_and cond ctx.divergence }, Divergence.Uniform)
          else
            (* approximate *)
            (set_unexact_cond ctx, Divergence.Divergent))
          in
          (approxi, divergence, Metrics.condition divergence)

    let add_if (cond:Exp.bexp) (ctx:t) : t * t * Divergence.t * Metrics.t =
      let (ctx1, div, metrics) = add_condition cond ctx in
      let (ctx2, _, _) = add_condition (Exp.b_not cond) ctx in
      (ctx1, ctx2, div, metrics)

    let add_range (uniform_loop:Range.t -> Range.t option) (range:Range.t) (ctx:t) : (Range.t * Divergence.t * t) option =
      let free_locals =
        Range.free_names range Variable.Set.empty
        |> Variable.Set.inter ctx.locals
      in
      let only_tid_in_locals =
        Variable.Set.diff free_locals Variable.tid_set
        |> Variable.Set.is_empty
      in
      (* Warp-uniform loop *)
      if Variable.Set.is_empty free_locals then
        Some (range, Divergence.Uniform, ctx)
      (* Warp-divergent loop *)
      else if only_tid_in_locals then (
        (* get the first number *)
        let init = Range.first range in
        match uniform_loop range with
        | Some range ->
          let ctx =
            (*
              If the first element of the range has a tid, then
              the loop variable should be considered a thread-local.
              Otherwise, the loop variable can be considered
              thread-global.
            *)
            if Exp.n_exists Variable.is_tid init then
              add_local (Range.var range) ctx
            else
              ctx
          in
          (* In either case we must mark the loop as inexact *)
          Some (range, Divergence.Divergent, set_unexact_loop ctx)
        | None ->
          None
      ) else
        None

  end

  let from_kernel
    ?(unif_cond=UniformCond.Exact)
    ?(strategy=Analysis_strategy.OverApproximation)
    (m:Metric.t)
    (cfg:Config.t)
    (k:Kernel.t)
  :
    (Ra.Stmt.t * Approx.t * Metrics.t, string) Result.t
  =
    let ( let* ) = Result.bind in
    let if_ : Exp.bexp -> Ra.Stmt.t -> Ra.Stmt.t -> Ra.Stmt.t =
      match unif_cond with
      | Exact -> Ra.Stmt.Opt.if_
      | Approximate -> fun _ -> Ra.Stmt.Opt.choice
    in
    let lin = L.linearize cfg k.arrays in
    let params = k.global_variables in
    let idx_analysis = I.run m cfg ~strategy in
    let uniform_loop = R.uniform (to_optimize strategy) params cfg.block_dim in
    let rec from_p (ctx:Context.t) :
      Code.t -> (Ra.Stmt.t * Approx.t * Metrics.t, string) Result.t
    =
      let open Ra.Stmt in
      function
      | Skip -> Ok (Skip, Approx.exact, Metrics.empty)
      | Seq (p, q) ->
        let* (p, approx1, metric1) = from_p ctx p in
        let* (q, approx2, metric2) = from_p ctx q in
        Ok (Seq (p, q), Approx.add approx1 approx2, Metrics.add metric1 metric2)
      | Access {array=x; index=l; _} ->
        Ok (
          l
          |> lin x (* Returns None when the array is being ignored *)
          |> Option.map (fun index ->
              let cost =
                idx_analysis
                  ~locals:ctx.locals
                  ~index
                  ~divergence:ctx.divergence
              in
              (
                cost.code,
                Approx.set_exact_index cost.exact ctx.approx,
                { Metrics.empty with index=Metrics.count_as cost.exact}
              )
            )
            (* When the array is ignored, return Skip *)
          |> Option.value ~default:(Ra.Stmt.Skip, Approx.exact, Metrics.empty)
        )
      | Sync _ -> Ok (Skip, Approx.exact, Metrics.empty)
      | Decl {body=p; var; _} ->
        from_p (Context.add_local var ctx) p
      | If (b, p, q) ->
        let (ctx1, ctx2, div, metrics) = Context.add_if b ctx in
        let* (p, approx1, metric1) = from_p ctx1 p in
        let* (q, approx2, metric2) = from_p ctx2 q in
        let code =
          match div with
          | Uniform -> if_ b p q
          | Divergent -> Seq (p, q)
        in
        Ok (code, Approx.add approx1 approx2, Metrics.add metrics (Metrics.add metric1 metric2))
      | Loop {range; body} ->
        (match Context.add_range uniform_loop range ctx with
         | Some (range, div, ctx) ->
           let* (body, approx, metric) = from_p ctx body in
           Ok (Loop {range; body;},
               approx,
               Metrics.add (Metrics.loop div) metric)
         | None ->
           let* (body, _, metric) = from_p ctx body in
          if Ra.Stmt.is_zero body then
            Ok (Skip, Approx.exact, Metrics.add (Metrics.loop Divergence.Uniform) metric)
          else
            (* Finally, we get to a point where the loop bounds are
                thread-local and we know nothing about them. *)
            Error ("Unsupported loop range: " ^ Range.to_string range)
        )
    in
    let ctx =
      Params.to_set k.local_variables
      |> Variable.Set.union Variable.tid_set
      |> Context.make
    in
    k.code
    |> Code.subst_block_dim cfg.block_dim
    |> Code.subst_grid_dim cfg.grid_dim
    |> from_p ctx
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
