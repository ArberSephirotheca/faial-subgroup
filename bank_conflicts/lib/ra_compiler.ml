open Stage0
open Protocols
module UniformCond = struct
  type t = Exact | Approximate
end

module Accuracy = struct
  type t = Exact | Approximate
end

module Counter : sig
  type t
  val make: exact:int -> approximate:int -> t
  val total : t -> int
  val approximate : t -> int
  val exact:  t -> int
  val empty: t
  val from_accuracy : Accuracy.t -> t
  val add : t -> t -> t
  val to_string : t -> string
end = struct

  type t = { approximate: int; exact : int }

  let make ~exact ~approximate = { approximate; exact }

  let empty = { approximate = 0; exact = 0 }

  let exact (e:t) = e.exact

  let approximate (e:t) = e.approximate

  let total (e:t) = e.exact + e.approximate

  let from_accuracy : Accuracy.t -> t =
    function
    | Exact -> {approximate=0; exact=1}
    | Approximate -> {approximate=1; exact=0}

  let add (l:t) (r:t) : t =
    {
      approximate = l.approximate + r.approximate;
      exact = l.exact + r.exact;
    }

  let to_string (e:t) : string =
    Printf.sprintf
      "{exact=%d, approximate=%d}"
      e.exact
      e.approximate
end


module Stats : sig
  type t
  val empty : t
  val add: t -> t -> t
  val make_condition : Accuracy.t -> t
  val make_loop : Accuracy.t -> t
  val make_index : Accuracy.t -> t
  val to_string : t -> string
  val loops : t -> Counter.t
  val conditions : t -> Counter.t
  val indices : t -> Counter.t

end = struct

  type t = {indices: Counter.t; loops: Counter.t; conditions: Counter.t}

  let empty : t = {
    indices = Counter.empty;
    loops = Counter.empty;
    conditions = Counter.empty;
  }

  let make_loop (d:Accuracy.t) = { empty with loops=Counter.from_accuracy d }

  let make_index (d:Accuracy.t) = { empty with indices=Counter.from_accuracy d }

  let make_condition (d:Accuracy.t) = { empty with conditions=Counter.from_accuracy d }

  let add (l:t) (r:t) : t =
    {
      indices = Counter.add l.indices r.indices;
      loops = Counter.add l.loops r.loops;
      conditions = Counter.add l.conditions r.conditions;
    }

  let loops (e:t) = e.loops

  let indices (e:t) = e.indices

  let conditions (e:t) = e.conditions

  let to_string (a:t) : string =
    Printf.sprintf
      "{index=%s, loop=%s, cond=%s}"
      (Counter.to_string a.indices)
      (Counter.to_string a.loops)
      (Counter.to_string a.conditions)
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
      locals: Variable.Set.t;
    }

    let make (locals:Variable.Set.t) : t =
      { divergence = Bool true; locals}

    let add_local (var:Variable.t) (ctx:t) : t =
      { ctx with locals = Variable.Set.add var ctx.locals }

    let rec add_condition (cond:Exp.bexp) (ctx:t) : t * Divergence.t * Stats.t =
      (*
        When we find an and, we try to each sub-expression so that we can
        be as precise as possible and possibly miss some sub-conditions.
      *)
      match cond with
      | BRel (BAnd, cond1, cond2) ->
        let (ctx, div1, stats1) = add_condition cond1 ctx in
        let (ctx, div2, stats2) = add_condition cond2 ctx in
        (ctx, Divergence.add div1 div2, Stats.add stats1 stats2)
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
          (* warp-uniform *)
          (ctx, Divergence.Uniform, Stats.make_condition Accuracy.Exact)
        else if only_tid_in_locals then
          (* warp-divergent + exact *)
          let ctx = { ctx with divergence = Exp.b_and cond ctx.divergence } in
          (ctx, Divergence.Divergent, Stats.make_condition Exact)
        else
          (* warp-divergent + approx *)
          (ctx, Divergence.Divergent, Stats.make_condition Approximate)

    let add_if (cond:Exp.bexp) (ctx:t) : t * t * Divergence.t * Stats.t =
      let (ctx1, div, metrics) = add_condition cond ctx in
      let (ctx2, _, _) = add_condition (Exp.b_not cond) ctx in
      (ctx1, ctx2, div, metrics)

    let add_range
      (uniform_loop:Range.t -> Range.t option)
      (range:Range.t)
      (ctx:t)
    :
      (Range.t * Divergence.t * t) option
    =
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
          Some (range, Divergence.Divergent, ctx)
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
    (Ra.Stmt.t * Stats.t, string) Result.t
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
      Code.t -> (Ra.Stmt.t * Stats.t, string) Result.t
    =
      let open Ra.Stmt in
      function
      | Skip -> Ok (Skip, Stats.empty)
      | Seq (p, q) ->
        let* (p, metric1) = from_p ctx p in
        let* (q, metric2) = from_p ctx q in
        Ok (Seq (p, q), Stats.add metric1 metric2)
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
                Stats.make_index Accuracy.Exact
              )
            )
            (* When the array is ignored, return Skip *)
          |> Option.value ~default:(Ra.Stmt.Skip, Stats.empty)
        )
      | Sync _ -> Ok (Skip, Stats.empty)
      | Decl {body=p; var; _} ->
        from_p (Context.add_local var ctx) p
      | If (b, p, q) ->
        let (ctx1, ctx2, div, metrics) = Context.add_if b ctx in
        let* (p, metric1) = from_p ctx1 p in
        let* (q, metric2) = from_p ctx2 q in
        let code =
          match div with
          | Uniform -> if_ b p q
          | Divergent -> Seq (p, q)
        in
        Ok (code, Stats.add metrics (Stats.add metric1 metric2))
      | Loop {range; body} ->
        (match Context.add_range uniform_loop range ctx with
         | Some (range, div, ctx) ->
           let* (body, metric) = from_p ctx body in
           let accu : Accuracy.t =
            match div with
            | Uniform -> Exact
            | Divergent -> Approximate
           in
           Ok (Loop {range; body;},
               Stats.add (Stats.make_loop accu) metric)
         | None ->
           let* (body, metric) = from_p ctx body in
          if Ra.Stmt.is_zero body then
            Ok (Skip, Stats.add (Stats.make_loop Exact) metric)
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
