open Stage0
open Protocols

let pretty_location (x : Variable.t) : string =
  x |> Variable.location_opt
  |> Option.map (fun x ->
      Location.to_string ~f:(fun x -> Fpath.(v x |> basename)) x ^ ": ")
  |> Option.value ~default:""

module UniformCond = struct
  type t = Exact | Approximate
end

module Accuracy = struct
  type t = Exact | Approximate
end

module Counter : sig
  type t

  val make : exact:int -> approximate:int -> t
  val total : t -> int
  val approximate : t -> int
  val exact : t -> int
  val empty : t
  val from_accuracy : Accuracy.t -> t
  val add : t -> t -> t
  val to_string : t -> string
end = struct
  type t = { approximate : int; exact : int }

  let make ~exact ~approximate = { approximate; exact }
  let empty = { approximate = 0; exact = 0 }
  let exact (e : t) = e.exact
  let approximate (e : t) = e.approximate
  let total (e : t) = e.exact + e.approximate

  let from_accuracy : Accuracy.t -> t = function
    | Exact -> { approximate = 0; exact = 1 }
    | Approximate -> { approximate = 1; exact = 0 }

  let add (l : t) (r : t) : t =
    { approximate = l.approximate + r.approximate; exact = l.exact + r.exact }

  let to_string (e : t) : string =
    Printf.sprintf "{exact=%d, approximate=%d}" e.exact e.approximate
end

module Stats : sig
  type t

  val empty : t
  val add : t -> t -> t

  val make :
    ?conditions:Counter.t -> ?loops:Counter.t -> ?indices:Counter.t -> unit -> t

  val make_condition : Accuracy.t -> t
  val make_loop : Accuracy.t -> t
  val make_index : Accuracy.t -> t
  val to_string : t -> string
  val loops : t -> Counter.t
  val conditions : t -> Counter.t
  val indices : t -> Counter.t
end = struct
  type t = { indices : Counter.t; loops : Counter.t; conditions : Counter.t }

  let make ?(conditions = Counter.empty) ?(loops = Counter.empty)
      ?(indices = Counter.empty) () =
    { conditions; loops; indices }

  let empty : t =
    {
      indices = Counter.empty;
      loops = Counter.empty;
      conditions = Counter.empty;
    }

  let make_loop (d : Accuracy.t) =
    { empty with loops = Counter.from_accuracy d }

  let make_index (d : Accuracy.t) =
    { empty with indices = Counter.from_accuracy d }

  let make_condition (d : Accuracy.t) =
    { empty with conditions = Counter.from_accuracy d }

  let add (l : t) (r : t) : t =
    {
      indices = Counter.add l.indices r.indices;
      loops = Counter.add l.loops r.loops;
      conditions = Counter.add l.conditions r.conditions;
    }

  let loops (e : t) = e.loops
  let indices (e : t) = e.indices
  let conditions (e : t) = e.conditions

  let to_string (a : t) : string =
    Printf.sprintf "{index=%s, loop=%s, cond=%s}"
      (Counter.to_string a.indices)
      (Counter.to_string a.loops)
      (Counter.to_string a.conditions)
end

let to_optimize : Analysis_strategy.t -> Uniform_range.t = function
  | OverApproximation -> Uniform_range.Maximize
  | UnderApproximation -> Uniform_range.Minimize

(* Module takes a boolean expression and extracts its sub-components in terms
    of uniform, non-uniform exact, and non-uniform approximate. *)
module UnifAnalysis : sig
  type t

  (* Given a set of thread-locals and a boolean expression, analyze its
      sub-components. *)
  val from_bexp : locals:Variable.Set.t -> Exp.bexp -> t

  (* Count how many warp-uniform sub-expressions were found *)
  val to_counter : t -> Counter.t

  (* Merge all non-uniform as a single expression *)
  val unif : t -> Exp.bexp option

  (* Merge all exact warp-non-uniform as a single expression *)
  val exact_non_unif : t -> Exp.bexp option

  (* Merge all approximate warp-non-uniform as a single expression *)
  val approx_non_unif : t -> Exp.bexp option

  (* Print the internal state of the uniform-analysis *)
  val to_string : t -> string
end = struct
  type t = {
    unif : Exp.bexp list;
    exact_non_unif : Exp.bexp list;
    approx_non_unif : Exp.bexp list;
  }

  let add (x : t) (y : t) : t =
    {
      unif = x.unif @ y.unif;
      exact_non_unif = x.exact_non_unif @ y.exact_non_unif;
      approx_non_unif = x.approx_non_unif @ y.approx_non_unif;
    }

  let from_bexp ~locals : Exp.bexp -> t =
    let rec from_bexp : Exp.bexp -> t = function
      | BRel (BAnd, cond1, cond2) ->
          let x = from_bexp cond1 in
          let y = from_bexp cond2 in
          add x y
      | cond ->
          let fns =
            Variable.Set.inter (Exp.b_free_names cond Variable.Set.empty) locals
          in
          if Variable.Set.is_empty fns then
            (* warp-uniform *)
            { unif = [ cond ]; approx_non_unif = []; exact_non_unif = [] }
          else
            let only_tid_in_locals =
              Variable.Set.diff fns Variable.tid_set |> Variable.Set.is_empty
            in
            (* warp-divergent, exact *)
            if only_tid_in_locals then
              { unif = []; approx_non_unif = []; exact_non_unif = [ cond ] }
            else { unif = []; approx_non_unif = [ cond ]; exact_non_unif = [] }
    in
    from_bexp

  let get (l : Exp.bexp list) : Exp.bexp option =
    if l = [] then None else Some (Exp.b_and_ex l)

  let unif (x : t) : Exp.bexp option = get x.unif
  let exact_non_unif (x : t) : Exp.bexp option = get x.exact_non_unif
  let approx_non_unif (x : t) : Exp.bexp option = get x.approx_non_unif

  let to_counter { unif = u1; exact_non_unif = u2; approx_non_unif = a } =
    Counter.make
      ~exact:(List.length u1 + List.length u2)
      ~approximate:(List.length a)

  let to_string (e : t) =
    let to_s b : string =
      if b = [] then "none" else Exp.b_and_ex b |> Exp.b_to_string
    in
    Printf.sprintf "{unif=%s; exact_div=%s; approx_div=%s}" (to_s e.unif)
      (to_s e.exact_non_unif) (to_s e.approx_non_unif)
end

module Make (LOG : Logger.Logger) = struct
  module M = Metric_analysis.Make (LOG)
  module L = Linearize_index.Make (LOG)

  module Context = struct
    type t = { divergence : Exp.bexp; locals : Variable.Set.t }

    let make (locals : Variable.Set.t) : t = { divergence = Bool true; locals }

    let add_local (var : Variable.t) (ctx : t) : t =
      { ctx with locals = Variable.Set.add var ctx.locals }

    let add_condition (cond : Exp.bexp) (ctx : t) : t =
      { ctx with divergence = Exp.b_and cond ctx.divergence }

    let add_if (cond : Exp.bexp) (ctx : t) : Exp.bexp option * t * t * Stats.t =
      let unif = UnifAnalysis.from_bexp ~locals:ctx.locals cond in
      (match UnifAnalysis.approx_non_unif unif with
      | Some b ->
          LOG.info (fun () ->
            Printf.sprintf "RA: approximate conditional: %s"
               (Exp.b_to_string b))
      | None -> ());
      let stats = Stats.make ~conditions:(UnifAnalysis.to_counter unif) () in
      let ctx1, ctx2 =
        match UnifAnalysis.exact_non_unif unif with
        | Some b -> (add_condition b ctx, add_condition (Exp.b_not b) ctx)
        | None -> (ctx, ctx)
      in
      (UnifAnalysis.unif unif, ctx1, ctx2, stats)

    let add_range (uniform_loop : Range.t -> Range.t option) (range : Range.t)
        (ctx : t) : (Range.t * Accuracy.t * t, string) Result.t =
      let locals =
        Range.free_names range Variable.Set.empty
        |> Variable.Set.inter ctx.locals
      in
      let non_tid_locals = Variable.Set.diff locals Variable.tid_set in
      (* Warp-uniform loop *)
      if Variable.Set.is_empty locals then Ok (range, Accuracy.Exact, ctx)
        (* Warp-divergent loop *)
      else if non_tid_locals |> Variable.Set.is_empty then
        (* get the first number *)
        let init = Range.first range in
        match uniform_loop range with
        | Some new_range ->
            let loc = range |> Range.var |> pretty_location in
            LOG.info (fun () ->
              loc ^ "RA: approximating range: " ^ "for ("
             ^ Range.to_string range ^ ") 🡆 " ^ "for ("
             ^ Range.to_string new_range ^ ")");
            let ctx =
              (*
              If the first element of the range has a tid, then
              the loop variable should be considered a thread-local.
              Otherwise, the loop variable can be considered
              thread-global.
            *)
              if Exp.n_exists Variable.is_tid init then
                add_local (Range.var new_range) ctx
              else ctx
            in
            (* In either case we must mark the loop as inexact *)
            Ok (new_range, Accuracy.Approximate, ctx)
        | None -> Error "Unabled to solve range"
      else
        Error
          (Printf.sprintf "Range has thread-locals: {%s}"
             (Variable.set_to_string non_tid_locals))
  end

  let from_kernel ?(unif_cond = UniformCond.Exact)
      ?(strategy = Analysis_strategy.OverApproximation) (m : Metric.t)
      (cfg : Config.t) (k : Kernel.t) : (Ra.Stmt.t * Stats.t, string) Result.t =
    let ( let* ) = Result.bind in
    let if_ : Exp.bexp -> Ra.Stmt.t -> Ra.Stmt.t -> Ra.Stmt.t =
      match unif_cond with
      | Exact -> Ra.Stmt.Opt.if_
      | Approximate -> fun _ -> Ra.Stmt.Opt.choice
    in
    let lin = L.linearize cfg k.arrays in
    let params = k.global_variables in
    let idx_analysis = M.run m cfg ~strategy in
    let uniform_loop =
      Uniform_range.uniform (to_optimize strategy) params cfg.block_dim
    in
    let rec from_p (ctx : Context.t) :
        Code.t -> (Ra.Stmt.t * Stats.t, string) Result.t =
      let open Ra.Stmt in
      function
      | Skip -> Ok (Skip, Stats.empty)
      | Seq (p, q) ->
          let* p, metric1 = from_p ctx p in
          let* q, metric2 = from_p ctx q in
          Ok (Seq (p, q), Stats.add metric1 metric2)
      | Access a ->
          let* a = lin a in
          let* index =
            match a.index with
            | [ n ] -> Ok n
            | _ ->
                Error
                  (Printf.sprintf "Cannot analyze multi-dimensional access: %s"
                     (Access.to_string a))
          in
          let cost =
            idx_analysis ~locals:ctx.locals ~index ~divergence:ctx.divergence
              ~verbose:false
          in
          Ok (cost.code, Stats.make_index Accuracy.Exact)
      | Sync _ -> Ok (Skip, Stats.empty)
      | Decl { body = p; var; _ } -> from_p (Context.add_local var ctx) p
      | If (b, p, q) ->
          let b, ctx1, ctx2, metrics = Context.add_if b ctx in
          let* p, metric1 = from_p ctx1 p in
          let* q, metric2 = from_p ctx2 q in
          let code = match b with Some b -> if_ b p q | None -> Seq (p, q) in
          Ok (code, Stats.add metrics (Stats.add metric1 metric2))
      | Loop { range; body } -> (
          match Context.add_range uniform_loop range ctx with
          | Ok (range, accu, ctx) ->
              let* body, metric = from_p ctx body in
              Ok (Loop { range; body }, Stats.add (Stats.make_loop accu) metric)
          | Error reason ->
              let* body, metric = from_p ctx body in
              if Ra.Stmt.is_zero body then
                Ok (Skip, Stats.add (Stats.make_loop Exact) metric)
              else
                (* Finally, we get to a point where the loop bounds are
                thread-local and we know nothing about them. *)
                Error
                  (Printf.sprintf "Unsupported loop range: %s: (%s)" reason
                     (Range.to_string range)))
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

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
