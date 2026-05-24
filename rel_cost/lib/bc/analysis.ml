open Stage0
open Protocols

(* BC pipeline integration: glues the baseline pre-pass
   ([Normalize]) and simulation ([Simulate]) with the new
   delin-based contribution ([Delinearize] + [Oracle] + [Rules])
   under a single toggle.

   This is the only BC module that's a functor over [Logger]; the
   four log lines for the BC pipeline land here so the
   contribution modules stay pure and individually testable. *)

(* Modular-oracle solver timeout in milliseconds. Z3 BV queries on
   [stride mod n] are typically instant against a small preload; a
   short timeout keeps a pathological [kernel.pre] from stalling the
   analysis. *)
let oracle_timeout = 1000

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

  (* Run the simulation, logging the fall-through if it errors. *)
  let simulate (vec : Vectorized.t) (index : Exp.nexp) : Cost.t =
    match Simulate.run vec index with
    | Ok cost -> cost
    | Error msg ->
      L.info (fun () ->
        "BC: could not simulate cost " ^ Exp.n_to_string index ^ ": " ^ msg);
      Simulate.fallback_cost vec

  let strip (ctx : Analysis_ctx.t) : Exp.nexp =
    let after = Normalize.strip ctx.config ctx.locals ctx.index in
    if ctx.index <> after then
      L.info (fun () ->
        "BC: removed offset: " ^ Exp.n_to_string ctx.index ^ " 🡆 "
       ^ Exp.n_to_string after);
    after

  (* Delin pipeline: normalize → delinearize → rules. Returns a
     [Rules.bc_outcome]. The all-uniform short-circuit ([Num 0] after
     strip) bypasses delin and returns [Exact 0] directly. *)
  let bc_preprocess ?(oracle : Oracle.t option) (ctx : Analysis_ctx.t)
      : Rules.bc_outcome =
    let stripped = strip ctx in
    match stripped with
    | Num 0 -> Rules.Exact (Cost.from_int ~value:0 ~exact:true ())
    | _ ->
      let d =
        Delinearize.from_exp ?oracle ~config:ctx.config
          ~locals:ctx.locals stripped
      in
      let vec = to_vectorized ctx in
      Rules.decide ~config:ctx.config
        ~tid_count:(Vectorized.tid_count vec) d

  (* Toggle dispatch. When [delin_bc] is false (default), runs the
     baseline pipeline (normalize → simulate) verbatim. When true,
     runs the delin pipeline (normalize → delinearize → rules) with
     a simulation fallback for [NeedsSimulation] outcomes. *)
  let run_bc ~(delin_bc : bool) (ctx : Analysis_ctx.t) : Index_cost.t =
    let vec = to_vectorized ctx in
    if delin_bc then
      let with_oracle f =
        if ctx.pre = Exp.Bool true then f None
        else
          Oracle.with_slot ~timeout:oracle_timeout ctx.pre
            (fun o -> f (Some o))
      in
      let outcome = with_oracle (fun oracle -> bc_preprocess ?oracle ctx) in
      match outcome with
      | Rules.Exact cost ->
        L.info (fun () ->
          "BC: delin Exact: " ^ Exp.n_to_string ctx.index ^ " 🡆 "
         ^ Cost.to_string cost);
        Index_cost.from_cost cost
      | Rules.NeedsSimulation index ->
        L.info (fun () ->
          "BC: delin NeedsSimulation: " ^ Exp.n_to_string ctx.index ^ " 🡆 "
         ^ Exp.n_to_string index);
        simulate vec index |> Index_cost.from_cost
    else
      let stripped = strip ctx in
      simulate vec stripped |> Index_cost.from_cost
end
