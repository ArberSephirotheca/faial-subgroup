open Protocols

(* The BC cost decision: 5 rules from rel-cost-delin.md applied over
   the per-axis labels produced by [Delinearize.from_exp]. Pure; the
   only science here is the cost model itself, independent of how the
   labels were obtained. *)

type bc_outcome =
  | Exact of Cost.t
  | NeedsSimulation of Exp.nexp

let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

let decide ~(config : Config.t) ~(tid_count : int) (d : Delinearize.t)
    : bc_outcome =
  let needs () = NeedsSimulation d.reduced in
  let exact value =
    Exact (Cost.from_int ~value ~exact:true ())
  in
  let bank_count = config.bank_count in
  match d.axes with
  | [] ->
      (* delin returned no candidate; the analysis layer hands
         [d.reduced] to simulation. *)
      needs ()
  | classes ->
      let unknown_present =
        List.exists
          (function Delinearize.Unknown -> true | _ -> false) classes
      in
      let warp_varying =
        List.filter
          (function
            | Delinearize.BankBlind | Diverse _ | NotInjective _ -> true
            | Uniform | Unknown -> false)
          classes
      in
      if unknown_present then needs ()
      else
        match warp_varying with
        | [] -> exact 0
        | [ BankBlind ] -> exact (max (tid_count - 1) 0)
        | [ Diverse sigma_hat ] ->
            let g = gcd sigma_hat bank_count in
            exact (max ((tid_count * g / bank_count) - 1) 0)
        | [ NotInjective _ ] -> needs ()
        | _ -> needs ()
