open Protocols

(* The BC cost decision: 5 rules applied over the per-axis labels
   produced by [Delinearize.from_exp]. Pure; the only science here
   is the cost model itself, independent of how the labels were
   obtained.

   Each rule's soundness witness is a named Rocq theorem:
   - all-uniform: [f_make_true_make] (broadcast collapses to one
     request, zero conflicts);
   - single BankBlind: [max_count] (every enabled thread can land in
     the same bank, so cost = tid_count - 1);
   - single Diverse g (full warp only): [bc_mul_tid_eq_gcd] for the
     bare-stride case, [bc_mul_tid_add_eq_gcd] for the offset
     variant, giving cost = g - 1;
   - the cap at [tid_count - 1] is [max_count] / [f_le_dim_size]. *)

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
  (* [Duality.bc_mul_tid_eq_gcd] is stated for [BMap.make true] (full
     warp). For divergent masks the closed form [bc = gcd(k, n)] no
     longer matches the per-bank multiplicity in general: a
     non-contiguous mask can concentrate enabled threads into one
     equivalence class mod [n/g], pushing bc above [⌊T*g/n⌋]. The
     full-warp gate keeps the Exact claim tied to the stride closed
     form; divergent warps fall through to simulation. *)
  let full_warp = tid_count = config.threads_per_warp in
  (* Cap any computed cost at [tid_count - 1] per [max_count]/
     [f_le_dim_size]. Redundant against [bank_count] and the closed
     form for full warps, but defensive and citation-aligned. *)
  let cap_cost v = max 0 (min v (tid_count - 1)) in
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
        | [ BankBlind ] -> exact (cap_cost (tid_count - 1))
        | [ Diverse sigma_hat ] when full_warp ->
            let g = gcd sigma_hat bank_count in
            exact (cap_cost (g - 1))
        | [ Diverse _ ] -> needs ()
        | [ NotInjective _ ] -> needs ()
        | _ -> needs ()
