(* The kernel's verdict.

   [Drf] is set when every per-proof outcome is safe and no
   UNKNOWN remains. [Racy] is set when any per-proof outcome is
   [Racy]; UNKNOWN outcomes alongside do not downgrade the
   verdict (a real witness wins). [Timeout] is set when no proof
   refuted DRF but at least one per-proof Z3 query returned
   UNKNOWN (typically due to the per-query timeout); the kernel
   is neither proven DRF nor refuted, and the cap is the
   load-bearing reason. [Vacuous] is set when the kernel's
   merged precondition is UNSAT; the race pipeline is skipped
   because every race goal would inherit the contradiction.
   [Zero_accesses] is set when the kernel's protocol holds no
   memory access at all, so there is no pair of accesses for the
   race pipeline to compare and the kernel would otherwise clear
   as [Drf]. A GPU kernel that touches no memory is almost never
   what was written, so an access-free protocol points at accesses
   dropped during inference rather than at a race-free kernel.
   This is a distinct condition from [Vacuous]: the precondition
   is satisfiable, it is the code that is empty. *)
module Verdict = struct
  type t = Drf | Racy | Timeout | Vacuous | Zero_accesses

  let to_string : t -> string = function
    | Drf -> "drf"
    | Racy -> "racy"
    | Timeout -> "timeout"
    | Vacuous -> "vacuous"
    | Zero_accesses -> "zero-accesses"
end

type t = {
  kernel : Protocols.Kernel.t;
  report : Solve_drf.Solution.t list;
  (* [Some pre] when the kernel's merged precondition was UNSAT
     (the race pipeline was skipped); [pre] is the contradicting
     conjunction so renderers can show the user which clauses
     conflict. [None] in every other case. *)
  vacuous : Protocols.Exp.bexp option;
}

let is_safe (a : t) : bool = a.report |> List.for_all Solve_drf.Solution.is_safe

let verdict (a : t) : Verdict.t =
  match a.vacuous with
  | Some _ -> Verdict.Vacuous
  | None when not (Protocols.Kernel.has_accesses a.kernel) ->
    Verdict.Zero_accesses
  | None ->
    let has_race, has_unknown =
      List.fold_left
        (fun (race, unk) (s : Solve_drf.Solution.t) ->
          match s.outcome with
          | Solve_drf.Outcome.Racy _ -> (true, unk)
          | Solve_drf.Outcome.Unknown -> (race, true)
          | Solve_drf.Outcome.Drf | Solve_drf.Outcome.Drf_with_core _ ->
            (race, unk))
        (false, false) a.report
    in
    if has_race then Verdict.Racy
    else if has_unknown then Verdict.Timeout
    else Verdict.Drf
