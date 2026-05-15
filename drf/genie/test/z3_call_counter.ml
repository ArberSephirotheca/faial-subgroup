(* Test interposer that counts Z3 satisfiability queries issued by
   [Reachability.check_kernel]. The implementation swaps the hook in
   [Reachability.z3_call_hook] for the duration of [with_counter] and
   restores it on exit (or on exception), so tests sharing the
   process can't leak counter state.

   The counter advances once per equivalence-class query, not once
   per access. That is the precise metric Phase 2's optimisation
   targets: kernels whose accesses share path conditions, or whose
   path conditions are parameter-free, must produce strictly fewer
   counter ticks than the access count. *)

open Drf_genie

let with_counter (f : unit -> 'a) : int * 'a =
  let count = ref 0 in
  let prev = !Reachability.z3_call_hook in
  Reachability.z3_call_hook := (fun () -> incr count);
  let restore () = Reachability.z3_call_hook := prev in
  match f () with
  | result -> restore (); (!count, result)
  | exception e -> restore (); raise e
