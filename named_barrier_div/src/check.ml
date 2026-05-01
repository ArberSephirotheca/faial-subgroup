(* Top-level driver for NBD analysis.

   Builds the initial Σ from a [Kernel.t] (globals → Unif, locals →
   Local), constructs the initial task with [pi = true], [delta = ⊤],
   runs tier 1 to drain the live partition, then tier 2 to discharge
   the parked partition. The kernel's pre stays separate from δ — it
   appears in the bd antecedent as a side condition on σ. *)

open Protocols

(* Build the initial Σ from a kernel:
   - thread-global parameters are Unif (uniform across threads).
   - thread-local parameters (tid, registers) are Local. *)
let initial_sigma (k : Kernel.t) : Sigma.t =
  let sigma =
    Variable.Set.fold
      (fun x s -> Sigma.add x Modifier.Unif s)
      (Kernel.global_set k) Sigma.empty
  in
  Variable.Set.fold
    (fun x s -> Sigma.add x Modifier.Local s)
    (Kernel.local_set k) sigma

(* Build the initial task: pi = true, delta = true, residual = code.
   Pre is threaded through bd discharge as a side condition. *)
let initial_task (k : Kernel.t) : Task.t =
  Task.make ~sigma:(initial_sigma k) k.code

(* Run tier 1 + tier 2 on a kernel. Returns the diagnostic list;
   empty list means the kernel is NBD-safe. *)
let run ?(timeout = 0) (k : Kernel.t) : Tier2.diagnostic list =
  let t = initial_task k in
  let state = Tier1.reduce t in
  (* Tier 1's invariant: the live partition is fully drained. *)
  assert (state.live = []);
  Tier2.check ~timeout ~pre:k.pre (initial_sigma k) state.parked

let is_safe ?timeout (k : Kernel.t) : bool = run ?timeout k = []
