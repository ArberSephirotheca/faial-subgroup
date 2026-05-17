open Protocols

type t = {
  target : Variable.t;
  ty : C_type.t;
  atomic : Atomic.t;
  array : Variable.t;
  index : Exp.nexp list;
  (* The additive amount for atomicAdd / atomicSub (and scoped
     variants), threaded through so the [Scoped.imp_to_scoped] step
     can decide whether to emit a thread-distinctness [pre] on the
     atomic target's [Decl] under the unique-return contract.
     [None] for non-Add-family atomics. *)
  increment : Exp.nexp option;
}

let to_access (a : t) : Access.t =
  Access.{ array = a.array; index = a.index; mode = Atomic a.atomic }
