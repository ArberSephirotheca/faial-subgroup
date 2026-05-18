open Protocols

type t = {
  target : Variable.t;
  ty : C_type.t;
  atomic : Atomic.t;
  array : Variable.t;
  index : Exp.nexp list;
  (* The additive amount for atomicAdd / atomicSub (and scoped
     variants), threaded through so the [Scoped.imp_to_scoped] step
     can decide whether to emit a thread-distinctness assert on the
     atomic target under the unique-return contract.
     [None] for non-Add-family atomics. *)
  increment : Exp.nexp option;
  (* The comparison value for atomicCAS / scoped variants and
     WGSL's [atomicCompareExchangeWeak]. Threaded through so the
     [Scoped.imp_to_scoped] step can emit a winner-uniqueness
     assert (at most one thread per address sees [target ==
     expected]) on the atomic target's [Decl]. [None] for
     non-CAS atomics. *)
  expected : Exp.nexp option;
}

let to_access (a : t) : Access.t =
  Access.{ array = a.array; index = a.index; mode = Atomic a.atomic }
