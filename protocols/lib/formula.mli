(* A solver query: the hypotheses it is asked under, and the goal it
   asks about. Both are [bexp], and keeping them apart is what makes
   the two questions differ correctly. Satisfiability asks about
   [facts && goal] and validity about [facts && not goal], so the
   negation reaches the goal and never the hypotheses.

   The type is abstract because [to_bexp] is doing more than joining
   the two halves: it inlines predicate definitions, replaces the
   cross-thread primitives, and instantiates every declaration
   governing a symbol that survives those steps. A [bexp] handed
   straight to the encoder would have had none of that done to it,
   which is why the encoder takes a [t]. *)

open Exp

type t

(* A query with no hypotheses yet. *)
val make : bexp -> t

(* Add a hypothesis: a kernel precondition, the runtime bounds on its
   parameters, or an axiom the analysis contributes. *)
val assume : bexp -> t -> t

val facts : t -> bexp
val goal : t -> bexp

(* Rewrite the goal, leaving the hypotheses alone. *)
val map_goal : (bexp -> bexp) -> t -> t

(* Ask validity instead of satisfiability. *)
val negate_goal : t -> t

(* The query as one encoder-ready [bexp]: predicates inlined,
   cross-thread primitives replaced, and every governing declaration
   instantiated at the applications that survive. *)
val to_bexp : t -> bexp

val free_names : t -> Variable.Set.t -> Variable.Set.t
