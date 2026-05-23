open Protocols

module type S = sig
  val candidates :
    globals:Variable.Set.t ->
    size_params:Term.t list ->
    Expr.t ->
    Index.t Seq.t
end
