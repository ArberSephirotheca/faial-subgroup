open Protocols

module type S = sig
  val candidates :
    globals:Variable.Set.t ->
    size_params:Mono.t list ->
    Poly.t ->
    Index.t Seq.t
end
