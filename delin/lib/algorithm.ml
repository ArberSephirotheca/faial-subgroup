open Protocols

module type S = sig
  val candidates :
    globals:Variable.Set.t ->
    size_params:Mono.t list ->
    Poly.t ->
    Subscript.t Seq.t
end

module type Infer = sig
  val infer_dimensions :
    globals:Variable.Set.t ->
    size_params:Mono.t list ->
    Poly.t ->
    Poly.t list Seq.t
end

module type Decompose = sig
  val delinearize : radix:Poly.t list -> Poly.t -> Poly.t list option
end

module Driver (I : Infer) (D : Decompose) : S = struct
  let candidates ~globals ~size_params expr =
    I.infer_dimensions ~globals ~size_params expr
    |> Seq.filter_map (fun radix ->
         D.delinearize ~radix expr
         |> Option.map (fun numeral ->
              { Subscript.numeral; radix }))
end

module Chain (A : Infer) (B : Infer) : Infer = struct
  let infer_dimensions ~globals ~size_params expr =
    Seq.append
      (A.infer_dimensions ~globals ~size_params expr)
      (B.infer_dimensions ~globals ~size_params expr)
end
