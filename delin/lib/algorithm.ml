open Protocols

module type S = sig
  val candidates :
    globals:Variable.Set.t ->
    size_params:Mono.t list ->
    Poly.t ->
    Subscript.t Seq.t

  val delinearize : radix:Poly.t list -> Poly.t -> Subscript.t option

  val yields :
    globals:Variable.Set.t ->
    size_params:Mono.t list ->
    Poly.t list ->
    (Poly.t list * Subscript.t list) Seq.t
end

module type Infer = sig
  val infer_dimensions :
    globals:Variable.Set.t ->
    size_params:Mono.t list ->
    Poly.t list ->
    Poly.t list Seq.t
end

module type Decompose = sig
  val delinearize : radix:Poly.t list -> Poly.t -> Poly.t list option
end

module Driver (I : Infer) (D : Decompose) : S = struct
  let candidates ~globals ~size_params expr =
    I.infer_dimensions ~globals ~size_params [ expr ]
    |> Seq.filter_map (fun radix ->
         D.delinearize ~radix expr
         |> Option.map (fun numeral ->
              { Subscript.numeral; radix }))

  let delinearize ~radix expr =
    D.delinearize ~radix expr
    |> Option.map (fun numeral -> { Subscript.numeral; radix })

  let yields ~globals ~size_params accesses =
    I.infer_dimensions ~globals ~size_params accesses
    |> Seq.filter_map (fun radix ->
         let decoded = List.map (delinearize ~radix) accesses in
         if List.for_all Option.is_some decoded
         then Some (radix, List.filter_map Fun.id decoded)
         else None)
end

module Chain (A : Infer) (B : Infer) : Infer = struct
  let infer_dimensions ~globals ~size_params accesses =
    Seq.append
      (A.infer_dimensions ~globals ~size_params accesses)
      (B.infer_dimensions ~globals ~size_params accesses)
end
