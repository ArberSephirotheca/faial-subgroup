let infer_dimensions ~globals:_ ~size_params _expr =
  Shape.dims size_params
  |> Option.map (fun ds ->
       Seq.return (List.map (fun m -> Poly.of_list [ m ]) ds))
  |> Option.value ~default:Seq.empty
