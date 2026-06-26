let candidates ~globals:_ ~size_params expr =
  match Shape.dims size_params with
  | None -> Seq.empty
  | Some ds ->
    let is = Shape.accesses ds expr in
    let dims_e = List.map (fun t -> Poly.of_list [t]) ds in
    Seq.return
      { Index.indices = is;
        dims = dims_e;
        conditions = [];
      }
