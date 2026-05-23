let candidates ~globals:_ ~size_params expr =
  match Polynomial.dims size_params with
  | None -> Seq.empty
  | Some ds ->
    let is = Polynomial.accesses ds expr in
    let dims_e = List.map (fun t -> Expr.of_list [t]) ds in
    Seq.return
      { Index.indices = is;
        dims = dims_e;
        conditions = [];
      }
