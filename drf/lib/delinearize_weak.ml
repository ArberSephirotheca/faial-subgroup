open Protocols

let get_accesses (unsync : Unsynced.t) : Exp.nexp list list Variable.Map.t =
  let open Unsynced in
  let rec walk = function
    | Skip | Assert _ -> Fun.id
    | Access { array; index; _ } -> Variable.Map.add_to_list array index
    | Cond (_, u) -> walk u
    | Loop (_, u) -> walk u
    | Seq (u, v) -> Fun.compose (walk u) (walk v)
  in
  walk unsync Variable.Map.empty

let rewrite_unsync
    ~(globals : Variable.Set.t)
    ~(in_range : bool)
    ~(loop_scope : Exp.bexp list)
    ~(check : scope:Exp.bexp list -> bound:Exp.bexp -> bool)
    ~(rewrite_access : bool)
    (unsync : Unsynced.t) : Unsynced.t =
  let open Unsynced in
  let flat =
    get_accesses unsync
    |> Variable.Map.filter_map (fun _ accesses ->
         List.fold_left
           (fun acc index ->
             match index with
             | [ a ] -> Option.map (List.cons a) acc
             | _ -> None)
           (Some []) accesses)
  in
  let viable =
    flat
    |> Variable.Map.filter_map (fun _ indices ->
         let frame, bound = Pv_decomp.analyze ~globals ~in_range indices in
         if check ~scope:loop_scope ~bound then Some (frame, bound) else None)
  in
  let rec walk = function
    | Access ({ index = [ a ]; array; _ } as acc)
      when Variable.Map.mem array viable ->
      let frame, bound = Variable.Map.find array viable in
      let body =
        if rewrite_access
        then Access { acc with index = Pv_decomp.subscripts ~globals ~frame a }
        else Access acc
      in
      Seq (Assert bound, body)
    | (Access _ | Skip | Assert _) as code -> code
    | Cond (b, u) -> Cond (b, walk u)
    | Loop (r, u) -> Loop (r, walk u)
    | Seq (u, v) -> Seq (walk u, walk v)
  in
  walk unsync

let rec rewrite_aligned
    ~(globals : Variable.Set.t)
    ~(in_range : bool)
    ~(loop_scope : Exp.bexp list)
    ~(check : scope:Exp.bexp list -> bound:Exp.bexp -> bool)
    ~(rewrite_access : bool) : Aligned.Code.t -> Aligned.Code.t =
  let open Aligned.Code in
  function
  | Sync c -> Sync (rewrite_unsync ~globals ~in_range ~loop_scope ~check ~rewrite_access c)
  | Loop { range; body } ->
    let globals =
      if Variable.Set.subset (Range.free_names range Variable.Set.empty) globals
      then Variable.Set.add range.var globals
      else globals
    in
    let loop_scope = Range.to_cond range :: loop_scope in
    Loop
      { range; body = rewrite_aligned ~globals ~in_range ~loop_scope ~check ~rewrite_access body }
  | Seq (a, b) ->
    Seq
      ( rewrite_aligned ~globals ~in_range ~loop_scope ~check ~rewrite_access a,
        rewrite_aligned ~globals ~in_range ~loop_scope ~check ~rewrite_access b )

let rewrite_kernel
    ~(rewrite_access : bool)
    ~(in_range : bool)
    ~(check : scope:Exp.bexp list -> bound:Exp.bexp -> bool)
    (kernel : Aligned.Kernel.t) : Aligned.Kernel.t =
  let globals = Params.to_set kernel.global_variables in
  {
    kernel with
    code =
      rewrite_aligned ~globals ~in_range ~loop_scope:[] ~check ~rewrite_access
        kernel.code;
  }
