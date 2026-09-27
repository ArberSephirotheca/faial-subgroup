open Protocols

(* Pre-existing rel_cost cleanup: additive-offset stripping.

   Given a memory-access index expression, classify every subterm by
   whether it depends on a warp-divergent name and drop the
   warp-uniform additive parts. The remaining expression is the
   warp-varying core that determines the bank-conflict pattern.

   Pure module (no functor): the caller logs the before/after if
   desired. *)

module BC = struct
  type t = Uniform | Any

  let bin : N_binary.t -> Exp.nexp * t -> Exp.nexp * t -> Exp.nexp * t =
   fun o (e1, x1) (e2, x2) ->
    let both : Exp.nexp = Binary (o, e1, e2) in
    match (o, x1, x2) with
    | (Plus _ | Minus _), Any, Uniform -> (e1, Any)
    | (Plus _ | Minus _), Uniform, Any -> (e2, Any)
    | _, Uniform, Uniform -> (both, Uniform)
    | _, _, _ -> (both, Any)

  let map (f : Exp.nexp -> Exp.nexp) ((e, x) : Exp.nexp * t) : Exp.nexp * t =
    (f e, x)

  let from_nexp (cfg : Config.t) (locals : Variable.Set.t) :
      Exp.nexp -> Exp.nexp * t =
    let locals = Variable.Set.union locals Variable.tid_set in
    let rec from_nexp : Exp.nexp -> Exp.nexp * t = function
      | Num n -> (Num n, Uniform)
      | Var x ->
          let r =
            if Config.is_warp_uniform x cfg then Uniform
            else if Variable.Set.mem x locals then Any
            else Uniform
          in
          (Var x, r)
      | Unary (o, e) -> map (fun e -> Unary (o, e)) (from_nexp e)
      | Binary (o, e1, e2) -> bin o (from_nexp e1) (from_nexp e2)
      | NCall (f, args) ->
          let args_t = List.map from_nexp args in
          let r =
            List.fold_left
              (fun acc (_, t) -> if acc = Any || t = Any then Any else Uniform)
              Uniform args_t
          in
          (NCall (f, List.map fst args_t), r)
      | ReadResult rd ->
          let args_t = List.map from_nexp rd.args in
          let r =
            List.fold_left
              (fun acc (_, t) -> if acc = Any || t = Any then Any else Uniform)
              Uniform args_t
          in
          (ReadResult { rd with args = List.map fst args_t }, r)
      (* Erased, as in [Ua_analysis] and [Reals.from_nexp]. *)
      | Convert c -> from_nexp c.arg
      | CastInt e ->
          let r = if Exp.b_intersects locals e then Any else Uniform in
          (CastInt e, r)
      | NIf (c, e1, e2) ->
          if Exp.b_intersects locals c then (NIf (c, e1, e2), Any)
          else
            let e1, r1 = from_nexp e1 in
            let e2, r2 = from_nexp e2 in
            let r = if r1 = r2 then r1 else Any in
            (NIf (c, e1, e2), r)
    in
    from_nexp

  let to_string : Exp.nexp * t -> string =
   fun (e, x) ->
    let prefix = match x with Any -> "any" | Uniform -> "unif" in
    Exp.n_to_string e ^ ": " ^ prefix
end

(* Strip warp-uniform additive offsets. Returns [Num 0] when the entire
   expression classifies as Uniform (the whole access is warp-uniform
   and contributes no bank diversity); otherwise returns the warp-varying
   core. *)
let strip (cfg : Config.t) (locals : Variable.Set.t) (index : Exp.nexp)
    : Exp.nexp =
  match BC.from_nexp cfg locals index with
  | _, Uniform -> Num 0
  | e, Any -> e
