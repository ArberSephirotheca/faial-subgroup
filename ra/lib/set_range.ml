open Protocols

type t = { var : Variable.t; lower_bound : Reals.t; upper_bound : Reals.t }

let count (s : t) : int option =
  match (s.lower_bound, s.upper_bound) with
  | Num lower_bound, Num upper_bound ->
      Some (max (1 + upper_bound - lower_bound) 0)
  | _, _ -> None

let make ~var ~lower_bound ~upper_bound : t = { var; lower_bound; upper_bound }

let to_string (b : t) : string =
  "{" ^ Variable.name b.var ^ " | "
  ^ Reals.to_string b.lower_bound
  ^ " ≤ " ^ Variable.name b.var ^ " ≤ "
  ^ Reals.to_string b.upper_bound
  ^ "}"

let subst (x : Variable.t * Reals.t) (s : t) : t =
  {
    s with
    lower_bound = Reals.i_subst x s.lower_bound;
    upper_bound = Reals.i_subst x s.upper_bound;
  }

let from_range : Range.t -> Reals.t option * t = function
  | { var; upper_bound; lower_bound; step = Plus (Num 1); _ } ->
      ( None,
        {
          var;
          lower_bound = Reals.from_nexp lower_bound;
          upper_bound = Reals.from_nexp upper_bound;
        } )
  | { var; step = Plus step; dir; lower_bound; upper_bound; _ } ->
      let open Exp in
      let x = Var var in
      (*
                  ub - lb
      last_elem = -------
                    k
    *)
      let new_range_var =
        match dir with
        | Increase ->
            (* x := lb + step * x *)
            n_plus lower_bound (n_mult x step)
        | Decrease ->
            (* x := ub - (x * step) *)
            n_minus upper_bound (n_mult x step)
      in
      let new_range_var = Reals.from_nexp new_range_var in
      let iters = n_div (n_minus upper_bound lower_bound) step in
      ( Some new_range_var,
        {
          var;
          lower_bound = Reals.from_int 0;
          upper_bound = Reals.from_nexp iters;
        } )
  | { var; step = Mult step; upper_bound; lower_bound; _ } ->
      (*
        log_k (ub - lb)

        For instance,
          for (x = 3; x < 100; x *= 2) ->
            3, 6, 12, 24, 48, 96 <- log2 (100 / 3)
            3*2^0, 3*2^1, 3*2^2, ...

          for (x = 25; x < 100; x *= 2) ->
            25, 50 <- log2 (100 / 3) = log2(100) - log2(3)
            25*2^0, 25*2^1
      *)
      let step = Reals.from_nexp step in
      let i_lower_bound = Reals.from_nexp lower_bound in
      let i_upper_bound = Reals.from_nexp upper_bound in
      let f_lower_bound = i_lower_bound |> Reals.int_to_float in
      let f_upper_bound = i_upper_bound |> Reals.int_to_float in
      let iters =
        (* we use subtraction of logs, rather than division of args of logs
          because ultimately we want to simplify the logs. *)
        Reals.minus
          (Reals.floor (Reals.log step f_upper_bound))
          (Reals.floor (Reals.log step f_lower_bound))
      in
      (* In summations we start with base 1; we decrement 1 so that we start in base 2^0 *)
      let new_range_var = Reals.mult i_lower_bound (Reals.pow step (Var var)) in
      (Some new_range_var, { var; lower_bound = Num 1; upper_bound = iters })
