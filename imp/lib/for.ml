open Protocols

let ( let* ) = Option.bind

type t = { init : Stmt.t; cond : Exp.bexp; inc : Stmt.t }

let to_string : t -> string = function
  | { init; cond; inc } ->
      let stmt (s : Stmt.t) : string =
        s |> Stmt.to_string
        |> Stage0.Common.replace ~substring:"\n" ~by:" "
        |> String.trim
      in
      Printf.sprintf "(%s; %s; %s)" (stmt init) (Exp.b_to_string cond)
        (stmt inc)

module Increment = struct
  type t = Plus | LeftShift | Mult | Minus | RightShift | Div

  let parse : N_binary.t -> t option = function
    | Minus _ -> Some Minus
    | Div _ -> Some Div
    | RightShift _ -> Some RightShift
    | Plus _ -> Some Plus
    | LeftShift -> Some LeftShift
    | Mult _ -> Some Mult
    | _ -> None
end

module Comparator = struct
  type t = Lt | Le | Gt | Ge | Neq

  let parse : N_rel.t -> t option = function
    | Lt _ -> Some Lt
    | Gt _ -> Some Gt
    | Le _ -> Some Le
    | Ge _ -> Some Ge
    | Neq -> Some Neq
    | Eq -> None
end

module Infer = struct
  type 'a unop = { var : Variable.t; op : 'a; arg : Exp.nexp }
  type for_ = t

  type t = {
    name : Variable.t;
    init : Exp.nexp option;
    pre_loop : Stmt.t;
    loop_guard : Exp.bexp;
    cond : Comparator.t unop;
    inc : Increment.t unop;
    (* Additive increments of other variables harvested out of the
       [inc] slot or from the body's top level. [extract_incs]
       prepends a [Decl i.var = i_inc * iters + i.var] shadow at
       the top of the body for each entry, so the body sees the
       iteration-start value while body-origin [Assign]s continue
       to mutate the shadow within the iteration. Body-origin
       entries are NOT stripped — the same trick already used for
       same-variable body increments via [needs_shadow]. *)
    other_incs : Increment.t unop list;
    (* Same-variable increments beyond the primary [inc]. These come from
       the body's top level (or from a multi-increment [inc] slot) and
       contribute to the loop's step. They are NOT extracted: body-origin
       ones stay where they are, inc-origin ones were already removed
       from the [inc] slot. *)
    extra_step : Increment.t unop list;
    (* True when at least one increment of [name] was harvested from the
       body's top level. The body is then prepended with [Decl name = name]
       so the body's writes target an iteration-local shadow rather than
       the For-loop's binding. *)
    needs_shadow : bool;
    post_body : Stmt.t;
  }

  let extract_incs (r : Range.t) (l : Increment.t unop list) : Stmt.t =
    let l : Decl.t list =
      match r.step with
      | Plus step ->
          l
          |> List.map (fun i ->
              let open Increment in
              match i.op with
              | Plus | Minus ->
                  let iters =
                    Exp.n_div (Exp.n_minus (Var r.var) r.lower_bound) step
                  in
                  let d =
                    (* i = i.inc * ((r.var - r.init) / r.inc) + i.init *)
                    let i_inc =
                      if i.op = Plus then i.arg else Exp.n_uminus i.arg
                    in
                    Decl.set i.var
                      (Exp.n_plus (Exp.n_mult i_inc iters) (Var i.var))
                  in
                  d
              | _ -> Decl.unset i.var)
      | Mult _ -> l |> List.map (fun i -> Decl.unset i.var)
    in
    l |> List.map Stmt.decl |> Stmt.from_list

  (** Final-value replacement for harvested affine increments: emit a
      post-[For] [Assign] of each variable's exit value so subsequent
      sequential code sees [i.var = i.var + i_inc * trip_count] via
      the substitution encoder (the standard SCEV /
      IndVarSimplify-style last-value substitution). Only fires when
      the loop's [step] is [Plus] and a closed-form trip count is
      computable via [Range.last]; otherwise the post-loop value is
      left unconstrained (matching the pre-existing behaviour for
      non-additive cases). *)
  let post_for_assigns (r : Range.t) (l : Increment.t unop list) : Stmt.t =
    match (r.step, Range.last r) with
    | Plus step, Some last ->
        let trip_count =
          Exp.n_plus
            (Exp.n_div (Exp.n_minus last r.lower_bound) step)
            (Num 1)
        in
        l
        |> List.filter_map (fun (i : Increment.t unop) ->
            let open Increment in
            match i.op with
            | Plus | Minus ->
                let i_inc =
                  if i.op = Plus then i.arg else Exp.n_uminus i.arg
                in
                let delta = Exp.n_mult i_inc trip_count in
                Some
                  (Stmt.assign Ty.int i.var
                     (Exp.n_plus (Var i.var) delta))
            | _ -> None)
        |> Stmt.from_list
    | _ -> Stmt.Skip

  (** Find the first init and return an alterated statement without the init
      found. *)
  let rec parse_init (x : Variable.t) : Stmt.t -> Exp.nexp option * Stmt.t =
    (* Search for a decl that is initialized; remove decl
       if found *)
    function
    | Decl { var; init; _ } when Variable.equal x var -> (init, Skip)
    | Assign { var; data; _ } when Variable.equal x var -> (Some data, Skip)
    | Seq (s1, s2) ->
        let d, s1 = parse_init x s1 in
        if Option.is_some d then
          (* Don't recurse to s2 *)
          (d, Stmt.seq s1 s2)
        else
          (* Not in s1, so recurse to s2 *)
          let d, s2 = parse_init x s2 in
          (d, Stmt.seq s1 s2)
    | s -> (None, s)

  let peel_shape (n : Exp.nexp) : Exp.nexp =
    match Exp.strip_convert n with
    | Binary (o, a, b) -> Binary (o, Exp.strip_convert a, Exp.strip_convert b)
    | n -> n

  let parse_cond (x : Variable.t) :
      Exp.bexp -> (Comparator.t unop * Exp.bexp) option =
    let ( let* ) = Option.bind in
    let rec parse ~accum : Exp.bexp -> (Comparator.t unop * Exp.bexp) option =
      function
      (* Not-equal is symmetric, so the loop variable may sit on either
         side: [i != n] and [n != i] are the same condition. *)
      | NRel (N_rel.Neq, lhs, rhs) -> (
          match parse_rel ~accum N_rel.Neq (peel_shape lhs) rhs with
          | Some _ as o -> o
          | None -> parse_rel ~accum N_rel.Neq (peel_shape rhs) lhs)
      | NRel (o, lhs, arg) -> parse_rel ~accum o (peel_shape lhs) arg
      | BRel (BAnd, e1, e2) -> (
          match parse ~accum:(Exp.b_and e2 accum) e1 with
          | Some x -> Some x
          | None -> parse ~accum:(Exp.b_and e1 accum) e2)
      | CastBool e -> (
          match peel_shape e with
          | Binary (Minus _, Var var, arg) ->
              Some ({ var; op = Neq; arg }, accum)
          | _ -> None)
      | _ -> None
    and parse_rel ~accum (o : N_rel.t) (lhs : Exp.nexp) (arg : Exp.nexp) :
        (Comparator.t unop * Exp.bexp) option =
      match lhs with
      (* x - e R arg ~~~> x R arg + e *)
      | Binary (Minus _, Var var, e) when Variable.equal var x ->
          let* op = Comparator.parse o in
          Some ({ var; op; arg = Exp.n_plus e arg }, accum)
      (* e + x R arg ~~~> x R arg - e *)
      | Binary (Plus _, e, Var var) when Variable.equal var x ->
          let* op = Comparator.parse o in
          Some ({ var; op; arg = Exp.n_minus arg e }, accum)
      (* x + e R arg ~~~> x R arg - e *)
      | Binary (Plus _, Var var, e) when Variable.equal var x ->
          let* op = Comparator.parse o in
          Some ({ var; op; arg = Exp.n_minus arg e }, accum)
      (* Default upper bound: x R o ~~~> x R o *)
      | Var var when Variable.equal var x ->
          let* op = Comparator.parse o in
          Some ({ var; op; arg }, accum)
      | _ -> None
    in
    parse ~accum:(Bool true)

  (* Match an Assign that has the shape of an increment, without modifying
     state. Used by both [parse_inc] (which removes them from the inc slot)
     and [parse_body_top_incs] (which leaves them in place). *)
  let match_inc (s : Stmt.t) : Increment.t unop option =
    let parse (var : Variable.t) (o : N_binary.t) (arg : Exp.nexp) :
        Increment.t unop option =
      o |> Increment.parse |> Option.map (fun op -> { var; op; arg })
    in
    match s with
    | Assign a -> (
        match peel_shape a.data with
        | Binary (o, Var l1, Var l2) ->
            if Variable.equal a.var l1 then parse a.var o (Var l2)
            else if Variable.equal a.var l2 then parse a.var o (Var l1)
            else None
        | Binary (o, r, Var l') | Binary (o, Var l', r) ->
            if Variable.equal a.var l' then parse a.var o r else None
        | _ -> None)
    | _ -> None

  (** Find every increment that is possible to find. The remainding statements
      should be kept in order. *)
  let parse_inc : Stmt.t -> Increment.t unop list * Stmt.t =
    let rec loop (accum : Increment.t unop list) (s : Stmt.t) :
        Stmt.t list -> Increment.t unop list * Stmt.t = function
      | [] -> (accum, s)
      | s1 :: l ->
          let s, accum =
            match match_inc s1 with
            | Some o -> (s, o :: accum)
            | None -> (Stmt.seq s1 s, accum)
          in
          loop accum s l
    in
    fun s -> loop [] Skip (Stmt.to_list s)

  (** Harvest increment-shaped assignments from the top level of the body.
      Does NOT modify the body — only collects matches. Nested statements
      (inside If, For, Star, etc.) are not visited; the scoped analysis
      handles them. *)
  let parse_body_top_incs (s : Stmt.t) : Increment.t unop list =
    Stmt.to_list s |> List.filter_map match_inc


  let parse ~(body : Stmt.t) (loop : for_) : t option =
    let inc_incs, inc_stmt = parse_inc loop.inc in
    let body_incs = parse_body_top_incs body in
    let tagged : (Increment.t unop * bool) list =
      (* bool = from_body *)
      List.map (fun i -> (i, false)) inc_incs
      @ List.map (fun i -> (i, true)) body_incs
    in
    let rec iter (skipped : (Increment.t unop * bool) list) :
        (Increment.t unop * bool) list -> t option = function
      | (inc, b) :: todo -> (
          let name = inc.var in
          (* Try to find a range from this increment: *)
          match
            let* cond, loop_guard = parse_cond name loop.cond in
            let init, pre_loop = parse_init name loop.init in
            let rest = skipped @ todo in
            (* Same-variable increments beyond the primary contribute to step. *)
            let extra_step =
              rest
              |> List.filter (fun (i, _) -> Variable.equal i.var name)
              |> List.map fst
            in
            (* Increments of other variables: inc-slot entries plus
               body-origin additive ones. Body-origin entries stay in
               place; [extract_incs] prepends a [Decl i.var = ...]
               shadow at the top of the body that takes the
               iteration-start value, and the body's [Assign]s
               continue to mutate the shadow within the iteration. *)
            let other_incs =
              rest
              |> List.filter (fun (i, _) ->
                     (not (Variable.equal i.var name))
                     && (match i.op with
                        | Increment.Plus | Minus -> true
                        | _ -> false))
              |> List.map fst
            in
            let needs_shadow =
              b
              || List.exists
                   (fun (i, from_body) ->
                     from_body && Variable.equal i.var name)
                   rest
            in
            Some
              {
                other_incs;
                extra_step;
                needs_shadow;
                post_body = Stmt.Skip;
                loop_guard;
                init;
                pre_loop;
                name;
                cond;
                inc;
              }
          with
          | Some _ as o ->
              (* This increment worked, return it *)
              o
          | None ->
              (* This increment didn't work, try again *)
              iter ((inc, b) :: skipped) todo)
      | [] -> None (* Failed inference *)
    in
    tagged
    (* Infer a range *)
    |> iter []
    (* And if we find it, add the non-increments to post_body *)
    |> Option.map (fun x ->
        { x with post_body = Stmt.seq x.post_body inc_stmt })

  (* Signed contribution of an additive increment. [Plus k] contributes
     +k, [Minus k] contributes -k. Returns None for non-additive ops. *)
  let signed_arg (i : Increment.t unop) : Exp.nexp option =
    match i.op with
    | Plus -> Some i.arg
    | Minus -> (
        match i.arg with Num n -> Some (Num (-n)) | a -> Some (Exp.n_uminus a))
    | _ -> None

  (* A relational condition fixes the direction on its own, but a not-equal
     one only names the value that stops the loop, never the side it is
     approached from, so there the direction has to be read off the total
     step. A step whose sign is not settled leaves the direction unknown and
     the loop is declined rather than guessed at. *)
  let direction (r : t) : Range.direction option =
    match r.cond.op with
    | Lt | Le -> Some Range.Increase
    | Ge | Gt -> Some Decrease
    | Neq -> (
        let total =
          List.fold_left
            (fun acc i ->
              let* acc = acc in
              let* s = signed_arg i in
              Some (Exp.n_plus acc s))
            (Some (Exp.Num 0))
            (r.inc :: r.extra_step)
        in
        match total with
        | Some (Num k) when k > 0 -> Some Increase
        | Some (Num k) when k < 0 -> Some Decrease
        | _ -> None)

  let infer_bounds (l : t) :
      (Exp.nexp * Exp.nexp * Range.direction) option =
    let init = Option.value ~default:(Exp.Var l.name) l.init in
    match l.cond with
    (* (int i = 0; i < 4; i++) *)
    | { op = Lt; arg = ub; _ } ->
        Some
          (init, Binary (Minus Signedness.Signed, ub, Num 1), Range.Increase)
    (* (int i = 0; i <= 4; i++) *)
    | { op = Le; arg = ub; _ } -> Some (init, ub, Increase)
    (* (int i = 4; i >= 0; i--) *)
    | { op = Ge; arg = lb; _ } -> Some (lb, init, Decrease)
    (* (int i = 4; i > 0; i--) *)
    | { op = Gt; arg = lb; _ } ->
        Some (Binary (Plus Signedness.Signed, Num 1, lb), init, Decrease)
    (* (int i = 0; i != n; i++), (int i = n; i != 0; i--) and the same
       three spellings written as a subtraction, (int i = 4; i - k; i++).
       The loop stops on reaching [bound], so [bound] itself is the first
       value the body does not see and the endpoint is the step before it. *)
    | { op = Neq; arg = bound; _ } -> (
        match direction l with
        | Some Increase -> Some (init, Exp.n_dec bound, Range.Increase)
        | Some Decrease -> Some (Exp.n_inc bound, init, Decrease)
        | None -> None)

  let infer_step (r : t) : Range.Step.t option =
    if r.extra_step = [] then
      (* No extras — original behavior. The arg is taken as-is and the
         direction is derived from the comparator by [infer_bounds]. *)
      match r.inc with
      | { op = Plus; arg = a; _ } | { op = Minus; arg = a; _ } ->
          Some (Range.Step.Plus a)
      | { op = Mult; arg = a; _ } | { op = Div; arg = a; _ } ->
          Some (Range.Step.Mult a)
      | { op = LeftShift; arg = Num a; _ }
      | { op = RightShift; arg = Num a; _ } ->
          Some (Range.Step.Mult (Num (Stage0.Common.pow ~base:2 a)))
      | _ -> None
    else
      (* Extras present: only additive ops compose. Sum signed args. *)
      let extras_additive =
        List.for_all
          (fun i ->
            match i.op with Increment.Plus | Minus -> true | _ -> false)
          r.extra_step
      in
      match r.inc with
      | { op = Plus | Minus; _ } when extras_additive -> (
          let* primary = signed_arg r.inc in
          let total =
            List.fold_left
              (fun acc i ->
                match signed_arg i with
                | Some s -> Exp.n_plus acc s
                | None -> acc)
              primary r.extra_step
          in
          match (total, direction r) with
          | Num 0, _ -> None
          | Num n, Some Range.Increase when n > 0 ->
              Some (Range.Step.Plus (Num n))
          | Num n, Some Decrease when n < 0 ->
              Some (Range.Step.Plus (Num (-n)))
          | Num _, _ -> None (* sign mismatch with the loop's direction *)
          | _, Some Increase -> Some (Range.Step.Plus total)
          (* refuse symbolic step with decreasing or unknown direction *)
          | _, (Some Decrease | None) -> None)
      | _ -> None

  let to_range (r : t) : Range.t option =
    let* lower_bound, upper_bound, dir = infer_bounds r in
    let* step = infer_step r in
    Some (Range.make ~lower_bound ~step ~dir r.name upper_bound)
end

let to_stmt (l : t) (body : Stmt.t) : Stmt.t =
  if body = Skip then Skip
  else
    match
      let* inf = Infer.parse ~body l in
      let* r = Infer.to_range inf in
      Some (inf, r)
    with
    | Some (inf, r) ->
        (* When the loop variable is mutated inside the body, shadow it
           with [Decl x = x] so the body's writes target an iteration-local
           binding rather than the For-loop's range variable. *)
        let body =
          if inf.needs_shadow then
            Stmt.seq (Stmt.decl_set inf.name (Var inf.name)) body
          else body
        in
        (* Prepend [extract_incs]'s closed-form [Decl] shadows for
           other-variable additive increments inside the [If]
           branch, so [Scoped.Code.fix_assigns] sees the shadow as
           defining the variable before it processes the branch's
           [Assign]s (which mutate the shadow) — otherwise the [If]
           case would reset its [defined] set and wrap the branch
           with a spurious [Decl.unset]. *)
        let body = Stmt.seq (Infer.extract_incs r inf.other_incs) body in
        let body =
          Stmt.from_list
            [ Stmt.if_ inf.loop_guard body Skip; inf.post_body ]
        in
        (* Final-value replacement: emit each harvested var's exit
           value as a post-[For] [Assign] so subsequent sequential
           code sees the right value through the substitution
           encoder. Skip when the loop has no computable closed-form
           trip count (handled by [post_for_assigns] returning
           [Skip]). *)
        let post_for = Infer.post_for_assigns r inf.other_incs in
        Stmt.seq inf.pre_loop (Stmt.seq (For (r, body)) post_for)
    | None ->
        let body = Stmt.If (l.cond, Stmt.seq body l.inc, Skip) in
        Stmt.seq l.init (Star body)

let infer_while (cond : Exp.bexp) (body : Stmt.t) : Stmt.t =
  let f = { init = Skip; inc = Stmt.last body; cond } in
  to_stmt f (Stmt.skip_last body)
