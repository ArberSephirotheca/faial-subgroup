open Stage0

let ( @ ) = Common.append_tr

type nexp =
  | Var of Variable.t
  | Num of int
  | Binary of N_binary.t * nexp * nexp
  | Unary of N_unary.t * nexp
  | NCall of string * nexp list
  | ReadResult of {
      array : Variable.t;
      version : int;
      (* [None] when the element type is not a value type, which is where
         the array-or-scalar seam does not hold. *)
      ty : Scalar.t option;
      args : nexp list;
    }
  | Convert of { ty : Scalar.t; arg : nexp }
  | NIf of bexp * nexp * nexp
  | CastInt of bexp

and bexp =
  | Bool of bool
  | NRel of N_rel.t * nexp * nexp
  | BRel of B_rel.t * bexp * bexp
  | BNot of bexp
  | Pred of string * nexp list
  | CastBool of nexp
  | Distinct of nexp list
  | AtomicResult of {
      target : Variable.t;
      array : Variable.t;
      index : nexp list;
      operation : nexp Atomic.Operation.t;
    }
  | IsThreadUnif of nexp



  let ( let@ ) c k = if c <> 0 then c else k ()

  let rec n_compare a b =
    match a, b with
    | Var x, Var y -> Variable.compare x y
    | Num x, Num y -> compare x y
    | Binary (op1, l1, r1), Binary (op2, l2, r2) ->
        let@ () = compare op1 op2 in
        let@ () = n_compare l1 l2 in
        n_compare r1 r2
    | Unary (op1, e1), Unary (op2, e2) ->
        let@ () = compare op1 op2 in
        n_compare e1 e2
    | NCall (f1, es1), NCall (f2, es2) ->
        let@ () = compare f1 f2 in
        List.compare n_compare es1 es2
    | NIf (b1, t1, f1), NIf (b2, t2, f2) ->
        let@ () = b_compare b1 b2 in
        let@ () = n_compare t1 t2 in
        n_compare f1 f2
    | ReadResult r1, ReadResult r2 ->
        let@ () = Variable.compare r1.array r2.array in
        let@ () = compare r1.version r2.version in
        List.compare n_compare r1.args r2.args
    | Convert c1, Convert c2 ->
        let@ () = Scalar.compare c1.ty c2.ty in
        n_compare c1.arg c2.arg
    | CastInt b1, CastInt b2 -> b_compare b1 b2
    | Var _, _ -> -1
    | _, Var _ -> 1
    | Num _, _ -> -1
    | _, Num _ -> 1
    | Binary _, _ -> -1
    | _, Binary _ -> 1
    | Unary _, _ -> -1
    | _, Unary _ -> 1
    | NCall _, _ -> -1
    | _, NCall _ -> 1
    | ReadResult _, _ -> -1
    | _, ReadResult _ -> 1
    | Convert _, _ -> -1
    | _, Convert _ -> 1
    | NIf _, _ -> -1
    | _, NIf _ -> 1

  and b_compare a b =
    match a, b with
    | Bool x, Bool y -> compare x y
    | NRel (op1, l1, r1), NRel (op2, l2, r2) ->
        let@ () = compare op1 op2 in
        let@ () = n_compare l1 l2 in
        n_compare r1 r2
    | BRel (op1, l1, r1), BRel (op2, l2, r2) ->
        let@ () = compare op1 op2 in
        let@ () = b_compare l1 l2 in
        b_compare r1 r2
    | BNot e1, BNot e2 -> b_compare e1 e2
    | Pred (p1, es1), Pred (p2, es2) ->
        let@ () = compare p1 p2 in
        List.compare n_compare es1 es2
    | CastBool e1, CastBool e2 -> n_compare e1 e2
    | Distinct l1, Distinct l2 -> List.compare n_compare l1 l2
    | ( AtomicResult { target = t1; array = a1; index = i1; operation = op1 },
        AtomicResult { target = t2; array = a2; index = i2; operation = op2 } )
      ->
        let@ () = Variable.compare t1 t2 in
        let@ () = Variable.compare a1 a2 in
        let@ () = List.compare n_compare i1 i2 in
        Atomic.Operation.compare n_compare op1 op2
    | IsThreadUnif e1, IsThreadUnif e2 -> n_compare e1 e2
    | Bool _, _ -> -1
    | _, Bool _ -> 1
    | NRel _, _ -> -1
    | _, NRel _ -> 1
    | BRel _, _ -> -1
    | _, BRel _ -> 1
    | BNot _, _ -> -1
    | _, BNot _ -> 1
    | Pred _, _ -> -1
    | _, Pred _ -> 1
    | CastBool _, _ -> -1
    | _, CastBool _ -> 1
    | Distinct _, _ -> -1
    | _, Distinct _ -> 1
    | AtomicResult _, _ -> -1
    | _, AtomicResult _ -> 1

let rec n_eval_res (n : nexp) : (int, string) Result.t =
  let ( let* ) = Result.bind in
  match n with
  | Var x -> Error ("n_eval: variable " ^ Variable.name x)
  | Num n -> Ok n
  | CastInt b ->
      let* b = b_eval_res b in
      Ok (if b then 1 else 0)
  | Unary (o, n) ->
      let* n = n_eval_res n in
      Ok (N_unary.eval o n)
  | Binary (o, n1, n2) -> (
      let* n1 = n_eval_res n1 in
      let* n2 = n_eval_res n2 in
      try Ok (N_binary.eval o n1 n2) with
      | N_binary.Unknown_width ->
          Error ("n_eval: no width for " ^ N_binary.to_string o)
      | N_binary.Shift_amount_out_of_range ->
          Error
            ("n_eval: shift amount out of range: " ^ N_binary.to_string o ^ " "
           ^ string_of_int n2))
  | NCall (x, _) -> Error ("n_eval: call " ^ x)
  | ReadResult r -> Error ("n_eval: read " ^ Variable.name r.array)
  | Convert c -> n_eval_res c.arg
  | NIf (b, n1, n2) ->
      let* b = b_eval_res b in
      if b then n_eval_res n1 else n_eval_res n2

and b_eval_res (b : bexp) : (bool, string) Result.t =
  let ( let* ) = Result.bind in
  match b with
  | Bool b -> Ok b
  | CastBool n ->
      let* n = n_eval_res n in
      Ok (n <> 0)
  | NRel (o, n1, n2) ->
      let* n1 = n_eval_res n1 in
      let* n2 = n_eval_res n2 in
      Ok (N_rel.eval o n1 n2)
  | BRel (o, b1, b2) ->
      let* b1 = b_eval_res b1 in
      let* b2 = b_eval_res b2 in
      Ok (B_rel.eval o b1 b2)
  | BNot b ->
      let* b = b_eval_res b in
      Ok (not b)
  | Pred (x, _) -> Error ("b_eval: pred " ^ x)
  | Distinct _ ->
      (* You'll implement this - placeholder for now *)
      Error "Distinct evaluation not implemented yet"
  | AtomicResult _ -> Error "b_eval: atomic_result"
  | IsThreadUnif _ -> Error "b_eval: thread_unif"

let n_eval_opt (n : nexp) : int option = n_eval_res n |> Result.to_option
let b_eval_opt (b : bexp) : bool option = b_eval_res b |> Result.to_option

let n_eval (n : nexp) : int =
  match n_eval_res n with Ok n -> n | Error e -> failwith e

let b_eval (b : bexp) : bool =
  match b_eval_res b with Ok b -> b | Error e -> failwith e

let num (n : int) : nexp = Num n
let n_zero = Num 0

let n_lt (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 < e2)
  | _, _ -> NRel (Lt Signedness.Signed, e1, e2)

let n_ult (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 < e2)
  | _, _ -> NRel (Lt Signedness.Unsigned, e1, e2)

let n_gt (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 > e2)
  | _, _ -> NRel (Gt Signedness.Signed, e1, e2)

let n_ugt (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 > e2)
  | _, _ -> NRel (Gt Signedness.Unsigned, e1, e2)

let n_le (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 <= e2)
  | _, _ -> NRel (Le Signedness.Signed, e1, e2)

let n_ule (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 <= e2)
  | _, _ -> NRel (Le Signedness.Unsigned, e1, e2)

let n_ge (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 >= e2)
  | _, _ -> NRel (Ge Signedness.Signed, e1, e2)

let n_uge (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | Num e1, Num e2 -> Bool (e1 >= e2)
  | _, _ -> NRel (Ge Signedness.Unsigned, e1, e2)

let n_eq (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | CastInt e, Num 0 | Num 0, CastInt e -> BNot e
  | CastInt e, Num 1 | Num 1, CastInt e -> e
  | Num e1, Num e2 -> Bool (e1 = e2)
  | _, _ -> NRel (Eq, e1, e2)

let n_neq (e1 : nexp) (e2 : nexp) : bexp =
  match (e1, e2) with
  | CastInt e, Num 0 -> e
  | Num e1, Num e2 -> Bool (e1 <> e2)
  | _, _ -> NRel (Neq, e1, e2)

let n_rel : N_rel.t -> nexp -> nexp -> bexp = function
  | N_rel.Lt Signed -> n_lt
  | Lt Unsigned -> n_ult
  | Gt Signed -> n_gt
  | Gt Unsigned -> n_ugt
  | Eq -> n_eq
  | Neq -> n_neq
  | Le Signed -> n_le
  | Le Unsigned -> n_ule
  | Ge Signed -> n_ge
  | Ge Unsigned -> n_uge

let n_if b n1 n2 =
  match b with Bool b -> if b then n1 else n2 | _ -> NIf (b, n1, n2)

let n_plus n1 n2 =
  match (n1, n2) with
  | Num 0, n | n, Num 0 -> n
  | Num n1, Num n2 -> Num (n1 + n2)
  | Num n1, Binary (Plus _, Num n2, e)
  | Binary (Plus _, Num n1, e), Num n2 ->
      Binary (Plus Signedness.Signed, Num (n1 + n2), e)
  | _, Num _ -> Binary (Plus Signedness.Signed, n2, n1)
  | _, _ -> Binary (Plus Signedness.Signed, n1, n2)

let n_inc (n : nexp) : nexp = n_plus n (Num 1)

let n_minus n1 n2 =
  match (n1, n2) with
  | n, Num 0 -> n
  | Num n1, Num n2 -> Num (n1 - n2)
  | _, _ -> Binary (Minus Signedness.Signed, n1, n2)

let n_dec (n : nexp) : nexp = n_minus n (Num 1)

let n_mult n1 n2 =
  match (n1, n2) with
  | Num 1, n | n, Num 1 -> n
  | Num 0, _ | _, Num 0 -> Num 0
  | Num n1, Num n2 -> Num (n1 * n2)
  | Num n1, Binary (Mult _, Num n2, e)
  | Num n1, Binary (Mult _, e, Num n2)
  | Binary (Mult _, Num n1, e), Num n2
  | Binary (Mult _, e, Num n1), Num n2 ->
      Binary (Mult Signedness.Signed, Num (n1 * n2), e)
  | _, _ -> Binary (Mult Signedness.Signed, n1, n2)

let n_uminus (n : nexp) : nexp = Unary (N_unary.Negate, n)
let sum : nexp list -> nexp = List.fold_left n_plus (Num 0)

let n_div n1 n2 =
  match (n1, n2) with
  | _, Num 1 -> n1
  | Num 0, _ -> Num 0
  (* Leave [_/0] unfolded rather than crashing: the source expression
     is undefined behavior at runtime, but the analyzer shouldn't fail
     when constant-folding a kernel that contains it (e.g. dead code
     under a guard the folder doesn't see through). *)
  | _, Num 0 -> Binary (Div Signedness.Signed, n1, n2)
  | Num n1, Num n2 -> Num (n1 / n2)
  | _, _ -> Binary (Div Signedness.Signed, n1, n2)

let n_udiv n1 n2 =
  match (n1, n2) with
  | _, Num 1 -> n1
  | Num 0, _ -> Num 0
  | _, Num 0 -> Binary (Div Signedness.Unsigned, n1, n2)
  | Num n1, Num n2 -> Num (n1 / n2)
  | _, _ -> Binary (Div Signedness.Unsigned, n1, n2)

let n_mod n1 n2 =
  match (n1, n2) with
  | Num n1, Num n2 -> Num (Common.modulo n1 n2)
  | _, _ -> Binary (Mod Signedness.Signed, n1, n2)

let n_umod n1 n2 =
  match (n1, n2) with
  | Num n1, Num n2 -> Num (Common.modulo n1 n2)
  | _, _ -> Binary (Mod Signedness.Unsigned, n1, n2)

let n_left_shift (l : nexp) (r : nexp) : nexp =
  match (l, r) with
  | a, Num n when n >= 0 && n < Sys.int_size - 1 ->
      Binary (Mult Signedness.Signed, a, Num (Common.pow ~base:2 n))
  | _, _ -> Binary (LeftShift, l, r)

(* Shifting a negative value right as unsigned reads it at its own width's
   two's complement, and nexp carries no width, so that fold is left to the
   solver rather than answered at a width we would have to invent. *)
let n_right_shift (s : Signedness.t) (l : nexp) (r : nexp) : nexp =
  match (s, l, r) with
  | Signedness.Signed, Num a, Num b when b >= 0 && b < 63 -> Num (a asr b)
  | Unsigned, Num a, Num b when a >= 0 && b >= 0 && b < 63 -> Num (a asr b)
  | _, _, _ -> Binary (RightShift s, l, r)

let n_bin o n1 n2 =
  try
    match (o, n1, n2) with
    | N_binary.RightShift s, _, _ -> n_right_shift s n1 n2
    | _, Num n1, Num n2 -> Num (N_binary.eval o n1 n2)
    | Plus _, _, _ -> n_plus n1 n2
    | Minus _, _, _ -> n_minus n1 n2
    | Mult _, _, _ -> n_mult n1 n2
    | Div Signed, _, _ -> n_div n1 n2
    | Div Unsigned, _, _ -> n_udiv n1 n2
    | Mod Signed, _, _ -> n_mod n1 n2
    | Mod Unsigned, _, _ -> n_umod n1 n2
    | LeftShift, _, _ -> n_left_shift n1 n2
    | _, _, _ -> Binary (o, n1, n2)
  with
  | Division_by_zero | N_binary.Unknown_width
  | N_binary.Shift_amount_out_of_range ->
      Binary (o, n1, n2)

let b_or b1 b2 =
  match (b1, b2) with
  | Bool true, _ | _, Bool true -> Bool true
  | Bool false, b | b, Bool false -> b
  | _, _ -> BRel (BOr, b1, b2)

let b_and b1 b2 =
  match (b1, b2) with
  | Bool true, b | b, Bool true -> b
  | Bool false, _ | _, Bool false -> Bool false
  | _, _ -> BRel (BAnd, b1, b2)

let b_rel o b1 b2 =
  match (o, b1, b2) with
  | _, Bool b1, Bool b2 -> Bool (B_rel.eval o b1 b2)
  | B_rel.BAnd, b1, b2 -> b_and b1 b2
  | BOr, b1, b2 -> b_or b1 b2

let rec b_not : bexp -> bexp = function
  | BNot b -> b
  | BRel (BAnd, b1, b2) -> b_or (b_not b1) (b_not b2)
  | BRel (BOr, b1, b2) -> b_and (b_not b1) (b_not b2)
  | NRel (Eq, n1, n2) -> n_neq n1 n2
  | NRel (Neq, n1, n2) -> n_eq n1 n2
  | NRel (Lt Signed, n1, n2) -> n_ge n1 n2
  | NRel (Lt Unsigned, n1, n2) -> n_uge n1 n2
  | NRel (Gt Signed, n1, n2) -> n_le n1 n2
  | NRel (Gt Unsigned, n1, n2) -> n_ule n1 n2
  | NRel (Le Signed, n1, n2) -> n_gt n1 n2
  | NRel (Le Unsigned, n1, n2) -> n_ugt n1 n2
  | NRel (Ge Signed, n1, n2) -> n_lt n1 n2
  | NRel (Ge Unsigned, n1, n2) -> n_ult n1 n2
  | Bool b -> Bool (not b)
  | b -> BNot b

let b_impl b1 b2 =
  match b1 with
  | Bool true -> b2
  | Bool false -> Bool true
  | _ -> b_or (b_not b1) b2

let b_true = Bool true
let b_false = Bool false

let n_bit_not : nexp -> nexp = function
  | Num n -> Num Int32.(of_int n |> lognot |> to_int)
  | e -> Unary (BitNot, e)

let convert (ty : Scalar.t) (arg : nexp) : nexp =
  match arg with
  | Num n when Scalar.contains n ty -> arg
  | _ -> Convert { ty; arg }

let cast_int : bexp -> nexp = function
  | Bool true -> Num 1
  | Bool false -> Num 0
  | CastBool n -> n
  | b -> CastInt b

let cast_bool : nexp -> bexp = function
  | Num n -> Bool (n <> 0)
  | CastInt b -> b
  | n -> CastBool n

let rec b_and_ex l =
  match l with [] -> Bool true | [ x ] -> x | x :: l -> b_and x (b_and_ex l)

let rec b_or_ex l =
  match l with [] -> Bool true | [ x ] -> x | x :: l -> b_or x (b_or_ex l)

let is_thread_unif (e : nexp) : bexp = IsThreadUnif e

let is_thread_distinct (idx : Variable.t list) : bexp =
  b_or_ex (List.map (fun x -> b_not (is_thread_unif (Var x))) idx)

let is_thread_unif_name : string = "__is_thread_unif"
let is_thread_distinct_name : string = "__is_thread_distinct"

(* Source-level spelling of the uniformity annotations. Both frontends
   recognise these names and build [IsThreadUnif] directly, so the
   annotation is never carried as a [Predicates.t]. *)
let is_uniformity_intrinsic (name : string) : bool =
  String.equal name is_thread_unif_name
  || String.equal name is_thread_distinct_name

let rec n_bin_split (o : N_binary.t) : nexp -> nexp list = function
  | Binary (o', e1, e2) when o' = o -> n_bin_split o e1 @ n_bin_split o e2
  | e -> [ e ]

let rec b_and_split : bexp -> bexp list = function
  | BRel (BAnd, b1, b2) -> b_and_split b1 @ b_and_split b2
  | b -> [ b ]

let rec b_or_split : bexp -> bexp list = function
  | BRel (BOr, b1, b2) -> b_or_split b1 @ b_or_split b2
  | b -> [ b ]

let rec n_fold f e a =
  match e with
  | CastInt e -> b_fold f e a
  | Num _ -> a
  | Var x -> f x a
  | Unary (_, e) -> n_fold f e a
  | Binary (_, e1, e2) -> n_fold f e1 a |> n_fold f e2
  | NIf (b, e1, e2) -> b_fold f b a |> n_fold f e1 |> n_fold f e2
  | NCall (_, es) -> List.fold_left (fun a e -> n_fold f e a) a es
  | ReadResult r -> List.fold_left (fun a e -> n_fold f e a) a r.args
  | Convert c -> n_fold f c.arg a

and b_fold f e a =
  match e with
  | CastBool n -> n_fold f n a
  | Pred (_, ns) -> List.fold_left (fun a n -> n_fold f n a) a ns
  | Bool _ -> a
  | NRel (_, n1, n2) -> n_fold f n1 a |> n_fold f n2
  | BRel (_, b1, b2) -> b_fold f b1 a |> b_fold f b2
  | BNot b -> b_fold f b a
  | Distinct exprs -> List.fold_left (fun acc expr -> n_fold f expr acc) a exprs
  | AtomicResult { target; array; index; operation } ->
      let a = f target a in
      let a = f array a in
      let a = List.fold_left (fun a e -> n_fold f e a) a index in
      Atomic.Operation.fold (fun e a -> n_fold f e a) operation a
  | IsThreadUnif e -> n_fold f e a

let n_free_names : nexp -> Variable.Set.t -> Variable.Set.t =
  n_fold Variable.Set.add

let b_free_names : bexp -> Variable.Set.t -> Variable.Set.t =
  b_fold Variable.Set.add

let n_equal (a : nexp) (b : nexp) : bool = n_compare a b = 0
let b_equal (a : bexp) (b : bexp) : bool = b_compare a b = 0

let rec strip_convert : nexp -> nexp = function
  | Convert c -> strip_convert c.arg
  | n -> n

let b_calls : bexp -> nexp list =
  let rec b_walk (acc : nexp list) (b : bexp) : nexp list =
    match b with
    | Bool _ -> acc
    | NRel (_, n1, n2) -> n_walk (n_walk acc n1) n2
    | BRel (_, b1, b2) -> b_walk (b_walk acc b1) b2
    | BNot b -> b_walk acc b
    | Pred (_, ns) | Distinct ns -> List.fold_left n_walk acc ns
    | CastBool n | IsThreadUnif n -> n_walk acc n
    | AtomicResult { index; operation; _ } ->
        let acc = List.fold_left n_walk acc index in
        Atomic.Operation.fold (fun n acc -> n_walk acc n) operation acc
  and n_walk (acc : nexp list) (n : nexp) : nexp list =
    match n with
    | Var _ | Num _ -> acc
    | Unary (_, e) -> n_walk acc e
    | Binary (_, n1, n2) -> n_walk (n_walk acc n1) n2
    | CastInt b -> b_walk acc b
    | NIf (b, n1, n2) -> n_walk (n_walk (b_walk acc b) n1) n2
    | NCall (_, args) -> List.fold_left n_walk (n :: acc) args
    | ReadResult r -> List.fold_left n_walk (n :: acc) r.args
    | Convert c -> n_walk acc c.arg
  in
  fun b -> b_walk [] b |> List.sort_uniq n_compare

(* Checks if variable [x] is in the given expression *)
let rec n_exists (f : Variable.t -> bool) : nexp -> bool = function
  | CastInt b -> b_exists f b
  | Var x -> f x
  | Num _ -> false
  | Binary (_, e1, e2) -> n_exists f e1 || n_exists f e2
  | NCall (_, es) -> List.exists (n_exists f) es
  | ReadResult r -> List.exists (n_exists f) r.args
  | Convert c -> n_exists f c.arg
  | Unary (_, e) -> n_exists f e
  | NIf (b, e1, e2) -> b_exists f b || n_exists f e1 || n_exists f e2

and b_exists (f : Variable.t -> bool) : bexp -> bool = function
  | Bool _ -> false
  | CastBool e -> n_exists f e
  | Pred (_, es) -> List.exists (n_exists f) es
  | NRel (_, e1, e2) -> n_exists f e1 || n_exists f e2
  | BRel (_, e1, e2) -> b_exists f e1 || b_exists f e2
  | BNot e -> b_exists f e
  | Distinct exprs -> List.exists (n_exists f) exprs
  | AtomicResult { target; array; index; operation } ->
      f target || f array
      || List.exists (n_exists f) index
      || Atomic.Operation.exists (n_exists f) operation
  | IsThreadUnif e -> n_exists f e

(* Checks if variable [x] is in the given expression *)
let n_mem (x : Variable.t) : nexp -> bool = n_exists (Variable.equal x)
let b_mem (x : Variable.t) : bexp -> bool = b_exists (Variable.equal x)

let n_intersects (s : Variable.Set.t) : nexp -> bool =
  n_exists (fun x -> Variable.Set.mem x s)

let b_intersects (s : Variable.Set.t) : bexp -> bool =
  b_exists (fun x -> Variable.Set.mem x s)

let rec erase_converts : nexp -> nexp = function
  | (Var _ | Num _) as n -> n
  | Convert c -> erase_converts c.arg
  | Unary (o, e) -> Unary (o, erase_converts e)
  | Binary (o, a, b) -> Binary (o, erase_converts a, erase_converts b)
  | NCall (x, args) -> NCall (x, List.map erase_converts args)
  | ReadResult r -> ReadResult { r with args = List.map erase_converts r.args }
  | NIf (b, a, c) -> NIf (b_erase_converts b, erase_converts a, erase_converts c)
  | CastInt b -> CastInt (b_erase_converts b)

and b_erase_converts : bexp -> bexp = function
  | (Bool _ | IsThreadUnif _) as b -> b
  | NRel (o, a, b) -> NRel (o, erase_converts a, erase_converts b)
  | BRel (o, a, b) -> BRel (o, b_erase_converts a, b_erase_converts b)
  | BNot b -> BNot (b_erase_converts b)
  | Pred (x, ns) -> Pred (x, List.map erase_converts ns)
  | CastBool n -> CastBool (erase_converts n)
  | Distinct ns -> Distinct (List.map erase_converts ns)
  | AtomicResult a ->
      AtomicResult { a with index = List.map erase_converts a.index }

let rec b_map (f : nexp -> nexp) : bexp -> bexp = function
  | Bool _ as b -> b
  | NRel (o, n1, n2) -> NRel (o, f n1, f n2)
  | BRel (o, b1, b2) -> BRel (o, b_map f b1, b_map f b2)
  | BNot b -> BNot (b_map f b)
  | Pred (s, es) -> Pred (s, List.map f es)
  | CastBool e -> CastBool (f e)
  | Distinct l -> Distinct (List.map f l)
  | AtomicResult { target; array; index; operation } ->
      AtomicResult
        {
          target;
          array;
          index = List.map f index;
          operation = Atomic.Operation.map f operation;
        }
  | IsThreadUnif e -> IsThreadUnif (f e)

let reset_variable_kind_n ~kernel_parameters ~loop_variables : nexp -> nexp =
  let reset_v = Variable.reset_kind ~kernel_parameters ~loop_variables in
  let rec reset = function
    | Var v -> Var (reset_v v)
    | Num _ as e -> e
    | Binary (o, a, b) -> Binary (o, reset a, reset b)
    | Unary (o, a) -> Unary (o, reset a)
    | NCall (g, es) -> NCall (g, List.map reset es)
    | ReadResult r -> ReadResult { r with args = List.map reset r.args }
    | Convert c -> convert c.ty (reset c.arg)
    | NIf (b, a1, a2) -> NIf (b_map reset b, reset a1, reset a2)
    | CastInt b -> CastInt (b_map reset b)
  in
  reset

let reset_variable_kind_b ~kernel_parameters ~loop_variables : bexp -> bexp =
  b_map (reset_variable_kind_n ~kernel_parameters ~loop_variables)

type side = Left | Right

let rec n_par ?context (* ?side *) (n : nexp) : string =
  match context, n with
  | ( Some (N_binary.Plus _),
      Binary ((N_binary.Plus _ | N_binary.Mult _ | N_binary.Div _), _, _) )
  | Some (N_binary.Mult _), Binary (N_binary.Mult _, _, _) ->
      n_to_string n
  | _, Num _ | _, Var _ | _, NCall _ | _, ReadResult _ | _, CastInt _ ->
      n_to_string n
  | _, NIf _ | _, Unary _ | _, Binary _ | _, Convert _ ->
      "(" ^ n_to_string n ^ ")"

and n_to_string : nexp -> string = function
  | Num n -> string_of_int n
  | Var x -> Variable.name x
  | Unary (o, n) -> N_unary.to_string o ^ n_par n
  | Binary (b, a1, a2) -> n_par ~context:b a1 ^ " " ^ N_binary.to_string b ^ " " ^ n_par ~context:b a2
  | NCall (x, args) ->
      x ^ "(" ^ String.concat ", " (List.map n_to_string args) ^ ")"
  | ReadResult r ->
      Read_symbol.name r.array ^ "("
      ^ String.concat ", "
          (string_of_int r.version :: List.map n_to_string r.args)
      ^ ")"
  | Convert c -> "(" ^ Scalar.to_string c.ty ^ ")" ^ n_par c.arg
  | NIf (b, n1, n2) -> b_par b ^ " ? " ^ n_par n1 ^ " : " ^ n_par n2
  | CastInt b -> "int(" ^ b_to_string b ^ ")"

and b_to_string : bexp -> string = function
  | Bool b -> if b then "true" else "false"
  | CastBool e -> "bool(" ^ n_to_string e ^ ")"
  | NRel (b, n1, n2) -> n_par n1 ^ " " ^ N_rel.to_string b ^ " " ^ n_par n2
  | BRel (b, b1, b2) -> b_par b1 ^ " " ^ B_rel.to_string b ^ " " ^ b_par b2
  | BNot b -> "!" ^ b_par b
  | Pred (x, vs) -> x ^ "(" ^ String.concat ", " (List.map n_to_string vs) ^ ")"
  | Distinct exprs ->
      "distinct(" ^ String.concat ", " (List.map n_to_string exprs) ^ ")"
  | AtomicResult { target; array; index; operation } ->
      let op_args =
        Atomic.Operation.to_list operation
        |> List.filter_map (Option.map n_to_string)
        |> String.concat ", "
      in
      let idx_s = List.map n_to_string index |> String.concat ", " in
      "atomic_result(" ^ Variable.name target ^ " = "
      ^ Variable.name array ^ "[" ^ idx_s ^ "], "
      ^ Atomic.Operation.to_string operation
      ^ (if op_args = "" then "" else "(" ^ op_args ^ ")")
      ^ ")"
  | IsThreadUnif e -> "thread_unif(" ^ n_to_string e ^ ")"

and b_par (b : bexp) : string =
  match b with
  | Pred _ | CastBool _ | Bool _ | BNot _ | Distinct _ | AtomicResult _
  | IsThreadUnif _ ->
      b_to_string b
  | BRel _ | NRel _ -> "(" ^ b_to_string b ^ ")"

let b_to_s : bexp -> Indent.t list =
  let rec to_s (in_and : bool) (b : bexp) : Indent.t list =
    let open Indent in
    match b with
    | NRel _ | Bool _ | BNot _ | CastBool _ | Pred _ | Distinct _
    | AtomicResult _ | IsThreadUnif _ ->
        [ Line (b_to_string b) ]
    | BRel (o, _, _) ->
        let op = B_rel.to_string o in
        b
        |> (if in_and then b_and_split else b_or_split)
        |> List.map (fun b ->
            match to_s (not in_and) b with [ Line b ] -> Line b | l -> Block l)
        |> List.mapi (fun i ->
            let op = if i = 0 then "" else op ^ " " in
            function
            | Line s -> [ Line (op ^ s) ]
            | Block l -> [ Line (op ^ "("); Block l; Line ")" ]
            | Nil -> [])
        |> List.concat
  in
  to_s true

(* The constraint a set of bounds imposes on a term: an end that cannot be
   written contributes no inequality, so the result is anything from [Bool
   true] through a single inequality to a conjunction of two. *)
let in_bounds (n : nexp) (b : Bounds.t) : bexp =
  [
    b.lower |> Option.map (fun lo -> n_le (Num lo) n);
    b.upper |> Option.map (fun hi -> n_le n (Num hi));
  ]
  |> List.filter_map Fun.id |> b_and_ex

let scalar_bound (n : nexp) (ty : Scalar.t) : bexp =
  match Scalar.to_bounds ty with Some b -> in_bounds n b | None -> Bool true

let ty_bound (n : nexp) (ty : Ty.t) : bexp =
  match Ty.to_bounds ty with Some b -> in_bounds n b | None -> Bool true

let int_dom_bound (n : nexp) (d : Int_dom.t) : bexp =
  in_bounds n (Int_dom.to_bounds d)
