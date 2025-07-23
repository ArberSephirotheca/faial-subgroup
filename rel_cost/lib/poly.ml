(*
open Protocols
open Stage0

type poly_ht = (int, Reals.integer) Hashtbl.t

type t =
  | Exp0 of Reals.integer
  | Exp1 of {constant: Reals.integer; coefficient: Reals.integer}
  | Many of poly_ht

let max_exponent : t -> int =
  function
  | Exp0 _ -> 0
  | Exp1 _ -> 1
  | Many ht -> Hashtbl.to_seq_keys ht |> Seq.fold_left max 0

let min_exponent : t -> int =
  function
  | Exp0 _ -> 0
  | Exp1 _ -> 0
  | Many ht ->
    Hashtbl.to_seq_keys ht |> Seq.fold_left min 0

let update_ht (ht:('a, 'b) Hashtbl.t) (k:'a)  (f:'b option -> 'b)  : unit =
  Hashtbl.replace ht k (f (Hashtbl.find_opt ht k))

let map (f:Reals.integer -> int -> Reals.integer) : t -> t =
  function
  | Exp0 n -> Exp0 (f n 0)
  | Exp1 {constant=n0; coefficient=n1} ->
    Exp1 {constant=f n0 0; coefficient=f n1 1}
  | Many ht ->
    let ht' = Hashtbl.copy ht in
    Hashtbl.filter_map_inplace (fun expi coef ->
      Some (f coef expi)
    ) ht';
    Many ht'

let map1 (f:Reals.integer -> Reals.integer) : t -> t =
  map (fun e _ -> f e)

let poly_update_ht (ht:poly_ht) (k:int) (f:Reals.integer -> Reals.integer) : unit =
  update_ht ht k (function | Some v -> f v | None -> f Reals.zero)

let poly_add_ht (src:poly_ht) (dst:poly_ht) : unit =
  Hashtbl.iter (fun i n ->
    poly_update_ht dst i (Reals.plus n)
  ) src

let copy_hashtbl ?(expected_size=10) : t -> poly_ht =
  function
  | Exp0 n ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n;
    ht
  | Exp1 {constant=n0; coefficient=n1} ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n0;
    Hashtbl.add ht 1 n1;
    ht
  | Many ht ->
    Hashtbl.copy ht

let hashtbl ?(expected_size=10) : t -> poly_ht =
  function
  | Exp0 n ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n;
    ht
  | Exp1 {constant=n0; coefficient=n1} ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n0;
    Hashtbl.add ht 1 n1;
    ht
  | Many ht ->
    ht

let make ?(expected_size=10) (e:Reals.integer) (n:int) : t =
  if n = 0 then
    Exp0 e
  else if n = 1 then
    Exp1 {constant=Reals.zero; coefficient=e}
  else
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht n e;
    Many ht

let constant (k:int) = make (Reals.from_int k) 0

let zero : t = constant 0

let to_list : t -> (Reals.integer * int) list =
  function
  | Exp0 x ->
    [(x, 0)]
  | Exp1 {constant=n1; coefficient=n2} ->
    [(n1, 0); (n2, 1)]
  | Many ht ->
    Common.hashtbl_elements ht
    |> List.map (fun (x, y) -> (y, x))

let add (e1: t) (e2: t) : t =
  let open Reals in
  match e1, e2 with
  | Exp0 n1, Exp0 n2 -> Exp0 (Reals.plus n1 n2)
  | Exp0 n1, Exp1 {constant=n2; coefficient=n3}
  | Exp1 {constant=n2; coefficient=n3}, Exp0 n1 ->
    Exp1 {constant=plus n2 n1; coefficient=n3}
  | Exp1 {constant=n1; coefficient=n2}, Exp1 {constant=n3; coefficient=n4} ->
    Exp1 {constant=plus n1 n3; coefficient=plus n2 n4}
  | Exp0 n1, Many ht
  | Many ht, Exp0 n1 ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (plus n1);
    Many ht
  | Exp1 {constant=n1; coefficient=n2}, Many ht
  | Many ht, Exp1 {constant=n1; coefficient=n2} ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (plus n1);
    poly_update_ht ht 1 (plus n2);
    Many ht
  | Many ht1, Many ht2 ->
    let ht2 = Hashtbl.copy ht2 in
    poly_add_ht ht1 ht2;
    Many ht2

let map_exponent (f:int -> int) : t -> t =
  function
  | Exp0 n when f 0 = 0 -> Exp0 n
  | p ->
    let ht =
      p
      |> copy_hashtbl
      |> Common.hashtbl_elements
      |> List.map (fun (x, y) -> (f x, y))
      |> Common.hashtbl_from_list
    in
    Many ht

let add_exponent (amount:int) : t -> t =
  map_exponent (fun x -> x + amount)

let to_seq : t -> (Reals.integer*int) Seq.t =
  function
  | Exp0 x -> Seq.return (x, 0)
  | Exp1 {constant=n1; coefficient=n2} ->
    [n1, 0; n2, 1] |> List.to_seq
  | Many ht ->
    Common.hashtbl_elements ht
    |> List.to_seq
    |> Seq.map (fun (x, y) -> (y, x))

(* Given an exponent, return the coefficient *)

let get_opt (exponent:int) : t -> Reals.integer option =
  function
  | Exp0 n when exponent = 0 -> Some n
  | Exp0 _ -> None
  | Exp1 {constant=n; _} when exponent = 0 -> Some n
  | Exp1 {coefficient=n; _} when exponent = 1 -> Some n
  | Exp1 _ -> None
  | Many ht -> Hashtbl.find_opt ht exponent

let get (exponent:int) (p: t) : Reals.integer =
  Option.value (get_opt exponent p) ~default:Reals.zero

let compare ((_:Reals.integer), (exp1:int)) ((_:Reals.integer), (exp2:int)) : int =
  compare exp1 exp2

let to_seq_ord (p: t) : (Reals.integer*int) Seq.t =
  p
  |> to_list
  |> List.sort compare
  |> List.to_seq

let uminus : t -> t =
  map1 Reals.uminus

let mult1 (e:Reals.integer) : t -> t =
  map1 (Reals.mult e)

let mult2 (coefficient:Reals.integer) (exponent:int) (p:t) : t =
  let p = mult1 coefficient p in
  if exponent = 0 then
    p
  else
    add_exponent exponent p

let mult (e1:t) (e2:t) : t =
  match e1, e2 with
  | Exp0 n1, Exp0 n2 ->
    Exp0 (Reals.mult n1 n2)

  | Exp0 n1, Exp1 {constant=n2; coefficient=n3}
  | Exp1 {constant=n2; coefficient=n3}, Exp0 n1 ->
    Exp1 {constant=Reals.mult n1 n2; coefficient=Reals.mult n1 n3}

  | Exp0 n1, p
  | p, Exp0 n1 ->
    mult1 n1 p

  | Exp1 {constant=n1; coefficient=n2}, p
  | p, Exp1 {constant=n1; coefficient=n2} ->
    let p1 = mult1 n1 p in
    let p2 = mult2 n2 1 p in
    add p1 p2

  | Many ht1, Many ht2 ->
    let (ht1, ht2) =
      if Hashtbl.length ht1 < Hashtbl.length ht2 then
        (ht1, ht2)
      else
        (ht2, ht1)
    in
    let p2 = Many ht2 in
    Hashtbl.fold (fun exponent coefficient r ->
      add (mult2 coefficient exponent p2) r
    ) ht1 zero

let div1 (p:t) (e:Reals.integer) : t =
  p
  |> map1 (fun x ->
    if x <> Num 0 then
      Reals.div x e
    else
      x
  )


let from_list : (Reals.integer * int) list -> t =
  function
  | [ (e, 0); (Num 0, _) ]
  | [ (Num 0, _); (e, 0) ]
  | [ (e, 0) ] -> Exp0 e
  | [ (n0, 0); (n1, 1) ]
  | [ (n1, 1); (n0, 0) ] -> Exp1 {constant=n0; coefficient=n1}
  | l ->
    let ht =
      l
      |> List.filter (fun (coef, _) -> let open Reals in coef <> Num 0)
      |> List.map (fun (x, y) -> (y, x))
      |> Common.hashtbl_from_list
    in
    Many ht

let optimize (p: t) : t =
  p
  |> map1 Reals.optimize
  |> to_list
  |> from_list

let inverse (p:t) : t =
  let inv : Reals.integer -> Reals.integer =
    function
    | Num 0 -> Num 0
    | n -> Reals.div (Num 1) n
  in
  match p with
  | Exp0 n -> Exp0 (inv n)
  | _ ->
    to_list p
    |> List.map (fun (coef, pow) -> (inv coef, (-pow)))
    |> from_list

let div (e1:t) (e2:t) : t =
  mult e1 (inverse e2)

let rec filter (to_keep: Reals.integer -> int -> bool) (p: t) : t =
  match p with
  | Exp0 n when to_keep n 0 -> Exp0 n
  | Exp0 _ -> Exp0 (Reals.zero)
  | Exp1 {coefficient=n1; constant=n0} when not (to_keep n1 1) ->
    filter to_keep (Exp0 n0)
  | Exp1 {coefficient=n1; constant=n0} when not (to_keep n0 0) ->
    Exp1 {coefficient=n1; constant=Reals.zero}
  | Exp1 _ -> p
  | Many ht ->
    let ht' = Hashtbl.copy ht in
    Hashtbl.filter_map_inplace (fun expi coef ->
      if to_keep coef expi then
        Some coef
      else None
    ) ht';
    Many ht'

let pow (base:Reals.integer) (exponent: int) : Reals.integer =
  let rec pow : int -> Reals.integer =
    function
    | 0 -> Num 1
    | 1 -> base
    | n -> Reals.mult base (pow (n - 1))
  in
  if exponent < 0 then
    Reals.div Reals.one (pow (-exponent))
  else
    pow exponent

let to_string ?(skip_zero=true) ?(sort=true) ?(var="x") (p: t) : string =
  let p = to_list p in
  let handle b f l = if b then f l else l in
  let result =
    p
    |> handle sort (List.sort compare)
    |> handle skip_zero (List.filter (fun (coef, _) -> coef <> Reals.Num 0))
    |> List.map (fun (coef, pow) ->
      let s_coef =
        match coef with
        | Reals.Num _
        | Var _ -> Reals.to_string coef
        | _ -> "(" ^ Reals.to_string coef ^ ")"
      in
      if pow = 0 then
        Reals.to_string coef
      else if pow = 1 then
        s_coef ^ "·" ^ var
      else
        s_coef ^ "·" ^ var ^ Reals.superscript pow
    )
    |> String.concat " + "
  in
  if result = "" then "0" else result

let to_reals (x:Variable.t) : t -> Reals.integer =
  function
  | Exp1 {constant=n; coefficient=Num 0}
  | Exp0 n -> n
  | Exp1 {constant=Num 0; coefficient=n1} -> Reals.mult n1 (Var x)
  | Exp1 {constant=n0; coefficient=n1} ->
    Reals.plus n0 (Reals.mult n1 (Var x))
  | Many ht ->
    Hashtbl.fold (fun exponent coef accum ->
      Reals.plus
        (Reals.mult coef (pow (Var x) exponent))
        accum
    ) ht Reals.zero

let equal (p1 : t) (p2: t) : bool =
  let p1 = optimize p1 in
  let p2 = optimize p2 in
  match p1, p2 with
  | Exp0 n0, Exp0 n0' -> n0 = n0'
  | Exp1 {constant=n0; coefficient=n1},
    Exp1 {constant=n0'; coefficient=n1'} ->
      n0 = n0' && n1 = n1'
  | Many ht, Many ht' ->
    if Hashtbl.length ht = Hashtbl.length ht' then
      Hashtbl.fold (fun k v p ->
        p && Hashtbl.find_opt ht k = Some v
      ) ht' true
    else
      false
  | _, _ -> false

let from_reals (x:Variable.t) : Reals.integer -> t option =
  let ( let* ) = Option.bind in
  let rec from_i : Reals.integer -> t option =
  function
  | Var y when Variable.equal x y ->
    Some (Exp1 {constant=Reals.zero; coefficient=Reals.one})
  | Binary (Plus, e1, e2) ->
    let* e1 = from_i e1 in
    let* e2 = from_i e2 in
    Some (add e1 e2)
  | Binary (Minus, e1, e2) ->
    let* e1 = from_i e1 in
    let* e2 = from_i e2 in
    Some (add e1 (uminus e2))
  | Binary (Mult, e1, e2) ->
    let* e1 = from_i e1 in
    let* e2 = from_i e2 in
    Some (mult e1 e2)
  | Binary (Div, e1, e2) ->
    let* e1 = from_i e1 in
    let* e2 = from_i e2 in
    Some (div e1 e2)
  | Num _ as n-> Some (Exp0 n)
  | Var _ as n -> Some (Exp0 n)
  | Binary _ | FloatToInt _
  | Unary _ | If _ | BoolToInt _ ->
    None
  in
  from_i

let rec optimize_reals (e:Reals.t) : Reals.t =
  let fvs = Reals.free_names e Variable.Set.empty in
  if Variable.Set.is_empty fvs then
    Reals.optimize e
  else
    let x = Variable.Set.choose fvs in
    match from_reals x e with
    | Some p ->
      p
      |> to_seq_ord
      |> Seq.filter_map (fun (coef, pow) ->
          let coef = optimize_reals coef in
          if Reals.is_zero coef then None
          else Some (
            match pow with
            | 0 -> coef
            | 1 -> Binary (Mult, coef, Var x)
            | -1 -> Binary (Div, coef, Var x)
            | _ ->
              if pow < 0 then
                Binary (Div, coef, Binary (Pow, Var x, Num pow))
              else
                Binary (Mult, coef, Binary (Pow, Var x, Num pow))
          )
        )
      |> List.of_seq
      |> List.fold_left Reals.plus Reals.zero
      |> Reals.optimize
    | None -> Reals.optimize e
*)
