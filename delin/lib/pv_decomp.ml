open Protocols

module Eval = struct
  type t = {
    place_value : Poly.t list;
    numeral : Poly.t list;
  }

  module Axis = struct
    type t = {
      digit : Poly.t;
      pv : Poly.t;
    }

    let of_mono (m : Mono.t) : t =
      let ind = Mono.filter (fun v _ -> Indet.is_induction v) m in
      {
        digit = Poly.of_list [ Mono.of_monic (Mono.monic ind) ];
        pv = Poly.of_list [ Mono.filter (fun v _ -> Indet.is_parameter v) m ];
      }

    let of_offset (offset : Poly.t) : t = { digit = offset; pv = Poly.of_int 1 }

    let try_add (l : t) (r : t) : t option =
      if Poly.compare l.digit r.digit = 0
      then Some { l with pv = Poly.(l.pv + r.pv) }
      else None
  end

  let add_axis (new_a : Axis.t) (axes : Axis.t list) : Axis.t list =
    let rec go = function
      | [] -> [ new_a ]
      | a :: rest ->
        (match Axis.try_add new_a a with
         | Some a -> a :: rest
         | None -> a :: go rest)
    in
    go axes

  let normalize ({ place_value; numeral } : t) : t =
    let grouped =
      List.fold_left2
        (fun m p n ->
          Poly.Map.update p
            (function Some n' -> Some Poly.(n + n') | None -> Some n)
            m)
        Poly.Map.empty place_value numeral
    in
    let place_value, numeral = Poly.Map.bindings grouped |> List.split in
    { place_value; numeral }

  let to_axes ~(globals : Variable.Set.t) (e : Exp.nexp) : Axis.t list =
    let induction, uniform =
      Poly.from_nexp ~globals e
      |> Poly.to_mono_list
      |> List.partition Mono.has_induction
    in
    let strided =
      List.fold_left (fun axes m -> add_axis (Axis.of_mono m) axes) [] induction
    in
    Axis.of_offset (Poly.of_list uniform) :: strided

  let of_axes (axes : Axis.t list) : t =
    {
      place_value = List.map (fun (a : Axis.t) -> a.pv) axes;
      numeral = List.map (fun (a : Axis.t) -> a.digit) axes;
    }

  let make ~(globals : Variable.Set.t) (e : Exp.nexp) : t =
    to_axes ~globals e |> of_axes |> normalize

  let numeral_at (pv : Poly.t) (t : t) : Poly.t =
    let rec go ps ns =
      match ps, ns with
      | p :: ps, n :: ns -> if Poly.compare p pv = 0 then n else go ps ns
      | _, _ -> Poly.zero
    in
    go t.place_value t.numeral
end

let bound_of ~(in_range : bool) (frame : Poly.t list) (rows : Poly.t list list) :
    Exp.bexp =
  let n = Poly.to_nexp in
  let one = Poly.of_int 1 in
  let columns =
    List.mapi (fun k pv -> (pv, List.map (fun r -> List.nth r k) rows)) frame
  in
  let units, strides =
    List.partition (fun (pv, _) -> Poly.compare pv one = 0) columns
  in
  let rec bound_for_order = function
    | (pk, xs) :: ((pnext, _) :: _ as rest) ->
      let divisibility =
        if Poly.compare pk one = 0
        then []
        else [ Exp.n_eq (Exp.n_umod (n pnext) (n pk)) (Exp.Num 0) ]
      in
      let spans =
        if in_range
        then
          List.concat_map
            (fun x ->
              let span = n Poly.(x * pk) in
              [ Exp.n_le (Exp.Num 0) span; Exp.n_lt span (n pnext) ])
            xs
        else []
      in
      divisibility @ spans @ bound_for_order rest
    | _ -> []
  in
  let pvs_ge_one = List.map (fun pv -> Exp.n_ge (n pv) (Exp.Num 1)) frame in
  let orders =
    Stage0.Common.permutations strides
    |> List.map (fun order -> Exp.b_and_ex (bound_for_order (units @ order)))
    |> Exp.b_or_ex
  in
  Exp.b_and_ex (orders :: pvs_ge_one)

type frame = Poly.t list

let analyze ~(globals : Variable.Set.t) ~(in_range : bool)
    (indices : Exp.nexp list) : frame * Exp.bexp =
  let evals = List.map (Eval.make ~globals) indices in
  let frame =
    List.concat_map (fun (e : Eval.t) -> e.place_value) evals
    |> List.sort_uniq Poly.compare
  in
  let rows =
    List.map (fun e -> List.map (fun p -> Eval.numeral_at p e) frame) evals
  in
  (frame, bound_of ~in_range frame rows)

let subscripts ~(globals : Variable.Set.t) ~(frame : frame) (index : Exp.nexp) :
    Exp.nexp list =
  let e = Eval.make ~globals index in
  List.map (fun p -> Poly.to_nexp (Eval.numeral_at p e)) frame

let make ~(globals : Variable.Set.t) ?(in_range = false) (indices : Exp.nexp list) :
    Exp.nexp list list * Exp.bexp =
  let frame, bound = analyze ~globals ~in_range indices in
  (List.map (subscripts ~globals ~frame) indices, bound)
