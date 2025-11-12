open Stage0
open Protocols
(*
  1. Generates a summation from a slice.
  2. Flattens a summation expression as a single numeric expression.
  *)

module Op = struct
  type t = Plus | Minus | Div | Mult | Max | Min

  let to_string : t -> string = function
    | Div -> "/"
    | Mult -> "*"
    | Minus -> "-"
    | Plus -> "+"
    | Max -> "max"
    | Min -> "min"

  let eval : t -> int -> int -> int = function
    | Mult -> ( * )
    | Div -> ( / )
    | Plus -> ( + )
    | Minus -> ( - )
    | Max -> max
    | Min -> min
end

type t =
  | Clamp of { value : Reals.t; upper_bound : int }
  | Const of int
  | Sum of Set_range.t * t
  | If of Reals.boolean * t * t
  | Bin of Op.t * t * t

let minus (lhs : t) (rhs : t) : t = Bin (Minus, lhs, rhs)

let sum (s : Set_range.t) (e : t) =
  match (Set_range.count s, e) with
  | Some n1, Const n2 -> Const (n1 * n2)
  | _, _ -> Sum (s, e)

let plus (lhs : t) (rhs : t) : t =
  match (lhs, rhs) with
  | Const lhs, Const rhs -> Const (lhs + rhs)
  | Const 0, e | e, Const 0 -> e
  | _, _ -> Bin (Plus, lhs, rhs)

let max (lhs : t) (rhs : t) : t =
  match (lhs, rhs) with
  | Const lhs, Const rhs -> Const (max lhs rhs)
  | _, _ -> Bin (Max, lhs, rhs)

let min (lhs : t) (rhs : t) : t =
  match (lhs, rhs) with
  | Const lhs, Const rhs -> Const (min lhs rhs)
  | _, _ -> Bin (Min, lhs, rhs)

let rec to_string : t -> string = function
  | If (b, p, q) ->
      "if (" ^ Reals.b_to_string b ^ ") then " ^ to_string p ^ " else "
      ^ to_string q
  | Clamp { value; upper_bound } ->
      "min(" ^ Reals.to_string value ^ ", " ^ string_of_int upper_bound ^ ")"
  | Const x -> string_of_int x
  | Sum (b, s) -> "Σ_" ^ Set_range.to_string b ^ " " ^ to_string s
  | Bin (Minus, lhs, rhs) -> "(" ^ to_string lhs ^ " - " ^ to_string rhs ^ ")"
  | Bin (Plus, lhs, rhs) -> "(" ^ to_string lhs ^ " + " ^ to_string rhs ^ ")"
  | Bin (o, lhs, rhs) ->
      Op.to_string o ^ "(" ^ to_string lhs ^ ", " ^ to_string rhs ^ ")"

let subst ((x, v) : Variable.t * Reals.t) : t -> t =
  let rec subst : t -> t = function
    | Const _ as e -> e
    | Clamp { value; upper_bound } ->
        Clamp { value = Reals.i_subst (x, v) value; upper_bound }
    | Bin (o, lhs, rhs) -> Bin (o, subst lhs, subst rhs)
    | If (b, p, q) -> If (Reals.b_subst (x, v) b, subst p, subst q)
    | Sum (b, p) ->
        let b = Set_range.subst (x, v) b in
        Sum (b, if Variable.equal b.var x then p else subst p)
  in
  subst

let adapt_error (r : ('a, string) Result.t) : ('a, Errors.t) Result.t =
  r
  |> Result.map_error (fun e ->
      let open Errors in
      { output = e; reason = UnsupportedInput })

let range_sum (r : Range.t) (s : t) : t =
  if s = Const 0 then Const 0
  else
    let o, b = Set_range.from_range r in
    match o with
    | Some new_range_var ->
        let s = subst (r.var, new_range_var) s in
        sum b s
    | None -> sum b s

module Strategy = struct
  type stmt = t
  type t = Max | Min

  let choice : t -> stmt -> stmt -> stmt = function Max -> max | Min -> min
end

let from_stmt ?(strategy = Strategy.Max) : Stmt.t -> t =
  let rec from_stmt : Stmt.t -> t = function
    | Stmt.Tick k -> Const k
    | Clamp { value; upper_bound } ->
        Clamp { value = Reals.from_nexp value; upper_bound }
    | Skip -> Const 0
    | If (b, p, q) -> If (Reals.from_bexp b, from_stmt p, from_stmt q)
    | Seq (p, q) -> plus (from_stmt p) (from_stmt q)
    | Loop { range = r; body = p } -> range_sum r (from_stmt p)
    | Choice (lhs, rhs) ->
        Strategy.choice strategy (from_stmt lhs) (from_stmt rhs)
  in
  from_stmt

let run ~show_code (r : Stmt.t) : string =
  let s = from_stmt r in
  if show_code then to_string s |> print_endline else ();
  to_string s
