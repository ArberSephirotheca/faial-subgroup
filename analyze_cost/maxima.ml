open Stage0
open Protocols

let brel_to_string : B_rel.t -> string = function BOr -> "or" | BAnd -> "and"

let tr_op_to_string : Reals.TruncateOp.t -> string = function
  | Ceiling -> "ceiling"
  | Floor -> "floor"

let par (s : string) : string = "(" ^ s ^ ")"

let call (name : string) (args : string list) : string =
  name ^ "(" ^ String.concat "," args ^ ")"

let infix ~lhs ~op ~rhs : string = "(" ^ lhs ^ " " ^ op ^ " " ^ rhs ^ ")"

let if_ ~cond ~then_branch ~else_branch : string =
  Printf.sprintf "(if (%s) then %s else %s)" cond then_branch else_branch

let rec i_to_string : Reals.integer -> string = function
  | Var x ->
      let x =
        Variable.name x
        |> Common.replace ~substring:"." ~by:"__"
        |> Common.replace ~substring:"@" ~by:"__"
      in
      if x = "step" then "__step__" else x
  | Num x -> string_of_int x
  | FloatToInt (o, e) -> tr_op_to_string o ^ "(" ^ f_to_string e ^ ")"
  | Binary (Div, e1, e2) ->
      call "floor" [ infix ~op:"/" ~lhs:(i_to_string e1) ~rhs:(i_to_string e2) ]
  | Binary (op, lhs, rhs) ->
      let lhs = i_to_string lhs in
      let rhs = i_to_string rhs in
      let is_infix =
        match op with
        | Plus | Minus | Mult | Div | Pow -> true
        | Mod | LeftShift | RightShift | BitXOr | BitOr | BitAnd -> false
      in
      let op =
        match op with
        | Plus -> "+"
        | Minus -> "-"
        | Mult -> "*"
        | Div -> "/" (* XXX: integer division *)
        | Mod -> "mod"
        | LeftShift -> "bit_lsh"
        | RightShift -> "bit_rsh"
        | BitXOr -> "bit_xor"
        | BitOr -> "bit_or"
        | BitAnd -> "bit_and"
        | Pow -> "^"
      in
      if is_infix then infix ~lhs ~op ~rhs else call op [ lhs; rhs ]
  | Unary (BitNot, e) -> call "bit_not" [ i_to_string e ]
  | Unary (Negate, e) -> call "-" [ i_to_string e ]
  | If (b, e1, e2) ->
      if_ ~cond:(b_to_string b) ~then_branch:(i_to_string e1)
        ~else_branch:(i_to_string e2)
  | BoolToInt e -> i_to_string (If (e, Num 1, Num 0))

and b_to_string : Reals.boolean -> string = function
  | Bool true -> "true"
  | Bool false -> "false"
  | NRel (Eq, e1, e2) -> call "equal" [ i_to_string e1; i_to_string e2 ]
  | NRel (Neq, e1, e2) -> call "notequal" [ i_to_string e1; i_to_string e2 ]
  | NRel (op, lhs, rhs) ->
      infix ~lhs:(i_to_string lhs) ~op:(N_rel.to_string op)
        ~rhs:(i_to_string rhs)
  | BRel (BOr, e1, e2) ->
      infix ~lhs:(b_to_string e1) ~op:"or" ~rhs:(b_to_string e2)
  | BRel (BAnd, e1, e2) ->
      infix ~lhs:(b_to_string e1) ~op:"and" ~rhs:(b_to_string e2)
  | BNot e -> Printf.sprintf "(not (%s))" (b_to_string e)
  | IntToBool e -> call "notequal" [ i_to_string e; "0" ]

and f_to_string : Reals.floating_point -> string = function
  | Float f -> string_of_float f
  | Log (b, e) ->
      infix
        ~lhs:(call "log" [ f_to_string e ])
        ~op:"/"
        ~rhs:(call "log" [ i_to_string b ])
  | IntToFloat e -> i_to_string e

let max (lhs : string) (rhs : string) : string = call "max" [ lhs; rhs ]
let min (lhs : string) (rhs : string) : string = call "min" [ lhs; rhs ]

let clamp ~value ~upper_bound : string =
  min (max (i_to_string value) "0") (string_of_int upper_bound)

let rec from_summation : Summation.t -> string = function
  | Const k -> string_of_int k
  | Clamp { value; upper_bound } -> clamp ~value ~upper_bound
  | If (b, p, q) ->
      if_ ~cond:(b_to_string b) ~then_branch:(from_summation p)
        ~else_branch:(from_summation q)
  | Sum (b, s) ->
      call "sum"
        [
          from_summation s;
          i_to_string (Var b.var);
          i_to_string b.lower_bound;
          i_to_string b.upper_bound;
        ]
  | Bin (op, lhs, rhs) ->
      let lhs = from_summation lhs in
      let rhs = from_summation rhs in
      let is_infix =
        match op with Plus | Minus | Mult | Div -> true | Max | Min -> false
      in
      let op =
        match op with
        | Plus -> "+"
        | Minus -> "-"
        | Mult -> "*"
        | Div -> "/" (* XXX: integer division *)
        | Max -> "max"
        | Min -> "min"
      in
      if is_infix then infix ~lhs ~op ~rhs else call op [ lhs; rhs ]

let from_stmt ?(strategy = Summation.Strategy.Max) (r : Ra.Stmt.t) : string =
  Summation.from_stmt ~strategy r |> from_summation

let parse_maxima (x : string) : string option =
  if Common.contains ~substring:"incorrect syntax" x then None
  else
    Some
      (let lines = String.split_on_char '\n' x in
       let max_len = List.map String.length lines |> List.fold_left Int.max 0 in
       let offsets =
         lines
         |> List.filter_map (fun line ->
                String.to_seqi line
                |> Seq.find (fun (_, a) -> a <> ' ')
                |> Option.map fst)
       in
       let min_offset = List.fold_left Int.min max_len offsets in
       lines
       |> List.map (fun line ->
              Slice.from_start min_offset |> Slice.substring line)
       |> String.concat "\n")

let compile ?(compact = false) (code : string) : string =
  "load(\"bitwise\")$\n"
  ^ (if compact then "pfeformat: true$\ndisplay2d: false$\n" else "")
  ^ code ^ ",logcontract,simpsum,ratsimp;"

let run_exe ?(verbose = false) ?(exe = "maxima") (expr : string) :
    (string, Errors.t) Result.t =
  if verbose then prerr_endline ("maxima output:\n" ^ expr ^ "\n") else ();
  Subprocess.make exe [ "--very-quiet"; "--disable-readline" ]
  |> Subprocess.run_combine ~stdin:expr
  |> Errors.handle_result parse_maxima

let run_ratio ~verbose ~exe ~compact ~numerator ~denominator :
    (string, Errors.t) Result.t =
  if Ra.Stmt.is_zero denominator then Ok "0"
  else
    let open Summation in
    let s =
      Bin (Div, Summation.from_stmt numerator, Summation.from_stmt denominator)
    in
    s |> to_string |> compile ~compact |> run_exe ~verbose ~exe

let run ?(verbose = false) ?(exe = "maxima") ?(compact = false) (x : Ra.Stmt.t)
    : (string, Errors.t) Result.t =
  x |> from_stmt |> compile ~compact |> run_exe ~verbose ~exe
