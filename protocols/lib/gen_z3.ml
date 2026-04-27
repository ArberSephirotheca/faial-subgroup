open Stage0
open Exp
module Expr = Z3.Expr
module Boolean = Z3.Boolean
module Arithmetic = Z3.Arithmetic
module Integer = Z3.Arithmetic.Integer
module BitVector = Z3.BitVector

exception Preprocessing_error of string
exception Not_implemented of string

module type GEN = sig
  val n_to_expr : Z3.context -> nexp -> Expr.expr
  val b_to_expr : Z3.context -> bexp -> Expr.expr
  val parse_num : string -> int
end

type binop = Z3.context -> Expr.expr -> Expr.expr -> Expr.expr
type unop = Z3.context -> Expr.expr -> Expr.expr

let bitvector_to_hex (x : string) : string option =
  if x = "" || x.[0] <> '#' then None
  else
    Some
      (let orig_x = x in
       (* Input is: #x0000004000000000 *)
       let offset n x =
         let len = String.length x - n in
         if len < 0 then
           raise (Invalid_argument (Printf.sprintf "parse_num: %s" orig_x))
         else String.sub x n len
       in
       (* We need to remove the prefix #x; input becomes: 0000004000000000 *)
       let x = offset 2 x in
       (* Removes the prefix: #x *)
       (* Then we need to remove the prefix 0s,
        otherwise Int32.of_string doesn't like it.
      Input becomes: 4000000000 *)
       let rec trim_0 x =
         if String.length x > 0 && String.get x 0 = '0' then trim_0 (offset 1 x)
         else x
       in
       (* Prefix it with a 0x so that Int64.of_string knows it's an hex *)
       let x = "0x" ^ trim_0 x in
       (* Finally, convert it into an int64 (signed),
        and then render it back to a string, as this is for display only *)
       if x = "0x" then "0x0" else x)

(* We define an abstract module to handle numeric operations
   so that we can support arbitrary backends. *)
module type NUMERIC_OPS = sig
  val mk_var : Z3.context -> string -> Expr.expr
  val mk_num : Z3.context -> int -> Expr.expr
  val mk_bit_and : binop
  val mk_bit_or : binop
  val mk_bit_xor : binop
  val mk_left_shift : binop
  val mk_right_shift : binop
  val mk_plus : binop
  val mk_minus : binop
  val mk_mult : binop
  val mk_div : binop
  val mk_mod : binop
  val mk_le : binop
  val mk_ge : binop
  val mk_gt : binop
  val mk_lt : binop
  val mk_not : unop
  val mk_unary_minus : unop
  val parse_num : string -> string
end

module ArithmeticOps : NUMERIC_OPS = struct
  let missing (name : string) : binop =
   fun _ _ _ -> raise (Not_implemented name)

  let missing1 (name : string) : unop = fun _ _ -> raise (Not_implemented name)
  let mk_var = Integer.mk_const_s
  let mk_num = Arithmetic.Integer.mk_numeral_i
  let mk_bit_and = missing "&"
  let mk_bit_or = missing "|"
  let mk_bit_xor = missing "^"
  let mk_left_shift = missing "<<"
  let mk_right_shift = missing ">>"
  let mk_plus ctx n1 n2 = Arithmetic.mk_add ctx [ n1; n2 ]
  let mk_minus ctx n1 n2 = Arithmetic.mk_sub ctx [ n1; n2 ]
  let mk_mult ctx n1 n2 = Arithmetic.mk_mul ctx [ n1; n2 ]
  let mk_div = Arithmetic.mk_div
  let mk_mod = Arithmetic.Integer.mk_mod
  let mk_le = Arithmetic.mk_le
  let mk_ge = Arithmetic.mk_ge
  let mk_gt = Arithmetic.mk_gt
  let mk_lt = Arithmetic.mk_lt
  let mk_unary_minus = Arithmetic.mk_unary_minus
  let mk_not = missing1 "~"
  let parse_num (x : string) = x
end

module type WordSize = sig
  val word_size : int
  val decode_hex : string -> string
end

module W32 = struct
  let word_size = 32
  let decode_hex x = Int32.of_string x |> Int32.to_string
end

module SIGNED_32 = struct
  let word_size = 32
  let max_int32 = Int32.max_int |> Int64.of_int32

  (*
    Bit-vector maximization has no notion of signedness.
    The following constrain guarantees that the goal being maximized
    is a signed-positive number.

    https://stackoverflow.com/questions/64484347/
  *)
  let decode_hex x = Int64.(sub (sub (of_string x) max_int32) one |> to_string)
end

module W64 = struct
  let word_size = 64
  let decode_hex x = Int64.of_string x |> Int64.to_string
end

module W16 = struct
  let word_size = 16
  let decode_hex x = Int32.of_string x |> Int32.to_string
end

module W8 = struct
  let word_size = 8
  let decode_hex x = Int32.of_string x |> Int32.to_string
end

module BitVectorOps (W : WordSize) = struct
  let mk_var ctx x = BitVector.mk_const_s ctx x W.word_size
  let mk_num ctx n = BitVector.mk_numeral ctx (string_of_int n) W.word_size
  let mk_bit_and = BitVector.mk_and
  let mk_bit_or = BitVector.mk_or
  let mk_bit_xor = BitVector.mk_xor
  let mk_left_shift = BitVector.mk_shl
  let mk_right_shift = BitVector.mk_ashr
  let mk_minus = BitVector.mk_sub
  let mk_plus = BitVector.mk_add
  let mk_mult = BitVector.mk_mul
  let mk_div = BitVector.mk_sdiv
  let mk_mod = BitVector.mk_smod
  let mk_le = BitVector.mk_sle
  let mk_ge = BitVector.mk_sge
  let mk_gt = BitVector.mk_sgt
  let mk_lt = BitVector.mk_slt
  let mk_not = BitVector.mk_not
  let mk_unary_minus = BitVector.mk_neg

  let parse_num x =
    x |> bitvector_to_hex |> Option.map W.decode_hex |> Option.value ~default:x
end

(* Convert a declaration to a variable *)
let decl_to_variable (d : Z3.FuncDecl.func_decl) : Variable.t =
  d |> Z3.FuncDecl.get_name |> Z3.Symbol.get_string |> Variable.from_name

module Solver = struct
  open Z3

  type t = Sat of Model.model | Unsat

  let to_string : t -> string = function
    | Sat m -> Printf.sprintf "SAT(%s)" (Model.to_string m)
    | Unsat -> "UNSAT"

  let is_sat : t -> bool = function Sat _ -> true | Unsat -> false
  let is_unsat : t -> bool = function Sat _ -> false | Unsat -> true

  let of_status (solver : Z3.Solver.solver) :
      Z3.Solver.status -> (t, string) Result.t = function
    | SATISFIABLE -> (
        match Solver.get_model solver with
        | Some model -> Ok (Sat model)
        | None ->
            raise
              (Invalid_argument
                 "Satisfiable result but no model available - check solver \
                  configuration"))
    | UNSATISFIABLE -> Ok Unsat
    | UNKNOWN -> Error (Solver.get_reason_unknown solver)

  let run (solver : Solver.solver) : (t, string) Result.t =
    Solver.check solver [] |> of_status solver
end

module Params = struct
  module Value = struct
    type t = Bool of bool | Int of int | Float of float | String of string

    let to_string : t -> string = function
      | Bool true -> "true"
      | Bool false -> "false"
      | Int i -> string_of_int i
      | Float f -> string_of_float f
      | String s -> s

    let add_param (ctx : Z3.context) (params : Z3.Params.params) (name : string)
        : t -> unit = function
      | Bool b -> Z3.Params.add_bool params (Z3.Symbol.mk_string ctx name) b
      | Int i -> Z3.Params.add_int params (Z3.Symbol.mk_string ctx name) i
      | Float f -> Z3.Params.add_float params (Z3.Symbol.mk_string ctx name) f
      | String s ->
          Z3.Params.add_symbol params
            (Z3.Symbol.mk_string ctx name)
            (Z3.Symbol.mk_string ctx s)
  end

  type t = (string * Value.t) list

  let to_z3 (ctx : Z3.context) (param_list : t) : Z3.Params.params =
    let z3_params = Z3.Params.mk_params ctx in
    List.iter
      (fun (name, value) -> Value.add_param ctx z3_params name value)
      param_list;
    z3_params

  let to_string (param_list : t) : string =
    param_list
    |> List.map (fun (name, value) ->
        Printf.sprintf "%s=%s" name (Value.to_string value))
    |> String.concat ", " |> Printf.sprintf "{%s}"
end

module Probe = struct
  type t =
    | Probe of string
    | Const of float
    | And of { left : t; right : t }
    | Or of { left : t; right : t }
    | Not of t

  let rec to_z3 (ctx : Z3.context) : t -> Z3.Probe.probe = function
    | Const f -> Z3.Probe.const ctx f
    | Probe name -> Z3.Probe.mk_probe ctx name
    | And { left; right } ->
        Z3.Probe.and_ ctx (to_z3 ctx left) (to_z3 ctx right)
    | Or { left; right } -> Z3.Probe.or_ ctx (to_z3 ctx left) (to_z3 ctx right)
    | Not probe -> Z3.Probe.not_ ctx (to_z3 ctx probe)

  let rec to_string : t -> string = function
    | Const f -> Printf.sprintf "%f" f
    | Probe name -> Printf.sprintf "(probe \"%s\")" name
    | And { left; right } ->
        Printf.sprintf "(and %s %s)" (to_string left) (to_string right)
    | Or { left; right } ->
        Printf.sprintf "(or %s %s)" (to_string left) (to_string right)
    | Not probe -> Printf.sprintf "(not %s)" (to_string probe)
end

module Tactic = struct
  type t =
    | Tactic of string
    | AndThen of { first : t; second : t }
    | OrElse of { first : t; fallback : t }
    | TryFor of { timeout_ms : int; body : t }
    | Repeat of { body : t; max_iterations : int }
    | ParOr of t list
    | ParAndThen of { first : t; second : t }
    | Cond of { probe : Probe.t; then_tactic : t; else_tactic : t }
    | FailIfNotDecided
    | UsingParams of { params : Params.t; body : t }
    | Skip
    | Fail
    | PrintGoals
    | Print of string

  let and_then (first : t) (second : t) : t =
    match first with Skip -> second | _ -> AndThen { first; second }

  let and_then_ex (l : t list) : t = l |> List.fold_left and_then Skip

  let rec to_z3 (ctx : Z3.context) : t -> Z3.Tactic.tactic = function
    | Tactic name -> Z3.Tactic.mk_tactic ctx name
    | AndThen { first; second } ->
        Z3.Tactic.and_then ctx (to_z3 ctx first) (to_z3 ctx second) []
    | OrElse { first; fallback } ->
        Z3.Tactic.or_else ctx (to_z3 ctx first) (to_z3 ctx fallback)
    | TryFor { timeout_ms; body } ->
        Z3.Tactic.try_for ctx (to_z3 ctx body) timeout_ms
    | Repeat { body; max_iterations } ->
        Z3.Tactic.repeat ctx (to_z3 ctx body) max_iterations
    | ParOr tactics -> Z3.Tactic.par_or ctx (List.map (to_z3 ctx) tactics)
    | ParAndThen { first; second } ->
        Z3.Tactic.par_and_then ctx (to_z3 ctx first) (to_z3 ctx second)
    | Cond { probe; then_tactic; else_tactic } ->
        Z3.Tactic.cond ctx (Probe.to_z3 ctx probe) (to_z3 ctx then_tactic)
          (to_z3 ctx else_tactic)
    | FailIfNotDecided -> Z3.Tactic.fail_if_not_decided ctx
    | UsingParams { params; body } ->
        Z3.Tactic.using_params ctx (to_z3 ctx body) (Params.to_z3 ctx params)
    | Skip -> Z3.Tactic.skip ctx
    | Fail -> Z3.Tactic.fail ctx
    | PrintGoals | Print _ -> Z3.Tactic.skip ctx

  let rec to_string : t -> string = function
    | Tactic name -> Printf.sprintf "(tactic \"%s\")" name
    | AndThen { first; second } ->
        Printf.sprintf "(and-then %s %s)" (to_string first) (to_string second)
    | OrElse { first; fallback } ->
        Printf.sprintf "(or-else %s %s)" (to_string first) (to_string fallback)
    | TryFor { timeout_ms; body } ->
        Printf.sprintf "(try-for %d %s)" timeout_ms (to_string body)
    | Repeat { body; max_iterations } ->
        Printf.sprintf "(repeat %s %d)" (to_string body) max_iterations
    | ParOr tactics ->
        let tactics_str = String.concat " " (List.map to_string tactics) in
        Printf.sprintf "(par-or %s)" tactics_str
    | ParAndThen { first; second } ->
        Printf.sprintf "(par-and-then %s %s)" (to_string first)
          (to_string second)
    | Cond { probe; then_tactic; else_tactic } ->
        Printf.sprintf "(cond %s %s %s)" (Probe.to_string probe)
          (to_string then_tactic) (to_string else_tactic)
    | FailIfNotDecided -> "fail-if-not-decided"
    | UsingParams { params; body } ->
        Printf.sprintf "(using-params %s %s)" (Params.to_string params)
          (to_string body)
    | Skip -> "skip"
    | Fail -> "fail"
    | PrintGoals -> "print-goals"
    | Print s -> Printf.sprintf "(print %s)" s
end

module Debugger = struct
  type goal = Z3.Goal.goal
  type t = { work : Tactic.t list; goals : goal list }

  let to_string (st : t) : string =
    let goals =
      st.goals
      |> List.mapi (fun i g ->
          Printf.sprintf "Goal %d:\n%s" i (Z3.Goal.to_string g))
      |> String.concat "\n"
    in
    let work = st.work |> List.map Tactic.to_string |> String.concat ";" in
    goals ^ work

  let apply (tactic : Z3.Tactic.tactic) (goal : goal) : goal list =
    Z3.Tactic.apply tactic goal None
    (* Convert apply_result to list of subgoals *)
    |> Z3.Tactic.ApplyResult.get_subgoals

  let apply_all (tactic : Z3.Tactic.tactic) : goal list -> goal list =
    List.concat_map (apply tactic)

  let check (solver : Z3.Solver.solver) (goals : goal list) : Z3.Solver.status =
    goals |> List.map Z3.Goal.get_formulas |> List.iter (Z3.Solver.add solver);
    Z3.Solver.check solver []

  let step (ctx : Z3.context) (work : Tactic.t list) (goals : goal list) :
      Tactic.t -> t list = function
    | Cond { probe; then_tactic; else_tactic } ->
        let probe = Probe.to_z3 ctx probe in
        let then_goals, else_goals =
          List.partition (fun g -> Z3.Probe.apply probe g <> 0.0) goals
        in
        [
          { work = then_tactic :: work; goals = then_goals };
          { work = else_tactic :: work; goals = else_goals };
        ]
        |> List.filter (fun x -> x.goals <> [])
        (* <- filter out any empty goals *)
    | AndThen { first; second } -> [ { work = first :: second :: work; goals } ]
    | Skip -> [ { work; goals } ]
    | PrintGoals ->
        print_endline (to_string { work; goals });
        flush stdout;
        [ { work; goals } ]
    | Print s ->
        print_string s;
        flush stdout;
        [ { work; goals } ]
    | p ->
        let p = Tactic.to_z3 ctx p in
        [ { work; goals = apply_all p goals } ]

  let debug (ctx : Z3.context) (solver : Z3.Solver.solver) :
      Z3.Expr.expr -> Tactic.t -> Z3.Solver.status =
    let rec iter : t list -> Z3.Solver.status = function
      | [] -> Z3.Solver.UNSATISFIABLE
      | { work = []; goals } :: st -> (
          (*
          For satisfiability (SAT):
            - The objective is to find any goal that is satisfiable
            - If any single goal can be satisfied, the overall result is SAT
            - This represents a logical OR - you only need one branch to be
            satisfiable
            - An UNKNOWN counts as an error, so execution also "aborts" by
              not recursing.
        *)
          match check solver goals with
          | Z3.Solver.UNSATISFIABLE -> iter st
          | s -> s)
      | { work = tac :: work; goals } :: st ->
          let st' = step ctx work goals tac in
          iter (st' @ st)
    in
    fun expr tac ->
      let goal = Z3.Goal.mk_goal ctx true false false in
      Z3.Goal.add goal [ expr ];
      iter [ { goals = [ goal ]; work = [ tac ] } ]
end

module Optimizer = struct
  open Z3

  module Strategy = struct
    type t = Maximize | Minimize

    let to_string : t -> string = function
      | Maximize -> "max"
      | Minimize -> "min"
  end

  type t = Sat of { model : Model.model; optimal : Expr.expr } | Unsat

  let run (opt : Optimize.optimize) (strategy : Strategy.t) (n : Expr.expr) :
      (t, string) Result.t =
    let handle =
      match strategy with
      | Maximize -> Optimize.maximize opt n
      | Minimize -> Optimize.minimize opt n
    in
    match Optimize.check opt with
    | SATISFIABLE -> (
        match Optimize.get_model opt with
        | Some model ->
            let optimal = Optimize.get_lower handle in
            Ok (Sat { model; optimal })
        | None ->
            raise
              (Invalid_argument
                 "Satisfiable result but no model available - check optimizer \
                  configuration"))
    | UNSATISFIABLE -> Ok Unsat
    | UNKNOWN -> Error (Optimize.get_reason_unknown opt)
end

module type Z3_SOLVER = sig
  val solve : ?timeout:int -> Exp.bexp -> (Solver.t, string) Result.t

  val solve_with_tactic :
    ?timeout:int ->
    ?debug:bool ->
    Tactic.t ->
    Exp.bexp ->
    (Solver.t, string) Result.t

  val optimize_expr :
    ?timeout:int ->
    ?pre:Exp.bexp ->
    Optimizer.Strategy.t ->
    Exp.nexp ->
    (int option, string) Result.t
end

module CodeGen (N : NUMERIC_OPS) = struct
  let parse_num = N.parse_num
  let preprocessing_error msg = raise (Preprocessing_error msg)

  let nbin_to_expr :
      N_binary.t -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
    | BitAnd -> N.mk_bit_and
    | BitOr -> N.mk_bit_or
    | BitXOr -> N.mk_bit_xor
    | LeftShift -> N.mk_left_shift
    | RightShift -> N.mk_right_shift
    | Plus -> N.mk_plus
    | Minus -> N.mk_minus
    | Mult -> N.mk_mult
    | Div -> N.mk_div
    | Mod -> N.mk_mod

  let nrel_to_expr :
      N_rel.t -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
    | Eq -> Boolean.mk_eq
    | Neq -> fun ctx n1 n2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx n1 n2)
    | Le -> N.mk_le
    | Ge -> N.mk_ge
    | Lt -> N.mk_lt
    | Gt -> N.mk_gt

  let brel_to_expr :
      B_rel.t -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
    | BOr -> fun ctx b1 b2 -> Boolean.mk_or ctx [ b1; b2 ]
    | BAnd -> fun ctx b1 b2 -> Boolean.mk_and ctx [ b1; b2 ]

  let rec n_to_expr (ctx : Z3.context) : nexp -> Expr.expr = function
    | Var x -> Variable.name x |> N.mk_var ctx
    | CastInt b -> n_to_expr ctx (n_if b (Num 1) (Num 0))
    | Unary (BitNot, n) -> N.mk_not ctx (n_to_expr ctx n)
    | Unary (Negate, n) -> N.mk_unary_minus ctx (n_to_expr ctx n)
    | Other n ->
        let n : string = Exp.n_to_string n in
        raise (Not_implemented ("n_to_expr: not implemented for Other of " ^ n))
    | NCall _ as c ->
        preprocessing_error
          ("b_to_expr: invoke Predicates.inline to remove predicates: "
         ^ n_to_string c)
    | Num (n : int) -> N.mk_num ctx n
    | Binary (op, n1, n2) ->
        (nbin_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
    | NIf (b, n1, n2) ->
        Boolean.mk_ite ctx (b_to_expr ctx b) (n_to_expr ctx n1)
          (n_to_expr ctx n2)

  and b_to_expr (ctx : Z3.context) : bexp -> Expr.expr = function
    | Bool (b : bool) -> Boolean.mk_val ctx b
    | CastBool n -> b_to_expr ctx (n_neq n (Num 0))
    | NRel (op, n1, n2) ->
        (nrel_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
    | BRel (op, b1, b2) ->
        (brel_to_expr op) ctx (b_to_expr ctx b1) (b_to_expr ctx b2)
    | BNot (b : bexp) -> Boolean.mk_not ctx (b_to_expr ctx b)
    | Pred _ as c ->
        preprocessing_error
          ("b_to_expr: invoke Predicates.inline to remove predicates: "
         ^ b_to_string c)
    | Distinct exprs ->
        let z3_exprs = List.map (n_to_expr ctx) exprs in
        Boolean.mk_distinct ctx z3_exprs

  let ( let* ) = Option.bind

  (* Get's the value of a symbolic (integer) expression *)
  let get_int (m : Z3.Model.model) (e : Z3.Expr.expr) : int option =
    (* Evaluate the expression in the model *)
    let* v = Z3.Model.eval m e true in
    (* Try to cast the result to an integer, returning none upon failure *)
    try Some (Expr.to_string v |> parse_num |> int_of_string)
    with Failure _ -> None

  let get_int_decl (m : Z3.Model.model) (d : Z3.FuncDecl.func_decl) : int option
      =
    (* Variables in the model are actually functions with
      0 args, so we create a function call *)
    (* We then evaluate the function call *)
    get_int m (Z3.FuncDecl.apply d [])

  (* Tries to get all variables in the set.
     Missed variable are skipped. *)
  let get_all (m : Z3.Model.model) (vars : Variable.Set.t) :
      (Variable.t * int) list =
    (* Go through all declarations of the model *)
    Z3.Model.get_const_decls m
    |> List.filter_map (fun d ->
        let x = decl_to_variable d in
        if Variable.Set.mem x vars then
          let* v = get_int_decl m d in
          Some (x, v)
        else None)

  (*
    Optimizes a numeric expression given a boolean expression.

    The [timeout] parameter is forwarded to Z3 as a context-level
    "timeout" parameter, expressed in milliseconds. It applies to a
    single optimizer call only — every invocation of [optimize] /
    [optimize_expr] / [solve] / [solve_with_tactic] starts its own
    Z3 context and gets its own independent timeout budget. Callers
    that issue multiple queries per analysis step (e.g. computing a
    max and a min, or running [equals] which does both) will see a
    total wall-clock cost of [N * timeout] in the worst case where
    every query saturates the budget. There is no global cap.
    *)
  let optimize ?(timeout = 0) (* per-Z3-call timeout in ms; 0 = unlimited *)
      (strategy : Optimizer.Strategy.t) (pre : Exp.bexp) (n : Exp.nexp) :
      (Optimizer.t, string) Result.t =
    let open Z3 in
    let args =
      if timeout > 0 then [ ("timeout", string_of_int timeout) ] else []
    in
    let ctx = mk_context args in
    let opt = Optimize.mk_opt ctx in
    Optimize.add opt [ b_to_expr ctx pre ];
    let n = n_to_expr ctx n in
    Optimizer.run opt strategy n

  let optimize_expr ?(timeout = 0) (* By default no timeout is given *)
      ?(pre = Bool true) (strategy : Optimizer.Strategy.t) (n : Exp.nexp) :
      (int option, string) Result.t =
    optimize ~timeout strategy pre n
    |> Result.map (function
      | Optimizer.Sat { optimal; model } ->
          Some (get_int model optimal |> Option.get)
      | Unsat -> None)

  let solve ?(timeout = 0) (pre : Exp.bexp) : (Solver.t, string) Result.t =
    let args =
      if timeout > 0 then [ ("timeout", string_of_int timeout) ] else []
    in
    let ctx = Z3.mk_context args in
    let solver = Z3.Solver.mk_solver ctx None in
    Z3.Solver.add solver [ b_to_expr ctx pre ];
    Solver.run solver

  let solve_with_tactic ?(timeout = 0) ?(debug = false) (tactic : Tactic.t)
      (pre : Exp.bexp) : (Solver.t, string) Result.t =
    let args =
      if timeout > 0 then [ ("timeout", string_of_int timeout) ] else []
    in
    let ctx = Z3.mk_context args in
    let goal = b_to_expr ctx pre in

    if debug then
      (* Debugging mode: Manual goal/tactic application for detailed info *)
      try
        let solver = Z3.Solver.mk_solver ctx None in
        Debugger.debug ctx solver goal tactic |> Solver.of_status solver
      with Z3.Error msg ->
        (* Z3 tactic failure - convert to proper Unknown result *)
        Error ("Tactic '" ^ Tactic.to_string tactic ^ "' failed: " ^ msg)
    else
      (* Production mode: Direct tactic-to-solver conversion *)
      try
        let z3_tactic = Tactic.to_z3 ctx tactic in
        let solver = Z3.Solver.mk_solver_t ctx z3_tactic in
        Z3.Solver.add solver [ goal ];
        Solver.run solver
      with Z3.Error msg ->
        (* Z3 tactic creation/solving failure *)
        Error ("Tactic '" ^ Tactic.to_string tactic ^ "' failed: " ^ msg)
end

module SignedBitVectorOps (W : WordSize) = struct
  (*
    Bit-vector maximization has no notion of signedness.
    The following constrain guarantees that the goal being maximized
    is a signed-positive number.

    https://stackoverflow.com/questions/64484347/
  *)
  let offset = Common.pow ~base:2 (W.word_size - 1)
  let mk_var ctx x = BitVector.mk_const_s ctx x W.word_size
  let mk_sort ctx = BitVector.mk_sort ctx W.word_size

  let mk_num ctx n =
    let n = n + offset in
    BitVector.mk_numeral ctx (string_of_int n) W.word_size

  let mk_bit_and = BitVector.mk_and
  let mk_bit_or = BitVector.mk_or
  let mk_bit_xor = BitVector.mk_xor
  let mk_left_shift = BitVector.mk_shl
  let mk_right_shift = BitVector.mk_ashr
  let mk_minus = BitVector.mk_sub
  let mk_plus = BitVector.mk_add
  let mk_mult = BitVector.mk_mul
  let mk_div = BitVector.mk_sdiv
  let mk_mod = BitVector.mk_smod
  let mk_le = BitVector.mk_sle
  let mk_ge = BitVector.mk_sge
  let mk_gt = BitVector.mk_sgt
  let mk_lt = BitVector.mk_slt
  let mk_not = BitVector.mk_not
  let mk_unary_minus = BitVector.mk_neg

  let parse_num x =
    x |> bitvector_to_hex |> Option.map W.decode_hex |> Option.value ~default:x
end

module IntGen = CodeGen (ArithmeticOps)
module Bv32Gen = CodeGen (BitVectorOps (W32))
module Bv64Gen = CodeGen (BitVectorOps (W64))
module SignedBv32Gen = CodeGen (SignedBitVectorOps (SIGNED_32))
