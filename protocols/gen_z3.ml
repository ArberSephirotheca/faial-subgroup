open Stage0

open Exp

module Expr = Z3.Expr
module Boolean = Z3.Boolean
module Arithmetic = Z3.Arithmetic
module Integer = Z3.Arithmetic.Integer
module BitVector = Z3.BitVector

exception Not_implemented of string

module type GEN = sig
  val n_to_expr: Z3.context -> nexp -> Expr.expr
  val b_to_expr: Z3.context -> bexp -> Expr.expr
  val parse_num: string -> int
end

type binop = Z3.context -> Expr.expr -> Expr.expr -> Expr.expr
type unop = Z3.context -> Expr.expr -> Expr.expr

(* We define an abstract module to handle numeric operations
   so that we can support arbitrary backends. *)
module type NUMERIC_OPS = sig
  val mk_var: Z3.context -> string -> Expr.expr
  val mk_num: Z3.context -> int -> Expr.expr
  val mk_bit_and: binop
  val mk_bit_or: binop
  val mk_bit_xor: binop
  val mk_left_shift: binop
  val mk_right_shift: binop
  val mk_plus: binop
  val mk_minus: binop
  val mk_mult: binop
  val mk_div: binop
  val mk_mod: binop
  val mk_le: binop
  val mk_ge: binop
  val mk_gt: binop
  val mk_lt: binop
  val mk_not: unop
  val mk_unary_minus: unop
  val parse_num: string -> string
end

module ArithmeticOps : NUMERIC_OPS = struct
  let missing (name:string) : binop = fun _ _ _ -> raise (Not_implemented name)
  let missing1 (name:string) : unop = fun _ _ -> raise (Not_implemented name)
  let mk_var = Integer.mk_const_s
  let mk_num = Arithmetic.Integer.mk_numeral_i
  let mk_bit_and = missing "&"
  let mk_bit_or = missing "|"
  let mk_bit_xor =  missing "^"
  let mk_left_shift = missing "<<"
  let mk_right_shift = missing ">>"
  let mk_plus ctx n1 n2 = Arithmetic.mk_add ctx [n1; n2]
  let mk_minus ctx n1 n2 = Arithmetic.mk_sub ctx [n1; n2]
  let mk_mult ctx n1 n2 = Arithmetic.mk_mul ctx [n1; n2]
  let mk_div = Arithmetic.mk_div
  let mk_mod = Arithmetic.Integer.mk_mod
  let mk_le = Arithmetic.mk_le
  let mk_ge = Arithmetic.mk_ge
  let mk_gt = Arithmetic.mk_gt
  let mk_lt = Arithmetic.mk_lt
  let mk_unary_minus = Arithmetic.mk_unary_minus
  let mk_not = missing1 "~"
  let parse_num (x:string) = x
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
  let decode_hex x =
    Int64.(sub (sub (of_string x) max_int32) one |> to_string)
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

module BitVectorOps (W:WordSize) = struct
  let mk_var ctx x = BitVector.mk_const_s ctx x W.word_size
  let mk_num ctx n = BitVector.mk_numeral ctx (string_of_int n) W.word_size
  let mk_bit_and = BitVector.mk_and
  let mk_bit_or = BitVector.mk_or
  let mk_bit_xor =  BitVector.mk_xor
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
  let parse_num (x:string) =
    (* Input is: #x0000004000000000 *)
    let offset n x = String.sub x n (String.length x - n) in
    (* We need to remove the prefix #x *)
    let x = offset 2 x in (* Removes the prefix: #x *)
    (* Then we need to remove the prefix 0s,
        otherwise Int32.of_string doesn't like it *)
    let rec trim_0 x =
      if String.length x > 0 && String.get x 0 = '0'
      then trim_0 (offset 1 x)
      else x
    in
    (* Prefix it with a 0x so that Int64.of_string knows it's an hex *)
    let x = "0x" ^ trim_0 x in
    (* Finally, convert it into an int64 (signed),
      and then render it back to a string, as this is for display only *)
    if x = "0x" then
      "0"
    else
      W.decode_hex x
end

(* Convert a declaration to a variable *)
let decl_to_variable (d:Z3.FuncDecl.func_decl) : Variable.t =
  d
  |> Z3.FuncDecl.get_name
  |> Z3.Symbol.get_string
  |> Variable.from_name


module Solver = struct
  open Z3

  type t =
    | Sat of Model.model
    | Unsat
    | Unknown of string

  let run (solver:Solver.solver) : t =
    match Solver.check solver [] with
    | SATISFIABLE ->
        (match Solver.get_model solver with
        | Some model ->
            Sat model
        | None ->
            raise (Invalid_argument "Satisfiable result but no model available - check solver configuration"))
    | UNSATISFIABLE ->
        Unsat
    | UNKNOWN ->
        Unknown (Solver.get_reason_unknown solver)
end

module Optimizer = struct
  open Z3

  module Strategy = struct
    type t =
      | Maximize
      | Minimize
  end

  type t =
    | Sat of { model: Model.model; optimal: Expr.expr }
    | Unsat
    | Unknown of string

  let run (opt:Optimize.optimize) (strategy:Strategy.t) (n:Expr.expr) : t =
    let handle =
      match strategy with
      | Maximize -> Optimize.maximize opt n
      | Minimize -> Optimize.minimize opt n
    in
    match Optimize.check opt with
    | SATISFIABLE ->
        (match Optimize.get_model opt with
        | Some model ->
            let optimal = Optimize.get_lower handle in
            Sat { model; optimal }
        | None ->
            raise (Invalid_argument "Satisfiable result but no model available - check optimizer configuration"))
    | UNSATISFIABLE ->
        Unsat
    | UNKNOWN ->
        Unknown (Optimize.get_reason_unknown opt)
end

module CodeGen (N:NUMERIC_OPS) = struct
  let parse_num = N.parse_num
  let nbin_to_expr : N_binary.t -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr =
    function
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

  let nrel_to_expr : N_rel.t -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr =
    function
    | Eq -> Boolean.mk_eq
    | Neq -> fun ctx n1 n2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx n1 n2)
    | Le -> N.mk_le
    | Ge -> N.mk_ge
    | Lt -> N.mk_lt
    | Gt -> N.mk_gt

  let brel_to_expr : B_rel.t -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr =
    function
    | BOr -> fun ctx b1 b2 -> Boolean.mk_or ctx [b1; b2]
    | BAnd -> fun ctx b1 b2 -> Boolean.mk_and ctx [b1; b2]

  let rec n_to_expr (ctx:Z3.context) : nexp -> Expr.expr =
    function
    | Var x -> Variable.name x |> N.mk_var ctx
    | CastInt b ->
      n_to_expr ctx (n_if b (Num 1) (Num 0))
    | Unary (BitNot, n) ->
      N.mk_not ctx (n_to_expr ctx n)
    | Unary (Negate, n) ->
      N.mk_unary_minus ctx (n_to_expr ctx n)
    | Other n ->
        let n : string = Exp.n_to_string n in
        raise (Not_implemented ("n_to_expr: not implemented for Other of " ^ n))
    | NCall _ ->
        failwith "b_to_expr: invoke Predicates.inline to remove predicates"
    | Num (n:int) -> N.mk_num ctx n
    | Binary (op, n1, n2) ->
        (nbin_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
    | NIf (b, n1, n2) -> Boolean.mk_ite ctx
        (b_to_expr ctx b) (n_to_expr ctx n1) (n_to_expr ctx n2)

  and b_to_expr (ctx:Z3.context) : bexp -> Expr.expr =
    function
    | Bool (b:bool) -> Boolean.mk_val ctx b
    | CastBool n -> b_to_expr ctx (n_neq n (Num 0))
    | NRel (op, n1, n2) ->
        (nrel_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
    | BRel (op, b1, b2) ->
        (brel_to_expr op) ctx (b_to_expr ctx b1) (b_to_expr ctx b2)
    | BNot (b:bexp) -> Boolean.mk_not ctx (b_to_expr ctx b)
    | Pred _ -> failwith "b_to_expr: invoke Predicates.inline to remove predicates"

  let ( let* ) = Option.bind


  (* Get's the value of a symbolic (integer) expression *)
  let get_int (m:Z3.Model.model) (e:Z3.Expr.expr) : int option =
    (* Evaluate the expression in the model *)
    let* v = Z3.Model.eval m e true in
    (* Try to cast the result to an integer, returning none upon failure *)
    try
      Some (Expr.to_string v |> parse_num |> int_of_string)
    with
      Failure _ -> None

  let get_int_decl (m:Z3.Model.model) (d:Z3.FuncDecl.func_decl) : int option =
    (* Variables in the model are actually functions with
      0 args, so we create a function call *)
    (* We then evaluate the function call *)
    get_int m (Z3.FuncDecl.apply d [])


  (* Tries to get all variables in the set.
     Missed variable are skipped. *)
  let get_all (m:Z3.Model.model) (vars:Variable.Set.t) : (Variable.t * int) list =
     (* Go through all declarations of the model *)
    Z3.Model.get_const_decls m
    |> List.filter_map (fun d ->
        let x = decl_to_variable d in
        if Variable.Set.mem x vars then
          let* v = get_int_decl m d in
          Some (x, v)
        else
          None
      )
  (*
    Optimizes a numeric expression given a boolean expression.
    *)
  let optimize
    ?(timeout=0) (* By default no timeout is given *)
    (strategy:Optimizer.Strategy.t)
    (pre:Exp.bexp)
    (n:Exp.nexp)
  :
    Optimizer.t
  =
    let open Z3 in
    let args =
      if timeout > 0 then ["timeout", string_of_int timeout] else []
    in
    let ctx = mk_context args in
    let opt = Optimize.mk_opt ctx in
    Optimize.add opt [
      b_to_expr ctx pre;
    ]
    ;
    let n = n_to_expr ctx n in
    Optimizer.run opt strategy n

  let optimize_expr
    ?(timeout=0) (* By default no timeout is given *)
    ?(pre=Bool true)
    (strategy:Optimizer.Strategy.t)
    (n:Exp.nexp)
  :
    (int, string) Result.t
  =
    let res = optimize ~timeout strategy pre n in
    match res with
    | Sat {optimal; model} ->
      Ok (get_int model optimal |> Option.get)
    | Unsat -> Error "unsat"
    | Unknown m -> Error m

  let solve
    ?(timeout=0) (* By default no timeout is given *)
    (pre:Exp.bexp)
  :
    Solver.t
  =
    let args =
      if timeout > 0 then ["timeout", string_of_int timeout] else []
    in
    let ctx = Z3.mk_context args in
    let solver = Z3.Solver.mk_solver ctx None in
    Z3.Solver.add solver [
      b_to_expr ctx pre;
    ];
    Solver.run solver

end

module SignedBitVectorOps (W:WordSize) = struct
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
  let mk_bit_xor =  BitVector.mk_xor
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
    (* Input is: #x0000004000000000 *)
    let offset n x = String.sub x n (String.length x - n) in
    (* We need to remove the prefix #x; input becomes: 0000004000000000 *)
    let x = offset 2 x in (* Removes the prefix: #x *)
    (* Then we need to remove the prefix 0s,
        otherwise Int32.of_string doesn't like it.
      Input becomes: 4000000000 *)
    let rec trim_0 x =
      if String.length x > 0 && String.get x 0 = '0' then
        trim_0 (offset 1 x)
      else
        x
    in
    (* Prefix it with a 0x so that Int64.of_string knows it's an hex *)
    let x = "0x" ^ trim_0 x in
    (* Finally, convert it into an int64 (signed),
        and then render it back to a string, as this is for display only *)
    if x = "0x"
    then (W.decode_hex "0x0")
    else W.decode_hex x
end

module IntGen = CodeGen (ArithmeticOps)
module Bv32Gen = CodeGen (BitVectorOps(W32))
module Bv64Gen = CodeGen (BitVectorOps(W64))
module SignedBv32Gen = CodeGen (SignedBitVectorOps(SIGNED_32))




