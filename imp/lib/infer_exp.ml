open Protocols
open Stage0 (* State monad *)

type n =
  | Var of Variable.t
  | Num of int
  | Unary of N_unary.t * t
  | Binary of N_binary.t * t * t
  | NCall of string * t
  | NIf of t * t * t

and b =
  | Bool of bool
  | NRel of N_rel.t * t * t
  | BRel of B_rel.t * t * t
  | BNot of t
  | Pred of string * t
  | ThreadUnif of t

and t = NExp of n | BExp of b | Unknown of string

let rec to_string : t -> string = function
  | NExp e -> to_n_string e
  | BExp e -> to_b_string e
  | Unknown x -> x

and to_n_string : n -> string = function
  | Var x -> Variable.name x
  | Num x -> string_of_int x
  | Unary (o, e) -> N_unary.to_string o ^ " (" ^ to_string e ^ ")"
  | Binary (o, l, r) ->
      "(" ^ to_string l ^ ") " ^ N_binary.to_string o ^ " (" ^ to_string r ^ ")"
  | NCall (o, e) -> o ^ "(" ^ to_string e ^ ")"
  | NIf (e1, e2, e3) ->
      let e1 = to_string e1 in
      let e2 = to_string e2 in
      let e3 = to_string e3 in
      "(" ^ e1 ^ ") ? (" ^ e2 ^ ") : (" ^ e3 ^ ")"

and to_b_string : b -> string = function
  | Bool b -> if b then "true" else "false"
  | NRel (o, e1, e2) ->
      let o = N_rel.to_string o in
      let e1 = to_string e1 in
      let e2 = to_string e2 in
      "(" ^ e1 ^ ") " ^ o ^ " (" ^ e2 ^ ")"
  | BRel (o, e1, e2) ->
      let o = B_rel.to_string o in
      let e1 = to_string e1 in
      let e2 = to_string e2 in
      "(" ^ e1 ^ ") " ^ o ^ " (" ^ e2 ^ ")"
  | BNot e -> "!(" ^ to_string e ^ ")"
  | Pred (o, e) -> o ^ "(" ^ to_string e ^ ")"
  | ThreadUnif e -> "thread_unif(" ^ to_string e ^ ")"

let n_bin (o : N_binary.t) (e1 : t) (e2 : t) : n = Binary (o, e1, e2)
let plus : t -> t -> n = n_bin (Plus Signedness.Signed)
let lt (e1 : t) (e2 : t) : b = NRel (Lt Signedness.Signed, e1, e2)
let gt (e1 : t) (e2 : t) : b = NRel (Gt Signedness.Signed, e1, e2)
let min (e1 : t) (e2 : t) : n = NIf (BExp (lt e1 e2), e1, e2)
let max (e1 : t) (e2 : t) : n = NIf (BExp (gt e1 e2), e1, e2)
let or_ (e1 : t) (e2 : t) : b = BRel (BOr, e1, e2)
let not_ (e : t) : b = BNot e
let n_eq (e1 : t) (e2 : t) : b = NRel (Eq, e1, e2)
let thread_equal (e : t) : b = ThreadUnif e
let thread_distinct (e : t) : b = BNot (BExp (ThreadUnif e))
let num (n : int) : t = NExp (Num n)
let bool (b : bool) : t = BExp (Bool b)
let unknown (lbl : string) : t = Unknown lbl
let true_ : t = bool true

type 'a state = (Variable.Set.t, 'a) State.t

let make_unknown (label : string) : Variable.t state =
  State.update_return (fun st ->
      let count = Variable.Set.cardinal st in
      let v =
        "@Unknown" ^ string_of_int count
        |> Variable.from_name |> Variable.set_label label
      in
      (Variable.Set.add v st, v))

open State.Syntax

let rec to_nexp (e : t) : Exp.nexp state =
  match e with
  | NExp n -> (
      match n with
      | Var x -> return (Exp.Var x)
      | Num x -> return (Exp.Num x)
      | Binary (o, n1, n2) ->
          let* n1 = to_nexp n1 in
          let* n2 = to_nexp n2 in
          return (Exp.Binary (o, n1, n2))
      | Unary (o, n) ->
          let* n = to_nexp n in
          return (Exp.Unary (o, n))
      | NCall (x, n) ->
          let* n = to_nexp n in
          return (Exp.NCall (x, [ n ]))
      | NIf (b, n1, n2) ->
          let* b = to_bexp b in
          let* n1 = to_nexp n1 in
          let* n2 = to_nexp n2 in
          return (Exp.NIf (b, n1, n2)))
  | BExp _ ->
      let* b = to_bexp e in
      return (Exp.cast_int b)
  | Unknown lbl ->
      let* x = make_unknown lbl in
      return (Exp.Var x)

and to_bexp (e : t) : Exp.bexp state =
  match e with
  | BExp b -> (
      match b with
      | Bool x -> return (Exp.Bool x)
      | NRel (o, n1, n2) ->
          let* n1 = to_nexp n1 in
          let* n2 = to_nexp n2 in
          return (Exp.NRel (o, n1, n2))
      | BRel (o, b1, b2) ->
          let* b1 = to_bexp b1 in
          let* b2 = to_bexp b2 in
          return (Exp.BRel (o, b1, b2))
      | BNot b ->
          let* b = to_bexp b in
          return (Exp.BNot b)
      | Pred (x, n) ->
          let* n = to_nexp n in
          return (Exp.Pred (x, [ n ]))
      | ThreadUnif n ->
          let* n = to_nexp n in
          return (Exp.ThreadUnif n))
  | NExp _ ->
      let* n = to_nexp e in
      return (Exp.cast_bool n)
  | Unknown lbl ->
      let* x = make_unknown lbl in
      return (Exp.cast_bool (Var x))

(** Runs a state monad and returns the set of unknown variables *)
let vars ?(init = Variable.Set.empty) (m : 'a state) : Variable.Set.t * 'a =
  State.run m init

let decls ?(init = Variable.Set.empty) (m : 'a state) : Stmt.t * 'a =
  let vs, a = vars ~init m in
  let delcs =
    vs |> Variable.Set.to_list |> List.map Stmt.decl_unset |> Stmt.from_list
  in
  (delcs, a)

let unknowns ?(init = Variable.Set.empty) (m : Stmt.t state) : Stmt.t =
  let d, a = decls ~init m in
  Stmt.seq d a

(** Run the state monad only and returns something only when there are no
    unknowns *)
let no_unknowns (m : 'a state) : 'a option =
  let vs, a = vars m in
  if Variable.Set.is_empty vs then Some a else None
