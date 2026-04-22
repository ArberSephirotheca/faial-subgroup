(** This module is responsible for simplifying control flow and extracting
    numeric/boolean expressions, which may generate new variable declarations.
*)

open Stage0
open Protocols
open State.Syntax
module O_Array_use = Array_use (* Refer to the root Array_use *)

module Array_use = struct
  type t = { array : Variable.t; offset : Infer_exp.t }

  let make ?(offset = Infer_exp.num 0) (array : Variable.t) : t =
    { array; offset }

  let add (e : Infer_exp.t) (u : t) : t =
    { u with offset = NExp (Infer_exp.plus u.offset e) }

  let infer : t -> O_Array_use.t Infer_exp.state = function
    | { array; offset } ->
        let* offset = Infer_exp.to_nexp offset in
        return { O_Array_use.array; O_Array_use.offset }
end

module O_Arg = Arg (* Refer to the root Arg *)

module Arg = struct
  type t = Scalar of Infer_exp.t | Array of Array_use.t | Unsupported

  let infer : t -> O_Arg.t Infer_exp.state = function
    | Scalar e ->
        let* e = Infer_exp.to_nexp e in
        return (O_Arg.Scalar e)
    | Array { array; offset } ->
        let* offset = Infer_exp.to_nexp offset in
        return (O_Arg.Array { array; offset })
    | Unsupported -> return O_Arg.Unsupported
end

type t =
  | Skip
  | Seq of t * t
  | Sync of Location.t option
  | NamedBarrier of Ptx.t
  | Assert of Infer_exp.t
  | Read of {
      target : (C_type.t * Variable.t) option;
      array : Variable.t;
      index : Infer_exp.t list;
    }
  | Atomic of {
      target : Variable.t;
      ty : C_type.t;
      atomic : Atomic.t;
      array : Variable.t;
      index : Infer_exp.t list;
    }
  | Write of {
      array : Variable.t;
      index : Infer_exp.t list;
      payload : int option;
    }
  | LocationAlias of {
      source : Variable.t;
      target : Variable.t;
      offset : Infer_exp.t;
    }
  | Decl of { var : Variable.t; ty : C_type.t; init : Infer_exp.t option }
  | Assign of { var : Variable.t; data : Infer_exp.t; ty : C_type.t }
  | If of (Infer_exp.t * t * t)
  | Call of {
      result : (Variable.t * C_type.t) option;
      kernel : string;
      ty : string;
      args : Arg.t list;
    }
  | Break
  | Continue
  | Return of Infer_exp.t option
  | While of (Infer_exp.t * t)
  | DoWhile of (Infer_exp.t * t)
  | For of { init : t; cond : Infer_exp.t; inc : t; body : t }

let decl_set ?(ty = C_type.int) (var : Variable.t) (init : Infer_exp.t) : t =
  Decl { init = Some init; ty; var }

let decl_unset ?(ty = C_type.int) (var : Variable.t) : t =
  Decl { init = None; ty; var }

let for_ ~init ~cond ~inc ~body : t = For { init; cond; inc; body }

let seq (s1 : t) (s2 : t) : t =
  match (s1, s2) with Skip, s | s, Skip -> s | _, _ -> Seq (s1, s2)

let rec last : t -> t = function Seq (_, s) -> last s | s -> s

let rec skip_last : t -> t = function
  | Seq (s1, s2) -> seq s1 (skip_last s2)
  | _ -> Skip

let from_list : t list -> t = List.fold_left seq Skip

let ret_assert (b : Infer_exp.t) (v : Assert.Visibility.t) : Stmt.t =
  match Infer_exp.(no_unknowns (to_bexp b)) with
  | Some b -> Assert (Assert.make b v)
  | None -> Skip

let rec to_stmt : t -> Stmt.t =
  let open Infer_exp in
  function
  | Skip -> Skip
  | Seq (p, q) -> Seq (to_stmt p, to_stmt q)
  | Sync l -> Sync l
  | NamedBarrier p -> NamedBarrier p
  | Assert e -> ret_assert e Global
  | Read { array; target; index } ->
      Infer_exp.unknowns
        (let* index = State.list_map to_nexp index in
         return (Stmt.Read { target; array; index }))
  | Atomic { target; ty; atomic; array; index } ->
      Infer_exp.unknowns
        (let* index = State.list_map to_nexp index in
         return (Stmt.Atomic { target; atomic; array; index; ty }))
  | Write { array; index; payload } ->
      Infer_exp.unknowns
        (let* index = State.list_map to_nexp index in
         return (Stmt.Write { array; index; payload }))
  | LocationAlias { source; target; offset } ->
      Infer_exp.unknowns
        (let* offset = to_nexp offset in
         return (Stmt.LocationAlias { target; source; offset }))
  | Decl { var; ty; init } ->
      Infer_exp.unknowns
        (let* init = State.option_map Infer_exp.to_nexp init in
         return (Stmt.Decl { var; ty; init }))
  | Assign { var; data; ty } ->
      Infer_exp.unknowns
        (let* data = to_nexp data in
         return (Stmt.assign ty var data))
  | If (e, (Break | Return None | Continue), Skip) ->
      ret_assert (Infer_exp.BExp (Infer_exp.not_ e)) Local
  | If (e, p, q) ->
      Infer_exp.unknowns
        (let* e = to_bexp e in
         let p = to_stmt p in
         let q = to_stmt q in
         return (Stmt.if_ e p q))
  | While (e, s) ->
      Infer_exp.unknowns
        (let* e = to_bexp e in
         return (For.infer_while e (to_stmt s)))
  | DoWhile (e, s) -> Seq (to_stmt s, to_stmt (While (e, s)))
  | For { init; cond; inc; body } ->
      Infer_exp.unknowns
        (let* cond = to_bexp cond in
         return
           (For.to_stmt
              { init = to_stmt init; cond; inc = to_stmt inc }
              (to_stmt body)))
  | Call { result; kernel; ty; args } ->
      Infer_exp.unknowns
        (let* args = State.list_map Arg.infer args in
         return (Stmt.Call { result; kernel; ty; args }))
  | Break -> Skip
  | Continue -> Skip
  | Return _ -> Skip

(** The infer function generates the Stmt.t code as well the value being
    returned if any. *)
let infer (s : t) : Stmt.t * Exp.nexp option =
  let code = skip_last s in
  let post, ret =
    match last s with
    | Return (Some e) ->
        let decls, e = Infer_exp.decls (Infer_exp.to_nexp e) in
        (decls, Some e)
    | s -> (to_stmt s, None)
  in
  (Stmt.seq (to_stmt code) post, ret)

(*
let to_s: t -> Indent.t list =
  let rec stmt_to_s : t -> Indent.t list =
    function
    | Call c -> [Line (Call.to_string c)]
    | Sync _ -> [Line "sync;"]
    | Assert b -> [Line (Assert.to_string b ^ ";")]
    | Atomic r -> [Line (C_type.to_string r.ty ^ " " ^ Variable.name r.target ^ " = atomic " ^ Variable.name r.array ^ Access.index_to_string r.index ^ ";")]
    | Read r ->
      let a = Variable.name r.array in
      let idx = Access.index_to_string r.index in
      let prefix =
        match r.target with
        | Some (ty, target) ->
          let x = Variable.name target in
          let ty = C_type.to_string ty in
          ty ^ " " ^ x ^ " = "
        | None ->
          ""
      in
      [
        Line (prefix ^ "rd " ^ a ^ idx ^ ";")
      ]
    | Write w ->
      let payload :string = match w.payload with
        | None -> ""
        | Some x -> " = " ^ string_of_int x
      in
      [Line ("wr " ^ Variable.name w.array ^ Access.index_to_string w.index ^ payload ^ ";")]
    | Skip -> [Line "skip;"]
    | Assign a -> [Line (Variable.name a.var ^ " = " ^ Exp.n_to_string a.data ^ ";")]
    | LocationAlias l ->
      [Line ("alias " ^ Alias.to_string l)]
    | Decl [] -> []
    | Decl l ->
      let entries = String.concat ", " (List.map Decl.to_string l) in
      [Line ("decl " ^ entries ^ ";")]

    | If (b, s1, Skip) -> [
        Line ("if (" ^ Exp.b_to_string b ^ ") {");
        Block (stmt_to_s s1);
        Line "}";
      ]

    | Seq (s1, s2) ->
      stmt_to_s s1 @ stmt_to_s s2

    | If (b, s1, s2) -> [
        Line ("if (" ^ Exp.b_to_string b ^ ") {");
        Block (stmt_to_s s1);
        Line "} else {";
        Block (stmt_to_s s2);
        Line "}"
      ]
    | Star s -> [
        Line ("foreach (?) {");
        Block (stmt_to_s s);
        Line ("}")
      ]
    | For (r, s) ->
        [
          Line ("foreach (" ^ Range.to_string r ^ ") {");
          Block (stmt_to_s s);
          Line ("}");
        ]
  in
  stmt_to_s

let to_string (s: t) : string =
  Indent.to_string (to_s s)
*)
