(** This module is responsible for simplifying control flow and extracting
    numeric/boolean expressions, which may generate new variable declarations.
*)

open Stage0
open Protocols
open State.Syntax

type t =
  | Skip
  | Seq of t * t
  | Sync of Sync.t
  | SyncOp of {
      mode : Sync.Mode.t;
      array : Variable.t;
      index : Infer_exp.t list;
      loc : Location.t option;
    }
  | Assert of Infer_exp.t
  | Read of {
      target : (Ty.t * Variable.t) option;
      array : Variable.t;
      index : Infer_exp.t list;
      guard : Infer_exp.t option;
    }
  | Atomic of {
      target : Variable.t;
      ty : Ty.t;
      atomic : Infer_exp.t Atomic.t;
      array : Variable.t;
      index : Infer_exp.t list;
      guard : Infer_exp.t option;
    }
  | Write of {
      array : Variable.t;
      index : Infer_exp.t list;
      payload : int option;
      guard : Infer_exp.t option;
    }
  | LocationAlias of { target : Variable.t; pointer : Infer_pointer.t }
  | Decl of { var : Variable.t; ty : Ty.t; init : Infer_exp.t option }
  | Assign of { var : Variable.t; data : Infer_exp.t; ty : Ty.t }
  | If of (Infer_exp.t * t * t)
  | Call of {
      result : (Variable.t * Ty.t) option;
      id : Function_id.t;
      args : Infer_exp.t list;
    }
  | Break
  | Continue
  | Return of Infer_exp.t option
  | While of (Infer_exp.t * t)
  | DoWhile of (Infer_exp.t * t)
  | For of { init : t; cond : Infer_exp.t; inc : t; body : t }

let decl_set ?(ty = Ty.int) (var : Variable.t) (init : Infer_exp.t) : t =
  Decl { init = Some init; ty; var }

let decl_unset ?(ty = Ty.int) (var : Variable.t) : t =
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
  | Sync s -> Sync s
  | SyncOp { mode; array; index; loc } ->
      Infer_exp.unknowns
        (let* index = State.list_map to_nexp index in
         let id = List.fold_left Exp.n_plus (Exp.Var array) index in
         return (Stmt.Sync { mode; id; participants = None; loc }))
  | Assert e -> ret_assert e Global
  | Read { array; target; index; guard } ->
      Infer_exp.unknowns
        (let* index = State.list_map to_nexp index in
         let* guard = State.option_map to_bexp guard in
         return (Stmt.Read { target; array; index; guard }))
  | Atomic { target; ty; atomic; array; index; guard } ->
      Infer_exp.unknowns
        (let* index = State.list_map to_nexp index in
         let* atomic = Atomic.map_state to_nexp atomic in
         let* guard = State.option_map to_bexp guard in
         return (Stmt.Atomic { target; atomic; array; index; ty; guard }))
  | Write { array; index; payload; guard } ->
      Infer_exp.unknowns
        (let* index = State.list_map to_nexp index in
         let* guard = State.option_map to_bexp guard in
         return (Stmt.Write { array; index; payload; guard }))
  | LocationAlias { target; pointer } ->
      Infer_exp.unknowns
        (let* pointer = Infer_pointer.to_pointer pointer in
         return (Stmt.LocationAlias { target; pointer }))
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
  | Call { result; id; args } ->
      Infer_exp.unknowns
        (let* args = State.list_map Infer_exp.to_nexp args in
         return (Stmt.Call { result; id; args }))
  | Break -> Skip
  | Continue -> Skip
  | Return _ -> Skip

(** If-conversion of conditional assignments: a scalar reassigned inside a
    straight-line [If] branch is lifted to a single [NIf]-valued assignment after
    the branch, so its value is preserved on both paths. Memory effects stay
    guarded in place; only [If] whose arms have no nested control flow are
    converted, everything else is left untouched. *)
module Convert_assigns = struct
  module IE = Infer_exp

  type arm = {
    residual : t;
    env : IE.t Variable.Map.t;
    local : Variable.Set.t;
    reads : Ty.t Variable.Map.t;
    assigned : Ty.t Variable.Map.t;
  }

  let empty : arm =
    {
      residual = Skip;
      env = Variable.Map.empty;
      local = Variable.Set.empty;
      reads = Variable.Map.empty;
      assigned = Variable.Map.empty;
    }

  let subst (env : IE.t Variable.Map.t) (e : IE.t) : IE.t =
    if Variable.Map.is_empty env then e
    else IE.subst (fun x -> Variable.Map.find_opt x env) e

  let keep (a : arm) (s : t) : arm = { a with residual = seq a.residual s }

  let bind_target (a : arm) : (Ty.t * Variable.t) option -> arm = function
    | Some (ty, x) ->
        { a with
          reads = Variable.Map.add x ty a.reads;
          env = Variable.Map.remove x a.env }
    | None -> a

  let rec fold (a : arm) (s : t) : arm option =
    let ( let* ) = Option.bind in
    match s with
    | Skip -> Some a
    | Seq (p, q) ->
        let* a = fold a p in
        fold a q
    | Assign { var; data; ty } ->
        let data = subst a.env data in
        let assigned =
          if Variable.Set.mem var a.local then a.assigned
          else Variable.Map.add var ty a.assigned
        in
        Some { a with env = Variable.Map.add var data a.env; assigned }
    | Decl { var; ty = _; init = Some e } ->
        Some
          { a with
            env = Variable.Map.add var (subst a.env e) a.env;
            local = Variable.Set.add var a.local }
    | Decl { var; ty; init = None } ->
        Some
          (keep
             { a with
               local = Variable.Set.add var a.local;
               env = Variable.Map.remove var a.env }
             (Decl { var; ty; init = None }))
    | Assert e -> Some (keep a (Assert (subst a.env e)))
    | Sync _ -> Some (keep a s)
    | SyncOp { mode; array; index; loc } ->
        Some
          (keep a
             (SyncOp { mode; array; index = List.map (subst a.env) index; loc }))
    | LocationAlias { target; pointer } ->
        Some
          (keep a
             (LocationAlias
                { target; pointer = Infer_pointer.map (subst a.env) pointer }))
    | Read { target; array; index; guard } ->
        let index = List.map (subst a.env) index in
        let guard = Option.map (subst a.env) guard in
        Some (keep (bind_target a target) (Read { target; array; index; guard }))
    | Write { array; index; payload; guard } ->
        let index = List.map (subst a.env) index in
        let guard = Option.map (subst a.env) guard in
        Some (keep a (Write { array; index; payload; guard }))
    | Atomic { target; ty; atomic; array; index; guard } ->
        let index = List.map (subst a.env) index in
        let guard = Option.map (subst a.env) guard in
        let atomic = Atomic.map (subst a.env) atomic in
        Some
          (keep
             (bind_target a (Some (ty, target)))
             (Atomic { target; ty; atomic; array; index; guard }))
    | Call { result; id; args } ->
        let args = List.map (subst a.env) args in
        let a = bind_target a (Option.map (fun (x, ty) -> (ty, x)) result) in
        Some (keep a (Call { result; id; args }))
    | If _ | While _ | DoWhile _ | For _ | Break | Continue | Return _ -> None

  let convert (cond : IE.t) (p : t) (q : t) : t =
    match (fold empty p, fold empty q) with
    | Some ap, Some aq ->
        let merge_vars =
          Variable.Map.union (fun _ ty _ -> Some ty) ap.assigned aq.assigned
        in
        if Variable.Map.is_empty merge_vars then If (cond, p, q)
        else
          let value (a : arm) (v : Variable.t) : IE.t =
            match Variable.Map.find_opt v a.env with
            | Some e -> e
            | None -> IE.NExp (IE.Var v)
          in
          let merges =
            Variable.Map.fold
              (fun v ty acc ->
                let data = IE.NExp (IE.NIf (cond, value ap v, value aq v)) in
                Assign { var = v; ty; data } :: acc)
              merge_vars []
          in
          let merge_free =
            List.fold_left
              (fun acc -> function
                | Assign { data; _ } -> IE.free_names data acc
                | _ -> acc)
              Variable.Set.empty merges
          in
          let reads =
            Variable.Map.union (fun _ ty _ -> Some ty) ap.reads aq.reads
          in
          let hoisted =
            Variable.Map.fold
              (fun x ty acc ->
                if Variable.Set.mem x merge_free then
                  Decl { var = x; ty; init = None } :: acc
                else acc)
              reads []
          in
          from_list (hoisted @ (If (cond, ap.residual, aq.residual) :: merges))
    | _ -> If (cond, p, q)

  let rec rewrite (s : t) : t =
    match s with
    | Seq (a, b) -> seq (rewrite a) (rewrite b)
    | If (c, p, q) -> convert c (rewrite p) (rewrite q)
    | While (c, s) -> While (c, rewrite s)
    | DoWhile (c, s) -> DoWhile (c, rewrite s)
    | For { init; cond; inc; body } ->
        For { init = rewrite init; cond; inc = rewrite inc; body = rewrite body }
    | Skip | Sync _ | SyncOp _ | Assert _ | Read _ | Atomic _ | Write _
    | LocationAlias _ | Decl _ | Assign _ | Call _ | Break | Continue | Return _
      ->
        s
end

(** The infer function generates the Stmt.t code as well the value being
    returned if any. *)
let infer (s : t) : Stmt.t * Exp.nexp option =
  let s = Convert_assigns.rewrite s in
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
    | Atomic r -> [Line (Ty.to_string r.ty ^ " " ^ Variable.name r.target ^ " = atomic " ^ Variable.name r.array ^ Access.index_to_string r.index ^ ";")]
    | Read r ->
      let a = Variable.name r.array in
      let idx = Access.index_to_string r.index in
      let prefix =
        match r.target with
        | Some (ty, target) ->
          let x = Variable.name target in
          let ty = Ty.to_string ty in
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
      [Line ("alias " ^ Variable.name l.target ^ " = "
             ^ Infer_pointer.to_string l.pointer ^ ";")]
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
