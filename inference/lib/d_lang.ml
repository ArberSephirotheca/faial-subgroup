open Stage0
open Protocols
module StackTrace = Stack_trace
module KernelAttr = C_lang.KernelAttr
module StringMap = Common.StringMap
module StringMapUtil = Common.StringMapUtil
module Param = C_lang.Param
module Ty_param = C_lang.Ty_param

type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result
type array_t = Protocols.Memory.t

let list_to_s (f : 'a -> string) (l : 'a list) : string =
  List.map f l |> String.concat ", "

module Expr = struct
  type t =
    | SizeOfExpr of Ty.t
    | Convert of { arg : t; ty : Ty.t }
    | CXXNewExpr of { arg : t; ty : Ty.t }
    | CXXDeleteExpr of { arg : t; ty : Ty.t }
    | RecoveryExpr of Ty.t
    | CharacterLiteral of int
    | BinaryOperator of d_binary
    | CallExpr of d_call
    | ConditionalOperator of {
        cond : t;
        then_expr : t;
        else_expr : t;
        ty : Ty.t;
      }
    | CXXConstructExpr of { args : t list; ty : Ty.t }
    | CXXBoolLiteralExpr of bool
    | CXXOperatorCallExpr of { func : t; args : t list; ty : Ty.t }
    | FloatingLiteral of float
    | IntegerLiteral of int
    | MemberExpr of { name : string; base : t; ty : Ty.t }
    | Ident of Decl_expr.t
    | UnaryOperator of { opcode : string; child : t; ty : Ty.t }
    | UnresolvedLookupExpr of { name : Variable.t; tys : Ty.t list }

  and d_binary = { opcode : string; lhs : t; rhs : t; ty : Ty.t }
  and d_call = { func : t; args : t list; ty : Ty.t }

  let ident ?(ty = J_type.int) ?(kind = Decl_expr.Kind.Var) (name : Variable.t)
      =
    Ident (Decl_expr.from_name ~ty ~kind name)

  let is_call : t -> bool = function CallExpr _ -> true | _ -> false

  let name = function
    | SizeOfExpr _ -> "SizeOfExpr"
    | Convert _ -> "Convert"
    | CXXNewExpr _ -> "CXXNewExpr"
    | CXXDeleteExpr _ -> "CXXDeleteExpr"
    | RecoveryExpr _ -> "RecoveryExpr"
    | CharacterLiteral _ -> "CharacterLiteral"
    | BinaryOperator _ -> "BinaryOperator"
    | CallExpr _ -> "CallExpr"
    | ConditionalOperator _ -> "ConditionalOperator"
    | CXXConstructExpr _ -> "CXXConstructExpr"
    | CXXBoolLiteralExpr _ -> "CXXBoolLiteralExpr"
    | CXXOperatorCallExpr _ -> "CXXOperatorCallExpr"
    | FloatingLiteral _ -> "FloatingLiteral"
    | IntegerLiteral _ -> "IntegerLiteral"
    | MemberExpr _ -> "MemberExpr"
    | UnaryOperator _ -> "UnaryOperator"
    | UnresolvedLookupExpr _ -> "UnresolvedLookupExpr"
    | Ident _ -> "Ident"

  let to_type : t -> Ty.t = function
    | SizeOfExpr _ -> J_type.int
    | Convert c -> c.ty
    | CXXNewExpr c -> c.ty
    | CXXDeleteExpr c -> c.ty
    | RecoveryExpr ty -> ty
    | CharacterLiteral _ -> J_type.char
    | BinaryOperator a -> a.ty
    | ConditionalOperator c -> c.ty
    | CXXBoolLiteralExpr _ -> J_type.bool
    | Ident a -> Decl_expr.ty a
    | CXXConstructExpr c -> c.ty
    | FloatingLiteral _ -> J_type.float
    | IntegerLiteral _ -> J_type.int
    | UnaryOperator a -> a.ty
    | CallExpr c -> c.ty
    | CXXOperatorCallExpr a -> a.ty
    | MemberExpr a -> a.ty
    | UnresolvedLookupExpr _ -> J_type.unknown

  let to_string ?(modifier : bool = false) ?(provenance : bool = false)
      ?(types : bool = false) : t -> string =
    let attr (s : string) : string = if modifier then "@" ^ s ^ " " else "" in
    let opcode (o : string) (j : Ty.t) : string =
      if types then "(" ^ o ^ "." ^ Ty.to_string j ^ ")" else o
    in
    let var_name : Variable.t -> string =
      if provenance then Variable.repr else Variable.name
    in
    let rec exp_to_s : t -> string =
      let par (e : t) : string =
        match e with
        | BinaryOperator _ | ConditionalOperator _ -> "(" ^ exp_to_s e ^ ")"
        | Convert _ -> "(" ^ exp_to_s e ^ ")"
        | UnaryOperator _ | CXXNewExpr _ | CXXDeleteExpr _ | Ident _
        | UnresolvedLookupExpr _ | CallExpr _ | CXXOperatorCallExpr _
        | CXXConstructExpr _ | CXXBoolLiteralExpr _ | MemberExpr _
        | IntegerLiteral _ | CharacterLiteral _ | RecoveryExpr _
        | FloatingLiteral _ | SizeOfExpr _ ->
            exp_to_s e
      in
      function
      | SizeOfExpr ty -> "sizeof(" ^ Ty.to_string ty ^ ")"
      | Convert c -> "(" ^ Ty.to_string c.ty ^ ")" ^ par c.arg
      | CXXNewExpr c ->
          "new " ^ Ty.to_string c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
      | CXXDeleteExpr c -> "del " ^ par c.arg
      | RecoveryExpr _ -> "?"
      | FloatingLiteral f -> string_of_float f
      | CharacterLiteral i | IntegerLiteral i -> string_of_int i
      | ConditionalOperator c ->
          par c.cond ^ " ? " ^ par c.then_expr ^ " : " ^ par c.else_expr
      | BinaryOperator b ->
          par b.lhs ^ " " ^ opcode b.opcode b.ty ^ " " ^ par b.rhs
      | MemberExpr m -> par m.base ^ "." ^ m.name
      | CXXBoolLiteralExpr b -> if b then "true" else "false"
      | CXXConstructExpr c ->
          attr "ctor" ^ Ty.to_string c.ty ^ "(" ^ list_to_s exp_to_s c.args
          ^ ")"
      | CXXOperatorCallExpr c ->
          exp_to_s c.func ^ "[" ^ list_to_s exp_to_s c.args ^ "]"
      | Ident v -> Decl_expr.to_string ~modifier v
      | CallExpr c -> par c.func ^ "(" ^ list_to_s exp_to_s c.args ^ ")"
      | UnresolvedLookupExpr v -> attr "unresolv" ^ var_name v.name
      | UnaryOperator u -> u.opcode ^ par u.child
    in
    exp_to_s

  let opt_to_string : t option -> string = function
    | Some c -> to_string c
    | None -> ""

  let ( let@ ) c k = if c <> 0 then c else k ()

  let rec compare (e1 : t) (e2 : t) : int =
    match e1, e2 with
    | Ident d1, Ident d2 -> Decl_expr.compare d1 d2
    | UnresolvedLookupExpr a, UnresolvedLookupExpr b ->
        let@ () = Variable.compare a.name b.name in
        Stdlib.compare a.tys b.tys
    | Convert a, Convert b ->
        let@ () = compare a.arg b.arg in
        Stdlib.compare a.ty b.ty
    | CXXNewExpr a, CXXNewExpr b ->
        let@ () = compare a.arg b.arg in
        Stdlib.compare a.ty b.ty
    | CXXDeleteExpr a, CXXDeleteExpr b ->
        let@ () = compare a.arg b.arg in
        Stdlib.compare a.ty b.ty
    | BinaryOperator a, BinaryOperator b ->
        let@ () = String.compare a.opcode b.opcode in
        let@ () = compare a.lhs b.lhs in
        let@ () = compare a.rhs b.rhs in
        Stdlib.compare a.ty b.ty
    | CallExpr a, CallExpr b ->
        let@ () = compare a.func b.func in
        let@ () = List.compare compare a.args b.args in
        Stdlib.compare a.ty b.ty
    | CXXOperatorCallExpr a, CXXOperatorCallExpr b ->
        let@ () = compare a.func b.func in
        let@ () = List.compare compare a.args b.args in
        Stdlib.compare a.ty b.ty
    | ConditionalOperator a, ConditionalOperator b ->
        let@ () = compare a.cond b.cond in
        let@ () = compare a.then_expr b.then_expr in
        let@ () = compare a.else_expr b.else_expr in
        Stdlib.compare a.ty b.ty
    | CXXConstructExpr a, CXXConstructExpr b ->
        let@ () = List.compare compare a.args b.args in
        Stdlib.compare a.ty b.ty
    | MemberExpr a, MemberExpr b ->
        let@ () = String.compare a.name b.name in
        let@ () = compare a.base b.base in
        Stdlib.compare a.ty b.ty
    | UnaryOperator a, UnaryOperator b ->
        let@ () = String.compare a.opcode b.opcode in
        let@ () = compare a.child b.child in
        Stdlib.compare a.ty b.ty
    | _ -> Stdlib.compare e1 e2

  let equal (e1 : t) (e2 : t) : bool = compare e1 e2 = 0

  (* Post-order stateful rewrite: children of [e] are rewritten first, then
     [f] is applied to the reconstructed node. *)
  let rec st_map (f : t -> ('s, t) State.t) (e : t) : ('s, t) State.t =
    let open State.Syntax in
    match e with
    | SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _
    | CXXBoolLiteralExpr _ | FloatingLiteral _ | IntegerLiteral _ | Ident _
    | UnresolvedLookupExpr _ ->
        f e
    | Convert { arg; ty } ->
        let* arg = st_map f arg in
        f (Convert { arg; ty })
    | CXXNewExpr { arg; ty } ->
        let* arg = st_map f arg in
        f (CXXNewExpr { arg; ty })
    | CXXDeleteExpr { arg; ty } ->
        let* arg = st_map f arg in
        f (CXXDeleteExpr { arg; ty })
    | BinaryOperator { opcode; lhs; rhs; ty } ->
        let* lhs = st_map f lhs in
        let* rhs = st_map f rhs in
        f (BinaryOperator { opcode; lhs; rhs; ty })
    | CallExpr { func; args; ty } ->
        let* func = st_map f func in
        let* args = State.list_map (st_map f) args in
        f (CallExpr { func; args; ty })
    | ConditionalOperator { cond; then_expr; else_expr; ty } ->
        let* cond = st_map f cond in
        let* then_expr = st_map f then_expr in
        let* else_expr = st_map f else_expr in
        f (ConditionalOperator { cond; then_expr; else_expr; ty })
    | CXXConstructExpr { args; ty } ->
        let* args = State.list_map (st_map f) args in
        f (CXXConstructExpr { args; ty })
    | CXXOperatorCallExpr { func; args; ty } ->
        let* func = st_map f func in
        let* args = State.list_map (st_map f) args in
        f (CXXOperatorCallExpr { func; args; ty })
    | MemberExpr { name; base; ty } ->
        let* base = st_map f base in
        f (MemberExpr { name; base; ty })
    | UnaryOperator { opcode; child; ty } ->
        let* child = st_map f child in
        f (UnaryOperator { opcode; child; ty })

  let rec map (f : t -> t) (e : t) : t =
    match e with
    | SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _
    | CXXBoolLiteralExpr _ | FloatingLiteral _ | IntegerLiteral _ | Ident _
    | UnresolvedLookupExpr _ ->
        f e
    | Convert { arg; ty } ->
        let arg = map f arg in
        f (Convert { arg; ty })
    | CXXNewExpr { arg; ty } ->
        let arg = map f arg in
        f (CXXNewExpr { arg; ty })
    | CXXDeleteExpr { arg; ty } ->
        let arg = map f arg in
        f (CXXDeleteExpr { arg; ty })
    | BinaryOperator { opcode; lhs; rhs; ty } ->
        let lhs = map f lhs in
        let rhs = map f rhs in
        f (BinaryOperator { opcode; lhs; rhs; ty })
    | CallExpr { func; args; ty } ->
        let func = map f func in
        let args = List.map (map f) args in
        f (CallExpr { func; args; ty })
    | ConditionalOperator { cond; then_expr; else_expr; ty } ->
        let cond = map f cond in
        let then_expr = map f then_expr in
        let else_expr = map f else_expr in
        f (ConditionalOperator { cond; then_expr; else_expr; ty })
    | CXXConstructExpr { args; ty } ->
        let args = List.map (map f) args in
        f (CXXConstructExpr { args; ty })
    | CXXOperatorCallExpr { func; args; ty } ->
        let func = map f func in
        let args = List.map (map f) args in
        f (CXXOperatorCallExpr { func; args; ty })
    | MemberExpr { name; base; ty } ->
        let base = map f base in
        f (MemberExpr { name; base; ty })
    | UnaryOperator { opcode; child; ty } ->
        let child = map f child in
        f (UnaryOperator { opcode; child; ty })

  let subst (var : Variable.t) (replacement : t) : t -> t =
    map (function
    | Ident d when Variable.equal (Decl_expr.name d) var -> replacement
    | e -> e)

  let children : t -> t list = function
    | SizeOfExpr _ | RecoveryExpr _ | CharacterLiteral _ | CXXBoolLiteralExpr _
    | FloatingLiteral _ | IntegerLiteral _ | Ident _ | UnresolvedLookupExpr _ ->
        []
    | Convert { arg; _ } | CXXNewExpr { arg; _ } | CXXDeleteExpr { arg; _ } ->
        [ arg ]
    | BinaryOperator { lhs; rhs; _ } -> [ lhs; rhs ]
    | CallExpr { func; args; _ } | CXXOperatorCallExpr { func; args; _ } ->
        func :: args
    | ConditionalOperator { cond; then_expr; else_expr; _ } ->
        [ cond; then_expr; else_expr ]
    | CXXConstructExpr { args; _ } -> args
    | MemberExpr { base; _ } -> [ base ]
    | UnaryOperator { child; _ } -> [ child ]

  let rec find_map (f : t -> 'a option) (e : t) : 'a option =
    match f e with
    | Some a -> Some a
    | None -> children e |> List.find_map (find_map f)

  module OT = struct
    type nonrec t = t

    let compare = compare
  end

  module Map = Map.Make (OT)
  module Set = Set.Make (OT)

end

module Init = struct
  type t =
    | CXXConstructExpr of { constructor : Ty.t; ty : Ty.t }
    | InitListExpr of { ty : Ty.t; args : Expr.t list }
    | IExpr of Expr.t

  let to_exp (i : t) : Expr.t list =
    match i with
    | CXXConstructExpr _ -> []
    | InitListExpr i -> i.args
    | IExpr e -> [ e ]

  let to_type : t -> Ty.t = function
    | CXXConstructExpr { ty; _ } | InitListExpr { ty; _ } -> ty
    | IExpr e -> Expr.to_type e

  let to_string : t -> string = function
    | CXXConstructExpr _ -> "ctor"
    | InitListExpr i -> list_to_s Expr.to_string i.args
    | IExpr i -> Expr.to_string i

  let st_map (f : Expr.t -> ('s, Expr.t) State.t) (i : t) : ('s, t) State.t =
    let open State.Syntax in
    match i with
    | IExpr e ->
        let* e = f e in
        return (IExpr e)
    | InitListExpr { ty; args } ->
        let* args = State.list_map f args in
        return (InitListExpr { ty; args })
    | CXXConstructExpr _ -> return i

  let map (f : Expr.t -> Expr.t) (i : t) : t =
    match i with
    | IExpr e -> IExpr (f e)
    | InitListExpr { ty; args } ->
        let args = List.map f args in
        InitListExpr { ty; args }
    | CXXConstructExpr _ -> i
end

module Decl = struct
  type t = {
    var : Variable.t;
    ty : Ty.t;
    init : Init.t option;
    attrs : string list;
  }

  let types (d : t) : Ty.t list =
    d.ty :: Option.to_list (Option.map Init.to_type d.init)

  let make ~ty ~var ~init ~attrs : t = { ty; var; init; attrs }

  let from_undef ?(attrs = []) (ty_var : Ty_variable.t) : t =
    {
      ty = Ty_variable.ty ty_var;
      var = Ty_variable.name ty_var;
      init = None;
      attrs;
    }

  let from_init ?(attrs = []) (ty_var : Ty_variable.t) (init : Init.t) : t =
    {
      ty = Ty_variable.ty ty_var;
      var = Ty_variable.name ty_var;
      init = Some init;
      attrs;
    }

  let from_expr ?(attrs = []) (ty_var : Ty_variable.t) (expr : Expr.t) : t =
    from_init ~attrs ty_var (IExpr expr)

  let var (d : t) : Variable.t = d.var

  let get_shared (d : t) : Memory.t option =
    if List.mem C_lang.c_attr_shared d.attrs then
      Some
        {
          hierarchy = SharedMemory;
          size = Ty.get_array_dims d.ty;
          data_type = Ty.get_array_type d.ty;
        }
    else None

  let to_exp (d : t) : Expr.t list =
    match d.init with Some i -> Init.to_exp i | None -> []

  let to_string (d : t) : string =
    let i =
      match d.init with Some e -> " = " ^ Init.to_string e | None -> ""
    in
    let attr =
      if d.attrs = [] then ""
      else
        let attrs = String.concat " " d.attrs |> String.trim in
        attrs ^ " "
    in
    let ty = Ty.to_string d.ty in
    let x = Variable.name d.var in
    attr ^ ty ^ " " ^ x ^ i

  let st_map (f : Expr.t -> ('s, Expr.t) State.t) (d : t) : ('s, t) State.t =
    let open State.Syntax in
    let* init = State.option_map (Init.st_map f) d.init in
    return { d with init }

  let map (f : Expr.t -> Expr.t) (d : t) : t =
    let init = Option.map (Init.map f) d.init in
    { d with init }
end

module ForInit = struct
  type t = Decls of Decl.t list | Expr of Expr.t

  let to_exp (f : t) : Expr.t list =
    match f with
    | Decls l ->
        List.fold_left (fun l d -> Common.append_rev1 (Decl.to_exp d) l) [] l
    | Expr e -> [ e ]

  (* Returns the binders of a for statement *)
  let loop_vars : t -> Variable.t list =
    let rec exp_var (e : Expr.t) : Variable.t list =
      match e with
      | BinaryOperator { lhs = l; opcode = ","; rhs = r; _ } ->
          exp_var l |> Common.append_rev1 (exp_var r)
      | BinaryOperator { lhs = Ident x; opcode = "="; _ } ->
          [ Decl_expr.name x ]
      | _ -> []
    in
    function Decls l -> List.map Decl.var l | Expr e -> exp_var e

  let to_string : t -> string = function
    | Decls d -> list_to_s Decl.to_string d
    | Expr e -> Expr.to_string e

  let opt_to_string (o : t option) : string =
    o |> Option.map to_string |> Option.value ~default:""

  (* Thread an [Expr.t] rewriter through a [ForInit.t]: for [Decls],
     descend into each decl's initializer; for [Expr], rewrite directly. *)
  let st_map (f : Expr.t -> ('s, Expr.t) State.t) (fi : t) : ('s, t) State.t =
    let open State.Syntax in
    match fi with
    | Decls ds ->
        let* ds = State.list_map (Decl.st_map f) ds in
        return (Decls ds)
    | Expr e ->
        let* e = f e in
        return (Expr e)
end

type d_subscript = {
  name : Variable.t;
  index : Expr.t list;
  selector : Expr.t list;
  ty : Ty.t;
  location : Location.t;
}

let subscript_to_s (s : d_subscript) : string =
  Variable.name s.name ^ "[" ^ list_to_s Expr.to_string s.index ^ "]"

let make_subscript ~name ~index ?(selector = []) ~ty ~location () :
    d_subscript =
  { name; index; selector; ty; location }

type d_write = {
  (* The index *)
  target : d_subscript;
  (* The value being written to the array *)
  source : Expr.t;
  (* A payload is used to detect *benign data-races*. If we are able to identify a
     literal being written to the array, then the value is captured in the
     payload. This particular value is propagated to MAPs.
     *)
  payload : int option;
  guard : Expr.t option;
}

type d_read = {
  target : Variable.t;
  source : d_subscript;
  ty : Ty.t;
  guard : Expr.t option;
}

type d_atomic = {
  target : Variable.t;
  source : d_subscript;
  atomic : Expr.t Atomic.t;
  ty : Ty.t;
  guard : Expr.t option;
}

module Stmt = struct
  type t =
    | Skip
    | Seq of t * t
    | WriteAccessStmt of d_write
    | ReadAccessStmt of d_read
    | AtomicAccessStmt of d_atomic
    | BreakStmt
    | GotoStmt
    | ReturnStmt of Expr.t option
    | ContinueStmt
    | IfStmt of { cond : Expr.t; then_stmt : t; else_stmt : t }
    | DeclStmt of Decl.t list
    | WhileStmt of d_cond
    | ForStmt of d_for
    | DoStmt of d_cond
    | SwitchStmt of d_cond
    | DefaultStmt of t
    | CaseStmt of { case : Expr.t; body : t }
    | SExpr of Expr.t
    | AsmStmt of Expr.t Asm.t
    | BarrierOp of {
        op : C_lang.BarrierOp.t;
        target : d_subscript;
        args : Expr.t list;
        loc : Location.t option;
      }
    (* Local C++ lambda binding [auto v = [captures](params) { body };].
       D_to_imp lifts this to a synthetic [Imp.Kernel.t] with
       [visibility = Device] and rewrites every [v(args)] callsite into
       a call to that synthetic kernel with [captures @ args]. *)
    | LambdaDecl of {
        var : Variable.t;
        captures : (Variable.t * Expr.t) list;
        params : C_lang.Param.t list;
        body : t;
        ret_ty : Ty.t;
      }

  and d_cond = { cond : Expr.t; body : t }

  and d_for = {
    init : ForInit.t option;
    cond : Expr.t option;
    inc : t;
    body : t;
  }

  let seq (s1 : t) (s2 : t) : t =
    match (s1, s2) with Skip, s | s, Skip -> s | _, _ -> Seq (s1, s2)

  let from_list (l : t list) : t = List.fold_left seq Skip l
  let rec first : t -> t = function Seq (s, _) -> first s | s -> s
  let rec last : t -> t = function Seq (_, s) -> last s | s -> s

  let rec skip_last : t -> t = function
    | Seq (s1, s2) -> seq s1 (skip_last s2)
    | _ -> Skip

  let read_access (target : Variable.t) (source : d_subscript) : t =
    let ty =
      (* If it's an array get the elements type *)
      Ty.strip_array source.ty
    in
    ReadAccessStmt { target; source; ty; guard = None }

  let atomic_access (target : Variable.t) (source : d_subscript)
      (atomic : Expr.t Atomic.t) : t =
    let ty =
      (* If it's an array get the elements type *)
      Ty.strip_array source.ty
    in
    AtomicAccessStmt { target; source; atomic; ty; guard = None }

  (* Build [assert(<cond>);] as a statement. [d_to_imp] recognises
     calls to [assert] and lifts them to [Imp.Stmt.Assert] with
     [Global] visibility, which becomes an SMT hypothesis on every
     subsequent access. *)
  let assert_stmt (cond : Expr.t) : t =
    let assert_func : Expr.t =
      Ident
        (Decl_expr.from_name ~ty:J_type.int ~kind:Decl_expr.Kind.Function
           (Variable.from_name "assert"))
    in
    SExpr (CallExpr { func = assert_func; args = [ cond ]; ty = J_type.int })

  let rec assigned_call : t -> Location.t option =
    let target (e : Expr.t) : Location.t option =
      let rec spelled : Expr.t -> Location.t option = function
        | Ident v -> Some (Variable.location (Decl_expr.name v))
        | MemberExpr { base; _ } -> spelled base
        | _ -> None
      in
      Expr.find_map
        (function
          | Expr.BinaryOperator
              { opcode = "="; lhs = CallExpr { func; _ }; _ }
          | Expr.BinaryOperator
              { opcode = "="; lhs = CXXOperatorCallExpr { func; _ }; _ } ->
              Some (Option.value (spelled func) ~default:Location.empty)
          | _ -> None)
        e
    in
    let ( ||| ) (a : Location.t option) (b : unit -> Location.t option) =
      match a with Some _ -> a | None -> b ()
    in
    function
    | Skip | BreakStmt | GotoStmt | ContinueStmt -> None
    | Seq (s1, s2) -> assigned_call s1 ||| fun () -> assigned_call s2
    | SExpr e | CaseStmt { case = e; body = Skip } -> target e
    | ReturnStmt e -> Option.bind e target
    | AsmStmt _ | BarrierOp _ -> None
    | WriteAccessStmt w -> target w.source
    | ReadAccessStmt _ | AtomicAccessStmt _ -> None
    | DeclStmt l ->
        l
        |> List.find_map (fun (d : Decl.t) ->
               match d.init with Some (IExpr e) -> target e | _ -> None)
    | IfStmt { cond; then_stmt; else_stmt } ->
        target cond
        ||| fun () ->
        assigned_call then_stmt ||| fun () -> assigned_call else_stmt
    | WhileStmt { cond; body } | DoStmt { cond; body }
    | SwitchStmt { cond; body } ->
        target cond ||| fun () -> assigned_call body
    | DefaultStmt s | CaseStmt { body = s; _ } | LambdaDecl { body = s; _ } ->
        assigned_call s
    | ForStmt f ->
        (match f.cond with Some e -> target e | None -> None)
        ||| fun () -> assigned_call f.inc ||| fun () -> assigned_call f.body

  let rec to_s : t -> Indent.t list = function
    | Skip -> [ Line "skip;" ]
    | Seq (s1, s2) -> to_s s1 @ to_s s2
    | WriteAccessStmt w ->
        [
          Line
            ("wr " ^ subscript_to_s w.target ^ " = " ^ Expr.to_string w.source);
        ]
    | ReadAccessStmt r ->
        [
          Line ("rd " ^ Variable.name r.target ^ " = " ^ subscript_to_s r.source);
        ]
    | AtomicAccessStmt r ->
        [
          Line
            ("atomic " ^ Ty.to_string r.ty ^ " " ^ Variable.name r.target
           ^ " = " ^ subscript_to_s r.source);
        ]
    | ReturnStmt None -> [ Line "return" ]
    | ReturnStmt (Some e) -> [ Line ("return " ^ Expr.to_string e) ]
    | GotoStmt -> [ Line "goto" ]
    | ContinueStmt -> [ Line "continue" ]
    | BreakStmt -> [ Line "break" ]
    | ForStmt f ->
        let inc = to_string ~inline:true f.inc in
        let open Indent in
        [
          Line
            ("for ("
            ^ ForInit.opt_to_string f.init
            ^ "; " ^ Expr.opt_to_string f.cond ^ "; " ^ inc ^ ") {");
          Block (to_s f.body);
          Line "}";
        ]
    | WhileStmt { cond = b; body = s } ->
        [
          Line ("while (" ^ Expr.to_string b ^ ") {"); Block (to_s s); Line "}";
        ]
    | DoStmt { cond = b; body = s } ->
        [ Line "}"; Block (to_s s); Line ("do (" ^ Expr.to_string b ^ ") {") ]
    | SwitchStmt { cond = b; body = s } ->
        [ Line ("switch " ^ Expr.to_string b ^ " {"); Block (to_s s); Line "}" ]
    | CaseStmt c ->
        [ Line ("case " ^ Expr.to_string c.case ^ ":"); Block (to_s c.body) ]
    | DefaultStmt d -> [ Line "default:"; Block (to_s d) ]
    | IfStmt { then_stmt = Skip; else_stmt = Skip; _ } -> [ Line ";" ]
    | IfStmt { cond = b; then_stmt = s1; else_stmt = s2 } ->
        let s1 = to_s s1 in
        let s2 =
          let open Indent in
          if s2 = Skip then [] else [ Line "} else {"; Block (to_s s2) ]
        in
        let open Indent in
        [ Line ("if (" ^ Expr.to_string b ^ ") {"); Block s1 ]
        @ s2 @ [ Line "}" ]
    | DeclStmt [] -> []
    | DeclStmt [ d ] -> [ Line ("decl " ^ Decl.to_string d) ]
    | DeclStmt d ->
        let open Indent in
        [
          Line "decl {";
          Block (List.map (fun e -> Line (Decl.to_string e)) d);
          Line "}";
        ]
    | SExpr e -> [ Line (Expr.to_string e) ]
    | AsmStmt a -> [ Line (Asm.to_string Expr.to_string a ^ ";") ]
    | BarrierOp { op; target; args; _ } ->
        let args_s =
          if args = [] then ""
          else "(" ^ list_to_s Expr.to_string args ^ ")"
        in
        [
          Line
            (subscript_to_s target
            ^ "." ^ C_lang.BarrierOp.to_string op ^ args_s ^ ";");
        ]
    | LambdaDecl { var; captures; params; body; _ } ->
        let cs =
          captures
          |> List.map (fun (n, e) ->
              Variable.name n ^ " = " ^ Expr.to_string e)
          |> String.concat ", "
        in
        let ps = list_to_s C_lang.Param.to_string params in
        let open Indent in
        [
          Line
            ("lambda " ^ Variable.name var ^ " = [" ^ cs ^ "] (" ^ ps ^ ") {");
          Block (to_s body);
          Line "};";
        ]

  and to_string ?(inline = false) (s : t) : string =
    s |> to_s |> Indent.to_string |> fun s ->
    if inline then s |> Common.replace ~substring:"\n" ~by:" " |> String.trim
    else s

  let summarize : t -> string =
    let rec stmt_to_s : t -> string = function
      | WriteAccessStmt w ->
          "wr " ^ subscript_to_s w.target ^ " = " ^ Expr.to_string w.source
          ^ ";"
      | ReadAccessStmt r ->
          "rd " ^ Variable.name r.target ^ " = " ^ subscript_to_s r.source ^ ";"
      | AtomicAccessStmt r ->
          "atomic " ^ Variable.name r.target ^ " = " ^ subscript_to_s r.source
          ^ ";"
      | ReturnStmt None -> "return;"
      | ReturnStmt (Some e) -> "return " ^ Expr.to_string e ^ ";"
      | GotoStmt -> "goto;"
      | BreakStmt -> "break;"
      | ContinueStmt -> "continue;"
      | ForStmt f ->
          "for ("
          ^ ForInit.opt_to_string f.init
          ^ "; " ^ Expr.opt_to_string f.cond ^ "; "
          ^ to_string ~inline:true f.inc
          ^ ") {...}"
      | WhileStmt { cond = b; _ } -> "while (" ^ Expr.to_string b ^ ") {...}"
      | DoStmt { cond = b; _ } -> "{...} do (" ^ Expr.to_string b ^ ")"
      | SwitchStmt { cond = b; _ } -> "switch (" ^ Expr.to_string b ^ ") {...}"
      | CaseStmt c -> "case " ^ Expr.to_string c.case ^ ": {...}"
      | DefaultStmt _ -> "default: {...}"
      | IfStmt { cond = b; _ } ->
          "if (" ^ Expr.to_string b ^ ") {...} else {...}"
      | DeclStmt d ->
          "decl {" ^ String.concat ", " (List.map Decl.to_string d) ^ "}"
      | SExpr e -> Expr.to_string e
      | AsmStmt a -> Asm.to_string Expr.to_string a
      | BarrierOp { op; target; _ } ->
          subscript_to_s target ^ "." ^ C_lang.BarrierOp.to_string op ^ "(...)"
      | LambdaDecl { var; _ } ->
          "lambda " ^ Variable.name var ^ " = [...] (...) {...};"
      | Skip -> ";"
      | Seq _ as s -> stmt_to_s (first s) ^ "; ..."
    in
    stmt_to_s

  (* Post-order stateful rewrite over child statements: each [t]-typed
     child of [s] is rewritten first (via [Stmt.seq] for [Seq], so [Skip]
     elision still happens), then [f] is applied to the reconstructed
     node. Expression / decl / subscript fields are not recursed into —
     the caller handles those in [f]. *)
  let rec st_map (f : t -> ('s, t) State.t) (s : t) : ('s, t) State.t =
    let open State.Syntax in
    match s with
    | Skip | BreakStmt | GotoStmt | ContinueStmt | ReturnStmt _
    | DeclStmt _ | SExpr _ | AsmStmt _ | WriteAccessStmt _
    | ReadAccessStmt _ | AtomicAccessStmt _ | BarrierOp _ ->
        f s
    | Seq (a, b) ->
        let* a = st_map f a in
        let* b = st_map f b in
        f (seq a b)
    | IfStmt { cond; then_stmt; else_stmt } ->
        let* then_stmt = st_map f then_stmt in
        let* else_stmt = st_map f else_stmt in
        f (IfStmt { cond; then_stmt; else_stmt })
    | WhileStmt { cond; body } ->
        let* body = st_map f body in
        f (WhileStmt { cond; body })
    | DoStmt { cond; body } ->
        let* body = st_map f body in
        f (DoStmt { cond; body })
    | ForStmt { init; cond; inc; body } ->
        let* inc = st_map f inc in
        let* body = st_map f body in
        f (ForStmt { init; cond; inc; body })
    | SwitchStmt { cond; body } ->
        let* body = st_map f body in
        f (SwitchStmt { cond; body })
    | DefaultStmt body ->
        let* body = st_map f body in
        f (DefaultStmt body)
    | CaseStmt { case; body } ->
        let* body = st_map f body in
        f (CaseStmt { case; body })
    | LambdaDecl { var; captures; params; body; ret_ty } ->
        let* body = st_map f body in
        f (LambdaDecl { var; captures; params; body; ret_ty })

  (* Thread an [Expr.t] rewriter through every [Expr.t] child of [s]
     (including those nested in [d_subscript], [Decl], [ForInit], and
     [Asm] operands), without recursing into child statements. Compose
     with [st_map] when both Stmt-level and Expr-level rewrites are
     needed. *)
  let st_map_expr (f : Expr.t -> ('s, Expr.t) State.t) (s : t) :
      ('s, t) State.t =
    let open State.Syntax in
    let map_subscript (s : d_subscript) : ('s, d_subscript) State.t =
      let* selector = State.list_map f s.selector in
      let* index = State.list_map f s.index in
      return { s with selector; index }
    in
    match s with
    | Skip | BreakStmt | GotoStmt | ContinueStmt | Seq _ | DefaultStmt _ ->
        return s
    | WriteAccessStmt w ->
        let* target = map_subscript w.target in
        let* source = f w.source in
        let* guard = State.option_map f w.guard in
        return (WriteAccessStmt { w with target; source; guard })
    | ReadAccessStmt r ->
        let* source = map_subscript r.source in
        let* guard = State.option_map f r.guard in
        return (ReadAccessStmt { r with source; guard })
    | AtomicAccessStmt a ->
        let* source = map_subscript a.source in
        let* atomic = Atomic.map_state f a.atomic in
        let* guard = State.option_map f a.guard in
        return (AtomicAccessStmt { a with source; atomic; guard })
    | ReturnStmt e ->
        let* e = State.option_map f e in
        return (ReturnStmt e)
    | IfStmt { cond; then_stmt; else_stmt } ->
        let* cond = f cond in
        return (IfStmt { cond; then_stmt; else_stmt })
    | DeclStmt ds ->
        let* ds = State.list_map (Decl.st_map f) ds in
        return (DeclStmt ds)
    | WhileStmt { cond; body } ->
        let* cond = f cond in
        return (WhileStmt { cond; body })
    | DoStmt { cond; body } ->
        let* cond = f cond in
        return (DoStmt { cond; body })
    | ForStmt { init; cond; inc; body } ->
        let* init = State.option_map (ForInit.st_map f) init in
        let* cond = State.option_map f cond in
        return (ForStmt { init; cond; inc; body })
    | SwitchStmt { cond; body } ->
        let* cond = f cond in
        return (SwitchStmt { cond; body })
    | CaseStmt { case; body } ->
        let* case = f case in
        return (CaseStmt { case; body })
    | SExpr e ->
        let* e = f e in
        return (SExpr e)
    | AsmStmt a ->
        let r_op (op : Expr.t Asm.operand) : ('s, Expr.t Asm.operand) State.t =
          let* expr = f op.expr in
          return { Asm.constr = op.constr; expr }
        in
        let* outputs = State.list_map r_op a.outputs in
        let* inputs = State.list_map r_op a.inputs in
        return (AsmStmt { a with outputs; inputs })
    | BarrierOp { op; target; args; loc } ->
        let* target = map_subscript target in
        let* args = State.list_map f args in
        return (BarrierOp { op; target; args; loc })
    | LambdaDecl { var; captures; params; body; ret_ty } ->
        let* captures =
          State.list_map
            (fun (n, e) ->
              let* e = f e in
              return (n, e))
            captures
        in
        return (LambdaDecl { var; captures; params; body; ret_ty })
end


let for_loop_vars (f : Stmt.d_for) : Variable.t list =
  f.init |> Option.map ForInit.loop_vars |> Option.value ~default:[]

module Kernel = struct
  type t = {
    id : Imp.Function_id.t;
    decl_id : string option;
    code : Stmt.t;
    type_params : Ty_param.t list;
    params : Param.t list;
    attribute : KernelAttr.t;
  }

  let is_global (k : t) : bool = k.attribute |> KernelAttr.is_global

  (* [name] is the bare name clang reports, [label] adds the enclosing
     namespaces and the template arguments. *)
  let name (k : t) : string = Imp.Function_id.name k.id
  let label (k : t) : string = Imp.Function_id.label k.id
  let ty (k : t) : string = Imp.Function_id.ty k.id

  let header (k : t) : string =
    let open C_lang in
    let tps =
      if k.type_params <> [] then
        "[" ^ list_to_s Ty_param.to_string k.type_params ^ "]"
      else ""
    in
    KernelAttr.to_string k.attribute
    ^ " " ^ label k ^ " " ^ tps ^ "("
    ^ list_to_s Param.to_string k.params
    ^ ")"

  let to_s (k : t) : Indent.t list =
    let open Indent in
    [ Line (header k ^ " {"); Block (Stmt.to_s k.code); Line "}" ]

  let signature_to_s (k : t) : Indent.t list =
    let open Indent in
    [ Line (header k ^ ";") ]
end

module Def = struct
  type t =
    | Kernel of Kernel.t
    | Prototype of Kernel.t
    | Declaration of Decl.t
    | Typedef of Typedef.t
    | Record of Record.t
    | UsingNamespace of string
    | Enum of Imp.Enum.t
    (* Launch metadata is propagated through the C->D lowering as-is:
       the expression slots stay in [C_lang.Expr.t] form because no
       D_lang consumer rewrites or analyzes them yet. If a downstream
       stage starts driving assumptions (e.g. on grid/block shape), a
       parallel [D_lang.LaunchParam.t] with rewritten expressions can
       be introduced and rewrite_def updated to convert. *)
    | LaunchParam of C_lang.LaunchParam.t

  let is_device_kernel : t -> bool = function
    | Kernel k when Kernel.is_global k -> true
    | _ -> false

  let to_s (d : t) : Indent.t list =
    let open Indent in
    match d with
    | Declaration d -> [ Line (Decl.to_string d ^ ";") ]
    | Kernel k -> Kernel.to_s k
    | Prototype k -> Kernel.signature_to_s k
    | Typedef d -> Typedef.to_s d
    | Record r -> Record.to_s r
    | UsingNamespace n -> [ Line ("using namespace " ^ n ^ ";") ]
    | Enum e -> Imp.Enum.to_s e
    | LaunchParam lp -> C_lang.LaunchParam.to_s lp
end

module Program = struct
  type t = Def.t list

  let to_s (p : t) : Indent.t list =
    List.concat_map (fun k -> Def.to_s k @ [ Line "" ]) p

  let print (p : t) : unit = Indent.print (to_s p)
  let filter (pred : Def.t -> bool) (p : t) : t = List.filter pred p
end

module SignatureDB = struct
  module Function_id = Imp.Function_id

  module Signature = struct
    type t = { id : Function_id.t; params : Variable.t list }

    let to_string (s : t) : string =
      Function_id.label s.id
      ^ "(" ^ Variable.list_to_string s.params ^ "):"
      ^ Function_id.ty s.id

    let from_kernel (k : Kernel.t) : t =
      { id = k.Kernel.id; params = List.map Param.name k.Kernel.params }
  end

  type t = {
    (* Every function we can see, keyed by what makes it distinct. A
       prototype and the definition it declares reach the same key, which
       is what merges them. *)
    by_id : Kernel.t Function_id.Map.t;
    (* Which function a declaration belongs to. A call site names the
       declaration clang resolved it to, and several declarations of one
       function land on one identity. *)
    by_decl : Function_id.t StringMap.t;
    (* Candidates for a call that names no declaration: an unresolved
       overload, or a call synthesised by faial itself. *)
    by_name : Function_id.t list StringMap.t;
  }

  let empty : t =
    { by_id = Function_id.Map.empty; by_decl = StringMap.empty;
      by_name = StringMap.empty }

  let index (k : Kernel.t) (db : t) : t =
    let id = k.Kernel.id in
    let name = Function_id.name id in
    let ids = db.by_name |> StringMap.find_opt name |> Option.value ~default:[] in
    {
      db with
      by_decl =
        (match k.Kernel.decl_id with
         | Some d -> StringMap.add d id db.by_decl
         | None -> db.by_decl);
      by_name =
        StringMap.add name
          (if List.exists (Function_id.equal id) ids then ids else ids @ [ id ])
          db.by_name;
    }

  let add (k : Kernel.t) (db : t) : t =
    let db = index k db in
    { db with by_id = Function_id.Map.add k.Kernel.id k db.by_id }

  (* A body-less declaration must not displace the body it declares. *)
  let add_if_absent (k : Kernel.t) (db : t) : t =
    let db = index k db in
    if Function_id.Map.mem k.Kernel.id db.by_id then db
    else { db with by_id = Function_id.Map.add k.Kernel.id k db.by_id }

  let to_string (db : t) : string =
    let curr =
      db.by_id |> Function_id.Map.bindings |> List.map snd
      |> List.map Signature.from_kernel
      |> List.map Signature.to_string
      |> String.concat ", "
    in
    "[" ^ curr ^ "]"

  let get_id (id : Function_id.t) (db : t) : Kernel.t option =
    Function_id.Map.find_opt id db.by_id

  let named (name : string) (db : t) : Kernel.t list =
    db.by_name |> StringMap.find_opt name
    |> Option.value ~default:[]
    |> List.filter_map (fun id -> get_id id db)

  (* A call that names no declaration: clang could not resolve the
     overload, or faial synthesised the call. [ty] is "?" for the
     former, where the arity is all there is to go on; otherwise the
     type still has to match, because a name alone would bind the call
     to an unrelated overload. *)
  let get_unresolved ~(name : string) ~(ty : string) ~(arg_count : int)
      (db : t) : Kernel.t option =
    let candidates = named name db in
    if ty = "?" then
      List.find_opt
        (fun (k : Kernel.t) -> List.length k.params = arg_count)
        candidates
    else
      List.find_opt
        (fun (k : Kernel.t) -> Function_id.ty k.Kernel.id = ty)
        candidates

  let get_method ~(record : Ty.segment list) ~(name : string) ~(ty : string)
      ~(arg_count : int) (db : t) : Kernel.t option =
    let of_class (k : Kernel.t) : bool =
      Function_id.qualifier k.Kernel.id = record
    in
    let of_arity (n : int) : Kernel.t option =
      let candidates =
        named name db
        |> List.filter (fun (k : Kernel.t) ->
               List.length k.params = n && of_class k)
      in
      match
        List.filter
          (fun (k : Kernel.t) -> Function_id.ty k.Kernel.id = ty)
          candidates
      with
      | [ k ] -> Some k
      | _ -> ( match candidates with [ k ] -> Some k | _ -> None)
    in
    match of_arity (arg_count + 1) with
    | Some k -> Some k
    | None -> of_arity arg_count

  let lookup (e : Expr.t) (arg_count : int) (db : t) : Signature.t option =
    let ( let* ) = Option.bind in
    let by_decl (d : string option) : Kernel.t option =
      let* d = d in
      let* id = StringMap.find_opt d db.by_decl in
      get_id id db
    in
    (match e with
     | UnresolvedLookupExpr { name = n; _ } ->
         get_unresolved ~name:(Variable.name n) ~ty:"?" ~arg_count db
     | Ident { name = n; kind = Function | CXXMethod; ty; decl_id; qualifier }
       -> (
         match by_decl decl_id with
         | Some k -> Some k
         | None -> (
             match
               get_unresolved ~name:(Variable.name n) ~ty:(Ty.to_string ty)
                 ~arg_count db
             with
             | Some k -> Some k
             | None ->
                 let record =
                   List.concat_map
                     (fun c ->
                       Record.type_path (Ty.opaque c) |> Option.value ~default:[])
                     qualifier
                 in
                 get_method ~record ~name:(Variable.name n)
                   ~ty:(Ty.to_string ty) ~arg_count db))
     | MemberExpr { base; name; ty } ->
         let* record = Record.type_path (Expr.to_type base) in
         get_method ~record ~name ~ty:(Ty.to_string ty) ~arg_count db
     | _ -> None)
    |> Option.map Signature.from_kernel

  let from_program ?(policy = Opaque_call_policy.default) (p : Program.t) : t =
    List.fold_left
      (fun kernels d ->
        let open Def in
        match d with
        | Kernel k -> add k kernels
        | Prototype k ->
            if
              Opaque_call_policy.is_opaque policy ~name:(Kernel.name k)
                ~params:k.params
            then add_if_absent k kernels
            else kernels
        | Declaration _ | Typedef _ | Record _ | Enum _ | LaunchParam _
        | UsingNamespace _ ->
            kernels)
      empty p
end

(* ------------------------------------- *)

let ( @ ) = Common.append_tr

type 'a state = (Stmt.t, 'a) State.t

open State.Syntax

module AccessState = struct
  let counter = ref 1

  (*let make *)

  let add (s : Stmt.t) : unit state = State.update (fun s' -> Stmt.seq s' s)

  let add_var ?(kind = Variable.Kind.Synthesized) (lbl : string)
      (f : Variable.t -> Stmt.t) : Variable.t state =
    let count = !counter in
    counter := count + 1;
    let name : string = "@AccessState" ^ string_of_int count in
    let x : Variable.t = Variable.make ~name ~label:lbl ~kind () in
    let* () = add (f x) in
    return x

  let add_expr (expr : Expr.t) (ty : Ty.t) : Variable.t state =
    add_var (Expr.to_string expr) (fun name ->
        let ty_var = Ty_variable.make ~ty ~name in
        DeclStmt [ Decl.from_expr ty_var expr ])

  let add_write (a : d_subscript) (source : Expr.t) (payload : int option) :
      Variable.t state =
    let wr x =
      Stmt.WriteAccessStmt
        { target = a; source = Ident { x with ty = a.ty }; payload; guard = None }
    in
    match source with
    | Ident x ->
        let* () = add (wr x) in
        return (Decl_expr.name x)
    | _ ->
        add_var (subscript_to_s a) (fun x ->
            let ty =
              (* If it's an array get the elements type *)
              Ty.strip_array a.ty
            in
            let ty_var = Ty_variable.make ~name:x ~ty in
            Seq
              ( DeclStmt [ Decl.from_expr ty_var source ],
                wr (Decl_expr.from_name x) ))

  let add_read (a : d_subscript) : Variable.t state =
    add_var ~kind:ReadResult (subscript_to_s a) (fun x -> Stmt.read_access x a)

  let add_atomic (atomic : Expr.t Atomic.t) (source : d_subscript) :
      Variable.t state =
    add_var ~kind:AtomicResult (subscript_to_s source) (fun target ->
        Stmt.atomic_access target source atomic)

  let add_call (c : Expr.d_call) : Variable.t state =
    let e = Expr.CallExpr c in
    add_var ~kind:FunctionResult (Expr.to_string e) (fun x ->
        let ty = Ty_variable.make ~name:x ~ty:(Expr.to_type e) in
        DeclStmt [ Decl.from_expr ty e ])
end

let curand_read : Variable.Set.t =
  [
    "curand_uniform";
    "curand_normal";
    "curand_log_normal";
    "curand_uniform_double";
    "curand_normal_double";
    "curand_log_normal_double";
    "curand_poisson";
    "curand_discrete";
    "curand_normal2";
    "curand_log_normal2";
    "curand_normal2_double";
    "curand_log_normal2_double";
  ]
  |> List.map Variable.from_name
  |> Variable.Set.of_list

let not_ (e : Expr.t) : Expr.t =
  Expr.UnaryOperator { opcode = "!"; child = e; ty = J_type.bool }

let and_ (a : Expr.t) (b : Expr.t) : Expr.t =
  Expr.BinaryOperator { opcode = "&&"; lhs = a; rhs = b; ty = J_type.bool }

let conj (g : Expr.t) : Expr.t option -> Expr.t option = function
  | None -> Some g
  | Some existing -> Some (and_ existing g)

let rec stamp_guard (g : Expr.t) (s : Stmt.t) : Stmt.t =
  let open Stmt in
  match s with
  | Seq (a, b) -> Seq (stamp_guard g a, stamp_guard g b)
  | ReadAccessStmt r -> ReadAccessStmt { r with guard = conj g r.guard }
  | WriteAccessStmt w -> WriteAccessStmt { w with guard = conj g w.guard }
  | AtomicAccessStmt a -> AtomicAccessStmt { a with guard = conj g a.guard }
  | s -> s

let capture (m : 'a state) : Stmt.t * 'a = State.run m Stmt.Skip

let to_subscript : C_lang.Expr.t -> C_lang.Expr.c_array_subscript option =
  let cell (x : Decl_expr.t) (rhs : C_lang.Expr.t) :
      C_lang.Expr.c_array_subscript option =
    Some
      { lhs = Ident x; rhs; ty = x.ty; location = Variable.location x.name }
  in
  function
  | ArraySubscriptExpr a -> Some a
  | UnaryOperator { opcode = "*"; child = Ident x; _ } ->
      cell x (IntegerLiteral 0)
  | UnaryOperator
      {
        opcode = "*";
        child = BinaryOperator { lhs = Ident x; rhs; opcode = "+"; _ };
        _;
      } ->
      cell x rhs
  | CXXOperatorCallExpr
      { func = UnresolvedLookupExpr { name = v; _ }; args = [ Ident x ]; _ }
    when Variable.name v = "operator*" ->
      cell x (IntegerLiteral 0)
  | _ -> None

(* An atomic's address is a base plus whatever is added to it, in any
   association and either order: [p + i + j] parses as [(p + i) + j] and
   [i + p] addresses the same cell as [p + i]. Walk the additive spine on
   whichever side carries the memory, which is what tells the base from
   the offsets, and sum the rest into one index. Reading the base off the
   left instead would take [i] as the array in [i + p]. *)
let atomic_address (e : Expr.t) : (Decl_expr.t * Expr.t option) option =
  let is_memory (e : Expr.t) : bool =
    Ty.is_array_or_pointer (Expr.to_type e)
  in
  let add (e : Expr.t) : Expr.t option -> Expr.t option = function
    | None -> Some e
    | Some acc ->
        Some
          (Expr.BinaryOperator
             { opcode = "+"; lhs = e; rhs = acc; ty = Expr.to_type e })
  in
  let rec walk (e : Expr.t) (offset : Expr.t option) :
      (Decl_expr.t * Expr.t option) option =
    match e with
    | Ident x -> Some (x, offset)
    | BinaryOperator { lhs; rhs; opcode = "+"; _ } ->
        if is_memory lhs then walk lhs (add rhs offset)
        else if is_memory rhs then walk rhs (add lhs offset)
        else None
    | _ -> None
  in
  walk e None

let rec rewrite_exp (c : C_lang.Expr.t) : Expr.t state =
  let open Expr in
  match to_subscript c with
  | Some a -> rewrite_read a
  | None -> (
  match c with
  (* When an atomic happens *)
  | CallExpr { func = Ident f; args = (e : C_lang.Expr.t) :: args; ty }
    when Atomic.is_valid f.name -> (
      let atomic = Atomic.from_name f.name |> Option.get in
      let addressed : C_lang.Expr.t option =
        match e with
        | UnaryOperator { opcode = "&"; child; _ } -> Some child
        | _ -> None
      in
      let* addr =
        match addressed with
        | Some c when Option.is_some (to_subscript c) ->
            let* a = rewrite_subscript (Option.get (to_subscript c)) in
            return (Either.Left { a with ty })
        | Some (MemberExpr { base; name = field; _ }) -> (
            let* path = rewrite_member_path base field in
            match path with
            (* Only a member of something indexed is memory, the same rule
               an assignment to a member follows. *)
            | Some (name, (_ :: _ as index)) ->
                return
                  (Either.Left
                     (make_subscript ~name ~index ~ty
                        ~location:(Variable.location name) ()))
            | Some _ | None ->
                let* e = rewrite_exp e in
                return (Either.Right e))
        | Some _ | None ->
            let* e = rewrite_exp e in
            return (Either.Right e)
      in
      (* Rewrite the remaining arguments to extract any reads they
         may hide, but most are discarded — the access-protocol
         layer only needs to know that an atomic happened on the
         target address, not what was done. The operands threaded
         into [atomic.operation] are the lone exceptions:
         [Atomic_seed_read] uses the CAS [expected] expression to
         identify the seed variable, and [Scoped.imp_to_scoped]
         uses [Add]/[Sub]'s argument to gate the unique-return
         assert on a positive literal increment. *)
      let* args = State.list_map rewrite_exp args in
      let operation : Expr.t Atomic.Operation.t =
        match atomic.operation, args with
        | CAS _, expected :: new_val :: _ ->
            CAS { expected = Some expected; new_val = Some new_val }
        | CAS _, [ expected ] ->
            CAS { expected = Some expected; new_val = None }
        | Add _, value :: _ -> Add (Some value)
        | Sub _, value :: _ -> Sub (Some value)
        | Inc _, value :: _ -> Inc (Some value)
        | Dec _, value :: _ -> Dec (Some value)
        | And _, value :: _ -> And (Some value)
        | Or _, value :: _ -> Or (Some value)
        | Xor _, value :: _ -> Xor (Some value)
        | Min _, value :: _ -> Min (Some value)
        | Max _, value :: _ -> Max (Some value)
        | Exch _, value :: _ -> Exch (Some value)
        | op, _ -> op
      in
      let atomic = { atomic with operation } in
      match addr with
      | Either.Left a -> rewrite_atomic atomic a
      | Either.Right e -> (
          match atomic_address e with
          | Some (x, offset) ->
              let index = Option.value offset ~default:(IntegerLiteral 0) in
              rewrite_atomic atomic
                (make_subscript ~name:x.name ~index:[ index ]
                   ~location:(Variable.location f.name) ~ty ())
          | None -> return (CallExpr { func = Ident f; args = e :: args; ty })))
  (* When a write happens *)
  | BinaryOperator { lhs; rhs = src; opcode = "="; ty } -> (
      match to_subscript lhs with
      | Some a -> rewrite_write a src
      | None -> (
          let* member =
            match lhs with
            | MemberExpr { base; name = field; ty } ->
                rewrite_member_write base field ty src
            | _ -> return None
          in
          match member with
          | Some w -> w
          | None -> (
          match lhs with
          (* Nested scalar assignment [x = e] used as an expression value
             (e.g. [(idx /= k) % m] after [c_lang] desugars to
             [(idx = idx / k) % m]). Lift the assignment as a sequenced
             [SExpr] side-effect and substitute the LHS identifier for the
             expression's value, matching C's "assignment-expression
             evaluates to the new value of [x]". The [SExpr] is then
             lowered by [d_to_imp]'s existing arm to an
             [Infer_stmt.Assign]. *)
          | Ident d ->
              let* src = rewrite_exp src in
              let* () =
                AccessState.add
                  (SExpr
                     (BinaryOperator
                        { lhs = Ident d; opcode = "="; rhs = src; ty }))
              in
              return (Ident d)
          | ( CallExpr { func; args; ty = target }
            | CXXOperatorCallExpr { func; args; ty = target } ) as call ->
              let* func = rewrite_exp func in
              let* args = State.list_map rewrite_exp args in
              let* rhs = rewrite_exp src in
              let lhs =
                match call with
                | CallExpr _ -> CallExpr { func; args; ty = target }
                | _ -> CXXOperatorCallExpr { func; args; ty = target }
              in
              return (BinaryOperator { lhs; rhs; opcode = "="; ty })
          | lhs ->
              let* lhs = rewrite_exp lhs in
              let* rhs = rewrite_exp src in
              return (BinaryOperator { lhs; rhs; opcode = "="; ty }))))
  | CXXOperatorCallExpr
      { func = Ident { name = v; _ } as func; args = [ lhs; src ]; ty }
    when Variable.name v = "operator=" -> (
      match to_subscript lhs with
      | Some a -> rewrite_write a src
      | None ->
          let* func = rewrite_exp func in
          let* args = State.list_map rewrite_exp [ lhs; src ] in
          return (CXXOperatorCallExpr { func; args; ty }))
  (* When a read happens *)
  | ArraySubscriptExpr a -> rewrite_read a
  | CallExpr
      {
        func = Ident { name = n; kind = Function; _ };
        args =
          [
            (* seed *)
            (* seq *)
            (* offset *)
            _;
            _;
            _;
            (* write *)
            UnaryOperator { child = ArraySubscriptExpr a; opcode = "&"; _ };
          ];
        _;
      }
    when Variable.name n = "curand_init" ->
      rewrite_write a C_lang.Expr.unknown
  | CallExpr
      {
        func = Ident { name = n; kind = Function; _ };
        args =
          UnaryOperator { child = ArraySubscriptExpr a; opcode = "&"; _ } :: _;
        _;
      }
    when Variable.Set.mem n curand_read ->
      rewrite_read a
  | SizeOfExpr ty -> return (SizeOfExpr ty)
  | RecoveryExpr ty -> return (RecoveryExpr ty)
  | BinaryOperator { lhs; rhs; opcode = "&&"; ty } ->
      let* lhs = rewrite_exp lhs in
      let rhs_pre, rhs = capture (rewrite_exp rhs) in
      let* () = AccessState.add (stamp_guard lhs rhs_pre) in
      return (BinaryOperator { lhs; rhs; opcode = "&&"; ty })
  | BinaryOperator { lhs; rhs; opcode = "||"; ty } ->
      let* lhs = rewrite_exp lhs in
      let rhs_pre, rhs = capture (rewrite_exp rhs) in
      let* () = AccessState.add (stamp_guard (not_ lhs) rhs_pre) in
      return (BinaryOperator { lhs; rhs; opcode = "||"; ty })
  | BinaryOperator { lhs; rhs; opcode; ty } ->
      let* lhs = rewrite_exp lhs in
      let* rhs = rewrite_exp rhs in
      return (BinaryOperator { lhs; rhs; opcode; ty })
  | ConditionalOperator { cond; then_expr; else_expr; ty } ->
      let* cond = rewrite_exp cond in
      let then_pre, then_expr = capture (rewrite_exp then_expr) in
      let else_pre, else_expr = capture (rewrite_exp else_expr) in
      let* () = AccessState.add (stamp_guard cond then_pre) in
      let* () = AccessState.add (stamp_guard (not_ cond) else_pre) in
      return (ConditionalOperator { cond; then_expr; else_expr; ty })
  | Convert { arg; ty } ->
      let* arg = rewrite_exp arg in
      return (Convert { arg; ty })
  | CXXNewExpr { arg; ty } ->
      let* arg = rewrite_exp arg in
      return (CXXNewExpr { arg; ty })
  | CXXDeleteExpr { arg; ty } ->
      let* arg = rewrite_exp arg in
      return (CXXDeleteExpr { arg; ty })
  | CXXOperatorCallExpr
      {
        func =
          (UnresolvedLookupExpr { name = n; _ } | Ident { name = n; _ }) as func;
        args = [ _; _ ] as args;
        ty;
      }
    when Variable.name n = "operator+" ->
      let* func = rewrite_exp func in
      let* args = State.list_map rewrite_exp args in
      return (CXXOperatorCallExpr { func; args; ty })
  | CXXOperatorCallExpr { func; args; ty } when Ty.is_void ty ->
      let* func = rewrite_exp func in
      let* args = State.list_map rewrite_exp args in
      return (CXXOperatorCallExpr { func; args; ty })
  | CXXOperatorCallExpr { func; args; ty } ->
      let* func = rewrite_exp func in
      let* args = State.list_map rewrite_exp args in
      rewrite_call { func; args; ty }
  | CallExpr { func; args; ty } when Ty.is_void ty ->
      let* func = rewrite_exp func in
      let* args = State.list_map rewrite_exp args in
      return (CallExpr { func; args; ty })
  | CallExpr { func; args; ty } ->
      let* func = rewrite_exp func in
      let* args = State.list_map rewrite_exp args in
      rewrite_call { func; args; ty }
  | CXXConstructExpr c ->
      let* args = State.list_map rewrite_exp c.args in
      State.return (CXXConstructExpr { args; ty = c.ty })
  | UnaryOperator { child = ArraySubscriptExpr a; opcode = "&"; ty } ->
      rewrite_exp
        (BinaryOperator { lhs = a.lhs; opcode = "+"; rhs = a.rhs; ty })
  | UnaryOperator { child; opcode; ty } ->
      let* child = rewrite_exp child in
      return (UnaryOperator { child; opcode; ty })
  | MemberExpr { base; name; ty } ->
      let* base = rewrite_exp base in
      return (MemberExpr { base; name; ty })
  | Ident v -> return (Ident v)
  | UnresolvedLookupExpr { name = n; tys } ->
      return (UnresolvedLookupExpr { name = n; tys })
  | FloatingLiteral f -> return (FloatingLiteral f)
  | IntegerLiteral i -> return (IntegerLiteral i)
  | CharacterLiteral c -> return (CharacterLiteral c)
  | CXXBoolLiteralExpr b -> return (CXXBoolLiteralExpr b)
  | StmtExpr _ ->
      (* StmtExpr should have been eliminated by Stmt.rewrite_stmtexpr
         before D-lowering. Hitting this case means the pass wasn't
         wired in for this kernel. *)
      failwith
        "D_lang.rewrite_exp: StmtExpr leaked past rewrite_stmtexpr — \
         pass not run?"
  | LambdaExpr _ ->
      (* The DeclStmt-with-lambda-init recognizer in [rewrite_stmt]
         catches lambda bindings before [rewrite_exp] sees the
         [LambdaExpr]. Reaching this case means a lambda appeared in a
         non-binding expression position, which we don't support. *)
      failwith
        "D_lang.rewrite_exp: LambdaExpr in non-binding position — only \
         [auto v = lambda { ... }] is supported"
  | PackExpansion e ->
      (* C_lang preserves the parameter-pack-expansion wrapper, but no
         D_lang consumer uses it today: drop the wrapper at the
         boundary and lower the pattern. If a downstream stage starts
         caring about pack semantics, mirror the constructor in
         [D_lang.Expr.t]. *)
      rewrite_exp e
  | DependentScopeRef d ->
      (* C_lang preserves [Traits<T>::value]-style references with name
         and qualifier so analyses can equate them by syntactic
         identity. D_lang has no consumer that uses this today, so
         lower to [RecoveryExpr]. Mirror in [D_lang.Expr.t] when a
         downstream stage starts caring. *)
      return (RecoveryExpr d.ty))

(* The name of a location is its selections and the index is its
   subscripts, so a member on a subscript chain contributes a name segment
   and leaves the chain's indices in front of its own. A member reached
   through a pointer carries the subscript the arrow leaves implicit:
   [p->f] is [p[0].f]. *)
and deref_name (name : Variable.t) : Variable.t =
  Variable.update_name (fun n -> "*" ^ n) name

and rewrite_member_path (base : C_lang.Expr.t) (field : string) :
    (Variable.t * Expr.t list) option state =
  let select (name : Variable.t) : Variable.t =
    Variable.update_name (fun n -> n ^ "." ^ field) name
  in
  let implied : Expr.t list =
    if Ty.is_array_or_pointer (C_lang.Expr.to_type base) then
      [ Expr.IntegerLiteral 0 ]
    else []
  in
  match base with
  | Ident b -> return (Some (select b.name, implied))
  | MemberExpr { base = inner; name = outer; _ } -> (
      let* path = rewrite_member_path inner outer in
      match path with
      | Some (name, prefix) -> return (Some (select name, prefix @ implied))
      | None -> return None)
  | ArraySubscriptExpr a ->
      let* s = rewrite_subscript a in
      return (Some (select s.name, s.index))
  | _ -> return None

and rewrite_subscript (c : C_lang.Expr.c_array_subscript) : d_subscript state =
  let rec rewrite_subscript (c : C_lang.Expr.c_array_subscript)
      (indices : Expr.t list) (loc : Location.t option) : d_subscript state =
    let* idx = rewrite_exp c.rhs in
    let loc =
      Some
        (match loc with
        | Some loc -> Location.add_or_lhs loc c.location
        | None -> c.location)
    in
    let indices = idx :: indices in
    let plain ~(name : Variable.t) ~(ty : Ty.t) : d_subscript state =
      return
        { name; index = indices; selector = []; ty; location = Option.get loc }
    in
    match c.lhs with
    | ArraySubscriptExpr a -> rewrite_subscript a indices loc
    | Ident { name; ty; _ } -> plain ~name ~ty
    | MemberExpr { base; name = field; ty } -> (
        let* path = rewrite_member_path base field in
        match path with
        | Some (name, prefix) ->
            (* A pointer member is an indirection: the subscripts that follow
               index the region it points at, not the object it sits in, so
               the object's own index is not part of the address, and the
               region is named apart from the storage that holds its
               address. The object's index says which of those regions, so it
               stays as the selector rather than being discarded. *)
            if Ty.is_pointer ty then
              return
                {
                  name = deref_name name;
                  index = indices;
                  selector = prefix;
                  ty;
                  location = Option.get loc;
                }
            else
              return
                {
                  name;
                  index = prefix @ indices;
                  selector = [];
                  ty;
                  location = Option.get loc;
                }
        | None ->
            let ty = C_lang.Expr.to_type c.lhs in
            let* e = rewrite_exp c.lhs in
            let* x = AccessState.add_expr e ty in
            plain ~name:x ~ty)
    | e ->
        let ty = C_lang.Expr.to_type e in
        let* e = rewrite_exp e in
        let* x = AccessState.add_expr e ty in
        plain ~name:x ~ty
  in
  rewrite_subscript c [] None

and rewrite_write (a : C_lang.Expr.c_array_subscript) (src : C_lang.Expr.t) :
    Expr.t state =
  let* a = rewrite_subscript a in
  rewrite_write_target a src

(* A store whose target is a member selection rather than a subscript: the
   name comes from the selections and the index from the subscripts that
   preceded them, so [C[i].key = v] stores to [C.key] at [i]. *)
and rewrite_member_write (base : C_lang.Expr.t) (field : string) (ty : Ty.t)
    (src : C_lang.Expr.t) : Expr.t state option state =
  let* path = rewrite_member_path base field in
  match path with
  (* Only a member of something indexed is memory. A member of a plain
     object is a value, and assigning it is an assignment, which is what
     [c.b = dim3(256)] on a launch configuration relies on. A pointer
     member qualifies: the storage holding the address is memory of the
     object, so [s[i].p = A] is a write to it. *)
  | Some (name, (_ :: _ as index)) ->
      let target =
        make_subscript ~name ~index ~ty ~location:(Variable.location name) ()
      in
      return (Some (rewrite_write_target target src))
  | Some _ | None -> return None

and rewrite_write_target (a : d_subscript) (src : C_lang.Expr.t) : Expr.t state =
  let* src' = rewrite_exp src in
  (* Reach the literal through a conversion, applying each one on the way
     back out and once more for the element type the store lands in: it is
     the converted value that reaches the cell, and [char *b; b[i] = 200]
     leaves [-56] there. A payload left unconverted pairs off two writes
     that store different values. *)
  let convert (ty : Ty.t) (n : int) : int option =
    match Ty.to_scalar ty with
    | Some s when Scalar.is_int s -> Scalar.reduce n s
    | _ -> Some n
  in
  let rec literal (e : C_lang.Expr.t) : int option =
    match e with
    | IntegerLiteral x -> Some x
    | CXXBoolLiteralExpr b -> Some (if b then 1 else 0)
    | Convert c -> Option.bind (literal c.arg) (convert c.ty)
    | _ -> None
  in
  let payload = Option.bind (literal src) (convert (Ty.strip_array a.ty)) in
  let* x = AccessState.add_write a src' payload in
  return (Expr.ident ~ty:(C_lang.Expr.to_type src) x)

and rewrite_read (a : C_lang.Expr.c_array_subscript) : Expr.t state =
  let* a = rewrite_subscript a in
  let* x = AccessState.add_read a in
  return (Expr.ident ~ty:a.ty x)

and rewrite_atomic (atomic : Expr.t Atomic.t) (a : d_subscript) : Expr.t state =
  let* x = AccessState.add_atomic atomic a in
  return (Expr.ident ~ty:a.ty x)

and rewrite_call (a : Expr.d_call) : Expr.t state =
  let* x = AccessState.add_call a in
  return (Expr.ident ~ty:a.ty x)

let rewrite_decl (d : C_lang.Decl.t) : Decl.t state =
  let rewrite_init (c : C_lang.Init.t) : Init.t state =
    match c with
    | InitListExpr { ty; args } ->
        let* args = State.list_map rewrite_exp args in
        return (Init.InitListExpr { ty; args })
    | IExpr e ->
        let* e = rewrite_exp e in
        return (Init.IExpr e)
  in
  let* init = State.option_map rewrite_init d.init in
  return
    (Decl.make ~ty:(C_lang.Decl.ty d) ~var:(C_lang.Decl.var d) ~init
       ~attrs:(C_lang.Decl.attrs d))

let rewrite_for_init (f : C_lang.ForInit.t) : ForInit.t state =
  match f with
  | Decls d ->
      let* d = State.list_map rewrite_decl d in
      return (ForInit.Decls d)
  | Expr e ->
      let* e = rewrite_exp e in
      return (ForInit.Expr e)

let add : Stmt.t -> unit state = AccessState.add

let run0 (m : 'a state) : Stmt.t * 'a =
  let st, a = State.run m Stmt.Skip in
  (st, a)

(* Lowering for [switch (cond) { case k1: ... case kN: ... default: ... }].
   Produces a chain of [IfStmt] nodes so downstream passes treat each
   case body as guarded by [cond == kI] instead of always reachable.
   The previous [d_to_imp.ml] arm recursed into each case body without
   the label predicate, modelling every thread as executing every case;
   the lowering moves the guard into the IR before that arm sees it. *)

type switch_chunk =
  | SCMark of Expr.t option (* [Some e] = [case e:], [None] = [default:] *)
  | SCStop                       (* [break] inside the switch body *)
  | SCPlain of Stmt.t

(* Flatten the switch body into a left-to-right chunk list, unfolding
   nested [CaseStmt] / [DefaultStmt] so multi-label cases
   ([case 1: case 2: body;]) appear as adjacent marks. Clang nests
   such labels in the AST: outer [CaseStmt { case=1; body =
   CaseStmt { case=2; body = BODY }}]. The recursion handles that
   shape uniformly. *)
let rec switch_chunks (s : Stmt.t) : switch_chunk list =
  match s with
  | Seq (a, b) -> switch_chunks a @ switch_chunks b
  | BreakStmt -> [ SCStop ]
  | CaseStmt { case; body } -> SCMark (Some case) :: switch_chunks body
  | DefaultStmt body -> SCMark None :: switch_chunks body
  | Skip -> []
  | _ -> [ SCPlain s ]

(* A parsed case group: one or more labels (multi-label allowed) and
   the body that runs when any label matches, plus a flag for
   [default:]. *)
type switch_group = {
  labels : Expr.t list; (* may be empty if [default] is true and no
                                explicit cases share the body *)
  has_default : bool;
  body : Stmt.t;
}

(* Walk the chunk list and split into groups. Strict semantics: every
   case body must terminate in [break]. Multi-label cases (consecutive
   [SCMark]s with no intervening [SCPlain]) accumulate into one group.
   Returns [Error msg] on shapes the lowering does not handle yet
   (fall-through, stray statements before the first case, stray
   [break] outside a case). *)
type partition_state =
  | PEmpty
  | PInLabels of switch_group (* labels accumulated, no stmts yet *)
  | PInBody of switch_group   (* labels closed, stmts accumulating *)

let partition_switch (chunks : switch_chunk list)
    : (switch_group list, string) Result.t =
  let groups = ref [] in
  let state = ref PEmpty in
  let error = ref None in
  let close (g : switch_group) (stmts : Stmt.t list) =
    let body = Stmt.from_list (List.rev stmts) in
    groups := { g with body } :: !groups;
    state := PEmpty
  in
  let add_label (m : Expr.t option) : switch_group =
    match !state with
    | PEmpty -> {
        labels = (match m with Some e -> [ e ] | None -> []);
        has_default = m = None;
        body = Skip;
      }
    | PInLabels g -> {
        g with
        labels =
          (match m with Some e -> e :: g.labels | None -> g.labels);
        has_default = g.has_default || m = None;
      }
    | PInBody _ -> assert false (* caller checks first *)
  in
  let stmts_so_far = ref [] in
  List.iter (fun chunk ->
    if !error <> None then () else
    match chunk, !state with
    | SCMark m, PEmpty ->
        state := PInLabels (add_label m)
    | SCMark m, PInLabels _ ->
        state := PInLabels (add_label m)
    | SCMark _, PInBody _ ->
        error := Some "fall-through to next case (no break)"
    | SCPlain s, PInLabels g ->
        stmts_so_far := [ s ];
        state := PInBody g
    | SCPlain s, PInBody _ ->
        stmts_so_far := s :: !stmts_so_far
    | SCPlain _, PEmpty ->
        error := Some "statement before first case label"
    | SCStop, PInBody g ->
        close g !stmts_so_far;
        stmts_so_far := []
    | SCStop, PInLabels g ->
        (* empty case body terminated by break: [case k: break;]. *)
        close g [];
        stmts_so_far := []
    | SCStop, PEmpty ->
        error := Some "stray break outside any case")
    chunks;
  (match !state with
   | PInBody _ ->
       (* implicit fall-through to end of switch: not strictly an error
          because no following case exists, but no [break] was issued
          either. Treat as a closure for the final group; control
          falls out of the switch naturally. *)
       (match !state with
        | PInBody g -> close g !stmts_so_far
        | _ -> ())
   | PInLabels _ | PEmpty -> ());
  match !error with
  | Some e -> Error e
  | None -> Ok (List.rev !groups)

(* Build the if-else chain from the groups. Cases come first, default
   becomes the final [else]. Each group's labels disjoin with [||]. *)
let build_switch_chain (cond : Expr.t) (groups : switch_group list)
    : Stmt.t =
  let eq_to (label : Expr.t) : Expr.t =
    BinaryOperator {
      opcode = "==";
      lhs = cond;
      rhs = label;
      ty = J_type.bool;
    }
  in
  let group_cond (g : switch_group) : Expr.t option =
    match g.labels with
    | [] -> None
    | [ l ] -> Some (eq_to l)
    | l :: rest ->
        Some (
          List.fold_left
            (fun acc next ->
              Expr.BinaryOperator {
                opcode = "||";
                lhs = acc;
                rhs = eq_to next;
                ty = J_type.bool;
              })
            (eq_to l) rest)
  in
  let cases, defaults =
    List.partition (fun g -> not g.has_default) groups
  in
  let default_body : Stmt.t =
    match defaults with
    | [] -> Skip
    | gs -> Stmt.from_list (List.map (fun g -> g.body) gs)
  in
  List.fold_right (fun g acc ->
    match group_cond g with
    | None -> acc (* unreachable: a "case" group has at least one label *)
    | Some c ->
        Stmt.IfStmt { cond = c; then_stmt = g.body; else_stmt = acc })
    cases default_body

(* Top-level switch lowering with a warning on unsupported shapes.
   On failure, returns the original [SwitchStmt] (preserves the
   previous behaviour rather than introducing a new failure mode). *)
let lower_switch (cond : Expr.t) (body : Stmt.t) : Stmt.t =
  match partition_switch (switch_chunks body) with
  | Ok groups -> build_switch_chain cond groups
  | Error msg ->
      prerr_endline
        ("D_lang.lower_switch: " ^ msg
         ^ " — preserving switch; case labels will be ignored downstream");
      SwitchStmt { cond; body }

let rec rewrite_stmt (s : C_lang.Stmt.t) : Stmt.t =
  let run (m : unit state) =
    let code, () = run0 m in
    code
  in
  match s with
  | Skip -> Skip
  | Seq (s1, s2) -> Seq (rewrite_stmt s1, rewrite_stmt s2)
  | BreakStmt -> BreakStmt
  | GotoStmt -> GotoStmt
  | ReturnStmt None -> ReturnStmt None
  | ReturnStmt (Some e) ->
      run
        (let* e = rewrite_exp e in
         add (ReturnStmt (Some e)))
  | ContinueStmt -> ContinueStmt
  | IfStmt { cond; then_stmt; else_stmt } ->
      run
        (let* cond = rewrite_exp cond in
         add
           (IfStmt
              {
                cond;
                then_stmt = rewrite_stmt then_stmt;
                else_stmt = rewrite_stmt else_stmt;
              }))
  (* [auto v = [captures](params) { body };] — a singleton DeclStmt whose
     init is a LambdaExpr. Lift to a structured [LambdaDecl] so D_to_imp
     can emit a synthetic Imp.Kernel.t for it and rewrite call sites. *)
  | DeclStmt
      [
        ({ var; init = Some (IExpr (LambdaExpr l)); _ } : C_lang.Decl.t);
      ] ->
      run
        (let* captures =
           State.list_map
             (fun (n, e) ->
               let* e = rewrite_exp e in
               State.return (n, e))
             l.captures
         in
         add
           (LambdaDecl
              {
                var;
                captures;
                params = l.params;
                body = rewrite_stmt l.body;
                ret_ty = l.ret_ty;
              }))
  | DeclStmt ({ var; init = Some (IExpr (ArraySubscriptExpr a)); _ } :: d) ->
      run
        (let* a = rewrite_subscript a in
         add (Stmt.seq (Stmt.read_access var a) (rewrite_stmt (DeclStmt d))))
  | DeclStmt d ->
      run
        (let* d = State.list_map rewrite_decl d in
         add (DeclStmt d))
  | WhileStmt { cond; body } ->
      let s, cond = run0 (rewrite_exp cond) in
      (* we unroll the side effects, before the loop runs, and
       at the end of each iteration *)
      let body = Stmt.seq (rewrite_stmt body) s in
      Stmt.seq s (WhileStmt { cond; body })
  | ForStmt { init; cond; inc; body } ->
      let s1, cond = run0 (State.option_map rewrite_exp cond) in
      let inc = rewrite_stmt inc in
      (* we unroll the side effects, before the loop runs, and
        at the end of each iteration *)
      let body : Stmt.t = Stmt.seq (rewrite_stmt body) s1 in
      run
        (let* init = State.option_map rewrite_for_init init in
         add (Stmt.seq s1 (ForStmt { init; cond; inc; body })))
  | DoStmt { cond; body } ->
      let s, cond = run0 (rewrite_exp cond) in
      DoStmt { cond; body = Stmt.seq (rewrite_stmt body) s }
  | SwitchStmt { cond; body } ->
      run
        (let* cond = rewrite_exp cond in
         let body = rewrite_stmt body in
         add (lower_switch cond body))
  | CaseStmt { case; body } ->
      (* Reached only when a [CaseStmt] appears outside any enclosing
         [SwitchStmt] — ill-formed C the frontend tolerates. Keep the
         existing C-to-D shape rewrite so downstream behaviour is
         unchanged for that pathological case. *)
      run
        (let* case = rewrite_exp case in
         add (CaseStmt { case; body = rewrite_stmt body }))
  | DefaultStmt s -> DefaultStmt (rewrite_stmt s)
  | SExpr e ->
      run
        (let* e = rewrite_exp e in
         add (SExpr e))
  | AsmStmt a ->
      let rewrite_operand (op : C_lang.Expr.t Asm.operand) : Expr.t Asm.operand state =
        let* expr = rewrite_exp op.expr in
        State.return { Asm.constr = op.constr; expr }
      in
      run
        (let* outputs = State.list_map rewrite_operand a.outputs in
         let* inputs = State.list_map rewrite_operand a.inputs in
         add
           (AsmStmt
              {
                asm_string = a.asm_string;
                is_volatile = a.is_volatile;
                outputs;
                inputs;
                clobbers = a.clobbers;
                loc = a.loc;
              }))
  | BarrierOp { op; target; args; loc } ->
      (* The receiver of a barrier method is an lvalue reference to a barrier
         object; we must NOT hoist it as a memory read. Normalize all shapes
         into a d_subscript (array + index list). *)
      let rec target_to_subscript (e : C_lang.Expr.t)
          (indices : Expr.t list) : d_subscript state =
        match e with
        | UnaryOperator { opcode = "&"; child; _ }
        | UnaryOperator { opcode = "*"; child; _ } ->
            target_to_subscript child indices
        | BinaryOperator { opcode = "+"; lhs; rhs; _ } ->
            let* idx = rewrite_exp rhs in
            target_to_subscript lhs (idx :: indices)
        | Ident { name; ty; _ } ->
            State.return
              {
                name;
                index = indices;
                selector = [];
                ty;
                location = Variable.location name;
              }
        | e -> (
            match to_subscript e with
            | Some a ->
                let* idx = rewrite_exp a.rhs in
                target_to_subscript a.lhs (idx :: indices)
            | None ->
                failwith
                  ("BarrierOp: unsupported target shape: "
                 ^ C_lang.Expr.to_string e))
      in
      run
        (let* target = target_to_subscript target [] in
         let* args = State.list_map rewrite_exp args in
         add (BarrierOp { op; target; args; loc }))

let rewrite_kernel (k : C_lang.Kernel.t) : Kernel.t =
  {
    id = C_lang.Kernel.id k;
    decl_id = C_lang.Kernel.decl_id k;
    code = rewrite_stmt k.code;
    params = k.params;
    type_params = k.type_params;
    attribute = k.attribute;
  }

let rewrite_def (d : C_lang.Def.t) : Def.t =
  match d with
  | Kernel k -> Kernel (rewrite_kernel k)
  | Prototype k -> Prototype (rewrite_kernel k)
  | Declaration d ->
      let _, d = run0 (rewrite_decl d) in
      Declaration d
  | Typedef d -> Typedef d
  | Record r -> Record r
  | UsingNamespace n -> UsingNamespace n
  | Enum e -> Enum e
  | LaunchParam lp -> LaunchParam lp

let rewrite_program : C_lang.Program.t -> Program.t = List.map rewrite_def
