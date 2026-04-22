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
    | SizeOfExpr of J_type.t
    | CXXNewExpr of { arg : t; ty : J_type.t }
    | CXXDeleteExpr of { arg : t; ty : J_type.t }
    | RecoveryExpr of J_type.t
    | CharacterLiteral of int
    | BinaryOperator of d_binary
    | CallExpr of d_call
    | ConditionalOperator of {
        cond : t;
        then_expr : t;
        else_expr : t;
        ty : J_type.t;
      }
    | CXXConstructExpr of { args : t list; ty : J_type.t }
    | CXXBoolLiteralExpr of bool
    | CXXOperatorCallExpr of { func : t; args : t list; ty : J_type.t }
    | FloatingLiteral of float
    | IntegerLiteral of int
    | MemberExpr of { name : string; base : t; ty : J_type.t }
    | Ident of Decl_expr.t
    | UnaryOperator of { opcode : string; child : t; ty : J_type.t }
    | UnresolvedLookupExpr of { name : Variable.t; tys : J_type.t list }

  and d_binary = { opcode : string; lhs : t; rhs : t; ty : J_type.t }
  and d_call = { func : t; args : t list; ty : J_type.t }

  let ident ?(ty = J_type.int) ?(kind = Decl_expr.Kind.Var) (name : Variable.t)
      =
    Ident (Decl_expr.from_name ~ty ~kind name)

  let is_call : t -> bool = function CallExpr _ -> true | _ -> false

  let name = function
    | SizeOfExpr _ -> "SizeOfExpr"
    | CXXNewExpr _ -> "CXXNewExpr"
    | CXXDeleteExpr _ -> "CXXNewExpr"
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

  let rec to_type : t -> J_type.t = function
    | SizeOfExpr _ -> J_type.int
    | CXXNewExpr c -> c.ty
    | CXXDeleteExpr c -> c.ty
    | RecoveryExpr ty -> ty
    | CharacterLiteral _ -> J_type.char
    | BinaryOperator a -> a.ty
    | ConditionalOperator c -> to_type c.then_expr
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
    let opcode (o : string) (j : J_type.t) : string =
      if types then "(" ^ o ^ "." ^ J_type.to_string j ^ ")" else o
    in
    let var_name : Variable.t -> string =
      if provenance then Variable.repr else Variable.name
    in
    let rec exp_to_s : t -> string =
      let par (e : t) : string =
        match e with
        | BinaryOperator _ | ConditionalOperator _ -> "(" ^ exp_to_s e ^ ")"
        | UnaryOperator _ | CXXNewExpr _ | CXXDeleteExpr _ | Ident _
        | UnresolvedLookupExpr _ | CallExpr _ | CXXOperatorCallExpr _
        | CXXConstructExpr _ | CXXBoolLiteralExpr _ | MemberExpr _
        | IntegerLiteral _ | CharacterLiteral _ | RecoveryExpr _
        | FloatingLiteral _ | SizeOfExpr _ ->
            exp_to_s e
      in
      function
      | SizeOfExpr ty -> "sizeof(" ^ J_type.to_string ty ^ ")"
      | CXXNewExpr c ->
          "new " ^ J_type.to_string c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
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
          attr "ctor" ^ J_type.to_string c.ty ^ "(" ^ list_to_s exp_to_s c.args
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
end

module Init = struct
  type t =
    | CXXConstructExpr of { constructor : J_type.t; ty : J_type.t }
    | InitListExpr of { ty : J_type.t; args : Expr.t list }
    | IExpr of Expr.t

  let to_exp (i : t) : Expr.t list =
    match i with
    | CXXConstructExpr _ -> []
    | InitListExpr i -> i.args
    | IExpr e -> [ e ]

  let to_type : t -> J_type.t = function
    | CXXConstructExpr { ty; _ } | InitListExpr { ty; _ } -> ty
    | IExpr e -> Expr.to_type e

  let to_string : t -> string = function
    | CXXConstructExpr _ -> "ctor"
    | InitListExpr i -> list_to_s Expr.to_string i.args
    | IExpr i -> Expr.to_string i
end

module Decl = struct
  type t = {
    var : Variable.t;
    ty : J_type.t;
    init : Init.t option;
    attrs : string list;
  }

  let types (d : t) : J_type.t list =
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
      match J_type.to_c_type_res d.ty with
      | Ok ty ->
          Some
            {
              hierarchy = SharedMemory;
              size = C_type.get_array_length ty;
              data_type = C_type.get_array_type ty;
            }
      | Error _ -> None
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
    let ty = J_type.to_string d.ty in
    let x = Variable.name d.var in
    attr ^ ty ^ " " ^ x ^ i
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
end

type d_subscript = {
  name : Variable.t;
  index : Expr.t list;
  ty : J_type.t;
  location : Location.t;
}

let subscript_to_s (s : d_subscript) : string =
  Variable.name s.name ^ "[" ^ list_to_s Expr.to_string s.index ^ "]"

let make_subscript ~name ~index ~ty ~location : d_subscript =
  { name; index; ty; location }

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
}

type d_read = { target : Variable.t; source : d_subscript; ty : C_type.t }

type d_atomic = {
  target : Variable.t;
  source : d_subscript;
  atomic : Atomic.t;
  ty : C_type.t;
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
      source.ty
      |> J_type.to_c_type ~default:C_type.int
      (* If it's an array get the elements type *)
      |> C_type.strip_array
    in
    ReadAccessStmt { target; source; ty }

  let atomic_access (target : Variable.t) (source : d_subscript)
      (atomic : Atomic.t) : t =
    let ty =
      source.ty
      |> J_type.to_c_type ~default:C_type.int
      (* If it's an array get the elements type *)
      |> C_type.strip_array
    in
    AtomicAccessStmt { target; source; atomic; ty }

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
            ("atomic " ^ C_type.to_string r.ty ^ " " ^ Variable.name r.target
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
      | Skip -> ";"
      | Seq _ as s -> stmt_to_s (first s) ^ "; ..."
    in
    stmt_to_s
end

(*
let for_to_expr (f:Stmt.d_for) : Expr.t list =
  let l1 = f.init |> Option.map ForInit.to_exp |> Option.value ~default:[] in
  let l2 = f.cond |> Option.map (fun x -> [x]) |> Option.value ~default:[] in
  let l3 = f.inc |> Option.map (fun x -> [x]) |> Option.value ~default:[] in
  l1
  |> Common.append_rev1 l2
  |> Common.append_rev1 l3
*)
let for_loop_vars (f : Stmt.d_for) : Variable.t list =
  f.init |> Option.map ForInit.loop_vars |> Option.value ~default:[]

module Kernel = struct
  type t = {
    ty : string;
    name : string;
    code : Stmt.t;
    type_params : Ty_param.t list;
    params : Param.t list;
    attribute : KernelAttr.t;
  }

  let is_global (k : t) : bool = k.attribute |> KernelAttr.is_global

  let to_s (k : t) : Indent.t list =
    let tps =
      let open C_lang in
      if k.type_params <> [] then
        "[" ^ list_to_s Ty_param.to_string k.type_params ^ "]"
      else ""
    in
    let open Indent in
    [
      (let open C_lang in
       Line
         (KernelAttr.to_string k.attribute
         ^ " " ^ k.name ^ " " ^ tps ^ "("
         ^ list_to_s Param.to_string k.params
         ^ ")"));
    ]
    @ Stmt.to_s k.code
end

module Def = struct
  type t =
    | Kernel of Kernel.t
    | Declaration of Decl.t
    | Typedef of Typedef.t
    | Enum of Imp.Enum.t

  let is_device_kernel : t -> bool = function
    | Kernel k when Kernel.is_global k -> true
    | _ -> false

  let to_s (d : t) : Indent.t list =
    let open Indent in
    match d with
    | Declaration d -> [ Line (Decl.to_string d ^ ";") ]
    | Kernel k -> Kernel.to_s k
    | Typedef d -> Typedef.to_s d
    | Enum e -> Imp.Enum.to_s e
end

module Program = struct
  type t = Def.t list

  let to_s (p : t) : Indent.t list =
    List.concat_map (fun k -> Def.to_s k @ [ Line "" ]) p

  let print (p : t) : unit = Indent.print (to_s p)
  let filter (pred : Def.t -> bool) (p : t) : t = List.filter pred p
end

module SignatureDB = struct
  module Signature = struct
    type t = { kernel : string; ty : string; params : Variable.t list }

    let to_string (s : t) : string =
      s.kernel ^ "(" ^ Variable.list_to_string s.params ^ "):" ^ s.ty

    let from_kernel (k : Kernel.t) : t =
      let open Kernel in
      { kernel = k.name; ty = k.ty; params = List.map Param.name k.params }
  end

  type t = Kernel.t StringMap.t StringMap.t

  let add (k : Kernel.t) (db : t) : t =
    let sigs : Kernel.t StringMap.t =
      db |> StringMap.find_opt k.name |> Option.value ~default:StringMap.empty
    in
    let sigs : Kernel.t StringMap.t = StringMap.add k.ty k sigs in
    StringMap.add k.name sigs db

  let to_string (db : t) : string =
    let curr =
      db |> StringMap.bindings |> List.map snd
      |> List.concat_map (fun tys ->
          tys |> StringMap.bindings |> List.map snd
          |> List.map Signature.from_kernel
          |> List.map Signature.to_string)
      |> String.concat ", "
    in
    "[" ^ curr ^ "]"

  let get ~kernel ~ty ~arg_count (db : t) : Kernel.t option =
    db |> StringMap.find_opt kernel
    |> Option.map (fun sigs ->
        match StringMap.find_opt ty sigs with
        | Some e -> Some e
        | None ->
            (* iterate over all kernels and try finding one with
           the same number of parameters *)
            sigs |> StringMap.bindings |> List.map snd
            |> List.find_opt (fun k ->
                let open Kernel in
                List.length k.params = arg_count))
    |> Option.join

  let lookup (e : Expr.t) (arg_count : int) (db : t) : Signature.t option =
    let ( let* ) = Option.bind in
    let* kernel, ty =
      match e with
      | UnresolvedLookupExpr { name = n; _ } -> Some (Variable.name n, "?")
      | Ident { name = n; kind = Function; ty } ->
          Some (Variable.name n, J_type.to_string ty)
      | _ -> None
    in
    get ~kernel ~ty ~arg_count db |> Option.map Signature.from_kernel

  (* Returns a map from kernel name to name of parameters *)
  let from_program (p : Program.t) : t =
    List.fold_left
      (fun kernels d ->
        let open Def in
        match d with
        | Kernel k -> add k kernels
        | Declaration _ | Typedef _ | Enum _ -> kernels)
      StringMap.empty p
end

(* ------------------------------------- *)

let ( @ ) = Common.append_tr

type 'a state = (Stmt.t, 'a) State.t

open State.Syntax

module AccessState = struct
  let counter = ref 1

  (*let make *)

  let add (s : Stmt.t) : unit state = State.update (fun s' -> Stmt.seq s' s)

  let add_var (lbl : string) (f : Variable.t -> Stmt.t) : Variable.t state =
    let count = !counter in
    counter := count + 1;
    let name : string = "@AccessState" ^ string_of_int count in
    let x : Variable.t = { name; label = Some lbl; location = None } in
    let* () = add (f x) in
    return x

  let add_expr (expr : Expr.t) (ty : J_type.t) : Variable.t state =
    add_var (Expr.to_string expr) (fun name ->
        let ty_var = Ty_variable.make ~ty ~name in
        DeclStmt [ Decl.from_expr ty_var expr ])

  let add_write (a : d_subscript) (source : Expr.t) (payload : int option) :
      Variable.t state =
    let wr x =
      Stmt.WriteAccessStmt
        { target = a; source = Ident { x with ty = a.ty }; payload }
    in
    match source with
    | Ident x ->
        let* () = add (wr x) in
        return (Decl_expr.name x)
    | _ ->
        add_var (subscript_to_s a) (fun x ->
            let ty =
              a.ty
              |> J_type.to_c_type ~default:C_type.int
              (* If it's an array get the elements type *)
              |> C_type.strip_array
              |> J_type.from_c_type
            in
            let ty_var = Ty_variable.make ~name:x ~ty in
            Seq
              ( wr (Decl_expr.from_name x),
                DeclStmt [ Decl.from_expr ty_var source ] ))

  let add_read (a : d_subscript) : Variable.t state =
    add_var (subscript_to_s a) (fun x -> Stmt.read_access x a)

  let add_atomic (atomic : Atomic.t) (source : d_subscript) : Variable.t state =
    add_var (subscript_to_s source) (fun target ->
        Stmt.atomic_access target source atomic)

  let add_call (c : Expr.d_call) : Variable.t state =
    let e = Expr.CallExpr c in
    add_var (Expr.to_string e) (fun x ->
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

let rec rewrite_exp (c : C_lang.Expr.t) : Expr.t state =
  let open Expr in
  match c with
  (* When an atomic happens *)
  | CallExpr { func = Ident f; args = (e : C_lang.Expr.t) :: args; ty }
    when Atomic.is_valid f.name -> (
      let atomic = Atomic.from_name f.name |> Option.get in
      let* e : Expr.t = rewrite_exp e in
      (* we want to make sure we extract any reads from the other arguments,
       but we can safely discard the arguments, as we only care that an
       atomic happened, not exactly what was done by the atomic. *)
      let* args = State.list_map rewrite_exp args in
      match e with
      | Ident x ->
          rewrite_atomic atomic
            (make_subscript ~name:x.name ~index:[ IntegerLiteral 0 ]
               ~location:(Variable.location f.name) ~ty)
      | BinaryOperator { lhs = Ident x; rhs = e; opcode = "+"; _ } ->
          rewrite_atomic atomic
            (make_subscript ~name:x.name ~index:[ e ]
               ~location:(Variable.location f.name) ~ty)
      | _ -> return (CallExpr { func = Ident f; args = e :: args; ty }))
  (* When a write happens *)
  | BinaryOperator { lhs = ArraySubscriptExpr a; rhs = src; opcode = "="; _ } ->
      rewrite_write a src
  (*   *w = *)
  | BinaryOperator
      {
        lhs =
          UnaryOperator
            { opcode = "*"; child = Ident { name = x; ty; _ } as lhs; _ };
        rhs = src;
        opcode = "=";
        _;
      } ->
      rewrite_write
        { lhs; rhs = C_lang.Expr.unknown; ty; location = Variable.location x }
        src
  (*   *w = *)
  | BinaryOperator
      {
        lhs =
          UnaryOperator
            {
              opcode = "*";
              child =
                BinaryOperator
                  {
                    lhs = Ident { name = x; ty; _ } as lhs;
                    rhs;
                    opcode = "+";
                    _;
                  };
              _;
            };
        rhs = src;
        opcode = "=";
        _;
      } ->
      rewrite_write { lhs; rhs; ty; location = Variable.location x } src
  (* operator*[w] = *)
  | BinaryOperator
      {
        lhs =
          CXXOperatorCallExpr
            {
              func = UnresolvedLookupExpr { name = v; _ };
              args = [ (Ident { name = x; ty; _ } as lhs) ];
              _;
            };
        rhs = src;
        opcode = "=";
        _;
      }
    when Variable.name v = "operator*" ->
      rewrite_write
        { lhs; rhs = C_lang.Expr.unknown; ty; location = Variable.location x }
        src
  | CXXOperatorCallExpr
      { func = Ident { name = v; _ }; args = [ ArraySubscriptExpr a; src ]; _ }
    when Variable.name v = "operator=" ->
      rewrite_write a src
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
  | BinaryOperator { lhs; rhs; opcode; ty } ->
      let* lhs = rewrite_exp lhs in
      let* rhs = rewrite_exp rhs in
      return (BinaryOperator { lhs; rhs; opcode; ty })
  | ConditionalOperator { cond; then_expr; else_expr; ty } ->
      let* cond = rewrite_exp cond in
      let* then_expr = rewrite_exp then_expr in
      let* else_expr = rewrite_exp else_expr in
      return (ConditionalOperator { cond; then_expr; else_expr; ty })
  | CXXNewExpr { arg; ty } ->
      let* arg = rewrite_exp arg in
      return (CXXNewExpr { arg; ty })
  | CXXDeleteExpr { arg; ty } ->
      let* arg = rewrite_exp arg in
      return (CXXDeleteExpr { arg; ty })
  | CXXOperatorCallExpr { func; args; ty } ->
      let* func = rewrite_exp func in
      let* args = State.list_map rewrite_exp args in
      return (CXXOperatorCallExpr { func; args; ty })
  | CallExpr { func; args; ty } when J_type.matches C_type.is_void ty ->
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
    match c.lhs with
    | ArraySubscriptExpr a -> rewrite_subscript a indices loc
    | Ident { name; ty; _ } ->
        return { name; index = indices; ty; location = Option.get loc }
    | e ->
        let ty = C_lang.Expr.to_type e in
        let* e = rewrite_exp e in
        let* x = AccessState.add_expr e ty in
        return { name = x; index = indices; ty; location = Option.get loc }
  in
  rewrite_subscript c [] None

and rewrite_write (a : C_lang.Expr.c_array_subscript) (src : C_lang.Expr.t) :
    Expr.t state =
  let* src' = rewrite_exp src in
  let* a = rewrite_subscript a in
  let payload = match src with IntegerLiteral x -> Some x | _ -> None in
  let* x = AccessState.add_write a src' payload in
  return (Expr.ident ~ty:(C_lang.Expr.to_type src) x)

and rewrite_read (a : C_lang.Expr.c_array_subscript) : Expr.t state =
  let* a = rewrite_subscript a in
  let* x = AccessState.add_read a in
  return (Expr.ident ~ty:a.ty x)

and rewrite_atomic (atomic : Atomic.t) (a : d_subscript) : Expr.t state =
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
  let st, a = State.run Stmt.Skip m in
  (st, a)

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
         add (SwitchStmt { cond; body = rewrite_stmt body }))
  | CaseStmt { case; body } ->
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
              }))

let rewrite_kernel (k : C_lang.Kernel.t) : Kernel.t =
  {
    ty = k.ty;
    name = k.name;
    code = rewrite_stmt k.code;
    params = k.params;
    type_params = k.type_params;
    attribute = k.attribute;
  }

let rewrite_def (d : C_lang.Def.t) : Def.t =
  match d with
  | Kernel k -> Kernel (rewrite_kernel k)
  | Declaration d ->
      let _, d = run0 (rewrite_decl d) in
      Declaration d
  | Typedef d -> Typedef d
  | Enum e -> Enum e

let rewrite_program : C_lang.Program.t -> Program.t = List.map rewrite_def
