open Stage0
open Protocols
open Logger
open Imp
module StackTrace = Stack_trace
module TypeAlias = Type_alias
module KernelAttr = C_lang.KernelAttr
module StringMap = Common.StringMap
module Param = C_lang.Param
module Ty_param = C_lang.Ty_param

let ( @ ) = Common.append_tr

open Exp

let value_member ~(field : string) ~(ty : Ty.t) (x : Variable.t) : Variable.t =
  let select (n : string) : string =
    let n = n ^ "." ^ field in
    if Ty.is_pointer ty then "*" ^ n else n
  in
  Variable.update_name select x

let parse_var : D_lang.Expr.t -> Variable.t = function
  | Ident v -> v.name
  | MemberExpr { base = Ident v; name = field; ty } ->
      value_member ~field ~ty v.name
  | e ->
      failwith ("parse_var: unexpected expression: " ^ D_lang.Expr.to_string e)

type d_access = {
  location : Variable.t;
  mode : Access.Mode.t;
  index : D_lang.Expr.t list;
}

(* What a subscript of the given arity leaves behind: one type level per
   index, an array level and a pointer level alike. This is the only witness
   that answers the question a row needs asked, which is whether the subscript
   reached an element or stopped short of one and left a pointer. Neither of
   the obvious alternatives works: [Ty.strip_array] collapses [float *[4]] and
   [float *] to the same [float], and the unpeeled type is a pointer for the
   ordinary read [float x = A[i]] and an array for the row [float *r = t[i]],
   so testing it answers backwards. *)
let rec peel_subscript (n : int) (ty : Ty.t) : Ty.t option =
  if n <= 0 then Some ty
  else
    match ty.inner with
    | Ty.Pointer p -> peel_subscript (n - 1) p
    | Ty.Array a -> peel_subscript (n - 1) a.base
    | _ -> None

let peel_element (n : int) (ty : Ty.t) : Ty.t option =
  match peel_subscript n ty with
  | Some ty -> Some ty
  | None -> if n = 1 && not (Ty.is_array_or_pointer ty) then Some ty else None

(* A pointer as the C source spells it: a name with a displacement, or a
   choice between two of them. The choice is a tree rather than a field
   because each arm names memory of its own, and each settles the units of
   its displacement against that memory. *)
type d_pointer =
  | Leaf of { source : D_lang.Expr.t; offset : D_lang.Expr.t }
  | Choice of { cond : D_lang.Expr.t; if_true : d_pointer; if_false : d_pointer }

let rec map_offset (f : D_lang.Expr.t -> D_lang.Expr.t) : d_pointer -> d_pointer
    = function
  | Leaf { source; offset } -> Leaf { source; offset = f offset }
  | Choice { cond; if_true; if_false } ->
      Choice
        {
          cond;
          if_true = map_offset f if_true;
          if_false = map_offset f if_false;
        }

module Make (L : Logger) = struct
  let parse_bin ?(sign = Signedness.Signed) (op : string)
      (l : Imp.Infer_exp.t) (r : Infer_exp.t) : Infer_exp.t =
    match op with
    (* bool -> bool -> bool *)
    | "||" -> BExp (BRel (BOr, l, r))
    | "&&" -> BExp (BRel (BAnd, l, r))
    (* int -> int -> bool *)
    | "==" -> BExp (NRel (Eq, l, r))
    | "!=" -> BExp (NRel (Neq, l, r))
    | "<=" -> BExp (NRel (Le sign, l, r))
    | "<" -> BExp (NRel (Lt sign, l, r))
    | ">=" -> BExp (NRel (Ge sign, l, r))
    | ">" -> BExp (NRel (Gt sign, l, r))
    (* int -> int -> int *)
    | "+" -> NExp (Binary (Plus sign, l, r))
    | "-" -> NExp (Binary (Minus sign, l, r))
    | "*" -> NExp (Binary (Mult sign, l, r))
    | "/" -> NExp (Binary (Div sign, l, r))
    | "%" -> NExp (Binary (Mod sign, l, r))
    | ">>" -> NExp (Binary (RightShift sign, l, r))
    | "<<" -> NExp (Binary (LeftShift, l, r))
    | "^" -> NExp (Binary (BitXOr, l, r))
    | "|" -> NExp (Binary (BitOr, l, r))
    | "&" -> NExp (Binary (BitAnd, l, r))
    | _ ->
        L.warning (fun () -> "parse_bin: rewriting to unknown binary operator: " ^ op);
        let lbl =
          "(" ^ Infer_exp.to_string l ^ ") " ^ op ^ " " ^ "("
          ^ Infer_exp.to_string r ^ ")"
        in
        Unknown lbl

  let rec infer_expr (e : D_lang.Expr.t) : Infer_exp.t =
    match e with
    (* ---------------- CUDA SPECIFIC ----------- *)
    | MemberExpr { base = Ident base; name = field; ty } ->
        NExp (Var (value_member ~field ~ty base.name))
    (* ------------------ nexp ------------------------ *)
    | Ident d -> NExp (Var d.name)
    | SizeOfExpr ty ->
        let size = Ty.sizeof ty |> Option.value ~default:4 in
        L.warning (fun () ->
          "sizeof(" ^ Ty.to_string ty ^ ") = " ^ string_of_int size);
        NExp (Num size)
    | IntegerLiteral n | CharacterLiteral n -> NExp (Num n)
    | FloatingLiteral n ->
        L.warning (fun () ->
          "parse_nexp: converting float '" ^ Float.to_string n ^ "' to integer");
        NExp (Num (Float.to_int n))
    | ConditionalOperator o ->
        let b = infer_expr o.cond in
        let n1 = infer_expr o.then_expr in
        let n2 = infer_expr o.else_expr in
        NExp (NIf (b, n1, n2))
    | UnaryOperator { opcode = "~"; child = e; _ } ->
        let n = infer_expr e in
        NExp (Unary (BitNot, n))
    (* The thread-uniformity intrinsics are the surface syntax for the
       cross-thread primitive, so they become [IsThreadUnif] here rather
       than a [Pred] the inliner would have to lower later. *)
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args = [ arg ]; _ }
      when Exp.is_uniformity_intrinsic (Variable.name f) ->
        let n = infer_expr arg in
        BExp
          (if String.equal (Variable.name f) Exp.is_thread_unif_name then
             Infer_exp.is_thread_unif n
           else Infer_exp.is_thread_distinct n)
    (* Whitelisted pure functions / predicates are lifted to [NCall]
       / [Pred] nodes; their lowering bodies live in [Functions] /
       [Predicates] and run at [Constfold] / [Predicates.b_inline]
       time. [Functions.supported] covers [divUp] / [min] / [max] /
       [log2] / [log] / [sqrt] / [__ffs] / [__clz]; [Predicates.supported]
       covers [__is_pow2]. *)
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args; _ }
      when Functions.supported (Variable.name f) ->
        let args = List.map infer_expr args in
        NExp (NCall (Variable.name f, args))
    | CallExpr
        { func = Ident { name = f; kind = Function; _ }; args; _ }
      when Predicates.supported (Variable.name f) ->
        let args = List.map infer_expr args in
        BExp (Pred (Variable.name f, args))
    | BinaryOperator { lhs = l; opcode = "&"; rhs = IntegerLiteral 1; _ } ->
        let n = infer_expr l in
        BExp
          (Infer_exp.n_neq
             (NExp (Binary (Mod Signedness.Signed, n, NExp (Num 2))))
             (NExp (Num 0)))
    | BinaryOperator
        {
          opcode = "==";
          lhs =
            BinaryOperator
              {
                opcode = "&";
                lhs = Ident n1 as e;
                rhs =
                  BinaryOperator
                    { opcode = "-"; lhs = Ident n2; rhs = IntegerLiteral 1; _ };
                _;
              };
          rhs = IntegerLiteral 0;
          _;
        }
      when Decl_expr.equal n1 n2 ->
        let n = infer_expr e in
        BExp
          (Infer_exp.or_
             (BExp (Pred ("pow2", [ n ])))
             (BExp (Infer_exp.n_eq n (NExp (Num 0)))))
    | BinaryOperator { opcode = ","; lhs = _; rhs = e; _ } -> infer_expr e
    | BinaryOperator { opcode = o; lhs = n1; rhs = n2; ty } ->
        (* Clang applies C's usual arithmetic conversions and types an
           arithmetic operator with their result, so the operator already
           carries the answer. A comparison does not: its type is the result
           type, bool, so its signedness has to come from the operands, which
           clang has promoted for the same reason. *)
        let sign : Signedness.t =
          let unsigned =
            match o with
            | "<" | "<=" | ">" | ">=" ->
                Ty.is_unsigned (D_lang.Expr.to_type n1)
                || Ty.is_unsigned (D_lang.Expr.to_type n2)
            | _ -> Ty.is_unsigned ty
          in
          if unsigned then Unsigned else Signed
        in
        let n1 = infer_expr n1 in
        let n2 = infer_expr n2 in
        parse_bin ~sign o n1 n2
    | Convert c -> (
        let arg = infer_expr c.arg in
        match Ty.to_scalar c.ty with
        | Some ty -> NExp (Convert { ty; arg })
        | None -> arg)
    | CXXBoolLiteralExpr b -> BExp (Bool b)
    | UnaryOperator u when u.opcode = "!" ->
        let b = infer_expr u.child in
        BExp (BNot b)
    (* [&x] denotes the same location as [x], and the argument classifier
       reads the base variable out of the expression, so the address-of
       must not reach the unknown arm below. Only a bare variable is
       unwrapped: [&a[i]] would sequence a read of [a] that the source
       never performs. *)
    | UnaryOperator { opcode = "&"; child = Ident _ as child; _ } ->
        infer_expr child
    | CXXConstructExpr { args = [ Ident v ]; ty }
      when Ty.vector_lanes ty |> Option.is_some ->
        NExp (Var v.name)
    | RecoveryExpr _ | CXXConstructExpr _ | MemberExpr _ | CallExpr _
    | UnaryOperator _ | CXXOperatorCallExpr _ | UnresolvedLookupExpr _ ->
        let lbl = D_lang.Expr.to_string e in
        L.warning (fun () -> "parse_exp: rewriting to unknown: " ^ lbl);
        Unknown lbl
    | _ ->
        failwith
          ("WARNING: parse_nexp: unsupported expression " ^ D_lang.Expr.name e
         ^ " : " ^ D_lang.Expr.to_string e)

  let to_nexp (e : D_lang.Expr.t) : Exp.nexp Infer_exp.state =
    Infer_exp.to_nexp (infer_expr e)

  let try_to_nexp (e : D_lang.Expr.t) : Exp.nexp option =
    e |> infer_expr |> Infer_exp.to_nexp |> Infer_exp.no_unknowns

  (* -------------------------------------------------------------- *)

  module PathMap = Map.Make (struct
    type t = Ty.segment list

    let compare = Stdlib.compare
  end)

  module Context = struct
    type t = {
      sigs : D_lang.SignatureDB.t;
      arrays : Memory.t Variable.Map.t;
      globals : Params.t;
      assigns : (Variable.t * nexp) list;
      typedefs : TypeAlias.t;
      enums : Enum.t Variable.Map.t;
      records : Record.t PathMap.t;
      (* A type that is not itself a class template specialisation, a
         pointer to one for instance, arrives as a written spelling with no
         path behind it. Such a use is matched against the spelling a
         record's own path prints as. *)
      spellings : Record.t StringMap.t;
      scope : Ty.segment list;
      usings : string list;
    }

    let to_string (ctx : t) : string =
      [
        "sigs: " ^ D_lang.SignatureDB.to_string ctx.sigs;
        "arrays: ["
        ^ (ctx.arrays |> Variable.Map.to_list |> List.map fst
         |> List.map Variable.name |> String.concat ", ")
        ^ "]";
      ]
      |> String.concat "\n"

    let from_signature_db (sigs : D_lang.SignatureDB.t) : t =
      {
        sigs;
        arrays = Variable.Map.empty;
        globals = Params.empty;
        assigns = [];
        typedefs = TypeAlias.empty;
        enums = Variable.Map.empty;
        records = PathMap.empty;
        spellings = StringMap.empty;
        scope = [];
        usings = [];
      }

    let resolve (ty : Ty.t) (b : t) : Ty.t =
      TypeAlias.resolve ty b.typedefs

    let is_enum (ty : Ty.t) (ctx : t) : bool =
      let name = Ty.to_string ty |> Variable.from_name in
      Variable.Map.mem name ctx.enums

    let is_int (ty : Ty.t) (ctx : t) : bool =
      Ty.is_int ty || is_enum ty ctx

    let get_enum (ty : Ty.t) (ctx : t) : Enum.t =
      let name = Ty.to_string ty |> Variable.from_name in
      Variable.Map.find name ctx.enums

    let add_array (var : Variable.t) (m : Memory.t) (b : t) : t =
      { b with arrays = Variable.Map.add var m b.arrays }

    let add_assign (var : Variable.t) (n : Exp.nexp) (b : t) : t =
      { b with assigns = (var, n) :: b.assigns }

    let add_global (var : Variable.t) (ty : Ty.t) (b : t) : t =
      { b with globals = Params.add var ty b.globals }

    let add_typedef (d : Typedef.t) (b : t) : t =
      { b with typedefs = TypeAlias.add d b.typedefs }

    let add_record (r : Record.t) (b : t) : t =
      {
        b with
        records = PathMap.add (Record.path r) r b.records;
        spellings = StringMap.add (Record.qualified_name r) r b.spellings;
      }

    let set_scope (scope : Ty.segment list) (b : t) : t = { b with scope }

    let add_using (n : string) (b : t) : t = { b with usings = n :: b.usings }

    (* Unqualified lookup: a name written inside [rw] may mean [rw::Cut] or
       [Cut], so the enclosing scopes are tried innermost first. A path that
       names a specialisation is also tried against the template it
       instantiates, which is what a use inside the pattern reaches. *)
    let candidates (b : t) (path : Ty.segment list) :
        Ty.segment list list =
      let rec prefixes : 'a list -> 'a list list = function
        | [] -> [ [] ]
        | l -> l :: prefixes (List.filteri (fun i _ -> i < List.length l - 1) l)
      in
      (prefixes b.scope |> List.map (fun p -> p @ path))
      @ List.map (fun n -> Ty.segment n :: path) b.usings

    let find_record (path : Ty.segment list) (b : t) : Record.t option =
      match
        path :: Option.to_list (Ty.pattern path)
        |> List.concat_map (candidates b)
        |> List.find_map (fun p -> PathMap.find_opt p b.records)
      with
      | Some r -> Some r
      | None -> StringMap.find_opt (Ty.to_string (Ty.named path)) b.spellings

    let record_bases (path : Ty.segment list) (b : t) : Ty.segment list list =
      find_record path b
      |> Option.map (fun (r : Record.t) -> r.bases)
      |> Option.value ~default:[]

    let lookup_sig (e : D_lang.Expr.t) (arg_count : int) (db : t) :
        D_lang.SignatureDB.Signature.t option =
      D_lang.SignatureDB.lookup
        ~bases:(fun path -> record_bases path db)
        e arg_count db.sigs

    let lookup_fields (ty : Ty.t) (b : t) : Record.Field.t list option =
      let rec fields (r : Record.t) : Record.Field.t list =
        List.concat_map
          (fun base ->
            match PathMap.find_opt base b.records with
            | Some r -> fields r
            | None -> [])
          r.bases
        @ r.fields
      in
      let declared =
        Record.type_path ty
        |> Fun.flip Option.bind (fun path -> find_record path b)
      in
      match declared, ty.inner with
      | Some r, _ -> Some (fields r)
      | None, Ty.Struct { members = _ :: _ as members } ->
          Some
            (members
            |> List.map (fun (name, ty) -> Record.Field.make ~name ~ty ()))
      | None, _ -> None

    let lookup_record (ty : Ty.t) (b : t) : (string * Ty.t) list option =
      lookup_fields ty b
      |> Option.map
           (List.map (fun (f : Record.Field.t) -> (f.name, f.ty)))

    let record_size (ty : Ty.t) (b : t) : int option =
      Record.type_path ty
      |> Fun.flip Option.bind (fun path -> find_record path b)
      |> Fun.flip Option.bind (fun (r : Record.t) -> r.size)

    let add_enum (e : Enum.t) (b : t) : t =
      let assigns =
        if Enum.ignore e then b.assigns else Enum.to_assigns e @ b.assigns
      in
      { b with assigns; enums = Variable.Map.add e.var e b.enums }

    (* Generate the preamble *)
    let gen_preamble (c : t) : Imp.Stmt.t =
      c.assigns
      |> List.map (fun (k, v) -> Imp.Stmt.decl_set k v)
      |> Stmt.from_list
  end

  let members_of ~(is_memory : Ty.t -> bool) (ctx : Context.t) (ty : Ty.t) :
      (string * Ty.t) list =
    let rec walk (ty : Ty.t) : (string * Ty.t) list =
      match Context.lookup_record ty ctx with
      | None -> []
      | Some fields ->
          fields
          |> List.concat_map (fun (field, ty) ->
              let ty = Context.resolve ty ctx in
              if is_memory ty || Context.is_int ty ctx then [ (field, ty) ]
              else
                walk ty |> List.map (fun (p, ty) -> (field ^ "." ^ p, ty)))
    in
    walk ty

  let rec reaches_memory (ty : Ty.t) : bool =
    match ty.inner with
    | Ty.Pointer _ -> true
    | Ty.Array a -> reaches_memory a.base
    | _ -> false

  let members_of_value = members_of ~is_memory:reaches_memory
  let members_of_memory = members_of ~is_memory:Ty.is_array_or_pointer


  let rec infer_load_expr (exp : D_lang.Expr.t) : d_pointer option =
    let ( let* ) = Option.bind in
    match exp with
    | Ident { ty; _ }
      when Ty.is_pointer ty
           || Ty.is_array_or_pointer ty ->
        Some (Leaf { source = exp; offset = IntegerLiteral 0 })
    | MemberExpr { base = Ident _; ty; _ } when Ty.is_array_or_pointer ty ->
        Some (Leaf { source = exp; offset = IntegerLiteral 0 })
    | UnaryOperator { opcode = "&"; child; _ }
      when Ty.is_array (D_lang.Expr.to_type child) ->
        infer_load_expr child
    (* Both arms have to name memory: a conditional that picks between a
       pointer and something else is not a pointer this can resolve, and
       declining it leaves the whole declaration to the fallback. *)
    | ConditionalOperator { cond; then_expr; else_expr; _ } ->
        let* if_true = infer_load_expr then_expr in
        let* if_false = infer_load_expr else_expr in
        Some (Choice { cond; if_true; if_false })
    | CXXOperatorCallExpr
        { func = UnresolvedLookupExpr { name = n; _ }; args = [ lhs; rhs ]; ty }
    | CXXOperatorCallExpr
        { func = Ident { name = n; _ }; args = [ lhs; rhs ]; ty }
      when Variable.name n = "operator+" ->
        let* l = infer_load_expr lhs in
        Some
          (map_offset
             (fun offset : D_lang.Expr.t ->
               BinaryOperator { opcode = "+"; lhs = offset; rhs; ty })
             l)
    | CXXOperatorCallExpr _ -> None
    | BinaryOperator ({ lhs = l; _ } as b) ->
        let* l = infer_load_expr l in
        Some
          (map_offset
             (fun offset : D_lang.Expr.t -> BinaryOperator { b with lhs = offset })
             l)
    | _ -> None

  (* Rewrite the additive spine of a pointer expression so that every term
     counts bytes. C scales each term by the step of the type at which that
     addition happens, so a char view of [A + 5], plus 3, on an [int]
     array is 23 bytes, where reading only the outermost type would say 8.
     All-or-nothing: a node whose type has no step declines the whole
     expression, since a partly converted offset has no unit at all. *)
  let rec to_byte_offset (resolve : Ty.t -> Ty.t) (e : D_lang.Expr.t) :
      D_lang.Expr.t option =
    let ( let* ) = Option.bind in
    match e with
    | BinaryOperator b ->
        let* lhs = to_byte_offset resolve b.lhs in
        let* step = b.ty |> resolve |> Ty.pointee_size in
        let rhs : D_lang.Expr.t =
          if step = 1 then b.rhs
          else
            BinaryOperator
              {
                opcode = "*";
                lhs = b.rhs;
                rhs = IntegerLiteral step;
                ty = J_type.int;
              }
        in
        let e : D_lang.Expr.t = BinaryOperator { b with lhs; rhs } in
        Some e
    | Ident x ->
        let* _ = Decl_expr.ty x |> resolve |> Ty.pointee_size in
        Some e
    | IntegerLiteral _ -> Some e
    | _ -> None

  let asserts : Variable.Set.t =
    Variable.Set.of_list
      [
        Variable.from_name "assert";
        Variable.from_name "static_assert";
        Variable.from_name "__requires";
        Variable.from_name "__builtin_assume";
      ]

  (* CUDA vector lanes are named [x], [y], [z], [w] in argument order. *)
  let axes_of_arity : int -> string list option = function
    | 1 -> Some [ "x" ]
    | 2 -> Some [ "x"; "y" ]
    | 3 -> Some [ "x"; "y"; "z" ]
    | 4 -> Some [ "x"; "y"; "z"; "w" ]
    | _ -> None

  (* A CUDA vector constructor [make_<type><N>] initialises components
     [x]..[w] from its [N] arguments in order. *)
  let vector_ctor_fields (name : string) : string list option =
    if String.starts_with ~prefix:"make_" name then
      axes_of_arity (Char.code name.[String.length name - 1] - Char.code '0')
    else None

  (* Lane axes of a CUDA vector type ([uint2], [const uint3], ...). *)
  let vector_type_axes (ty : Ty.t) : string list option = Ty.vector_lanes ty

  let aggregate (ctx : Context.t) ~(decomposed : Variable.Set.t)
      ~(array : Variable.t) (ty : Ty.t) : Ty.t option =
    if
      Option.is_some (Context.lookup_record ty ctx)
      || Variable.Set.mem array decomposed
         && Option.is_some (vector_type_axes ty)
    then Some ty
    else None

  (* Touching a whole record touches every scalar under it, so the access
     is expanded into one per cell of every leaf. The cells are enumerated
     rather than bound by a variable: a bound index would need a
     declaration, and an unset declaration following an access is the shape
     [bind_uniform_reads] fills with the value that access loaded. A leaf
     whose extents are not all known, or whose cell count is beyond the
     cap, leaves the access naming the enclosing object. *)
  let expand_access (ctx : Context.t) ~(read : bool) ~(array : Variable.t)
      ~(index : Infer_exp.t list) ~(guard : Infer_exp.t option) (ty : Ty.t) :
      Infer_stmt.t option =
    let module T = Imp.Type_tree.Make (struct
      let members (ty : Ty.t) : Imp.Type_tree.Field.t list option =
        Context.lookup_fields (Context.resolve ty ctx) ctx
        |> Option.map
             (List.map (fun (f : Record.Field.t) ->
                  Imp.Type_tree.Field.make ?offset:f.offset ~name:f.name
                    ~ty:f.ty ()))

      let size (ty : Ty.t) : int option =
        Context.record_size (Context.resolve ty ctx) ctx
    end) in
    let access (name : Variable.t) (extra : Infer_exp.t list) : Infer_stmt.t =
      let path = Field_path.parse name in
      if read then Infer_stmt.Read { target = None; path; index = index @ extra; guard }
      else Infer_stmt.Write { path; index = index @ extra; payload = None; guard }
    in
    (* An extent the type does not state is one value per object, so it is an
       uninterpreted function of the indices that reach the object: two
       threads asking about the same one get the same bound, and the solver
       is free to choose it. *)
    let last (name : Variable.t) (dim : int option) : Infer_exp.t =
      match dim with
      | Some n -> Infer_exp.num (n - 1)
      | None ->
          Infer_exp.NExp
            (Infer_exp.n_bin
               (N_binary.Minus Signedness.Signed)
               (Infer_exp.NExp
                  (Infer_exp.NCall ("@extent_" ^ Variable.name name, index)))
               (Infer_exp.num 1))
    in
    let leaf (l : Imp.Type_tree.Leaf.t) : Infer_stmt.t =
      let name = Imp.Type_tree.Leaf.name l in
      let vars =
        List.mapi
          (fun i _ -> Variable.from_name ("@cell" ^ string_of_int i))
          l.dims
      in
      let body =
        access name
          (List.map (fun v -> Infer_exp.NExp (Infer_exp.Var v)) vars)
      in
      List.fold_right2
        (fun v dim (s : Infer_stmt.t) ->
          Infer_stmt.Foreach { var = v; last = last name dim; body = s })
        vars l.dims body
    in
    let own (t : Imp.Type_tree.t) : Imp.Type_tree.Leaf.t list =
      t.leaves
      |> List.filter (fun (l : Imp.Type_tree.Leaf.t) ->
             not (Field_path.is_deref l.path))
    in
    match own (T.of_declaration ~root:array ty) with
    | [] -> None
    | leaves ->
        (* A zero extent has no cells, and its loop would run from nought to
           minus one. *)
        if List.exists (fun (l : Imp.Type_tree.Leaf.t) ->
             List.exists (function Some n -> n <= 0 | None -> false) l.dims)
           leaves
        then None
        else Some (leaves |> List.map leaf |> Infer_stmt.from_list)

  let parameter_tree (ctx : Context.t) ~(root : Variable.t) (ty : Ty.t) :
      Imp.Type_tree.t =
    let module T = Imp.Type_tree.Make (struct
      let members (ty : Ty.t) : Imp.Type_tree.Field.t list option =
        Context.lookup_fields (Context.resolve ty ctx) ctx
        |> Option.map
             (List.map (fun (f : Record.Field.t) ->
                  Imp.Type_tree.Field.make ?offset:f.offset ~name:f.name
                    ~ty:f.ty ()))

      let size (ty : Ty.t) : int option =
        Context.record_size (Context.resolve ty ctx) ctx
    end) in
    T.of_parameter ~root ty

  (* The parameters a declaration contributes, and the lane views of one
     that came back as a single region rather than an array per field. *)
  let parse_param ~(expand_vectors : bool) (ctx : Context.t) (p : Param.t) :
      Kernel.Parameter.t list * (Variable.t * Imp.Pointer.t) list =
    let mk_array (h : Mem_hierarchy.t) (ty : Ty.t) : Memory.t =
      {
        hierarchy = h;
        size = Ty.get_array_dims ty;
        data_type = Ty.get_array_type ty;
        layout = None;
      }
    in
    let ty = Context.resolve p.ty_var.ty ctx in
    (* A const reference cannot be assigned through, so the parameter
       binds the referent's value and is classified as the referent. A
       mutable reference names storage the callee can write, which the
       substrate has no term for, so it stays unsupported. *)
    let ty =
      Ty.deref_const ty
      |> Option.map (fun ty -> Context.resolve ty ctx)
      |> Option.value ~default:ty
    in
    let x = p.ty_var.name in
    let h =
      if p.is_shared then Mem_hierarchy.SharedMemory
      else Mem_hierarchy.GlobalMemory
    in
    let to_params (members : (string * Ty.t) list) : Kernel.Parameter.t list =
      members
      |> List.map (fun (field, ty) ->
          let v = value_member ~field ~ty x in
          if Ty.is_array_or_pointer ty then
            Kernel.Parameter.array v
              { (mk_array h ty) with size = [ None ] }
          else Kernel.Parameter.scalar v ty)
    in
    if Context.is_enum ty ctx then
      ([ Kernel.Parameter.enum x (Context.get_enum ty ctx) ], [])
    else if Context.is_int ty ctx then ([ Kernel.Parameter.scalar x ty ], [])
    else if Ty.is_array_or_pointer ty then
      let leaves = parameter_tree ctx ~root:x ty in
      match Imp.Type_tree.to_region ~hierarchy:h leaves with
      | Some ((_, region), views) ->
          ([ Kernel.Parameter.array x region ], views)
      | None -> (
          let leaves =
            leaves
            |> Imp.Type_tree.to_arrays ~hierarchy:h
            |> List.filter (fun (v, _) -> not (Variable.equal v x))
            |> List.map (fun (v, m) -> Kernel.Parameter.array v m)
          in
          match leaves with
          | [] -> ([ Kernel.Parameter.array x (mk_array h ty) ], [])
          | leaves -> (Kernel.Parameter.unsupported x ty :: leaves, []))
    else
      let members = members_of_value ctx ty in
      if members <> [] then (to_params members, [])
      else
        match (if expand_vectors then vector_type_axes p.ty_var.ty else None)
        with
        (* A vector param [uintN v] exposes each lane [v.x], [v.y], ... as
           a uniform scalar parameter, so component reads resolve to a
           per-launch value rather than a thread-divergent free var. Only
           top-level kernels expand: a device function's vector params are
           bound through inlining, where lane-splitting would break the
           call's argument arity. *)
        | Some axes ->
            ( List.map
                (fun axis ->
                  let lane = Variable.update_name (fun n -> n ^ "." ^ axis) x in
                  Kernel.Parameter.scalar lane Ty.int)
                axes,
              [] )
        | None -> ([ Kernel.Parameter.unsupported x ty ], [])

  let returns_location : D_lang.SignatureDB.Signature.t option -> bool =
    function
    | Some s -> s.returns_location
    | None -> false

  let address_ty (ty : Ty.t) : Ty.t = Ty.make (Ty.Pointer ty)

  let rec spelled_location : D_lang.Expr.t -> Location.t = function
    | Ident v -> Variable.location (Decl_expr.name v)
    | MemberExpr { base; _ } -> spelled_location base
    | _ -> Location.empty

  let ref_result (x : Variable.t) : Variable.t =
    Variable.update_name (fun n -> "@ref_" ^ n) x

  let infer_stmt ~(private_scalars : Ty.t option Variable.Map.t)
      ~(decomposed : Variable.Set.t)
      ~(regions : Memory.t Variable.Map.t) (ctx : Context.t) :
      D_lang.Stmt.t -> Imp.Infer_stmt.t =
    let resolve (ty : Ty.t) : Ty.t = Context.resolve ty ctx in

    let view_step_of (e : D_lang.Expr.t) : int option =
      e |> D_lang.Expr.to_type |> resolve |> Ty.pointee_size
    in

    let elem_step_of (e : D_lang.Expr.t) : int option =
      match Variable.Map.find_opt (parse_var e) regions with
      | Some m -> Memory.step m
      | None -> e |> D_lang.Expr.to_type |> resolve |> Ty.cell_width
    in

    (* The view is the step of the pointer being declared, so it is settled
       once for the whole declaration; the element step and the byte
       conversion belong to each arm, since each arm names memory of its
       own. *)
    let infer_pointer (target : D_lang.Expr.t) (p : d_pointer) :
        Imp.Infer_pointer.t =
      let view = view_step_of target in
      let rec walk : d_pointer -> Imp.Infer_pointer.t = function
        | Leaf { source; offset } ->
            (* The steps and the offset's unit are one decision: bytes when
               both sides have a step and the spine converts, source units
               otherwise. *)
            let scaled =
              let ( let* ) = Option.bind in
              let* view = view in
              let* elem = elem_step_of source in
              let* offset = to_byte_offset resolve offset in
              Some (Some (Imp.Pointer.Step.make ~view ~elem), offset)
            in
            let step, offset = Option.value scaled ~default:(None, offset) in
            Imp.Infer_pointer.from_array (parse_var source)
            |> Imp.Infer_pointer.shift ~offset:(infer_expr offset) ~step
        | Choice { cond; if_true; if_false } ->
            Imp.Infer_pointer.select ~cond:(infer_expr cond)
              ~if_true:(walk if_true) ~if_false:(walk if_false)
      in
      walk p
    in

    let infer_location_alias (target : D_lang.Expr.t) (p : d_pointer) :
        Imp.Infer_stmt.t =
      let root = parse_var target in
      (* A cast changes the field layout, not the backing allocation. Bind
         each known field to its byte range; unknown layouts still reject. *)
      let field_view (field : Imp.Type_tree.Leaf.t) =
        let ( let* ) = Option.bind in
        let* layout = field.layout in
        let* width = Ty.width field.ty in
        if width <= 0 || Field_path.is_deref field.path
           || Variable.equal (Imp.Type_tree.Leaf.name field) root
           || not (List.for_all (fun stride -> stride mod width = 0) layout.strides)
        then None
        else
          let rec view = function
            | Leaf { source; offset } ->
                let* elem = elem_step_of source in
                let* offset = to_byte_offset resolve offset in
                let offset : D_lang.Expr.t =
                  BinaryOperator { opcode = "+"; lhs = offset;
                    rhs = IntegerLiteral layout.offset; ty = J_type.int }
                in
                Some (Imp.Infer_pointer.from_array (parse_var source)
                  |> Imp.Infer_pointer.shift ~offset:(infer_expr offset)
                       ~step:(Some (Imp.Pointer.Step.make ~view:width ~elem))
                  |> Imp.Infer_pointer.linear
                       ~scale:(List.map (fun stride -> stride / width) layout.strides)
                       ~shift:(Imp.Infer_exp.num 0))
            | Choice { cond; if_true; if_false } ->
                let* if_true = view if_true in
                let* if_false = view if_false in
                Some (Imp.Infer_pointer.select ~cond:(infer_expr cond)
                  ~if_true ~if_false)
          in
          let* pointer = view p in
          Some (Infer_stmt.LocationAlias
            { target = Imp.Type_tree.Leaf.name field; pointer })
      in
      let fields =
        parameter_tree ctx ~root (D_lang.Expr.to_type target |> resolve)
        |> fun tree -> List.filter_map field_view tree.leaves
      in
      Infer_stmt.from_list
        (Infer_stmt.LocationAlias { target = root; pointer = infer_pointer target p }
         :: fields)
    in

    let infer_decl (d : D_lang.Decl.t) : Infer_stmt.t =
      let x = d.var in
      match
        D_lang.Decl.types d
        |> List.map (fun ty -> Context.resolve ty ctx)
        |> List.find_opt (fun ty -> Context.is_int ty ctx)
      with
      | Some ty ->
          let init : Infer_exp.t option =
            match d.init with
            | Some (IExpr n) -> Some (infer_expr n)
            | _ -> None
          in
          let d : Infer_stmt.t =
            match init with
            | Some n -> Infer_stmt.decl_set ~ty x n
            | None -> Infer_stmt.decl_unset ~ty x
          in
          d
      | None ->
          let x = Variable.name x in
          let ty = Ty.to_string d.ty in
          L.warning (fun () ->
            "parse_decl: skipping non-int local variable '" ^ x ^ "' "
           ^ "type: " ^ ty);
          Skip
    in

    let byte_arg (a : D_lang.Expr.t) : D_lang.Expr.t =
      if a |> D_lang.Expr.to_type |> resolve |> Ty.is_array_or_pointer then
        Option.value (to_byte_offset resolve a) ~default:a
      else a
    in

    let expand_arg ?(param : Ty.t option) ~(expand_vectors : bool)
        (a : D_lang.Expr.t) : Infer_exp.t list =
      let ty = Context.resolve (D_lang.Expr.to_type a) ctx in
      let pointee =
        match ty.inner with
        | Ty.Pointer p | Ty.Array { base = p; _ } -> Some (Context.resolve p ctx)
        | _ -> None
      in
      let rec base_var (a : D_lang.Expr.t) : Variable.t option =
        match a with
        | Ident v -> Some v.name
        | CXXConstructExpr { args = [ a ]; _ } -> base_var a
        | UnaryOperator { opcode = "&"; child; _ } -> base_var child
        | MemberExpr { base; name = field; ty } ->
            base_var base |> Option.map (value_member ~field ~ty)
        | BinaryOperator { lhs; _ } -> base_var lhs
        | _ -> None
      in
      (* The argument list is built by the same descent as the parameter
         list, so the two cannot drift out of step and slide the positional
         binding along. *)
      let leaves ?(ty = ty) (root : Variable.t) :
          (Variable.t * Memory.t) list =
        let tree = parameter_tree ctx ~root ty in
        match
          Imp.Type_tree.to_region ~hierarchy:Mem_hierarchy.GlobalMemory tree
        with
        (* The fields are lanes of one region, which the parameter takes
           whole, so the argument is the region and not a leaf apiece. *)
        | Some _ -> []
        | None ->
            tree
            |> Imp.Type_tree.to_arrays ~hierarchy:Mem_hierarchy.GlobalMemory
            |> List.filter (fun (v, _) -> not (Variable.equal v root))
      in
      let named (v : Variable.t) : Infer_exp.t = NExp (Var v) in
      let rec rebase ~(root : Variable.t) ~(step : int) (e : D_lang.Expr.t) :
          D_lang.Expr.t option =
        match e with
        | Ident v -> Some (D_lang.Expr.Ident { v with name = root })
        | BinaryOperator b ->
            rebase ~root ~step b.lhs
            |> Option.map (fun lhs ->
                   let rhs : D_lang.Expr.t =
                     if step = 1 then b.rhs
                     else
                       BinaryOperator
                         {
                           opcode = "*";
                           lhs = b.rhs;
                           rhs = IntegerLiteral step;
                           ty = J_type.int;
                         }
                   in
                   D_lang.Expr.BinaryOperator { b with lhs; rhs })
        | _ -> None
      in
      let leaf_arg ((v, m) : Variable.t * Memory.t) : Infer_exp.t =
        match (a, Memory.step m) with
        | D_lang.Expr.BinaryOperator _, Some step -> (
            match rebase ~root:v ~step a with
            | Some e -> infer_expr e
            | None -> named v)
        | _ -> named v
      in
      let of_argument () : Infer_exp.t list =
        match (Option.is_some pointee, base_var a) with
        | true, Some v -> (
            match leaves v with
            | [] -> [ infer_expr (byte_arg a) ]
            | l -> infer_expr (byte_arg a) :: List.map leaf_arg l)
        | _, _ -> (
            match members_of_value ctx ty with
            | [] -> [ infer_expr (byte_arg a) ]
            | members ->
                members
                |> List.map (fun (field, ty) ->
                    match base_var a with
                    | Some v -> named (value_member ~field ~ty v)
                    | None ->
                        Unknown
                          (Variable.name (value_member ~field ~ty
                                            (Variable.from_name
                                               (D_lang.Expr.to_string a))))))
      in
      let of_parameter (ty : Ty.t) : int =
        let name =
          Option.value (base_var a) ~default:(Variable.from_name "@arg")
        in
        parse_param ~expand_vectors ctx
          (Param.make ~is_used:true ~is_shared:false
             ~ty_var:(Ty_variable.make ~name ~ty))
        |> fst |> List.length
      in
      match param with
      | None -> of_argument ()
      | Some param ->
          let expected = of_parameter param in
          let given = of_argument () in
          if List.length given = expected then given
          else List.init expected (fun _ -> infer_expr (byte_arg a))
    in
    let infer_call ?(result = None) (func : D_lang.Expr.t)
        (args : D_lang.Expr.t list) : Infer_stmt.t =
      let private_scalar = function
        | D_lang.Expr.UnaryOperator { opcode = "&"; child = Ident d; _ } ->
            Variable.Map.find_opt d.name private_scalars |> Option.join
            |> Option.map (fun ty -> (d.name, ty))
        | _ -> None
      in
      let scalar_copy =
        match func, args with
        | Ident f, [dst; src; size]
          when Variable.name f.name = "memcpy" ->
            let ( let* ) = Option.bind in
            let* dst, dst_ty = private_scalar dst in
            let* _, src_ty = private_scalar src in
            let* width = Ty.width dst_ty in
            let size = match size with
              | IntegerLiteral n -> Some n
              | SizeOfExpr ty -> Ty.width (resolve ty)
              | _ -> None
            in
            if size = Some width && Ty.width src_ty = Some width then
              (* Private scalar bit copies have no cross-thread memory effect.
                 Forget the value, rather than equating bits with a numeric cast. *)
              Some (Infer_stmt.from_list
                (Infer_stmt.Assign { var = dst; ty = dst_ty;
                   data = Infer_exp.Unknown "private memcpy" }
                 :: (match result with
                     | None -> []
                     | Some (var, ty) -> [Infer_stmt.Assign { var; ty;
                         data = Infer_exp.Unknown "private memcpy address" }])))
            else None
        | _ -> None
      in
      let arg_count = List.length args in
      match scalar_copy, func, result with
      | Some stmt, _, _ -> stmt
      (* Model [v = make_uintN(a, ...)] as per-component assignments
         [v.x := a; ...] so downstream member reads [v.x] resolve,
         instead of leaving [v] an opaque call result. *)
      | None, Ident { name = f; _ }, Some (var, _)
        when (match vector_ctor_fields (Variable.name f) with
              | Some fields -> List.length fields = arg_count
              | None -> false) ->
          let fields = Option.get (vector_ctor_fields (Variable.name f)) in
          List.map2
            (fun field arg ->
              let member =
                Variable.update_name (fun n -> n ^ "." ^ field) var
              in
              Infer_stmt.Assign
                { var = member; ty = Ty.int; data = infer_expr arg })
            fields args
          |> Infer_stmt.from_list
      | _ -> (
          match Context.lookup_sig func arg_count ctx with
          | Some s ->
              let args =
                match func with
                | MemberExpr { base; _ }
                  when List.length s.params = arg_count + 1 ->
                    base :: args
                | _ -> args
              in
              if List.length s.params = List.length args then
                let open Imp.Infer_stmt in
                let types =
                  if List.length s.types = List.length args then
                    List.map Option.some s.types
                  else List.map (fun _ -> None) args
                in
                Call
                  {
                    result;
                    id = s.id;
                    args =
                      List.concat
                        (List.map2
                           (fun param a ->
                             expand_arg ?param
                               ~expand_vectors:s.expand_vectors a)
                           types args);
                  }
              else Skip
          | None -> Skip)
    in

    let rec infer : D_lang.Stmt.t -> Imp.Infer_stmt.t = function
      | Skip -> Skip
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = []; _ })
        when Variable.name n = "__syncthreads" ->
          Sync (Sync.syncthreads ?loc:n.location ())
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = [ _ ]; _ })
        when Variable.name n = "sync" ->
          Sync (Sync.syncthreads ?loc:n.location ())
          (* Static assert may have a message as second argument *)
      | SExpr
          (CallExpr
             { func = Ident { name = n; kind = Function; _ }; args = b :: _; _ })
        when Variable.Set.mem n asserts ->
          Infer_stmt.Assert (infer_expr b)
      | SExpr (CallExpr { func; args; _ }) -> infer_call func args
      (* [f(i)] on an object already arrives with the object leading the
         argument list, which is the shape the callee's own parameter list
         takes once its [this] is a parameter. Only a statement is routed
         here: an operator call in expression position is a value, and the
         reference one of those returns is bound as a value rather than as
         the location it names. *)
      | SExpr (CXXOperatorCallExpr { func; args; _ }) -> infer_call func args
      | WriteAccessStmt w ->
          let path =
            D_lang.subscript_path w.target
            |> Field_path.map infer_expr
            |> Field_path.set_location w.target.location
          in
          let array =
            D_lang.subscript_name w.target
            |> Variable.set_location w.target.location
          in
          let index = List.map infer_expr (D_lang.subscript_index w.target) in
          let guard = Option.map infer_expr w.guard in
          let element =
            peel_element
              (List.length w.target.index)
              (resolve w.target.ty)
            |> Option.map resolve
            |> Fun.flip Option.bind (aggregate ctx ~decomposed ~array)
          in
          (match element with
           | None ->
               Infer_stmt.Write { path; index; payload = w.payload; guard }
           | Some ty -> (
               match expand_access ctx ~read:false ~array ~index ~guard ty with
               | Some p -> p
               | None ->
                   Infer_stmt.Write { path; index; payload = w.payload; guard }))
      | ReadAccessStmt r ->
          let path =
            D_lang.subscript_path r.source
            |> Field_path.map infer_expr
            |> Field_path.set_location r.source.location
          in
          let array =
            D_lang.subscript_name r.source
            |> Variable.set_location r.source.location
          in
          let index = List.map infer_expr (D_lang.subscript_index r.source) in
          let ty = r.ty |> resolve |> Ty.strip_array in
          let guard = Option.map infer_expr r.guard in
          let rd =
            Infer_stmt.Read { target = Some (ty, r.target); path; index; guard }
          in
          (* A subscript that stops short of an element leaves a pointer, and
             the target names that memory from here on. The subscript becomes
             the pointer rather than an access of its own, which is what
             writing it out in full already does: [t[i][j]] is one
             two-dimensional access on [t], with no separate load of the
             pointer in [t[i]]. Recording the load as well would put a
             one-index access on an array whose other accesses carry two, and
             the race check compares those as though they addressed the same
             cell. *)
          let leaves_memory =
            peel_subscript
              (List.length r.source.index)
              (resolve r.source.ty)
            |> Option.map Ty.is_array_or_pointer
            |> Option.value ~default:false
          in
          let element =
            peel_element
              (List.length r.source.index)
              (resolve r.source.ty)
            |> Option.map resolve
            |> Fun.flip Option.bind (aggregate ctx ~decomposed ~array)
          in
          if leaves_memory then
            let pointer =
              List.fold_left
                (fun p index -> Imp.Infer_pointer.row ~index p)
                (Imp.Infer_pointer.from_array array)
                index
            in
            Infer_stmt.LocationAlias { target = r.target; pointer }
          else (
            match element with
            | None -> rd
            | Some ety -> (
                (* The loaded record has no single value, so the target is
                   left unconstrained and each cell is read on its own. *)
                match expand_access ctx ~read:true ~array ~index ~guard ety with
                | Some p ->
                    Infer_stmt.seq (Infer_stmt.decl_unset ~ty r.target) p
                | None -> rd))
      | AtomicAccessStmt r ->
          let path =
            D_lang.subscript_path r.source
            |> Field_path.map infer_expr
            |> Field_path.set_location r.source.location
          in
          let index = List.map infer_expr (D_lang.subscript_index r.source) in
          let ty = r.ty |> resolve |> Ty.strip_array in
          let atomic = Atomic.map infer_expr r.atomic in
          let guard = Option.map infer_expr r.guard in
          Infer_stmt.Atomic
            { target = r.target; atomic; path; index; ty; guard }
      | IfStmt { cond; then_stmt; else_stmt } ->
          Imp.Infer_stmt.If (infer_expr cond, infer then_stmt, infer else_stmt)
      (* Support for location aliasing that declares a new variable *)
      | DeclStmt [ d ] -> (
          let ( let* ) = Option.bind in
          (* Detect non-standard declarations: *)
          let s =
            match d with
            (* Detect kernel-calls: *)
            | { init = Some (IExpr (CallExpr { func; args; _ })); _ }
              when Context.lookup_sig func (List.length args) ctx
                   |> Option.is_some ->
                (* Found a kernel call, so extract the call and parse
                   the rest of the declaration yet unsetting the first
                   decl. The signature lookup gates this arm so calls
                   to pure functions (e.g. [log2]) fall through to
                   [infer_decl], which lifts them via the [Functions]
                   registry into an [NCall]-init decl. *)
                let ty = resolve d.ty in
                let sigma = Context.lookup_sig func (List.length args) ctx in
                if returns_location sigma then
                  Some
                    (Infer_stmt.seq
                       (infer_call
                          ~result:(Some (ref_result d.var, address_ty ty))
                          func args)
                       (Infer_stmt.Read
                          {
                            target = Some (ty, d.var);
                            path = Field_path.root (ref_result d.var);
                            index = [ Infer_exp.num 0 ];
                            guard = None;
                          }))
                else Some (infer_call ~result:(Some (d.var, ty)) func args)
            (* Detect a by-value vector copy [uintN v = w]: bind each
               lane [v.x := w.x; ...] so the caller's component reads
               resolve through the temporary the compiler introduces
               for a by-value struct result. *)
            | { ty; init = Some (IExpr (Ident src)); _ }
              when vector_type_axes ty |> Option.is_some ->
                let axes = Option.get (vector_type_axes ty) in
                Some
                  (List.map
                     (fun axis ->
                       let lane = Variable.update_name (fun n -> n ^ "." ^ axis) in
                       Infer_stmt.Assign
                         {
                           var = lane d.var;
                           ty = Ty.int;
                           data = NExp (Var (lane src.name));
                         })
                     axes
                  |> Infer_stmt.from_list)
            (* Detect array alias: *)
            | { ty; init = Some (IExpr rhs); _ }
              when Ty.is_pointer ty || Ty.is_auto ty ->
                let d_ty = resolve d.ty in
                let lhs : D_lang.Expr.t =
                  Ident (Decl_expr.from_name ~ty:d_ty d.var)
                in
                let* a = infer_load_expr rhs in
                Some (infer_location_alias lhs a)
            (* Otherwise, nothing found *)
            | _ -> None
          in
          match s with
          | Some s -> s
          | None ->
              (* fall back to the default parsing of decls *)
              infer_decl d)
      | DeclStmt (d :: l) ->
          Infer_stmt.seq (infer (DeclStmt [ d ])) (infer (DeclStmt l))
      | DeclStmt [] -> Skip
      | SExpr
          (BinaryOperator
             { opcode = "=";
               lhs = (CallExpr { func; args; _ } | CXXOperatorCallExpr { func; args; _ });
               ty; _ })
        when Context.lookup_sig func (List.length args) ctx |> returns_location ->
          let ty = resolve ty in
          let location = spelled_location func in
          let target =
            Variable.from_name "@ref" |> Variable.set_location location
          in
          Infer_stmt.seq
            (infer_call ~result:(Some (target, address_ty ty)) func args)
            (Infer_stmt.Write
               {
                 path = Field_path.root target |> Field_path.set_location location;
                 index = [ Infer_exp.num 0 ];
                 payload = None;
                 guard = None;
               })
      | SExpr
          (BinaryOperator { opcode = "="; lhs = Ident { ty; _ } as lhs; rhs; _ })
        when Ty.is_pointer ty ->
          infer_load_expr rhs
          |> Option.map (infer_location_alias lhs)
          |> Option.value ~default:Infer_stmt.Skip
      | SExpr
          (BinaryOperator
             { opcode = "="; lhs = Ident { name = var; _ }; rhs; ty; _ }) ->
          let rhs = infer_expr rhs in
          let ty = ty |> resolve in
          Infer_stmt.Assign { var; ty; data = rhs }
      | SExpr
          (BinaryOperator
             { opcode = "=";
               lhs = MemberExpr { base = Ident base; name = field; _ };
               rhs; ty; _ }) ->
          let var =
            base.name |> Variable.update_name (fun n -> n ^ "." ^ field)
          in
          let rhs = infer_expr rhs in
          let ty = ty |> resolve in
          Infer_stmt.Assign { var; ty; data = rhs }
      (* [++x] / [--x] / [x++] / [x--] as a statement-expression. Clang
         normalises these to [x = x + 1] inside [for]-loop inc slots
         before c-to-json sees them, but they survive in other
         positions (e.g. statement-expressions, synthesised increments
         from CXXForRangeStmt lowering). Lower them to the same
         Assign shape so the loop-inference in [imp/lib/for.ml]
         recognises them as well-formed increments and avoids
         falling back to an unbounded [Star]. *)
      | SExpr
          (UnaryOperator
             { opcode = ("++" | "--") as opcode;
               child = Ident { name = var; _ };
               ty;
             }) ->
          let op : N_binary.t =
            if opcode = "++" then Plus Signedness.Signed
            else Minus Signedness.Signed
          in
          let data : Infer_exp.t =
            NExp (Binary (op, NExp (Var var), NExp (Num 1)))
          in
          let ty = ty |> resolve in
          Infer_stmt.Assign { var; ty; data }
      | ContinueStmt -> Continue
      | BreakStmt -> Break
      | GotoStmt -> Skip
      (* A vector-constructor return [return make_uintN(a, ...)] cannot
         be a single scalar return value, so lower it to per-lane
         assignments on a synthetic return variable and return that
         variable; the inliner then binds the caller's lanes from it. *)
      | ReturnStmt
          (Some (CallExpr { func = Ident { name = f; _ }; args; _ }))
        when (match vector_ctor_fields (Variable.name f) with
              | Some fields -> List.length fields = List.length args
              | None -> false) ->
          let fields = Option.get (vector_ctor_fields (Variable.name f)) in
          let retvar = Variable.from_name "@vec_return" in
          List.map2
            (fun field arg ->
              let lane = Variable.update_name (fun n -> n ^ "." ^ field) retvar in
              Infer_stmt.Assign
                { var = lane; ty = Ty.int; data = infer_expr arg })
            fields args
          @ [ Infer_stmt.Return (Some (NExp (Var retvar))) ]
          |> Infer_stmt.from_list
      | ReturnStmt e -> Return (Option.map infer_expr e)
      | SExpr _ -> Skip
      | ForStmt s ->
          let init : Infer_stmt.t =
            s.init
            |> Option.map (fun (f : D_lang.ForInit.t) : Infer_stmt.t ->
                let s : D_lang.Stmt.t =
                  match f with
                  | Decls d -> D_lang.Stmt.DeclStmt d
                  | Expr e -> SExpr e
                in
                infer s)
            |> Option.value ~default:Infer_stmt.Skip
          in
          let cond =
            s.cond |> Option.map infer_expr
            |> Option.value ~default:Infer_exp.true_
          in
          let body = infer s.body in
          let inc = infer s.inc in
          Infer_stmt.For { cond; init; body; inc }
      | DoStmt w ->
          let body = infer w.body in
          let cond = infer_expr w.cond in
          DoWhile (cond, body)
      | WhileStmt w ->
          let body = infer w.body in
          let cond = infer_expr w.cond in
          While (cond, body)
      | SwitchStmt { body = s; _ } | CaseStmt { body = s; _ } | DefaultStmt s ->
          infer s
      | AsmStmt a ->
          (* Outputs precede inputs in %N indexing, per GCC inline-asm. *)
          let operands : Exp.nexp option list =
            (a.outputs @ a.inputs)
            |> List.map (fun (o : D_lang.Expr.t Asm.operand) ->
                   try_to_nexp o.expr)
          in
          (match Ptx.parse ?loc:a.loc ~operands a.asm_string with
           | Some s -> Infer_stmt.Sync s
           | None ->
               L.warning (fun () ->
                 "asm: dropping (unrecognized PTX template): " ^ a.asm_string);
               Skip)
      | BarrierOp { op; target; args = _; loc } ->
          let mode : Sync.Mode.t =
            match op with
            | Arrive -> Sync.Mode.Arrive
            | Wait -> Sync.Mode.Wait
            | ArriveAndWait -> Sync.Mode.ArriveAndWait
            | ArriveAndDrop -> Sync.Mode.ArriveAndDrop
          in
          let index = List.map infer_expr target.index in
          Infer_stmt.SyncOp
            { mode; array = D_lang.subscript_name target; index; loc }
      | Seq (s1, s2) -> Seq (infer s1, infer s2)
      | LambdaDecl _ ->
          (* [Lift_lambdas.lift_program] runs at the start of
             [parse_program] and removes every [LambdaDecl]. *)
          failwith
            "D_to_imp.infer: LambdaDecl leaked past Lift_lambdas — \
             pass not run?"
    in
    infer

  type param = (Variable.t * Ty.t, Variable.t * Memory.t) Either.t

  and shared = (Variable.t * Memory.t) list * (Variable.t * Imp.Pointer.t) list

  let parse_shared (ctx : Context.t) (s : D_lang.Stmt.t) : shared =
    let open D_lang in
    (* A decl declares an array of barriers iff its element type, after
       typedef resolution, is [cuda::barrier<_>]. Such decls are identity-only
       (the array names a set of named barriers) and must not be added to
       data-memory tracking. *)
    let is_barrier_decl (d : Decl.t) : bool =
      let resolved = Context.resolve (Ty.strip_array d.ty) ctx in
      C_lang.BarrierOp.is_barrier_c_type resolved
      || C_lang.BarrierOp.is_barrier_base_type d.ty
    in
    let module T = Imp.Type_tree.Make (struct
      let members (ty : Ty.t) : Imp.Type_tree.Field.t list option =
        Context.lookup_fields (Context.resolve ty ctx) ctx
        |> Option.map
             (List.map (fun (f : Record.Field.t) ->
                  Imp.Type_tree.Field.make ?offset:f.offset ~name:f.name
                    ~ty:f.ty ()))

      let size (ty : Ty.t) : int option =
        Context.record_size (Context.resolve ty ctx) ctx
    end) in
    let members (d : Decl.t) :
        (Variable.t * array_t) list * (Variable.t * Imp.Pointer.t) list =
      let tree = T.of_declaration ~root:d.var (Context.resolve d.ty ctx) in
      match Imp.Type_tree.to_region ~hierarchy:SharedMemory tree with
      | Some (region, views) -> ([ region ], views)
      | None ->
          ( Imp.Type_tree.to_arrays ~hierarchy:SharedMemory tree
            |> List.filter (fun (v, _) -> not (Variable.equal v d.var)),
            [] )
    in
    let rec find_shared (found : shared) (s : Stmt.t) : shared =
      match s with
      | DeclStmt l ->
          List.concat_map
            (fun (d : Decl.t) ->
              if is_barrier_decl d then []
              else
                match Decl.get_shared d with
                | None -> []
                | Some a -> (
                    match members d with
                    | [], _ -> [ ([ (d.var, a) ], []) ]
                    | m -> [ m ]))
            l
          |> List.fold_left
               (fun (arrays, views) (a, v) ->
                 (Common.append_tr arrays a, Common.append_tr views v))
               found
      | WriteAccessStmt _ | ReadAccessStmt _ | AtomicAccessStmt _ | GotoStmt
      | ReturnStmt _ | ContinueStmt | BreakStmt | SExpr _ | AsmStmt _
      | BarrierOp _ | Skip | LambdaDecl _ ->
          (* [LambdaDecl] is removed by [Lift_lambdas.lift_program]
             before [parse_kernel] runs; if one survives here, it
             carries no shared declarations the caller could see. *)
          found
      | Seq (s1, s2) | IfStmt { then_stmt = s1; else_stmt = s2; _ } ->
          let found = find_shared found s1 in
          find_shared found s2
      | ForStmt { body = d; _ }
      | WhileStmt { body = d; _ }
      | DoStmt { body = d; _ }
      | SwitchStmt { body = d; _ }
      | DefaultStmt d
      | CaseStmt { body = d; _ } ->
          find_shared found d
    in
    find_shared ([], []) s

  (* The array map this model would derive from the parameter types, set
     beside the one the front end accumulates from declarations, so the two
     can be compared before anything depends on the derived one. *)
  let type_tree_report (ctx : Context.t) (k : D_lang.Kernel.t)
      (parameters : Imp.Kernel.Parameter.t list) : string =
    let module T = Imp.Type_tree.Make (struct
      let members (ty : Ty.t) : Imp.Type_tree.Field.t list option =
        Context.lookup_fields ty ctx
        |> Option.map
             (List.map (fun (f : Record.Field.t) ->
                  Imp.Type_tree.Field.make ?offset:f.offset ~name:f.name
                    ~ty:f.ty ()))

      let size (ty : Ty.t) : int option = Context.record_size ty ctx
    end) in
    let tree =
      k.params
      |> List.map (fun (p : Param.t) ->
          T.of_parameter ~root:p.ty_var.name
            (Context.resolve p.ty_var.ty ctx))
      |> Imp.Type_tree.concat
    in
    let declared =
      Variable.Map.bindings ctx.arrays
      |> List.filter (fun (x, _) ->
          not (String.contains (Variable.name x) '.'))
      |> List.map (fun (x, m) ->
          T.of_declaration ~dims:m.Memory.size
            ~root:x (Memory.data_ty m))
      |> Imp.Type_tree.concat
    in
    let tree = Imp.Type_tree.union tree declared in
    let derived =
      tree.leaves
      |> List.map (fun (l : Imp.Type_tree.Leaf.t) ->
          (Variable.name (Imp.Type_tree.Leaf.name l),
           Imp.Type_tree.Leaf.to_string l))
    in
    let accumulated =
      (parameters
       |> List.filter_map Imp.Kernel.Parameter.to_array
       |> List.map (fun (x, m) -> (Variable.name x, Memory.to_string m)))
      @ (Variable.Map.bindings ctx.arrays
         |> List.map (fun (x, m) -> (Variable.name x, Memory.to_string m)))
    in
    let names l = l |> List.map fst |> Common.StringSet.of_list in
    let d = names derived and a = names accumulated in
    let line (tag : string) (n : string) (l : (string * string) list) : string =
      "  " ^ tag ^ " " ^ n ^ " :: " ^ List.assoc n l
    in
    let both = Common.StringSet.inter d a |> Common.StringSet.elements in
    let only_derived = Common.StringSet.diff d a |> Common.StringSet.elements in
    let only_accumulated = Common.StringSet.diff a d |> Common.StringSet.elements in
    String.concat "\n"
      (("### type-tree " ^ Imp.Function_id.label k.id)
       :: List.map (fun n -> line "=" n derived) both
       @ List.map (fun n -> line "+" n derived) only_derived
       @ List.map (fun n -> line "-" n accumulated) only_accumulated)

  let parse_kernel ?(report = fun (_ : unit -> string) -> ())
      (ctx : Context.t) (k : D_lang.Kernel.t) :
      Context.t * Imp.Kernel.t =
    let ctx = Context.set_scope (Imp.Function_id.qualifier k.id) ctx in
    (* Parse kernel parameters *)
    let expand_vectors =
      match k.attribute with KernelAttr.Default -> true | _ -> false
    in
    let per_param = List.map (parse_param ~expand_vectors ctx) k.params in
    let param_views = List.concat_map snd per_param in
    let per_param = List.map fst per_param in
    let parameters = List.concat per_param in
    let decomposed =
      per_param
      |> List.filter_map (function
           | (x, Imp.Kernel.Parameter.Type.Unsupported _) :: _ :: _ -> Some x
           | _ -> None)
      |> Variable.Set.of_list
    in
    let shared, lane_views = parse_shared ctx k.code in
    let decomposed =
      (shared |> List.map fst)
      @ (Variable.Map.bindings ctx.arrays |> List.map fst)
      |> List.filter_map (fun x ->
             let p = Field_path.parse x in
             if Field_path.is_root p then None else Some (Field_path.base p))
      |> List.fold_left (Fun.flip Variable.Set.add) decomposed
    in
    let ctx =
      List.fold_left (fun ctx (x, m) -> Context.add_array x m ctx) ctx shared
    in
    let regions =
      List.fold_left
        (fun m (x, t) ->
          match t with
          | Imp.Kernel.Parameter.Type.Array a -> Variable.Map.add x a m
          | Scalar _ | Enum _ | Unsupported _ -> m)
        ctx.arrays parameters
    in
    let code, return =
      let add_decl locals (d : D_lang.Decl.t) =
        let ty = Context.resolve d.ty ctx in
        let value =
          if Variable.Map.mem d.var locals || Params.mem d.var ctx.globals
             || Variable.Map.mem d.var regions || d.attrs <> []
             || Ty.to_scalar ty = None then None else Some ty
        in
        Variable.Map.add d.var value locals
      in
      let initial = List.fold_left
        (fun locals (p : Param.t) -> Variable.Map.add p.ty_var.name None locals)
        Variable.Map.empty k.params in
      let private_scalars, _ = D_lang.Stmt.st_map
        (fun stmt locals ->
          let decls = match stmt with
            | DeclStmt ds | ForStmt { init = Some (Decls ds); _ } -> ds
            | _ -> []
          in
          List.fold_left add_decl locals decls, stmt)
        k.code initial in
      infer_stmt ~private_scalars ~decomposed ~regions ctx k.code
      |> Imp.Atomic_seed_read.rewrite
      |> Infer_stmt.infer
    in
    report (fun () -> type_tree_report ctx k parameters);
    (* type parameters become global variables because c-t-j doesn't represent
     type instantiations.
    *)
    let rec add_type_params (params : Params.t) : Ty_param.t list -> Params.t =
      function
      | [] -> params
      | TemplateType _ :: l -> add_type_params params l
      | NonTypeTemplate x :: l ->
          let params =
            if Ty.is_int x.ty then Params.add x.name x.ty params else params
          in
          add_type_params params l
    in
    let global_variables = add_type_params ctx.globals k.type_params in
    let open Imp.Stmt in
    let code = Seq (Context.gen_preamble ctx, code) in
    let code =
      List.fold_left
        (fun code (target, pointer) ->
          Seq (LocationAlias { target; pointer }, code))
        code (lane_views @ param_views)
    in
    let open Imp.Kernel in
    ( ctx,
      {
        id = k.D_lang.Kernel.id;
        code;
        parameters;
        global_arrays = ctx.arrays;
        global_variables;
        visibility =
          (match k.attribute with
          | Default -> Visibility.Global
          | Auxiliary -> Visibility.Device);
        block_dim = None;
        grid_dim = None;
        return;
        unsupported =
          D_lang.Stmt.assigned_call
            ~resolved:(fun func arg_count ->
              Context.lookup_sig func arg_count ctx |> returns_location)
            k.code
          |> Option.map (fun location ->
                 Imp.Rejected_kernel.Reason.WriteThroughCall { location });
      } )

  let parse_program ?(policy = Opaque_call_policy.default)
      ?(report = fun (_ : unit -> string) -> ())
      (p : D_lang.Program.t) : Imp.Kernel.t list =
    (* Hoist C++ lambdas into synthetic [D_lang.Kernel.t] entries with
       [Auxiliary] visibility before parsing. The synthetic kernels
       become regular [Imp.Kernel.t] with [Visibility.Device] and are
       inlined by [Imp.Inline_calls]. *)
    let p = Lift_lambdas.lift_program p in
    let rec parse_p (ctx : Context.t) (p : D_lang.Program.t) : Imp.Kernel.t list
        =
      match p with
      | Declaration v :: l ->
          let b =
            (* Skip arrays of barriers: they're identity-only, not data memory. *)
            if C_lang.BarrierOp.is_barrier_base_type v.ty then ctx
            else
              (* make sure we resolve the type before we query it *)
              let ty = Context.resolve v.ty ctx in
              let is_mut = not (Ty.is_const ty) in
              let declare (h : Mem_hierarchy.t) (ctx : Context.t) : Context.t =
                let module T = Imp.Type_tree.Make (struct
                  let members (ty : Ty.t) : Imp.Type_tree.Field.t list option =
                    Context.lookup_fields (Context.resolve ty ctx) ctx
                    |> Option.map
                         (List.map (fun (f : Record.Field.t) ->
                              Imp.Type_tree.Field.make ?offset:f.offset
                                ~name:f.name ~ty:f.ty ()))

                  let size (ty : Ty.t) : int option =
                    Context.record_size (Context.resolve ty ctx) ctx
                end) in
                T.of_declaration ~root:v.var ty
                |> Imp.Type_tree.to_arrays ~hierarchy:h
                |> List.filter (fun (x, _) -> not (Variable.equal x v.var))
                |> function
                | [] -> Context.add_array v.var (Memory.from_type h ty) ctx
                | leaves ->
                    List.fold_left
                      (fun ctx (x, m) -> Context.add_array x m ctx)
                      ctx leaves
              in
              if is_mut && List.mem C_lang.c_attr_shared v.attrs then
                declare SharedMemory ctx
              else if is_mut && List.mem C_lang.c_attr_device v.attrs then
                declare GlobalMemory ctx
              else if
                is_mut
                && List.mem C_lang.c_attr_constant v.attrs
                && not (Context.is_int ty ctx)
              then declare ConstantMemory ctx
              else if Context.is_int ty ctx then
                (* Fold a global's initializer into a constant only
                   when it is immutable: a C-level [const], or a
                   global cu-to-json proved is never written (the
                   [c_attr_immutable] tag). A mutable global, e.g. one
                   accumulated at runtime before a launch, would
                   otherwise be pinned to its stale initializer;
                   without proof of immutability, keep it symbolic. *)
                let immutable =
                  (not is_mut)
                  || List.mem C_lang.c_attr_immutable v.attrs
                in
                let g =
                  if immutable then
                    (match v.init with
                     | Some (IExpr n) -> try_to_nexp n
                     | _ -> None)
                  else None
                in
                match g with
                | Some g -> Context.add_assign v.var g ctx
                | None -> Context.add_global v.var ty ctx
              else ctx
          in
          parse_p b l
      | Kernel k :: l ->
          let ctx, k = parse_kernel ~report ctx k in
          let ks = parse_p ctx l in
          k :: ks
      | Prototype _ :: l -> parse_p ctx l
      | Typedef d :: l -> parse_p (Context.add_typedef d ctx) l
      | Record r :: l -> parse_p (Context.add_record r ctx) l
      | UsingNamespace n :: l -> parse_p (Context.add_using n ctx) l
      | Enum e :: l -> parse_p (Context.add_enum e ctx) l
      | LaunchParam _ :: l ->
          (* Launch metadata flows through the pipeline as data only;
             d_to_imp produces Imp.Kernel.t which has no slot for
             launches. Drop here until a downstream stage consumes. *)
          parse_p ctx l
      | [] -> []
    in
    let sigs = D_lang.SignatureDB.from_program ~policy p in
    let ctx =
      List.fold_left
        (fun ctx -> function
          | D_lang.Def.Record r -> Context.add_record r ctx
          | _ -> ctx)
        (Context.from_signature_db sigs)
        p
    in
    parse_p ctx p
end

module Default = Make (Logger.Colors)
module Silent = Make (Logger.Silent)
