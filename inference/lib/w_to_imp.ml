open Stage0 (* State *)
open Protocols

let ( let* ) = Option.bind

module Arrays = struct
  let tr_address_space : W_lang.AddressSpace.t -> Mem_hierarchy.t option =
    function
    | WorkGroup -> Some Mem_hierarchy.SharedMemory
    | Storage ReadWrite | Storage WriteOnly | Storage ReadOnly ->
        Some Mem_hierarchy.GlobalMemory
    | Uniform | Handle | PushConstant | Function | Private -> None

  let tr_type (ty : W_lang.Type.t) : (int list * string list) option =
    match ty.inner with
    | Array { base; size } ->
        Some
          ( size |> W_lang.ArraySize.to_int |> Option.to_list,
            [ W_lang.Type.to_string base ] )
    | _ -> None

  let tr_memory ?(hierarchy = Mem_hierarchy.GlobalMemory) (ty : W_lang.Type.t) :
      Memory.t option =
    let ( let* ) = Option.bind in
    let* size, data_type = tr_type ty in
    Some { Memory.hierarchy; size; data_type }

  let tr_decl (d : W_lang.Declaration.t) : (Variable.t * Memory.t) option =
    let ( let* ) = Option.bind in
    match d.kind with
    | GlobalVariable g ->
        let* hierarchy : Mem_hierarchy.t = tr_address_space g.space in
        let* m = tr_memory ~hierarchy d.ty in
        Some (Variable.from_name d.name, m)
    | _ -> None

  let tr (globals : W_lang.Declaration.t list) : Memory.t Variable.Map.t =
    globals |> List.filter_map tr_decl |> Variable.Map.of_list
end

module Literals = struct
  open Imp

  type t = Bool of bool | Int of int

  let to_int : t -> int option = function Int i -> Some i | Bool _ -> None

  let parse : W_lang.Literal.t -> t option = function
    | U32 v -> Some (Int v)
    | I32 v -> Some (Int v)
    | U64 v -> Some (Int v)
    | I64 v -> Some (Int v)
    | AbstractInt v -> Some (Int v)
    | Bool v -> Some (Bool v)
    | F64 _ | F32 _ | AbstractFloat _ -> None

  let n_tr (l : W_lang.Literal.t) : Exp.nexp option =
    W_lang.Literal.to_int l |> Option.map (fun x -> Exp.Num x)

  let b_tr (l : W_lang.Literal.t) : Exp.bexp option =
    W_lang.Literal.to_bool l |> Option.map (fun x -> Exp.Bool x)

  let tr : t -> Infer_exp.t = function
    | Int n -> Infer_exp.num n
    | Bool b -> Infer_exp.bool b
end

module Types = struct
  open W_lang

  let tr (ty : Type.t) : C_type.t =
    match ty.inner with
    | Scalar s when Scalar.is_int s ->
        let unsigned = if Scalar.is_unsigned s then "u" else "" in
        let width = string_of_int (s.width * 8) in
        C_type.make (unsigned ^ "int" ^ width ^ "_t")
    | _ -> ty |> Type.to_string |> C_type.make

  let to_parameter (ty : Type.t) : Imp.Kernel.Parameter.Type.t =
    let open Imp.Kernel.Parameter.Type in
    if W_lang.Type.is_array ty then
      ty |> Arrays.tr_memory
      |> Option.map (fun m -> Array m)
      |> Option.value ~default:(Unsupported (tr ty))
    else if W_lang.Type.is_int ty then Scalar (tr ty)
    else
      (* unsupported; retain the translated [C_type.t] from [tr]. *)
      Unsupported (tr ty)
end

module Variables = struct
  let tr : W_lang.Expression.t -> (C_type.t * Variable.t) option =
    let open W_lang.Expression in
    function Ident { var; ty; _ } -> Some (Types.tr ty, var) | _ -> None

  let inline_field (index : int) (a : W_lang.Ident.t) : W_lang.Ident.t option =
    let open W_lang in
    let open W_lang.Ident in
    let ( let* ) = Option.bind in
    (* Project the type *)
    let* ty = Type.nth index a.ty in
    (* Try to get a default variable name *)
    let var =
      if IdentKind.is_local_invocation_id a.kind then
        List.nth_opt Variable.tid_list index
      else if IdentKind.is_workgroup_id a.kind then
        List.nth_opt Variable.bid_list index
      else if IdentKind.is_num_workgroups a.kind then
        List.nth_opt Variable.gdim_list index
      else None
    in
    let* a =
      match var with
      (* we found a system variable *)
      | Some var ->
          Some
            {
              a with
              var =
                a.var
                (* When the variable is pretty-printed, use original variable's name *)
                |> Variable.set_label a.var.name
                (* When the variable is used internally, use our internal name *)
                |> Variable.set_name var.name;
            }
      | None ->
          (* no system variable, but we can flatten the name *)
          a.ty |> Type.lookup_field index
          |> Option.map (fun f -> add_suffix ("." ^ f) a)
      (*|> Option.value ~default:(add_suffix (string_of_int index ^ ".") a)*)
    in
    Some { a with ty }
end

module NDAccess = struct
  open W_lang

  type t = { array : Ident.t; index : Expression.t list }

  (* We are only able to flatten an NDAccess when the identifier is
     a function-arg/global _and_ the type is a series of structs/vecs
     _and_ every lookup is a literal number.
    *)
  let flatten (e : t) : Ident.t option =
    let ( let* ) = Option.bind in
    if not (Ident.is_global e.array || Ident.is_function_argument e.array) then
      None
    else
      let rec loop (ident : Ident.t) (index : Expression.t list) :
          Ident.t option =
        if index = [] then Some ident
        else
          let* field, index =
            match index with
            | Literal l :: index ->
                l |> Literal.to_int |> Option.map (fun i -> (i, index))
            | _ -> None
          in
          let* ident = Variables.inline_field field ident in
          loop ident index
      in
      loop e.array e.index

  let from_expression (location : Stage0.Location.t) (e : Expression.t) :
      t option =
    let ( let* ) = Option.bind in
    let* e, l =
      match e with
      | Access { base = e; index; _ } -> Some (e, [ index ])
      | AccessIndex { base = e; index; _ } -> Some (e, [ Expression.int index ])
      | _ -> None
    in
    let rec from_exp (e : Expression.t) (l : Expression.t list) : t option =
      match e with
      | Access { base; index; _ } -> from_exp base (index :: l)
      | AccessIndex { base; index; _ } ->
          from_exp base (Expression.int index :: l)
      | Ident i -> Some { array = Ident.set_location location i; index = l }
      | _ -> None
    in
    from_exp e l
end

module Expressions = struct
  open Stage0 (* because of State *)
  open W_lang
  open Imp

  type t =
    | Unsupported
    | Literal of Literals.t
    | ZeroValue of Type.t
    | Compose of { ty : Type.t; components : t list }
    | Splat of { size : VectorSize.t; value : t }
    | Ident of Ident.t
    | Unary of { op : UnaryOperator.t; expr : t }
    | Binary of { op : BinaryOperator.t; left : t; right : t }
    | Select of { condition : t; accept : t; reject : t }
    | Relational of { fun_ : RelationalFunction.t; argument : t }
    | Math of { fun_ : MathFunction.t; args : t list }
    | As of { expr : t; kind : ScalarKind.t; convert : int option }
    | AtomicResult of { ty : Type.t; comparison : bool }
    | WorkGroupUniformLoadResult of Type.t
    | SubgroupBallotResult
    | SubgroupOperationResult of Type.t

  let map_ident (f : Ident.t -> Ident.t) : t -> t = function
    | Ident i -> Ident (f i)
    | e -> e

  let int (i : int) : t = Literal (Literals.Int i)
  let mult (left : t) (right : t) : t = Binary { op = Multiply; left; right }
  let plus (left : t) (right : t) : t = Binary { op = Add; left; right }

  let thread_idx (index : int) : t option =
    let open W_lang.Ident in
    let ( let* ) = Option.bind in
    let* var = List.nth_opt Variable.tid_list index in
    Some (Ident { var; ty = Type.u32; kind = GlobalVariable })

  let block_dim (index : int) : t option =
    let open W_lang.Ident in
    let ( let* ) = Option.bind in
    let* var = List.nth_opt Variable.bdim_list index in
    Some (Ident { var; ty = Type.u32; kind = GlobalVariable })

  let block_idx (index : int) : t option =
    let open W_lang.Ident in
    let ( let* ) = Option.bind in
    let* var = List.nth_opt Variable.bid_list index in
    Some (Ident { var; ty = Type.u32; kind = GlobalVariable })

  let global_idx (i : int) : t option =
    let ( let* ) = Option.bind in
    let* tid = thread_idx i in
    let* bid = block_idx i in
    let* bdim = block_dim i in
    Some (plus tid (mult bid bdim))

  let to_int : t -> int option = function
    | Literal l -> Literals.to_int l
    | _ -> None

  open State.Syntax

  module Read = struct
    type expr = t

    type t = {
      target : (Type.t * Variable.t) option;
      array : Variable.t;
      index : expr list;
    }

    let read ~array (index : expr list) : t = { target = None; array; index }

    let read_to ~target_var ~target_ty ~array (index : expr list) : t =
      { target = Some (target_ty, target_var); array; index }
  end

  type context = Read.t list
  type 'a state = (context, 'a) State.t

  let empty : context = []
  let push_read (r : Read.t) : unit state = State.update (fun s -> r :: s)

  (* Counter is used to generate new variables *)
  let counter = ref 1

  let alloc (f : Variable.t -> Read.t) : Variable.t state =
    let count = !counter in
    counter := count + 1;
    let var = "@AccessState" ^ string_of_int count |> Variable.from_name in
    let* _ = push_read (f var) in
    return var

  (* Add a read to the current state, returns a variable that represents
      the value being read. *)
  let add ?(target = None) ~(array : Ident.t) ~(index : t list) () : t state =
    match target with
    | Some target ->
        let* () = push_read (Read.read ~array:array.var index) in
        return (Ident target)
    | None ->
        (* Get the type of the value being read *)
        let target_ty =
          Type.deref_list (List.map to_int index) array.ty
          (* Defaults to a non-int and non-bool type so that it short circuits *)
          |> Option.value ~default:Type.f64
        in
        if Type.is_int target_ty || Type.is_bool target_ty then
          let* var =
            alloc (fun target_var ->
                Read.read_to ~target_var ~target_ty ~array:array.var index)
          in
          return (Ident { var; ty = target_ty; kind = LocalVariable })
        else
          (* Don't generate a variable declaration when reading a non-int *)
          let* () = push_read (Read.read ~array:array.var index) in
          return Unsupported

  let rec rewrite (e : W_lang.Expression.t) : t state =
    let unsupported ?(expr = e) () : t state =
      let* _ = State.list_map rewrite (W_lang.Expression.children expr) in
      return Unsupported
    in
    match e with
    | Literal l ->
        return
          (match Literals.parse l with
          | Some l -> Literal l
          | None -> Unsupported)
    | ZeroValue ty -> return (ZeroValue ty)
    | Compose { ty; components } ->
        let* components = State.list_map rewrite components in
        return (Compose { ty; components })
    | Load (Access { location; _ } as e)
    | Load (AccessIndex { location; _ } as e) -> (
        (* Try to parse the access from the expression using NDAccess *)
        match NDAccess.from_expression location e with
        | Some a ->
            (* rewrite the inferred list of indices *)
            let* index = State.list_map rewrite a.index in
            (* try to extract the target variable if there is on *)
            let target = NDAccess.flatten a in
            (* emit a read *)
            add ~target ~array:a.array ~index ()
        | None ->
            (* we override the target expression we're passing to unsupported
            because we want to get the children of the access, not the children
            of the load (which is what the default is). *)
            unsupported ~expr:e ())
    (* We need a special case for handling gid, because we do not
        support GID natively. *)
    | AccessIndex { base = Ident i; index; _ }
      when IdentKind.is_global_invocation_id i.kind ->
        return (global_idx index |> Option.value ~default:Unsupported)
    | AccessIndex { base; index; _ } ->
        let* base = rewrite base in
        return
          (map_ident
             (fun (x : W_lang.Ident.t) ->
               Variables.inline_field index x |> Option.value ~default:x)
             base)
    | Splat { size; value } ->
        let* value = rewrite value in
        return (Splat { size; value })
    | Ident i -> return (Ident i)
    | Load e -> rewrite e
    | Unary { expr; op } ->
        let* expr = rewrite expr in
        return (Unary { expr; op })
    | Binary { op; left; right } ->
        let* left = rewrite left in
        let* right = rewrite right in
        return (Binary { op; left; right })
    | Select { condition; accept; reject } ->
        let* condition = rewrite condition in
        let* accept = rewrite accept in
        let* reject = rewrite reject in
        return (Select { condition; accept; reject })
    | Relational { argument; fun_ } ->
        let* argument = rewrite argument in
        return (Relational { fun_; argument })
    | Math { fun_; args } ->
        let* args = State.list_map rewrite args in
        return (Math { fun_; args })
    | As { expr; kind; convert } ->
        let* expr = rewrite expr in
        return (As { expr; kind; convert })
    | ArrayLength (Ident i) -> return (Ident (W_lang.Ident.add_suffix ".len" i))
    | ArrayLength _ | Derivative _ | Access _ | Swizzle _ | ImageSample _
    | ImageLoad _ | ImageQuery _ ->
        unsupported ()

  let rec to_i_exp : t -> Imp.Infer_exp.t = function
    | Literal l -> Literals.tr l
    | Ident { var; _ } -> NExp (Var var)
    | Binary { op; left; right } -> (
        let left = to_i_exp left in
        let right = to_i_exp right in
        let open N_binary in
        let open N_rel in
        let open B_rel in
        match op with
        | Add -> NExp (Binary (Plus Signedness.Signed, left, right))
        | Subtract -> NExp (Binary (Minus Signedness.Signed, left, right))
        | Multiply -> NExp (Binary (Mult Signedness.Signed, left, right))
        | Divide -> NExp (Binary (Div Signedness.Signed, left, right))
        | Modulo -> NExp (Binary (Mod Signedness.Signed, left, right))
        | Equal -> BExp (NRel (Eq, left, right))
        | NotEqual -> BExp (NRel (Neq, left, right))
        | Less -> BExp (NRel (Lt Signedness.Signed, left, right))
        | LessEqual -> BExp (NRel (Le Signedness.Signed, left, right))
        | Greater -> BExp (NRel (Gt Signedness.Signed, left, right))
        | GreaterEqual -> BExp (NRel (Ge Signedness.Signed, left, right))
        | And -> NExp (Binary (BitAnd, left, right))
        | ExclusiveOr -> NExp (Binary (BitXOr, left, right))
        | InclusiveOr -> NExp (Binary (BitOr, left, right))
        | LogicalAnd -> BExp (BRel (BAnd, left, right))
        | LogicalOr -> BExp (BRel (BOr, left, right))
        | ShiftLeft -> NExp (Binary (LeftShift, left, right))
        | ShiftRight -> NExp (Binary (RightShift Signedness.Signed, left, right)))
    | Select { condition; accept; reject } ->
        NExp (NIf (to_i_exp condition, to_i_exp accept, to_i_exp reject))
    | As { expr; _ } -> to_i_exp expr
    | Unary { op; expr } -> (
        let expr = to_i_exp expr in
        match op with
        | Negate -> NExp (Unary (Negate, expr))
        | LogicalNot -> BExp (BNot expr)
        | BitwiseNot -> NExp (Unary (BitNot, expr)))
    | Splat _ -> Unknown "Splat"
    | ZeroValue _ -> Unknown "ZeroValue"
    | Compose _ -> Unknown "Compose"
    | Math { fun_ = Min; args = [ arg1; arg2 ] } ->
        NExp (Imp.Infer_exp.min (to_i_exp arg1) (to_i_exp arg2))
    | Math { fun_ = Max; args = [ arg1; arg2 ] } ->
        NExp (Imp.Infer_exp.max (to_i_exp arg1) (to_i_exp arg2))
    | _ -> Unknown "?"

  let tr (e : W_lang.Expression.t) : Imp.Infer_exp.t state =
    let* e = rewrite e in
    return (to_i_exp e)

  let run (e : Infer_stmt.t state) : Infer_stmt.t =
    (* given an empty context, output the final statement and context *)
    let reads, post = State.run e empty in
    (* convert each read to a statement, and output unknowns *)
    let reads =
      List.map
        (fun (r : Read.t) ->
          let array = r.array in
          let target = Option.map (fun (ty, x) -> (Types.tr ty, x)) r.target in
          let index = List.map to_i_exp r.index in
          Infer_stmt.Read { index; array; target; guard = None })
        reads
    in
    (* generate all of the statements *)
    Infer_stmt.seq (Infer_stmt.from_list reads) post

  let reads (l : W_lang.Expression.t list) : Infer_stmt.t =
    run
      (let* _ = Stage0.State.list_map rewrite l in
       return Infer_stmt.Skip)

  let pure (e : W_lang.Expression.t) : Imp.Infer_exp.t option =
    let ctx, e =
      Stage0.State.run
        (let* e = rewrite e in
         return (to_i_exp e))
        empty
    in
    if ctx = [] then Some e else None

  let no_unknowns (f : Imp.Infer_exp.t -> 'a Imp.Infer_exp.state)
      (e : W_lang.Expression.t) : 'a option =
    match pure e with Some e -> Imp.Infer_exp.no_unknowns (f e) | None -> None

  let try_nexp (e : W_lang.Expression.t) : Exp.nexp option =
    no_unknowns Imp.Infer_exp.to_nexp e

  let try_bexp (e : W_lang.Expression.t) : Exp.nexp option =
    no_unknowns Imp.Infer_exp.to_nexp e
end

module Signature = struct
  open W_lang

  type t = (string * Type.t) list

  let from_function (f : Function.t) : t =
    f.arguments
    |> List.map (fun a ->
        let open FunctionArgument in
        (a.name, a.ty))
end

module Typing = struct
  open W_lang
  open Stage0.Common

  type t = Signature.t StringMap.t

  let add_function (f : Function.t) (self : t) : t =
    let f_ty = Signature.from_function f in
    StringMap.add f.name f_ty self

  let add (d : ProgramEntry.t) (self : t) : t =
    match d with
    | EntryPoint e -> add_function e.function_ self
    | Function f -> add_function f self
    | Declaration _ -> self

  let empty : t = StringMap.empty
  let from_program : Program.t -> t = List.fold_left (fun s d -> add d s) empty
  let lookup (name : string) (ctx : t) : Signature.t = StringMap.find name ctx
end

module Globals = struct
  type t = { var : Variable.t; ty : C_type.t; init : Exp.nexp option }

  let tr (d : W_lang.Declaration.t) : t list =
    match d.ty.inner with
    | Scalar s when W_lang.Scalar.is_int s ->
        let init =
          match d.init with Some e -> Expressions.try_nexp e | None -> None
        in
        [ { var = Variable.from_name d.name; ty = Types.tr d.ty; init } ]
    | Array _ ->
        [
          {
            var = Variable.from_name (d.name ^ ".len");
            ty = C_type.int;
            init = None;
          };
        ]
    | Struct { members = l; _ } ->
        List.filter_map
          (fun (m : W_lang.Type.struct_member) : t option ->
            let open W_lang.Type in
            if W_lang.Type.is_int m.ty then
              Some
                {
                  var = Variable.from_name (d.name ^ "." ^ m.name);
                  ty = Types.tr m.ty;
                  init = None;
                }
            else None)
          l
    | _ -> []
end

module Const = struct
  type t = { var : Variable.t; ty : C_type.t; init : Exp.nexp }

  let to_stmt (c : t) : Imp.Stmt.t option =
    if C_type.is_int c.ty then
      Some
        (Imp.Stmt.Decl
           { var = c.var; ty = c.ty; init = Some c.init })
    else None
end

module Context = struct
  open W_lang

  type t = {
    arrays : Memory.t Variable.Map.t;
    params : Params.t;
    typing : Typing.t;
    assigns : Const.t list;
  }

  let empty : t =
    {
      arrays = Variable.Map.empty;
      params = Params.empty;
      typing = Typing.empty;
      assigns = [];
    }

  let add_to_arrays (p : ProgramEntry.t) (arrays : Memory.t Variable.Map.t) :
      Memory.t Variable.Map.t =
    match p with
    | Declaration d -> (
        match Arrays.tr_decl d with
        | Some (array, m) -> Variable.Map.add array m arrays
        | None -> arrays)
    | _ -> arrays

  let add_global (g : Globals.t) (ctx : t) : t =
    match g.init with
    | Some init ->
        {
          ctx with
          assigns = Const.{ var = g.var; ty = g.ty; init } :: ctx.assigns;
        }
    | None -> { ctx with params = Params.add g.var g.ty ctx.params }

  let add_scalar (p : ProgramEntry.t) (ctx : t) : t =
    match p with
    | Declaration d ->
        d |> Globals.tr
        |> List.fold_left
             (fun (ctx : t) (g : Globals.t) : t -> add_global g ctx)
             ctx
    | _ -> ctx

  let add (p : ProgramEntry.t) (ctx : t) : t =
    {
      ctx with
      arrays = add_to_arrays p ctx.arrays;
      typing = Typing.add p ctx.typing;
    }
    |> add_scalar p

  let from_program (p : Program.t) : t =
    p
    |> List.fold_left
         (fun (ctx : t) (p : ProgramEntry.t) : t -> add p ctx)
         empty

  let assigns (ctx : t) : Imp.Stmt.t =
    ctx.assigns |> List.rev
    |> List.filter_map Const.to_stmt
    |> Imp.Stmt.from_list

  let lookup (name : string) (ctx : t) : Signature.t =
    Typing.lookup name ctx.typing
end

module Statements = struct
  open Imp
  open Stage0 (* make the state monad available *)

  let tr (ctx : Context.t) : W_lang.Statement.t list -> Infer_stmt.t =
    let rec tr : W_lang.Statement.t -> Infer_stmt.t =
     fun s ->
      let r =
        match s with
        | Store { pointer = Access { location; _ } as e; value }
        | Store { pointer = AccessIndex { location; _ } as e; value } ->
            let* a = NDAccess.from_expression location e in
            let open Stage0.State.Syntax in
            Some
              (Expressions.run
                 (let* index = State.list_map Expressions.tr a.index in
                  let* value = Expressions.tr value in
                  let payload =
                    match value with NExp (Num n) -> Some n | _ -> None
                  in
                  return
                    (Infer_stmt.Write
                       { array = a.array.var; index; payload; guard = None })))
        | Atomic { pointer = e; fun_; value; result; location } ->
            let* a = NDAccess.from_expression location e in
            Some
              (Expressions.run
                 (let open State.Syntax in
                  (* find any reads inside value, but discard the result *)
                  let* _ = Expressions.rewrite value in
                  (* translate the index *)
                  let* index = State.list_map Expressions.tr a.index in
                  let* value = Expressions.tr value in
                  (* Build the [Atomic.Operation] from WGSL's
                     [AtomicFunction]. CAS variants ([Exchange {
                     compare = Some _ }]) surface [expected] for
                     [Atomic_seed_read] and the winner-uniqueness
                     contract; arithmetic / bitwise / min / max
                     operations surface the [value] operand for
                     downstream contracts that need it. *)
                  let* operation : Infer_exp.t Atomic.Operation.t =
                    match fun_ with
                    | W_lang.AtomicFunction.Exchange { compare = Some e } ->
                        let* expected = Expressions.tr e in
                        return
                          (Atomic.Operation.CAS
                             { expected = Some expected; new_val = Some value })
                    | W_lang.AtomicFunction.Exchange { compare = None } ->
                        return (Atomic.Operation.Exch (Some value))
                    | W_lang.AtomicFunction.Add ->
                        return (Atomic.Operation.Add (Some value))
                    | W_lang.AtomicFunction.Subtract ->
                        return (Atomic.Operation.Sub (Some value))
                    | W_lang.AtomicFunction.And ->
                        return (Atomic.Operation.And (Some value))
                    | W_lang.AtomicFunction.ExclusiveOr ->
                        return (Atomic.Operation.Xor (Some value))
                    | W_lang.AtomicFunction.InclusiveOr ->
                        return (Atomic.Operation.Or (Some value))
                    | W_lang.AtomicFunction.Min ->
                        return (Atomic.Operation.Min (Some value))
                    | W_lang.AtomicFunction.Max ->
                        return (Atomic.Operation.Max (Some value))
                  in
                  let atomic : Infer_exp.t Atomic.t =
                    {
                      operation;
                      (* TODO: what scope does WGSL default to? *)
                      scope = Atomic.Scope.Device;
                      location = Some location;
                    }
                  in
                  let array = Variable.set_location location a.array.var in
                  let default = W_lang.Ident.atomic_result W_lang.Type.u32 in
                  let result = Option.value ~default result in
                  return
                    (Infer_stmt.Atomic
                       {
                         array;
                         index;
                         ty = Types.tr result.ty;
                         atomic;
                         target = result.var;
                         guard = None;
                       })))
        | Store { pointer = Ident { ty; _ } as i; value }
          when W_lang.Type.is_int ty ->
            let* ty, var = Variables.tr i in
            Some
              (Expressions.run
                 (let open State.Syntax in
                  let* data = Expressions.tr value in
                  return (Infer_stmt.Assign { var; data; ty })))
            (* TODO: add support for stores *)
        | Store _ -> None
        | Block [ s ] -> Some (tr s)
        | Block [] -> Some Skip
        | Block (s :: l) ->
            let s1 = tr s in
            let s2 = tr (Block l) in
            Some (Seq (s1, s2))
        | If { condition; accept; reject } ->
            let accept = tr (Block accept) in
            let reject = tr (Block reject) in
            Some
              (let open Expressions in
               let open State.Syntax in
               run
                 (let* c = Expressions.tr condition in
                  return (Infer_stmt.If (c, accept, reject))))
        (* When condition is specified in break_if *)
        | Loop { body; continuing = c; break_if = Some cond }
        (* When break_if is unset, yet there's an if+break at the beginning
           of body *)
        | Loop
            {
              body =
                If { condition = cond; accept = [ Break ]; reject = [] } :: body;
              continuing = c;
              break_if = None;
            } ->
            Some
              (let open State.Syntax in
               Expressions.run
                 (let* cond = Expressions.tr cond in
                  return
                    (Infer_stmt.For
                       {
                         init = Skip;
                         cond = BExp (Infer_exp.not_ cond);
                         inc = tr (Block c);
                         body = tr (Block body);
                       })))
        (* When break_if is unset, yet there's an if+else-break
           at the beginning of body *)
        | Loop
            {
              body =
                If { condition = cond; accept = []; reject = [ Break ] } :: body;
              continuing = c;
              break_if = None;
            } ->
            Some
              (let open State.Syntax in
               Expressions.run
                 (let* cond = Expressions.tr cond in
                  return
                    (Infer_stmt.For
                       {
                         init = Skip;
                         cond;
                         inc = tr (Block c);
                         body = tr (Block body);
                       })))
        (* Otherwise, when break_if is unset *)
        | Loop { body; continuing = c; break_if = None } ->
            Some
              (Infer_stmt.For
                 {
                   init = Skip;
                   cond = Infer_exp.true_;
                   body = tr (Block body);
                   inc = tr (Block c);
                 })
        | Barrier _ -> Some (Infer_stmt.Sync (Sync.syncthreads ()))
        | Call { result; function_ = kernel; arguments = args } ->
            let k = Context.lookup kernel ctx in
            Some
              (let open State.Syntax in
               Expressions.run
                 (let* args = State.list_map Expressions.tr args in
                  let args : Infer_stmt.Arg.t list =
                    List.map2
                      (fun (arg : Infer_exp.t) (_, ty) ->
                        let arg =
                          let open Imp in
                          if W_lang.Type.is_array ty then
                            match arg with
                            | Infer_exp.NExp (Var x) ->
                                Infer_stmt.Arg.Array
                                  (Infer_stmt.Array_use.make x)
                            | _ -> Unsupported (Types.tr ty)
                          else if W_lang.Type.is_int ty then
                            (* handle scalar *)
                            Scalar arg
                          else
                            (* unsupported; retain the translated
                               [C_type.t] from [Types.tr]. *)
                            Unsupported (Types.tr ty)
                        in
                        arg)
                      args k
                  in
                  let result =
                    Option.map
                      (fun r ->
                        let open W_lang.Ident in
                        (r.var, Types.tr r.ty))
                      result
                  in
                  return (Infer_stmt.Call { result; args; kernel; ty = kernel })))
        | Return e ->
            Some
              (let open State.Syntax in
               Expressions.run
                 (let* e = State.option_map Expressions.tr e in
                  return (Infer_stmt.Return e)))
        | Break -> Some Break
        | Continue -> Some Continue
        | _ -> None
      in
      match r with
      | Some r -> r
      | None ->
          let l = W_lang.Statement.expressions s in
          Expressions.reads l
    in
    fun l -> tr (Block l)
end

module LocalDeclarations = struct
  open Imp

  let tr_local (l : W_lang.LocalDeclaration.t) : Infer_stmt.t =
    let open State.Syntax in
    Expressions.run
      (let* init = State.option_map Expressions.tr l.init in
       return
         (if W_lang.Type.is_int l.ty then
            Infer_stmt.Decl { var = l.var; ty = Types.tr l.ty; init }
          else Skip))

  let tr : W_lang.LocalDeclaration.t list -> Infer_stmt.t =
    List.fold_left (fun s d -> Infer_stmt.seq s (tr_local d)) Infer_stmt.Skip
end

module FunctionArguments = struct
  let tr : W_lang.FunctionArgument.t -> Imp.Kernel.Parameter.t option = function
    | { name; ty; binding = None } ->
        Some (Variable.from_name name, Types.to_parameter ty)
    | _ -> None
end

module Functions = struct
  let tr (ctx : Context.t) (e : W_lang.Function.t) : Imp.Kernel.t =
    let open Imp in
    let body, return =
      let body = e.body |> Statements.tr ctx in
      Infer_stmt.seq (LocalDeclarations.tr e.locals) body
      |> Imp.Atomic_seed_read.rewrite
      |> Infer_stmt.infer
    in
    {
      name = e.name;
      ty = e.name;
      global_arrays = ctx.arrays;
      global_variables = ctx.params;
      parameters = List.filter_map FunctionArguments.tr e.arguments;
      code = Stmt.seq (Context.assigns ctx) body;
      visibility = Device;
      grid_dim = None;
      block_dim = None;
      return;
    }
end

module EntryPoints = struct
  let tr (ctx : Context.t) (e : W_lang.EntryPoint.t) : Imp.Kernel.t =
    {
      (Functions.tr ctx e.function_) with
      block_dim = Some e.workgroup_size;
      visibility = Global;
    }
end

let translate (p : W_lang.Program.t) : Imp.Kernel.t list =
  let ctx = Context.from_program p in
  p
  |> W_lang.Program.map_expression W_lang.Expression.simplify
  |> List.filter_map
       (let open W_lang.ProgramEntry in
        function
        | EntryPoint e -> Some (EntryPoints.tr ctx e)
        | Function f -> Some (Functions.tr ctx f)
        | Declaration _ -> None)
