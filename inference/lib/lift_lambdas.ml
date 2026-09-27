open Stage0
open Protocols
open D_lang

(* Eliminates [LambdaDecl] from the D_lang AST by hoisting each lambda
   body to a fresh top-level [Auxiliary] [D_lang.Kernel.t] and rewriting
   [v(args)] call sites to [__lambda_<id>(captures @ args)]. After this
   pass runs, [D_to_imp] never sees a [LambdaDecl]; the synthetic
   kernels become [Imp.Kernel.t] with [Visibility.Device] and are
   inlined by [Imp.Inline_calls] exactly like any hand-written
   [__device__] helper.

   Lambda-of-lambda is handled by *capture splicing*: when an enclosing
   lambda captures another lambda binding, the captures of that binding
   are spliced into the enclosing lambda's effective capture list, and
   calls inside the enclosing body have already been rewritten to
   direct calls to the synthetic. So lambda-typed parameters never
   appear in the generated kernels.

   State is threaded through the [State] monad: a fresh-name counter
   and an accumulator of synthetic kernels flow through [Context.t]. *)

module Param = C_lang.Param

let mk_param ~(name : Variable.t) ~(ty : Ty.t) : Param.t =
  Param.make
    ~ty_var:(Ty_variable.make ~ty ~name)
    ~is_used:true ~is_shared:false

module Context = struct
  type binding = {
    fname : Variable.t;
    (* Effective captures (after lambda-of-lambda splicing): names that
       appear in the synthetic kernel's parameter list, paired with the
       init expression spliced as an argument at every call site. *)
    captures : (Variable.t * Expr.t) list;
  }

  type t = {
    next : int;
    bindings : binding Variable.Map.t;
    (* Synthetic kernels in reverse order of synthesis. *)
    synthetics : Kernel.t list;
    (* Argument types observed at each synthetic's call sites, keyed by
       the synthetic's name. A generic lambda writes [auto], so the
       call site is the only place its parameter type exists. Two call
       sites that disagree map to [None]: one lifted body cannot serve
       both instantiations, and picking either would substitute a body
       the source never wrote. *)
    arg_types : Ty.t list option Variable.Map.t;
  }

  let make (next : int) : t =
    {
      next;
      bindings = Variable.Map.empty;
      synthetics = [];
      arg_types = Variable.Map.empty;
    }

  let bindings : (t, binding Variable.Map.t) State.t =
    State.get_return (fun s -> s.bindings)

  let lookup (v : Variable.t) : (t, binding option) State.t =
    State.get_return (fun s -> Variable.Map.find_opt v s.bindings)

  let fresh_name (label : string) : (t, Variable.t) State.t =
    State.update_return (fun s ->
        let name = Printf.sprintf "__lambda_%s_%d" label s.next in
        ({ s with next = s.next + 1 }, Variable.from_name name))

  let add_binding (var : Variable.t) (b : binding) : (t, unit) State.t =
    State.update (fun s ->
        { s with bindings = Variable.Map.add var b s.bindings })

  let add_synthetic (k : Kernel.t) : (t, unit) State.t =
    State.update (fun s -> { s with synthetics = k :: s.synthetics })

  let add_arg_types (fname : Variable.t) (tys : Ty.t list) : (t, unit) State.t =
    let same (a : Ty.t list) (b : Ty.t list) : bool =
      List.length a = List.length b
      && List.for_all2 (fun x y -> Ty.to_string x = Ty.to_string y) a b
    in
    State.update (fun s ->
        let arg_types =
          Variable.Map.update fname
            (function
              | None -> Some (Some tys)
              | Some (Some prev) when same prev tys -> Some (Some prev)
              | Some _ -> Some None)
            s.arg_types
        in
        { s with arg_types })

  (* Resolve lambda-of-lambda captures: replace a capture whose name is
     itself a lambda binding with the lambda's effective captures. Then
     dedupe (a single name might be reachable through multiple paths). *)
  let splice_captures (caps : (Variable.t * Expr.t) list) :
      (t, (Variable.t * Expr.t) list) State.t =
    let open State.Syntax in
    let* env = bindings in
    let expanded =
      List.concat_map
        (fun (name, init_expr) ->
          match Variable.Map.find_opt name env with
          | Some b -> b.captures
          | None -> [ (name, init_expr) ])
        caps
    in
    let rec dedupe seen = function
      | [] -> []
      | (n, e) :: t ->
          if Variable.Set.mem n seen then dedupe seen t
          else (n, e) :: dedupe (Variable.Set.add n seen) t
    in
    return (dedupe Variable.Set.empty expanded)
end

type 'a state = (Context.t, 'a) State.t

open State.Syntax

(* Walk a D_lang.Expr.t, replacing any lambda call sites. The bindings map
   is stable for the duration of an expression walk (only [rewrite_stmt]
   ever adds bindings), so we snapshot it once and let [Expr.st_map] handle
   the recursion. *)
let rewrite_expr (e : Expr.t) : Expr.t state =
  let* env = Context.bindings in
  let synth_call (b : Context.binding) (args : Expr.t list) (ty : Ty.t) :
      Expr.t =
    let cap_args = List.map snd b.captures in
    let func =
      Expr.Ident
        (Decl_expr.from_name ~ty:J_type.unknown ~kind:Decl_expr.Kind.Function
           b.fname)
    in
    Expr.CallExpr { func; args = cap_args @ args; ty }
  in
  let call (v : Variable.t) (args : Expr.t list) (ty : Ty.t) : Expr.t state =
    let b = Variable.Map.find v env in
    let* () = Context.add_arg_types b.fname (List.map Expr.to_type args) in
    return (synth_call b args ty)
  in
  Expr.st_map
    (function
      | CallExpr { func = Ident { name = v; _ }; args; ty }
        when Variable.Map.mem v env ->
          call v args ty
      | CXXOperatorCallExpr { args = Ident { name = v; _ } :: args; ty; _ }
        when Variable.Map.mem v env ->
          call v args ty
      | e -> return e)
    e

(* Walk a D_lang.Stmt.t, lifting any [LambdaDecl] into [Context.synthetics]
   and rewriting its call sites in subsequent siblings. [Stmt.st_map]
   handles child-stmt recursion post-order; [Stmt.st_map_expr] takes
   care of every contained [Expr.t] for the non-lambda cases. Only
   [LambdaDecl] needs custom handling. *)
let rewrite_stmt (st : Stmt.t) : Stmt.t state =
  Stmt.st_map
    (fun st ->
      match st with
      | LambdaDecl { var; captures; params; body; ret_ty } ->
          (* Captures' init exprs are rewritten in the outer env. The
             body above has already been rewritten in the env that was
             current when we entered this LambdaDecl, so calls to
             in-scope sibling lambdas were inlined. *)
          let* captures =
            State.list_map
              (fun (n, e) ->
                let* e = rewrite_expr e in
                return (n, e))
              captures
          in
          let* effective = Context.splice_captures captures in
          let* fname = Context.fresh_name (Variable.name var) in
          let cap_params =
            List.map
              (fun (n, e) -> mk_param ~name:n ~ty:(Expr.to_type e))
              effective
          in
          let synth : Kernel.t =
            {
              id =
                Imp.Function_id.make ~name:(Variable.name fname)
                  ~ty:(Ty.to_string ret_ty) ();
              decl_id = None;
              code = body;
              type_params = [];
              template_args = [];
              params = cap_params @ params;
              attribute = KernelAttr.Auxiliary;
              returns_location = false;
            }
          in
          let* () =
            Context.add_binding var { fname; captures = effective }
          in
          let* () = Context.add_synthetic synth in
          return Stmt.Skip
      | st -> Stmt.st_map_expr rewrite_expr st)
    st

(* A generic lambda writes [auto] for its parameters, so the lifted
   function has no type by which to bind an argument. Take the type from
   the call site, the only place it exists. The lambda's own parameters
   are the tail of the list, since the captures were prepended. *)
let deduce_auto (tys : Ty.t list) (k : Kernel.t) : Kernel.t =
  let captures = List.length k.params - List.length tys in
  if captures < 0 then k
  else
    let params =
      List.mapi
        (fun i (p : Param.t) ->
          let ty_var = Param.ty_var p in
          if i < captures || not (Ty.is_auto (Ty_variable.ty ty_var)) then p
          else
            match List.nth_opt tys (i - captures) with
            | Some ty ->
                Param.make
                  ~ty_var:(Ty_variable.make ~ty ~name:(Ty_variable.name ty_var))
                  ~is_used:true ~is_shared:false
            | None -> p)
        k.params
    in
    { k with params }

let lift_kernel (next : int) (k : Kernel.t) : int * Kernel.t list * Kernel.t =
  let s, code = State.run (rewrite_stmt k.code) (Context.make next) in
  let refine (synth : Kernel.t) : Kernel.t =
    match
      Variable.Map.find_opt (Variable.from_name (Kernel.name synth)) s.arg_types
    with
    | Some (Some tys) -> deduce_auto tys synth
    | Some None | None -> synth
  in
  (s.next, s.synthetics |> List.rev |> List.map refine, { k with code })

(* Lift every [Def.Kernel] in [p], producing a new program where each
   kernel's synthetic auxiliaries appear *before* the kernel itself.
   Earlier ordering matters because [D_to_imp.parse_p] threads its
   [Context.t] through the program in order, so callees must be parsed
   before callers (their shared-array decls flow into the caller's
   context). *)
let lift_program (p : Program.t) : Program.t =
  let lift_def (next : int) (def : Def.t) : int * Def.t list =
    match def with
    | Kernel k ->
        let next, synths, k' = lift_kernel next k in
        (next, List.map (fun s -> Def.Kernel s) synths @ [ Def.Kernel k' ])
    | other -> (next, [ other ])
  in
  let _, rev =
    List.fold_left
      (fun (next, acc) def ->
        let next, defs = lift_def next def in
        (next, List.rev_append defs acc))
      (0, []) p
  in
  List.rev rev
