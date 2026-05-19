open Stage0
open Protocols
open Location_parser
open Ast

module TemplateArgument = Template_argument
module ConstBinding = Const_binding

(* One CUDA launch site, populated from c-to-json's [LaunchParam] node
   in TranslationUnitDecl.inner[]. The expression slots (grid / block
   / shared_mem / stream / args) are real AST subtrees that round-trip
   through [parse_expr]: c-to-json's resolution policy const-folds
   where possible and emits the original AST otherwise, but every
   shape parses with the existing [c_expr] arms. *)
type t = {
  loc : Location.t;
  kernel : Decl_expr.t;
  host_function : Decl_expr.t option;
  template_args : TemplateArgument.t list;
  launch_api : string option;
  grid : c_expr;
  block : c_expr;
  shared_mem : c_expr;
  stream : c_expr;
  args : c_expr list;
  (* Sound conjunction of host-side guards (from enclosing
     [if]/[while]/[for]) that hold whenever this launch executes,
     as emitted by c-to-json's [path_condition] slot. The dropper
     on the c-to-json side excludes anything potentially mutated
     between the guard's branch entry and the launch — calls,
     members, escaped locals, side effects — so what survives is
     always pure arithmetic / boolean over [Ident]s and literals
     that [Launch_arg.lift_pure] handles directly. Absent when no
     conjunct survives the soundness check. *)
  path_condition : c_expr option;
  (* Host-local [const]-qualified variables reachable from the
     launch's emitted expressions, paired with their initialisers.
     Surfaces equalities like [inum == numk * 1024] so a downstream
     consumer can conjoin them to the wrapper invariant without
     rewriting use sites — [inum] stays a named identifier in the
     grid expression, the path condition, and any kernel arg.
     Empty when c-to-json's BFS admits no bindings. *)
  const_bindings : ConstBinding.t list;
  notes : string option;
}

let parse (j : Yojson.Basic.t) : t Rjson.j_result =
  let open Rjson in
  (let* o = cast_object j in
   let* loc = with_field "range" parse_location o in
   let* kernel = with_field "kernel" Parsers.parse_bare_decl_ref o in
   let* host_function =
     with_opt_field "host_function" Parsers.parse_bare_decl_ref o
   in
   let* template_args =
     with_field_or "template_args"
       (cast_map Parsers.parse_c_template_argument) [] o
   in
   let* launch_api = with_opt_field "launch_api" cast_string o in
   let* grid = with_field "grid" Parsers.parse_expr o in
   let* block = with_field "block" Parsers.parse_expr o in
   let* shared_mem = with_field "shared_mem" Parsers.parse_expr o in
   let* stream = with_field "stream" Parsers.parse_expr o in
   let* args = with_field_or "args" (cast_map Parsers.parse_expr) [] o in
   let* path_condition =
     with_opt_field "path_condition" Parsers.parse_expr o
   in
   let* const_bindings =
     with_field_or "const_bindings" ConstBinding.parse_list [] o
   in
   let* notes = with_opt_field "notes" cast_string o in
   Ok
     {
       loc;
       kernel;
       host_function;
       template_args;
       launch_api;
       grid;
       block;
       shared_mem;
       stream;
       args;
       path_condition;
       const_bindings;
       notes;
     })
  |> Rjson.add_reason "LaunchParam" j

let location (lp : t) : Location.t = lp.loc

let to_s (lp : t) : Indent.t list =
  let targs =
    if lp.template_args <> [] then
      "<" ^ list_to_s TemplateArgument.to_string lp.template_args ^ ">"
    else ""
  in
  let host =
    match lp.host_function with
    | Some h -> " from " ^ Variable.name h.name
    | None -> ""
  in
  let pc =
    match lp.path_condition with
    | Some e -> " when " ^ Expr.to_string e
    | None -> ""
  in
  let cb =
    if lp.const_bindings = [] then ""
    else " where " ^ list_to_s ConstBinding.to_string lp.const_bindings
  in
  let cfg =
    [
      "gridDim=" ^ Expr.to_string lp.grid;
      "blockDim=" ^ Expr.to_string lp.block;
    ]
    @ (let s = Expr.to_string lp.shared_mem in
       if s = "0" then [] else [ "sharedMem=" ^ s ])
    @ (let s = Expr.to_string lp.stream in
       if s = "0" then [] else [ "stream=" ^ s ])
  in
  [
    Indent.Line
      (Variable.name lp.kernel.name
      ^ targs
      ^ "<<<" ^ String.concat ", " cfg ^ ">>>"
      ^ "(" ^ list_to_s Expr.to_string lp.args ^ ")"
      ^ host ^ pc ^ cb);
  ]

let free_vars (lp : t) : Decl_expr.Set.t =
  let exprs =
    [ lp.grid; lp.block; lp.shared_mem; lp.stream ]
    @ lp.args
    @ Option.to_list lp.path_condition
    @ List.map (fun (b : ConstBinding.t) -> b.init) lp.const_bindings
  in
  List.fold_left
    (fun acc e -> Decl_expr.Set.union acc (Expr.shallow_free_vars e))
    Decl_expr.Set.empty exprs

(* c-to-json emits a [LaunchParamWarning] node for every [<<<>>>] /
   [cudaLaunchKernel] site whose callee can't be resolved to a
   [FunctionDecl] — function-pointer kernels, dependent
   unresolved-lookups, helper-wrapped launches. Faial doesn't carry
   these in the AST: we have no consumer for them, and the textual
   warning at parse time is sufficient observability. If a downstream
   stage ever needs to enumerate or count unresolvable launches, this
   should grow back into a [Def.t] variant. *)
let log_warning (j : Yojson.Basic.t) : unit Rjson.j_result =
  let open Rjson in
  let* o = cast_object j in
  let* loc =
    match List.assoc_opt "range" o with
    | Some r -> parse_location r
    | None -> with_field "loc" (parse_position ?filename:None) o
  in
  let* reason = with_field "reason" cast_string o in
  let* host_function =
    with_opt_field "host_function" Parsers.parse_bare_decl_ref o
  in
  let host =
    match host_function with
    | Some h -> " in " ^ Variable.name h.name
    | None -> ""
  in
  prerr_endline
    ("WARNING: unresolved launch at " ^ Location.to_string loc ^ host
    ^ ": " ^ reason);
  Ok ()
