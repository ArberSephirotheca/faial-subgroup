open Stage0
open Protocols
open Ast
open Parse_util

(* One [ConstBinding] entry inside [LaunchParam.const_bindings]. c-to-json
   surfaces every host-local [const]-qualified variable reachable from the
   launch's emitted expressions paired with its initialiser, so equalities
   like [inum == numk * 1024] reach faial without the emitter rewriting use
   sites: [inum] stays a named [DeclRefExpr] in the grid expression, the
   path condition, and any kernel arg, and the equality travels alongside.

   c-to-json filters by type-system const + no address-taken — the strongest
   immutability guarantee available without value reconstruction — so each
   binding is sound as a launch-time hypothesis. The init expression is
   already resolved (const-fold + trivial-init substitution + pure-helper
   inlining), so it round-trips through [parse_expr] like any other slot. *)

type t = {
  name : Variable.t;
  ty : Ty.t;
  init : c_expr;
}

let parse (j : Yojson.Basic.t) : t Rjson.j_result =
  let open Rjson in
  (let* o = cast_object j in
   let* () = expect_kind "ConstBinding" o in
   let* name_str = with_field "name" cast_string o in
   let* ty = get_field "type" o in
   let* inner = with_field "inner" (cast_map Parsers.parse_expr) o in
   let* init =
     match inner with
     | [ e ] -> Ok e
     | _ ->
         root_cause
           "ConstBinding: expected exactly one init Expr in inner[]" j
   in
   Ok
     {
       name = Variable.from_name name_str;
       ty = J_type.parse ty;
       init;
     })
  |> Rjson.add_reason "ConstBinding" j

let to_string (b : t) : string =
  Variable.name b.name ^ " = " ^ Expr.to_string b.init

(* Parse the [const_bindings] wrapper:
     {kind: "ConstBindings", inner: [<ConstBinding>...]}
   c-to-json wraps the list in a single named slot for streamer-layout
   reasons (two labeled-array slots at the same level malform the JSON);
   on this side we just project [inner]. *)
let parse_list (j : Yojson.Basic.t) : t list Rjson.j_result =
  let open Rjson in
  (let* o = cast_object j in
   let* () = expect_kind "ConstBindings" o in
   with_field "inner" (cast_map parse) o)
  |> Rjson.add_reason "ConstBindings" j
