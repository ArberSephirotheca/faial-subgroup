open Stage0
open Protocols
open Inference

(* "int *" is recognised by [C_type.is_array], so a [J_type.t] built
   from it makes [rewrite_arg] take the pointer-shaped path. *)
let ptr_ty : J_type.t = J_type.from_c_type (C_type.make "int *")

(* Plain scalar type for offset expressions. *)
let int_ty : J_type.t = J_type.int

let ident ?(ty = ptr_ty) (name : string) : C_lang.Expr.t =
  Ident (Decl_expr.from_name ~ty (Variable.from_name name))

let int_lit (n : int) : C_lang.Expr.t = IntegerLiteral n

let addr_of (e : C_lang.Expr.t) : C_lang.Expr.t =
  UnaryOperator { opcode = "&"; child = e; ty = ptr_ty }

let subscript (base : C_lang.Expr.t) (idx : C_lang.Expr.t) : C_lang.Expr.t =
  ArraySubscriptExpr { lhs = base; rhs = idx; ty = int_ty; location = Location.empty }

(* Infix [+] that picks the result type from the operands: if either
   side is array/pointer-typed the result is [ptr_ty]; otherwise it
   is [int_ty]. Shadows [Stdlib.( + )] inside this file; we don't
   need integer addition here. *)
let ( + ) (l : C_lang.Expr.t) (r : C_lang.Expr.t) : C_lang.Expr.t =
  let is_ptr (e : C_lang.Expr.t) : bool =
    J_type.matches C_type.is_array (C_lang.Expr.to_type e)
  in
  let ty = if is_ptr l || is_ptr r then ptr_ty else int_ty in
  BinaryOperator { opcode = "+"; lhs = l; rhs = r; ty }

(* A pointer-returning host function call. Uses an [int *] return
   type so the result lands on the pointer-shaped path of [rewrite_arg]
   (which then falls through to [abstract] because [strip_pointer_offset]
   rejects [CallExpr]). *)
let call_returning_ptr (fn_name : string) (args : C_lang.Expr.t list)
    : C_lang.Expr.t =
  CallExpr {
    func = Ident (Decl_expr.from_name
                    ~ty:ptr_ty
                    ~kind:Decl_expr.Kind.Function
                    (Variable.from_name fn_name));
    args;
    ty = ptr_ty;
  }

(* Run [Host_translate.rewrite_arg 0 e] on a fresh state and return
   the result expression as a string and the freshly-minted parameter
   names. *)
let render (e : C_lang.Expr.t) : string * string list =
  let st, d = State.run (Host_translate.rewrite_expr e) Host_translate.empty in
  let params =
    Host_translate.fresh_params st
    |> List.map (fun p -> Variable.name (C_lang.Param.name p))
  in
  (D_lang.Expr.to_string ~types:true d, params)

let buf : C_lang.Expr.t = ident "buf"
let i : C_lang.Expr.t = ident ~ty:int_ty "i"

(* One-line builder for a test case: runs [render input] and pins
   the output expression and the fresh-param list separately. *)
let test_rewrite_arg (label : string) (input : C_lang.Expr.t)
    (expected_out : string) (expected_fresh : string list)
    : unit Alcotest.test_case =
  (label, `Quick, fun () ->
    let actual_out, actual_fresh = render input in
    Alcotest.(check string) (label ^ ": out") expected_out actual_out;
    Alcotest.(check (list string)) (label ^ ": fresh") expected_fresh actual_fresh)

let tests : unit Alcotest.test_case list =
  [
    test_rewrite_arg "bare pointer"
      buf
      "buf" [];
    test_rewrite_arg "buf + i"
      (buf + i)
      "buf (+.int *) i" [];
    (* [lift_pure] succeeds on the whole expression, so the pointer-shape
       branch is bypassed and the expression survives verbatim. *)
    test_rewrite_arg "buf + (i + 1)"
      (buf + (i + int_lit 1))
      "buf (+.int *) (i (+.int) 1)" [];
    (* [lift_pure] fails on the [CallExpr] in the offset; the pointer-shape
       branch fires and [rewrite_offset] interns the call under
       [__faial_launch_arg_0_off]. *)
    test_rewrite_arg "buf + getOffset()"
      (buf + call_returning_ptr "getOffset" [])
      "buf (+.int *) @Launch0"
      ["@Launch0"];
    (* No pointer-shape match; the whole call is abstracted under the
       positional name [__faial_launch_arg_0]. *)
    test_rewrite_arg "getBuffer()"
      (call_returning_ptr "getBuffer" [])
      "@Launch0"
      ["@Launch0"];
    (* [&buf[i]]: by [&a[k] == a + k], this is equivalent to [buf + i]
       in C. The new [rewrite_arg] recognises the [UnaryOperator(&,
       ArraySubscriptExpr _)] shape and rewrites it to the same form
       as [buf + i], keeping [buf] visible as the array base. *)
    test_rewrite_arg "&buf[i]"
      (addr_of (subscript buf i))
      "buf (+.int *) i" [];
  ]

let () = Alcotest.run "Host_translate" [ ("rewrite_arg", tests) ]
