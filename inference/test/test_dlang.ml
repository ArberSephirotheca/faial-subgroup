open Stage0
open Protocols
open Inference
open D_lang

let stmt : Stmt.t Alcotest.testable =
  let pp fmt stmt = Format.fprintf fmt "%s" (Stmt.to_string stmt) in
  let equal = ( = ) in
  Alcotest.testable pp equal

let test_last_and_skip_last () : unit =
  let open Stmt in
  let s = Stmt.from_list [ BreakStmt; GotoStmt; ContinueStmt ] in
  Alcotest.check stmt "last returns ContinueStmt" ContinueStmt (last s);
  Alcotest.check stmt "skip_last returns correct list"
    (Stmt.from_list [ BreakStmt; GotoStmt ])
    (skip_last s)

(* Pin the IntegerLiteral parser to specific values across the three
   widths it handles (OCaml int, signed Int64, uint64 reinterpreted as
   signed). A regression to the [Int.max_int] fallback would be caught
   here even when an end-to-end fixture's DRF verdict is unaffected. *)
let parse_int_literal (s : string) : int =
  let j : Yojson.Basic.t =
    `Assoc
      [
        ("kind", `String "IntegerLiteral");
        ("value", `String s);
        ("type", `Assoc [ ("qualType", `String "unsigned long long") ]);
      ]
  in
  match C_lang.parse_expr j with
  | Ok (IntegerLiteral i) -> i
  | Ok _ -> Alcotest.failf "parse_expr: expected IntegerLiteral for %s" s
  | Error e -> Alcotest.failf "parse_expr failed: %s" (Rjson.error_to_string e)

let test_integer_literal_parses_ocaml_int_range () : unit =
  Alcotest.(check int) "small positive" 42 (parse_int_literal "42");
  Alcotest.(check int) "small negative" (-42) (parse_int_literal "-42");
  Alcotest.(check int) "OCaml max_int" Int.max_int
    (parse_int_literal (string_of_int Int.max_int))

let test_integer_literal_parses_uint64_sentinel () : unit =
  (* [0xFFFFFFFFFFFFFFFFULL] (decimal [18446744073709551615]) exceeds
     signed Int64; it parses through the ["0u" ^ s] path and
     reinterprets to signed [-1], which fits OCaml [int]. *)
  Alcotest.(check int) "uint64 max" (-1)
    (parse_int_literal "18446744073709551615");
  (* Real-world sentinels observed in HeCBench logic-rewrite-cuda
     ([0xFF42E54B94E2DA0DULL] and [0xC4D7F9E2C7CDA4D3ULL]). The
     two's-complement signed values fit OCaml's 63-bit [int]. *)
  Alcotest.(check int) "logic-rewrite 0xFF42... sentinel"
    (-49064778989728563)
    (parse_int_literal "18397679294719823053");
  Alcotest.(check int) "logic-rewrite 0xC4D7... sentinel"
    (-4265267296055464877)
    (parse_int_literal "14181476777654086739")

(* c-to-json's path_condition can carry a synthetic for-init-bound
   conjunct: a [BinaryOperator] with opcode [>=]/[<=] whose LHS is a
   bare [DeclRefExpr] (no [ImplicitCastExpr] wrap) and whose neither the
   compare node nor the LHS [DeclRefExpr] carries a [range] — both
   synthesised after parsing. The path-condition parser must accept
   that shape. *)
let test_synthetic_for_init_bound_parses () : unit =
  let v_ref : Yojson.Basic.t =
    `Assoc
      [
        ("kind", `String "DeclRefExpr");
        ("type", `Assoc [ ("qualType", `String "int") ]);
        ( "referencedDecl",
          `Assoc
            [
              ("kind", `String "VarDecl");
              ("name", `String "v");
              ("type", `Assoc [ ("qualType", `String "int") ]);
            ] );
      ]
  in
  let zero : Yojson.Basic.t =
    `Assoc
      [
        ("kind", `String "IntegerLiteral");
        ("value", `String "0");
        ("type", `Assoc [ ("qualType", `String "int") ]);
      ]
  in
  let init_bound : Yojson.Basic.t =
    `Assoc
      [
        ("kind", `String "BinaryOperator");
        ("type", `Assoc [ ("qualType", `String "bool") ]);
        ("opcode", `String ">=");
        ("inner", `List [ v_ref; zero ]);
      ]
  in
  match C_lang.parse_expr init_bound with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "parse_expr on synthetic init-bound failed: %s"
        (Rjson.error_to_string e)

(* Pointer-dereference rewrite shapes. These pin what [rewrite_exp]
   produces for [*p = src] / [*(p + n) = src] / [p[n] = src] so we can
   agree on the right per-shape index before changing the rewriter. *)

let ptr_int_ty : Ty.t = J_type.int (* placeholder — the deref's
   recorded element type; concrete value isn't asserted below *)

let ident (name : string) : C_lang.Expr.t =
  Ident (Decl_expr.from_name ~ty:ptr_int_ty (Variable.from_name name))

let deref (child : C_lang.Expr.t) : C_lang.Expr.t =
  UnaryOperator { opcode = "*"; child; ty = ptr_int_ty }

let assign (lhs : C_lang.Expr.t) (rhs : C_lang.Expr.t) : C_lang.Expr.t =
  BinaryOperator { opcode = "="; lhs; rhs; ty = ptr_int_ty }

let plus (l : C_lang.Expr.t) (r : C_lang.Expr.t) : C_lang.Expr.t =
  BinaryOperator { opcode = "+"; lhs = l; rhs = r; ty = ptr_int_ty }

(* Run [rewrite_exp] on a C-AST expression and return the first
   [WriteAccessStmt] target found in the accumulated D-AST statement,
   or fail. *)
let first_write_target (e : C_lang.Expr.t) : D_lang.d_subscript =
  let stmt, _ = D_lang.run0 (D_lang.rewrite_exp e) in
  let rec walk : D_lang.Stmt.t -> D_lang.d_subscript option = function
    | WriteAccessStmt w -> Some w.target
    | Seq (a, b) -> (
        match walk a with Some _ as r -> r | None -> walk b)
    | _ -> None
  in
  match walk stmt with
  | Some t -> t
  | None ->
      Alcotest.failf
        "expected a WriteAccessStmt; got: %s" (D_lang.Stmt.to_string stmt)

let index_to_string (idx : D_lang.Expr.t list) : string =
  "[" ^ String.concat "; " (List.map D_lang.Expr.to_string idx) ^ "]"

(* Shape 1: bare-deref write [*p = 5]. The rewrite currently emits an
   unknown index ([rhs = C_lang.Expr.unknown] in [d_lang.ml]); this
   test pins the desired shape [p[0]] — a bare deref accesses one
   element at offset 0 from the pointer's value, and the downstream
   alias pass can resolve [p[0]] back to [base[offset]] when [p] is an
   alias for [base + offset]. *)
let test_bare_deref_write_index_zero () : unit =
  let expr = assign (deref (ident "p")) (IntegerLiteral 5) in
  let t = first_write_target expr in
  Alcotest.(check string) "target array name" "p" (Variable.name (D_lang.subscript_name t));
  match t.index with
  | [ IntegerLiteral 0 ] -> ()
  | other ->
      Alcotest.failf "expected target.index = [IntegerLiteral 0]; got %s"
        (index_to_string other)

(* Shape 2: offset-deref write [*(p + 3) = 5]. The rewrite already
   passes the offset through; pin that as a regression guard. *)
let test_offset_deref_write_index_offset () : unit =
  let expr = assign (deref (plus (ident "p") (IntegerLiteral 3))) (IntegerLiteral 5) in
  let t = first_write_target expr in
  Alcotest.(check string) "target array name" "p" (Variable.name (D_lang.subscript_name t));
  match t.index with
  | [ IntegerLiteral 3 ] -> ()
  | other ->
      Alcotest.failf "expected target.index = [IntegerLiteral 3]; got %s"
        (index_to_string other)

(* Shape 3: bare-deref write through an aliased pointer:
     [float* p = q + 5; *p = 1;]
   The rewrite happens at the d_lang level, before any alias
   propagation (which runs later in IMP). So at this stage the
   produced D-AST should be:
     - a [DeclStmt] for [p] whose init is the [q + 5] expression, and
     - a [WriteAccessStmt] for the deref with target.name = p.
   The interesting assertion is the target.index — under the desired
   shape it's [IntegerLiteral 0] (alias propagation downstream will
   then resolve [p[0]] back to [q[5]]); under the current
   implementation it's [?]. *)
let first_write_target_stmt (s : C_lang.Stmt.t) : D_lang.d_subscript =
  let rec walk : D_lang.Stmt.t -> D_lang.d_subscript option = function
    | WriteAccessStmt w -> Some w.target
    | Seq (a, b) -> (
        match walk a with Some _ as r -> r | None -> walk b)
    | IfStmt { then_stmt; else_stmt; _ } -> (
        match walk then_stmt with Some _ as r -> r | None -> walk else_stmt)
    | _ -> None
  in
  let result = D_lang.rewrite_stmt s in
  match walk result with
  | Some t -> t
  | None ->
      Alcotest.failf
        "expected a WriteAccessStmt; got: %s" (D_lang.Stmt.to_string result)

let test_alias_then_bare_deref_write () : unit =
  let p_decl : C_lang.c_decl =
    {
      var = Variable.from_name "p";
      ty = ptr_int_ty;
      init = Some (IExpr (plus (ident "q") (IntegerLiteral 5)));
      attrs = [];
    }
  in
  let stmt : C_lang.Stmt.t =
    Seq (DeclStmt [ p_decl ],
         SExpr (assign (deref (ident "p")) (IntegerLiteral 1)))
  in
  let t = first_write_target_stmt stmt in
  Alcotest.(check string) "target array name" "p" (Variable.name (D_lang.subscript_name t));
  match t.index with
  | [ IntegerLiteral 0 ] -> ()
  | other ->
      Alcotest.failf "expected target.index = [IntegerLiteral 0]; got %s"
        (index_to_string other)

(* Shape 4: bare-deref write after pointer reassignment:
     [float* c = q; c = c + 5; *c = 7;]
   The pointer has been bumped, so [c] no longer equals its
   initialisation. We still want [*c] to mean [c[0]] at the d_lang
   level — alias propagation in IMP is responsible for tracking
   [c]'s current base+offset and resolving [c[0]] to [q[5]] when
   that information survives the bumps. The d_lang test only pins
   the local rewrite: [*c] becomes [c[0]] regardless of c's history. *)
let test_bumped_pointer_bare_deref_write () : unit =
  let c_decl : C_lang.c_decl =
    {
      var = Variable.from_name "c";
      ty = ptr_int_ty;
      init = Some (IExpr (ident "q"));
      attrs = [];
    }
  in
  let bump : C_lang.Stmt.t =
    SExpr (assign (ident "c") (plus (ident "c") (IntegerLiteral 5)))
  in
  let deref_write : C_lang.Stmt.t =
    SExpr (assign (deref (ident "c")) (IntegerLiteral 7))
  in
  let stmt : C_lang.Stmt.t =
    Seq (DeclStmt [ c_decl ], Seq (bump, deref_write))
  in
  let t = first_write_target_stmt stmt in
  Alcotest.(check string) "target array name" "c" (Variable.name (D_lang.subscript_name t));
  match t.index with
  | [ IntegerLiteral 0 ] -> ()
  | other ->
      Alcotest.failf "expected target.index = [IntegerLiteral 0]; got %s"
        (index_to_string other)

(* Shape 5: bare-deref READ [... = *p]. Currently no read access is
   emitted at all — the [UnaryOperator { opcode = "*" }] expression
   falls through to the catch-all in [rewrite_exp] and survives into
   the D-AST as a UnaryOperator node. Downstream, [d_to_imp] then
   rewrites it to an [Unknown] nexp. The desired shape: a
   [ReadAccessStmt] with target = [p[0]], symmetric to the write
   side. *)
let first_read_source (e : C_lang.Expr.t) : D_lang.d_subscript =
  let stmt, _ = D_lang.run0 (D_lang.rewrite_exp e) in
  let rec walk : D_lang.Stmt.t -> D_lang.d_subscript option = function
    | ReadAccessStmt r -> Some r.source
    | Seq (a, b) -> (
        match walk a with Some _ as r -> r | None -> walk b)
    | _ -> None
  in
  match walk stmt with
  | Some s -> s
  | None ->
      Alcotest.failf
        "expected a ReadAccessStmt; got: %s" (D_lang.Stmt.to_string stmt)

let test_bare_deref_read_index_zero () : unit =
  let s = first_read_source (deref (ident "p")) in
  Alcotest.(check string) "source array name" "p" (Variable.name (D_lang.subscript_name s));
  match s.index with
  | [ IntegerLiteral 0 ] -> ()
  | other ->
      Alcotest.failf "expected source.index = [IntegerLiteral 0]; got %s"
        (index_to_string other)

(* Shape 6: nested scalar assignment-in-expression
     [(idx = idx / 1) % 6]
   should desugar at the d_lang level into:
     - a sequenced side-effect [SExpr (idx = idx / 1)], and
     - the expression value [idx], substituted in place of the
       assignment so the host expression becomes [idx %% 6].
   [rewrite_exp] has dedicated [opcode = "="] arms for every
   non-scalar lvalue shape (array subscripts, pointer derefs, C++
   overloaded operators) but no arm for [BinaryOperator
   { lhs = Ident _; opcode = "="; rhs; _ }]. With no arm, the
   assignment survives into the D-AST verbatim, [d_to_imp]'s
   [parse_bin] degrades it to [Unknown], and the dataflow chain
   into the host expression is lost. This test pins the desired
   shape. *)
let div (l : C_lang.Expr.t) (r : C_lang.Expr.t) : C_lang.Expr.t =
  BinaryOperator { opcode = "/"; lhs = l; rhs = r; ty = ptr_int_ty }

let mod_ (l : C_lang.Expr.t) (r : C_lang.Expr.t) : C_lang.Expr.t =
  BinaryOperator { opcode = "%"; lhs = l; rhs = r; ty = ptr_int_ty }

let collect_assign_sexprs (s : D_lang.Stmt.t) : D_lang.Expr.t list =
  let rec walk acc = function
    | D_lang.Stmt.SExpr
        (D_lang.Expr.BinaryOperator { opcode = "="; _ } as e) ->
        e :: acc
    | D_lang.Stmt.Seq (a, b) -> walk (walk acc a) b
    | _ -> acc
  in
  List.rev (walk [] s)

let ident_name : D_lang.Expr.t -> string option = function
  | D_lang.Expr.Ident x -> Some (Variable.name (Decl_expr.name x))
  | _ -> None

let test_nested_scalar_assignment_in_expression () : unit =
  let idx = ident "idx" in
  let nested = assign idx (div idx (IntegerLiteral 1)) in
  let host = mod_ nested (IntegerLiteral 6) in
  let stmt, value = D_lang.run0 (D_lang.rewrite_exp host) in
  (* Exactly one [SExpr (idx = idx / 1)] side-effect. *)
  (match collect_assign_sexprs stmt with
   | [ BinaryOperator
         { opcode = "=";
           lhs = D_lang.Expr.Ident _ as lhs;
           rhs = D_lang.Expr.BinaryOperator
                   { opcode = "/";
                     lhs = D_lang.Expr.Ident _ as div_lhs;
                     rhs = D_lang.Expr.IntegerLiteral 1;
                     _ };
           _ } ]
     when ident_name lhs = Some "idx" && ident_name div_lhs = Some "idx" ->
       ()
   | _ ->
       Alcotest.failf
         "expected one [SExpr (idx = idx / 1)] side-effect; got: %s"
         (D_lang.Stmt.to_string stmt));
  (* The expression value is [idx %% 6], with [idx] (not the
     assignment) in the LHS slot. *)
  match value with
  | D_lang.Expr.BinaryOperator
      { opcode = "%";
        lhs = D_lang.Expr.Ident _ as lhs;
        rhs = D_lang.Expr.IntegerLiteral 6;
        _ }
    when ident_name lhs = Some "idx" -> ()
  | other ->
      Alcotest.failf
        "expected [idx %% 6] as the value expression; got: %s"
        (D_lang.Expr.to_string other)

let subscript (base : C_lang.Expr.t) (idx : C_lang.Expr.t) : C_lang.Expr.t =
  ArraySubscriptExpr
    { lhs = base; rhs = idx; ty = ptr_int_ty; location = Location.empty }

let ternary (c : C_lang.Expr.t) (t : C_lang.Expr.t) (e : C_lang.Expr.t) :
    C_lang.Expr.t =
  ConditionalOperator { cond = c; then_expr = t; else_expr = e; ty = ptr_int_ty }

let lt (l : C_lang.Expr.t) (r : C_lang.Expr.t) : C_lang.Expr.t =
  BinaryOperator { opcode = "<"; lhs = l; rhs = r; ty = J_type.bool }

let and_c (l : C_lang.Expr.t) (r : C_lang.Expr.t) : C_lang.Expr.t =
  BinaryOperator { opcode = "&&"; lhs = l; rhs = r; ty = J_type.bool }

(* Find the guard carried by the hoisted read of [array]. Outer option:
   whether a matching ReadAccessStmt was found; inner: its guard. *)
let read_guard (array : string) (e : C_lang.Expr.t) :
    D_lang.Expr.t option option =
  let stmt, _ = D_lang.run0 (D_lang.rewrite_exp e) in
  let rec walk : D_lang.Stmt.t -> D_lang.Expr.t option option = function
    | ReadAccessStmt { source; guard; _ }
      when Variable.name (D_lang.subscript_name source) = array ->
        Some guard
    | Seq (a, b) -> ( match walk a with Some _ as r -> r | None -> walk b)
    | _ -> None
  in
  walk stmt

(* A read hoisted out of a ?: operand keeps the ternary condition as a
   guard on the access. *)
let test_ternary_read_is_guarded () : unit =
  let e =
    ternary
      (lt (ident "tid") (ident "n"))
      (subscript (ident "s") (ident "tid"))
      (IntegerLiteral 0)
  in
  match read_guard "s" e with
  | Some (Some (BinaryOperator { opcode = "<"; lhs; rhs; _ }))
    when ident_name lhs = Some "tid" && ident_name rhs = Some "n" ->
      ()
  | Some (Some other) ->
      Alcotest.failf "s[tid] read guarded by unexpected expr: %s"
        (D_lang.Expr.to_string other)
  | Some None ->
      Alcotest.fail "s[tid] read from a ?: is unguarded (expected tid < n)"
  | None -> Alcotest.fail "expected a ReadAccessStmt for s"

(* The right operand of a short-circuit && is only evaluated when the
   left operand holds, so a read there is guarded by the left operand. *)
let test_and_rhs_read_is_guarded () : unit =
  let e = and_c (ident "x") (subscript (ident "a") (ident "i")) in
  match read_guard "a" e with
  | Some (Some g) when ident_name g = Some "x" -> ()
  | Some (Some other) ->
      Alcotest.failf "a[i] read in (x && a[i]) guarded by unexpected expr: %s"
        (D_lang.Expr.to_string other)
  | Some None ->
      Alcotest.fail "a[i] read in (x && a[i]) is unguarded (expected x)"
  | None -> Alcotest.fail "expected a ReadAccessStmt for a"

let tests : unit Alcotest.test_case list =
  [
    ("last + skip_last", `Quick, test_last_and_skip_last);
    ( "IntegerLiteral: OCaml int range",
      `Quick,
      test_integer_literal_parses_ocaml_int_range );
    ( "IntegerLiteral: uint64 sentinel",
      `Quick,
      test_integer_literal_parses_uint64_sentinel );
    ( "path_condition: synthetic for-init bound",
      `Quick,
      test_synthetic_for_init_bound_parses );
    ( "deref: *p = 5 desired as p[0]",
      `Quick,
      test_bare_deref_write_index_zero );
    ( "deref: *(p + 3) = 5 keeps offset",
      `Quick,
      test_offset_deref_write_index_offset );
    ( "deref: float* p = q + 5; *p = 1; — alias then bare-deref write",
      `Quick,
      test_alias_then_bare_deref_write );
    ( "deref: float* c = q; c = c + 5; *c = 7; — write after pointer bump",
      `Quick,
      test_bumped_pointer_bare_deref_write );
    ( "deref: bare-deref read [... = *p] emits a ReadAccessStmt",
      `Quick,
      test_bare_deref_read_index_zero );
    ( "nested scalar [(x = e) op v]: lift the assign as a side-effect",
      `Quick,
      test_nested_scalar_assignment_in_expression );
    ( "ternary read [(tid < n) ? s[tid] : 0]: access guarded by tid < n",
      `Quick,
      test_ternary_read_is_guarded );
    ( "short-circuit [x && a[i]]: RHS read guarded by x",
      `Quick,
      test_and_rhs_read_is_guarded );
  ]

let () = Alcotest.run "D_lang" [ ("dlang", tests) ]
