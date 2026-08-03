open Protocols
open Exp
open Imp

(* Helpers *)
let var (name : string) : Variable.t = Variable.from_name name

(* --- Test inputs ---------------------------------------------------- *)

(* Models the binomialOptions shape:
     for (int k = c_start - 1; k >= c_end; ) {
         A[k] = ...;
         k = k - 1;
         A[k] = ...;
         k = k - 1;
     }

   The inc slot is empty. Both decrements live at the top of the body.
   Without the body-internal-increment fix this falls through to [Star];
   we want a structured [For] of step Plus(2) / dir Decrease, with:
   - lower_bound = Var c_end
   - upper_bound = (Var c_start) - 1
   - the body's decrements preserved at their positions
   - [Decl k = k] prepended to the For body *)
let body_decrement_input : For.t * Stmt.t =
  let k = var "k" in
  let init = Stmt.decl_set k (n_minus (Var (var "c_start")) (Num 1)) in
  let cond = n_ge (Var k) (Var (var "c_end")) in
  let inc = Stmt.Skip in
  let body =
    Stmt.from_list
      [
        Stmt.Write { selector = []; array = var "A"; index = [ Var k ]; payload = None; guard = None };
        Stmt.assign Ty.int k (n_minus (Var k) (Num 1));
        Stmt.Write { selector = []; array = var "A"; index = [ Var k ]; payload = None; guard = None };
        Stmt.assign Ty.int k (n_minus (Var k) (Num 1));
      ]
  in
  ({ init; cond; inc }, body)

(* Models:
     for (int i = 0; i < n; ) {
         A[i] = ...;
         i = i + 1;
         A[i] = ...;
         i = i + 1;
     }

   Same shape but ascending. Expected step Plus(2) / dir Increase. *)
let body_increment_input : For.t * Stmt.t =
  let i = var "i" in
  let init = Stmt.decl_set i (Num 0) in
  let cond = n_lt (Var i) (Var (var "n")) in
  let inc = Stmt.Skip in
  let body =
    Stmt.from_list
      [
        Stmt.Write { selector = []; array = var "A"; index = [ Var i ]; payload = None; guard = None };
        Stmt.assign Ty.int i (n_plus (Var i) (Num 1));
        Stmt.Write { selector = []; array = var "A"; index = [ Var i ]; payload = None; guard = None };
        Stmt.assign Ty.int i (n_plus (Var i) (Num 1));
      ]
  in
  ({ init; cond; inc }, body)

(* --- Predicates over the produced [Stmt.t] -------------------------- *)

(* Walk the produced statement and return the first [For] range we see,
   alongside its body. *)
let find_for : Stmt.t -> (Range.t * Stmt.t) option =
  let rec go : Stmt.t -> (Range.t * Stmt.t) option = function
    | For (r, body) -> Some (r, body)
    | Seq (s1, s2) -> ( match go s1 with Some _ as r -> r | None -> go s2)
    | If (_, s1, s2) -> ( match go s1 with Some _ as r -> r | None -> go s2)
    | Star s -> go s
    | _ -> None
  in
  go

let has_star : Stmt.t -> bool =
  let rec go : Stmt.t -> bool = function
    | Star _ -> true
    | Seq (s1, s2) | If (_, s1, s2) -> go s1 || go s2
    | For (_, s) -> go s
    | _ -> false
  in
  go

(* Body-prefix predicate: the For body must start with [Decl x = x]. *)
let starts_with_self_shadow (x : Variable.t) (body : Stmt.t) : bool =
  let rec first_decl : Stmt.t -> Decl.t option = function
    | Decl d -> Some d
    | Seq (s1, _) -> first_decl s1
    | _ -> None
  in
  match first_decl body with
  | Some d ->
      Variable.equal d.var x
      && (match d.init with Some (Var y) -> Variable.equal y x | _ -> false)
  | None -> false

(* Count occurrences of [Assign x = x ± k] anywhere in the statement. *)
let count_self_assigns (x : Variable.t) (s : Stmt.t) : int =
  let is_self_inc : Stmt.t -> bool = function
    | Assign { var; data = Binary (Plus Signedness.Signed, Var y, _); _ }
    | Assign { var; data = Binary (Plus Signedness.Signed, _, Var y); _ }
    | Assign { var; data = Binary (Minus _, Var y, _); _ } ->
        Variable.equal var x && Variable.equal y x
    | _ -> false
  in
  let rec go (acc : int) : Stmt.t -> int = function
    | s when is_self_inc s -> acc + 1
    | Seq (s1, s2) -> go (go acc s1) s2
    | If (_, s1, s2) -> go (go acc s1) s2
    | For (_, s) | Star s -> go acc s
    | _ -> acc
  in
  go 0 s

(* --- Tests ---------------------------------------------------------- *)

let test_body_decrement_produces_for () =
  let loop, body = body_decrement_input in
  let out = For.to_stmt loop body in
  Alcotest.(check bool)
    "no Star fall-through"
    false (has_star out);
  match find_for out with
  | None -> Alcotest.failf "expected a For; got: %s" (Stmt.to_string out)
  | Some (r, _) ->
      Alcotest.(check string) "loop var" "k" (Variable.name r.var);
      (match r.dir with
      | Decrease -> ()
      | Increase -> Alcotest.failf "expected Decrease, got Increase");
      (match r.step with
      | Plus (Num 2) -> ()
      | s ->
          Alcotest.failf "expected step Plus(2), got %s"
            (Range.Step.to_string s));
      (* lower_bound = Var c_end *)
      (match r.lower_bound with
      | Var v when Variable.name v = "c_end" -> ()
      | e ->
          Alcotest.failf "expected lower_bound = c_end, got %s"
            (Exp.n_to_string e));
      (* upper_bound = (Var c_start) - 1 — init was decl_set k (c_start - 1) *)
      match r.upper_bound with
      | Binary (Minus _, Var v, Num 1) when Variable.name v = "c_start" -> ()
      | e ->
          Alcotest.failf "expected upper_bound = c_start - 1, got %s"
            (Exp.n_to_string e)

let test_body_decrement_shadow_in_body () =
  let loop, body = body_decrement_input in
  let out = For.to_stmt loop body in
  match find_for out with
  | None -> Alcotest.fail "expected a For"
  | Some (_, for_body) ->
      Alcotest.(check bool)
        "For body starts with Decl k = k"
        true
        (starts_with_self_shadow (var "k") for_body);
      (* The body's decrements must remain — same count as the source. *)
      Alcotest.(check int)
        "two k = k - 1 assigns preserved in For body"
        2
        (count_self_assigns (var "k") for_body)

let test_body_increment_produces_for () =
  let loop, body = body_increment_input in
  let out = For.to_stmt loop body in
  Alcotest.(check bool) "no Star fall-through" false (has_star out);
  match find_for out with
  | None -> Alcotest.failf "expected a For; got: %s" (Stmt.to_string out)
  | Some (r, _) ->
      Alcotest.(check string) "loop var" "i" (Variable.name r.var);
      (match r.dir with
      | Increase -> ()
      | Decrease -> Alcotest.failf "expected Increase, got Decrease");
      (match r.step with
      | Plus (Num 2) -> ()
      | s ->
          Alcotest.failf "expected step Plus(2), got %s"
            (Range.Step.to_string s))

(* Regression test for the no-extras path: when [extra_step] is empty,
   [for(j = ub; j >= lb; j -= step)] must keep the symbolic step verbatim
   (i.e. [Plus(step)], not [Plus(-(step))] from naive sign-flipping). *)
let test_no_extras_symbolic_minus_preserved () =
  let j = var "j" in
  let init = Stmt.decl_set j (Var (var "ub")) in
  let cond = n_ge (Var j) (Var (var "lb")) in
  let inc = Stmt.assign Ty.int j (n_minus (Var j) (Var (var "step"))) in
  let body = Stmt.Sync (Sync.syncthreads ()) in
  let out = For.to_stmt { init; cond; inc } body in
  match find_for out with
  | None -> Alcotest.failf "expected a For; got: %s" (Stmt.to_string out)
  | Some (r, _) -> (
      match r.step with
      | Plus (Var v) when Variable.name v = "step" -> ()
      | s ->
          Alcotest.failf "expected step Plus(step), got %s"
            (Range.Step.to_string s))

(* Mixed source: the inc slot has [i++] AND the body has another [i++].
   Total step = +2; the body's increment stays in place under a shadow. *)
let test_mixed_inc_and_body_sums () =
  let i = var "i" in
  let init = Stmt.decl_set i (Num 0) in
  let cond = n_lt (Var i) (Var (var "n")) in
  let inc = Stmt.assign Ty.int i (n_plus (Var i) (Num 1)) in
  let body =
    Stmt.from_list
      [
        Stmt.Write { selector = []; array = var "A"; index = [ Var i ]; payload = None; guard = None };
        Stmt.assign Ty.int i (n_plus (Var i) (Num 1));
      ]
  in
  let out = For.to_stmt { init; cond; inc } body in
  match find_for out with
  | None -> Alcotest.failf "expected a For; got: %s" (Stmt.to_string out)
  | Some (r, for_body) ->
      (match r.step with
      | Plus (Num 2) -> ()
      | s ->
          Alcotest.failf "expected step Plus(2), got %s"
            (Range.Step.to_string s));
      Alcotest.(check bool)
        "shadow Decl present" true
        (starts_with_self_shadow i for_body);
      Alcotest.(check int)
        "the body's i = i + 1 is preserved (inc-slot one is gone)"
        1
        (count_self_assigns i for_body)

(* --- Conditions that stand in for a comparison against zero ---------- *)

(* [for (int i = n; i; i--)] and [for (int i = n; i != 0; i--)] are the same
   loop in C, so both spellings must yield the same descending range. *)
let truthiness_loop (cond : bexp) : Stmt.t =
  let i = var "i" in
  let init = Stmt.decl_set i (Var (var "n")) in
  let inc = Stmt.assign Ty.int i (n_minus (Var i) (Num 1)) in
  let body =
    Stmt.Write
      { array = var "S"; selector = []; index = [ Var i ]; payload = None;
        guard = None }
  in
  For.to_stmt { init; cond; inc } body

let test_bare_variable_is_bounded () =
  let out = truthiness_loop (cast_bool (Var (var "i"))) in
  Alcotest.(check bool) "no Star fall-through" false (has_star out);
  match find_for out with
  | None -> Alcotest.failf "expected a For; got: %s" (Stmt.to_string out)
  | Some (r, _) ->
      Alcotest.(check string) "loop var" "i" (Variable.name r.var);
      (match r.dir with
      | Decrease -> ()
      | Increase -> Alcotest.failf "expected Decrease, got Increase");
      (match r.step with
      | Plus (Num 1) -> ()
      | s ->
          Alcotest.failf "expected step Plus(1), got %s"
            (Range.Step.to_string s));
      (match r.lower_bound with
      | Num 1 -> ()
      | e ->
          Alcotest.failf "expected lower_bound = 1, got %s"
            (Exp.n_to_string e));
      (match r.upper_bound with
      | Var v when Variable.name v = "n" -> ()
      | e ->
          Alcotest.failf "expected upper_bound = n, got %s"
            (Exp.n_to_string e))

let test_bare_variable_matches_neq_zero () =
  let range_of (cond : bexp) : string =
    match find_for (truthiness_loop cond) with
    | Some (r, _) -> Range.to_string r
    | None ->
        Alcotest.failf "expected a For for condition %s" (Exp.b_to_string cond)
  in
  Alcotest.(check string)
    "bare variable agrees with an explicit != 0"
    (range_of (n_neq (Var (var "i")) (Num 0)))
    (range_of (cast_bool (Var (var "i"))))

(* The subtraction spelling [for (int i = 0; i - n; i++)] already worked and
   must keep the same bounds now that it routes through the shared
   comparison parser. *)
let test_subtraction_shape_preserved () =
  let i = var "i" in
  let init = Stmt.decl_set i (Num 0) in
  let cond = cast_bool (n_minus (Var i) (Var (var "n"))) in
  let inc = Stmt.assign Ty.int i (n_plus (Var i) (Num 1)) in
  let body =
    Stmt.Write
      { array = var "S"; selector = []; index = [ Var i ]; payload = None;
        guard = None }
  in
  let out = For.to_stmt { init; cond; inc } body in
  match find_for out with
  | None -> Alcotest.failf "expected a For; got: %s" (Stmt.to_string out)
  | Some (r, _) ->
      Alcotest.(check string) "loop var" "i" (Variable.name r.var);
      (match r.lower_bound with
      | Num 0 -> ()
      | e ->
          Alcotest.failf "expected lower_bound = 0, got %s"
            (Exp.n_to_string e));
      (match r.upper_bound with
      | Binary (Minus _, Var v, Num 1) when Variable.name v = "n" -> ()
      | e ->
          Alcotest.failf "expected upper_bound = n - 1, got %s"
            (Exp.n_to_string e))

(* [for (int i = 0; j - n; i++)] says nothing about [i], so no range over [i]
   may be derived from it. *)
let test_other_variable_declined () =
  let i = var "i" in
  let init = Stmt.decl_set i (Num 0) in
  let cond = cast_bool (n_minus (Var (var "j")) (Var (var "n"))) in
  let inc = Stmt.assign Ty.int i (n_plus (Var i) (Num 1)) in
  let body =
    Stmt.Write
      { array = var "S"; selector = []; index = [ Var i ]; payload = None;
        guard = None }
  in
  let out = For.to_stmt { init; cond; inc } body in
  match find_for out with
  | None -> Alcotest.(check bool) "degrades to Star" true (has_star out)
  | Some (r, _) ->
      Alcotest.failf "expected no range; got one over %s" (Range.to_string r)

(* With two increments, the condition picks which one carries the range:
   [for (int i = 0; j - n; j++, i++)] is a loop over [j]. *)
let test_condition_selects_its_own_variable () =
  let i = var "i" and j = var "j" in
  let init = Stmt.decl_set i (Num 0) in
  let cond = cast_bool (n_minus (Var j) (Var (var "n"))) in
  let inc =
    Stmt.from_list
      [
        Stmt.assign Ty.int j (n_plus (Var j) (Num 1));
        Stmt.assign Ty.int i (n_plus (Var i) (Num 1));
      ]
  in
  let body =
    Stmt.Write
      { array = var "S"; selector = []; index = [ Var i ]; payload = None;
        guard = None }
  in
  let out = For.to_stmt { init; cond; inc } body in
  match find_for out with
  | None -> Alcotest.failf "expected a For; got: %s" (Stmt.to_string out)
  | Some (r, _) ->
      Alcotest.(check string)
        "range is over the variable the condition constrains" "j"
        (Variable.name r.var)

let truthiness_tests =
  [
    ( "a bare variable is a comparison against zero",
      `Quick,
      test_bare_variable_is_bounded );
    ( "a bare variable agrees with the != 0 spelling",
      `Quick,
      test_bare_variable_matches_neq_zero );
    ( "the subtraction spelling keeps its bounds",
      `Quick,
      test_subtraction_shape_preserved );
    ( "a condition on another variable yields no range",
      `Quick,
      test_other_variable_declined );
    ( "the condition selects which increment carries the range",
      `Quick,
      test_condition_selects_its_own_variable );
  ]

let body_increment_tests =
  [
    ( "body-internal decrements produce structured For",
      `Quick,
      test_body_decrement_produces_for );
    ( "body-internal decrements add Decl k = k shadow",
      `Quick,
      test_body_decrement_shadow_in_body );
    ( "body-internal increments produce structured For",
      `Quick,
      test_body_increment_produces_for );
    ( "no extras: symbolic minus step preserved verbatim",
      `Quick,
      test_no_extras_symbolic_minus_preserved );
    ( "mixed inc-slot and body increments sum to step",
      `Quick,
      test_mixed_inc_and_body_sums );
  ]

let all_tests =
  [
    ("body-internal increments", body_increment_tests);
    ("zero-comparison conditions", truthiness_tests);
  ]
let () = Alcotest.run "for" all_tests
