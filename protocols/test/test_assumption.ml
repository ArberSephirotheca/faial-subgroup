open Protocols
open Exp
module M = Assumption.Match

let v (name : string) : Variable.t = Variable.from_name name
let nvar (name : string) : nexp = Var (v name)
let lt (a : nexp) (b : nexp) : bexp = NRel (Lt Signedness.Signed, a, b)
let access (idx : nexp) : Code.t = Code.Access (Access.read (v "A") [ idx ])
let x_lt_10 : bexp = lt (nvar "x") (Num 10)
let dst_lt_N : bexp = lt (nvar "dst") (nvar "N")
let x_lt_N : bexp = lt (nvar "x") (nvar "N")
let n_lt_10 : bexp = lt (nvar "N") (Num 10)
let zzz_lt_10 : bexp = lt (nvar "zzz") (Num 10)
let bdimx_lt_32 : bexp = lt (Var Variable.bdim_x) (Num 32)
let x_lt_i : bexp = lt (nvar "x") (nvar "i")
let x_lt_zzz : bexp = lt (nvar "x") (nvar "zzz")

(* A binder variable with an optional distinct label and source line. *)
let bvar ?label ?line (name : string) : Variable.t =
  let base =
    match line with
    | Some l ->
        Variable.make ~name
          ~location:
            (Stage0.Location.make ~filename:"t.cu" ~interval:Stage0.Interval.zero
               ~line:(Stage0.Index.from_base1 l))
          ()
    | None -> Variable.from_name name
  in
  match label with Some l -> Variable.set_label l base | None -> base

let binder_a ?(kernel = M.Any) ~label ?(line = M.Any) (bexp : bexp) : Assumption.t
    =
  { Assumption.kernel; target = Assumption.Target.Binder { label; line }; bexp }

let pre_a ?(kernel = M.Any) (bexp : bexp) : Assumption.t =
  { Assumption.kernel; target = Assumption.Target.Pre; bexp }

let mem_conjunct (needle : bexp) (haystack : bexp) : bool =
  List.mem needle (b_and_split haystack)

(* The cond of the first binder labelled [label]. *)
let rec binder_cond (label : string) : Code.t -> bexp option = function
  | Code.Decl d when Variable.label d.var = label -> Some d.cond
  | Code.Loop { cond_range; _ }
    when Variable.label (Cond_range.var cond_range) = label ->
      Some cond_range.cond
  | Code.Decl { body; _ } | Code.Loop { body; _ } -> binder_cond label body
  | Code.If (_, p, q) | Code.Seq (p, q) -> (
      match binder_cond label p with Some c -> Some c | None -> binder_cond label q)
  | Code.Access _ | Code.Sync _ | Code.Skip -> None

let mk_kernel (code : Code.t) : Kernel.t =
  {
    name = "k_test";
    global_variables = Params.add (v "N") Ty.int Params.empty;
    local_variables = Params.empty;
    arrays = Variable.Map.empty;
    pre = Bool true;
    code;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

(* -------------------------------------------------------------------------- *)

let to_string_tests : unit Alcotest.test_case list =
  let check name expected a =
    (name, `Quick, fun () ->
      Alcotest.(check string) name expected (Assumption.to_string a))
  in
  let b = x_lt_10 in
  let bx = Exp.b_to_string b in
  [
    check "bare bexp" bx (pre_a b);
    check "kernel filter, pre" ("kernel=foo: " ^ bx)
      (pre_a ~kernel:(M.Exact "foo") b);
    check "binder only" ("binder=x: " ^ bx) (binder_a ~label:"x" b);
    check "kernel + binder + line"
      ("kernel=foo,binder=x,line=100: " ^ bx)
      (binder_a ~kernel:(M.Exact "foo") ~label:"x" ~line:(M.Exact 100) b);
  ]

let add_to_code_tests : unit Alcotest.test_case list =
  [
    ( "decl match conjoins cond and reports the binder",
      `Quick,
      fun () ->
        let code = Code.decl (bvar "dst") (access (nvar "dst")) in
        let code', hits = Assumption.add_to_code (binder_a ~label:"dst" x_lt_10) code in
        Alcotest.(check int) "one hit" 1 (List.length hits);
        Alcotest.(check bool) "cond carries the bexp" true
          (match binder_cond "dst" code' with
          | Some c -> mem_conjunct x_lt_10 c
          | None -> false) );
    ( "loop counter match conjoins onto the range cond",
      `Quick,
      fun () ->
        let code =
          Code.loop (Range.make (bvar ~label:"i" "i") (nvar "N")) (access (nvar "i"))
        in
        let code', hits = Assumption.add_to_code (binder_a ~label:"i" x_lt_10) code in
        Alcotest.(check int) "one hit" 1 (List.length hits);
        Alcotest.(check bool) "loop cond carries the bexp" true
          (match binder_cond "i" code' with
          | Some c -> mem_conjunct x_lt_10 c
          | None -> false) );
    ( "matches on label, not internal name",
      `Quick,
      fun () ->
        (* the counter was renamed to i_2 but keeps label i *)
        let code =
          Code.loop
            (Range.make (bvar ~label:"i" "i_2") (nvar "N"))
            (access (nvar "i_2"))
        in
        let _, hits = Assumption.add_to_code (binder_a ~label:"i" x_lt_10) code in
        Alcotest.(check int) "matched via label" 1 (List.length hits) );
    ( "line=Any over a reused label hits every binder",
      `Quick,
      fun () ->
        let code =
          Code.decl (bvar ~label:"x" ~line:10 "x_a")
            (Code.decl (bvar ~label:"x" ~line:20 "x_b") (access (nvar "x_a")))
        in
        let _, hits = Assumption.add_to_code (binder_a ~label:"x" x_lt_10) code in
        Alcotest.(check int) "both binders hit" 2 (List.length hits) );
    ( "line=Exact pins one of the reused binders",
      `Quick,
      fun () ->
        let code =
          Code.decl (bvar ~label:"x" ~line:10 "x_a")
            (Code.decl (bvar ~label:"x" ~line:20 "x_b") (access (nvar "x_a")))
        in
        let _, hits =
          Assumption.add_to_code (binder_a ~label:"x" ~line:(M.Exact 20) x_lt_10) code
        in
        Alcotest.(check int) "one binder hit" 1 (List.length hits) );
    ( "no match leaves the code alone",
      `Quick,
      fun () ->
        let code = Code.decl (bvar "dst") (access (nvar "dst")) in
        let code', hits = Assumption.add_to_code (binder_a ~label:"zzz" x_lt_10) code in
        Alcotest.(check int) "no hits" 0 (List.length hits);
        Alcotest.(check bool) "unchanged" true (code' = code) );
    ( "Pre target is a no-op on code",
      `Quick,
      fun () ->
        let code = Code.decl (bvar "dst") (access (nvar "dst")) in
        let code', hits = Assumption.add_to_code (pre_a x_lt_10) code in
        Alcotest.(check int) "no hits" 0 (List.length hits);
        Alcotest.(check bool) "unchanged" true (code' = code) );
  ]

let add_to_kernel_tests : unit Alcotest.test_case list =
  let one_decl () = mk_kernel (Code.decl (bvar "dst") (access (nvar "dst"))) in
  let two_x () =
    mk_kernel
      (Code.decl (bvar ~label:"x" ~line:10 "x_a")
         (Code.decl (bvar ~label:"x" ~line:20 "x_b") (access (nvar "x_a"))))
  in
  (* decl x nested in loop i: i is in scope inside x *)
  let loop_decl () =
    mk_kernel
      (Code.loop
         (Range.make (bvar ~label:"i" "i") (nvar "N"))
         (Code.decl (bvar "x") (access (nvar "x"))))
  in
  [
    ( "Pre conjoins a parameter fact into the precondition",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (pre_a n_lt_10) (one_decl ()) with
        | Ok k -> Alcotest.(check bool) "pre carries bexp" true (mem_conjunct n_lt_10 k.pre)
        | Error e -> Alcotest.fail e );
    ( "Binder found conjoins onto the binder",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (binder_a ~label:"dst" dst_lt_N) (one_decl ()) with
        | Ok k ->
            Alcotest.(check bool) "binder cond carries bexp" true
              (match binder_cond "dst" k.code with
              | Some c -> mem_conjunct dst_lt_N c
              | None -> false)
        | Error e -> Alcotest.fail e );
    ( "Binder not found errors",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (binder_a ~label:"zzz" dst_lt_N) (one_decl ()) with
        | Ok _ -> Alcotest.fail "expected Error"
        | Error _ -> () );
    ( "ambiguous binder errors",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (binder_a ~label:"x" x_lt_N) (two_x ()) with
        | Ok _ -> Alcotest.fail "expected Error"
        | Error _ -> () );
    ( "kernel filter mismatch skips",
      `Quick,
      fun () ->
        let k = one_decl () in
        match
          Assumption.add_to_kernel
            (binder_a ~kernel:(M.Exact "other") ~label:"dst" x_lt_10)
            k
        with
        | Ok k' -> Alcotest.(check bool) "unchanged" true (k' = k)
        | Error e -> Alcotest.fail e );
    ( "Pre with a non-parameter name is rejected",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (pre_a zzz_lt_10) (one_decl ()) with
        | Ok _ -> Alcotest.fail "expected Error"
        | Error _ -> () );
    ( "Pre over a launch-config built-in is well-formed",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (pre_a bdimx_lt_32) (one_decl ()) with
        | Ok k ->
            Alcotest.(check bool) "pre carries bexp" true (mem_conjunct bdimx_lt_32 k.pre)
        | Error e -> Alcotest.fail e );
    ( "binder fact referencing an enclosing binder is well-formed",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (binder_a ~label:"x" x_lt_i) (loop_decl ()) with
        | Ok k ->
            Alcotest.(check bool) "cond carries bexp" true
              (match binder_cond "x" k.code with
              | Some c -> mem_conjunct x_lt_i c
              | None -> false)
        | Error e -> Alcotest.fail e );
    ( "binder fact referencing an out-of-scope name is rejected",
      `Quick,
      fun () ->
        match Assumption.add_to_kernel (binder_a ~label:"x" x_lt_zzz) (loop_decl ()) with
        | Ok _ -> Alcotest.fail "expected Error"
        | Error _ -> () );
  ]

let () =
  Alcotest.run "Assumption"
    [
      ("to_string", to_string_tests);
      ("add_to_code", add_to_code_tests);
      ("add_to_kernel", add_to_kernel_tests);
    ]
