open Protocols
open Exp

let v (name : string) : Variable.t = Variable.from_name name
let nvar (name : string) : nexp = Var (v name)
let lt (a : nexp) (b : nexp) : bexp = NRel (Lt Signedness.Signed, a, b)
let array_a : Variable.t = v "A"
let access (idx : nexp) : Code.t = Code.Access (Access.read array_a [ idx ])
let range (name : string) (ub : nexp) : Range.t = Range.make (v name) ub

let mk_kernel (code : Code.t) : Kernel.t =
  {
    name = "k_test";
    global_variables = Params.add (v "N") C_type.int Params.empty;
    local_variables = Params.empty;
    arrays = Variable.Map.empty;
    pre = Bool true;
    code;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

let rec loop_cond (name : string) : Code.t -> bexp option = function
  | Loop { cond_range; body } ->
      if Variable.name (Cond_range.var cond_range) = name then Some cond_range.cond
      else loop_cond name body
  | Decl { body; _ } -> loop_cond name body
  | If (_, p, q) | Seq (p, q) -> (
      match loop_cond name p with Some c -> Some c | None -> loop_cond name q)
  | Access _ | Sync _ | Skip -> None

let mem_conjunct (needle : bexp) (haystack : bexp) : bool =
  List.mem needle (b_and_split haystack)

(* [for i { for j { decl x [x<N && x<i && x<j] { A[x] } } }]: the
   conjunctions split and float to the innermost binder each references. *)
let test_split_and_route () =
  let x = nvar "x" and i = nvar "i" and j = nvar "j" and n = nvar "N" in
  let cond = b_and_ex [ lt x n; lt x i; lt x j ] in
  let code =
    Code.loop (range "i" n)
      (Code.loop (range "j" n) (Code.decl ~cond (v "x") (access x)))
  in
  let k = Kernel.hoist_decls (mk_kernel code) in
  Alcotest.(check bool) "x<N reaches pre" true (mem_conjunct (lt x n) k.pre);
  Alcotest.(check bool) "x<i does not reach pre" false
    (mem_conjunct (lt x i) k.pre);
  (match loop_cond "i" k.code with
  | Some c ->
      Alcotest.(check bool) "x<i lands on loop i" true (mem_conjunct (lt x i) c);
      Alcotest.(check bool) "x<j does not land on loop i" false
        (mem_conjunct (lt x j) c)
  | None -> Alcotest.fail "loop i not found");
  match loop_cond "j" k.code with
  | Some c ->
      Alcotest.(check bool) "x<j lands on loop j" true (mem_conjunct (lt x j) c)
  | None -> Alcotest.fail "loop j not found"

(* A conjunct naming two counters stops at the innermost, never rising
   past it. [x < i + j] must rest on loop j. *)
let test_stops_at_innermost () =
  let x = nvar "x" and i = nvar "i" and j = nvar "j" and n = nvar "N" in
  let cond = lt x (n_plus i j) in
  let code =
    Code.loop (range "i" n)
      (Code.loop (range "j" n) (Code.decl ~cond (v "x") (access x)))
  in
  let k = Kernel.hoist_decls (mk_kernel code) in
  Alcotest.(check bool) "x<i+j does not reach pre" false
    (mem_conjunct cond k.pre);
  (match loop_cond "j" k.code with
  | Some c ->
      Alcotest.(check bool) "x<i+j lands on loop j" true (mem_conjunct cond c)
  | None -> Alcotest.fail "loop j not found");
  match loop_cond "i" k.code with
  | Some c ->
      Alcotest.(check bool) "loop i carries no such conjunct" false
        (mem_conjunct cond c)
  | None -> Alcotest.fail "loop i not found"

let () =
  Alcotest.run "hoist_decls"
    [
      ( "routing",
        [
          Alcotest.test_case "split and route to binders" `Quick
            test_split_and_route;
          Alcotest.test_case "conjunct stops at innermost loop" `Quick
            test_stops_at_innermost;
        ] );
    ]
