open Protocols
open Protocols.Exp

let var (name : string) : nexp = Var (Variable.from_name name)
let x = var "x"
let y = var "y"

let nexp = Alcotest.testable (Fmt.of_to_string n_to_string) n_equal

let check_call (name : string) (args : nexp list) (expected : nexp option) () =
  Alcotest.(check (option nexp))
    (name ^ "/" ^ string_of_int (List.length args))
    expected
    (Functions.call_opt name args)

(* An entry with a body lowers at every application, not only where
   the arguments happen to be literal. Gating on literals leaves the
   call an uninterpreted function whose result no declaration
   describes, which is the false alarm declarations exist to close. *)
let rewrite_tests =
  [
    ("min of two symbols", `Quick,
     check_call "min" [ x; y ] (Some (n_if (n_lt x y) x y)));
    ("max of two symbols", `Quick,
     check_call "max" [ x; y ] (Some (n_if (n_gt x y) x y)));
    ("min of a symbol and a literal", `Quick,
     check_call "min" [ x; Num 3 ] (Some (n_if (n_lt x (Num 3)) x (Num 3))));
    ("min of two literals folds", `Quick,
     check_call "min" [ Num 3; Num 4 ] (Some (Num 3)));
    ("max of two literals folds", `Quick,
     check_call "max" [ Num 3; Num 4 ] (Some (Num 4)));
    ("divUp by one is the identity", `Quick,
     check_call "divUp" [ x; Num 1 ] (Some x));
    ("divUp of two literals folds", `Quick,
     check_call "divUp" [ Num 7; Num 2 ] (Some (Num 4)));
  ]

(* An uninterpreted entry keeps its symbol unless every argument is a
   literal. The folded value has to agree with the intrinsic on the
   operand width it actually reads, so a literal wider than that
   width, or a negative one, folds through the truncation. *)
let fold_tests =
  [
    ("__clz keeps its symbol on a symbol", `Quick,
     check_call "__clz" [ x ] None);
    ("__clz of zero", `Quick, check_call "__clz" [ Num 0 ] (Some (Num 32)));
    ("__clz of one", `Quick, check_call "__clz" [ Num 1 ] (Some (Num 31)));
    ("__clz of a negative sets the top bit", `Quick,
     check_call "__clz" [ Num (-1) ] (Some (Num 0)));
    ("__clz truncates an operand wider than 32 bits", `Quick,
     check_call "__clz" [ Num (Stage0.Common.pow ~base:2 32) ] (Some (Num 32)));
    ("__ffs of zero", `Quick, check_call "__ffs" [ Num 0 ] (Some (Num 0)));
    ("__ffs of eight", `Quick, check_call "__ffs" [ Num 8 ] (Some (Num 4)));
    ("__ffs truncates an operand wider than 32 bits", `Quick,
     check_call "__ffs" [ Num (Stage0.Common.pow ~base:2 32) ] (Some (Num 0)));
    ("__popc of zero", `Quick, check_call "__popc" [ Num 0 ] (Some (Num 0)));
    ("__popc of a byte", `Quick,
     check_call "__popc" [ Num 255 ] (Some (Num 8)));
    ("__popc of a negative counts 32 bits", `Quick,
     check_call "__popc" [ Num (-1) ] (Some (Num 32)));
    ("__clzll counts over a 64-bit word", `Quick,
     check_call "__clzll" [ Num 1 ] (Some (Num 63)));
    ("__ffsll of eight", `Quick,
     check_call "__ffsll" [ Num 8 ] (Some (Num 4)));
    ("__popcll of a byte", `Quick,
     check_call "__popcll" [ Num 255 ] (Some (Num 8)));
    ("__umulhi keeps its symbol on literals too", `Quick,
     check_call "__umulhi" [ Num 2; Num 3 ] (Some (NCall ("__umulhi", [ Num 2; Num 3 ]))));
  ]

(* A registered name applied at another arity is a different function.
   Its body would raise and its declaration would describe the wrong
   graph, so the entry must not govern the application at all. *)
let arity_tests =
  [
    ("min at arity one", `Quick, check_call "min" [ x ] None);
    ("min at arity three", `Quick, check_call "min" [ x; y; x ] None);
    ("__clz at arity two", `Quick, check_call "__clz" [ x; y ] None);
    ("an unregistered name", `Quick, check_call "notAFunction" [ x ] None);
    ("no declaration at the wrong arity", `Quick,
     fun () ->
       Alcotest.(check bool)
         "__clz(x, y) carries no postcondition" true
         (Functions.postcondition (NCall ("__clz", [ x; y ])) = None));
    (* Every entry's body accepts the arity the entry declares. *)
    ("every body accepts its declared arity", `Quick,
     fun () ->
       Functions.all
       |> List.iter (fun (e : Functions.t) ->
           let args = List.init e.arity (fun i -> var ("a" ^ string_of_int i)) in
           match e.body args with
           | _ -> ()
           | exception ex ->
               Alcotest.failf "%s at arity %d raised: %s" e.name e.arity
                 (Printexc.to_string ex)));
  ]

(* A claim is instantiated at the applications occurring in the goal,
   one fact per application, and an application nested inside another
   is one of them. *)
let postcondition_tests =
  [
    ("a range is asserted at the application", `Quick,
     fun () ->
       let goal = n_eq (NCall ("__clz", [ x ])) (Num 7) in
       Alcotest.(check bool)
         "the goal grew a fact" true
         (not (b_equal goal (Functions.add_postconditions goal))));
    ("an entry with a body asserts nothing", `Quick,
     fun () ->
       let goal = n_eq (NCall ("min", [ x; y ])) (Num 7) in
       Alcotest.(check bool)
         "the goal is unchanged" true
         (b_equal goal (Functions.add_postconditions goal)));
    ("a nested application gets its own fact", `Quick,
     fun () ->
       let inner = NCall ("__ffs", [ x ]) in
       let goal = n_eq (NCall ("__clz", [ inner ])) (Num 7) in
       let facts = b_calls goal |> List.filter_map Functions.postcondition in
       Alcotest.(check int) "one fact per application" 2 (List.length facts));
    ("repeated applications are instantiated once", `Quick,
     fun () ->
       let c = NCall ("__clz", [ x ]) in
       let goal = n_eq (n_plus c c) (Num 7) in
       let facts = b_calls goal |> List.filter_map Functions.postcondition in
       Alcotest.(check int) "one fact" 1 (List.length facts));
  ]

(* A range whose ends are swapped is a contradiction, and a
   contradiction in a proof goal reports data-race freedom. Both
   builders refuse rather than emit one. *)
let range_tests =
  [
    ("an empty interval is refused", `Quick,
     fun () ->
       Alcotest.check_raises "in_range 1 0" (Invalid_argument
         "in_range: empty interval 1..0")
         (fun () -> ignore (Functions.in_range 1 0)));
    ("a width a Num cannot hold is refused", `Quick,
     fun () ->
       Alcotest.check_raises "unsigned_bits 64" (Invalid_argument
         "unsigned_bits: a 64-bit range does not fit in Num")
         (fun () -> ignore (Functions.unsigned_bits 64)));
    ("a 32-bit width is accepted", `Quick,
     fun () -> ignore (Functions.unsigned_bits 32));
  ]

let all_tests =
  [
    ("bodies", rewrite_tests);
    ("concrete folding", fold_tests);
    ("arity", arity_tests);
    ("postconditions", postcondition_tests);
    ("range builders", range_tests);
  ]

let () = Alcotest.run "Functions" all_tests
