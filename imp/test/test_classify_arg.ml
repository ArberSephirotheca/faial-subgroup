open Protocols
open Exp
open Imp

let var (name : string) : Variable.t = Variable.from_name name
let arrays (names : string list) : Variable.Set.t =
  names |> List.map var |> Variable.Set.of_list

let plus (l : nexp) (r : nexp) : nexp =
  Binary (N_binary.Plus Signedness.Signed, l, r)

let memory : Memory.t =
  {
    hierarchy = Mem_hierarchy.GlobalMemory;
    size = [];
    data_type = [ "int" ];
    layout = None;
  }

let arg_testable : Arg.t Alcotest.testable =
  Alcotest.testable
    (fun fmt (a : Arg.t) -> Format.fprintf fmt "%s" (Arg.to_string a))
    ( = )

let check_classify (name : string) ?(known = []) (ty : Kernel.Parameter.Type.t)
    (given : nexp) (expected : Arg.t) =
  ( name,
    `Quick,
    fun () ->
      Alcotest.check arg_testable name expected
        (Classify_arg.classify ~arrays:(arrays known) ty given) )

(* The parameter decides which of the three shapes an argument takes, so
   the same expression classifies differently under different parameters. *)
let classify_tests =
  [
    check_classify "array parameter, bare variable"
      (Array memory) (Var (var "A"))
      (Arg.Array { array = var "A"; offset = Num 0 });
    check_classify "array parameter, offset from a known array" ~known:[ "A" ]
      (Array memory)
      (plus (Var (var "A")) (Var (var "i")))
      (Arg.Array { array = var "A"; offset = Var (var "i") });
    check_classify "array parameter, known array on the right" ~known:[ "A" ]
      (Array memory)
      (plus (Var (var "i")) (Var (var "A")))
      (Arg.Array { array = var "A"; offset = Var (var "i") });
    (* Neither operand names memory the caller knows, so the left is kept:
       that is where a pointer conventionally sits. The access is dropped
       downstream by [filter_locs] if the name is not an array after all. *)
    check_classify "array parameter, neither operand is known memory"
      (Array memory)
      (plus (Var (var "p")) (Var (var "i")))
      (Arg.Array { array = var "p"; offset = Var (var "i") });
    check_classify "array parameter, expression addresses nothing"
      (Array memory) (Num 3)
      (Arg.Unsupported (Kernel.Parameter.Type.to_c_type (Array memory)));
    check_classify "scalar parameter" (Scalar Ty.int) (Num 3) (Arg.Scalar (Num 3));
    (* An enum parameter is an integer with a constrained range, so it
       binds by value like any other scalar. *)
    check_classify "enum parameter"
      (Enum { var = var "colour"; constants = [] })
      (Var (var "e"))
      (Arg.Scalar (Var (var "e")));
    (* A vector parameter is unsupported as a whole, and the inliner binds
       it lane by lane, so the variable is carried through as a scalar. *)
    check_classify "vector parameter, bare variable"
      (Unsupported (Ty.of_c_string "uint3"))
      (Var (var "v"))
      (Arg.Scalar (Var (var "v")));
    check_classify "vector parameter, not a variable"
      (Unsupported (Ty.of_c_string "uint3"))
      (Num 0)
      (Arg.Unsupported (Ty.of_c_string "uint3"));
    (* A mutable reference names storage the substrate cannot express, so
       it stays unsupported rather than binding by value. *)
    check_classify "mutable reference parameter"
      (Unsupported (Ty.of_c_string "int &"))
      (Var (var "i"))
      (Arg.Unsupported (Ty.of_c_string "int &"));
  ]

let check_from_nexp (name : string) ?(known = []) (given : nexp)
    (expected : Array_use.t option) =
  ( name,
    `Quick,
    fun () ->
      Alcotest.(check (option (of_pp (fun fmt a ->
          Format.fprintf fmt "%s" (Array_use.to_string a)))))
        name expected
        (Array_use.from_nexp ~arrays:(arrays known) given) )

let from_nexp_tests =
  [
    check_from_nexp "a variable addresses itself" (Var (var "A"))
      (Some { array = var "A"; offset = Num 0 });
    (* [Exp.n_plus] folds, so the accumulated offset arrives constant. *)
    check_from_nexp "a nested sum accumulates the offset" ~known:[ "A" ]
      (plus (plus (Var (var "A")) (Num 1)) (Num 2))
      (Some { array = var "A"; offset = Num 3 });
    check_from_nexp "a nested sum keeps a symbolic offset" ~known:[ "A" ]
      (plus (plus (Var (var "A")) (Var (var "i"))) (Var (var "j")))
      (Some { array = var "A"; offset = Exp.n_plus (Var (var "i")) (Var (var "j")) });
    check_from_nexp "a literal addresses nothing" (Num 7) None;
    check_from_nexp "a product addresses nothing"
      (Binary (N_binary.Mult Signedness.Signed, Var (var "A"), Num 2))
      None;
  ]

let () =
  Alcotest.run "Classify_arg"
    [ ("classify", classify_tests); ("from_nexp", from_nexp_tests) ]
