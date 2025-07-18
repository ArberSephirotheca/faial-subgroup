open OUnit2
open Protocols
open Exp
open Bank_conflicts
open Bank_conflicts.Symbolic_metric_analysis

(* Factory function for Config objects *)
let make_config (threads_per_warp: int) : Config.t =
  Config.make 
    ~threads_per_warp
    ~block_dim:(Dim3.make ~x:1 ())
    ~grid_dim:(Dim3.make ~x:1 ())
    ()

(* Utility function to create variables *)
let var_ (name: string) : nexp = Var (Variable.from_name name)

(* Pretty printer for (bexp * nexp) list *)
let pp_replicate_result (result: (bexp * nexp) list) : string =
  let pp_pair (b, n) = 
    Printf.sprintf "(%s, %s)" (b_to_string b) (n_to_string n) in
  "[" ^ (String.concat "; " (List.map pp_pair result)) ^ "]"

(* Utility function that wraps replicate and provides better error messages *)
let assert_replicate 
  ~expected:(expected: (bexp * nexp) list) 
  ~threads_per_warp:(threads_per_warp: int)
  ~locals:(locals: Variable.Set.t)
  ~cond:(cond: bexp)
  ~index:(index: nexp)
  : unit =
  let cfg = make_config threads_per_warp in
  let result = replicate cfg locals cond index in
  assert_equal 
    ~printer:pp_replicate_result
    expected result

(* Utility function that wraps encode_ua and provides better error messages *)
let assert_encode_ua
  ~expected:(expected: nexp)
  ~threads_per_warp:(threads_per_warp: int)
  ~locals:(locals: Variable.Set.t)
  ~cond:(cond: bexp)
  ~index:(index: nexp)
  : unit =
  let cfg = make_config threads_per_warp in
  let result = encode_ua cfg locals cond index in
  assert_equal 
    ~printer:n_to_string
    expected result

(* Utility function that wraps ua and provides better error messages *)
let assert_ua
  ~expected:(expected: int option)
  ~threads_per_warp:(threads_per_warp: int)
  ~locals:(locals: Variable.Set.t)
  ~cond:(cond: bexp)
  ~index:(index: nexp)
  : unit =
  let cfg = make_config threads_per_warp in
  let result = ua cfg locals cond index in
  let printer = function
    | Some x -> string_of_int x
    | None -> "none"
  in
  assert_equal 
    ~printer
    expected result

let tests = "test_symbolic_metric_analysis" >::: [
  "replicate_empty_locals" >:: (fun _ ->
    (* Test with empty locals, cfg with 2 threads_per_warp *)
    assert_replicate
      ~expected:[(b_true, Num 0); (b_true, Num 0)]
      ~threads_per_warp:2
      ~locals:Variable.Set.empty
      ~cond:b_true
      ~index:(Num 0)
  );
  
  "replicate_with_local_variable" >:: (fun _ ->
    (* Test with local variable x *)
    let x = Variable.from_name "x" in
    let locals = Variable.Set.singleton x in
    assert_replicate
      ~expected:[
        (b_true, var_ "x$1");
        (b_true, var_ "x$2")
      ]
      ~threads_per_warp:2
      ~locals:locals
      ~cond:b_true
      ~index:(var_ "x")
  );
  
  "encode_ua_empty_locals" >:: (fun _ ->
    (* Test encode_ua with empty locals, 2 threads accessing same index *)
    assert_encode_ua
      ~expected:(Num 1)
      ~threads_per_warp:2
      ~locals:Variable.Set.empty
      ~cond:b_true
      ~index:(Num 0)
  );
  
  "encode_ua_with_local_variable" >:: (fun _ ->
    (* Test encode_ua with local variable x *)
    let x = Variable.from_name "x" in
    let locals = Variable.Set.singleton x in
    let expected = n_plus (Num 1) (NIf (n_neq (var_ "x$2") (var_ "x$1"), Num 1, Num 0)) in
    assert_encode_ua
      ~expected
      ~threads_per_warp:2
      ~locals:locals
      ~cond:b_true
      ~index:(var_ "x")
  );
  
  "ua_empty_locals" >:: (fun _ ->
    (* Test ua with empty locals, 2 threads accessing same index *)
    assert_ua
      ~expected:(Some 1)
      ~threads_per_warp:2
      ~locals:Variable.Set.empty
      ~cond:b_true
      ~index:(Num 0)
  );
  
  "ua_with_local_variable" >:: (fun _ ->
    (* Test ua with local variable x *)
    let x = Variable.from_name "x" in
    let locals = Variable.Set.singleton x in
    assert_ua
      ~expected:(Some 2)
      ~threads_per_warp:2
      ~locals:locals
      ~cond:b_true
      ~index:(var_ "x")
  );
]

let _ = run_test_tt_main tests