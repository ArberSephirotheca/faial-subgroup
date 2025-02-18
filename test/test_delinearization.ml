(* open Stage0 *)
open Protocols
open Exp
open Drf
(* open Inference *)

open OUnit2

module Build = struct
  let var x = Var (Variable.from_name x)
  let ( + ) a b = Binary (Plus, a, b)
  let ( * ) a b = Binary (Mult, a, b) 
end

let positive_examples: (string * nexp * (Delinearize.t option)) list = 
  let open Build in
  let open Delinearize in [
    "constant", num 1, [num 1], [];
    "linear", var "x", [var "x"], [];
    "affine", var "x" + num 1, [var "x" + num 1], [];
    "constdim", num 10 * var "x" + var "y", [var "x"; var "y"], [num 10];
    "numdim", var "N" * var "x" + var "y", [var "x"; var "y"], [var "N"];
  ] |> List.map (fun (name, before, after, dims) -> name, before, Some {
    indices = after;
    dims;
  })

let string_of_option f o =
  match o with
  | Some x -> f x
  | None -> "null"

let tests = "examples" >::: [
  "positive examples" >:: (fun _ ->
    positive_examples |> List.iter (fun (name, before, res) -> 
      assert_equal
        ~printer:(string_of_option Delinearize.to_string)
        ~msg:name
        res 
        (Delinearize.from_nexp before)
    )
  )    
]

let _ = run_test_tt_main tests