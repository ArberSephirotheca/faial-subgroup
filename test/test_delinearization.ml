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

  let x = var "x"
  let y = var "y"
  let z = var "z"
  let vM = var "M"
  let vN = var "N"
end

let normalize (e : nexp): nexp = e |> Delinearize.Expr.from_nexp |> Delinearize.Expr.to_nexp


let size_param_examples: (string * nexp * nexp list) list = 
  let open Build in [
    "constant", num 1, []; 
    "linear", x, []; 
    "affine", x + num 1, []; 
    "constdim", num 10 * x + y, []; 
    "numdim", vN * x + y,
      [vN * x];
    "3dim", var "M" * vN * x + vN * y + z,
      [vM * vN * x; vN * y]; 
    "3dim+dist", vN * (vM * x + y) + z,
      [vM * vN * x; vN * y]; 
  ] |> List.map (fun (l, b, a) -> l, b, List.map normalize a)

let dim_examples: (string * nexp * nexp list) list = 
  let open Build in [
    "constant", num 1, []; 
    "linear", x, []; 
    "affine", x + num 1, []; 
    (* "constdim", num 10 * x + y, [num 10];  *)
    "numdim", vN * x + y,
      [vN];
    "3dim", var "M" * vN * x + vN * y + z,
      [vM; vN]; 
    "3dim+dist", vN * (vM * x + y) + z,
      [vM; vN]; 
  ] |> List.map (fun (l, b, a) -> l, b, List.map normalize a)


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

let string_of_list (f : 'a -> string) (l : 'a list) : string = l
  |> List.map f
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let tests = "examples" >::: [
  "collect parameters (stage1)" >:: (fun _ -> 
    size_param_examples |> List.iter (fun (name, exp, params) ->
      assert_equal
        ~msg:name
        ~printer:(string_of_list Exp.n_to_string)
        params
        Delinearize.(exp
          |> Expr.from_nexp
          |> size_params
          |> List.map Delinearize.Term.to_nexp
        )
    )
  );
  "derive dimensionality/size (stage2)" >:: (fun _ -> 
    dim_examples |> List.iter (fun (name, exp, dim) ->
      assert_equal
        ~msg:name
        ~printer:(Exp.n_to_string |> string_of_list |> string_of_option)
        (Some dim)
        Delinearize.(exp
          |> Expr.from_nexp
          |> size_params
          |> Delinearize.dims
          |> Option.map (List.map Delinearize.Term.to_nexp)
        )
    )
  );
  "positive examples (stage3)" >:: (fun _ ->
    positive_examples |> List.iter (fun (name, before, res) -> 
      assert_equal
        ~msg:name
        ~printer:(string_of_option Delinearize.to_string)
        res 
        (Delinearize.from_nexp before)
    )
  )    
]

let _ = run_test_tt_main tests