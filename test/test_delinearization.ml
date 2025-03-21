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
  let m = var "m"
  let n = var "n"
  let vM = var "M"
  let vN = var "N"
  let aA = Variable.from_name "A" 
end

let globals = Variable.Set.of_list (["M"; "N"] |> List.map Variable.from_name)

let normalize (e : nexp): nexp = e |> Delinearize.Expr.from_nexp ~globals |> Delinearize.Expr.to_nexp


let size_param_examples: (string * nexp * nexp list) list = 
  let open Build in [
    "constant", num 1, []; 
    "linear", x, []; 
    "affine", x + num 1, []; 
    "constdim", num 10 * x + y, []; 
    "numdim", vN * x + y,
      [vN];
    "3dim", vM * vN * x + vN * y + z,
      [vM * vN; vN]; 
    "3dim+dist", vN * (vM * x + y) + z,
      [vM * vN; vN]; 
    "duplicate", vN * (x + y) + z,
      [vN];
  ] |> List.map (fun (l, b, a) -> l, b, List.map normalize a)

let dim_examples: (string * nexp * nexp list) list = 
  let open Build in [
    "constant", num 1, []; 
    "linear", x, []; 
    "affine", x + num 1, []; 
    "constdim", num 10 * x + y, []; (* we may want to make this a dimension? *)
    "numdim", vN * x + y, [vN];
    "numdim_scaled", num 10 * vN * x + y, [num 10 * vN];
    "numdim_nested", num 10 * (vM * x + y) + z, [num 10 * vM];
    "3dim", var "M" * vN * x + vN * y + z,
      [vM; vN]; 
    "3dim+dist", vN * (vM * x + y) + z,
      [vM; vN]; 
    "duplicate", vN * (x + y) + z,
      [vN];
  ] |> List.map (fun (l, b, a) -> l, b, List.map normalize a)


let positive_examples: (string * nexp * (Delinearize.t option)) list = 
  let open Build in
  let open Delinearize in [
    "constant", num 1, [num 1], []; 
    "linear", x, [x], []; 
    "affine", x + num 1, [x + num 1], []; 
    "constdim", num 10 * x + y, [num 10 * x + y], []; (* we may want to make this a dimension? *)
    "numdim", vN * x + y, [x; y], [vN];
    "numdim_scaled", num 10 * vN * x + y, [x; y], [num 10 * vN];
    "numdim_nested", num 10 * (vM * x + y) + z,
      [x; num 10 * y +  z], [num 10 * vM];
    "3dim", vM * vN * x + vN * y + z,
      [x; y; z], [vM; vN]; 
    "3dim+dist", vN * (vM * x + y) + z,
      [x; y; z], [vM; vN]; 
    "duplicate", vN * (x + y) + z,
      [x + y; z], [vN];
  ] |> List.map (fun (name, before, indices, dims) -> name, before, Some {
    indices = List.map normalize indices;
    dims = List.map normalize dims;
  })

let make_kernel ((name: string), (globals: string list), (before: Aligned.Code.t), (after: Aligned.Code.t)): string * Aligned.Kernel.t * Aligned.Kernel.t = 
  let kernel: Aligned.Kernel.t = {
    name = "";
    global_variables = globals |> List.map (fun name -> (Variable.from_name name, C_type.int)) |> Params.from_list;
    local_variables = Params.empty;
    arrays = Variable.Map.empty;
    pre = Exp.b_true;
    code = before;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None
  } in
  name, kernel, { kernel with code = after }

let kernels: (string * Aligned.Kernel.t * Aligned.Kernel.t) list = 
  let open Aligned.Code in
  let open Build in 
  let acc array index = Unsync.Access { array; index; mode = (Write None) } in
  [
    "trivial", [], Sync Skip, Sync Skip;
    "3dim+param", ["m"; "n"],
    Sync (acc aA [m * n * x + n * y + z]),
    Sync (acc aA [x; y; z])
  ] |> List.map make_kernel

(* let  *)

let string_of_option (f : 'a -> string) (o : 'a option) =
  match o with
  | Some x -> f x
  | None -> "null"

let string_of_list (f : 'a -> string) (l : 'a list) : string = l
  |> List.map f
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let tests = "examples" >::: [
  "collect parameters (stage1)" >:: (fun _ -> 
    size_param_examples |> List.iter (fun (msg, exp, params) ->
      assert_equal
        ~msg
        ~printer:(string_of_list Exp.n_to_string)
        params
        Delinearize.(exp
          |> Expr.from_nexp ~globals
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
          |> Expr.from_nexp ~globals
          |> size_params
          |> Delinearize.dims
          |> Option.map (List.map Delinearize.Term.to_nexp)
        )
    )
  );
  "positive examples (stage3)" >:: (fun _ ->
    positive_examples |> List.iter (fun (name, before, after) -> 
      assert_equal
        ~msg:name
        ~printer:(string_of_option Delinearize.to_string)
        after 
        (Delinearize.from_nexp ~globals before)
    )
  );
  "full kernels" >:: (fun _ ->
    kernels |> List.iter (fun (name, before, after) ->
      assert_equal
        ~msg:name
        ~printer:Aligned.Kernel.to_string
        after 
        (Delinearize.rewrite_kernel before)
    )
  );
]

let _ = run_test_tt_main tests