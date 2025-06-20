(* allow unused names *)
[@@@warning "-32"]

(* open Stage0 *)
open Protocols
open Exp
open Drf
(* open Inference *)

open OUnit2

(* open Delinearize.Default *)

module Build = struct
  let var x = Var (Variable.from_name x)
  let ( + ) a b = Binary (Plus, a, b)
  let ( * ) a b = Binary (Mult, a, b) 
  (* let ( % ) a b = Binary (Mod, a, b) *)
  let ( / ) a b = Binary (Div, a, b)

  let ( < ) a b = NRel (Lt, a, b)

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

(* let ex = let open Build in
  (((((var "blockIdx.y" * var "BM") + (((var "threadIdx.x" / num 32) / (var "BN" / var "WN")) * var "WM")) * var "N") + (var "blockIdx.x" * var "BN")) + (((var "threadIdx.x" / num 32) % (var "BN" / var "WN")) * var "WN")) + ((((var "wSubRowIdx2" * (var "WM" / ((var "WM" * var "WN") / (((num 32 * var "TM") * var "TN") * var "WNITER")))) * var "N") + (var "wSubColIdx2" * (var "WN" / var "WNITER"))) + (((((((var "threadIdx.x" % num 32) / ((var "WN" / var "WNITER") / var "TN")) * var "TM") + var"resIdxM1") * var "N") + (((var "threadIdx.x" % num 32) % ((var "WN" / var "WNITER") / var "TN")) * var "TN")) + var "resIdxN1"))
let ex_globals: Variable.Set.t = Exp.n_free_names ex Variable.Set.empty
  |> Variable.Set.filter (fun v -> Char.uppercase_ascii v.name.[0] == v.name.[0])

let _ = ex
  |> Delinearize.Expr.from_nexp ~globals:ex_globals
  |> Delinearize.Expr.to_list
  |> List.map Delinearize.Term.to_string
  |> List.iter print_endline;
  print_endline "" *)

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
    "constant", num 1, [num 1], [], []; 
    "linear", x, [x], [], []; 
    "affine", x + num 1, [x + num 1], [], []; 
    (* we may want to make this a dimension? *)
    "constdim", num 10 * x + y, [num 10 * x + y], [], [];
    "numdim", vN * x + y, [x; y], [vN], [y < vN];
    "numdim_scaled", num 10 * vN * x + y, [x; y], [num 10 * vN], [y < num 10 * vN];
    "numdim_nested", num 10 * (vM * x + y) + z,
      [x; num 10 * y + z], [num 10 * vM], [num 10 * y + z < num 10 * vM];
    "3dim", vM * vN * x + vN * y + z,
      [x; y; z], [vM; vN], [y < vM; z < vN];
    "3dim+dist", vN * (vM * x + y) + z,
      [x; y; z], [vM; vN], [y < vM; z < vN]; 
    "duplicate", vN * (x + y) + z,
      [x + y; z], [vN], [z < vN];
  ] |> List.map (fun (name, before, indices, dims, conditions) -> name, before, Some {
    indices = List.map normalize indices;
    dims = List.map normalize dims;
    conditions
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
  let loop (var: string) (body: Aligned.Code.t): Aligned.Code.t = Loop {
    range = {
      var = Variable.from_name var;
      ty = C_type.int;
      dir = Range.Increase;
      lower_bound = Num 0;
      upper_bound = Num 0;
      step = Range.Step.Plus (Num 1)
    };
    body
  } in
  [
    "trivial", [], Sync Skip, Sync Skip;
    "3dim+param", ["m"; "n"],
      Sync (acc aA [m * n * x + n * y + z]),
      Sync (acc aA [x; y; z]);
    "3dim+loop", [],
      loop "m" (loop "n" (Sync (acc aA [m * n * x + n * y + z]))),
      loop "m" (loop "n" (Sync (acc aA  [x; y; z])));
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

(* let tests = "examples" >::: [
  (* "expr representation" >:: (fun _ -> 
    let x = var "x"
    let y = var "x"
    let z = var "x"
    let term l = l |> List.map (fun (a, e) -> Expr.Atom.from_nexp ~globals a, e) |> Expr.Term.of_factors in
    let t1 = x + y in
    let t2 = y + x in
  ); *)
  "freaky code - slice7dgrad" >:: (fun _ -> 
    let open Delinearize in
    let open Build in
    let pos = var "pos" in
    let l6 = var "l6" in
    let l7 = var "l7" in
    let d6 = var "d6" in
    let d7 = var "d7" in
    let globals = Variable.Set.of_list (["d6"; "d7"] |> List.map Variable.from_name) in
    let term l = l |> List.map (fun (a, e) -> Expr.Atom.from_nexp ~globals a, e) |> Expr.Term.of_factors in
    let t1 = term [
        pos, 1
      ] in
    let t2 = term [
        pos / l7, 1;
        d7, 1;
      ] in
    let t3 = term [
        (pos / (l6 * l7)), 1;
        d6, 1;
        d7, 1;
      ] in
    [t1; t2; t3] |> List.iter (fun a -> 
      [t1; t2; t3] |> List.iter (fun b ->
        Printf.printf "%3d " (Term.compare a b);
      );
      print_newline ()
    );
    let expr1 = Expr.of_list [t1; t2; t3] in
    let expr2 = Expr.of_list [t3; t2; t1] in
    assert_equal
      0
      (Expr.compare expr1 expr2);
    (* failwith "aaa" *)
      (* [pos / (l6 * l7); pos / l7; pos], [d6; d7], [pos / l7 < d6; pos < d7]; *)
  );
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
      dim_examples |> List.iter (fun name, exp, dim) ->
        assert_equal 
          ~msg:name
          ~printer:(Exp.n_to_string |> string_of_list |> string_of_option)
          (Some dim)
          Delinearize.(exp
            |> Expr.from_nexp ~globals
            |> size_params
            |> dims
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
          (from_nexp ~globals before)
      )
    );
    "full kernels" >:: (fun _ ->
      kernels |> List.iter (fun (name, before, after) ->
        assert_equal
          ~msg:name
          ~printer:Aligned.Kernel.to_string
          after 
          (rewrite_kernel before)
      )
    );
] *)

let quicktests = let open QCheck_ounit in
  to_ounit2_test
    QCheck2.(Test.make ~count:1000
            ~print:Print.(list int)
            Gen.(list int)
            (fun l -> List.rev (List.rev l) = l))

(* let _ = run_test_tt_main tests *)
let _ = run_test_tt_main quicktests