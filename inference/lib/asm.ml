open Stage0

type json = Yojson.Basic.t
type 'a j_result = 'a Rjson.j_result

type 'a operand = { constr : string; expr : 'a }

type 'a t = {
  asm_string : string;
  is_volatile : bool;
  outputs : 'a operand list;
  inputs : 'a operand list;
  clobbers : string list;
}

let map_operand (f : 'a -> 'b) (o : 'a operand) : 'b operand =
  { constr = o.constr; expr = f o.expr }

let map_expr (f : 'a -> 'b) (a : 'a t) : 'b t =
  {
    asm_string = a.asm_string;
    is_volatile = a.is_volatile;
    outputs = List.map (map_operand f) a.outputs;
    inputs = List.map (map_operand f) a.inputs;
    clobbers = a.clobbers;
  }

let parse (parse_expr : json -> 'a j_result) (j : json) : 'a t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* asm_string = with_field "asmString" cast_string o in
  let* is_volatile = with_field_or "isVolatile" cast_bool false o in
  let* num_outputs = with_field_or "numOutputs" cast_int 0 o in
  let* num_inputs = with_field_or "numInputs" cast_int 0 o in
  let* constraints =
    with_field_or "constraints" (cast_map cast_string) [] o
  in
  let* clobbers = with_field_or "clobbers" (cast_map cast_string) [] o in
  let* exprs = with_field_or "inner" (cast_map parse_expr) [] o in
  let expected = num_outputs + num_inputs in
  if List.length constraints <> expected then
    root_cause
      (Printf.sprintf
         "GCCAsmStmt: expected %d constraints (numOutputs=%d + numInputs=%d), \
          got %d"
         expected num_outputs num_inputs (List.length constraints))
      j
  else if List.length exprs <> expected then
    root_cause
      (Printf.sprintf
         "GCCAsmStmt: expected %d operand expressions (numOutputs=%d + \
          numInputs=%d), got %d"
         expected num_outputs num_inputs (List.length exprs))
      j
  else
    let operands =
      List.map2 (fun constr expr -> { constr; expr }) constraints exprs
    in
    let outputs, inputs =
      let rec split n xs =
        if n = 0 then ([], xs)
        else
          match xs with
          | [] -> ([], [])
          | x :: rest ->
              let hd, tl = split (n - 1) rest in
              (x :: hd, tl)
      in
      split num_outputs operands
    in
    Ok { asm_string; is_volatile; outputs; inputs; clobbers }

let to_string (expr_to_string : 'a -> string) (a : 'a t) : string =
  let operand_to_string (o : 'a operand) : string =
    "\"" ^ o.constr ^ "\"(" ^ expr_to_string o.expr ^ ")"
  in
  let vol = if a.is_volatile then "volatile " else "" in
  let sections =
    match (a.outputs, a.inputs, a.clobbers) with
    | [], [], [] -> ""
    | _ ->
        let outs = List.map operand_to_string a.outputs |> String.concat ", " in
        let ins = List.map operand_to_string a.inputs |> String.concat ", " in
        let clb =
          List.map (fun c -> "\"" ^ c ^ "\"") a.clobbers |> String.concat ", "
        in
        " : " ^ outs ^ " : " ^ ins ^ " : " ^ clb
  in
  "asm " ^ vol ^ "(\"" ^ String.escaped a.asm_string ^ "\"" ^ sections ^ ")"
