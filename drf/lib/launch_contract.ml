open Protocols
open Exp

type t = {
  row_id : string;
  family : Launch_contract_rows.family;
  manifest_kernel : string;
  parsed_kernel : string;
  template_param : string;
  template_value : int;
}

type error =
  | Unknown_row of string
  | Kernel_mismatch of { expected : string; actual : string }
  | Conflicting_param of { key : string; expected : int; actual : int }
  | Conflicting_block_dim of { expected : Dim3.t; actual : Dim3.t }
  | Grid_dim_unsupported of Dim3.t
  | All_dims_unsupported of string
  | Missing_kernel_selection of { row_id : string; expected : string }
  | Subgroup_kernel_unsupported of string
  | Duplicate_row of string

let error_to_string : error -> string = function
  | Unknown_row row_id -> "unknown launch contract row '" ^ row_id ^ "'"
  | Duplicate_row row_id ->
      "duplicate launch contract row '" ^ row_id ^ "'"
  | Kernel_mismatch { expected; actual } ->
      "launch contract expects parsed kernel '" ^ expected ^ "', got '" ^ actual
      ^ "'"
  | Conflicting_param { key; expected; actual } ->
      "launch contract expects parameter " ^ key ^ "=" ^ string_of_int expected
      ^ ", got " ^ string_of_int actual
  | Conflicting_block_dim { expected; actual } ->
      "launch contract expects --block-dim " ^ Dim3.to_string expected
      ^ ", got " ^ Dim3.to_string actual
  | Grid_dim_unsupported actual ->
      "launch contract carries symbolic gridDim.x; do not pass concrete \
       --grid-dim " ^ Dim3.to_string actual
  | All_dims_unsupported row_id ->
      "launch contract '" ^ row_id
      ^ "' requires concrete blockDim and symbolic gridDim; do not pass \
         --all-dims"
  | Missing_kernel_selection { row_id; expected } ->
      "launch contract '" ^ row_id ^ "' requires --kernel " ^ expected
  | Subgroup_kernel_unsupported kernel ->
      "launch contract is currently ordinary-DRF only, got subgroup/matrix \
       kernel '" ^ kernel ^ "'"

let of_row (row : Launch_contract_rows.t) : t =
  {
    row_id = row.row_id;
    family = row.family;
    manifest_kernel = row.manifest_kernel;
    parsed_kernel = row.parsed_kernel;
    template_param = row.template_param;
    template_value = row.template_value;
  }

let all : t list = List.map of_row Launch_contract_rows.all

let of_row_id (row_id : string) : (t, error) result =
  match List.filter (fun c -> String.equal c.row_id row_id) all with
  | [ contract ] -> Ok contract
  | [] -> Error (Unknown_row row_id)
  | _ -> Error (Duplicate_row row_id)

let block_dim (contract : t) : Dim3.t =
  match contract.family with
  | Gla | Wkv -> Dim3.make ~x:contract.template_value ()

let required_params (contract : t) : (string * int) list =
  match contract.family with
  | Gla | Wkv -> [ (contract.template_param, contract.template_value) ]

let var (name : string) : nexp = Var (Variable.from_name name)

let row_shape_precondition (contract : t) : bexp =
  let positive name = n_gt (var name) (Num 0) in
  b_and_ex
    [
      n_eq (var contract.template_param) (Num contract.template_value);
      n_eq (Var Variable.bdim_x) (Num contract.template_value);
      n_eq (Var Variable.bdim_y) (Num 1);
      n_eq (Var Variable.bdim_z) (Num 1);
      n_eq (n_div (var "C") (var "H")) (Num contract.template_value);
      positive "B";
      positive "T";
      positive "C";
      positive "H";
      n_eq (Var Variable.gdim_x) (n_mult (var "B") (var "H"));
      n_eq (Var Variable.gdim_y) (Num 1);
      n_eq (Var Variable.gdim_z) (Num 1);
    ]

let precondition (contract : t) : bexp =
  match contract.family with
  | Gla | Wkv -> row_shape_precondition contract

let add_global_ints (names : string list) (kernel : Kernel.t) : Kernel.t =
  let globals =
    names
    |> List.map (fun name -> (Variable.from_name name, C_type.int))
    |> Params.from_list
  in
  {
    kernel with
    global_variables = Params.union_right kernel.global_variables globals;
  }

let apply_to_kernel (contract : t) (kernel : Kernel.t) :
    (Kernel.t, error) result =
  let actual = Kernel.name kernel in
  if not (String.equal actual contract.parsed_kernel) then
    Error (Kernel_mismatch { expected = contract.parsed_kernel; actual })
  else
    let kernel =
      kernel
      |> add_global_ints [ "B"; "T"; "C"; "H"; contract.template_param ]
      |> fun kernel ->
      { kernel with pre = b_and kernel.pre (precondition contract) }
    in
    Ok kernel

let check_block_dim (contract : t) (actual : Dim3.t option) :
    (Dim3.t option, error) result =
  let expected = block_dim contract in
  match actual with
  | None -> Ok (Some expected)
  | Some actual when Dim3.compare actual expected = 0 -> Ok (Some actual)
  | Some actual -> Error (Conflicting_block_dim { expected; actual })

let check_grid_dim (_contract : t) (actual : Dim3.t option) :
    (unit, error) result =
  match actual with
  | None -> Ok ()
  | Some actual -> Error (Grid_dim_unsupported actual)

let check_all_dims (contract : t) (all_dims : bool) : (unit, error) result =
  if all_dims then Error (All_dims_unsupported contract.row_id) else Ok ()

let check_only_kernel (contract : t) (only_kernel : string option) :
    (unit, error) result =
  match only_kernel with
  | Some actual when String.equal actual contract.parsed_kernel -> Ok ()
  | Some actual ->
      Error (Kernel_mismatch { expected = contract.parsed_kernel; actual })
  | None ->
      Error
        (Missing_kernel_selection
           { row_id = contract.row_id; expected = contract.parsed_kernel })

let merge_params (contract : t) (params : (string * int) list) :
    ((string * int) list, error) result =
  let merge_one acc (key, expected) =
    match List.assoc_opt key params with
    | Some actual when actual <> expected ->
        Error (Conflicting_param { key; expected; actual })
    | Some _ -> Ok acc
    | None -> Ok ((key, expected) :: acc)
  in
  List.fold_left
    (fun acc required ->
      match acc with
      | Error _ as error -> error
      | Ok acc -> merge_one acc required)
    (Ok params) (required_params contract)
