open Protocols
open Exp

type family = Launch_contract_generator.contract_family =
  | Gla
  | Wkv
  | Wkv7
  | Solve_tri_fast
  | Finite_type_template

type template_domain = Launch_contract_generator.template_domain =
  | Int_template_domain of { parameter : string; value : int }
  | Finite_type_domain of { parameter : string; values : string list }

type t = {
  row_id : string;
  family : family;
  manifest_kernel : string;
  parsed_kernel : string;
  template_arg : string;
  template_param : string option;
  template_value : int option;
  template_bindings : (string * int) list;
  template_domains : template_domain list;
  shape_contract : Launch_contract_generator.shape_contract;
  block_dim : Dim3.t option;
  symbolic_dimension_carrier :
    Launch_contract_generator.solve_tri_symbolic_dimension_carrier option;
}

type error =
  | Unknown_row of string
  | Kernel_mismatch of { expected : string; actual : string }
  | Conflicting_param of { key : string; expected : int; actual : int }
  | Conflicting_block_dim of { expected : Dim3.t; actual : Dim3.t }
  | Concrete_block_dim_unsupported of { row_id : string; actual : Dim3.t }
  | Grid_dim_unsupported of Dim3.t
  | All_dims_unsupported of string
  | All_dims_required of string
  | Missing_kernel_selection of { row_id : string; expected : string }
  | Subgroup_route_required of string
  | Subgroup_kernel_unsupported of string
  | Duplicate_row of string

let error_to_string : error -> string = function
  | Unknown_row row_id -> "unknown launch contract row '" ^ row_id ^ "'"
  | Duplicate_row row_id -> "duplicate launch contract row '" ^ row_id ^ "'"
  | Kernel_mismatch { expected; actual } ->
      "launch contract expects parsed kernel '" ^ expected ^ "', got '" ^ actual
      ^ "'"
  | Conflicting_param { key; expected; actual } ->
      "launch contract expects parameter " ^ key ^ "=" ^ string_of_int expected
      ^ ", got " ^ string_of_int actual
  | Conflicting_block_dim { expected; actual } ->
      "launch contract expects --block-dim " ^ Dim3.to_string expected
      ^ ", got " ^ Dim3.to_string actual
  | Concrete_block_dim_unsupported { row_id; actual } ->
      "launch contract '" ^ row_id
      ^ "' carries symbolic/bounded blockDim facts; do not pass concrete \
         --block-dim " ^ Dim3.to_string actual
  | Grid_dim_unsupported actual ->
      "launch contract carries symbolic gridDim.x; do not pass concrete \
       --grid-dim " ^ Dim3.to_string actual
  | All_dims_unsupported row_id ->
      "launch contract '" ^ row_id
      ^ "' requires concrete blockDim and symbolic gridDim; do not pass \
         --all-dims"
  | All_dims_required row_id ->
      "launch contract '" ^ row_id
      ^ "' carries symbolic/bounded blockDim facts; run it with --all-dims"
  | Missing_kernel_selection { row_id; expected } ->
      "launch contract '" ^ row_id ^ "' requires --kernel " ^ expected
  | Subgroup_route_required kernel ->
      "launch contract requires subgroup/matrix route for kernel '" ^ kernel
      ^ "'"
  | Subgroup_kernel_unsupported kernel ->
      "launch contract is currently ordinary-DRF only, got subgroup/matrix \
       kernel '" ^ kernel ^ "'"

let template_domain_to_string = function
  | Int_template_domain { parameter; value } ->
      parameter ^ "=" ^ string_of_int value
  | Finite_type_domain { parameter; values } ->
      parameter ^ "={" ^ String.concat "," values ^ "}"

let var (name : string) : nexp = Var (Variable.from_name name)

let launch_builtin_dim_to_variable = function
  | Launch_contract_generator.Block_dim_x -> Variable.bdim_x
  | Launch_contract_generator.Block_dim_y -> Variable.bdim_y
  | Launch_contract_generator.Block_dim_z -> Variable.bdim_z
  | Launch_contract_generator.Grid_dim_x -> Variable.gdim_x
  | Launch_contract_generator.Grid_dim_y -> Variable.gdim_y
  | Launch_contract_generator.Grid_dim_z -> Variable.gdim_z

let rec launch_nexp_to_exp = function
  | Launch_contract_generator.Launch_num value -> Num value
  | Launch_contract_generator.Launch_var name -> var name
  | Launch_contract_generator.Launch_builtin dim ->
      Var (launch_builtin_dim_to_variable dim)
  | Launch_contract_generator.Launch_div (lhs, rhs) ->
      n_div (launch_nexp_to_exp lhs) (launch_nexp_to_exp rhs)
  | Launch_contract_generator.Launch_mul (lhs, rhs) ->
      n_mult (launch_nexp_to_exp lhs) (launch_nexp_to_exp rhs)

let shape_fact_to_exp = function
  | Launch_contract_generator.Shape_eq (lhs, rhs) ->
      n_eq (launch_nexp_to_exp lhs) (launch_nexp_to_exp rhs)
  | Launch_contract_generator.Shape_gt (lhs, rhs) ->
      n_gt (launch_nexp_to_exp lhs) (launch_nexp_to_exp rhs)
  | Launch_contract_generator.Shape_ge (lhs, rhs) ->
      n_ge (launch_nexp_to_exp lhs) (launch_nexp_to_exp rhs)
  | Launch_contract_generator.Shape_le (lhs, rhs) ->
      n_le (launch_nexp_to_exp lhs) (launch_nexp_to_exp rhs)

let shape_contract_precondition
    (contract : Launch_contract_generator.shape_contract) : bexp =
  b_and_ex (List.map shape_fact_to_exp contract.shape_facts)

let dim3_of_list row_id = function
  | [ x; y; z ] -> Dim3.make ~x ~y ~z ()
  | _ -> invalid_arg ("launch contract row " ^ row_id ^ " has invalid dim3")

let dim3_option_of_list row_id = function
  | Some values -> Some (dim3_of_list row_id values)
  | None -> None

let of_launch_contract_row (row : Launch_contract_generator.launch_contract_row)
    : t =
  {
    row_id = row.launch_row_id;
    family = row.launch_family;
    manifest_kernel = row.launch_manifest_kernel;
    parsed_kernel = row.launch_parsed_kernel;
    template_arg = row.launch_template_arg;
    template_param = row.launch_template_param;
    template_value = row.launch_template_value;
    template_bindings = row.launch_template_bindings;
    template_domains = row.launch_template_domains;
    shape_contract = row.launch_shape_contract;
    block_dim = dim3_option_of_list row.launch_row_id row.launch_block_dim;
    symbolic_dimension_carrier = row.launch_symbolic_dimension_carrier;
  }

let catalog_rows : t list =
  List.map of_launch_contract_row
    Launch_contract_generator.catalog_launch_contract_rows

let all : t list = catalog_rows

let lookup_rows : t list =
  List.map of_launch_contract_row
    Launch_contract_generator.lookup_launch_contract_rows

let of_row_id (row_id : string) : (t, error) result =
  match List.filter (fun c -> String.equal c.row_id row_id) lookup_rows with
  | [ contract ] -> Ok contract
  | [] -> Error (Unknown_row row_id)
  | _ -> Error (Duplicate_row row_id)

let block_dim_option (contract : t) : Dim3.t option = contract.block_dim

let block_dim (contract : t) : Dim3.t =
  match block_dim_option contract with
  | Some block_dim -> block_dim
  | None ->
      invalid_arg
        ("launch contract row " ^ contract.row_id
       ^ " does not carry a concrete blockDim")

let symbolic_dimension_carrier (contract : t) :
    Launch_contract_generator.solve_tri_symbolic_dimension_carrier option =
  contract.symbolic_dimension_carrier

let required_params (contract : t) : (string * int) list =
  contract.template_bindings

let template_domains (contract : t) : template_domain list =
  contract.template_domains

let finite_type_domains (contract : t) : (string * string list) list =
  contract.template_domains
  |> List.filter_map (function
    | Int_template_domain _ -> None
    | Finite_type_domain { parameter; values } -> Some (parameter, values))

let precondition (contract : t) : bexp =
  shape_contract_precondition contract.shape_contract

let subgroup_route_size (contract : t) : int option =
  contract.shape_contract.shape_subgroup_size

let requires_subgroup_route (contract : t) : bool =
  Option.is_some (subgroup_route_size contract)

let allows_subgroup_route (contract : t) ~(subgroup_size : int option) : bool =
  match (subgroup_route_size contract, subgroup_size) with
  | Some expected, Some actual -> Int.equal expected actual
  | Some _, None | None, Some _ | None, None -> false

let add_global_ints (names : string list) (kernel : Kernel.t) : Kernel.t =
  let globals =
    names
    |> List.map (fun name -> (Variable.from_name name, Ty.int))
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
      kernel |> add_global_ints contract.shape_contract.shape_global_ints
      |> fun kernel ->
      { kernel with pre = b_and kernel.pre (precondition contract) }
    in
    Ok kernel

let check_block_dim (contract : t) (actual : Dim3.t option) :
    (Dim3.t option, error) result =
  match (block_dim_option contract, actual) with
  | Some expected, None -> Ok (Some expected)
  | Some expected, Some actual when Dim3.compare actual expected = 0 ->
      Ok (Some actual)
  | Some expected, Some actual ->
      Error (Conflicting_block_dim { expected; actual })
  | None, None -> Ok None
  | None, Some actual ->
      Error
        (Concrete_block_dim_unsupported { row_id = contract.row_id; actual })

let check_grid_dim (_contract : t) (actual : Dim3.t option) :
    (unit, error) result =
  match actual with
  | None -> Ok ()
  | Some actual -> Error (Grid_dim_unsupported actual)

let check_all_dims (contract : t) (all_dims : bool) : (unit, error) result =
  match (all_dims, block_dim_option contract) with
  | true, Some _ -> Error (All_dims_unsupported contract.row_id)
  | false, None -> Error (All_dims_required contract.row_id)
  | true, None | false, Some _ -> Ok ()

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
