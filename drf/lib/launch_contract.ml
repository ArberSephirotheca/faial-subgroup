open Protocols
open Exp

type family = Gla | Wkv | Wkv7 | Solve_tri_fast

type t = {
  row_id : string;
  family : family;
  manifest_kernel : string;
  parsed_kernel : string;
  template_arg : string;
  template_param : string;
  template_value : int;
  template_bindings : (string * int) list;
  block_dim : Dim3.t;
}

type error =
  | Unknown_row of string
  | Kernel_mismatch of { expected : string; actual : string }
  | Conflicting_param of { key : string; expected : int; actual : int }
  | Conflicting_block_dim of { expected : Dim3.t; actual : Dim3.t }
  | Grid_dim_unsupported of Dim3.t
  | All_dims_unsupported of string
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
  | Grid_dim_unsupported actual ->
      "launch contract carries symbolic gridDim.x; do not pass concrete \
       --grid-dim " ^ Dim3.to_string actual
  | All_dims_unsupported row_id ->
      "launch contract '" ^ row_id
      ^ "' requires concrete blockDim and symbolic gridDim; do not pass \
         --all-dims"
  | Missing_kernel_selection { row_id; expected } ->
      "launch contract '" ^ row_id ^ "' requires --kernel " ^ expected
  | Subgroup_route_required kernel ->
      "launch contract requires subgroup/matrix route for kernel '" ^ kernel
      ^ "'"
  | Subgroup_kernel_unsupported kernel ->
      "launch contract is currently ordinary-DRF only, got subgroup/matrix \
       kernel '" ^ kernel ^ "'"

let of_row (row : Launch_contract_rows.t) : t =
  let family =
    match row.family with
    | Launch_contract_rows.Gla -> Gla
    | Launch_contract_rows.Wkv -> Wkv
    | Launch_contract_rows.Wkv7 -> Wkv7
  in
  {
    row_id = row.row_id;
    family;
    manifest_kernel = row.manifest_kernel;
    parsed_kernel = row.parsed_kernel;
    template_arg = row.template_arg;
    template_param = row.template_param;
    template_value = row.template_value;
    template_bindings = [ (row.template_param, row.template_value) ];
    block_dim = Dim3.make ~x:row.template_value ();
  }

let is_gla_row (row : Launch_contract_rows.t) =
  match row.family with
  | Launch_contract_rows.Gla -> true
  | Launch_contract_rows.Wkv | Launch_contract_rows.Wkv7 -> false

let generated_rows : Launch_contract_rows.t list =
  Launch_contract_generator.contracts

let generated_gla_rows : Launch_contract_rows.t list =
  generated_rows |> List.filter is_gla_row

let generated_wkv_rows : Launch_contract_rows.t list =
  generated_rows |> List.filter (fun row -> not (is_gla_row row))

let catalog_rows : Launch_contract_rows.t list = generated_rows
let all : t list = List.map of_row catalog_rows

let selected_contract (selected : Launch_contract_generator.selected_row) : t =
  let n_template =
    match List.assoc_opt "n_template" selected.selected_template_bindings with
    | Some value -> value
    | None ->
        invalid_arg
          ("solve-tri selected row " ^ selected.selected_row_id
         ^ " is missing n_template")
  in
  {
    row_id = selected.selected_row_id;
    family = Solve_tri_fast;
    manifest_kernel = selected.selected_manifest_kernel;
    parsed_kernel = selected.selected_parsed_kernel;
    template_arg = selected.selected_template_arg;
    template_param = "n_template";
    template_value = n_template;
    template_bindings = selected.selected_template_bindings;
    block_dim =
      (match selected.selected_concrete_block_dim with
      | [ x; y; z ] -> Dim3.make ~x ~y ~z ()
      | _ ->
          invalid_arg
            ("solve-tri selected row " ^ selected.selected_row_id
           ^ " has invalid concrete block dim"));
  }

let pending_lookup_rows : t list =
  List.map selected_contract Launch_contract_generator.selected_rows

let lookup_rows : t list = all @ pending_lookup_rows

let solve_tri_symbolic_k_guard =
  Launch_contract_generator.solve_tri_symbolic_k_guard

let validate_solve_tri_symbolic_k_guard =
  Launch_contract_generator.validate_solve_tri_symbolic_k_guard

let solve_tri_symbolic_k_obligation_blocker =
  Launch_contract_generator.solve_tri_symbolic_k_obligation_blocker

let solve_tri_symbolic_k_obligation_blocker_lines () =
  Launch_contract_generator.symbolic_obligation_blocker_lines
    solve_tri_symbolic_k_obligation_blocker

let of_row_id (row_id : string) : (t, error) result =
  match List.filter (fun c -> String.equal c.row_id row_id) lookup_rows with
  | [ contract ] -> Ok contract
  | [] -> Error (Unknown_row row_id)
  | _ -> Error (Duplicate_row row_id)

let block_dim (contract : t) : Dim3.t = contract.block_dim

let required_params (contract : t) : (string * int) list =
  contract.template_bindings

let var (name : string) : nexp = Var (Variable.from_name name)

let standard_row_shape_precondition (contract : t) : bexp =
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

let solve_tri_fast_precondition (contract : t) : bexp =
  let template_value name =
    match List.assoc_opt name contract.template_bindings with
    | Some value -> value
    | None ->
        invalid_arg
          ("solve-tri launch contract " ^ contract.row_id
         ^ " is missing template binding " ^ name)
  in
  let n_template = template_value "n_template" in
  let k_template = template_value "k_template" in
  b_and_ex
    [
      n_eq (var "n_template") (Num n_template);
      n_eq (var "k_template") (Num k_template);
      n_gt (var "n_template") (Num 0);
      n_gt (var "k_template") (Num 0);
      n_eq (Var Variable.bdim_x) (Num contract.block_dim.x);
      n_eq (Var Variable.bdim_y) (Num contract.block_dim.y);
      n_eq (Var Variable.bdim_z) (Num contract.block_dim.z);
      n_gt (Var Variable.gdim_x) (Num 0);
      n_eq (Var Variable.gdim_y) (Num 1);
      n_eq (Var Variable.gdim_z) (Num 1);
    ]

let precondition (contract : t) : bexp =
  match contract.family with
  | Gla | Wkv | Wkv7 -> standard_row_shape_precondition contract
  | Solve_tri_fast -> solve_tri_fast_precondition contract

let subgroup_route_size (contract : t) : int option =
  match contract.family with
  | Gla | Wkv | Wkv7 -> None
  | Solve_tri_fast ->
      let selected =
        match Launch_contract_generator.selected_of_row_id contract.row_id with
        | Ok selected -> selected
        | Error error ->
            invalid_arg (Launch_contract_generator.error_to_string error)
      in
      Some selected.selected_subgroup_size

let requires_subgroup_route (contract : t) : bool =
  Option.is_some (subgroup_route_size contract)

let allows_subgroup_route (contract : t) ~(subgroup_size : int option) : bool =
  match (subgroup_route_size contract, subgroup_size) with
  | Some expected, Some actual -> Int.equal expected actual
  | Some _, None | None, Some _ | None, None -> false

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
    let globals =
      match contract.family with
      | Gla | Wkv | Wkv7 -> [ "B"; "T"; "C"; "H"; contract.template_param ]
      | Solve_tri_fast -> List.map fst contract.template_bindings
    in
    let kernel =
      kernel |> add_global_ints globals |> fun kernel ->
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
