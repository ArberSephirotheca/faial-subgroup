open Protocols

module Site = struct
  type id = int

  type t = {
    id : id;
    location : Stage0.Location.t option;
    label : string option;
  }

  let make ?location ?label (id : id) : t =
    if id < 0 then invalid_arg "subgroup/matrix site ids must be non-negative";
    Option.iter
      (fun label ->
        if String.equal label "" then
          invalid_arg "subgroup/matrix site labels must not be empty")
      label;
    { id; location; label }

  let id (site : t) : id = site.id
  let location_opt (site : t) : Stage0.Location.t option = site.location
  let label_opt (site : t) : string option = site.label

  let to_string (site : t) : string =
    let label =
      site.label
      |> Option.map (Printf.sprintf "[%s]")
      |> Option.value ~default:""
    in
    let location =
      site.location
      |> Option.map (fun location -> "@" ^ Stage0.Location.to_string location)
      |> Option.value ~default:""
    in
    Printf.sprintf "site#%d%s%s" site.id label location
end

module Target_config = struct
  type target = Cuda_like
  type subgroup_size = int
  type thread_projection = { x : Variable.t; y : Variable.t; z : Variable.t }

  type cuda_lane_mapping =
    | Thread_idx_x_contiguous of { subgroup_size : subgroup_size }

  type cuda_config = { lane_mapping : cuda_lane_mapping }
  type t = Cuda of cuda_config | Missing of { target : target }

  type error =
    | Invalid_subgroup_size of int
    | Unsupported_target_configuration of { target : target; reason : string }

  let target_to_string : target -> string = function Cuda_like -> "cuda-like"

  let error_to_string : error -> string = function
    | Invalid_subgroup_size value ->
        Printf.sprintf "invalid subgroup size %d: size must be non-zero" value
    | Unsupported_target_configuration { target; reason } ->
        Printf.sprintf "unsupported %s subgroup target configuration: %s"
          (target_to_string target) reason

  let subgroup_size (value : int) : (subgroup_size, error) result =
    if value <= 0 then Error (Invalid_subgroup_size value) else Ok value

  let subgroup_size_exn (value : int) : subgroup_size =
    match subgroup_size value with
    | Ok size -> size
    | Error error -> invalid_arg (error_to_string error)

  let subgroup_size_value (value : subgroup_size) : int = value

  let cuda_x_contiguous (subgroup_size : subgroup_size) : t =
    Cuda { lane_mapping = Thread_idx_x_contiguous { subgroup_size } }

  let missing_cuda : t = Missing { target = Cuda_like }

  let cuda_thread_idx : thread_projection =
    { x = Variable.tid_x; y = Variable.tid_y; z = Variable.tid_z }

  let cuda_thread_idx_with_suffix (suffix : string) : thread_projection =
    {
      x = Variable.add_suffix suffix Variable.tid_x;
      y = Variable.add_suffix suffix Variable.tid_y;
      z = Variable.add_suffix suffix Variable.tid_z;
    }

  let same_variable (left : Variable.t) (right : Variable.t) : Exp.bexp =
    Exp.n_eq (Exp.Var left) (Exp.Var right)

  let subgroup_index (thread_x : Variable.t) (subgroup_size : subgroup_size) :
      Exp.nexp =
    Exp.n_div (Exp.Var thread_x) (Exp.Num subgroup_size)

  let cuda_same_subgroup (config : cuda_config) ~(left : thread_projection)
      ~(right : thread_projection) : Exp.bexp =
    match config.lane_mapping with
    | Thread_idx_x_contiguous { subgroup_size } ->
        Exp.b_and
          (Exp.b_and
             (same_variable left.y right.y)
             (same_variable left.z right.z))
          (Exp.n_eq
             (subgroup_index left.x subgroup_size)
             (subgroup_index right.x subgroup_size))

  let same_subgroup (config : t) ~(left : thread_projection)
      ~(right : thread_projection) : (Exp.bexp, error) result =
    match config with
    | Cuda config -> Ok (cuda_same_subgroup config ~left ~right)
    | Missing { target } ->
        Error
          (Unsupported_target_configuration
             { target; reason = "missing explicit subgroup lane mapping" })

  let cuda_x_contiguous_subgroup_size : t -> subgroup_size option = function
    | Cuda { lane_mapping = Thread_idx_x_contiguous { subgroup_size } } ->
        Some subgroup_size
    | Missing _ -> None

  let to_string : t -> string = function
    | Cuda { lane_mapping = Thread_idx_x_contiguous { subgroup_size } } ->
        Printf.sprintf "cuda-like(threadIdx.x-contiguous(size=%d))"
          subgroup_size
    | Missing { target } ->
        Printf.sprintf "%s(missing subgroup mapping)" (target_to_string target)
end

module Barrier = struct
  type mem_sem = No_memory_semantics | Acquire | Release | Acq_rel

  type kind = {
    storage : bool;
    workgroup : bool;
    subgroup : bool;
    mem_sem : mem_sem;
  }

  type t = { site : Site.t; kind : kind }

  let kind ?(storage = false) ?(workgroup = false) ?(subgroup = false)
      ?(mem_sem = No_memory_semantics) () : kind =
    { storage; workgroup; subgroup; mem_sem }

  let make (site : Site.t) (kind : kind) : t = { site; kind }

  let scopes (kind : kind) : string list =
    [
      (if kind.storage then Some "storage" else None);
      (if kind.workgroup then Some "workgroup" else None);
      (if kind.subgroup then Some "subgroup" else None);
    ]
    |> List.filter_map Fun.id

  let mem_sem_to_string : mem_sem -> string = function
    | No_memory_semantics -> "none"
    | Acquire -> "acquire"
    | Release -> "release"
    | Acq_rel -> "acqrel"

  let kind_to_string (kind : kind) : string =
    let scopes = scopes kind in
    let scope_text =
      match scopes with [] -> "none" | scopes -> String.concat ", " scopes
    in
    match kind.mem_sem with
    | No_memory_semantics -> Printf.sprintf "barrier<%s>" scope_text
    | mem_sem ->
        Printf.sprintf "barrier<%s; memsem=%s>" scope_text
          (mem_sem_to_string mem_sem)

  let to_string (barrier : t) : string =
    Printf.sprintf "%s %s"
      (Site.to_string barrier.site)
      (kind_to_string barrier.kind)
end

module Collective = struct
  type operand = Numeric of Exp.nexp | Bool of Exp.bexp

  type gather_mode =
    | Broadcast_first
    | Broadcast of Exp.nexp
    | Shuffle of Exp.nexp
    | Shuffle_down of Exp.nexp
    | Shuffle_up of Exp.nexp
    | Shuffle_xor of Exp.nexp

  type operation = Reduce | Inclusive_scan | Exclusive_scan
  type subgroup_operation = All | Any | Add | Mul | Min | Max | And | Or | Xor

  type kind =
    | Ballot
    | Gather of gather_mode
    | Operation of { op : subgroup_operation; collective_op : operation }

  type payload =
    | Ballot_payload of { result : Variable.t; predicate : Exp.bexp option }
    | Gather_payload of {
        mode : gather_mode;
        argument : operand;
        result : Variable.t;
      }
    | Operation_payload of {
        op : subgroup_operation;
        collective_op : operation;
        argument : operand;
        result : Variable.t;
      }

  type t = { site : Site.t; payload : payload }

  let make (site : Site.t) (payload : payload) : t = { site; payload }

  let kind (collective : t) : kind =
    match collective.payload with
    | Ballot_payload _ -> Ballot
    | Gather_payload { mode; _ } -> Gather mode
    | Operation_payload { op; collective_op; _ } ->
        Operation { op; collective_op }

  let result (collective : t) : Variable.t =
    match collective.payload with
    | Ballot_payload { result; _ }
    | Gather_payload { result; _ }
    | Operation_payload { result; _ } ->
        result

  let operand_to_string : operand -> string = function
    | Numeric expr -> Exp.n_to_string expr
    | Bool expr -> Exp.b_to_string expr

  let gather_mode_to_string : gather_mode -> string = function
    | Broadcast_first -> "broadcast_first"
    | Broadcast expr -> "broadcast(" ^ Exp.n_to_string expr ^ ")"
    | Shuffle expr -> "shuffle(" ^ Exp.n_to_string expr ^ ")"
    | Shuffle_down expr -> "shuffle_down(" ^ Exp.n_to_string expr ^ ")"
    | Shuffle_up expr -> "shuffle_up(" ^ Exp.n_to_string expr ^ ")"
    | Shuffle_xor expr -> "shuffle_xor(" ^ Exp.n_to_string expr ^ ")"

  let operation_to_string : operation -> string = function
    | Reduce -> "reduce"
    | Inclusive_scan -> "inclusive_scan"
    | Exclusive_scan -> "exclusive_scan"

  let subgroup_operation_to_string : subgroup_operation -> string = function
    | All -> "all"
    | Any -> "any"
    | Add -> "add"
    | Mul -> "mul"
    | Min -> "min"
    | Max -> "max"
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"

  let kind_to_string : kind -> string = function
    | Ballot -> "collective<ballot>"
    | Gather mode -> "collective<gather:" ^ gather_mode_to_string mode ^ ">"
    | Operation { op; collective_op } ->
        Printf.sprintf "collective<%s:%s>"
          (operation_to_string collective_op)
          (subgroup_operation_to_string op)

  let payload_to_string : payload -> string = function
    | Ballot_payload { result; predicate } ->
        let predicate =
          predicate |> Option.map Exp.b_to_string
          |> Option.value ~default:"<implicit>"
        in
        Printf.sprintf "%s -> %s predicate=%s" (kind_to_string Ballot)
          (Variable.name result) predicate
    | Gather_payload { mode; argument; result } ->
        Printf.sprintf "%s(%s) -> %s"
          (kind_to_string (Gather mode))
          (operand_to_string argument)
          (Variable.name result)
    | Operation_payload { op; collective_op; argument; result } ->
        Printf.sprintf "%s(%s) -> %s"
          (kind_to_string (Operation { op; collective_op }))
          (operand_to_string argument)
          (Variable.name result)

  let to_string (collective : t) : string =
    Printf.sprintf "%s %s"
      (Site.to_string collective.site)
      (payload_to_string collective.payload)
end

module Matrix = struct
  type collective_kind =
    | Fill_fragment
    | Load_matrix_sync
    | Mma_sync
    | Store_matrix_sync

  type layout = Row_major | Col_major

  type footprint =
    | Scalar of Access.t
    | Rectangular of {
        base : Access.t;
        rows : Exp.nexp;
        cols : Exp.nexp;
        leading_dimension : Exp.nexp;
        layout : layout;
        row : Variable.t;
        col : Variable.t;
      }

  type memory_effect = Read of footprint | Write of footprint

  type collective = {
    site : Site.t;
    kind : collective_kind;
    memory : memory_effect option;
  }

  let scalar (access : Access.t) : footprint = Scalar access

  let rectangular ~(base : Access.t) ~(rows : Exp.nexp) ~(cols : Exp.nexp)
      ~(leading_dimension : Exp.nexp) ~(layout : layout) ~(row : Variable.t)
      ~(col : Variable.t) : (footprint, string) result =
    match base.index with
    | [] | [ _ ] ->
        Ok
          (Rectangular { base; rows; cols; leading_dimension; layout; row; col })
    | _ -> Error "matrix footprints require a scalar pointer base"

  let access : footprint -> Access.t = function
    | Scalar access -> access
    | Rectangular { base; _ } -> base

  let indexed_access : footprint -> Access.t = function
    | Scalar access -> access
    | Rectangular { base; leading_dimension; layout; row; col; _ } ->
        let row = Exp.Var row in
        let col = Exp.Var col in
        let tile_offset =
          match layout with
          | Row_major -> Exp.n_plus (Exp.n_mult row leading_dimension) col
          | Col_major -> Exp.n_plus (Exp.n_mult col leading_dimension) row
        in
        let index =
          match base.index with
          | [] -> tile_offset
          | [ base_index ] -> Exp.n_plus base_index tile_offset
          | _ ->
              failwith "matrix footprint constructor rejects multi-index bases"
        in
        { base with index = [ index ] }

  let bounds_condition : footprint -> Exp.bexp = function
    | Scalar _ -> Exp.Bool true
    | Rectangular { rows; cols; row; col; _ } ->
        let row_expr = Exp.Var row in
        let col_expr = Exp.Var col in
        Exp.b_and
          (Exp.b_and (Exp.n_ge row_expr (Exp.Num 0)) (Exp.n_lt row_expr rows))
          (Exp.b_and (Exp.n_ge col_expr (Exp.Num 0)) (Exp.n_lt col_expr cols))

  let with_mode (mode : Access.Mode.t) : footprint -> footprint = function
    | Scalar access -> Scalar { access with mode }
    | Rectangular data ->
        Rectangular { data with base = { data.base with mode } }

  let read_effect (footprint : footprint) : (memory_effect, string) result =
    if Access.is_read (access footprint) then Ok (Read footprint)
    else Error "matrix load memory effects require read-mode accesses"

  let write_effect (footprint : footprint) : (memory_effect, string) result =
    if Access.is_write (access footprint) then Ok (Write footprint)
    else Error "matrix store memory effects require write-mode accesses"

  let make_collective (site : Site.t) (kind : collective_kind)
      (memory : memory_effect option) : (collective, string) result =
    match (kind, memory) with
    | (Fill_fragment | Mma_sync), None -> Ok { site; kind; memory = None }
    | Load_matrix_sync, Some (Read _ as memory) ->
        Ok { site; kind; memory = Some memory }
    | Store_matrix_sync, Some (Write _ as memory) ->
        Ok { site; kind; memory = Some memory }
    | (Fill_fragment | Mma_sync), Some _ ->
        Error "fill_fragment and mma_sync must not carry matrix memory effects"
    | Load_matrix_sync, _ ->
        Error "load_matrix_sync must carry a read matrix memory effect"
    | Store_matrix_sync, _ ->
        Error "store_matrix_sync must carry a write matrix memory effect"

  let fill_fragment (site : Site.t) : collective =
    { site; kind = Fill_fragment; memory = None }

  let load_matrix_sync (site : Site.t) (footprint : footprint) :
      (collective, string) result =
    let ( let* ) = Result.bind in
    let* memory = read_effect footprint in
    make_collective site Load_matrix_sync (Some memory)

  let mma_sync (site : Site.t) : collective =
    { site; kind = Mma_sync; memory = None }

  let store_matrix_sync (site : Site.t) (footprint : footprint) :
      (collective, string) result =
    let ( let* ) = Result.bind in
    let* memory = write_effect footprint in
    make_collective site Store_matrix_sync (Some memory)

  let memory_effect_access : memory_effect -> Access.t = function
    | Read footprint | Write footprint -> access footprint

  let memory_effect_footprint : memory_effect -> footprint = function
    | Read footprint | Write footprint -> footprint

  let collective_kind_to_string : collective_kind -> string = function
    | Fill_fragment -> "fill_fragment"
    | Load_matrix_sync -> "load_matrix_sync"
    | Mma_sync -> "mma_sync"
    | Store_matrix_sync -> "store_matrix_sync"

  let layout_to_string : layout -> string = function
    | Row_major -> "row_major"
    | Col_major -> "col_major"

  let footprint_to_string : footprint -> string = function
    | Scalar access -> Access.to_string access
    | Rectangular { base; rows; cols; leading_dimension; layout; row; col } ->
        Printf.sprintf "%s footprint<%sx%s, ldm=%s, %s, row=%s, col=%s>"
          (Access.to_string base) (Exp.n_to_string rows) (Exp.n_to_string cols)
          (Exp.n_to_string leading_dimension)
          (layout_to_string layout) (Variable.name row) (Variable.name col)

  let memory_effect_to_string : memory_effect -> string = function
    | Read footprint -> "read " ^ footprint_to_string footprint
    | Write footprint -> "write " ^ footprint_to_string footprint

  let collective_to_string (collective : collective) : string =
    let memory =
      collective.memory
      |> Option.map (fun memory -> " " ^ memory_effect_to_string memory)
      |> Option.value ~default:""
    in
    Printf.sprintf "%s matrix<%s>%s"
      (Site.to_string collective.site)
      (collective_kind_to_string collective.kind)
      memory
end

module Stmt = struct
  type t =
    | Workgroup_barrier of Barrier.t
    | Subgroup_barrier of Barrier.t
    | Subgroup_collective of Collective.t
    | Matrix_collective of Matrix.collective

  let workgroup_barrier (site : Site.t) : t =
    Workgroup_barrier (Barrier.make site (Barrier.kind ~workgroup:true ()))

  let subgroup_barrier (site : Site.t) : t =
    Subgroup_barrier (Barrier.make site (Barrier.kind ~subgroup:true ()))

  let site : t -> Site.t = function
    | Workgroup_barrier barrier | Subgroup_barrier barrier -> barrier.site
    | Subgroup_collective collective -> collective.site
    | Matrix_collective collective -> collective.site

  let is_subgroup_boundary : t -> bool = function
    | Workgroup_barrier _ -> false
    | Subgroup_barrier _ | Subgroup_collective _ | Matrix_collective _ -> true

  let matrix_memory_effect : t -> Matrix.memory_effect option = function
    | Matrix_collective collective -> collective.memory
    | Workgroup_barrier _ | Subgroup_barrier _ | Subgroup_collective _ -> None

  let to_string : t -> string = function
    | Workgroup_barrier barrier | Subgroup_barrier barrier ->
        Barrier.to_string barrier
    | Subgroup_collective collective -> Collective.to_string collective
    | Matrix_collective collective -> Matrix.collective_to_string collective
end

module Kernel = struct
  type t = {
    name : string;
    target_config : Target_config.t;
    body : Stmt.t list;
  }

  let make ?(target_config = Target_config.missing_cuda) ~(name : string)
      (body : Stmt.t list) : t =
    if String.equal name "" then
      invalid_arg "subgroup/matrix kernels must have a name";
    { name; target_config; body }

  let has_subgroup_boundary (kernel : t) : bool =
    List.exists Stmt.is_subgroup_boundary kernel.body
end
