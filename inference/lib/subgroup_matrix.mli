open Protocols

module Site : sig
  type id = int

  type t = private {
    id : id;
    location : Stage0.Location.t option;
    label : string option;
    may_repeat : bool;
  }

  val make :
    ?location:Stage0.Location.t -> ?label:string -> ?may_repeat:bool -> id -> t

  val id : t -> id
  val location_opt : t -> Stage0.Location.t option
  val label_opt : t -> string option
  val may_repeat : t -> bool
  val to_string : t -> string
end

module Target_config : sig
  type target = Cuda_like
  type subgroup_size
  type thread_projection = { x : Variable.t; y : Variable.t; z : Variable.t }

  type cuda_lane_mapping =
    | Thread_idx_x_contiguous of { subgroup_size : subgroup_size }

  type cuda_config = { lane_mapping : cuda_lane_mapping }
  type t = Cuda of cuda_config | Missing of { target : target }

  type error =
    | Invalid_subgroup_size of int
    | Unsupported_target_configuration of { target : target; reason : string }

  val subgroup_size : int -> (subgroup_size, error) result
  val subgroup_size_exn : int -> subgroup_size
  val subgroup_size_value : subgroup_size -> int
  val cuda_x_contiguous : subgroup_size -> t
  val missing_cuda : t
  val cuda_thread_idx : thread_projection
  val cuda_thread_idx_with_suffix : string -> thread_projection

  val same_subgroup :
    t ->
    left:thread_projection ->
    right:thread_projection ->
    (Exp.bexp, error) result

  val cuda_x_contiguous_subgroup_size : t -> subgroup_size option
  val target_to_string : target -> string
  val error_to_string : error -> string
  val to_string : t -> string
end

module Barrier : sig
  type mem_sem = No_memory_semantics | Acquire | Release | Acq_rel

  type kind = private {
    storage : bool;
    workgroup : bool;
    subgroup : bool;
    mem_sem : mem_sem;
  }

  type t = private { site : Site.t; kind : kind }

  val kind :
    ?storage:bool ->
    ?workgroup:bool ->
    ?subgroup:bool ->
    ?mem_sem:mem_sem ->
    unit ->
    kind

  val make : Site.t -> kind -> t
  val scopes : kind -> string list
  val orders_memory : kind -> bool
  val to_string : t -> string
end

module Collective : sig
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

  type t = private { site : Site.t; payload : payload }

  val make : Site.t -> payload -> t
  val kind : t -> kind
  val result : t -> Variable.t
  val to_string : t -> string
end

module Matrix : sig
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

  type collective = private {
    site : Site.t;
    kind : collective_kind;
    memory : memory_effect option;
  }

  val scalar : Access.t -> footprint

  val rectangular :
    base:Access.t ->
    rows:Exp.nexp ->
    cols:Exp.nexp ->
    leading_dimension:Exp.nexp ->
    layout:layout ->
    row:Variable.t ->
    col:Variable.t ->
    (footprint, string) result

  val access : footprint -> Access.t
  val indexed_access : footprint -> Access.t
  val bounds_condition : footprint -> Exp.bexp
  val with_mode : Access.Mode.t -> footprint -> footprint
  val read_effect : footprint -> (memory_effect, string) result
  val write_effect : footprint -> (memory_effect, string) result

  val make_collective :
    Site.t ->
    collective_kind ->
    memory_effect option ->
    (collective, string) result

  val fill_fragment : Site.t -> collective
  val load_matrix_sync : Site.t -> footprint -> (collective, string) result
  val mma_sync : Site.t -> collective
  val store_matrix_sync : Site.t -> footprint -> (collective, string) result
  val memory_effect_access : memory_effect -> Access.t
  val memory_effect_footprint : memory_effect -> footprint
  val collective_kind_to_string : collective_kind -> string
  val layout_to_string : layout -> string
  val footprint_to_string : footprint -> string
  val memory_effect_to_string : memory_effect -> string
  val collective_to_string : collective -> string
end

module Stmt : sig
  type t =
    | Workgroup_barrier of Barrier.t
    | Subgroup_barrier of Barrier.t
    | Subgroup_collective of Collective.t
    | Matrix_collective of Matrix.collective

  val workgroup_barrier : Site.t -> t
  val subgroup_barrier : Site.t -> t
  val site : t -> Site.t
  val is_subgroup_boundary : t -> bool
  val orders_memory : t -> bool
  val matrix_memory_effect : t -> Matrix.memory_effect option
  val to_string : t -> string
end

module Kernel : sig
  type t = private {
    name : string;
    target_config : Target_config.t;
    body : Stmt.t list;
  }

  val make : ?target_config:Target_config.t -> name:string -> Stmt.t list -> t
  val has_subgroup_boundary : t -> bool
end
