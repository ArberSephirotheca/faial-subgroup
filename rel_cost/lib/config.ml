open Protocols

type t = {
  block_dim : Dim3.t;
  grid_dim : Dim3.t;
  threads_per_warp : int;
  bank_count : int;
  bytes_per_word : int;
}

let to_string (c : t) : string =
  Printf.sprintf
    "{block_dim=%s, grid_dim=%s, threads_per_warp=%d, bank_count=%d, \
     bytes_per_word=%d}"
    (Dim3.to_string c.block_dim)
    (Dim3.to_string c.grid_dim)
    c.threads_per_warp c.bank_count c.bytes_per_word

let total_blocks (cfg : t) : int = Dim3.total cfg.grid_dim
let total_threads (cfg : t) : int = Dim3.total cfg.block_dim * total_blocks cfg
let total_warps (cfg : t) : int =
 let ceil_div a b =
    let q = a / b in
    if a mod b = 0 then q else q + 1
  in
  ceil_div (Dim3.total cfg.block_dim) cfg.threads_per_warp

(* Returns true when threadIdx.x is warp uniform. *)
let tid_x_is_warp_uniform (cfg : t) : bool = cfg.block_dim.x = 1

(* Returns true when threadIdx.x is warp divergent. *)
let tid_x_is_warp_divergent (cfg : t) : bool = not (tid_x_is_warp_uniform cfg)

let tid_y_is_warp_uniform (cfg : t) : bool =
  cfg.block_dim.y = 1 || cfg.block_dim.x >= cfg.threads_per_warp

let tid_y_is_warp_divergent (cfg : t) : bool = not (tid_y_is_warp_uniform cfg)

let tid_z_is_warp_uniform (cfg : t) : bool =
  cfg.block_dim.z = 1
  || cfg.block_dim.x * cfg.block_dim.y >= cfg.threads_per_warp

let tid_z_is_warp_divergent (cfg : t) : bool = not (tid_z_is_warp_uniform cfg)

(** Returns true if a threadIdx variable is warp-uniform (same value for every
    warp). *)
let is_warp_uniform (x : Variable.t) (cfg : t) : bool =
  (* threadIdx.x is warp-local *)
  (Variable.equal x Variable.tid_x && tid_x_is_warp_uniform cfg)
  (* threadIdx.y is warp-local *)
  || (Variable.equal x Variable.tid_y && tid_y_is_warp_uniform cfg)
  (* threadIdx.z is warp-local *)
  || (Variable.equal x Variable.tid_z && tid_z_is_warp_uniform cfg)

let is_warp_divergent (x : Variable.t) (cfg : t) : bool =
  not (is_warp_uniform x cfg)

let add_when (b : bool) (x : Variable.t) (l : Variable.t list) : Variable.t list
    =
  if b then x :: l else l

let warp_uniform_tid_list (cfg : t) : Variable.t list =
  []
  |> add_when (tid_x_is_warp_uniform cfg) Variable.tid_x
  |> add_when (tid_y_is_warp_uniform cfg) Variable.tid_y
  |> add_when (tid_z_is_warp_uniform cfg) Variable.tid_z

let warp_divergent_tid_list (cfg : t) : Variable.t list =
  []
  |> add_when (tid_x_is_warp_divergent cfg) Variable.tid_x
  |> add_when (tid_y_is_warp_divergent cfg) Variable.tid_y
  |> add_when (tid_z_is_warp_divergent cfg) Variable.tid_z

let warp_divergent_tid_set (cfg : t) : Variable.Set.t =
  cfg |> warp_divergent_tid_list |> Variable.Set.of_list

let warp_uniform_tid_set (cfg : t) : Variable.Set.t =
  cfg |> warp_uniform_tid_list |> Variable.Set.of_list

(** Returns the memory segment size in bits used for memory transaction
    granularity.

    In GPU memory coalescing analysis, memory transactions occur at specific
    granularities. For uncoalesced access analysis, we need to determine how
    many distinct memory segments are accessed by a warp's memory transactions.

    The memory segment size represents the fundamental unit of memory
    transaction granularity used in the analysis. This is computed as 8 *
    bytes_per_word where bytes_per_word (default 4) represents the size of the
    basic data type being accessed.

    @param cfg Configuration containing bytes_per_word field
    @return Memory segment size in bits for memory transaction analysis *)
let memory_segments_bits (cfg : t) : int =
  8 * cfg.bytes_per_word (* 8 represents number of bits per byte *)

let make ?(bank_count = 32) ?(threads_per_warp = 32) ?(bytes_per_word = 4)
    ~block_dim ~grid_dim () : t =
  { block_dim; grid_dim; threads_per_warp; bank_count; bytes_per_word }
