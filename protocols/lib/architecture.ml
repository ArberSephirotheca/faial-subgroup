open Exp

type t = Grid | Block

let to_string : t -> string = function Grid -> "grid" | Block -> "block"
let is_grid (x : t) : bool = x = Grid

module Defaults = struct
  type t = { globals : Params.t; locals : Params.t; distinct : bexp }

  let base : bexp =
    let idx_lt_dim : bexp =
      [
        (Variable.tid_x, Variable.bdim_x);
        (Variable.tid_y, Variable.bdim_y);
        (Variable.tid_z, Variable.bdim_z);
        (Variable.bid_x, Variable.gdim_x);
        (Variable.bid_y, Variable.gdim_y);
        (Variable.bid_z, Variable.gdim_z);
      ]
      |> List.map (fun (x, y) -> n_lt (Var x) (Var y))
      |> b_and_ex
    in
    let idx_ge_0 : bexp =
      [
        Variable.tid_x;
        Variable.tid_y;
        Variable.tid_z;
        Variable.bid_x;
        Variable.bid_y;
        Variable.bid_z;
      ]
      |> List.map (fun x -> n_ge (Var x) (Num 0))
      |> b_and_ex
    in
    let dim_ge_1 : bexp =
      [
        Variable.bdim_x;
        Variable.bdim_y;
        Variable.bdim_z;
        Variable.gdim_x;
        Variable.gdim_y;
        Variable.gdim_z;
      ]
      |> List.map (fun x -> n_ge (Var x) (Num 1))
      |> b_and_ex
    in
    (* CUDA's [warpSize] is a compile-time constant of 32 on every
       NVIDIA architecture. c-to-json picks it up as an [extern
       const int], leaving it free in the SMT and letting Z3 witness
       racy alignments at unreasonable values like [warpSize == 1021].
       Pin it at the architecture level so every kernel inherits the
       constant. *)
    let warp_size_eq_32 : bexp =
      n_eq (Var (Variable.from_name "warpSize")) (Num 32)
    in
    b_and_ex [ idx_lt_dim; idx_ge_0; dim_ge_1; warp_size_eq_32 ]

  let block : t =
    {
      globals =
        Variable.Set.empty
        |> Variable.Set.union Variable.bid_set
        |> Variable.Set.union Variable.bdim_set
        |> Variable.Set.union Variable.gdim_set
        |> Params.from_set Ty.unsigned_int;
      locals = Variable.tid_set |> Params.from_set Ty.unsigned_int;
      distinct : bexp = is_thread_distinct Variable.tid_list;
    }

  let grid : t =
    {
      globals =
        Variable.Set.empty
        |> Variable.Set.union Variable.bdim_set
        |> Variable.Set.union Variable.gdim_set
        |> Params.from_set Ty.unsigned_int;
      locals =
        Variable.bid_set
        |> Variable.Set.union Variable.tid_set
        |> Params.from_set Ty.unsigned_int;
      distinct : bexp = is_thread_distinct Variable.bid_list;
    }

  let to_bexp (e : t) : bexp = b_and e.distinct base
end

let to_defaults : t -> Defaults.t = function
  | Grid -> Defaults.grid
  | Block -> Defaults.block
