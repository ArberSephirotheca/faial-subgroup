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
    b_and_ex [ idx_lt_dim; idx_ge_0; dim_ge_1 ]

  let block : t =
    {
      globals =
        Variable.Set.empty
        |> Variable.Set.union Variable.bid_set
        |> Variable.Set.union Variable.bdim_set
        |> Variable.Set.union Variable.gdim_set
        |> Params.from_set C_type.unsigned_int;
      locals = Variable.tid_set |> Params.from_set C_type.unsigned_int;
      distinct : bexp = thread_distinct Variable.tid_list;
    }

  let grid : t =
    {
      globals =
        Variable.Set.empty
        |> Variable.Set.union Variable.bdim_set
        |> Variable.Set.union Variable.gdim_set
        |> Params.from_set C_type.unsigned_int;
      locals =
        Variable.bid_set
        |> Variable.Set.union Variable.tid_set
        |> Params.from_set C_type.unsigned_int;
      distinct : bexp = thread_distinct Variable.bid_list;
    }

  let to_bexp (e : t) : bexp = b_and e.distinct base
end

let to_defaults : t -> Defaults.t = function
  | Grid -> Defaults.grid
  | Block -> Defaults.block
