open Protocols

type t = {
  path : Path.t;
  index : Exp.nexp list;
  mode : Access.Mode.t;
  id : Access.Id.t;
}

let make ~(path : Path.t) ~(index : Exp.nexp list) ~(mode : Access.Mode.t) : t =
  { path; index; mode; id = Access.Id.unstamped }

let from_array ~(array : Variable.t) ~(index : Exp.nexp list)
    ~(mode : Access.Mode.t) : t =
  make ~path:(Path.root array) ~index ~mode

let write (array : Variable.t) (index : Exp.nexp list) (v : int option) : t =
  from_array ~array ~index ~mode:(Write v)

let read (array : Variable.t) (index : Exp.nexp list) : t =
  from_array ~array ~index ~mode:Read

let atomic ~array ~atomic (index : Exp.nexp list) : t option =
  Atomic.from_name atomic
  |> Option.map (fun a -> from_array ~array ~index ~mode:(Atomic a))

let array (x : t) : Variable.t = Path.to_variable x.path
let root (x : t) : Variable.t = Path.base x.path
let location (x : t) : Stage0.Location.t = Variable.location (Path.base x.path)
let is_write (x : t) : bool = Access.Mode.is_write x.mode
let is_read (x : t) : bool = Access.Mode.is_read x.mode
let map (f : Exp.nexp -> Exp.nexp) (a : t) : t = { a with index = List.map f a.index }

let index_intersects (s : Variable.Set.t) (a : t) : bool =
  List.exists (Exp.n_intersects s) a.index

let free_names (a : t) (fns : Variable.Set.t) : Variable.Set.t =
  List.fold_right Exp.n_free_names a.index fns

let to_access (a : t) : Access.t =
  { Access.array = array a; index = a.index; mode = a.mode; id = a.id }

let to_string (a : t) : string = Access.to_string (to_access a)
