open Stage0 (* Loads Location.t *)

type t = { name : string; label : string option; location : Location.t option }

let make ~name ~location : t = { name; label = None; location = Some location }
let from_name (name : string) : t = { name; label = None; location = None }
let compare (x1 : t) (x2 : t) : int = String.compare x1.name x2.name
let label (x : t) = match x.label with Some l -> l | None -> x.name
let label_opt (x : t) = x.label
let set_label (label : string) (x : t) : t = { x with label = Some label }
let tid_x : t = from_name "threadIdx.x"
let tid_y : t = from_name "threadIdx.y"
let tid_z : t = from_name "threadIdx.z"
let bid_x : t = from_name "blockIdx.x"
let bid_y : t = from_name "blockIdx.y"
let bid_z : t = from_name "blockIdx.z"
let bdim_x : t = from_name "blockDim.x"
let bdim_y : t = from_name "blockDim.y"
let bdim_z : t = from_name "blockDim.z"
let gdim_x : t = from_name "gridDim.x"
let gdim_y : t = from_name "gridDim.y"
let gdim_z : t = from_name "gridDim.z"
let update_name (f : string -> string) (v : t) : t = { v with name = f v.name }
let add_suffix (suffix : string) (v : t) : t = { v with name = v.name ^ suffix }
let set_name (name : string) : t -> t = update_name (fun _ -> name)

let set_location (location : Location.t) (v : t) : t =
  { v with location = Some location }

let clear_location (v : t) = { v with location = None }
let name (x : t) : string = x.name
let location_opt (x : t) : Location.t option = x.location

let location (x : t) : Location.t =
  location_opt x |> Option.value ~default:Location.empty

let equal (x : t) (y : t) = String.equal x.name y.name

let name_line (x : t) : string =
  let l : string =
    match location_opt x with
    | Some l -> ":" ^ (l |> Location.line |> Index.to_base1 |> string_of_int)
    | None -> ""
  in
  name x ^ l

let repr (x : t) : string =
  let l = Option.map Location.repr x.location |> Option.value ~default:"null" in
  "{name=\"" ^ x.name ^ "\", location=" ^ l ^ "}"

module OT = struct
  type t' = t
  type t = t'

  let compare = fun x y -> String.compare x.name y.name
end

module Set = Set.Make (OT)
module Map = Map.Make (OT)
module MapUtil = Common.MapUtil (Map)
module MapSetUtil = Common.MapSetUtil (Set) (Map)

let map_to_string ?(sep = ": ") (f : 'a -> string) (m : 'a Map.t) : string =
  let keys =
    m |> Map.to_list
    |> List.map (fun (k, v) -> k.name ^ sep ^ f v)
    |> String.concat ", "
  in
  "{" ^ keys ^ "}"

let list_to_string (vs : t list) : string =
  vs |> List.map name |> String.concat ", "

let set_to_string (vs : Set.t) : string =
  Set.elements vs
  |> List.sort (fun a b -> String.compare a.name b.name)
  |> list_to_string

(** Given a variable and a set of known variables, returns a fresh variable
    name. *)

let fresh (xs : Set.t) (x : t) : t =
  let rec do_fresh_name x n =
    let new_x =
      let name = x.name ^ string_of_int n in
      { x with name }
    in
    if Set.mem new_x xs then do_fresh_name x (n + 1) else new_x
  in
  if Set.mem x xs then do_fresh_name x 1 else x

let is_tid (x : t) : bool = equal x tid_x || equal x tid_y || equal x tid_z
let tid_list : t list = [ tid_x; tid_y; tid_z ]
let tid_set : Set.t = Set.of_list tid_list
let bid_list : t list = [ bid_x; bid_y; bid_z ]
let bid_set : Set.t = Set.of_list bid_list
let bdim_list : t list = [ bdim_x; bdim_y; bdim_z ]
let bdim_set : Set.t = Set.of_list bdim_list
let gdim_list : t list = [ gdim_x; gdim_y; gdim_z ]
let gdim_set : Set.t = Set.of_list gdim_list

let contains_tids (vs : Set.t) : bool =
  Set.mem tid_x vs || Set.mem tid_y vs || Set.mem tid_z vs
