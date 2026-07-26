type t = {
  hierarchy : Mem_hierarchy.t;
  size : int list; (* Empty means unknown *)
  data_type : string list; (* Empty means unknown *)
}

let is_global (x : t) : bool = Mem_hierarchy.is_global x.hierarchy
let is_shared (x : t) : bool = Mem_hierarchy.is_shared x.hierarchy
let hierarchy (x : t) : Mem_hierarchy.t = x.hierarchy

let make (h : Mem_hierarchy.t) : t =
  { hierarchy = h; size = []; data_type = [] }

let from_type (h : Mem_hierarchy.t) (ty : Ty.t) : t =
  {
    hierarchy = h;
    size = Ty.get_array_length ty;
    data_type = Ty.get_array_type ty;
  }

let make_map (h : Mem_hierarchy.t) (vs : Variable.t list) : t Variable.Map.t =
  vs |> List.map (fun x -> (x, make h)) |> Variable.Map.of_list

let to_string (a : t) : string =
  let ty = a.data_type |> String.concat " " in
  let ty = if ty = "" then "" else ty ^ "  " in
  let size = List.map string_of_int a.size |> String.concat ", " in
  let h = a.hierarchy |> Mem_hierarchy.to_string in
  h ^ " " ^ ty ^ "[" ^ size ^ "]"

let map_to_string (vs : t Variable.Map.t) : string =
  Variable.Map.bindings vs
  |> List.map (fun (k, v) -> Variable.name k ^ ": " ^ to_string v)
  |> String.concat ", "
