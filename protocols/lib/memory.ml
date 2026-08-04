module Layout = struct
  type t = { offset : int; strides : int list }

  let make ~(offset : int) ~(strides : int list) : t = { offset; strides }

  let to_string (x : t) : string =
    "+" ^ string_of_int x.offset ^ " by ["
    ^ (x.strides |> List.map string_of_int |> String.concat ", ")
    ^ "]"
end

type t = {
  hierarchy : Mem_hierarchy.t;
  (* One entry per dimension, outermost first. [None] is an extent the
     source does not give, which C allows only on the outermost one and
     which a pointer level always supplies. *)
  size : int option list;
  data_type : string list; (* Empty means unknown *)
  layout : Layout.t option;
}

(* The extents as every consumer written before per-dimension extents
   reads them: all of them, or none when any is missing. *)
let known_size (x : t) : int list =
  if List.exists Option.is_none x.size then []
  else List.filter_map Fun.id x.size

let is_global (x : t) : bool = Mem_hierarchy.is_global x.hierarchy
let is_shared (x : t) : bool = Mem_hierarchy.is_shared x.hierarchy
let is_constant (x : t) : bool = Mem_hierarchy.is_constant x.hierarchy
let hierarchy (x : t) : Mem_hierarchy.t = x.hierarchy

let make (h : Mem_hierarchy.t) : t =
  { hierarchy = h; size = []; data_type = []; layout = None }

let layout (x : t) : Layout.t option = x.layout

let from_type (h : Mem_hierarchy.t) (ty : Ty.t) : t =
  {
    hierarchy = h;
    size = Ty.get_array_dims ty;
    data_type = Ty.get_array_type ty;
    layout = None;
  }

let data_ty (x : t) : Ty.t = x.data_type |> String.concat " " |> Ty.of_c_string

(* [data_ty] was already stripped by [Ty.get_array_type], so this reads
   [Ty.width] rather than [Ty.pointee_size], and answers for a
   multi-dimensional array too: its cells are its scalars, and a flat byte
   address retyped into them is split back across the axes afterwards. *)
let step (x : t) : int option = x |> data_ty |> Ty.width

let make_map (h : Mem_hierarchy.t) (vs : Variable.t list) : t Variable.Map.t =
  vs |> List.map (fun x -> (x, make h)) |> Variable.Map.of_list

let to_string (a : t) : string =
  let ty = a.data_type |> String.concat " " in
  let ty = if ty = "" then "" else ty ^ "  " in
  let size =
    a.size
    |> List.map (function Some n -> string_of_int n | None -> "?")
    |> String.concat ", "
  in
  let h = a.hierarchy |> Mem_hierarchy.to_string in
  h ^ " " ^ ty ^ "[" ^ size ^ "]"

let map_to_string (vs : t Variable.Map.t) : string =
  Variable.Map.bindings vs
  |> List.map (fun (k, v) -> Variable.name k ^ ": " ^ to_string v)
  |> String.concat ", "
