type t = { x : int; y : int; z : int }

let make ?(x = 1) ?(y = 1) ?(z = 1) () : t = { x; y; z }
let one : t = { x = 1; y = 1; z = 1 }
let zero : t = { x = 0; y = 0; z = 0 }
let total (e : t) : int = e.x * e.y * e.z
let set_x (x : int) (d : t) : t = { d with x }
let set_y (y : int) (d : t) : t = { d with y }
let set_z (z : int) (d : t) : t = { d with z }

let from_json ?(default = one) (j : Yojson.Basic.t) : (t, string) Result.t =
  match j with
  | `List [ `Int x; `Int y; `Int z ] -> Ok { x; y; z }
  | `List [ `Int x; `Int y ] -> Ok { default with x; y }
  | `List [ `Int x ] | `Int x -> Ok { default with x }
  | `List [] -> Ok default
  | _ -> Error "Expecting a number of a list of up to 3 numbers (eg, [x,y,z])"

let parse ?(default = one) (s : string) : (t, string) Result.t =
  try Yojson.Basic.from_string s |> from_json ~default
  with _ -> Error "Error parsing dim3"

let to_list (d : t) : int list = [ d.x; d.y; d.z ]

let compare (p1 : t) (p2 : t) : int =
  let c = compare p1.x p2.x in
  if c <> 0 then c
  else
    let c = compare p1.y p2.y in
    if c <> 0 then c else compare p1.z p2.z

let to_string (d : t) : string =
  match d with
  | { x; y = 1; z = 1 } -> string_of_int x
  | { x; y; z = 1 } -> "[" ^ string_of_int x ^ ", " ^ string_of_int y ^ "]"
  | _ ->
      let x = string_of_int d.x in
      let y = string_of_int d.y in
      let z = string_of_int d.z in
      "[" ^ x ^ ", " ^ y ^ ", " ^ z ^ "]"

let to_assoc ?(prefix = "") (d : t) : (string * int) list =
  [ (prefix ^ "x", d.x); (prefix ^ "y", d.y); (prefix ^ "z", d.z) ]
