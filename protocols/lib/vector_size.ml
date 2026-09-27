type t = One | Two | Three | Four

let to_int : t -> int = function One -> 1 | Two -> 2 | Three -> 3 | Four -> 4

let of_int : int -> t option = function
  | 1 -> Some One
  | 2 -> Some Two
  | 3 -> Some Three
  | 4 -> Some Four
  | _ -> None

let to_string (x : t) : string = x |> to_int |> string_of_int

(* CUDA vector types ([uint2], [int3], [float4], ...) and WGSL vectors name
   their lanes [x], [y], [z], [w] in order. *)
let lanes : t -> string list = function
  | One -> [ "x" ]
  | Two -> [ "x"; "y" ]
  | Three -> [ "x"; "y"; "z" ]
  | Four -> [ "x"; "y"; "z"; "w" ]
