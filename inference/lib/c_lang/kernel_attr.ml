open Ast

type t = Default | Auxiliary

let to_string : t -> string = function
  | Default -> "__global__"
  | Auxiliary -> "__device__"

let is_global : t -> bool = function Default -> true | Auxiliary -> false
let is_device : t -> bool = function Default -> false | Auxiliary -> true

let parse (x : string) : t option =
  if x = c_attr_global then Some Default
  else if x = c_attr_device then Some Auxiliary
  else None

let can_parse (x : string) : bool = parse x |> Option.is_some
