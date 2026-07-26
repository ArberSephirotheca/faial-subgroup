open Protocols
open Stage0

type t = { name : string; ty : Ty.t; location : Location.t }

let location (d : t) : Location.t = d.location
let to_string (d : t) = "typedef " ^ Ty.to_string d.ty ^ " " ^ d.name
let to_s (d : t) : Indent.t list = [ Line (to_string d ^ ";") ]
