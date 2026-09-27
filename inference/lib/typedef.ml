open Protocols
open Stage0

type t = { alias : Ty.t; ty : Ty.t; location : Location.t }

let location (d : t) : Location.t = d.location

let to_string (d : t) =
  "typedef " ^ Ty.to_string d.ty ^ " " ^ Ty.to_string d.alias
let to_s (d : t) : Indent.t list = [ Line (to_string d ^ ";") ]
