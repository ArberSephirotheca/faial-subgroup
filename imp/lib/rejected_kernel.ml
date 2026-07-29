module Reason = struct
  type t =
    | RecursiveCall of { path : string list }
    | UndefinedKernel of { path : string list }

  let to_string : t -> string = function
    | RecursiveCall { path } ->
        "recursion through " ^ String.concat " -> " path
    | UndefinedKernel { path } ->
        "calls a function with no visible body, through "
        ^ String.concat " -> " path
end

type t = { kernel : string; reason : Reason.t }

let make ~(kernel : string) ~(reason : Reason.t) : t = { kernel; reason }

let to_string (r : t) : string =
  "kernel '" ^ r.kernel ^ "' was discarded: " ^ Reason.to_string r.reason
