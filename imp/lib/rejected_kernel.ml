module Reason = struct
  type t =
    | RecursiveCall of { path : string list }
    | UndefinedKernel of { path : string list }
    | ManyRegions of { region : string }
    | UnnamedRegion of { region : string }

  let to_string : t -> string = function
    | RecursiveCall { path } ->
        "recursion through " ^ String.concat " -> " path
    | UndefinedKernel { path } ->
        "calls a function with no visible body, through "
        ^ String.concat " -> " path
    | ManyRegions { region } ->
        "reads a stored pointer from a cell that is not decided statically, "
        ^ "so " ^ region ^ " names a family of regions rather than one"
    | UnnamedRegion { region } ->
        "reaches " ^ region ^ " through a stored pointer, and the memory "
        ^ "behind that pointer has no region of its own"

  let label : t -> string = function
    | RecursiveCall _ -> "recursive-call"
    | UndefinedKernel _ -> "undefined-kernel"
    | ManyRegions _ -> "many-regions"
    | UnnamedRegion _ -> "unnamed-region"

  let path : t -> string list = function
    | RecursiveCall { path } | UndefinedKernel { path } -> path
    | ManyRegions _ | UnnamedRegion _ -> []
end

type t = { kernel : string; reason : Reason.t }

let make ~(kernel : string) ~(reason : Reason.t) : t = { kernel; reason }

let to_string (r : t) : string =
  "kernel '" ^ r.kernel ^ "' was discarded: " ^ Reason.to_string r.reason

let to_json (r : t) : Yojson.Basic.t =
  `Assoc
    [
      ("kernel_name", `String r.kernel);
      ("reason", `String (Reason.label r.reason));
      ( "path",
        `List (Reason.path r.reason |> List.map (fun (x : string) -> `String x))
      );
    ]
