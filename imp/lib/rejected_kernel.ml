module Location = Stage0.Location

module Reason = struct
  type t =
    | RecursiveCall of { path : string list }
    | UndefinedKernel of { path : string list }
    | RuntimePointerField of { location : Location.t }
    | PointerFieldToRecord of { location : Location.t }
    | WriteThroughCall of { location : Location.t }

  let to_string : t -> string = function
    | RecursiveCall _ -> "recursive calls are unsupported"
    | UndefinedKernel _ ->
        "called a function that cannot be analyzed (missing function body)"
    | RuntimePointerField _ ->
        "unsupported field access"
    | PointerFieldToRecord _ ->
        "unsupported field access"
    | WriteThroughCall _ -> "unsupported assignment target"

  let hint : t -> string option = function
    | RecursiveCall { path } ->
        Some ("Call cycle is " ^ String.concat " -> " path ^ ".")
    | UndefinedKernel { path } ->
        Some
          ("Reached through " ^ String.concat " -> " path
         ^ ". Consider including the definition by analyzing several files at \
            once, or pass --opaque-calls=skip-all to ignore every such call.")
    | RuntimePointerField _ ->
        Some
          "A pointer stored in a struct is followed only when faial can tell \
           which struct holds it, so every subscript on the way to the field \
           has to be a constant."
    | PointerFieldToRecord _ ->
        Some
          "The fields of a struct reached through a pointer field are not \
           tracked. A pointer field that points to a scalar, such as int *, is \
           supported."
    | WriteThroughCall _ ->
        Some
          "Assigning to what a function returns needs the location it returns, \
           and a function is analyzed for the value it returns. Assign through \
           the pointer or the array itself."

  let label : t -> string = function
    | RecursiveCall _ -> "recursive-call"
    | UndefinedKernel _ -> "undefined-kernel"
    | RuntimePointerField _ -> "runtime-pointer-field"
    | PointerFieldToRecord _ -> "pointer-field-to-struct"
    | WriteThroughCall _ -> "write-through-call"

  let path : t -> string list = function
    | RecursiveCall { path } | UndefinedKernel { path } -> path
    | RuntimePointerField _ | PointerFieldToRecord _ | WriteThroughCall _ -> []

  let location : t -> Location.t option = function
    | RecursiveCall _ | UndefinedKernel _ -> None
    | RuntimePointerField { location }
    | PointerFieldToRecord { location }
    | WriteThroughCall { location } ->
        Some location
end

type t = { kernel : string; reason : Reason.t }

let make ~(kernel : string) ~(reason : Reason.t) : t = { kernel; reason }

let to_string (r : t) : string =
  "kernel '" ^ r.kernel ^ "' is unsupported: " ^ Reason.to_string r.reason

let to_json (r : t) : Yojson.Basic.t =
  `Assoc
    ([
       ("kernel_name", `String r.kernel);
       ("reason", `String (Reason.label r.reason));
       ( "path",
         `List (Reason.path r.reason |> List.map (fun (x : string) -> `String x))
       );
     ]
    @
    match Reason.location r.reason with
    | Some l -> [ ("location", Location.to_json l) ]
    | None -> [])
