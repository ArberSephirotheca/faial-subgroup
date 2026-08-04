module Location = Stage0.Location

module Reason = struct
  type t =
    | RecursiveCall of { path : string list }
    | UndefinedKernel of { path : string list }
    | UnnamedRegion of { location : Location.t; region : string }
    | CallArity of { callee : string; parameters : int; arguments : int }
    | WriteThroughCall of { location : Location.t }

  let to_string : t -> string = function
    | RecursiveCall _ -> "recursive calls are unsupported"
    | UndefinedKernel _ ->
        "called a function that cannot be analyzed (missing function body)"
    | UnnamedRegion _ -> "unsupported field access"
    | CallArity _ -> "unsupported call"
    | WriteThroughCall _ -> "unsupported assignment target"

  let hint : t -> string option = function
    | RecursiveCall { path } ->
        Some ("Call cycle is " ^ String.concat " -> " path ^ ".")
    | UndefinedKernel { path } ->
        Some
          ("Reached through " ^ String.concat " -> " path
         ^ ". Consider including the definition by analyzing several files at \
            once, or pass --opaque-calls=skip-all to ignore every such call.")
    | UnnamedRegion { region; _ } ->
        Some
          ("The memory " ^ region
         ^ " is reached from an array faial knows, and faial cannot say which \
            region it is. Analyzing the kernel without it would answer for a \
            program with that access missing.")
    | CallArity { callee; parameters; arguments } ->
        Some
          (Printf.sprintf
             "Binding %s's parameters to this call's arguments lines up %d \
              against %d. A parameter that points at a struct stands for one \
              array per member of that struct, and an argument is read the \
              same way, so the two agree unless the argument's type is not \
              the parameter's, which is what a cast at the call site does."
             callee parameters arguments)
    | WriteThroughCall _ ->
        Some
          "Assigning to what a function returns needs the location it returns, \
           and a function is analyzed for the value it returns. Assign through \
           the pointer or the array itself."

  let label : t -> string = function
    | RecursiveCall _ -> "recursive-call"
    | UndefinedKernel _ -> "undefined-kernel"
    | UnnamedRegion _ -> "unnamed-region"
    | CallArity _ -> "call-arity"
    | WriteThroughCall _ -> "write-through-call"

  let path : t -> string list = function
    | RecursiveCall { path } | UndefinedKernel { path } -> path
    | UnnamedRegion _ | WriteThroughCall _ | CallArity _ -> []

  let location : t -> Location.t option = function
    | RecursiveCall _ | UndefinedKernel _ | CallArity _ -> None
    | UnnamedRegion { location; _ } | WriteThroughCall { location } ->
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
