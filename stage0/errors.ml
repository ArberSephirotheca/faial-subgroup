module Reason = struct
  type t =
    | UnexpectedProcessStatus of Unix.process_status
    | UnexpectedOutput
    | UnsupportedInput

  let to_string : t -> string = function
    | UnexpectedProcessStatus r ->
        "Unexpected process status: " ^ Subprocess.process_status_to_string r
    | UnexpectedOutput -> "Could not parse tool's output"
    | UnsupportedInput -> "Cannot handle the given RA problem"
end

type t = { output : string; reason : Reason.t }

let to_string (err : t) : string =
  Reason.to_string err.reason ^ "\nProcess output:\n" ^ err.output

let handle_result (parse_data : string -> string option)
    (r : Subprocess.Completed1.t) : (string, t) Result.t =
  if Subprocess.Completed1.is_ok r then
    match parse_data r.output with
    | Some data -> Ok data
    | None -> Error { output = r.output; reason = UnexpectedOutput }
  else Error { output = r.output; reason = UnexpectedProcessStatus r.status }
