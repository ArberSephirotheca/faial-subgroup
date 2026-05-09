(* Logger interface. Each method takes a [unit -> string] thunk so
   modules whose log level discards a category (e.g. [Warnings.info])
   pay nothing — the message string is only built when the implementation
   actually consumes it. Call sites should wrap their argument in
   [fun () -> ...] when it does any non-trivial concatenation, sprintf,
   or to_string work. *)

module type Logger = sig
  val error : (unit -> string) -> unit
  val warning : (unit -> string) -> unit
  val info : (unit -> string) -> unit
end

module Default : Logger = struct
  let info f = prerr_endline ("INFO: " ^ f ())
  let warning f = prerr_endline ("WARNING: " ^ f ())
  let error f = prerr_endline ("ERROR: " ^ f ())
end

module Colors : Logger = struct
  let info f =
    let open ANSITerminal in
    prerr_string [ Foreground Magenta ] ("INFO: " ^ f () ^ "\n")

  let warning f =
    let open ANSITerminal in
    prerr_string [ Foreground Yellow ] ("WARNING: " ^ f () ^ "\n")

  let error f =
    let open ANSITerminal in
    prerr_string [ Bold; Foreground Red ] ("ERROR: " ^ f () ^ "\n")
end

module Warnings : Logger = struct
  let info _ = ()
  let warning f = prerr_endline ("WARNING: " ^ f ())
  let error f = prerr_endline ("ERROR: " ^ f ())
end

module WarningsColors : Logger = struct
  let info _ = ()

  let warning f =
    let open ANSITerminal in
    prerr_string [ Foreground Yellow ] ("WARNING: " ^ f () ^ "\n")

  let error f =
    let open ANSITerminal in
    prerr_string [ Bold; Foreground Red ] ("ERROR: " ^ f () ^ "\n")
end

module Silent : Logger = struct
  let info _ = ()
  let warning _ = ()
  let error _ = ()
end
