open Inference
open Stage0

module JUI = struct
  open Yojson.Basic

  type json = Yojson.Basic.t

  let to_json (kernels : Barrier.Kernel.t list) : json =
    let kernels_json =
      `List
        (kernels
        |> List.map (fun k ->
            let is_unif = Barrier.Kernel.is_uniform k in
            let divs =
              Barrier.Kernel.divergent k
              |> List.filter_map (fun loc ->
                  loc
                  |> Option.map (fun loc -> `String (Location.to_string loc)))
            in
            `Assoc
              [
                ("name", `String k.name);
                ("is_uniform", `Bool is_unif);
                ("divergent", `List divs);
              ]))
    in
    `Assoc
      [
        ("kernels", kernels_json);
        ( "argv",
          `List (Sys.argv |> Array.to_list |> List.map (fun s -> `String s)) );
        ("executable_name", `String Sys.executable_name);
      ]

  let run (protocol_kernels : Protocols.Kernel.t list) : unit =
    protocol_kernels
    |> List.map Barrier.Kernel.from_proto
    |> to_json |> to_string |> print_endline
end

module TUI = struct
  let run (protocol_kernels : Protocols.Kernel.t list) : unit =
    protocol_kernels
    |> List.iter (fun k ->
        let k = Barrier.Kernel.from_proto k in
        let is_unif = Barrier.Kernel.is_uniform k in
        print_endline (k.name ^ ": " ^ if is_unif then "true" else "false");
        Barrier.Kernel.divergent k
        |> List.iter (fun l ->
            l |> Option.iter (fun i -> Stage0.Tui_helper.LocationUI.print i)))
end

let main (fname : string) (ignore_parsing_errors : bool) (output_json : bool) :
    unit =
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      fname
  in
  if output_json then JUI.run parsed.kernels else TUI.run parsed.kernels

open Cmdliner

let get_fname : string Term.t =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let ignore_parsing_errors : bool Term.t =
  let doc = "Ignore parsing errors." in
  Arg.(value & flag & info [ "ignore-parsing-errors" ] ~doc)

let output_json : bool Term.t =
  let doc = "Output in JSON format." in
  Arg.(value & flag & info [ "json" ] ~doc)

let main_t : unit Term.t =
  Term.(const main $ get_fname $ ignore_parsing_errors $ output_json)

let info =
  let doc = "Check for barrier divergence errors" in
  Cmd.info "faial-sync" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
