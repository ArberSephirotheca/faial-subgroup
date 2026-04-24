open Inference
open Stage0

let _ = Analysis.Check.of_kernel

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
  module T = ANSITerminal

  let print_box : PrintBox.t -> unit = PrintBox_text.output stdout

  let render_tasks (w : Analysis.Proof.Witness.t) : PrintBox.t =
    let open PrintBox in
    let t1 = List.to_seq w.t1_locals |> Hashtbl.of_seq in
    let t2 = List.to_seq w.t2_locals |> Hashtbl.of_seq in
    let keys =
      List.map fst w.t1_locals @ List.map fst w.t2_locals
      |> List.sort_uniq String.compare
    in
    let header =
      [| text_with_style Style.bold "";
         text_with_style Style.bold "T1";
         text_with_style Style.bold "T2" |]
    in
    let row k =
      let lookup h = Hashtbl.find_opt h k |> Option.value ~default:"?" in
      [| text k; text (lookup t1); text (lookup t2) |]
    in
    header :: List.map row keys |> Array.of_list |> grid |> frame

  let render_globals (w : Analysis.Proof.Witness.t) : PrintBox.t =
    let open PrintBox in
    w.globals
    |> List.map (fun (k, v) -> [| text k; text v |])
    |> Array.of_list |> grid |> frame

  let print_witness (m : Z3.Model.model) : unit =
    let w = Analysis.Proof.Witness.parse m in
    T.print_string [ T.Bold ] "Locals\n";
    print_box (render_tasks w);
    T.print_string [ T.Bold ] "\nGlobals\n";
    print_box (render_globals w);
    print_endline ""

  let check_proof (p : Analysis.Proof.t) : unit =
    let open Protocols.Gen_z3.Solver in
    let loc =
      match p.barrier.loc with
      | Some l -> Location.to_string l
      | None -> "<unknown location>"
    in
    let header =
      Printf.sprintf "[%s] %s @ %s" p.kernel_name
        (Protocols.Sync.to_string p.barrier) loc
    in
    match Analysis.Proof.solve p with
    | Ok Unsat ->
        T.print_string [ T.Foreground T.Green ] (header ^ ": divergence-free\n")
    | Ok (Sat m) ->
        T.print_string [ T.Bold; T.Foreground T.Red ]
          (header ^ ": POTENTIAL DIVERGENCE\n");
        (match p.barrier.loc with
         | Some l -> Stage0.Tui_helper.LocationUI.print l
         | None -> ());
        print_witness m
    | Error e -> print_endline (header ^ ": solver error: " ^ e)

  let run ~show_map ~show_check ~show_symbexp
      (protocol_kernels : Protocols.Kernel.t list) : unit =
    protocol_kernels
    |> List.iter (fun k ->
        if show_map then Protocols.Kernel.print k;
        let check = Analysis.Check.of_kernel k in
        if show_check then Analysis.Check.print check;
        let proofs = Analysis.Proof.of_check check in
        if show_symbexp then Analysis.Proof.print_seq proofs;
        Seq.iter check_proof proofs;
        let k = Barrier.Kernel.from_proto k in
        let is_unif = Barrier.Kernel.is_uniform k in
        print_endline (k.name ^ ": " ^ if is_unif then "true" else "false");
        Barrier.Kernel.divergent k
        |> List.iter (fun l ->
            l |> Option.iter (fun i -> Stage0.Tui_helper.LocationUI.print i)))
end

let main (fname : string) (ignore_parsing_errors : bool) (output_json : bool)
    (show_map : bool) (show_check : bool) (show_symbexp : bool) : unit =
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      fname
  in
  if output_json then JUI.run parsed.kernels
  else TUI.run ~show_map ~show_check ~show_symbexp parsed.kernels

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

let show_map : bool Term.t =
  let doc = "Show the MAP kernel." in
  Arg.(value & flag & info [ "show-map" ] ~doc)

let show_check : bool Term.t =
  let doc = "Show the Check — barriers with their path conditions." in
  Arg.(value & flag & info [ "show-check" ] ~doc)

let show_symbexp : bool Term.t =
  let doc = "Show the generated proof obligations." in
  Arg.(value & flag & info [ "show-symbexp" ] ~doc)

let main_t : unit Term.t =
  Term.(
    const main $ get_fname $ ignore_parsing_errors $ output_json $ show_map
    $ show_check $ show_symbexp)

let info =
  let doc = "Check for barrier divergence errors" in
  Cmd.info "faial-sync" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
