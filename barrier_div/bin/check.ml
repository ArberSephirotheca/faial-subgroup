open Inference
open Stage0
open Barrier_div

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

  let print_divergence ~index (p : Analysis.Proof.t) (m : Z3.Model.model) : unit =
    T.print_string
      [ T.Bold; T.Foreground T.Blue ]
      ("\n~~~~ Barrier divergence " ^ string_of_int (index + 1) ^ " ~~~~\n\n");
    (match p.barrier.loc with
     | Some l -> Stage0.Tui_helper.LocationUI.print l
     | None -> print_endline "<unknown location>");
    print_endline "";
    print_witness m;
    T.print_string [ T.Underlined ]
      ("(proof #" ^ string_of_int p.id ^ ")\n")

  let check_kernel (k : Protocols.Kernel.t) ~show_map ~show_check ~show_symbexp
      : bool =
    if show_map then Protocols.Kernel.print k;
    let check = Analysis.Check.of_kernel k in
    if show_check then Analysis.Check.print check;
    let proofs = Analysis.Proof.of_check check in
    if show_symbexp then Analysis.Proof.print_seq proofs;
    let outcomes =
      proofs
      |> Seq.map (fun p -> (p, Analysis.Proof.solve p))
      |> List.of_seq
    in
    let divergent =
      outcomes
      |> List.filter_map (fun (p, r) ->
          match r with
          | Ok (Protocols.Gen_z3.Solver.Sat m) -> Some (p, m)
          | _ -> None)
    in
    let errors =
      outcomes
      |> List.filter_map (fun (p, r) ->
          match r with Error e -> Some (p, e) | _ -> None)
    in
    match (divergent, errors) with
    | [], [] ->
        T.print_string
          [ T.Bold; T.Foreground T.Green ]
          ("Kernel '" ^ check.kernel_name ^ "' is well-synchronized!\n");
        true
    | _, _ ->
        let n = List.length divergent in
        let noun = if n = 1 then "divergence" else "divergences" in
        T.print_string
          [ T.Bold; T.Foreground T.Red ]
          ("Kernel '" ^ check.kernel_name ^ "' has " ^ string_of_int n
         ^ " potential " ^ noun ^ ".\n");
        List.iteri
          (fun index (p, m) -> print_divergence ~index p m)
          divergent;
        List.iter
          (fun (p, e) ->
            T.print_string
              [ T.Foreground T.Red ]
              ("solver error on proof #" ^ string_of_int p.Analysis.Proof.id
             ^ ": " ^ e ^ "\n"))
          errors;
        false

  let run ~show_map ~show_check ~show_symbexp
      (protocol_kernels : Protocols.Kernel.t list) : bool =
    protocol_kernels
    |> List.fold_left
         (fun all_safe k ->
           let safe = check_kernel ~show_map ~show_check ~show_symbexp k in
           all_safe && safe)
         true
end

let main (fname : string) (ignore_parsing_errors : bool) (output_json : bool)
    (show_map : bool) (show_check : bool) (show_symbexp : bool) : unit =
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      fname
  in
  if output_json then JUI.run parsed.kernels
  else if not (TUI.run ~show_map ~show_check ~show_symbexp parsed.kernels) then
    exit 1

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
