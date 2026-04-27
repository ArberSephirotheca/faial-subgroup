open Inference
open Stage0
open Protocols
open Barrier_div

(* Both verification properties are checked on every kernel by default.
   See barrier_div/src/analysis.ml: [Property.Well_sync] catches barriers
   whose reachability depends on thread-private state; [Property.Barrier_div]
   catches the GPUVerify litmus pattern (different threads in the same warp
   disagree on a guard wrapping a barrier). *)
let all_properties : Analysis.Property.t list =
  [
    Analysis.Property.Well_sync;
    Analysis.Property.Barrier_div;
    Analysis.Property.Missing_participants;
  ]

(* Selector for the [--check] flag. [Both] is the default and runs every
   property in [all_properties]; the singletons restrict to one. *)
type check_selector = Both | Only of Analysis.Property.t

let resolve_selector : check_selector -> Analysis.Property.t list = function
  | Both -> all_properties
  | Only p -> [ p ]

module JUI = struct
  open Yojson.Basic

  type json = Yojson.Basic.t

  let property_to_json (property : Analysis.Property.t)
      (k : Protocols.Kernel.t) : json =
    let check = Analysis.Check.of_kernel ~property k in
    let outcomes =
      Analysis.Proof.of_check check
      |> Seq.map (fun (p : Analysis.Proof.t) -> (p, Analysis.Proof.solve p))
      |> List.of_seq
    in
    let divergent_locs =
      outcomes
      |> List.filter_map (fun (p, r) ->
          match r with
          | Ok (Protocols.Gen_z3.Solver.Sat _) ->
              p.Analysis.Proof.barrier.loc
              |> Option.map (fun l -> `String (Location.to_string l))
          | _ -> None)
    in
    let has_non_unsat =
      outcomes
      |> List.exists (fun (_, r) ->
          match r with
          | Ok Protocols.Gen_z3.Solver.Unsat -> false
          | _ -> true)
    in
    `Assoc
      [
        ("property", `String (Analysis.Property.to_string property));
        ("is_uniform", `Bool (not has_non_unsat));
        ("divergent", `List divergent_locs);
      ]

  let kernel_to_json (properties : Analysis.Property.t list)
      (k : Protocols.Kernel.t) : json =
    `Assoc
      [
        ("name", `String k.name);
        ( "results",
          `List (List.map (fun p -> property_to_json p k) properties) );
      ]

  let run (properties : Analysis.Property.t list)
      (protocol_kernels : Protocols.Kernel.t list) : unit =
    let kernels_json =
      `List (List.map (kernel_to_json properties) protocol_kernels)
    in
    `Assoc
      [
        ("kernels", kernels_json);
        ( "argv",
          `List (Sys.argv |> Array.to_list |> List.map (fun s -> `String s)) );
        ("executable_name", `String Sys.executable_name);
      ]
    |> to_string |> print_endline
end

module TUI = struct
  module T = ANSITerminal

  let print_box : PrintBox.t -> unit = PrintBox_text.output stdout

  let render_paired_locals (t1_locals : (string * string) list)
      (t2_locals : (string * string) list) : PrintBox.t =
    let open PrintBox in
    let t1 = List.to_seq t1_locals |> Hashtbl.of_seq in
    let t2 = List.to_seq t2_locals |> Hashtbl.of_seq in
    let keys =
      List.map fst t1_locals @ List.map fst t2_locals
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

  let render_unary_locals (locals : (string * string) list) : PrintBox.t =
    let open PrintBox in
    locals
    |> List.map (fun (k, v) -> [| text k; text v |])
    |> Array.of_list |> grid |> frame

  let render_globals (globals : (string * string) list) : PrintBox.t =
    let open PrintBox in
    globals
    |> List.map (fun (k, v) -> [| text k; text v |])
    |> Array.of_list |> grid |> frame

  let print_witness (frame : Analysis.Property.frame) (m : Z3.Model.model) :
      unit =
    let w = Analysis.Proof.Witness.parse frame m in
    (match w with
     | Paired { t1_locals; t2_locals; globals } ->
         T.print_string [ T.Bold ] "Locals\n";
         print_box (render_paired_locals t1_locals t2_locals);
         T.print_string [ T.Bold ] "\nGlobals\n";
         print_box (render_globals globals)
     | Unary { locals; globals } ->
         T.print_string [ T.Bold ] "Failing thread\n";
         print_box (render_unary_locals locals);
         T.print_string [ T.Bold ] "\nGlobals\n";
         print_box (render_globals globals));
    print_endline ""

  (* Property-specific labels used in headers and counter-example banners.
     [ok_msg] is shown when the property is established; [fail_noun] is
     used to count counter-examples (e.g. "Kernel 'k' has 2 races"). *)
  let labels : Analysis.Property.t -> string * string * string = function
    | Well_sync ->
        ( "well-synchronized",
          "Barrier non-determinism",
          "non-deterministic barrier" )
    | Barrier_div ->
        ( "free of barrier divergence",
          "Barrier divergence",
          "divergent barrier" )
    | Missing_participants ->
        ( "fully attended at every barrier",
          "Missing participant",
          "barrier with a missing participant" )

  let print_counter_example ~banner ~index (p : Analysis.Proof.t)
      (m : Z3.Model.model) : unit =
    T.print_string
      [ T.Bold; T.Foreground T.Blue ]
      ("\n~~~~ " ^ banner ^ " " ^ string_of_int (index + 1) ^ " ~~~~\n\n");
    (match p.barrier.loc with
     | Some l -> Stage0.Tui_helper.LocationUI.print l
     | None -> print_endline "<unknown location>");
    print_endline "";
    print_witness (Analysis.Property.frame p.property) m;
    T.print_string [ T.Underlined ]
      ("(proof #" ^ string_of_int p.id ^ ")\n")

  let check_property (property : Analysis.Property.t)
      (k : Protocols.Kernel.t) ~show_check ~show_symbexp : bool =
    let ok_msg, banner, noun = labels property in
    let check = Analysis.Check.of_kernel ~property k in
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
    let label = Analysis.Property.to_string property in
    match (divergent, errors) with
    | [], [] ->
        T.print_string
          [ T.Bold; T.Foreground T.Green ]
          ("[" ^ label ^ "] Kernel '" ^ check.kernel_name ^ "' is "
           ^ ok_msg ^ "!\n");
        true
    | _, _ ->
        let n = List.length divergent in
        let plural = if n = 1 then "" else "s" in
        T.print_string
          [ T.Bold; T.Foreground T.Red ]
          ("[" ^ label ^ "] Kernel '" ^ check.kernel_name ^ "' has "
           ^ string_of_int n ^ " potential " ^ noun ^ plural ^ ".\n");
        List.iteri
          (fun index (p, m) -> print_counter_example ~banner ~index p m)
          divergent;
        List.iter
          (fun (p, e) ->
            T.print_string
              [ T.Foreground T.Red ]
              ("solver error on proof #" ^ string_of_int p.Analysis.Proof.id
             ^ ": " ^ e ^ "\n"))
          errors;
        false

  let check_kernel ~(properties : Analysis.Property.t list)
      (k : Protocols.Kernel.t) ~show_map ~show_check ~show_symbexp : bool =
    if show_map then Protocols.Kernel.print k;
    (* Run each requested check; report independently. Use [List.fold_left]
       (not [List.for_all]) so a failure on one property doesn't suppress
       reporting of the other. *)
    List.fold_left
      (fun all_safe property ->
        let safe =
          check_property property k ~show_check ~show_symbexp
        in
        all_safe && safe)
      true properties

  let run ~(properties : Analysis.Property.t list)
      ~show_map ~show_check ~show_symbexp
      (protocol_kernels : Protocols.Kernel.t list) : bool =
    protocol_kernels
    |> List.fold_left
         (fun all_safe k ->
           let safe =
             check_kernel ~properties k ~show_map ~show_check ~show_symbexp
           in
           all_safe && safe)
         true
end

(* Mirror the kernel-level preprocessing the [drf] driver does in
   [drf/bin/app.ml] [translate]: pin block/grid dimensions when known
   (so [inline_globals] can substitute concrete values), substitute
   integer parameters (-p key=val) into globals, fold dimensions,
   ensure every free name has a binder, and run constant folding so the
   analyser sees a simplified IR. *)
let preprocess ~(block_dim : Dim3.t option) ~(grid_dim : Dim3.t option)
    (params : (string * int) list) (k : Kernel.t) : Kernel.t =
  (* Pin launch dimensions, then bind the arch defaults and attach the
     [base] precondition before [inline_globals]. The order matters:
     [inline_globals] -> [subst_vars] substitutes blockDim/gridDim
     occurrences in both [code] and [pre], which only works if the
     [base] precondition is already in [pre] when the substitution
     runs. The analysis (Check.of_kernel) consumes the kernel as-is
     after this — it does not re-bind the arch defaults, which would
     undo the substitution by reintroducing free blockDim/gridDim
     globals. *)
  k
  |> Kernel.try_set_block_dim block_dim
  |> Kernel.try_set_grid_dim grid_dim
  |> Kernel.apply_arch_binders Architecture.Defaults.block
  |> (fun k -> { k with pre = Exp.b_and Architecture.Defaults.base k.pre })
  |> Kernel.inline_globals params
  |> Kernel.add_missing_binders
  |> Kernel.opt

let main (fname : string) (ignore_parsing_errors : bool) (output_json : bool)
    (show_map : bool) (show_check : bool) (show_symbexp : bool)
    (selector : check_selector) (block_dim : Dim3.t option)
    (grid_dim : Dim3.t option) (all_dims : bool)
    (macros : string list)
    (params : (string * int) list) : unit =
  if all_dims && (Option.is_some block_dim || Option.is_some grid_dim) then begin
    prerr_endline
      "Cannot run with options: --all-dims and --grid-dim/--block-dim.\n\
       Use --all-dims and -p instead.";
    exit 2
  end;
  let properties = resolve_selector selector in
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~block_dim ~grid_dim ~macros
      fname
  in
  (* parsed.options has merged the user overrides on top of any
     GPUVerify pragma in the source (and the parser defaults). With
     --all-dims, leave the dims free; otherwise pin them so the
     analyzer treats blockDim/gridDim as known constants. *)
  let block_dim =
    if all_dims then None else Some parsed.options.block_dim
  in
  let grid_dim =
    if all_dims then None else Some parsed.options.grid_dim
  in
  let kernels =
    List.map (preprocess ~block_dim ~grid_dim params) parsed.kernels
  in
  if output_json then JUI.run properties kernels
  else if
    not (TUI.run ~properties ~show_map ~show_check ~show_symbexp kernels)
  then exit 1

open Cmdliner

(* Cmdliner converter for the [a,b,c] / scalar Dim3 syntax used by
   --block-dim and --grid-dim. Mirrors drf/bin/main.ml. *)
let dim_help =
  {|
The value will be loaded from header if omitted.
Examples (without quotes): "[2,2,2]" or "32".
|}
  |> Common.replace ~substring:"\n" ~by:""

let conv_dim3 default =
  let parse s =
    match Dim3.parse ~default s with
    | Ok e -> Ok e
    | Error e -> Error (`Msg e)
  in
  let print ppf (l : Dim3.t) = Format.fprintf ppf "%s" (Dim3.to_string l) in
  Arg.conv (parse, print)

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

let check_arg : check_selector Term.t =
  let doc =
    "Which property to verify: $(b,well-sync) (each thread is internally \
     deterministic at every barrier), $(b,barrier-div) (any two threads of \
     the same group agree on every barrier), $(b,missing-participants) \
     (every thread reaches every block-wide barrier), or $(b,both) for \
     all of them (default)."
  in
  let choices =
    [
      ("both", Both);
      ("well-sync", Only Analysis.Property.Well_sync);
      ("barrier-div", Only Analysis.Property.Barrier_div);
      ("missing-participants", Only Analysis.Property.Missing_participants);
    ]
  in
  Arg.(
    value & opt (enum choices) Both
    & info [ "check" ] ~docv:"PROPERTY" ~doc)

let block_dim_arg : Dim3.t option Term.t =
  let d = Gv_parser.default_block_dim |> Dim3.to_string in
  let doc =
    "Sets the number of threads per block." ^ dim_help ^ " Default: " ^ d
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.one)) None
    & info [ "b"; "block-dim"; "blockDim" ] ~docv:"DIM3" ~doc)

let grid_dim_arg : Dim3.t option Term.t =
  let d = Gv_parser.default_grid_dim |> Dim3.to_string in
  let doc =
    "Sets the number of blocks per grid." ^ dim_help ^ " Default: " ^ d
  in
  Arg.(
    value
    & opt (some (conv_dim3 Dim3.one)) None
    & info [ "g"; "grid-dim"; "gridDim" ] ~docv:"DIM3" ~doc)

let all_dims_arg : bool Term.t =
  let doc =
    "Do not pin gridDim/blockDim; the verifier ranges over all possible \
     launch dimensions."
  in
  Arg.(value & flag & info [ "all-dims" ] ~doc)

let macros : string list Term.t =
  let doc = "Define <macro> to <value> (or 1 if <value> omitted)." in
  Arg.(
    value & opt_all string []
    & info [ "D"; "macro" ] ~docv:"<macro>=<value>" ~doc)

let params : (string * int) list Term.t =
  let doc = "Set the value of an integer parameter." in
  Arg.(
    value & opt_all (pair ~sep:'=' string int) []
    & info [ "p"; "param" ] ~docv:"KEYVAL" ~doc)

let main_t : unit Term.t =
  Term.(
    const main $ get_fname $ ignore_parsing_errors $ output_json $ show_map
    $ show_check $ show_symbexp $ check_arg $ block_dim_arg $ grid_dim_arg
    $ all_dims_arg $ macros $ params)

let info =
  let doc = "Check for barrier divergence errors" in
  Cmd.info "faial-sync" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
