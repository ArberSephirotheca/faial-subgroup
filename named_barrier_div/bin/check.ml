open Inference
open Stage0
open Protocols
open Named_barrier_div

(* Driver for the named-barrier-divergence analysis (faial-nbd).

   Reads a CUDA file, runs [Named_barrier_div.Check.run] on each kernel, and emits
   any diagnostics. Exit 0 if every kernel is clean; exit 1 if any
   kernel produces diagnostics. *)

let preprocess ~block_dim ~grid_dim params (k : Kernel.t) : Kernel.t =
  k
  |> Kernel.try_set_block_dim block_dim
  |> Kernel.try_set_grid_dim grid_dim
  |> Kernel.apply_arch_binders Architecture.Defaults.block
  |> (fun k -> { k with pre = Exp.b_and Architecture.Defaults.base k.pre })
  |> Kernel.inline_globals params
  |> Kernel.add_missing_binders
  |> Kernel.opt
  |> fun k -> { k with code = Code.vars_distinct k.code (Kernel.parameter_set k) }

let print_kernel_result (k : Kernel.t) (diags : Tier2.diagnostic list) : bool =
  let module T = ANSITerminal in
  if diags = [] then begin
    T.print_string [ T.Bold; T.Foreground T.Green ]
      ("Kernel '" ^ k.name ^ "' is NBD-safe.\n");
    true
  end
  else begin
    T.print_string [ T.Bold; T.Foreground T.Red ]
      ("Kernel '" ^ k.name ^ "' has "
      ^ string_of_int (List.length diags)
      ^ " NBD diagnostic"
      ^ (if List.length diags = 1 then "" else "s")
      ^ ":\n");
    List.iter
      (fun d ->
        T.print_string [ T.Foreground T.Red ]
          ("  - " ^ Diagnostic.to_string d ^ "\n"))
      diags;
    false
  end

let main (fname : string) (ignore_parsing_errors : bool)
    (block_dim : Dim3.t option) (grid_dim : Dim3.t option) (timeout : int)
    (macros : string list) (params : (string * int) list) : unit =
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~block_dim ~grid_dim ~macros fname
  in
  let block_dim = Some parsed.options.block_dim in
  let grid_dim = Some parsed.options.grid_dim in
  let kernels =
    List.map (preprocess ~block_dim ~grid_dim params) parsed.kernels
  in
  let all_safe =
    List.fold_left
      (fun all_safe (k : Kernel.t) ->
        let diags = Check.run ~timeout k in
        let safe = print_kernel_result k diags in
        all_safe && safe)
      true kernels
  in
  if not all_safe then exit 1

open Cmdliner

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

let block_dim : Dim3.t option Term.t =
  let d = Gv_parser.default_block_dim in
  let doc = "Sets the number of threads per block. " ^ dim_help in
  Arg.(value & opt (some (conv_dim3 d)) None
       & info [ "b"; "block-dim"; "blockDim" ] ~docv:"BLOCK_DIM" ~doc)

let grid_dim : Dim3.t option Term.t =
  let d = Gv_parser.default_grid_dim in
  let doc = "Sets the number of blocks per grid. " ^ dim_help in
  Arg.(value & opt (some (conv_dim3 d)) None
       & info [ "g"; "grid-dim"; "gridDim" ] ~docv:"GRID_DIM" ~doc)

let timeout : int Term.t =
  let doc =
    "Per-SMT-query timeout in milliseconds. 0 (default) means no timeout. \
     Bounds an individual Z3 call, not the whole run."
  in
  Arg.(value & opt int 0 & info [ "t"; "timeout" ] ~docv:"MS" ~doc)

let macros : string list Term.t =
  let doc = "Define <macro> to <value> (or 1 if <value> omitted)." in
  Arg.(value & opt_all string []
       & info [ "D"; "macro" ] ~docv:"<macro>=<value>" ~doc)

let params : (string * int) list Term.t =
  let doc = "Set the value of an integer parameter." in
  Arg.(value & opt_all (pair ~sep:'=' string int) []
       & info [ "p"; "param" ] ~docv:"KEYVAL" ~doc)

let main_t : unit Term.t =
  Term.(
    const main $ get_fname $ ignore_parsing_errors $ block_dim $ grid_dim
    $ timeout $ macros $ params)

let info =
  let doc = "Check for named-barrier divergence" in
  Cmd.info "faial-nbd" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
