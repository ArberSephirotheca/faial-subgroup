open Inference
open Stage0
open Protocols
open Barrier_div

(* Driver for the participant analysis (faial-sync-sym).

   Reads a CUDA file, runs Symexec.check on each kernel, and emits any
   Diagnostic.t entries. Exit 0 if every kernel is clean; exit 1 if
   any kernel produces diagnostics. *)

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

(* For block-wide __syncthreads, we need to count threads across the
   entire block in one symbolic-warp sample. Setting threads_per_warp
   to the block size achieves this. (The symbolic-metric layer's
   "warp" terminology is its own; for our purposes it is "the cohort
   of threads we sample together".) *)
let cfg_of_kernel (k : Kernel.t) : Rel_cost.Config.t =
  let block_dim = Option.value k.block_dim ~default:(Dim3.make ~x:32 ()) in
  let grid_dim = Option.value k.grid_dim ~default:Dim3.one in
  let threads_per_warp = Dim3.total block_dim in
  Rel_cost.Config.make ~threads_per_warp ~block_dim ~grid_dim ()

let print_kernel_result (k : Kernel.t) (diags : Diagnostic.t list) : bool =
  let module T = ANSITerminal in
  if diags = [] then begin
    T.print_string [ T.Bold; T.Foreground T.Green ]
      ("Kernel '" ^ k.name ^ "' has no participant errors.\n");
    true
  end
  else begin
    T.print_string [ T.Bold; T.Foreground T.Red ]
      ("Kernel '" ^ k.name ^ "' has "
      ^ string_of_int (List.length diags)
      ^ " participant error"
      ^ (if List.length diags = 1 then "" else "s")
      ^ ":\n");
    List.iter
      (fun d ->
        let sync = Diagnostic.sync_of d in
        let loc =
          match sync.loc with
          | Some l -> Location.to_string l
          | None -> "<unknown location>"
        in
        T.print_string [ T.Foreground T.Red ]
          ("  - " ^ Diagnostic.to_string d ^ " at " ^ loc ^ "\n"))
      diags;
    false
  end

let main (fname : string) (ignore_parsing_errors : bool)
    (block_dim : Dim3.t option) (grid_dim : Dim3.t option) (timeout : int)
    (precise : bool) (macros : string list)
    (params : (string * int) list) : unit =
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~block_dim ~grid_dim ~macros fname
  in
  let block_dim = Some parsed.options.block_dim in
  let grid_dim = Some parsed.options.grid_dim in
  let kernels = List.map (preprocess ~block_dim ~grid_dim params) parsed.kernels in
  let mode = if precise then Diagnostic.Precise else Diagnostic.Witness in
  let all_safe =
    List.fold_left
      (fun all_safe (k : Kernel.t) ->
        let cfg = cfg_of_kernel k in
        let locals = Kernel.local_set k in
        let initial : Thread.t = { path_cond = k.pre; proto = k.code } in
        let final =
          State.reduce ~timeout cfg locals (State.initial initial)
        in
        let diags =
          Diagnostic.of_state ~mode ~timeout ~pre:k.pre cfg locals final
        in
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
     A query that exceeds the timeout returns no result, which the \
     analysis treats conservatively (count predicates fail, blocking fire \
     and merge and producing no diagnostic from that query). NOTE: this \
     bounds an individual Z3 call, not the whole run. Total wall-clock \
     cost scales as O(phases * queries-per-phase * timeout) — see \
     --precise for the trade-off."
  in
  Arg.(value & opt int 0 & info [ "t"; "timeout" ] ~docv:"MS" ~doc)

let precise : bool Term.t =
  let doc =
    "Refine reported cohort sizes to their actual extrema (min for missing \
     participants, max for oversize cohorts) using the SMT optimizer. \
     Without this flag, cohort sizes are read off a SAT witness — fast \
     but only one valuation, not the worst. With this flag, every SAT \
     diagnostic triggers an additional optimizer call to tighten the \
     reported size; if the optimizer times out, the SAT witness is \
     used as a fallback so the diagnostic is never lost. Off by default."
  in
  Arg.(value & flag & info [ "precise" ] ~doc)

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
    $ timeout $ precise $ macros $ params)

let info =
  let doc = "Check for missing-participant errors at barriers" in
  Cmd.info "faial-sync-sym" ~version:Build_info.commit ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
