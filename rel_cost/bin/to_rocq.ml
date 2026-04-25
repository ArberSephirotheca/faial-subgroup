(** [faial-to-rocq]: emit a Coq [ProtoLet.t] definition for each
    kernel in a CUDA source file.

    The generated Coq source is a starting point for hand-written
    analyses (cost upper bounds, equivalences, transformations) using
    the [faial-cost-rocq] formalism. The output mirrors the style of
    the worked examples in [src/Warp/Copper.v]. *)

open Stage0
open Inference
open Rel_cost
open Protocols

type kernel = Protocols.Kernel.t

let abort_when (b : bool) (msg : string) : unit =
  if b then (
    Logger.Colors.error msg;
    exit (-2))
  else ()

(** Same preprocessing as [faial-cost]: filter arrays to those the
    chosen metric supports, fix the dim parameters, inline globals,
    constant-fold, then linearize the array indices.

    Metric mapping (Rocq side ↔ Faial side):
    - [Rocq.Metric.MemReads]      ↔ [Rel_cost.Metric.UncoalescedAccesses]
                                    (filters to global arrays)
    - [Rocq.Metric.ActiveThreads] ↔ [Rel_cost.Metric.ActiveThreads]
                                    (no array filtering — applies to any
                                    memory) *)
module Pipeline = struct
  module L = Linearize_index.Make (Logger.Silent)

  let to_faial_metric : Rocq.Metric.t -> Metric.t = function
    | MemReads -> UncoalescedAccesses
    | ActiveThreads -> ActiveThreads

  let linearize_kernel (cfg : Config.t) (k : kernel) :
      (kernel, string) Result.t =
    let ( let* ) = Result.bind in
    let linearize = L.linearize cfg k.arrays in
    let rec on_code : Code.t -> (Code.t, string) Result.t = function
      | Skip -> Ok Skip
      | Sync s -> Ok (Sync s)
      | Access a ->
          let* a = linearize a in
          Ok (Code.Access a)
      | Seq (p, q) ->
          let* p = on_code p in
          let* q = on_code q in
          Ok (Code.Seq (p, q))
      | If (b, p, q) ->
          let* p = on_code p in
          let* q = on_code q in
          Ok (Code.If (b, p, q))
      | Loop { range; body } ->
          let* body = on_code body in
          Ok (Code.Loop { range; body })
      | Decl d ->
          let* body = on_code d.body in
          Ok (Code.Decl { d with body })
    in
    let* code = on_code k.code in
    Ok { k with code }

  let prepare ~(block_dim : Dim3.t) ~(grid_dim : Dim3.t)
      ~(params : (string * int) list) ~(metric : Rocq.Metric.t)
      (cfg : Config.t) (k : kernel) : (kernel, string) Result.t =
    let open Protocols.Kernel in
    let faial_metric = to_faial_metric metric in
    let supported_arrays = Metric.supported_arrays k.arrays faial_metric in
    k
    |> filter_array (fun x -> Variable.Set.mem x supported_arrays)
    |> set_block_dim block_dim |> set_grid_dim grid_dim
    |> apply_arch_binders Architecture.Defaults.block
    |> inline_globals params |> opt
    |> linearize_kernel cfg
end

let pico (fname : string) (block_dim : Dim3.t option)
    (grid_dim : Dim3.t option) (params : (string * int) list)
    (ignore_parsing_errors : bool) (bank_count : int)
    (threads_per_warp : int) (metric : Rocq.Metric.t) (output : string option)
    : unit =
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~block_dim ~grid_dim fname
  in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  let cfg = Config.make ~block_dim ~grid_dim ~bank_count ~threads_per_warp () in
  let kernels =
    parsed.kernels
    |> List.map (fun k ->
           match Pipeline.prepare ~block_dim ~grid_dim ~params ~metric cfg k with
           | Ok k -> k
           | Error e ->
               Logger.Colors.error
                 ("preprocessing kernel " ^ k.Kernel.name ^ ": " ^ e);
               exit (-1))
  in
  abort_when (kernels = []) "No kernels found.";
  match Rocq.from_kernels ~metric kernels with
  | Error e ->
      Logger.Colors.error e;
      exit (-1)
  | Ok lines ->
      let s = Rocq.to_string lines in
      (match output with
       | None -> print_string s
       | Some f ->
           let oc = open_out f in
           output_string oc s;
           close_out oc)

(* Command-line interface *)

open Cmdliner

let dim3 : Dim3.t Cmdliner.Arg.conv =
  let parse =
   fun s -> match Dim3.parse s with Ok r -> Ok r | Error e -> Error (`Msg e)
  in
  let print : Dim3.t Cmdliner.Arg.printer =
   fun ppf v -> Format.fprintf ppf "%s" (Dim3.to_string v)
  in
  Arg.conv (parse, print)

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let block_dim =
  let doc = "Sets the CUDA variable blockDim." in
  Arg.(
    value
    & opt (some dim3) None
    & info [ "b"; "block-dim"; "blockDim" ] ~docv:"BLOCK_DIM" ~doc)

let grid_dim =
  let doc = "Sets the CUDA variable gridDim." in
  Arg.(
    value
    & opt (some dim3) None
    & info [ "g"; "grid-dim"; "gridDim" ] ~docv:"GRID_DIM" ~doc)

let params =
  let doc = "Set the value of an integer parameter." in
  Arg.(
    value
    & opt_all (pair ~sep:'=' string int) []
    & info [ "p"; "param" ] ~docv:"KEYVAL" ~doc)

let ignore_parsing_errors =
  let doc = "Parsing errors do not abort analysis." in
  Arg.(value & flag & info [ "ignore-parsing-errors" ] ~doc)

let bank_count =
  let info =
    Arg.info [ "bank-count" ] ~docv:"BANK_COUNT"
      ~doc:"The number of banks available in the GPU device."
  in
  Arg.value (Arg.opt Arg.int 32 info)

let warp_size =
  let info =
    Arg.info [ "warp-size" ] ~docv:"WARP_SIZE"
      ~doc:"The number of threads per warp available in the GPU device."
  in
  Arg.value (Arg.opt Arg.int 32 info)

let output =
  let doc = "Write generated Coq code to $(docv) instead of stdout." in
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)

let metric =
  let doc =
    "Coq metric to bind in [Access] nodes. $(docv) ∈ {mem-reads, active}: \
     mem-reads → Warp.MemReads.T (filters to global arrays); active → \
     Warp.Metric.CountEnabled.T (counts enabled threads, no filter)."
  in
  Arg.(
    value
    & opt (enum Rocq.Metric.choices) Rocq.Metric.MemReads
    & info [ "m"; "metric" ] ~docv:"METRIC" ~doc)

let pico_t =
  Term.(
    const pico $ get_fname $ block_dim $ grid_dim $ params
    $ ignore_parsing_errors $ bank_count $ warp_size $ metric $ output)

let info =
  let doc =
    "Translate a CUDA kernel into a Coq ProtoLet definition for the \
     faial-cost-rocq formalism."
  in
  Cmd.info "faial-to-rocq" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info pico_t |> Cmd.eval |> exit
