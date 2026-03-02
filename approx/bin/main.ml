open Inference
open Protocols
open Stage0

let run_kernel ~ignore_parsing_errors (fname : string) : unit =
  let k =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      fname
  in
  k.kernels
  |> List.iter (fun k ->
      let k = Protocols.Kernel.apply_arch Architecture.Block k in
      let cidi = Approx.Check.per_kernel k in
      let ci = if cidi.control_independent then "ind" else "ctrl" in
      let di = if cidi.data_independent then "ind" else "data" in
      print_endline (k.name ^ "," ^ di ^ "," ^ ci))

let run_acc ~ignore_parsing_errors (fname : string) : unit =
  let k =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      fname
  in
  k.kernels
  |> List.iter (fun k ->
      let k = Protocols.Kernel.apply_arch Architecture.Block k in
      Approx.Check.per_access k
      |> List.iter (fun (c, cidi) ->
          let l = Approx.Code.location c |> Location.to_string in
          print_endline (k.name ^ "," ^ Approx.Check.to_string cidi ^ "," ^ l)))

let main (fname : string) (ignore_parsing_errors : bool) (per_access : bool) :
    unit =
  if per_access then run_acc ~ignore_parsing_errors fname
  else run_kernel ~ignore_parsing_errors fname

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let ignore_parsing_errors =
  let doc = "Ignore parsing errors." in
  Arg.(value & flag & info [ "ignore-parsing-errors" ] ~doc)

let per_access =
  let doc = "List approximation analysis per access." in
  Arg.(value & flag & info [ "per-access"; "A" ] ~doc)

let main_t = Term.(const main $ get_fname $ ignore_parsing_errors $ per_access)

let info =
  let doc = "Data-dependency analysis for GPU programs" in
  Cmd.info "data-dep" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
