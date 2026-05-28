open Stage0
open Inference

let parse (j : Yojson.Basic.t) : W_lang.Program.t =
  match W_lang.Program.parse j with
  | Ok k1 -> k1
  | Error e ->
      Rjson.print_error e;
      exit (-1)

let main (fname : string) : unit =
  let j = Wgsl_to_json.wgsl_to_json fname in
  let p = parse j |> W_lang.Program.map_expression W_lang.Expression.simplify in
  print_string (W_lang.Program.to_string p);
  print_endline
    "\n\
     -------------------------------------- IMP \
     --------------------------------------\n";
  p |> W_to_imp.translate |> List.iter Imp.Kernel.print;
  ()

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Print the WGSL-AST" in
  Cmd.info "wgsl-ast" ~version:Build_info.commit ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
