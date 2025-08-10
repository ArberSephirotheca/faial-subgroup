open Stage0
open Inference
open Queries
module Decl = C_lang.Decl

let analyze (j : Yojson.Basic.t) :
    C_lang.Program.t * D_lang.Program.t * Imp.Kernel.t list =
  match C_lang.Program.parse j with
  | Ok k1 ->
      let k2 = D_lang.rewrite_program k1 in
      let k3 = D_to_imp.Default.parse_program k2 in
      (k1, k2, k3)
  | Error e ->
      Rjson.print_error e;
      exit (-1)

let print_json_summary (k1 : C_lang.Program.t) (k2 : D_lang.Program.t)
    (k3 : Imp.Kernel.t list) : unit =
  let k1_len = List.length k1 in
  let k2_ht = Hashtbl.create k1_len in
  let k3_ht = Hashtbl.create k1_len in
  k2
  |> List.iter
       (let open D_lang in
        let open Def in
        function
        | Kernel k -> Hashtbl.add k2_ht k.name k
        | Declaration _ | Typedef _ | Enum _ -> ());
  k3
  |> List.iter (fun k ->
         let open Imp.Kernel in
         Hashtbl.add k3_ht k.name k);
  let l =
    List.fold_left
      (fun ((decls : Decl.t list), js) ->
        let open C_lang in
        let open Def in
        function
        | Kernel k -> (
            try
              (*         let k2 = Hashtbl.find k2_ht k.name in *)
              let k3 = Hashtbl.find k3_ht k.name in
              ( decls,
                `Assoc
                  [
                    ("function calls", Calls.summarize decls k);
                    ("nested loops", NestedLoops.summarize k.code);
                    ("loops", Loops.summarize k.code);
                    (*           "loop inference", ForEach.summarize k2.code; *)
                    ("mutated vars", MutatedVar.summarize k.code);
                    ("declarations", Declarations.summarize k.code);
                    ("conditionals", Conditionals.summarize k.code);
                    ("variables", Variables.summarize k.code);
                    ("params", Params.summarize k);
                    ("accesses", Accesses.summarize k3.code);
                    ("global decls", GlobalDeclArrays.summarize decls);
                    ("divergence", Divergence.summarize k3.code);
                    ("kernel", Queries.Kernel.summarize k3);
                  ]
                :: js )
            with Not_found -> (decls, js))
        | Declaration d ->
            let decls =
              if Decl.matches Protocols.C_type.is_array d then d :: decls
              else decls
            in
            (decls, js)
        | Typedef _ | Enum _ -> (decls, js))
      ([], []) k1
    |> snd
  in
  print_endline (Yojson.Basic.pretty_to_string (`List l))

let main (fname : string) (silent : bool) (skip_json : bool)
    (only_global : bool) : unit =
  let j = Cu_to_json.cu_to_json ~ignore_fail:true fname in
  let k1, k2, k3 = analyze j in

  let k1_filtered =
    C_lang.Program.filter
      (function
        | Kernel k -> (not only_global) || C_lang.Kernel.is_global k
        | _ -> not only_global)
      k1
  in
  let k2_filtered =
    D_lang.Program.filter
      (function
        | Kernel k -> (not only_global) || D_lang.Kernel.is_global k
        | _ -> not only_global)
      k2
  in
  let k3_filtered =
    if only_global then List.filter Imp.Kernel.is_global k3 else k3
  in
  if silent then ()
  else (
    print_endline "\n==================== STAGE 1: C\n";
    C_lang.Program.print k1_filtered;
    print_endline
      "==================== STAGE 2: C with reads/writes as statements\n";
    D_lang.Program.print k2_filtered;
    print_endline "==================== STAGE 3: IMP\n";
    List.iter Imp.Kernel.print k3_filtered;
    print_endline "==================== STAGE 4: stats\n");
  if not skip_json then print_json_summary k1 k2 k3

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let silent =
  let doc = "Silence output" in
  Arg.(value & flag & info [ "silent" ] ~doc)

let skip_json =
  let doc = "Skip JSON serialization output" in
  Arg.(value & flag & info [ "skip-json" ] ~doc)

let only_global =
  let doc = "Only print __global__ kernels" in
  Arg.(value & flag & info [ "only-global" ] ~doc)

let main_t = Term.(const main $ get_fname $ silent $ skip_json $ only_global)

let info =
  let doc = "Print the C-AST" in
  Cmd.info "c-ast" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
