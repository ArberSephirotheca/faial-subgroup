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

let main (fname : string) (silent : bool) : unit =
  let j = Cu_to_json.cu_to_json ~ignore_fail:true fname in
  let k1, k2, k3 = analyze j in
  if silent then ()
  else (
    print_endline "\n==================== STAGE 1: C\n";
    C_lang.Program.print k1;
    print_endline
      "==================== STAGE 2: C with reads/writes as statements\n";
    D_lang.Program.print k2;
    print_endline "==================== STAGE 3: IMP\n";
    List.iter Imp.Kernel.print k3;
    print_endline "==================== STAGE 4: stats\n");
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

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let silent =
  let doc = "Silence output" in
  Arg.(value & flag & info [ "silent" ] ~doc)

let main_t = Term.(const main $ get_fname $ silent)

let info =
  let doc = "Print the C-AST" in
  Cmd.info "c-ast" ~version:"%%VERSION%%" ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
