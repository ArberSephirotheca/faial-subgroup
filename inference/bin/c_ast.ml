open Stage0
open Protocols
open Inference
open Queries
module Decl = C_lang.Decl
module StringSet = Set.Make (String)

(* Locate the c-to-json stdlib directory by PATH-searching for the
   [cu-to-json] binary without resolving its symlink. The install
   layout puts cu-to-json at [$PREFIX/bin/cu-to-json] and its bundled
   headers at [$PREFIX/share/c-to-json/include]; [dirname dirname] of
   the PATH hit yields $PREFIX. We deliberately avoid
   [Sys.executable_name]: on Linux the OCaml runtime initialises it
   from [/proc/self/exe], which resolves through any symlink in
   [$PREFIX/bin] and would leak the dune build prefix when c-ast is
   reached through one. *)
let stdlib_dir : Fpath.t option =
  let path = try Sys.getenv "PATH" with Not_found -> "" in
  String.split_on_char ':' path
  |> List.find_map (fun d ->
      if d = "" then None
      else
        let cand = Fpath.append (Fpath.v d) (Fpath.v "cu-to-json") in
        if Files.exists cand then Some cand else None)
  |> Option.map (fun p ->
      let bin_dir = Fpath.parent p in
      let prefix = Fpath.parent bin_dir in
      Fpath.append prefix (Files.from_string "share/c-to-json/include"))

(* A Def is part of the c-to-json stdlib when its source filename
   sits under [stdlib_dir], or when it has no source location at all
   (clang's compiler builtins like [__int128_t] emit decls with an
   empty filename). *)
let is_stdlib (loc : Location.t) : bool =
  let f = Location.filename loc in
  if f = "" then true
  else
    match stdlib_dir with
    | None -> false
    | Some d -> Fpath.is_prefix d (Fpath.v f)

let analyze (verbose : bool) (assume_launch : bool) (j : Yojson.Basic.t) :
    C_lang.Program.t * D_lang.Program.t * Imp.Kernel.t list =
  match C_lang.Program.parse j with
  | Ok k1 ->
      let synth =
        if assume_launch then Synthesise_launches.rewrite_program else Fun.id
      in
      let k2 = k1 |> D_lang.rewrite_program |> synth in
      let k3 =
        if verbose then D_to_imp.Default.parse_program k2
        else D_to_imp.Silent.parse_program k2
      in
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
        | Kernel k -> Hashtbl.add k2_ht (Imp.Function_id.to_string k.id) k
        | Prototype _ | Declaration _ | Typedef _ | Enum _ | LaunchParam _ ->
            ());
  k3
  |> List.iter (fun k ->
      Hashtbl.add k3_ht
        (Imp.Function_id.to_string (Imp.Kernel.unique_id k)) k);
  let l =
    List.fold_left
      (fun ((decls : Decl.t list), js) ->
        let open C_lang in
        let open Def in
        function
        | Kernel k -> (
            try
              (*         let k2 = Hashtbl.find k2_ht k.name in *)
              let k3 =
                Hashtbl.find k3_ht
                  (Imp.Function_id.to_string (C_lang.Kernel.id k))
              in
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
              if Decl.matches Protocols.Ty.is_array_or_pointer d then d :: decls
              else decls
            in
            (decls, js)
        | Prototype _ | Typedef _ | Enum _ | LaunchParam _ -> (decls, js))
      ([], []) k1
    |> snd
  in
  print_endline (Yojson.Basic.pretty_to_string (`List l))

let main (fname : string) (silent : bool) (json : bool) (verbose : bool)
    (only_global : bool) (show_stdlib : bool) (assume_launch : bool)
    (includes : string list) (macros : string list) : unit =
  let j =
    Cu_to_json.cu_to_json ~ignore_fail:true ~launch_params:true ~includes
      ~macros [ fname ]
  in
  let k1, k2, k3 = analyze verbose assume_launch j in
  let keep_loc (loc : Location.t) : bool = show_stdlib || not (is_stdlib loc) in
  (* Conservative drop list for the D_lang and Imp stages, which carry
     no [location] on their [Kernel.t]: the names of every C_lang
     kernel that the stdlib filter would reject. Anything not on this
     list — user kernels, but also synth kernels emitted by
     [Synthesise_launches] under [--assume-launch] — is kept. *)
  let stdlib_kernel_names : StringSet.t =
    k1
    |> List.fold_left
         (fun acc d ->
           match d with
           | (C_lang.Def.Kernel k | C_lang.Def.Prototype k)
             when not (keep_loc (C_lang.Kernel.location k)) ->
               StringSet.add k.name acc
           | _ -> acc)
         StringSet.empty
  in
  let c_lang_keep (d : C_lang.Def.t) : bool =
    let open C_lang in
    (match d with Def.Kernel k -> (not only_global) || Kernel.is_global k
     | _ -> not only_global)
    && keep_loc (C_lang.Def.location d)
  in
  let d_lang_keep (d : D_lang.Def.t) : bool =
    let open D_lang in
    match d with
    | Kernel k ->
        ((not only_global) || Kernel.is_global k)
        && not (StringSet.mem (Kernel.name k) stdlib_kernel_names)
    | Prototype k ->
        (not only_global)
        && not (StringSet.mem (Kernel.name k) stdlib_kernel_names)
    | Declaration d ->
        (not only_global) && keep_loc (Variable.location (Decl.var d))
    | Typedef d -> (not only_global) && keep_loc (Typedef.location d)
    | Enum e -> (not only_global) && keep_loc (Imp.Enum.location e)
    | LaunchParam lp -> (not only_global) && keep_loc lp.loc
  in
  let k1_filtered = C_lang.Program.filter c_lang_keep k1 in
  let k2_filtered = D_lang.Program.filter d_lang_keep k2 in
  let k3_filtered =
    k3
    |> List.filter (fun k ->
        ((not only_global) || Imp.Kernel.is_global k)
        && not (StringSet.mem (Imp.Kernel.name k) stdlib_kernel_names))
  in
  let scoped = List.map Imp.Scoped.Kernel.from_imp k3 in
  let inlined, rejected = Imp.Inline_calls.inline_calls scoped in
  let proto = List.map Imp.Compiler.compile inlined in
  let keep_named (name : string) (is_global : bool) : bool =
    ((not only_global) || is_global)
    && not (StringSet.mem name stdlib_kernel_names)
  in
  let scoped_filter =
    List.filter (fun k ->
        keep_named (Imp.Scoped.Kernel.name k) (Imp.Scoped.Kernel.is_global k))
  in
  let scoped_filtered = scoped_filter scoped in
  let inlined_filtered = scoped_filter inlined in
  let proto_filtered =
    proto
    |> List.filter (fun k ->
        keep_named (Protocols.Kernel.name k) (Protocols.Kernel.is_global k))
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
    print_endline "==================== STAGE 4: Scoped\n";
    List.iter Imp.Scoped.Kernel.print scoped_filtered;
    print_endline "==================== STAGE 5: Scoped, calls inlined\n";
    List.iter Imp.Scoped.Kernel.print inlined_filtered;
    List.iter
      (fun r -> print_endline (Imp.Rejected_kernel.to_string r))
      rejected;
    print_endline "==================== STAGE 6: Protocols\n";
    List.iter Protocols.Kernel.print proto_filtered;
    print_endline "==================== STAGE 7: stats\n");
  if json then print_json_summary k1 k2 k3

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let silent =
  let doc = "Silence output" in
  Arg.(value & flag & info [ "silent" ] ~doc)

let json =
  let doc = "Emit the JSON serialization summary" in
  Arg.(value & flag & info [ "json" ] ~doc)

let verbose =
  let doc = "Print warnings emitted by the inference pipeline" in
  Arg.(value & flag & info [ "verbose"; "v" ] ~doc)

let only_global =
  let doc = "Only print __global__ kernels" in
  Arg.(value & flag & info [ "only-global" ] ~doc)

let show_stdlib =
  let doc =
    "Include the c-to-json stdlib (cuda.h, stdio.h, iostream, \
     compiler-builtin typedefs, etc.). By default these are hidden so \
     only user-authored declarations remain."
  in
  Arg.(value & flag & info [ "show-stdlib" ] ~doc)

let assume_launch =
  let on_doc =
    "Synthesise a pseudo-kernel from each <<<...>>> launch site so the \
     launch arguments and grid/block configuration are inlined into the \
     kernel body (default)."
  in
  let off_doc =
    "Skip launch-site synthesis. Kernel parameters remain free variables \
     in the Imp stage even when the launch hands them literal values."
  in
  Arg.(
    value
    & vflag true
        [
          (true, info [ "assume-launch" ] ~doc:on_doc);
          (false, info [ "no-assume-launch" ] ~doc:off_doc);
        ])

let includes =
  let doc =
    "Add the specified directory to the search path for include files."
  in
  Arg.(value & opt_all string [] & info [ "I"; "include-dir" ] ~docv:"DIR" ~doc)

let macros =
  let doc = "Define $(docv) to <value> (or 1 if <value> omitted)" in
  Arg.(
    value & opt_all string []
    & info [ "D"; "macro" ] ~docv:"<macro>=<value>" ~doc)

let main_t =
  Term.(
    const main $ get_fname $ silent $ json $ verbose $ only_global
    $ show_stdlib $ assume_launch $ includes $ macros)

let info =
  let doc = "Print the C-AST" in
  Cmd.info "c-ast" ~version:Build_info.commit ~doc

let () = Cmd.v info main_t |> Cmd.eval |> exit
