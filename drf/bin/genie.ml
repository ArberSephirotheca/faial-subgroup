open Protocols
open Protocols_parsing
open Drf
open Cmdliner

let conv_bexp =
  let parse s =
    match Parsers.BExpParser.of_string s with
    | Ok b -> Ok b
    | Error msg -> Error (`Msg msg)
  in
  let print ppf (b : Exp.bexp) = Format.fprintf ppf "%s" (Exp.b_to_string b) in
  Arg.conv (parse, print)

let launch_config_names : string list =
  let open Variable in
  List.map name (tid_list @ bid_list @ bdim_list @ gdim_list)

let launch_config_set : Variable.Set.t =
  let open Variable in
  Set.union (Set.union tid_set bid_set) (Set.union bdim_set gdim_set)

let[@warning "-32"] is_launch_config_name (n : string) : bool =
  List.mem n launch_config_names

let[@warning "-32"] is_dim_name (n : string) : bool =
  let open Variable in
  List.mem n (List.map name (bdim_list @ gdim_list))

let int_params (k : Kernel.t) : Variable.t list =
  Params.to_list k.global_variables
  |> List.filter_map (fun (v, ty) ->
      if C_type.is_int ty && not (Variable.Set.mem v launch_config_set)
      then Some v else None)

let unique_int_params (app : App.t) : Variable.t list =
  app.kernels
  |> List.concat_map int_params
  |> List.sort_uniq Variable.compare

let all_safe (rs : Analysis.t list) : bool =
  List.for_all Analysis.is_safe rs

let access_set_of (app : App.t) : Reachability.AccessSet.t =
  app.kernels |> App.only_kernel app
  |> List.concat_map (fun k ->
    k
    |> Reachability.prepare_kernel
         ~assumes:app.assumes
         ~assume_dims:app.assume_dims
         ~params:app.params
    |> Reachability.check_kernel ?timeout:app.timeout)
  |> Reachability.reachable_set

(* Stricter alternative kept for comparison; one Z3 query per access. *)
let[@warning "-32"] gate_holds_per_access
    (baseline : Reachability.AccessSet.t) (app : App.t) : bool =
  Reachability.AccessSet.subset baseline (access_set_of app)

let gate_holds_simple (_baseline : Reachability.AccessSet.t)
    (app : App.t) : bool =
  app.kernels |> App.only_kernel app
  |> List.for_all (fun k ->
    k
    |> Reachability.prepare_kernel
         ~assumes:app.assumes
         ~assume_dims:app.assume_dims
         ~params:app.params
    |> Reachability.preconditions_satisfiable ?timeout:app.timeout)

let gate_holds = gate_holds_simple

let run_assuming (assumptions : Exp.bexp list) (app : App.t) : Analysis.t list =
  { app with assumes = app.assumes @ assumptions }
  |> App.run

let verifies_drf_only (app : App.t) (assumptions : Exp.bexp list) : bool =
  app
  |> run_assuming assumptions
  |> all_safe

let abductive_loop ?(iter_cap = 32) (app : App.t) : Exp.bexp list option =
  let kernels = App.only_kernel app app.kernels in
  if kernels = [] then None
  else
    let session = Abduction.create_for_kernels kernels in
    let rec loop iter extras =
      if iter >= iter_cap then Some extras
      else
        let result = run_assuming extras app in
        if all_safe result then Some extras
        else
          let added = Abduction.add_all result session in
          if added = 0 then None
          else
            match Abduction.solve session with
            | None -> None
            | Some new_extras -> loop (iter + 1) new_extras
    in
    loop 0 []

let blanket_extras (app : App.t) : Exp.bexp list =
  let dims = [
    Variable.bdim_x; Variable.bdim_y; Variable.bdim_z;
    Variable.gdim_x; Variable.gdim_y; Variable.gdim_z;
  ] in
  let params = unique_int_params app in
  let signs =
    params |> List.map (fun v -> Exp.n_gt (Exp.Var v) (Exp.Num 0))
  in
  let bounds =
    params |> List.concat_map (fun p ->
      List.map (fun d -> Exp.n_ge (Exp.Var p) (Exp.Var d)) dims)
  in
  signs @ bounds

let shrink (_baseline : Reachability.AccessSet.t) (app : App.t)
    (extras : Exp.bexp list) : Exp.bexp list =
  let rec loop kept remaining =
    match remaining with
    | [] -> kept
    | c :: rest ->
      if verifies_drf_only app (kept @ rest)
      then loop kept rest
      else loop (kept @ [c]) rest
  in
  loop [] extras

let format_assume_flags (extras : Exp.bexp list) : string =
  extras
  |> List.map (fun b -> "--assume \"" ^ Exp.b_to_string b ^ "\"")
  |> String.concat " "

let main =
  let doc = "Search for assume-constraints that make a CUDA kernel DRF." in
  let info = Cmd.info "faial-genie" ~doc in
  Cmd.v info
  @@
  let open Cmdliner.Term.Syntax in
  let+ filename =
    Arg.(required & pos 0 (some file) None
         & info [] ~docv:"FILENAME"
             ~doc:"Path to the GPU program.")
  and+ timeout =
    Arg.(value & opt (some int) None
         & info [ "t"; "timeout" ] ~docv:"MS"
             ~doc:"Per-iteration solver timeout in milliseconds.")
  and+ logic =
    Arg.(value & opt (some string) None
         & info [ "logic" ] ~doc:"Z3 logic.")
  and+ includes =
    Arg.(value & opt_all string []
         & info [ "I"; "include-dir" ] ~docv:"DIR"
             ~doc:"Add to include search path.")
  and+ params =
    Arg.(value & opt_all (pair ~sep:'=' string int) []
         & info [ "p"; "param" ] ~docv:"K=V"
             ~doc:"Set integer parameter.")
  and+ macros =
    Arg.(value & opt_all string []
         & info [ "D"; "macro" ] ~docv:"NAME[=VAL]"
             ~doc:"Define macro.")
  and+ cu_to_json =
    Arg.(value & opt string "cu-to-json"
         & info [ "cu-to-json" ] ~docv:"PATH"
             ~doc:"Path to cu-to-json.")
  and+ cbor =
    Arg.(value & flag
         & info [ "cbor" ] ~doc:"Use cu-to-json's CBOR output.")
  and+ ignore_parsing_errors =
    Arg.(value & flag
         & info [ "ignore-parsing-errors" ] ~doc:"Ignore parsing errors.")
  and+ ignore_calls =
    Arg.(value & flag
         & info [ "ignore-calls" ] ~doc:"Skip kernel-call inlining.")
  and+ ignore_asserts =
    Arg.(value & flag
         & info [ "ignore-asserts" ] ~doc:"Ignore asserts.")
  and+ only_kernel =
    Arg.(value & opt (some string) None
         & info [ "kernel" ] ~doc:"Only check a specific kernel.")
  and+ extra_assumes =
    Arg.(value & opt_all conv_bexp []
         & info [ "assume" ] ~docv:"BEXP"
             ~doc:"Pre-condition added to all kernels at the baseline.")
  in
  let archs = [ Architecture.Block ] in
  let app =
    App.parse
      ~filename ~timeout
      ~show_proofs:false ~show_proto:false ~show_wf:false ~show_align:false
      ~show_delin:false ~show_phase_split:false ~show_loc_split:false
      ~show_flat_acc:false ~show_symbexp:false
      ~logic ~ge_index:[] ~le_index:[] ~eq_index:[]
      ~only_array:None ~only_kernel
      ~only_true_data_races:false
      ~thread_idx_1:None ~thread_idx_2:None
      ~block_idx_1:None ~block_idx_2:None
      ~archs
      ~inline_calls:(not ignore_calls)
      ~ignore_parsing_errors
      ~includes
      ~block_dim:None ~grid_dim:None
      ~params
      ~macros
      ~cu_to_json
      ~all_dims:true
      ~ignore_asserts
      ~log_delinearize:false
      ~assume_delin:true
      ~assumes:extra_assumes
      ~assume_dims:true
      ~assume_launch:true
      ~cbor
      ~stop_at:None
  in
  let baseline_reachable = access_set_of app in
  let baseline = App.run app in
  if all_safe baseline then begin
    if not (Reachability.AccessSet.is_empty baseline_reachable) then begin
      print_endline "DRF under baseline (--assume-launch --assume-dims --assume-delin).";
      print_endline "No extra --assume needed.";
      Ok ()
    end else begin
      print_endline "Baseline preconditions are unsatisfiable — kernel is vacuously DRF.";
      print_endline "Check that the kernel and any user --assume flags are mutually satisfiable.";
      Ok ()
    end
  end else begin
    let try_finalize (extras : Exp.bexp list) (source : string) : bool =
      let minimal = shrink baseline_reachable app extras in
      let app' = { app with assumes = app.assumes @ minimal } in
      if gate_holds baseline_reachable app' then begin
        print_endline ("DRF after " ^ source ^ ".");
        print_endline ("Discovered: " ^ format_assume_flags minimal);
        true
      end else false
    in
    let abductive_result =
      match abductive_loop app with
      | Some (_ :: _ as extras) when verifies_drf_only app extras
                                     && try_finalize extras "abductive refinement" -> true
      | _ -> false
    in
    if abductive_result then Ok ()
    else begin
      let blanket = blanket_extras app in
      if blanket <> [] && verifies_drf_only app blanket
         && try_finalize blanket "blanket fallback"
      then Ok ()
      else begin
        print_endline "Racy; either a real race or a modelling gap (or vacuous DRF rejected).";
        Ok ()
      end
    end
  end

let () = exit (Cmd.eval_result main)
