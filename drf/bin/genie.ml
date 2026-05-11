open Stage0
open Protocols
open Protocols_parsing
open Drf
open Cmdliner

(* genie searches for a set of [--assume] preconditions that make a CUDA
   kernel verify as DRF, on top of the always-on baseline of
   [--assume-launch], [--assume-dims], [--assume-delin].

   Refinement is witness-driven: each racy [Solve_drf] outcome carries a
   [Witness.t] with the model values Z3 picked for the symbolic
   parameters and launch dimensions. We propose only predicates that
   contradict the witness — [p > 0] for params at non-positive values,
   [p >= dim] for params whose magnitude is below a referenced launch
   dim — and iterate until DRF or until no new predicate is suggested.
   A final shrink pass drops anything that became redundant once later
   iterations strengthened other clauses. *)

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

let is_launch_config_name (n : string) : bool =
  List.mem n launch_config_names

let is_dim_name (n : string) : bool =
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

let parse_int_opt (s : string) : int option =
  try Some (int_of_string (String.trim s))
  with Failure _ -> None

let int_globals (w : Solve_drf.Witness.t) : (string * int) list =
  w.globals.variables
  |> List.filter_map (fun (k, v) ->
      match parse_int_opt v with
      | Some n -> Some (k, n)
      | None -> None)

(* Predicates contradicting a single witness: sign-fix for params Z3
   picked at non-positive values, and bound-fix for params whose
   absolute value is below some referenced launch dim. *)
let propose_from_witness (w : Solve_drf.Witness.t) : Exp.bexp list =
  let kvs = int_globals w in
  let params = List.filter (fun (k, _) -> not (is_launch_config_name k)) kvs in
  let dims = List.filter (fun (k, _) -> is_dim_name k) kvs in
  let var_of (n : string) : Variable.t = Variable.from_name n in
  let sign_preds =
    params
    |> List.filter_map (fun (k, v) ->
        if v <= 0
        then Some (Exp.n_gt (Exp.Var (var_of k)) (Exp.Num 0))
        else None)
  in
  let bound_preds =
    params |> List.concat_map (fun (k, vp) ->
      dims |> List.filter_map (fun (d, vd) ->
        if abs vp < vd
        then Some (Exp.n_ge (Exp.Var (var_of k)) (Exp.Var (var_of d)))
        else None))
  in
  (* Multiplicative bound: total threads on an axis is [gridDim.x *
     blockDim.x] (and analogously for y/z). When a param sits below
     that product in the model, propose [p >= gridDim.X * blockDim.X]. *)
  let mul_pairs = [
    ("gridDim.x", "blockDim.x");
    ("gridDim.y", "blockDim.y");
    ("gridDim.z", "blockDim.z");
    ("blockDim.x", "blockDim.y");
    ("blockDim.y", "blockDim.z");
    ("blockDim.x", "blockDim.z");
    ("gridDim.x", "gridDim.y");
    ("gridDim.y", "gridDim.z");
    ("gridDim.x", "gridDim.z");
  ] in
  let mul_preds =
    params |> List.concat_map (fun (k, vp) ->
      mul_pairs |> List.filter_map (fun (gn, bn) ->
        match List.assoc_opt gn dims, List.assoc_opt bn dims with
        | Some vg, Some vb when abs vp < vg * vb ->
          Some (Exp.n_ge
                  (Exp.Var (var_of k))
                  (Exp.n_mult (Exp.Var (var_of gn)) (Exp.Var (var_of bn))))
        | _ -> None))
  in
  (* Divisibility: if the witness has [p] not a multiple of dim [d]
     (and |p| >= d so the constraint isn't trivially false), propose
     [p % d == 0]. Captures stride/offset alignment patterns. *)
  let div_preds =
    params |> List.concat_map (fun (k, vp) ->
      dims |> List.filter_map (fun (d, vd) ->
        if vd > 0 && abs vp >= vd && (abs vp) mod vd <> 0
        then Some (Exp.n_eq
                     (Exp.n_mod (Exp.Var (var_of k)) (Exp.Var (var_of d)))
                     (Exp.Num 0))
        else None))
  in
  (* Cross-param bound: when two source params are both in the racing
     access and the witness ordered them [v1 < v2], propose [p1 >= p2].
     Captures source-level invariants like [width >= height]. *)
  let cross_preds =
    params |> List.concat_map (fun (k1, v1) ->
      params |> List.filter_map (fun (k2, v2) ->
        if k1 <> k2 && v1 < v2
        then Some (Exp.n_ge (Exp.Var (var_of k1)) (Exp.Var (var_of k2)))
        else None))
  in
  (* Scaled bound: scan / reduction / 2-element-per-thread kernels use
     [block_size = 2 * blockDim.x] or [4 * blockDim.x]. When a param
     sits below [K * dim] in the model, propose [p >= K * dim]. *)
  let scaled_pairs = [
    (2, "blockDim.x"); (2, "blockDim.y"); (2, "blockDim.z");
    (2, "gridDim.x");  (2, "gridDim.y");  (2, "gridDim.z");
    (4, "blockDim.x"); (4, "blockDim.y"); (4, "blockDim.z");
    (4, "gridDim.x");  (4, "gridDim.y");  (4, "gridDim.z");
  ] in
  let scaled_preds =
    params |> List.concat_map (fun (k, vp) ->
      scaled_pairs |> List.filter_map (fun (kk, dn) ->
        match List.assoc_opt dn dims with
        | Some vd when abs vp < kk * vd ->
          Some (Exp.n_ge
                  (Exp.Var (var_of k))
                  (Exp.n_mult (Exp.Num kk) (Exp.Var (var_of dn))))
        | _ -> None))
  in
  (* Param-equals-dim: kernels that mirror a launch axis in a param
     (e.g. [width = blockDim.x], [size = blockDim.x * gridDim.x]).
     When the witness has [p != dim], propose [p == dim]. *)
  let eq_dim_preds =
    let single_dims =
      ["blockDim.x"; "blockDim.y"; "blockDim.z";
       "gridDim.x"; "gridDim.y"; "gridDim.z"]
    in
    let from_single =
      params |> List.concat_map (fun (k, vp) ->
        single_dims |> List.filter_map (fun dn ->
          match List.assoc_opt dn dims with
          | Some vd when vp <> vd ->
            Some (Exp.n_eq (Exp.Var (var_of k)) (Exp.Var (var_of dn)))
          | _ -> None))
    in
    let from_product =
      params |> List.concat_map (fun (k, vp) ->
        mul_pairs |> List.filter_map (fun (gn, bn) ->
          match List.assoc_opt gn dims, List.assoc_opt bn dims with
          | Some vg, Some vb when vp <> vg * vb ->
            Some (Exp.n_eq
                    (Exp.Var (var_of k))
                    (Exp.n_mult (Exp.Var (var_of gn)) (Exp.Var (var_of bn))))
          | _ -> None))
    in
    from_single @ from_product
  in
  sign_preds @ bound_preds @ mul_preds @ div_preds @ cross_preds
  @ scaled_preds @ eq_dim_preds

let witnesses_of (rs : Analysis.t list) : Solve_drf.Witness.t list =
  rs |> List.concat_map (fun (a : Analysis.t) ->
    a.report |> List.filter_map (fun (s : Solve_drf.Solution.t) ->
      match s.outcome with
      | Solve_drf.Outcome.Racy w -> Some w
      | _ -> None))

let bexp_eq (a : Exp.bexp) (b : Exp.bexp) : bool = Exp.b_compare a b = 0

let dedupe (xs : Exp.bexp list) : Exp.bexp list =
  List.sort_uniq Exp.b_compare xs

let all_safe (rs : Analysis.t list) : bool =
  List.for_all Analysis.is_safe rs

(* Reachability gate: when the accumulated preconditions are
   contradictory, every sanity proof becomes UNSAT and the kernel
   "verifies as DRF" vacuously. We require at least one SATISFIABLE
   sanity proof per kernel — per-access UNSAT is normal (dead
   branches), but if every access is unreachable the precondition
   itself is unsatisfiable. *)
let preconditions_reachable (app : App.t) : bool =
  let solve_safe p =
    try
      match Solve_drf.solve ~timeout:app.timeout ~logic:app.logic p with
      | Z3.Solver.SATISFIABLE | Z3.Solver.UNKNOWN -> `Reachable
      | Z3.Solver.UNSATISFIABLE -> `Unreachable
    with Gen_z3.Not_implemented _ -> `Unknown
  in
  try
    app.kernels |> App.only_kernel app
    |> List.for_all (fun kernel ->
      let results =
        kernel
        |> App.translate Architecture.Block app
        |> Symbexp.sanity_check Architecture.Block
        |> Streamutil.to_list
        |> List.map solve_safe
      in
      (* If any proof says reachable, the kernel is reachable. If
         every solve raised Not_implemented (BV-only operators), we
         can't decide; treat as reachable rather than reject. *)
      List.exists (fun r -> r = `Reachable) results
      || List.for_all (fun r -> r = `Unknown) results)
  with App.Stop_at_stage -> true

(* CEGAR loop. [accumulated] grows monotonically; each iteration runs
   the analysis once, and either declares DRF, proposes new predicates
   to add, or gives up because no witness suggests anything we don't
   already have. The [iter] cap is a safety net — termination is
   already guaranteed because each iteration adds at least one new
   predicate over a finite param/dim space. *)
let rec witness_loop ?(iter_cap = 16) (app : App.t)
    (accumulated : Exp.bexp list) (iter : int) : Exp.bexp list option =
  if iter >= iter_cap then None
  else
    let app' = { app with assumes = app.assumes @ accumulated } in
    let result = App.run app' in
    if all_safe result && preconditions_reachable app' then Some accumulated
    else
      let proposed = witnesses_of result |> List.concat_map propose_from_witness |> dedupe in
      let fresh =
        List.filter (fun p ->
          not (List.exists (fun a -> bexp_eq a p) accumulated))
          proposed
      in
      if fresh = [] then None
      else witness_loop ~iter_cap app (accumulated @ fresh) (iter + 1)

let verifies (app : App.t) (extras : Exp.bexp list) : bool =
  let app' = { app with assumes = app.assumes @ extras } in
  all_safe (App.run app') && preconditions_reachable app'

(* Blanket fallback for cases where the witness loop exits without
   clearing — that happens when Z3 picks witnesses whose values
   already satisfy the structurally-needed predicate (so witness-
   driven can't propose it). Predicates over all int kernel params
   against every block/grid axis; shrink trims the redundant ones. *)
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

(* Greedy drop-clause shrinker. Walks [extras] in order and keeps a
   clause iff dropping it causes some kernel to fail DRF. Locally
   minimal; not globally minimal. *)
let shrink (app : App.t) (extras : Exp.bexp list) : Exp.bexp list =
  let rec loop kept remaining =
    match remaining with
    | [] -> kept
    | c :: rest ->
      if verifies app (kept @ rest)
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
  let baseline = App.run app in
  if all_safe baseline then begin
    print_endline "DRF under baseline (--assume-launch --assume-dims --assume-delin).";
    print_endline "No extra --assume needed.";
    Ok ()
  end else begin
    match witness_loop app [] 0 with
    | Some extras when extras <> [] ->
      let minimal = shrink app extras in
      print_endline "DRF after witness-driven refinement.";
      print_endline ("Discovered: " ^ format_assume_flags minimal);
      Ok ()
    | _ ->
      let blanket = blanket_extras app in
      if blanket <> [] && verifies app blanket then begin
        let minimal = shrink app blanket in
        print_endline "DRF after blanket fallback.";
        print_endline ("Discovered: " ^ format_assume_flags minimal);
        Ok ()
      end else begin
        print_endline "Racy; either a real race or a modelling gap.";
        Ok ()
      end
  end

let () = exit (Cmd.eval_result main)
