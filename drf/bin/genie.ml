open Stage0
open Protocols
open Protocols_parsing
open Drf
open Cmdliner

type verdict_source = Source_baseline | Source_abductive | Source_blanket

let source_to_string = function
  | Source_baseline -> "baseline"
  | Source_abductive -> "abductive"
  | Source_blanket -> "blanket"

type verdict =
  | Drf of { source : verdict_source; assumes : Exp.bexp list }
  | Drf_vacuous
  | Racy

let conv_bexp =
  let parse s =
    match Parsers.BExpParser.of_string s with
    | Ok b -> Ok b
    | Error msg -> Error (`Msg msg)
  in
  let print ppf (b : Exp.bexp) = Format.fprintf ppf "%s" (Exp.b_to_string b) in
  Arg.conv (parse, print)

let conv_tactic =
  let parse s =
    match Parsers.TacticParser.of_string s with
    | Ok t -> Ok t
    | Error msg -> Error (`Msg msg)
  in
  let print ppf (t : Gen_z3.Tactic.t) =
    Format.fprintf ppf "%s" (Gen_z3.Tactic.to_string t)
  in
  Arg.conv (parse, print)

let default_solve_tactic : Gen_z3.Tactic.t =
  Gen_z3.Tactic.and_then_ex
    [ Tactic "simplify"; Tactic "solve-eqs"; Tactic "bv" ]

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

(* CUDA built-in launch-config variables (threadIdx, blockIdx,
   blockDim, gridDim) are unsigned int. Other variables are looked
   up in the app's kernels' [Params]; absent or non-int → default
   [Signed]. First match across kernels wins. *)
let signedness_of_app (app : App.t) (v : Variable.t) : Signedness.t =
  if Variable.Set.mem v launch_config_set then Signedness.Unsigned
  else
    let rec scan = function
      | [] -> Signedness.Signed
      | (k : Kernel.t) :: rest ->
        let p = Params.union_left k.global_variables k.local_variables in
        match Params.find_opt v p with
        | Some (_, ty) when C_type.is_unsigned ty -> Signedness.Unsigned
        | Some _ -> Signedness.Signed
        | None -> scan rest
    in
    scan app.kernels

(* Reduce a [bexp]'s free-variable set to a single signedness by
   "any-unsigned wins" — matches C's usual arithmetic conversions
   for the operator that would coerce these operands. *)
let bexp_signedness (sign : Variable.t -> Signedness.t) (b : Exp.bexp)
    : Signedness.t =
  let fvs = Exp.b_free_names b Variable.Set.empty in
  Variable.Set.fold (fun v acc ->
    match acc, sign v with
    | Signedness.Unsigned, _ | _, Signedness.Unsigned -> Signedness.Unsigned
    | _, _ -> Signedness.Signed)
    fvs Signedness.Signed

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

(* Cached gate. Per kernel we keep one Z3 context, one solver with
   the base encoding ([kernel.pre + runtime] under
   [prepare_kernel ~assumes:[]]) permanently added, and the
   substitution that translates [Kernel.inline_globals]'s effect on
   bexps. Per gate call the round's assumes are substituted through
   the same map, then pushed onto the solver, checked, and popped —
   keeping Z3's learned clauses alive across CEGAR rounds. *)
module Gate_cache = struct
  type t = (string, Reachability.Slot.t) Hashtbl.t

  let create () : t = Hashtbl.create 8

  let get_or_init (cache : t) ~(timeout : int option)
      ~(assume_dims : bool) ~(params : (string * int) list)
      (k : Kernel.t) : Reachability.Slot.t =
    match Hashtbl.find_opt cache k.name with
    | Some s -> s
    | None ->
      let s =
        Reachability.make_slot ~timeout ~assume_dims ~params k
      in
      Hashtbl.add cache k.name s;
      s
end

let gate_holds_cached (cache : Gate_cache.t)
    (_baseline : Reachability.AccessSet.t) (app : App.t) : bool =
  app.kernels |> App.only_kernel app
  |> List.for_all (fun k ->
    let slot =
      Gate_cache.get_or_init cache
        ~timeout:app.timeout
        ~assume_dims:app.assume_dims
        ~params:app.params
        k
    in
    Reachability.preconditions_satisfiable_delta slot app.assumes)

let[@warning "-32"] gate_holds = gate_holds_simple

let run_assuming (assumptions : Exp.bexp list) (app : App.t) : Analysis.t list =
  { app with assumes = app.assumes @ assumptions }
  |> App.run

let verifies_drf_only (app : App.t) (assumptions : Exp.bexp list) : bool =
  app
  |> run_assuming assumptions
  |> all_safe

let shrink_linear (_baseline : Reachability.AccessSet.t) (app : App.t)
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

(* UNSAT-core shrink: run the DRF pipeline once with [extras] added as
   tracked Z3 assumptions (named [extra_<id>]) instead of conjoining
   them into [kernel.pre]. Each per-proof outcome is either DRF (with
   the subset of extras the Z3 unsat-core mentions) or Racy /
   Unknown. The minimal set is the UNION of cores across all proofs.

   Replaces the O(N) drop-clause loop in [shrink_linear] with one
   pipeline run; the speedup is roughly the number of extras (~18
   for is-cuda).

   Falls back to [None] if any proof returns Racy / Unknown, or if
   the union is empty under non-empty [extras] (defensive: would
   imply the formula was already UNSAT without any extra). The
   caller should use [shrink_linear] as the fallback. *)
let shrink_via_core (_baseline : Reachability.AccessSet.t) (app : App.t)
    (extras : Exp.bexp list) : Exp.bexp list option =
  if extras = [] then Some []
  else
    let extras_a = Array.of_list extras in
    let tagged =
      List.mapi (fun i b -> (string_of_int i, b)) extras
    in
    let analyses = App.run { app with core_extras = tagged } in
    if not (all_safe analyses) then None
    else
      let needed_ids =
        analyses
        |> List.concat_map (fun (a : Analysis.t) ->
            a.report
            |> List.concat_map (fun (s : Solve_drf.Solution.t) ->
                match s.outcome with
                | Solve_drf.Outcome.Drf_with_core c -> c
                | _ -> []))
        |> List.sort_uniq String.compare
      in
      (* Empty-core guard. With non-empty [extras], a union that
         collapses to [] means one of:
         - the baseline pre is already UNSAT-strong enough (compute_verdict
           would normally catch this earlier — but a stale [extras] set
           could land here),
         - tracker elimination by a tactic stage (we currently bypass the
           tactic for the core path, so this shouldn't happen — but if a
           future change reactivates the tactic, this catches the silent
           empty-core case the specialist flagged),
         - some other Z3 quirk.
         In all three, the linear shrink is the safe fallback. *)
      if needed_ids = [] then None
      else
        let ( let* ) = Option.bind in
        let kept =
          needed_ids
          |> List.filter_map (fun id ->
              let* i = int_of_string_opt id in
              if i >= 0 && i < Array.length extras_a
              then Some extras_a.(i) else None)
        in
        Some kept

let shrink ~(use_core : bool) (baseline : Reachability.AccessSet.t)
    (app : App.t) (extras : Exp.bexp list) : Exp.bexp list =
  if use_core then
    match shrink_via_core baseline app extras with
    | Some kept -> kept
    | None -> shrink_linear baseline app extras
  else
    shrink_linear baseline app extras

(* Per-clause weakening lattice. Replacing an equality with one of its
   one-sided variants admits strictly more models; if the kernel still
   clears DRF and passes the gate under the weaker variant, prefer it.
   In the bm3d-style synthesis miss [size == gridDim.x ∧ size == bdim*gdim],
   weakening the first clause to [size >= gridDim.x] breaks the
   conjunction's implied [bdim.x == 1] and lets the gate accept. *)
let weaken_clause (sign : Variable.t -> Signedness.t)
    : Exp.bexp -> Exp.bexp list = function
  | Exp.NRel (Eq, e1, e2) as b ->
    let s = bexp_signedness sign b in
    [ Exp.NRel (Ge s, e1, e2); Exp.NRel (Le s, e1, e2) ]
  | _ -> []

(* Walk [extras] left-to-right; for each clause, if some weaker variant
   keeps the predicate [check] true, swap it in. Single-pass and
   per-clause (no joint weakening) — keeps the search tractable. *)
let weaken_for_gate (sign : Variable.t -> Signedness.t)
    (check : Exp.bexp list -> bool) (extras : Exp.bexp list) : Exp.bexp list =
  let rec loop acc = function
    | [] -> acc
    | c :: rest ->
      let weakers = weaken_clause sign c in
      let best =
        List.find_opt
          (fun w -> check (acc @ (w :: rest)))
          weakers
      in
      let kept = match best with Some w -> w | None -> c in
      loop (acc @ [ kept ]) rest
  in
  loop [] extras

(* Abductive search with weakening and CEGIS-style gate-rejection
   feedback. On each iteration:
     - Solve MaxSAT for a minimum-cardinality clearance.
     - Run faial; if still racy, add witnesses, re-solve.
     - If DRF: shrink, then check the gate. If gate accepts, return.
       If gate rejects, try clause-wise weakening; if that recovers,
       return the weakened set. Otherwise add [¬extras] to the session
       (CEGIS) and re-solve. *)
let abductive_loop
    ?(iter_cap = 32)
    ~(use_core_shrink : bool)
    ~(gate_check : Reachability.AccessSet.t -> App.t -> bool)
    (app : App.t)
    (baseline_reachable : Reachability.AccessSet.t) : Exp.bexp list option =
  let kernels = App.only_kernel app app.kernels in
  if kernels = [] then None
  else
    let session = Abduction.create_for_kernels kernels in
    let sign = signedness_of_app app in
    let drf_and_gate extras =
      verifies_drf_only app extras
      && gate_check baseline_reachable
           { app with assumes = app.assumes @ extras }
    in
    let shrink' = shrink ~use_core:use_core_shrink in
    let try_finalize extras =
      let minimal = shrink' baseline_reachable app extras in
      let app' = { app with assumes = app.assumes @ minimal } in
      if gate_check baseline_reachable app' then Some minimal
      else
        let weakened = weaken_for_gate sign drf_and_gate minimal in
        let weakened_min = shrink' baseline_reachable app weakened in
        let app'' = { app with assumes = app.assumes @ weakened_min } in
        if gate_check baseline_reachable app'' then Some weakened_min
        else None
    in
    let rec loop iter extras =
      if iter >= iter_cap then None
      else
        let result = run_assuming extras app in
        if all_safe result then
          match try_finalize extras with
          | Some final -> Some final
          | None ->
            (* Gate rejected even after weakening — ban this exact
               combination and re-solve. *)
            if Abduction.reject_combination session extras = 0 then None
            else
              (match Abduction.solve session with
               | None -> None
               | Some new_extras -> loop (iter + 1) new_extras)
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
  let sign = signedness_of_app app in
  let signs =
    (* [v > 0] in C uses [v]'s declared signedness against the [0]
       literal. Per "any-unsigned wins", [sign v] dominates (the
       numeric [0] would promote). *)
    params |> List.map (fun v ->
      Exp.NRel (Gt (sign v), Exp.Var v, Exp.Num 0))
  in
  let bounds =
    (* [p >= dim] mixes a kernel-param [p] with a CUDA built-in dim.
       Dim is unsigned, so the comparison is unsigned. *)
    params |> List.concat_map (fun p ->
      List.map (fun d ->
        Exp.NRel (Ge Unsigned, Exp.Var p, Exp.Var d)) dims)
  in
  signs @ bounds

let format_assume_flags (extras : Exp.bexp list) : string =
  extras
  |> List.map (fun b -> "--assume \"" ^ Exp.b_to_string b ^ "\"")
  |> String.concat " "

(* Use-derived dim bounds. For each axis (x, y, z) and level (thread,
   block): if the corresponding index variable is referenced in the
   kernel code, the dim is constrained to [>= 2]; otherwise the dim is
   pinned to [== 1]. The [>= 2] half is what stops abductive from
   landing on trivialising clearances (e.g. bm3d's
   [size == gridDim.x ∧ size == blockDim.x * gridDim.x] entailing
   [blockDim.x == 1]).

   Each candidate is pre-flight SAT-checked against the kernel's
   prepared pre. Constraints that conflict with an existing pin
   (most commonly a launch literal — [bm3d]'s launch site pins
   [blockDim.y == 1] via [--assume-launch]) are dropped. *)
let usage_constrained_kernel
    ?(timeout : int option)
    ~(params : (string * int) list)
    (k : Kernel.t) : Kernel.t =
  let used = Code.free_names k.code Variable.Set.empty in
  let probe0 =
    Reachability.prepare_kernel ~assumes:[] ~assume_dims:false ~params k
  in
  let open Variable in
  [ tid_x, bdim_x; tid_y, bdim_y; tid_z, bdim_z;
    bid_x, gdim_x; bid_y, gdim_y; bid_z, gdim_z ]
  |> List.fold_left (fun (probe, k_acc) (idx, dim) ->
    let candidate =
      if Set.mem idx used
      (* [dim] is a CUDA built-in (unsigned int), so the comparison
         is unsigned. *)
      then Exp.NRel (Ge Unsigned, Var dim, Num 2)
      else Exp.NRel (Eq, Var dim, Num 1)
    in
    let probe' = Kernel.add_pre candidate probe in
    if Reachability.preconditions_satisfiable ?timeout probe'
    then (probe', Kernel.add_pre candidate k_acc)
    else (probe, k_acc))
    (probe0, k)
  |> snd

(* Z3 raises [Z3.Error "max. memory exceeded"] when a query exhausts
   its memory cap (default ~6 GB). Treat it as an inconclusive result —
   we couldn't prove DRF, so report [Racy] and let the caller decide. *)
let compute_verdict ~(use_core_shrink : bool) ~(cached_gate : bool)
    (app : App.t) : verdict =
  try
    let gate_check =
      if cached_gate then
        let cache = Gate_cache.create () in
        gate_holds_cached cache
      else
        gate_holds_simple
    in
    let baseline_reachable = access_set_of app in
    let baseline = App.run app in
    if all_safe baseline then
      if Reachability.AccessSet.is_empty baseline_reachable then Drf_vacuous
      else Drf { source = Source_baseline; assumes = [] }
    else
      match
        abductive_loop ~use_core_shrink ~gate_check app baseline_reachable
      with
      | Some minimal -> Drf { source = Source_abductive; assumes = minimal }
      | None ->
        let blanket = blanket_extras app in
        if blanket = [] || not (verifies_drf_only app blanket) then Racy
        else
          let minimal =
            shrink ~use_core:use_core_shrink baseline_reachable app blanket
          in
          let app' = { app with assumes = app.assumes @ minimal } in
          if gate_check baseline_reachable app'
          then Drf { source = Source_blanket; assumes = minimal }
          else Racy
  with Z3.Error _ -> Racy

let report_prose (v : verdict) : unit =
  match v with
  | Drf { source = Source_baseline; _ } ->
    print_endline
      "DRF under baseline (--assume-launch --assume-dims --assume-delin).";
    print_endline "No extra --assume needed."
  | Drf_vacuous ->
    print_endline
      "Baseline preconditions are unsatisfiable — kernel is vacuously DRF.";
    print_endline
      "Check that the kernel and any user --assume flags are mutually \
       satisfiable."
  | Drf { source; assumes } ->
    let label = match source with
      | Source_abductive -> "abductive refinement"
      | Source_blanket -> "blanket fallback"
      | Source_baseline -> assert false
    in
    print_endline ("DRF after " ^ label ^ ".");
    print_endline ("Discovered: " ^ format_assume_flags assumes)
  | Racy ->
    print_endline
      "Racy; either a real race or a modelling gap (or vacuous DRF rejected)."

let report_json (app : App.t) (v : verdict) : unit =
  let verdict_str, source_json, assumes_json = match v with
    | Drf { source; assumes } ->
      "drf",
      `String (source_to_string source),
      `List (List.map (fun b -> `String (Exp.b_to_string b)) assumes)
    | Drf_vacuous -> "drf_vacuous", `Null, `List []
    | Racy -> "racy", `Null, `List []
  in
  let status = match v with Racy -> "racy" | _ -> "drf" in
  let kernels =
    App.only_kernel app app.kernels
    |> List.map (fun (k : Kernel.t) ->
      `Assoc [
        ("kernel_name", `String k.name);
        ("status", `String status);
      ])
  in
  `Assoc [
    ("verdict", `String verdict_str);
    ("source", source_json);
    ("assumes", assumes_json);
    ("kernels", `List kernels);
    ("phase_times", Phase_timer.to_json ());
    ("argv",
     `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x)));
    ("executable_name", `String Sys.executable_name);
    ("z3_version", `String Z3.Version.to_string);
  ]
  |> Yojson.Basic.to_string
  |> print_endline

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
  and+ solve_tactic =
    let default_doc =
      Gen_z3.Tactic.to_string default_solve_tactic
    in
    Arg.(value & opt (some conv_tactic) (Some default_solve_tactic)
         & info [ "solve-tactic" ] ~docv:"TACTIC"
             ~doc:("Z3 tactic expression for the race-query solver. \
                    Default: " ^ default_doc))
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
  and+ output_json =
    Arg.(value & flag
         & info [ "json" ] ~doc:"Output result as a single JSON object.")
  and+ use_core_shrink =
    Arg.(value & flag
         & info [ "shrink-core" ]
             ~doc:"Use UNSAT-core extraction to shrink the abductive \
                   precondition in one Z3 call, instead of the default \
                   linear drop-clause loop. Falls back to the linear \
                   path on any racy / unknown subproof.")
  and+ cached_gate =
    Arg.(value & flag
         & info [ "gate-cache" ]
             ~doc:"Reuse a single Z3 context and solver per kernel \
                   across abductive rounds (push/pop on the assertion \
                   stack), preserving learned clauses. Disable to fall \
                   back to a fresh context per gate call.")
  in
  let archs = [ Architecture.Block ] in
  let app =
    App.parse
      ~filename ~timeout
      ~show_proofs:false ~show_proto:false ~show_wf:false ~show_align:false
      ~show_delin:false ~show_phase_split:false ~show_loc_split:false
      ~show_flat_acc:false ~show_symbexp:false
      ~logic ~solve_tactic
      ~ge_index:[] ~le_index:[] ~eq_index:[]
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
      ~assume_dims:false
      ~assume_launch:true
      ~cbor:true
      ~stop_at:None
  in
  let app =
    let kernels =
      List.map (usage_constrained_kernel ?timeout ~params:app.params)
        app.kernels
    in
    { app with kernels }
  in
  let v = compute_verdict ~use_core_shrink ~cached_gate app in
  if output_json then report_json app v else report_prose v;
  Ok ()

let () = exit (Cmd.eval_result main)
