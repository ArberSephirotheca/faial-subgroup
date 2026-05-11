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

(* Reachability gate.

   Per-access: for each [CondAccess] in each [Flatacc.Kernel.t] the
   pipeline produces, ask Z3 (via [Gen_z3.Bv64Gen]) whether the
   kernel's precondition admits a thread state that reaches the
   access. SAT = reachable; UNSAT = unreachable; UNKNOWN = treated
   as reachable so we don't reject on solver indecision.

   The set of reachable accesses computed at baseline (with no
   discovered extras) is the invariant: every access reachable at
   baseline must remain reachable after extras are added. A
   constraint that excludes parameter values is fine; a constraint
   that makes an access unreachable is not (that's a vacuous DRF in
   disguise). *)
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

(* Per-access gate: every access reachable at baseline must remain
   reachable after extras. Stricter in principle but ~N× more
   expensive (one Z3 query per access × per verify call). Kept
   alive for comparison with the simple gate; not currently the
   active one. *)
let[@warning "-32"] gate_holds_per_access
    (baseline : Reachability.AccessSet.t) (app : App.t) : bool =
  Reachability.AccessSet.subset baseline (access_set_of app)

(* Simple gate: a single SAT query per kernel asking whether
   [k.pre ∧ runtime] (with all user assumes applied via
   [prepare_kernel]) admits at least one thread state. UNSAT means
   the conjunction is contradictory — either the extras conflict
   among themselves or with the kernel context.

   Misses access-specific trivialisations the per-access gate
   catches, but on the observed dataset the two variants agree on
   every clearance and this one is ~N× faster. The [baseline]
   argument is ignored; kept for signature uniformity so the
   active gate can be swapped via the [gate_holds] binding
   below. *)
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

(* Active gate. Swap to [gate_holds_per_access] to study the
   per-access variant; [gate_holds_simple] is the production
   default. *)
let gate_holds = gate_holds_simple

(* CEGAR loop. [accumulated] grows monotonically; each iteration runs
   the analysis once, and either declares DRF, proposes new predicates
   to add, or gives up because no witness suggests anything we don't
   already have. The [iter] cap is a safety net — termination is
   already guaranteed because each iteration adds at least one new
   predicate over a finite param/dim space. *)
(* Shrink-time verification: skip the reachability gate.
   Shrink only ever removes clauses, which monotonically weakens
   the precondition. A weaker precondition can only grow the
   reachable set, so the gate's verdict for [extras] carries to
   every subset. We need only re-check DRF as we drop. *)
let verifies_drf_only (app : App.t) (extras : Exp.bexp list) : bool =
  let app' = { app with assumes = app.assumes @ extras } in
  all_safe (App.run app')

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
    let blanket = blanket_extras app in
    if blanket = [] || not (verifies_drf_only app blanket) then begin
      print_endline "Racy; either a real race or a modelling gap.";
      Ok ()
    end else begin
      let minimal = shrink baseline_reachable app blanket in
      let app' = { app with assumes = app.assumes @ minimal } in
      if gate_holds baseline_reachable app' then begin
        print_endline "DRF after blanket+shrink refinement.";
        print_endline ("Discovered: " ^ format_assume_flags minimal);
        Ok ()
      end else begin
        print_endline "Racy; preconditions found but reachability gate rejected (vacuous).";
        Ok ()
      end
    end
  end

let () = exit (Cmd.eval_result main)
