open Stage0
open Protocols
open Drf
open Inference

type t = {
  filename : string;
  kernels : Kernel.t list;
  timeout : int option;
  show_proofs : bool;
  show_proto : bool;
  show_wf : bool;
  show_align : bool;
  show_phase_split : bool;
  show_loc_split : bool;
  show_flat_acc : bool;
  show_symbexp : bool;
  logic : string option;
  le_index : int list;
  ge_index : int list;
  eq_index : int list;
  only_kernel : string option;
  only_array : string option;
  only_true_data_races : bool;
  thread_idx_1 : Dim3.t option;
  thread_idx_2 : Dim3.t option;
  block_idx_1 : Dim3.t option;
  block_idx_2 : Dim3.t option;
  archs : Architecture.t list;
  block_dim : Dim3.t option;
  grid_dim : Dim3.t option;
  params : (string * int) list;
  macros : string list;
  ignore_asserts : bool;
  assumes : Exp.bexp list;
  assume_dims : bool;
  assume_launch : bool;
}

let to_string (app : t) : string =
  let opt_s (o : string option) : string = Option.value ~default:"null" o in
  let opt : 'a. ('a -> string) -> 'a option -> string =
   fun f o -> o |> Option.map f |> opt_s
  in
  let int = string_of_int in
  let bool (b : bool) : string = if b then "true" else "false" in
  let dim3 (o : Dim3.t) : string = Dim3.to_string o in

  let list_string (l : string list) : string =
    "[" ^ String.concat ", " l ^ "]"
  in

  let list_arch (l : Architecture.t list) : string =
    list_string (List.map Architecture.to_string l)
  in
  match app with
  | {
   filename;
   kernels;
   timeout;
   show_proofs;
   show_proto;
   show_wf;
   show_align;
   show_phase_split;
   show_loc_split;
   show_flat_acc;
   show_symbexp;
   logic;
   le_index = _;
   ge_index = _;
   eq_index = _;
   only_array = _;
   thread_idx_1 = _;
   block_idx_1 = _;
   thread_idx_2 = _;
   block_idx_2 = _;
   archs;
   block_dim;
   grid_dim;
   params = _;
   only_kernel;
   macros;
   only_true_data_races;
   ignore_asserts;
   assumes;
   assume_dims;
   assume_launch;
  } ->
      let only_kernel = Option.value ~default:"(null)" only_kernel in
      let kernels = List.length kernels |> string_of_int in
      "filename: " ^ filename ^ "\nonly_kernel: " ^ only_kernel
      ^ "\nblock_dim: " ^ opt dim3 block_dim ^ "\ngrid_dim: "
      ^ opt dim3 grid_dim ^ "\nkernels: " ^ kernels ^ "\ntimeout: "
      ^ opt int timeout ^ "\nlogic: " ^ opt_s logic ^ "\narchs: "
      ^ list_arch archs ^ "\nshow_proofs: " ^ bool show_proofs
      ^ "\nshow_proto: " ^ bool show_proto ^ "\nshow_wf: " ^ bool show_wf
      ^ "\nshow_align: " ^ bool show_align ^ "\nshow_phase_split: "
      ^ bool show_phase_split ^ "\nshow_loc_split: " ^ bool show_loc_split
      ^ "\nshow_flat_acc: " ^ bool show_flat_acc ^ "\nshow_symbexp: "
      ^ bool show_symbexp ^ "\nmacros = " ^ list_string macros
      ^ "\nonly_true_data_races = ^ " ^ bool only_true_data_races
      ^ "\nignore_asserts = " ^ bool ignore_asserts
      ^ "\nassume_dims = " ^ bool assume_dims
      ^ "\nassume_launch = " ^ bool assume_launch
      ^ "\nassumes: "
      ^ list_string (List.map Exp.b_to_string assumes)
      ^ "\n"

let parse ~filename ~timeout ~show_proofs ~show_proto ~show_wf ~show_align
    ~show_phase_split ~show_loc_split ~show_flat_acc ~show_symbexp ~logic
    ~ge_index ~le_index ~eq_index ~only_array ~only_kernel ~only_true_data_races
    ~thread_idx_1 ~thread_idx_2 ~block_idx_1 ~block_idx_2 ~block_dim ~grid_dim
    ~includes ~inline_calls ~archs ~ignore_parsing_errors ~params ~macros
    ~cu_to_json ~all_dims ~ignore_asserts ~assumes ~assume_dims
    ~assume_launch : t =
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      ~includes ~block_dim ~grid_dim ~inline_calls ~macros ~cu_to_json
      ~ignore_asserts ~assume_launch filename
  in
  let kernels = parsed.kernels in
  let block_dim = if all_dims then None else Some parsed.options.block_dim in
  let grid_dim = if all_dims then None else Some parsed.options.grid_dim in
  {
    filename;
    timeout;
    show_proofs;
    show_proto;
    show_wf;
    show_align;
    show_phase_split;
    show_loc_split;
    show_flat_acc;
    show_symbexp;
    logic;
    kernels;
    ge_index;
    le_index;
    eq_index;
    only_array;
    thread_idx_1;
    thread_idx_2;
    block_idx_1;
    block_idx_2;
    archs;
    grid_dim;
    block_dim;
    params;
    only_kernel;
    only_true_data_races;
    macros;
    ignore_asserts;
    assumes;
    assume_dims;
    assume_launch;
  }

let show (b : bool) (call : 'a -> unit) (x : 'a) : 'a =
  if b then call x else ();
  x

let translate (arch : Architecture.t) (a : t) (k : Kernel.t) :
    Flatacc.Kernel.t Streamutil.stream =
  k
  (* 0. filter arrays *)
  |> (fun k ->
  match a.only_array with
  | Some arr -> Protocols.Kernel.filter_array (fun x -> Variable.name x = arr) k
  | None -> k)
  (* 1. apply block-level/grid-level analysis constraints and set dimensions *)
  |> Protocols.Kernel.try_set_block_dim a.block_dim
  |> Protocols.Kernel.try_set_grid_dim a.grid_dim
  |> Protocols.Kernel.apply_arch arch
  (* 1.1 inject user-provided assumptions into the kernel precondition *)
  |> (fun k ->
    List.fold_left (fun k b -> Protocols.Kernel.add_pre b k) k a.assumes)
  (* 1.2 optionally pin unreferenced launch dimensions to 1 *)
  |> (if a.assume_dims then Protocols.Kernel.add_dim_assumptions else Fun.id)
  (* 2. inline global assignments, including block_dim/grid_dim *)
  |> Protocols.Kernel.inline_globals a.params
  (* 2.1 inline block_id as a constant when architecture is Grid *)
  |> (fun k ->
  match (arch, a.block_idx_1) with
  | Architecture.Block, Some bid ->
      let kvs = Dim3.to_assoc ~prefix:"blockIdx." bid in
      Protocols.Kernel.assign_globals kvs k
  | _, _ -> k)
  |> Protocols.Kernel.add_missing_binders
  |> (if a.only_true_data_races then Protocols.Kernel.to_ci_di else Fun.id)
  |> show a.show_proto Protocols.Kernel.print
  (* 3. constant folding optimization *)
  |> Protocols.Kernel.opt
  (* 4. convert to well-formed protocol *)
  |> Wellformed.translate
  (* 4.1. remove unnecessary binders *)
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> show a.show_wf Wellformed.print_kernels
  (* 5. align protocol *)
  |> Aligned.translate
  |> show a.show_align Aligned.print_kernels
  (* 6. split per sync *)
  |> Phasesplit.translate
  |> show a.show_phase_split Phasesplit.print_kernels
  (* 7. split per location *)
  |> Locsplit.translate
  |> show a.show_loc_split Locsplit.print_kernels
  (* 8. flatten control-flow structures *)
  |> Flatacc.translate arch
  |> show a.show_flat_acc Flatacc.print_kernels

let only_kernel (a : t) (ks : Protocols.Kernel.t list) : Protocols.Kernel.t list
    =
  match a.only_kernel with
  | Some name ->
      let ks = ks |> List.filter (fun k -> Protocols.Kernel.name k = name) in
      if ks = [] then (
        Logger.Colors.error ("kernel '" ^ name ^ "' not found!");
        exit (-1))
      else ks
  | None -> ks

let check_unreachable (a : t) : unit =
  a.kernels |> only_kernel a
  |> List.iter (fun kernel ->
      let report =
        kernel
        |> translate Architecture.Block a
        |> Symbexp.sanity_check Architecture.Block
        |> show a.show_symbexp Symbexp.print_kernels
        |> Streamutil.map (fun b ->
            (b, Solve_drf.solve ~timeout:a.timeout ~logic:a.logic b))
        |> Streamutil.to_list
      in
      Stdlib.flush_all ();
      report
      |> List.iter (fun (p, s) ->
          let open Z3.Solver in
          match s with
          | UNSATISFIABLE | UNKNOWN ->
              Symbexp.Proof.to_string p |> print_endline
          | SATISFIABLE -> ()))

let run (a : t) : Analysis.t list =
  let check_kernel arch (kernel : Protocols.Kernel.t) : Analysis.t =
    let report =
      kernel |> translate arch a |> Symbexp.translate arch
      |> Symbexp.add_rel_index N_rel.Le a.le_index
      |> Symbexp.add_rel_index N_rel.Ge a.ge_index
      |> Symbexp.add_rel_index N_rel.Eq a.eq_index
      |> Symbexp.add ~tid:a.thread_idx_1 ~bid:a.block_idx_1
      |> Symbexp.add ~tid:a.thread_idx_2 ~bid:a.block_idx_2
      |> show a.show_symbexp Symbexp.print_kernels
      |> Solve_drf.Solution.solve ~timeout:a.timeout ~_show_proofs:a.show_proofs
           ~logic:a.logic
      |> Streamutil.to_list
    in
    Analysis.{ kernel; report }
  in
  a.kernels |> only_kernel a
  |> List.map (fun kernel ->
      let rec check_until (archs : Architecture.t list) : Analysis.t =
        match archs with
        | [] -> Analysis.{ kernel; report = [] }
        | [ arch ] -> check_kernel arch kernel
        | arch :: archs ->
            let a = check_kernel arch kernel in
            if Analysis.is_safe a then check_until archs else a
      in
      check_until a.archs)
