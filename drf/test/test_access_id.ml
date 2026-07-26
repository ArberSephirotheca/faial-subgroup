open Stage0
open Protocols
open Exp
open Drf

let arch : Architecture.t = Architecture.Block

let array_a : Variable.t = Variable.from_name "A"
let tid : nexp = Var Variable.tid_x
let shared_int : Memory.t = Memory.from_type Mem_hierarchy.SharedMemory Ty.int

let write (idx : nexp) : Code.t = Code.Access (Access.write array_a [ idx ] None)

let mk_kernel (code : Code.t) : Kernel.t =
  {
    name = "k_test";
    global_variables = Params.empty;
    local_variables = Params.empty;
    arrays = Variable.Map.add array_a shared_int Variable.Map.empty;
    pre = Bool true;
    code;
    visibility = Visibility.Global;
    grid_dim = None;
    block_dim = None;
  }

let race_proofs (k : Kernel.t) : Symbexp.Proof.t list =
  k
  |> Kernel.apply_arch arch
  |> Wellformed.translate
  |> Streamutil.map Wellformed.Kernel.trim_binders
  |> Aligned.translate
  |> Phasesplit.translate
  |> Locsplit.translate
  |> Flatacc.translate arch
  |> Symbexp.translate arch
  |> Streamutil.to_list

let rec collect_ids (c : Code.t) : Access.Id.t list =
  match c with
  | Code.Skip | Code.Sync _ -> []
  | Code.Access a -> [ Access.id a ]
  | Code.If (_, p, q) | Code.Seq (p, q) -> collect_ids p @ collect_ids q
  | Code.Loop { body; _ } | Code.Decl { body; _ } -> collect_ids body

let all_stamped (ids : Access.Id.t list) : bool =
  List.for_all (fun i -> not (Access.Id.equal i Access.Id.unstamped)) ids

let all_distinct (ids : Access.Id.t list) : bool =
  let ints = List.map Access.Id.to_int ids in
  List.length (List.sort_uniq Int.compare ints) = List.length ints

let proof_ids (p : Symbexp.Proof.t) : Access.Id.t list =
  List.map (fun (a : Symbexp.AccessSummary.t) -> Access.id a.access) p.accesses

(* Two structurally-identical writes to A[tid], plus A[tid+1]. Structural
   equality alone would collapse the first two; the mint must keep them
   apart by giving every static occurrence its own id. *)
let dup_body : Code.t =
  Code.seq (write tid) (Code.seq (write tid) (write (n_plus tid (Num 1))))

let test_mint_stamps_distinct () =
  let k = Kernel.reset_variable_kind (mk_kernel dup_body) in
  let ids = collect_ids k.code in
  Alcotest.(check int) "three accesses stamped" 3 (List.length ids);
  Alcotest.(check bool) "no access left unstamped" true (all_stamped ids);
  Alcotest.(check bool) "all access ids distinct" true (all_distinct ids)

let test_within_proof_distinct () =
  let k = Kernel.reset_variable_kind (mk_kernel dup_body) in
  let proofs = race_proofs k in
  Alcotest.(check bool) "at least one proof emitted" true (proofs <> []);
  Alcotest.(check bool) "some proof carries multiple accesses" true
    (List.exists
       (fun (p : Symbexp.Proof.t) -> List.length p.accesses >= 2)
       proofs);
  List.iter
    (fun (p : Symbexp.Proof.t) ->
      let ids = proof_ids p in
      Alcotest.(check bool) "proof access ids stamped" true (all_stamped ids);
      Alcotest.(check bool) "proof access ids distinct" true (all_distinct ids))
    proofs

let () =
  Alcotest.run "access_id"
    [
      ( "mint",
        [
          Alcotest.test_case "reset stamps distinct ids" `Quick
            test_mint_stamps_distinct;
        ] );
      ( "within-proof",
        [
          Alcotest.test_case "proof access ids are distinct" `Quick
            test_within_proof_distinct;
        ] );
    ]
