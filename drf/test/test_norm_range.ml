open Protocols
open Exp
open Drf

let v (name : string) : Variable.t = Variable.from_name name
let lt (a : nexp) (b : nexp) : bexp = NRel (Lt Signedness.Signed, a, b)

(* A strided loop [for (i=0; i<N; i+=2)] carrying [i < N]. After loop
   normalization the counter is reparametrized ([i -> lb + 2*i$q]); the
   rewrite must reach the loop's [cond], so [i] is gone and the fresh
   index appears in its place. This is the nested-loop path. *)
let test_nested_cond_rewrite () =
  let i = v "i" in
  let range = Range.make ~step:(Range.Step.Plus (Num 2)) i (Var (v "N")) in
  let cond = lt (Var i) (Var (v "N")) in
  let body = Unsynced.Access (Access.read (v "A") [ Var i ]) in
  let cr = Cond_range.make range cond in
  match Unsynced.normalize_loops (Unsynced.Loop (Norm_range.Plain cr, body)) with
  | Unsynced.Loop ((Norm_range.Index ix as nr), _) ->
      let fvs = b_free_names ix.cond Variable.Set.empty in
      Alcotest.(check bool) "original counter i removed from cond" false
        (Variable.Set.mem i fvs);
      Alcotest.(check bool) "fresh index present in cond" true
        (Variable.Set.mem (Norm_range.var nr) fvs)
  | _ -> Alcotest.fail "expected a reparametrized (Index) loop"

(* The hoisted-loop path: a strided range with a [cond] in [k.ranges].
   [Flatacc.Kernel.from_loc_split] must apply the counter rewrite before
   conjoining the cond into [pre], so [pre] mentions no original [i]. *)
let test_hoisted_cond_rewrite () =
  let i = v "i" in
  let range = Range.make ~step:(Range.Step.Plus (Num 2)) i (Var (v "N")) in
  let cond = lt (Var i) (Var (v "N")) in
  let k : Locsplit.Kernel.t =
    {
      name = "k";
      array_name = "A";
      global_variables = Params.add (v "N") Ty.int Params.empty;
      local_variables = Params.empty;
      ranges = [ Cond_range.make range cond ];
      code = Unsynced.Access (Access.write (v "A") [ Var i ] None);
    }
  in
  match Flatacc.Kernel.from_loc_split Architecture.Block k with
  | Some fk ->
      let fvs = b_free_names fk.pre Variable.Set.empty in
      Alcotest.(check bool) "original counter i removed from hoisted pre" false
        (Variable.Set.mem i fvs)
  | None -> Alcotest.fail "expected a flat kernel"

let () =
  Alcotest.run "norm_range"
    [
      ( "substitution safety",
        [
          Alcotest.test_case "nested loop cond reparametrized" `Quick
            test_nested_cond_rewrite;
          Alcotest.test_case "hoisted loop cond reparametrized" `Quick
            test_hoisted_cond_rewrite;
        ] );
    ]
