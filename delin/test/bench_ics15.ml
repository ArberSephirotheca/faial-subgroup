(* Slow differential benchmark for the ICS15 drivers. Parked on the
   [@slow] alias so it does not run under [dune test] / [dune runtest];
   invoke with [dune build @delin/test/slow] (or via [scripts/test-slow]).

   For an n-parameter row-major shape the reference driver enumerates
   all n! permutations, recomputing the bucket map each time, while the
   optimized driver computes the bucket map once and prunes the search
   with the per-atom quotient table. Consumers take only the first
   candidate, which both drivers reach early, so to expose the per-
   permutation cost the bench instead drains the whole candidate stream
   (forcing the reference through every permutation) and times that. It
   asserts both drivers agree on the first candidate and on the total
   candidate count; any disagreement exits non-zero so the alias fails. *)

open Protocols
open Exp

let mult a b = Binary (N_binary.Mult Signedness.Signed, a, b)
let plus a b = Binary (N_binary.Plus Signedness.Signed, a, b)
let p k = Var (Variable.from_name (Printf.sprintf "P%d" k))
let i k = Var (Variable.from_name (Printf.sprintf "i%d" k))

(* Linearisation of a shape [A[?][P1]..[Pn]]: term j is index [i_j]
   scaled by the product [P_{j+1} * .. * Pn] (term n is bare [i_n]).
   The candidate parameter set is exactly [{P1, .., Pn}], so the search
   ranges over n! permutations. *)
let build_expr (n : int) : nexp =
  let rec suffix_prod j =
    if j > n then None
    else match suffix_prod (j + 1) with
      | None -> Some (p j)
      | Some r -> Some (mult (p j) r)
  in
  let term j =
    match suffix_prod (j + 1) with None -> i j | Some sp -> mult (i j) sp
  in
  let rec sum j = if j = n then term n else plus (term j) (sum (j + 1)) in
  sum 0

let globals (n : int) : Variable.Set.t =
  List.init n (fun k -> Variable.from_name (Printf.sprintf "P%d" (k + 1)))
  |> Variable.Set.of_list

let expr_list_eq (a : Poly.t list) (b : Poly.t list) : bool =
  List.length a = List.length b
  && List.for_all2 (fun x y -> Poly.compare x y = 0) a b

let index_eq (a : Subscript.t) (b : Subscript.t) : bool =
  expr_list_eq a.numeral b.numeral
  && expr_list_eq a.radix b.radix

let time (f : unit -> 'a) : 'a * float =
  let t0 = Sys.time () in
  let r = f () in
  (r, Sys.time () -. t0)

let head s = s |> Seq.uncons |> Option.map fst
let count s = Seq.fold_left (fun n _ -> n + 1) 0 s

let run_one (n : int) : bool =
  let expr = Poly.from_nexp ~globals:(globals n) (build_expr n) in
  let size_params = Shape.size_params expr in
  let globals = globals n in
  let first_ref = head (Ics15.candidates ~globals ~size_params expr) in
  let first_opt = head (Ics15_opt.candidates ~globals ~size_params expr) in
  let n_ref, t_ref =
    time (fun () -> count (Ics15.candidates ~globals ~size_params expr))
  in
  let n_opt, t_opt =
    time (fun () -> count (Ics15_opt.candidates ~globals ~size_params expr))
  in
  let first_agree =
    match first_ref, first_opt with
    | None, None -> true
    | Some a, Some b -> index_eq a b
    | _ -> false
  in
  let agree = first_agree && n_ref = n_opt in
  let found = match first_opt with Some _ -> "found" | None -> "none" in
  let speedup = if t_opt > 0. then t_ref /. t_opt else infinity in
  Printf.printf
    "n=%d  reference=%.3fs (%d cand)  optimized=%.3fs (%d cand)  \
     speedup=%.1fx  first=%s  agree=%s\n%!"
    n t_ref n_ref t_opt n_opt speedup found (if agree then "yes" else "NO");
  agree

let () =
  let ns =
    match Array.to_list Sys.argv with
    | _ :: (_ :: _ as rest) -> List.filter_map int_of_string_opt rest
    | _ -> [ 5; 6; 7 ]
  in
  let all_agree = List.fold_left (fun acc n -> run_one n && acc) true ns in
  if not all_agree then (
    prerr_endline "bench_ics15: drivers disagreed";
    exit 1)
