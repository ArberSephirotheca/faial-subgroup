open Stage0
open Drf_genie

(* Stats and Phase_timer carry module-level mutable state that
   [compute_verdict] reads to emit per-invocation JSON. If [reset] is
   ever silently broken, multi-invocation harnesses would report
   stale or accumulated counters. These tests pin the contract. *)

let read_stats_count (key : string) : int =
  match Stats.to_json () with
  | `Assoc kvs ->
    (match List.assoc_opt key kvs with
     | Some (`Int n) -> n
     | _ -> 0)
  | _ -> 0

let stats_is_empty () : bool =
  Stats.to_json () = `Assoc []

let test_stats_incr_and_reset () =
  Stats.reset ();
  Alcotest.(check bool) "reset leaves stats empty" true (stats_is_empty ());
  Stats.incr "a";
  Stats.incr "a";
  Stats.incr ~by:5 "b";
  Stats.set "c" 42;
  Alcotest.(check int) "incr a twice" 2 (read_stats_count "a");
  Alcotest.(check int) "incr b by 5"  5 (read_stats_count "b");
  Alcotest.(check int) "set c to 42" 42 (read_stats_count "c");
  Stats.reset ();
  Alcotest.(check bool) "reset clears all" true (stats_is_empty ());
  Alcotest.(check int)  "a is gone" 0 (read_stats_count "a");
  Alcotest.(check int)  "b is gone" 0 (read_stats_count "b");
  Alcotest.(check int)  "c is gone" 0 (read_stats_count "c")

let test_stats_set_overrides () =
  Stats.reset ();
  Stats.set "x" 1;
  Stats.set "x" 7;
  Alcotest.(check int) "set replaces" 7 (read_stats_count "x");
  Stats.reset ()

let test_stats_order_preserves_first_insertion () =
  Stats.reset ();
  Stats.incr "first";
  Stats.incr "second";
  Stats.incr "third";
  Stats.incr "first";  (* updating, not inserting *)
  let keys = match Stats.to_json () with
    | `Assoc kvs -> List.map fst kvs
    | _ -> []
  in
  Alcotest.(check (list string)) "insertion order"
    [ "first"; "second"; "third" ] keys;
  Stats.reset ()

let read_phase_seconds (key : string) : float =
  match Phase_timer.to_json () with
  | `Assoc kvs ->
    (match List.assoc_opt key kvs with
     | Some (`Float f) -> f
     | _ -> 0.0)
  | _ -> 0.0

let phase_timer_is_empty () : bool =
  Phase_timer.to_json () = `Assoc []

let test_phase_timer_measure_and_reset () =
  Phase_timer.reset ();
  Alcotest.(check bool) "reset leaves phase_timer empty"
    true (phase_timer_is_empty ());
  let _ = Phase_timer.measure "noop" (fun () -> 42) in
  let _ = Phase_timer.measure "noop" (fun () -> 43) in
  let _ = Phase_timer.measure "other" (fun () -> ()) in
  Alcotest.(check bool) "noop accumulated"
    true (read_phase_seconds "noop" >= 0.0);
  Alcotest.(check bool) "other recorded"
    true (read_phase_seconds "other" >= 0.0);
  Alcotest.(check bool) "non-empty after measure"
    false (phase_timer_is_empty ());
  Phase_timer.reset ();
  Alcotest.(check bool) "reset clears phase_timer"
    true (phase_timer_is_empty ())

let test_phase_timer_records_exception () =
  Phase_timer.reset ();
  let raised =
    try
      let _ = Phase_timer.measure "raises" (fun () -> failwith "boom") in
      false
    with Failure _ -> true
  in
  Alcotest.(check bool) "exception propagates" true raised;
  Alcotest.(check bool) "elapsed still recorded under exception"
    false (phase_timer_is_empty ());
  Phase_timer.reset ()

let stats_tests = [
  ("incr / set / reset roundtrip", `Quick, test_stats_incr_and_reset);
  ("set overrides prior value",   `Quick, test_stats_set_overrides);
  ("first-insertion order kept",  `Quick, test_stats_order_preserves_first_insertion);
]

let phase_timer_tests = [
  ("measure / reset roundtrip",       `Quick, test_phase_timer_measure_and_reset);
  ("measure records on exception",    `Quick, test_phase_timer_records_exception);
]

let () =
  Alcotest.run "drf_genie/stats" [
    ("Stats", stats_tests);
    ("Phase_timer", phase_timer_tests);
  ]
