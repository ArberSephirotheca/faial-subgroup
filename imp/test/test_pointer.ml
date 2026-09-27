open Protocols
open Exp
open Imp

let var (name : string) : Variable.t = Variable.from_name name

let plus (l : nexp) (r : nexp) : nexp =
  Binary (N_binary.Plus Signedness.Signed, l, r)

let index_testable : Pointer.Index.t Alcotest.testable =
  Alcotest.testable
    (fun fmt (i : Pointer.Index.t) ->
      Format.fprintf fmt "%s" (Pointer.Index.to_string i))
    ( = )

let addresses_testable : Pointer.Address.t list Alcotest.testable =
  Alcotest.testable
    (fun fmt (l : Pointer.Address.t list) ->
      let one (a : Pointer.Address.t) =
        Variable.name a.array ^ "["
        ^ (a.index |> List.map Pointer.Index.to_string |> String.concat ", ")
        ^ "]"
      in
      Format.fprintf fmt "%s" (l |> List.map one |> String.concat "; "))
    ( = )

let exact (value : nexp) : Pointer.Index.t = Pointer.Index.Exact { value }

let check_rescale (name : string) ~(off : nexp) ~(view : int) ~(elem : int)
    (index : nexp) (expected : Pointer.Index.t) =
  ( name,
    `Quick,
    fun () ->
      Alcotest.check index_testable name expected
        (Pointer.Offset.rescale ~off ~step:(Pointer.Step.make ~view ~elem)
           index) )

(* A byte offset over an array whose element is that same width leaves the
   index alone, and a wider or narrower view divides or spreads it. These are
   the numbers the [ptr-view] examples exercise. *)
let rescale_tests =
  [
    check_rescale "same width divides out" ~off:(Num 20) ~view:4 ~elem:4
      (Var (var "i"))
      (exact (plus (Num 5) (Var (var "i"))));
    check_rescale "byte view of an int array" ~off:(Num 0) ~view:1 ~elem:4
      (Var (var "i"))
      (exact (n_div (Var (var "i")) (Num 4)));
    check_rescale "byte view at an unaligned offset" ~off:(Num 1) ~view:1
      ~elem:4 (Var (var "i"))
      (exact (n_div (plus (Num 1) (Var (var "i"))) (Num 4)));
    ( "an int view of a byte array spans four cells",
      `Quick,
      fun () ->
        let got =
          Pointer.Offset.rescale ~off:(Num 0)
            ~step:(Pointer.Step.make ~view:4 ~elem:1)
            (Var (var "i"))
        in
        match got with
        | Pointer.Index.Span _ -> ()
        | Pointer.Index.Exact _ ->
            Alcotest.failf "expected a span, got %s"
              (Pointer.Index.to_string got) );
    ( "a view wider than its element spans",
      `Quick,
      fun () ->
        let got =
          Pointer.Offset.rescale ~off:(Num 0)
            ~step:(Pointer.Step.make ~view:16 ~elem:4)
            (Var (var "i"))
        in
        match got with
        | Pointer.Index.Span _ -> ()
        | Pointer.Index.Exact _ ->
            Alcotest.failf "expected a span, got %s"
              (Pointer.Index.to_string got) );
  ]

let check_addresses (name : string) (p : Pointer.t) (index : nexp list)
    (expected : Pointer.Address.t list) =
  ( name,
    `Quick,
    fun () ->
      Alcotest.check addresses_testable name expected
        (Pointer.addresses ~index p) )

let a : Variable.t = var "A"

let address_tests =
  [
    check_addresses "a bare array keeps its index"
      (Pointer.from_array a)
      [ Var (var "i") ]
      [ { array = a; index = [ exact (Var (var "i")) ]; guard = None } ];
    check_addresses "a shift lands on the head index only"
      (Pointer.from_array a
      |> Pointer.shift ~offset:(Pointer.Offset.elements (Num 3)))
      [ Var (var "i"); Var (var "j") ]
      [
        {
          array = a;
          index = [ exact (plus (Num 3) (Var (var "i"))); exact (Var (var "j")) ];
          guard = None;
        };
      ];
    check_addresses "a zero shift is the array itself"
      (Pointer.from_array a |> Pointer.shift ~offset:Pointer.Offset.zero)
      [ Var (var "i") ]
      [ { array = a; index = [ exact (Var (var "i")) ]; guard = None } ];
    check_addresses "a row prepends its index"
      (Pointer.from_array a |> Pointer.row ~index:(Var (var "cat")))
      [ Var (var "i") ]
      [
        {
          array = a;
          index = [ exact (Var (var "cat")); exact (Var (var "i")) ];
          guard = None;
        };
      ];
    check_addresses "a shift over a row lands inside the row"
      (Pointer.from_array a
      |> Pointer.row ~index:(Var (var "cat"))
      |> Pointer.shift ~offset:(Pointer.Offset.elements (Num 3)))
      [ Var (var "i") ]
      [
        {
          array = a;
          index =
            [ exact (Var (var "cat")); exact (plus (Num 3) (Var (var "i"))) ];
          guard = None;
        };
      ];
    ( "a choice reaches both arms under complementary guards",
      `Quick,
      fun () ->
        let b = var "B" in
        let cond = NRel (N_rel.Eq, Var (var "c"), Num 0) in
        let got =
          Pointer.addresses ~index:[ Num 0 ]
            (Pointer.select ~cond ~if_true:(Pointer.from_array a)
               ~if_false:(Pointer.from_array b))
        in
        Alcotest.check addresses_testable "arms"
          [
            { array = a; index = [ exact (Num 0) ]; guard = Some cond };
            {
              array = b;
              index = [ exact (Num 0) ];
              guard = Some (b_not cond);
            };
          ]
          got );
  ]

(* A shift whose units match the memory it lands on leaves the payload of a
   write meaningful; one that changes units does not, since the bits a store
   writes depend on the width it was written at. *)
let payload_tests =
  [
    ( "an unscaled shift keeps the payload",
      `Quick,
      fun () ->
        Alcotest.(check bool)
          "keeps" true
          (Pointer.keeps_payload
             (Pointer.from_array a
             |> Pointer.shift
                  ~offset:
                    (Pointer.Offset.bytes ~amount:(Num 0)
                       ~step:(Pointer.Step.make ~view:4 ~elem:4)))) );
    ( "a scaled shift drops the payload",
      `Quick,
      fun () ->
        Alcotest.(check bool)
          "drops" false
          (Pointer.keeps_payload
             (Pointer.from_array a
             |> Pointer.shift
                  ~offset:
                    (Pointer.Offset.bytes ~amount:(Num 0)
                       ~step:(Pointer.Step.make ~view:1 ~elem:4)))) );
    ( "a bare array keeps the payload",
      `Quick,
      fun () ->
        Alcotest.(check bool)
          "keeps" true
          (Pointer.keeps_payload (Pointer.from_array a)) );
  ]

let () =
  Alcotest.run "pointer"
    [
      ("rescale", rescale_tests);
      ("addresses", address_tests);
      ("payload", payload_tests);
    ]
