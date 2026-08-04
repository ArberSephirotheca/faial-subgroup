open Protocols
open Imp

let atom : Ty.t = Ty.make ~name:"Atom" (Ty.Opaque "Atom")
let cell : Ty.t = Ty.make ~name:"Cell" (Ty.Opaque "Cell")
let vee : Ty.t = Ty.make ~name:"V" (Ty.Opaque "V")
let inner : Ty.t = Ty.make ~name:"Inner" (Ty.Opaque "Inner")
let outer : Ty.t = Ty.make ~name:"Outer" (Ty.Opaque "Outer")

let array_of ?size (base : Ty.t) : Ty.t = Ty.make (Ty.Array { base; size })
let ptr_to (base : Ty.t) : Ty.t = Ty.make (Ty.Pointer base)

let double : Ty.t = Ty.make (Ty.Scalar Scalar.double)

module Records = struct
  let table : (string * (string * Ty.t) list) list =
    [
      ("Atom", [ ("f", array_of ~size:3 double) ]);
      ("Cell", [ ("key", Ty.int); ("val", Ty.int) ]);
      ("V", [ ("p", ptr_to Ty.int) ]);
      ("Inner", [ ("a", array_of ~size:4 double); ("b", double) ]);
      ("Outer", [ ("x", array_of ~size:8 inner); ("y", double) ]);
    ]

  let members (ty : Ty.t) : (string * Ty.t) list option =
    match ty.inner with
    | Ty.Opaque name -> List.assoc_opt name table
    | _ -> None
end

module T = Type_tree.Make (Records)

let var (name : string) : Variable.t = Variable.from_name name

let leaves (t : Type_tree.t) : string list =
  t.leaves |> List.map Type_tree.Leaf.to_string

let check_leaves (name : string) (expected : string list) (t : Type_tree.t) =
  ( name,
    `Quick,
    fun () -> Alcotest.(check (list string)) name expected (leaves t) )

let parameter (name : string) (ty : Ty.t) : Type_tree.t =
  T.of_parameter ~root:(var name) ty

let descent_tests =
  [
    check_leaves "a plain pointer parameter is one leaf, outermost unknown"
      [ "A : int [?]" ]
      (parameter "A" (ptr_to Ty.int));
    check_leaves "a pointer to array keeps the declared inner extent"
      [ "A : int [?, 4]" ]
      (parameter "A" (ptr_to (array_of ~size:4 Ty.int)));
    check_leaves "an array of structs names the member, not the object"
      [ "s.f : double [?, 3]" ]
      (parameter "s" (ptr_to atom));
    check_leaves "a scalar member of memory is an array of its own"
      [ "C.key : int [?]"; "C.val : int [?]" ]
      (parameter "C" (ptr_to cell));
    check_leaves "structs of arrays of structs flatten to one leaf per scalar"
      [ "s.x.a : double [?, 8, 4]"; "s.x.b : double [?, 8]"; "s.y : double [?]" ]
      (parameter "s" (ptr_to outer));
    check_leaves "a by-value struct promotes no inline member"
      [] (parameter "b" atom);
  ]

let pointer_member_tests =
  [
    check_leaves
      "a pointer member is its storage and the region its address names"
      [ "s.p : int * [?]"; "*s.p : int [?, ?]" ]
      (parameter "s" (ptr_to vee));
    check_leaves "the same pair for a by-value struct"
      [ "v.p : int * []"; "*v.p : int [?, ?]" ]
      (parameter "v" vee);
  ]

let declaration_tests =
  [
    check_leaves "a shared struct declaration has no leading dimension"
      [ "s.f : double [3]" ]
      (T.of_declaration ~root:(var "s") atom);
    check_leaves "a shared array of structs carries its own extent"
      [ "s.f : double [16, 3]" ]
      (T.of_declaration ~root:(var "s") (array_of ~size:16 atom));
  ]

let all_tests =
  [
    ("descent", descent_tests);
    ("pointer members", pointer_member_tests);
    ("declarations", declaration_tests);
  ]

let () = Alcotest.run "Type_tree" all_tests
