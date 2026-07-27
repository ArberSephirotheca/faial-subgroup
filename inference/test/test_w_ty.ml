open Inference
open W_lang

let show_scalar (x : Protocols.Ty.t) : string =
  x |> Protocols.Ty.to_scalar
  |> Option.map Protocols.Scalar.to_string
  |> Option.value ~default:"-"

let check_scalar (name : string) (expected : string) (given : Type.t) =
  Alcotest.(check string) name expected (show_scalar (Type.to_ty given))

(* naga gives a width in bytes. *)
let test_widths () : unit =
  check_scalar "u32" "unsigned int" Type.u32;
  check_scalar "i32" "int" Type.i32;
  check_scalar "u64" "unsigned long" Type.u64;
  check_scalar "f32" "float" Type.f32;
  check_scalar "f64" "double" Type.f64;
  check_scalar "i8" "char" (Type.scalar { kind = Sint; width = 1 });
  check_scalar "u16" "unsigned short" (Type.scalar { kind = Uint; width = 2 })

(* The abstract literal types are un-materialized; WGSL gives them 64 bits. *)
let test_abstract_literals () : unit =
  check_scalar "AbstractInt" "long" (Type.scalar Scalar.int);
  check_scalar "AbstractFloat" "double" (Type.scalar Scalar.float)

let test_bool () : unit =
  let x = Type.to_ty Type.bool in
  Alcotest.(check string) "bool is not an integer domain" "-"
    (x |> Protocols.Ty.to_int_dom
    |> Option.map Protocols.Int_dom.to_string
    |> Option.value ~default:"-");
  Alcotest.(check (option (pair (option int) (option int))))
    "bool ranges over 0..1"
    (Some (Some 0, Some 1))
    (Protocols.Ty.to_bounds x
    |> Option.map (fun (b : Protocols.Bounds.t) -> (b.lower, b.upper)))

(* A width naga could emit but faial has no size for. *)
let test_unrepresentable_width_is_opaque () : unit =
  let x = Type.to_ty (Type.scalar { kind = Sint; width = 3 }) in
  Alcotest.(check bool) "opaque" true
    (Protocols.Ty.to_opaque x |> Option.is_some)

let test_vectors () : unit =
  let x = Type.to_ty Type.vec4_u32 in
  Alcotest.(check (option (list string)))
    "vec4 lanes"
    (Some [ "x"; "y"; "z"; "w" ])
    (Protocols.Ty.vector_lanes x);
  let y = Type.to_ty (Type.vec 3 Scalar.u32) in
  Alcotest.(check (option (list string)))
    "vec3 lanes"
    (Some [ "x"; "y"; "z" ])
    (Protocols.Ty.vector_lanes y)

let test_arrays () : unit =
  let sized = Type.to_ty (Type.array ~size:(ArraySize.Constant 32) Type.f32) in
  Alcotest.(check bool) "is_array" true (Protocols.Ty.is_array sized);
  Alcotest.(check (list int))
    "array length" [ 32 ]
    (Protocols.Ty.get_array_length sized);
  Alcotest.(check string)
    "element type" "float"
    (show_scalar (Protocols.Ty.strip_array sized));
  let dynamic = Type.to_ty (Type.array Type.f32) in
  Alcotest.(check (list int))
    "a runtime-sized array has no length" []
    (Protocols.Ty.get_array_length dynamic)

(* An image is not modelled, but it keeps its printed spelling. *)
let test_images_are_opaque () : unit =
  let image : Type.t =
    Type.make
      (Image
         {
           dim = D2;
           arrayed = false;
           image_class = Sampled { kind = Float; multi = false };
         })
  in
  let x = Type.to_ty image in
  Alcotest.(check (option string))
    "image keeps its spelling"
    (Some (Type.to_string image))
    (Protocols.Ty.to_opaque x)

(* The spelling faial has always shown for a WGSL integer scalar. *)
let test_display_spelling () : unit =
  List.iter
    (fun (given, expected) ->
      Alcotest.(check string)
        expected expected
        (Protocols.Ty.to_string (Type.to_ty given)))
    [
      (Type.u32, "uint32_t");
      (Type.i32, "int32_t");
      (Type.u64, "uint64_t");
      (Type.f32, "f32");
      (Type.bool, "bool");
    ]

let tests : unit Alcotest.test_case list =
  [
    ("scalar widths", `Quick, test_widths);
    ("abstract literals", `Quick, test_abstract_literals);
    ("bool", `Quick, test_bool);
    ("an unrepresentable width is opaque", `Quick,
     test_unrepresentable_width_is_opaque);
    ("vectors", `Quick, test_vectors);
    ("arrays", `Quick, test_arrays);
    ("images are opaque", `Quick, test_images_are_opaque);
    ("display spelling", `Quick, test_display_spelling);
  ]

let () = Alcotest.run "W_lang.Type.to_ty" [ ("to_ty", tests) ]
