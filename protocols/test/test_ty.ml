open Protocols

let parse (s : string) : Ty.t = Ty.of_c_string s

let show_scalar (x : Ty.t) : string =
  x |> Ty.to_scalar |> Option.map Scalar.to_string |> Option.value ~default:"-"

let show_int_dom (x : Ty.t) : string =
  x |> Ty.to_int_dom |> Option.map Int_dom.to_string
  |> Option.value ~default:"-"

let check_scalar (given : string) (expected : string) =
  Alcotest.(check string)
    ("to_scalar " ^ given) expected
    (show_scalar (parse given))

let check_int_dom (given : string) (expected : string) =
  Alcotest.(check string)
    ("to_int_dom " ^ given) expected
    (show_int_dom (parse given))

let check_sizeof (given : string) (expected : int option) =
  Alcotest.(check (option int))
    ("sizeof " ^ given) expected
    (Ty.sizeof (parse given))

let check_bool (name : string) (f : Ty.t -> bool) (given : string)
    (expected : bool) =
  Alcotest.(check bool) (name ^ " " ^ given) expected (f (parse given))

let check_bounds (given : string)
    (expected : (int option * int option) option) =
  Alcotest.(check (option (pair (option int) (option int))))
    ("to_bounds " ^ given) expected
    (Ty.to_bounds (parse given)
    |> Option.map (fun (b : Bounds.t) -> (b.lower, b.upper)))

let check_lanes (given : string) (expected : string list option) =
  Alcotest.(check (option (list string)))
    ("vector_lanes " ^ given) expected
    (Ty.vector_lanes (parse given))

let check_array_length (given : string) (expected : int list) =
  Alcotest.(check (list int))
    ("get_array_length " ^ given) expected
    (Ty.get_array_length (parse given))

let check_array_type (given : string) (expected : string list) =
  Alcotest.(check (list string))
    ("get_array_type " ^ given) expected
    (Ty.get_array_type (parse given))

let check_to_string (given : string) (expected : string) =
  Alcotest.(check string)
    ("to_string " ^ given) expected
    (Ty.to_string (parse given))

(* ------------------------------- scalars ------------------------------ *)

let test_scalars () : unit =
  List.iter
    (fun (given, expected) -> check_scalar given expected)
    [
      ("char", "char");
      ("signed char", "char");
      ("unsigned char", "unsigned char");
      ("short", "short");
      ("unsigned short", "unsigned short");
      ("int", "int");
      ("signed int", "int");
      ("unsigned int", "unsigned int");
      ("long", "long");
      ("unsigned long", "unsigned long");
      ("long long", "long");
      ("unsigned long long", "unsigned long");
      ("float", "float");
      ("double", "double");
      ("bool", "bool");
      ("void", "-");
    ]

let test_fixed_width_typedefs () : unit =
  List.iter
    (fun (given, expected) -> check_scalar given expected)
    [
      ("int8_t", "char");
      ("uint8_t", "unsigned char");
      ("int16_t", "short");
      ("uint16_t", "unsigned short");
      ("int32_t", "int");
      ("uint32_t", "unsigned int");
      ("int64_t", "long");
      ("uint64_t", "unsigned long");
      ("size_t", "unsigned long");
      ("uchar", "unsigned char");
      ("ushort", "unsigned short");
      ("uint", "unsigned int");
      ("ulong", "unsigned long");
    ]

(* [to_int_dom] used to hold the typo ["signed shot"], so a [signed short]
   declaration was not recognised as an integer. *)
let test_signed_short_is_an_integer () : unit =
  check_int_dom "signed short" "short";
  check_bool "is_int" Ty.is_int "signed short" true

(* [sizeof]'s filter [fun x -> x <> "const" || x <> "unsigned"] was a
   tautology, so every unsigned spelling declined. *)
let test_sizeof_unsigned () : unit =
  List.iter
    (fun (given, expected) -> check_sizeof given expected)
    [
      ("unsigned char", Some 1);
      ("unsigned short", Some 2);
      ("unsigned int", Some 4);
      ("unsigned long", Some 8);
      ("unsigned long long", Some 8);
    ]

let test_sizeof () : unit =
  List.iter
    (fun (given, expected) -> check_sizeof given expected)
    [
      ("char", Some 1);
      ("short", Some 2);
      ("int", Some 4);
      ("long", Some 8);
      ("float", Some 4);
      ("double", Some 8);
      ("float *", Some 8);
      ("void **", Some 8);
      ("int[8]", None);
      ("long double", None);
      ("T", None);
    ]

let test_bool_has_no_integer_domain () : unit =
  check_int_dom "bool" "-";
  check_bounds "bool" (Some (Some 0, Some 1));
  check_bool "is_int" Ty.is_int "bool" true;
  check_bool "is_int" Ty.is_int "float" false

(* Neither 64-bit end is an OCaml [int], so a 64-bit type states only the
   end it can: nothing at all when signed, non-negativity when unsigned. *)
let test_64_bit_bounds () : unit =
  List.iter
    (fun (given, expected) -> check_bounds given expected)
    [
      ("int", Some (Some (-2147483648), Some 2147483647));
      ("unsigned int", Some (Some 0, Some 4294967295));
      ("long", Some (None, None));
      ("long long", Some (None, None));
      ("int64_t", Some (None, None));
      ("unsigned long", Some (Some 0, None));
      ("size_t", Some (Some 0, None));
      ("float", None);
      ("T", None);
    ]

(* [is_int] decides whether a declaration is modelled, so it must not
   follow whether a bound can be written for the type. *)
let test_64_bit_is_an_integer () : unit =
  List.iter
    (fun given -> check_bool "is_int" Ty.is_int given true)
    [ "long"; "long long"; "int64_t"; "unsigned long"; "size_t"; "uint64_t" ]

(* Literal containment is exactly answerable, because the literal is itself
   an OCaml [int]: every one of them fits a signed 64-bit type, and the
   non-negative ones fit the unsigned domain. *)
let test_64_bit_contains () : unit =
  List.iter
    (fun (n, ty, expected) ->
      Alcotest.(check bool)
        (string_of_int n ^ " in " ^ Scalar.to_string ty)
        expected (Scalar.contains n ty))
    [
      (Int.max_int, Scalar.long, true);
      (Int.min_int, Scalar.long, true);
      (4294967296, Scalar.long, true);
      (-1, Scalar.long, true);
      (Int.max_int, Scalar.unsigned_long, true);
      (0, Scalar.unsigned_long, true);
      (-1, Scalar.unsigned_long, false);
      (Int.min_int, Scalar.unsigned_long, false);
      (4294967296, Scalar.int, false);
    ]

(* Converting a literal is answerable wherever the result is an OCaml [int].
   Targets of one width share a residue class and differ only in which
   representative they name, so a value out of a signed target's range comes
   back negative where the unsigned target of that width keeps it. Neither
   64-bit modulus can be divided by, so both sizes answer without one, and
   the single case with no [int] to name is a negative value against an
   unsigned 64-bit target. *)
let test_reduce () : unit =
  List.iter
    (fun (n, ty, expected) ->
      Alcotest.(check (option int))
        (string_of_int n ^ " to " ^ Scalar.to_string ty)
        expected (Scalar.reduce n ty))
    [
      (100, Scalar.char, Some 100);
      (200, Scalar.char, Some (-56));
      (-56, Scalar.char, Some (-56));
      (200, Scalar.unsigned_char, Some 200);
      (456, Scalar.unsigned_char, Some 200);
      (-56, Scalar.unsigned_char, Some 200);
      (70000, Scalar.short, Some 4464);
      (-56, Scalar.unsigned_short, Some 65480);
      (-1, Scalar.unsigned_int, Some 4294967295);
      (4294967296, Scalar.int, Some 0);
      (Int.max_int, Scalar.long, Some Int.max_int);
      (Int.min_int, Scalar.long, Some Int.min_int);
      (Int.max_int, Scalar.unsigned_long, Some Int.max_int);
      (0, Scalar.unsigned_long, Some 0);
      (-1, Scalar.unsigned_long, None);
      (200, Scalar.float, None);
      (200, Scalar.bool, None);
    ]

let test_signedness () : unit =
  List.iter
    (fun (given, expected) -> check_bool "is_unsigned" Ty.is_unsigned given expected)
    [
      ("int", false);
      ("unsigned int", true);
      ("size_t", true);
      ("long long", false);
      ("unsigned long long", true);
      ("float", false);
      ("T", false);
    ]

(* ------------------------------- arrays ------------------------------- *)

let test_arrays () : unit =
  List.iter
    (fun given -> check_bool "is_array" Ty.is_array given true)
    [ "int[3]"; "int [128]"; "float[1024]"; "int[8][8]"; "T[N]"; "char[]" ];
  check_bool "is_array" Ty.is_array "int" false;
  check_bool "is_array" Ty.is_array "float *" false

let test_array_length () : unit =
  List.iter
    (fun (given, expected) -> check_array_length given expected)
    [
      ("int[3]", [ 3 ]);
      ("int [128]", [ 128 ]);
      ("float[1024]", [ 1024 ]);
      ("int[8][8]", [ 8; 8 ]);
      ("unsigned int [8][8] *", [ 8; 8 ]);
      (* an unsized dimension makes the whole shape unknown *)
      ("T[N]", []);
      ("char[]", []);
      ("float *", []);
      ("int", []);
    ]

let test_array_type () : unit =
  List.iter
    (fun (given, expected) -> check_array_type given expected)
    [
      ("int[3]", [ "int" ]);
      ("float[1024]", [ "float" ]);
      ("unsigned int [8][8] *", [ "unsigned"; "int" ]);
      ("const char[1]", [ "const"; "char" ]);
      ("volatile unsigned int[256]", [ "volatile"; "unsigned"; "int" ]);
      ("float *", [ "float" ]);
      ("const float *", [ "const"; "float" ]);
      ("int", []);
    ]

let test_strip_array () : unit =
  List.iter
    (fun (given, expected) ->
      Alcotest.(check string)
        ("strip_array " ^ given) expected
        (parse given |> Ty.strip_array |> Ty.to_string))
    [
      ("int[8][8]", "int");
      ("float[1024]", "float");
      ("const char[1]", "const char");
      (* a read of [float *A] needs [float] as its element type *)
      ("float *", "float");
      ("const float *", "const float");
      ("int **", "int *");
      ("int", "int");
    ]

(* ------------------------------ pointers ------------------------------ *)

let test_pointers () : unit =
  List.iter
    (fun (given, expected) -> check_bool "is_pointer" Ty.is_pointer given expected)
    [
      ("float *", true);
      ("const char *", true);
      ("void **", true);
      ("float *const", true);
      ("volatile T *const __restrict", true);
      ("int", false);
      ("int[3]", false);
      ("T &", false);
    ]

let test_qualifiers () : unit =
  List.iter
    (fun (given, expected) -> check_bool "is_const" Ty.is_const given expected)
    [
      ("const int", true);
      ("const float *", true);
      ("const float[32]", true);
      ("int", false);
      ("volatile int", false);
    ];
  Alcotest.(check string)
    "strip_const const int" "int"
    (parse "const int" |> Ty.strip_const |> Ty.to_string)

(* ------------------------------ vectors ------------------------------- *)

let test_vector_lanes () : unit =
  List.iter
    (fun (given, expected) -> check_lanes given expected)
    [
      ("char1", Some [ "x" ]);
      ("float2", Some [ "x"; "y" ]);
      ("uint3", Some [ "x"; "y"; "z" ]);
      ("int4", Some [ "x"; "y"; "z"; "w" ]);
      ("const float4", Some [ "x"; "y"; "z"; "w" ]);
      ("ulonglong2", Some [ "x"; "y" ]);
      ("int", None);
      ("int5", None);
      ("dim3", None);
      ("struct float2", None);
    ]

(* ------------------------- the desugared fallback --------------------- *)

(* Every CUDA vector spelling desugars to a struct, so the written
   spelling has to lead or [vector_lanes] would answer for none of them. *)
let test_written_spelling_leads () : unit =
  let x = Ty.of_c_string ~desugared:"struct float2" "float2" in
  Alcotest.(check (option (list string)))
    "float2 stays a vector" (Some [ "x"; "y" ]) (Ty.vector_lanes x);
  Alcotest.(check bool) "float2 is not a struct" false (Ty.is_struct x);
  Alcotest.(check string) "float2 prints as written" "float2" (Ty.to_string x)

let test_desugared_fallback () : unit =
  let time_t = Ty.of_c_string ~desugared:"long" "time_t" in
  Alcotest.(check string) "time_t resolves" "long" (show_scalar time_t);
  Alcotest.(check string)
    "time_t prints as written" "time_t" (Ty.to_string time_t);
  let iterator = Ty.of_c_string ~desugared:"T *" "iterator" in
  Alcotest.(check bool) "iterator resolves" true (Ty.is_pointer iterator);
  (* [size_t] is in the table, so it resolves without the fallback and a
     [size_t] in element position resolves too. *)
  Alcotest.(check string)
    "size_t without a desugared form" "unsigned long"
    (show_scalar (parse "size_t"));
  check_array_type "size_t[32]" [ "size_t" ];
  Alcotest.(check string)
    "size_t element resolves" "unsigned long"
    (show_scalar (parse "size_t[32]" |> Ty.strip_array))

(* The desugared spelling reaches the [Opaque] payload while [name] keeps
   the written one, which is how a typedef'd barrier is still recognised. *)
let test_opaque_carries_the_resolved_spelling () : unit =
  let x =
    Ty.of_c_string ~desugared:"cuda::barrier<cuda::thread_scope_block>"
      "barrier_t"
  in
  Alcotest.(check (option string))
    "opaque payload is the resolved spelling"
    (Some "cuda::barrier<cuda::thread_scope_block>")
    (Ty.to_opaque x);
  Alcotest.(check string) "name is the written spelling" "barrier_t"
    (Ty.to_string x)

(* ------------------------- opaque and the rest ------------------------ *)

let test_opaque () : unit =
  List.iter
    (fun given ->
      Alcotest.(check (option string))
        ("opaque " ^ given) (Some given)
        (Ty.to_opaque (parse given)))
    [
      "long double";
      "__int128";
      "unsigned __int128";
      "T";
      "T &";
      "T &&";
      "Args &&...";
      "<dependent type>";
      "std::initializer_list<T>";
      "typename remove_cv<T>::type";
      "dim3";
      "cudaError";
      "enum cudaTextureReadMode";
      "auto";
    ];
  check_bool "is_auto" Ty.is_auto "auto" true;
  check_bool "is_unknown" Ty.is_unknown "?" true;
  (* a qualifier is lifted onto the wrapper, so the payload loses it *)
  check_bool "is_const" Ty.is_const "const T &" true;
  check_to_string "const T &" "const T &";
  Alcotest.(check (option string))
    "opaque const T &" (Some "T &")
    (Ty.to_opaque (parse "const T &"))

let test_shapes () : unit =
  check_bool "is_void" Ty.is_void "void" true;
  check_bool "is_struct" Ty.is_struct "struct float2" true;
  check_bool "is_struct" Ty.is_struct "class Foo" true;
  check_bool "is_struct" Ty.is_struct "dim3" false;
  check_bool "is_function" Ty.is_function "void (*)()" true;
  check_bool "is_function" Ty.is_function "void (int)" true;
  check_bool "is_function" Ty.is_function "unsigned int (unsigned int, unsigned int)" true;
  check_bool "is_function" Ty.is_function "int" false

(* Nothing that prints a type may lose its spelling: [--show-map] shows
   [size_t] rather than [unsigned long]. *)
let test_to_string_round_trip () : unit =
  List.iter
    (fun given -> check_to_string given given)
    [
      "int";
      "unsigned int";
      "const int";
      "size_t";
      "float2";
      "float *";
      "const char *";
      "volatile T *const __restrict";
      "int[8][8]";
      "volatile unsigned int[256]";
      "long double";
      "<dependent type>";
      "int (&)[3]";
    ]

let tests : unit Alcotest.test_case list =
  [
    ("scalars", `Quick, test_scalars);
    ("fixed-width typedefs", `Quick, test_fixed_width_typedefs);
    ("signed short is an integer", `Quick, test_signed_short_is_an_integer);
    ("sizeof of unsigned types", `Quick, test_sizeof_unsigned);
    ("sizeof", `Quick, test_sizeof);
    ("bool has no integer domain", `Quick, test_bool_has_no_integer_domain);
    ("64-bit bounds", `Quick, test_64_bit_bounds);
    ("64-bit is an integer", `Quick, test_64_bit_is_an_integer);
    ("64-bit literal containment", `Quick, test_64_bit_contains);
    ("converting a literal to a type", `Quick, test_reduce);
    ("signedness", `Quick, test_signedness);
    ("arrays", `Quick, test_arrays);
    ("array length", `Quick, test_array_length);
    ("array element type", `Quick, test_array_type);
    ("strip_array", `Quick, test_strip_array);
    ("pointers", `Quick, test_pointers);
    ("qualifiers", `Quick, test_qualifiers);
    ("vector lanes", `Quick, test_vector_lanes);
    ("the written spelling leads", `Quick, test_written_spelling_leads);
    ("the desugared fallback", `Quick, test_desugared_fallback);
    ( "opaque carries the resolved spelling",
      `Quick,
      test_opaque_carries_the_resolved_spelling );
    ("opaque", `Quick, test_opaque);
    ("shapes", `Quick, test_shapes);
    ("to_string round trip", `Quick, test_to_string_round_trip);
  ]

let () = Alcotest.run "Ty" [ ("ty", tests) ]
