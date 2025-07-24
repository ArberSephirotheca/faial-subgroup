open Inference
open W_lang

let vec ?(scalar = Scalar.i32) (size : int) (components : Expression.t list) :
    Expression.t =
  let ty = Type.vec size scalar in
  Compose { ty; components }

let splat (size : int) (e : Expression.t) : Expression.t =
  Splat { size = VectorSize.from_int size |> Option.get; value = e }

let i32 (i : int) : Expression.t = Expression.i32 i

let expression : Expression.t Alcotest.testable =
  let pp fmt e = Format.fprintf fmt "%s" (Expression.to_string e) in
  let equal = (=) in
  Alcotest.testable pp equal

let simplify ~expected ~given : unit =
  let msg = Printf.sprintf "simplify(%s)" (Expression.to_string given) in
  Alcotest.check expression msg expected (Expression.simplify given)

let test_compose_vec2 () : unit =
  simplify ~given:(vec 2 []) ~expected:(vec 2 [ i32 0; i32 0 ]);
  simplify
    ~given:(vec 2 [ i32 10 ])
    ~expected:(vec 2 [ i32 10; i32 10 ]);
  simplify
    ~given:(vec 2 [ i32 1; i32 2 ])
    ~expected:(vec 2 [ i32 1; i32 2 ]);
  simplify
    ~given:(vec 2 [ vec 2 [ i32 1; i32 2 ] ])
    ~expected:(vec 2 [ i32 1; i32 2 ])

let test_compose_vec3 () : unit =
  simplify ~given:(vec 3 []) ~expected:(vec 3 [ i32 0; i32 0; i32 0 ]);
  simplify
    ~given:(vec 3 [ i32 10 ])
    ~expected:(vec 3 [ i32 10; i32 10; i32 10 ]);
  simplify
    ~given:(vec 3 [ i32 1; vec 2 [ i32 2; i32 3 ] ])
    ~expected:(vec 3 [ i32 1; i32 2; i32 3 ]);
  simplify
    ~given:(vec 3 [ vec 2 [ i32 1; i32 2 ]; i32 3 ])
    ~expected:(vec 3 [ i32 1; i32 2; i32 3 ]);
  simplify
    ~given:(vec 3 [ i32 1; i32 2; i32 3 ])
    ~expected:(vec 3 [ i32 1; i32 2; i32 3 ])

let test_compose_vec4 () : unit =
  (* zero *)
  simplify ~given:(vec 4 [])
    ~expected:(vec 4 [ i32 0; i32 0; i32 0; i32 0 ]);
  (* component-wise *)
  simplify
    ~given:(vec 4 [ i32 10 ])
    ~expected:(vec 4 [ i32 10; i32 10; i32 10; i32 10 ]);
  (* 4 scalars *)
  simplify
    ~given:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]);
  (* 1 vec2 + 2 scalars *)
  simplify
    ~given:(vec 4 [ vec 2 [ i32 1; i32 2 ]; i32 3; i32 4 ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]);
  simplify
    ~given:(vec 4 [ i32 1; vec 2 [ i32 2; i32 3 ]; i32 4 ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]);
  simplify
    ~given:(vec 4 [ i32 1; i32 2; vec 2 [ i32 3; i32 4 ] ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]);
  (* 2 vec2 *)
  simplify
    ~given:(vec 4 [ vec 2 [ i32 1; i32 2 ]; vec 2 [ i32 3; i32 4 ] ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]);
  (* 1 scalar + 1 vec3 *)
  simplify
    ~given:(vec 4 [ i32 1; vec 3 [ i32 2; i32 3; i32 4 ] ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]);
  simplify
    ~given:(vec 4 [ vec 3 [ i32 1; i32 2; i32 3 ]; i32 4 ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]);
  (* 1 vec4 *)
  simplify
    ~given:(vec 4 [ vec 4 [ i32 1; i32 2; i32 3; i32 4 ] ])
    ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ])

let test_splat_vec () : unit =
  simplify ~given:(splat 2 (i32 1)) ~expected:(vec 2 [ i32 1; i32 1 ]);
  simplify
    ~given:(splat 3 (i32 1))
    ~expected:(vec 3 [ i32 1; i32 1; i32 1 ]);
  simplify
    ~given:(splat 4 (i32 1))
    ~expected:(vec 4 [ i32 1; i32 1; i32 1; i32 1 ])

let tests : unit Alcotest.test_case list =
  [
    ("compose-vec2", `Quick, test_compose_vec2);
    ("compose-vec3", `Quick, test_compose_vec3);
    ("compose-vec4", `Quick, test_compose_vec4);
    ("splat-vec", `Quick, test_splat_vec);
  ]

let () = Alcotest.run "WGSL" [ ("tests", tests) ]
