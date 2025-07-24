open Inference
open OUnit2
open W_lang

let vec ?(scalar = Scalar.i32) (size : int) (components : Expression.t list) :
    Expression.t =
  let ty = Type.vec size scalar in
  Compose { ty; components }

let splat (size : int) (e : Expression.t) : Expression.t =
  Splat { size = VectorSize.from_int size |> Option.get; value = e }

let i32 (i : int) : Expression.t = Expression.i32 i

let _e_equal ~expected:(e1 : Expression.t) ~given:(e2 : Expression.t) : unit =
  assert_equal e1 e2 ~printer:Expression.to_string

let simplify ~expected ~given : unit =
  assert_equal expected
    (Expression.simplify given)
    ~printer:Expression.to_string
    ~msg:(Printf.sprintf "simplify(%s)" (Expression.to_string given))

let suite =
  "WGSL tests"
  >::: [
         ( "compose-vec2" >:: fun _ ->
           simplify ~given:(vec 2 []) ~expected:(vec 2 [ i32 0; i32 0 ]);
           simplify
             ~given:(vec 2 [ i32 10 ])
             ~expected:(vec 2 [ i32 10; i32 10 ]);
           simplify
             ~given:(vec 2 [ i32 1; i32 2 ])
             ~expected:(vec 2 [ i32 1; i32 2 ]);
           simplify
             ~given:(vec 2 [ vec 2 [ i32 1; i32 2 ] ])
             ~expected:(vec 2 [ i32 1; i32 2 ]) );
         ( "compose-vec3" >:: fun _ ->
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
             ~expected:(vec 3 [ i32 1; i32 2; i32 3 ]) );
         ( "compose-vec4" >:: fun _ ->
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
             ~expected:(vec 4 [ i32 1; i32 2; i32 3; i32 4 ]) );
         ( "splat-vec" >:: fun _ ->
           simplify ~given:(splat 2 (i32 1)) ~expected:(vec 2 [ i32 1; i32 1 ]);
           simplify
             ~given:(splat 3 (i32 1))
             ~expected:(vec 3 [ i32 1; i32 1; i32 1 ]);
           simplify
             ~given:(splat 4 (i32 1))
             ~expected:(vec 4 [ i32 1; i32 1; i32 1; i32 1 ]) );
       ]

let () = run_test_tt_main suite
