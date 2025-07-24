open Inference
open D_lang

let stmt : Stmt.t Alcotest.testable =
  let pp fmt stmt = Format.fprintf fmt "%s" (Stmt.to_string stmt) in
  let equal = (=) in
  Alcotest.testable pp equal

let test_last_and_skip_last () : unit =
  let open Stmt in
  let s = Stmt.from_list [ BreakStmt; GotoStmt; ContinueStmt ] in
  Alcotest.check stmt "last returns ContinueStmt" ContinueStmt (last s);
  Alcotest.check stmt "skip_last returns correct list" 
    (Stmt.from_list [ BreakStmt; GotoStmt ]) (skip_last s)

let tests : unit Alcotest.test_case list =
  [ ("last + skip_last", `Quick, test_last_and_skip_last) ]

let () = Alcotest.run "D_lang" [ ("dlang", tests) ]
