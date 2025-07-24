open Stage0
open Protocols

module Rule = struct
  open Environ

  type t = rule

  let make ~src ~cost ~cnd ~dst : t = { src; cost; dst; cnd }
  let src (x : t) : call = { id = x.src; args = [] }

  let to_string (env : Environ.t) (x : t) =
    let arr l = "(" ^ String.concat "," l ^ ")" in
    let b_to_s s =
      s |> Environ.b_to_string env |> Common.replace ~substring:"==" ~by:"="
    in
    let cnd = List.map b_to_s x.cnd in
    let cost = string_of_int x.cost in
    let src : string = Environ.c_to_string env (src x) in
    let cnd =
      if List.length cnd = 0 then "" else " :|: " ^ String.concat " && " cnd
    in
    let dst = List.map (Environ.c_to_string env) x.dst in
    if List.length dst = 0 then ""
    else
      Printf.sprintf "%s -{%s}> Com_%d%s %s" src cost (List.length dst)
        (arr dst) cnd
end

type inst = Rule of Rule.t | Comment of string
type prog = { env : Environ.t; code : inst list }

let rule (src : int) ?(cost = 0) ?(dst = []) ?(cnd = []) () : inst =
  Rule (Rule.make ~src ~cost ~dst ~cnd)

let jump ?(cost = 0) ~src ~dst () : inst =
  rule src ~cost ~cnd:[] ~dst:[ { id = dst; args = [] } ] ()

let cond_jump (cnd : Exp.bexp) ?(cost = 0) ~src ~dst () : inst =
  rule src ~cost ~cnd:[ cnd ] ~dst:[ { id = dst; args = [] } ] ()

let from_stmt (env : Environ.t) (s : Stmt.t) : prog =
  let open Exp in
  (* Compute a map from variable to its identifier *)
  let rec translate (idx : int) : Stmt.t -> int * inst list = function
    | Skip ->
        (idx + 1, [ rule idx ~cost:0 ~dst:[ { id = idx + 1; args = [] } ] () ])
    | Clamp { value; upper_bound } ->
        translate idx (Stmt.clamp_to_tick ~value ~upper_bound)
    | Tick k ->
        (idx + 1, [ rule idx ~cost:k ~dst:[ { id = idx + 1; args = [] } ] () ])
    | Choice _ -> failwith "choice"
    | Seq (p, q) ->
        let idx, l1 = translate idx p in
        let idx, l2 = translate idx q in
        (idx, l1 @ l2)
    | If (c, p, q) ->
        (*

        idx -> idx + 1 -> .... -> idx_l
          \--------------------> idx_l + 1 -> ... -> idx_r
       *)
        let idx_l, l1 = translate (idx + 1) p in
        let idx_r, l2 = translate (idx_l + 1) q in
        ( idx_r + 1,
          [
            cond_jump c ~src:idx ~dst:(idx + 1) ();
            cond_jump (Exp.b_not c) ~src:idx ~dst:(idx_l + 1) ();
            jump ~src:idx_l ~dst:(idx_r + 1) ();
            jump ~src:idx_r ~dst:(idx_r + 1) ();
          ]
          @ l1 @ l2 )
    | Loop { range = r; body = s } ->
        let idx', rest = translate (idx + 2) s in
        let x = r.var in
        ( idx' + 1,
          [
            (* Initialize the loop *)
            rule idx
              ~dst:[ { id = idx + 1; args = [ (x, Range.while_init r) ] } ]
              ();
            (* Transition of next iteration *)
            rule (idx + 1)
              ~dst:
                [
                  { id = idx + 2; args = [ (x, Range.while_inc r) ] };
                  (* next iter *)
                ]
              ~cnd:[ Range.while_cond r ]
              ();
          ]
          @ rest
          @ [
              rule idx' ~dst:[ { id = idx + 1; args = [] } ] ();
              (* Transition of end of loop: *)
              rule (idx + 1)
                ~cnd:[ Exp.b_not (Range.while_cond r) |> Constfold.b_opt ]
                ~dst:[ { id = idx' + 1; args = [] } ]
                ();
            ] )
  in
  let idx, l = translate 0 s in
  let code = l @ [ rule idx () ] in
  { env; code }

let to_string (p : prog) : string =
  let rules =
    p.code
    |> List.map (function
         | Comment c -> "# " ^ c
         | Rule r -> Rule.to_string p.env r)
    |> String.concat "\n"
  in
  "(GOAL COMPLEXITY)\n" ^ "(STARTTERM (FUNCTIONSYMBOLS inst_0))\n" ^ "(VAR "
  ^ String.concat " " (Array.to_list p.env.data)
  ^ ")\n" ^ "(RULES\n" ^ rules ^ "\n)"

let r_id = Str.regexp {|nat(\([A-Za-z_0-9-]+\))|}

let parse_asympt (env : Environ.t) (x : string) : string option =
  let ( let* ) = Option.bind in
  let* _, x = Common.split '{' x in
  let* x, _ = Common.split '}' x in
  let x = Environ.decode (String.trim x) env |> Str.global_replace r_id "\\1" in
  Some x

let parse_cost (env : Environ.t) (x : string) : string option =
  let ( let* ) = Option.bind in
  let* x, _ = Common.split '{' x in
  let x = Environ.decode (String.trim x) env |> Str.global_replace r_id "\\1" in
  Some x

let run_exe ?(asympt = false) ?(exe = "koat2") ?(verbose = false)
    (env : Environ.t) (expr : string) : (string, Errors.t) Result.t =
  if verbose then prerr_endline ("KoAT output:\n" ^ expr ^ "\n") else ();
  let parse = if asympt then parse_asympt else parse_cost in
  Subprocess.make exe [ "analyse"; "-i"; "/dev/stdin" ]
  |> Subprocess.run_combine ~stdin:expr
  |> Errors.handle_result (parse env)

let run ?(asympt = false) ?(exe = "koat2") ?(verbose = false) (s : Stmt.t) :
    (string, Errors.t) Result.t =
  let env = Stmt.to_environ s in
  let expr = from_stmt env s |> to_string in
  run_exe ~asympt ~exe ~verbose env expr
