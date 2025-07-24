open Stage0
open Protocols

let from_stmt : Stmt.t -> string =
  let indent (depth : int) : string = String.make depth '\t' in
  let rec translate (depth : int) : Stmt.t -> string = function
    | Skip -> indent depth ^ "tick 0\n"
    | Tick k -> indent depth ^ "tick " ^ string_of_int k ^ "\n"
    | Clamp { value; upper_bound } ->
        translate depth (Stmt.clamp_to_tick ~value ~upper_bound)
    | Choice (p, q) ->
        indent depth ^ "if random:\n"
        ^ translate (depth + 1) p
        ^ indent depth ^ "else:\n"
        ^ translate (depth + 1) q
    | If (b, p, q) ->
        indent depth ^ "if " ^ Exp.b_to_string b ^ ":\n"
        ^ translate (depth + 1) p
        ^ indent depth ^ "else:\n"
        ^ translate (depth + 1) q
    | Loop { range = r; body = s } ->
        let r = { r with dir = Range.Increase } in
        indent depth ^ Variable.name r.var ^ " = "
        ^ Exp.n_to_string (Range.while_init r)
        ^ "\n" ^ indent depth ^ "while "
        ^ Exp.b_to_string (Range.while_cond r)
        ^ ":\n"
        ^ translate (depth + 1) s
        ^ indent (depth + 1)
        ^ Variable.name r.var ^ " = "
        ^ Exp.n_to_string (Range.while_inc r)
        ^ "\n"
    | Seq (p, q) -> translate depth p ^ translate depth q
  in
  fun x -> "def f():\n" ^ translate 1 x

let with_tmp ~prefix ~suffix (f : string -> 'a) : 'a =
  let fname = Filename.temp_file prefix suffix in
  try
    let res = f fname in
    Sys.remove fname;
    res
  with ex ->
    Sys.remove fname;
    raise ex

let write_string ~filename ~data : unit =
  let oc = open_out filename in
  try
    output_string oc data;
    close_out oc
  with ex ->
    close_out oc;
    raise ex

let parse_absynth (x : string) : string option =
  let ( let* ) = Option.bind in
  let* x =
    String.split_on_char '\n' x
    |> List.find_opt (String.starts_with ~prefix:"    Bound:")
  in
  let* _, x = Common.split ':' x in
  Some (String.trim x)

let run_exe ?(asympt = false) ?(verbose = false) ?(exe = "absynth")
    (data : string) : (string, Errors.t) Result.t =
  if verbose then prerr_endline ("Absynth output:\n" ^ data ^ "\n") else ();
  let args = if asympt then [ "-asympt" ] else [] in
  with_tmp ~prefix:"absynth_" ~suffix:".imp" (fun filename ->
      write_string ~filename ~data;
      Subprocess.make exe (args @ [ filename ]) |> Subprocess.run_combine)
  |> Errors.handle_result parse_absynth

let run ?(asympt = false) ?(verbose = false) ?(exe = "absynth") (x : Stmt.t) :
    (string, Errors.t) Result.t =
  run_exe ~asympt ~verbose ~exe (from_stmt x)
