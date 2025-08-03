module type CloseProcess = sig
  type t

  val close_process : t -> Unix.process_status
end

module WithProcess (C : CloseProcess) = struct
  let with_process (handle : C.t -> 'a) (ch : C.t) : Unix.process_status * 'a =
    let res =
      try handle ch
      with exc ->
        let _ = C.close_process ch in
        raise exc
    in
    (C.close_process ch, res)
end

module WithProcessIn = WithProcess (struct
  type t = in_channel

  let close_process = Unix.close_process_in
end)

let with_process_in :
    (in_channel -> 'a) -> in_channel -> Unix.process_status * 'a =
  WithProcessIn.with_process

module WithProcessOut = WithProcess (struct
  type t = out_channel

  let close_process = Unix.close_process_out
end)

let with_process_out :
    (out_channel -> 'a) -> out_channel -> Unix.process_status * 'a =
  WithProcessOut.with_process

module WithProcessInOut = WithProcess (struct
  type t = in_channel * out_channel

  let close_process = Unix.close_process
end)

let with_process_in_out :
    (in_channel * out_channel -> 'a) ->
    in_channel * out_channel ->
    Unix.process_status * 'a =
  WithProcessInOut.with_process

module WithProcessSplit = WithProcess (struct
  type t = in_channel * out_channel * in_channel

  let close_process = Unix.close_process_full
end)

let with_process_split :
    (in_channel * out_channel * in_channel -> 'a) ->
    in_channel * out_channel * in_channel ->
    Unix.process_status * 'a =
  WithProcessSplit.with_process

let ic_to_string ?(chunk_size = 1024) (ic : in_channel) : string =
  let buffer = Buffer.create chunk_size in
  let rec loop () =
    try
      Buffer.add_channel buffer ic chunk_size;
      loop ()
    with End_of_file -> Buffer.contents buffer
  in
  loop ()

let stdout_stderr_to_string ?(stdin = "") ((ic, oc) : in_channel * out_channel)
    : string =
  (* Send the expression to be processed *)
  output_string oc stdin;
  (* Close output to ensure it is processed *)
  close_out oc;
  (* Receive the output *)
  ic_to_string ic

let split_to_string ?(stdin = "")
    ((stdout, oc, stderr) : in_channel * out_channel * in_channel) :
    string * string =
  (* Send the expression to be processed *)
  output_string oc stdin;
  (* Close output to ensure it is processed *)
  close_out oc;
  (* Receive the output *)
  (ic_to_string stdout, ic_to_string stderr)

let neither_to_string ?(stdin = "") (oc : out_channel) : unit =
  (* Send the expression to be processed *)
  output_string oc stdin;
  (* Close output to ensure it is processed *)
  close_out oc

let exit_code : Unix.process_status -> int = function
  | WEXITED n | WSIGNALED n | WSTOPPED n -> n

let process_status_to_string : Unix.process_status -> string = function
  | WEXITED n -> "Process exited with return code: " ^ string_of_int n
  | WSIGNALED n -> "Process was killed by a signal: " ^ string_of_int n
  | WSTOPPED n -> "Process was stopped by a signal: " ^ string_of_int n

type t = { exe : string; args : string list }

let make (exe : string) (args : string list) : t = { exe; args }
let quote (c : t) : string = Filename.quote_command c.exe c.args
let to_string (c : t) : string = quote c

module Completed2 = struct
  type t = { status : Unix.process_status; stdout : string; stderr : string }

  let make ?(stdout = "") ?(stderr = "") (status : Unix.process_status) : t =
    { stdout; stderr; status }

  let is_ok (c : t) : bool = c.status = Unix.WEXITED 0

  let check_stdout (c : t) : string option =
    if is_ok c then Some c.stdout else None

  let check_stderr (c : t) : string option =
    if is_ok c then Some c.stderr else None

  let to_string (x : t) : string = process_status_to_string x.status
end

module Completed1 = struct
  type t = { status : Unix.process_status; output : string }

  let make ?(output = "") (status : Unix.process_status) : t =
    { output; status }

  let is_ok (c : t) : bool = c.status = Unix.WEXITED 0

  let check_output (c : t) : string option =
    if is_ok c then Some c.output else None

  let to_string (x : t) : string = process_status_to_string x.status
end

(* Run a process while capturing the stdout and stderr *)
let run_split ?(stdin = "") (c : t) : Completed2.t =
  c |> quote
  |> (fun cmd -> Unix.open_process_full cmd (Unix.environment ()))
  |> with_process_split (split_to_string ~stdin)
  |> fun (s, (o, e)) -> Completed2.make ~stdout:o ~stderr:e s

(* Run a process while capturing the stdout and stderr *)
let run_combine ?(stdin = "") (c : t) : Completed1.t =
  c |> quote
  (* redirect stderr to stdout *)
  |> fun x ->
  x ^ " 2>&1" |> Unix.open_process
  |> with_process_in_out (stdout_stderr_to_string ~stdin)
  |> fun (s, o) -> Completed1.make ~output:o s

(* Runs a process and returns the output that results from
   the combined stdout and stderr.
   Returns the output only when the execution is successful
   (status = 0). *)
let check_output ?(stdin = "") (c : t) : string option =
  c |> run_combine ~stdin |> Completed1.check_output

(* Runs a process and returns the contents of stderr.
   Returns the stderr only when the execution is successful
   (status = 0). *)
let check_stderr ?(stdin = "") (c : t) : string option =
  c |> run_split ~stdin |> Completed2.check_stderr

(* Runs a process and returns the contents of stdout.
   Returns the stdout only when the execution is successful
   (status = 0). *)
let check_stdout ?(stdin = "") (c : t) : string option =
  c |> run_split ~stdin |> Completed2.check_stdout

let run_echo ?(stdin = "") (c : t) : Unix.process_status =
  c |> quote |> Unix.open_process_out
  |> with_process_out (neither_to_string ~stdin)
  |> fst
