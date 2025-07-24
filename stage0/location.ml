(** Represents a source code location. *)

type t = { filename : string; line : Index.t; interval : Interval.t }

let to_json (loc : t) : Yojson.Basic.t =
  `Assoc
    [
      ("filename", `String loc.filename);
      ("line", `Int (Index.to_base1 loc.line));
      ("col_start", `Int (loc.interval |> Interval.start |> Index.to_base1));
      ("col_finish", `Int (loc.interval |> Interval.finish |> Index.to_base1));
    ]

let make ~filename ~interval ~line : t = { filename; interval; line }
let empty : t = { filename = ""; line = Index.zero; interval = Interval.zero }
let line (x : t) : Index.t = x.line
let filename (x : t) : string = x.filename
let interval (x : t) : Interval.t = x.interval
let length (x : t) : int = x.interval |> Interval.length

let set_length (len : int) (x : t) : t =
  { x with interval = Interval.set_length len x.interval }

let repr (l : t) : string =
  "{filename=\"" ^ filename l ^ "\", " ^ "line=" ^ Index.repr l.line ^ ", "
  ^ "interval=" ^ Interval.repr l.interval ^ "}"

let to_string ?(f = fun x -> x) (l : t) : string =
  Printf.sprintf "%s:%d:%d" (f l.filename) (l.line |> Index.to_base1)
    (l.interval |> Interval.start |> Index.to_base1)

let from_position (pos : Lexing.position) : t =
  let open Lexing in
  let interval =
    Interval.from_range
      ~start:(Index.from_base1 (pos.pos_cnum - pos.pos_bol + 1))
      ~length:0
  in
  { filename = pos.pos_fname; line = Index.from_base1 pos.pos_lnum; interval }

let intersect (lhs : t) (rhs : t) : bool =
  lhs.filename = rhs.filename && lhs.line = rhs.line

let add_or (f : t -> t -> t) (lhs : t) (rhs : t) : t =
  if intersect lhs rhs then
    { lhs with interval = Interval.union lhs.interval rhs.interval }
  else f lhs rhs

let add_or_reset_lhs : t -> t -> t =
  add_or (fun lhs _ ->
      { lhs with interval = Interval.set_length 0 lhs.interval })

let add_or_lhs : t -> t -> t = add_or (fun lhs _ -> lhs)

let from_lexing_pair ((p_start : Lexing.position), (p_end : Lexing.position)) :
    t =
  add_or_reset_lhs (from_position p_start) (from_position p_end)

let from_lexbuf (lb : Lexing.lexbuf) : t =
  let open Lexing in
  from_lexing_pair (lb.lex_start_p, lb.lex_curr_p)

(** Returns a list of the lines that comprise the location. *)
let read_line (loc : t) : string =
  Common.get_line (loc.line |> Index.to_base0) loc.filename

let split (loc : t) : string * string * string =
  loc |> interval |> Slice.from_interval |> Slice.split (read_line loc)
