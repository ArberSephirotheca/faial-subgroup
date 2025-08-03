let make_bold ?(color = [ ANSITerminal.Foreground ANSITerminal.Red ]) =
  ANSITerminal.sprintf ([ ANSITerminal.Bold ] @ color) "%s"

module LocationUI = struct
  let to_string (loc : Location.t) : string =
    let left, mid, right = Location.split loc in
    let lineno = Location.line loc |> Index.to_base1 in
    let prefix = (lineno |> string_of_int) ^ " | " |> make_bold ~color:[] in
    Printf.sprintf "%s%s%s%s" prefix left (make_bold mid) right

  let pair_to_string (l1 : Location.t) (l2 : Location.t) : string =
    if l1.filename = l2.filename && l1.line = l2.line then
      let left, mid, right = Location.split l1 in
      let lineno = Location.line l1 |> Index.to_base1 in
      let prefix = (lineno |> string_of_int) ^ " | " |> make_bold ~color:[] in
      let amount = Interval.finish l1.interval |> Index.to_base0 in
      let i = Interval.translate (-amount) (Location.interval l2) in
      let r1, r2, r3 = Slice.from_interval i |> Slice.split right in
      [ prefix; left; make_bold mid; r1; make_bold r2; r3 ] |> String.concat ""
    else [ to_string l1; "\n"; to_string l2 ] |> String.concat ""

  let bprint (b : Buffer.t) (loc : Location.t) : unit =
    Printf.bprintf b "%s\n" (to_string loc)

  let print (l : Location.t) : unit = print_endline (to_string l)

  let print2 (l1 : Location.t) (l2 : Location.t) : unit =
    pair_to_string l1 l2 |> print_endline
end

let print_frame ~title ~body : unit =
  let max_len = 80 in
  let title_len = String.length title in
  let part_len = (max_len - title_len - 2) / 2 in
  let h_dash (count : int) : string = Common.repeat "─" count in
  let part = h_dash part_len in
  let space = if title_len mod 2 = 1 then " " else "" in
  let open ANSITerminal in
  print_string
    [ Bold; Foreground Magenta ]
    ("╭" ^ part ^ " " ^ title ^ " " ^ space ^ part ^ "╮\n");
  print_string [] body;
  print_string [ Bold; Foreground Magenta ] ("╰" ^ h_dash max_len ^ "╯\n")

let bprint_errors b (errs : (string * Location.t option) list) : bool =
  let print_err ((msg, loc) : string * Location.t option) =
    match loc with
    | Some loc -> (
        Printf.bprintf b "%s %s" (Location.to_string loc) msg;
        try Printf.bprintf b "\n\n%a" LocationUI.bprint loc
        with Sys_error _ -> ())
    | None -> Printf.bprintf b "%s" msg
  in
  List.iter print_err errs;
  List.length errs > 0
