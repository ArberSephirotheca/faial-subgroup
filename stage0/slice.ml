(* Accepts Python-slice expression:
  >> "asdf"[1:-1]
  "sd"
*)
type t = { start : int; finish : int option }

let empty = { start = 0; finish = None }
let from_start start : t = { start; finish = None }
let from_finish finish : t = { start = 0; finish = Some finish }

(* Specify start offset and finish offset *)
let from ~start ~finish : t = { start; finish = Some finish }
let make ~start ~finish : t = { start; finish }

let from_interval (i : Interval.t) : t =
  {
    start = Interval.start i |> Index.to_base0;
    finish = Some (Interval.finish i |> Index.to_base0);
  }

let repr : t -> string = function
  | { start = 0; finish = None } -> "[:]"
  | { start = s; finish = None } -> "[" ^ string_of_int s ^ ":]"
  | { start = 0; finish = Some f } -> "[:" ^ string_of_int f ^ "]"
  | { start = s; finish = Some f } ->
      "[" ^ string_of_int s ^ ":" ^ string_of_int f ^ "]"

let to_interval (total_len : int) (s : t) : Interval.t =
  (* start: *)
  let start = s.start in
  let start = if start < 0 then total_len + start else start in
  let start = min start total_len in
  let start = max start 0 in
  (* finish: *)
  let finish = s.finish in
  let finish =
    match finish with
    | None -> total_len
    | Some n when n < 0 -> total_len + n
    | Some n -> n
  in
  let finish = min finish total_len in
  let finish = max finish 0 in
  (* Convert the offset finish to a length *)
  let length = max (finish - start) 0 in
  Interval.from_range ~start:(Index.from_base0 start) ~length

let substring ?(max_len = None) (data : string) (x : t) : string =
  let max_len =
    match max_len with Some max_len -> max_len | None -> String.length data
  in
  to_interval max_len x |> Interval.substring data

let subarray (data : 'a array) (x : t) : 'a array =
  to_interval (Array.length data) x |> Interval.subarray data

let sublist (data : 'a list) (x : t) : 'a list =
  let data = Array.of_list data in
  subarray data x |> Array.to_list

let split (line : string) (x : t) : string * string * string =
  let max_len = String.length line in
  let i = to_interval max_len x in
  let left = Interval.start i |> Index.to_base0 in
  let right = Interval.finish i |> Index.to_base0 in
  let sub = substring ~max_len:(Some max_len) line in
  ( from ~start:0 ~finish:left |> sub,
    from ~start:left ~finish:right |> sub,
    from_start right |> sub )
