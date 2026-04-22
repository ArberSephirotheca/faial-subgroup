open Stage0

module Mode = struct
  type t = Sync | Arrive

  let to_string : t -> string = function Sync -> "sync" | Arrive -> "arrive"
end

type t = {
  mode : Mode.t;
  id : int;
  count : int option;
  loc : Location.t option;
}

let threadsync ?loc () : t =
  { mode = Mode.Sync; id = 0; count = None; loc }

let to_string (s : t) : string =
  match (s.mode, s.id, s.count) with
  | Sync, 0, None -> "__syncthreads"
  | mode, id, None ->
      Printf.sprintf "bar.%s(%d)" (Mode.to_string mode) id
  | mode, id, Some c ->
      Printf.sprintf "bar.%s(%d, %d)" (Mode.to_string mode) id c
