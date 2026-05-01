open Stage0

module Mode = struct
  type t = Sync | Arrive | Wait | ArriveAndWait | ArriveAndDrop

  let to_string : t -> string = function
    | Sync -> "sync"
    | Arrive -> "arrive"
    | Wait -> "wait"
    | ArriveAndWait -> "arrive_and_wait"
    | ArriveAndDrop -> "arrive_and_drop"
end

(* A barrier is identified by an array variable plus an optional list of
   indices into it.
   - [__syncthreads]: [array] is the sentinel [__syncthreads], [index = []].
   - Legacy PTX [bar.sync N[, M]]: [array] is [bar], [index = [Num N]],
     [count = M].
   - [cuda::barrier]: [array] is the declared barrier variable, [index] is
     the subscript(s) used at the call site. *)
type t = {
  mode : Mode.t;
  array : Variable.t;
  index : Exp.nexp list;
  count : Exp.nexp option;
  loc : Location.t option;
}

let threadsync_array : Variable.t = Variable.from_name "__syncthreads"

let threadsync ?loc () : t =
  { mode = Mode.Sync; array = threadsync_array; index = []; count = None; loc }

let is_threadsync (s : t) : bool =
  s.mode = Mode.Sync
  && Variable.equal s.array threadsync_array
  && s.index = []
  && s.count = None

let to_string (s : t) : string =
  if is_threadsync s then "__syncthreads"
  else
    let idx_s =
      match s.index with
      | [] -> ""
      | l ->
          "[" ^ String.concat ", " (List.map Exp.n_to_string l) ^ "]"
    in
    let args_s =
      match s.count with
      | None -> "()"
      | Some c -> "(" ^ Exp.n_to_string c ^ ")"
    in
    Printf.sprintf "%s%s.%s%s"
      (Variable.name s.array) idx_s (Mode.to_string s.mode) args_s
