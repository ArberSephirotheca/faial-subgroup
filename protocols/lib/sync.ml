open Stage0

module Mode = struct
  type t = Arrive | Wait | ArriveAndWait | ArriveAndDrop

  let to_string : t -> string = function
    | Arrive -> "arrive"
    | Wait -> "wait"
    | ArriveAndWait -> "arrive_and_wait"
    | ArriveAndDrop -> "arrive_and_drop"
end

(* A barrier is identified by a numeric expression [id] — the slot
   threads synchronise on — plus a [mode] and an optional [participants]
   count. All barriers live in one numeric id space; PTX named slots
   are concrete integers 0..15, [__syncthreads] is slot 0, and
   user-declared [cuda::barrier] objects are symbolic variables that
   the analysis can constrain to be disjoint from those slots.
   - [__syncthreads]: [id = Num 0], [participants = None].
   - PTX [bar.sync N[, M]]: [id = Num N] (or the resolved placeholder),
     [participants = M].
   - [cuda::barrier foo]: [id = Var foo].
   - [cuda::barrier foo[i]]: [id = Var foo + i]. *)
type t = {
  mode : Mode.t;
  id : Exp.nexp;
  participants : Exp.nexp option;
  loc : Location.t option;
}

let syncthreads_id : Exp.nexp = Exp.Num 0

let syncthreads ?loc () : t =
  { mode = Mode.ArriveAndWait; id = syncthreads_id; participants = None; loc }

let is_syncthreads (s : t) : bool =
  s.mode = Mode.ArriveAndWait
  && s.id = syncthreads_id
  && s.participants = None

let to_string (s : t) : string =
  if is_syncthreads s then "__syncthreads"
  else
    let args_s =
      match s.participants with
      | None -> "()"
      | Some c -> "(" ^ Exp.n_to_string c ^ ")"
    in
    Printf.sprintf "%s.%s%s"
      (Exp.n_to_string s.id) (Mode.to_string s.mode) args_s
