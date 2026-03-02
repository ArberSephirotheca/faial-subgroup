module Mode = struct
  type t =
    | Read
    (* The payload of the write is used
       to detect benign data-races.
       See can-conflict for potentially
       racy accesses. *)
    | Write of int option
    | Atomic of Atomic.t

  let to_string : t -> string = function
    | Read -> "ro"
    | Write (Some n) -> "rw(" ^ string_of_int n ^ ")"
    | Write None -> "rw"
    | Atomic x -> Atomic.to_string x

  let is_read : t -> bool = function Read -> true | _ -> false
  let is_write : t -> bool = function Write _ -> true | _ -> false
  let is_atomic : t -> bool = function Atomic _ -> true | _ -> false

  let can_conflict (m1 : t) (m2 : t) : bool =
    match (m1, m2) with
    | Read, Read -> false
    | Write (Some x), Write (Some y) -> x <> y
    | _, _ -> true
end

(* An access pairs the index-expression with the access mode (R/W) *)
type t = { array : Variable.t; index : Exp.nexp list; mode : Mode.t }

let array (x : t) : Variable.t = x.array
let location (e : t) : Stage0.Location.t = Variable.location e.array
let mode (x : t) : Mode.t = x.mode
let is_write (x : t) : bool = Mode.is_write x.mode
let is_read (x : t) : bool = Mode.is_read x.mode

let map (f : Exp.nexp -> Exp.nexp) (a : t) : t =
  { a with index = List.map f a.index }

let index_to_string (ns : Exp.nexp list) : string =
  let idx = ns |> List.map Exp.n_to_string |> String.concat ", " in
  "[" ^ idx ^ "]"

let to_string (a : t) : string =
  Mode.to_string a.mode ^ " " ^ Variable.name a.array ^ index_to_string a.index

let write (array : Variable.t) (index : Exp.nexp list) (v : int option) : t =
  { array; index; mode = Write v }

let read (array : Variable.t) (index : Exp.nexp list) : t =
  { array; index; mode = Read }

let atomic ~array ~atomic (index : Exp.nexp list) : t option =
  Atomic.from_name atomic
  |> Option.map (fun a -> { array; index; mode = Atomic a })

let index_intersects (s : Variable.Set.t) (a : t) : bool =
  List.exists (Exp.n_intersects s) a.index

let can_conflict (a1 : t) (a2 : t) = Mode.can_conflict a1.mode a2.mode

let free_names (a : t) (fns : Variable.Set.t) : Variable.Set.t =
  List.fold_right Exp.n_free_names a.index fns
