type kind = Sint | Uint | Float | Bool
type t = { kind : kind; size : Size.t }

let make (kind : kind) (size : Size.t) : t = { kind; size }
let char : t = { kind = Sint; size = Bit8 }
let unsigned_char : t = { kind = Uint; size = Bit8 }
let short : t = { kind = Sint; size = Bit16 }
let unsigned_short : t = { kind = Uint; size = Bit16 }
let int : t = { kind = Sint; size = Bit32 }
let unsigned_int : t = { kind = Uint; size = Bit32 }
let long : t = { kind = Sint; size = Bit64 }
let unsigned_long : t = { kind = Uint; size = Bit64 }
let bool : t = { kind = Bool; size = Bit8 }
let float : t = { kind = Float; size = Bit32 }
let double : t = { kind = Float; size = Bit64 }

let equal : t -> t -> bool = ( = )
let compare : t -> t -> int = Stdlib.compare

let is_int (x : t) : bool =
  match x.kind with Sint | Uint -> true | Float | Bool -> false

let is_unsigned (x : t) : bool = x.kind = Uint
let is_bool (x : t) : bool = x.kind = Bool
let sizeof (x : t) : int = Size.bytes x.size

(* A bool is not an integer domain even though it is char-sized: converting
   to [_Bool] yields 1 for any nonzero value, where a modular body over a
   char domain would compute [256 mod 256 = 0]. *)
let to_int_dom (x : t) : Int_dom.t option =
  match x.kind with
  | Sint -> Some { Int_dom.size = x.size; signed = true }
  | Uint -> Some { Int_dom.size = x.size; signed = false }
  | Float | Bool -> None

let of_int_dom (d : Int_dom.t) : t =
  { kind = (if d.signed then Sint else Uint); size = d.size }

let to_bounds (x : t) : Bounds.t option =
  match x.kind with
  | Bool -> Some (Bounds.between 0 1)
  | Sint | Uint -> x |> to_int_dom |> Option.map Int_dom.to_bounds
  | Float -> None

let contains (n : int) (x : t) : bool =
  match to_bounds x with Some b -> Bounds.contains n b | None -> false

let to_string (x : t) : string =
  match (x.kind, x.size) with
  | Bool, _ -> "bool"
  | Float, Bit8 -> "__fp8"
  | Float, Bit16 -> "half"
  | Float, Bit32 -> "float"
  | Float, Bit64 -> "double"
  | Sint, s -> Size.to_string s
  | Uint, s -> "unsigned " ^ Size.to_string s
