type t = Const | Volatile | Restrict

let to_string : t -> string = function
  | Const -> "const"
  | Volatile -> "volatile"
  | Restrict -> "restrict"

(* The spellings c-to-json emits, longest first so that [__restrict__] is
   not mistaken for [__restrict] followed by a stray [__]. *)
let spellings : (string * t) list =
  [
    ("__restrict__", Restrict);
    ("__restrict", Restrict);
    ("restrict", Restrict);
    ("volatile", Volatile);
    ("const", Const);
  ]

let of_string (s : string) : t option = List.assoc_opt s spellings

module Set = Set.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
end)
